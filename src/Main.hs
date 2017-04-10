{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import Control.Monad        (liftM, when, unless, guard)
import Control.Monad.Trans  (lift, MonadIO, liftIO)
import Control.Monad.Trans.Resource  (ResourceT, runResourceT)
import Control.Monad.Logger    (runNoLoggingT, runStderrLoggingT, NoLoggingT)

import Data.Char (toLower)
import Data.Word8 hiding (toLower)
import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA256 as SHA
import System.Random

import qualified Data.ByteString.Base16 as B16

import qualified Data.ByteString.Char8 as B
import qualified Data.Configurator as C
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
--import Data.UnixTime (getUnixTime, toClockTime, utSeconds)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import qualified Database.Persist as P
import Database.Persist.Postgresql ( ConnectionString, createPostgresqlPool
                                   , SqlPersistT)
import Database.Persist.Sql hiding (get)
import qualified Database.Persist.Sql as PSQL
import Database.Persist.TH
import Lucid
import Network.Wai.Middleware.Static (staticPolicy, addBase)

------ Spock ----------
import Web.Spock ( get, getpost, post, getState, HasSpock, html, lazyBytes, middleware
                 , redirect, runSpock, spock, SpockM, SpockCtxM, SpockConn, ActionCtxT
                 , SpockActionCtx, root, RouteSpec, renderRoute, runQuery, text, var
                 , Var, (<//>) )

import Web.Spock.Config  ( defaultSpockCfg, PoolOrConn (PCNoDatabase, PCPool)
                         , SpockCfg )
import Web.Spock.Action  ( request, body, params, param, param')
import Web.Spock.SessionActions (getSessionId, readSession, writeSession)

------ Pidgeon --------
import PidgeonClub.Views
--import PidgeonClub.Actions
import PidgeonClub.Core

main :: IO ()
main = do
  sitecfg <- parseConfig "siteconfig.cfg" --TODO: default to 'siteconfig.cfg',
  pool <- dbpool sitecfg
  -- defaultSpockCfg :: sess -> PoolOrConn conn -> st -> IO (SpockCfg conn sess st)
  cfg <- defaultSpockCfg (Nothing) pool (AppState sitecfg)
  runSpock 8080 (spock cfg app)

dbpool :: PidgeonConfig -> IO (PoolOrConn SqlBackend)
dbpool pcfg = do
    pgpool <- runNoLoggingT $ createPostgresqlPool (getConnString pcfg) 10
    runSqlPool (runMigration migrateAll) pgpool
    return $ PCPool pgpool

parseConfig :: FilePath -> IO PidgeonConfig
parseConfig f = do
    cfg <- C.load [C.Required f]
    host   <- C.require cfg "postgres.host"
    port   <- C.require cfg "postgres.port"
    dbname <- C.require cfg "postgres.db"
    user   <- C.require cfg "postgres.user"
    passwd <- C.require cfg "postgres.passwd"
    return (PidgeonConfig host port dbname user passwd)

getConnString :: PidgeonConfig -> ConnectionString
getConnString p = B.pack $ concat [ "host=", (dbHost p)
                                  , " dbname=", (dbName p)
                                  , " user=", (dbUser p)
                                  , " password=", (dbPass p)
                                  , " port=", show (dbPort p)
                                  ]

app :: PidgeonApp () ()
app =  do
    middleware (staticPolicy (addBase "static"))

    get "/" $ do
        sid <- getSessionId
        r <- readSession
        liftIO $ print sid
        liftIO $ print r
        (lucid $ homePage)

    get "/contact" $ do
        text("contact page")

    post "/contact" $ do
        showRequest

    get "/signup" $ do
        lucid (signupPage Nothing)

    post "/signup" $ do
        email <- param "email"
        password <- param "password"
        passwordConf <- param "passwordConfirm"
        -- TODO: Check if email isn't already in database
        case email of
          Just e -> case password of
            Just p1 -> case passwordConf of
                Just p2 ->
                   if p1 == p2 then do
                       g <- liftIO $ newStdGen
                       let salt = randomBS 16 g
                           hash = hashPassword p1 salt
                           mail = T.toLower e
                       runDB $ insert $ Person mail (makeHex hash) (makeHex salt)
                       liftIO $ print ("Added: "++ T.unpack e)
                       text("succes!")
                   else do
                       text("passwords don't match!")
                Nothing -> text("oops, passwordConfirm param missing from form")
            Nothing -> text("oops, password param missing from form")
          Nothing -> text("oops, email param missing from form")

    -- TODO: Make accessible only for admins
    get "/allusers" $ do
        users <- runDB $ selectList [] [Asc PersonEmail] -- [Entity record]
        liftIO $ print $ map entityVal users
        lucid $ allUsersPage (map (\u -> let e = entityVal u
                                         in (personEmail e, personPassword e,personSalt e )) users)

    -- The user's public page
    get ("/user" <//> var) $ \user -> (lucid $ profilePage user)

    -- The user's settings page
    get "/profile" $ do
        mSession <- readSession
        liftIO $ print mSession
        case mSession of
            Just sid -> do
                mSid <- runDB $ PSQL.get sid
                liftIO $ print mSid
                case mSid of
                    Just sess -> do
                        -- TODO: check if sess is not expired
                        mPerson <- runDB $ PSQL.get (sessiePersonId sess)
                        liftIO $ print mPerson
                        case mPerson of
                            Just p -> text(T.pack $ show $ personEmail p)
                            Nothing -> text("user doesn't exist anymore")
                        text("person exists")
                    Nothing -> text("Invalid session")
                text("cool")
            Nothing -> text("Please login first.")

    get "/login" $ lucid loginPage

    post "/login" $ do
        showRequest
        email <- param "email"
        password <- param "password"
        case email of
          Just e -> case password of
            Just p -> do
               mPerson <- runDB $ getBy (UniqueUsername $ T.toLower e)
               mPass <- runDB $ getBy (UniqueUsername p)
               case mPerson of
                   Just personEntity -> do
                       liftIO $ print personEntity
                       now <- liftIO getCurrentTime

                       let validTil = addUTCTime 3600 now
                           person = entityKey personEntity
                           salt = personSalt $ entityVal personEntity
                           hash = personPassword $ entityVal personEntity

                       if hash == (makeHex $ hashPassword p (decodeHex $ salt))
                       then do sid <- runDB $ insert (Sessie validTil person)
                               liftIO $ print sid
                               liftIO $ print salt
                               writeSession (Just sid)
                               text("Login succesful.")
                       else text("Invalid email or password")
                   Nothing -> text("Invalid email or password")
            Nothing -> text("oops, password param missing from form")
          Nothing -> text("oops, email param missing from form")
        redirect "/"

---------------------- Lucid stuff -----------------------
lucid :: MonadIO m => Html a1 -> ActionCtxT ctx m a
lucid = lazyBytes . renderBS

---------------------- Persistent ------------------------
runDB :: (HasSpock m, SpockConn m ~ SqlBackend) =>
         SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runDB action = runQuery $ \conn ->
    runResourceT $ runNoLoggingT $ runSqlConn action conn

--------------- Hex / UnHex --------------------------
makeHex :: BS.ByteString -> T.Text
makeHex = T.decodeUtf8 . B16.encode
{-# INLINE makeHex #-}

decodeHex :: T.Text -> BS.ByteString
decodeHex = fst . B16.decode . T.encodeUtf8
{-# INLINE decodeHex #-}

--------------------- Crypto -----------------------------
randomBytes:: Int -> StdGen -> [Word8]
randomBytes 0 _ = []
randomBytes ct g =
    let (value, nextG) = next g
    in fromIntegral value:randomBytes (ct - 1) nextG

randomBS :: Int -> StdGen -> BS.ByteString
randomBS len g =
    BS.pack $ randomBytes len g

hashPassword :: T.Text -> BS.ByteString -> BS.ByteString
hashPassword password salt =
     SHA.finalize $ SHA.updates SHA.init [salt, T.encodeUtf8 $ password]

