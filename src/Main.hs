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
  -- TODO: get port from cmd argument
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

    get "/" $ lucid homePage

    get "/signup" $ do
        lucid (signupPage Nothing)

    post "/signup" $ do
        mEmail <- param "email"
        mPassword <- param "password"
        mPasswordConf <- param "passwordConfirm"
        case mEmail of
          Just email -> do
            mPerson <- runDB $ getBy (UniqueUsername $ T.toLower email)
            case mPerson of
              Just person -> simpleText ("This user already exists")
              Nothing ->
                  case mPassword of
                    Just p1 -> case mPasswordConf of
                        Just p2 ->
                           if p1 == p2 then do
                               g <- liftIO $ newStdGen
                               let salt = randomBS 16 g
                                   hash = hashPassword p1 salt
                                   mail = T.toLower email
                               runDB $ insert $ Person mail (makeHex hash) (makeHex salt) Nothing
                               liftIO $ print ("Added: "++ T.unpack mail)
                               simpleText ("succes!")
                           else do
                               simpleText ("passwords don't match!")
                        Nothing -> simpleText ("oops, passwordConfirm param missing from form")
                    Nothing -> simpleText ("oops, password param missing from form")
          Nothing -> simpleText ("oops, email param missing from form")
    -- TODO:
    -- Currently this code will short circuit on the first Nothing, but
    -- actually want to check all params and return errors on all params.

    -- TODO: Make accessible only for admins
    -- TODO: For now, just add requireUser
    get "/allusers" $ do
        users <- runDB $ selectList [] [Asc PersonEmail] -- [Entity record]
        lucid $ allUsersPage (map (\u -> let e = entityVal u
                                         in (personEmail e, personPassword e,personSalt e )) users)

    -- The user's public page
    -- Display some publicly available information on the user on this page
    get ("/user" <//> var) $ \user -> (lucid $ profilePage user)

    -- The user's settings page
    get "/profile" $ requireUser $ \u -> do
        liftIO $ print u
        mPerson <- runDB $ PSQL.get u
        case mPerson of
            Just p -> simpleText $ "Logged in as: " <> personEmail p
            Nothing -> simpleText "user doesn't exist anymore"

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
                               writeSession (Just sid)
                               simpleText ("Login succesful.")
                       else simpleText ("Invalid email or password")
                   Nothing -> simpleText ("Invalid email or password")
            Nothing -> simpleText ("oops, password param missing from form")
          Nothing -> simpleText ("oops, email param missing from form")
        redirect "/"

requireUser :: (Key Person -> PidgeonAction a) -> PidgeonAction a
requireUser action = do
  mSessid <- readSession
  case mSessid of
    Just sess -> do
      mPersonId <- getUserFromSession sess
      case mPersonId of
         Nothing -> simpleText "Sorry, no access!"
         Just user -> action user
    Nothing -> simpleText "Please login first"

getUserFromSession :: SessieId -> PidgeonAction (Maybe PersonId)
getUserFromSession sid = do
  mSid <- runDB $ PSQL.get sid -- :: Maybe Sessie
  liftIO $ print mSid
  case mSid of
      Just sess -> do
          liftIO $ print sess
          -- TODO: check if sess is not expired
          return $ Just (sessiePersonId sess)
      Nothing -> simpleText ("Invalid session")

---------------------- Lucid stuff -----------------------
-- TODO: move out of Main.hs
lucid :: MonadIO m => Html a1 -> ActionCtxT ctx m a
lucid = lazyBytes . renderBS

simpleText :: MonadIO m => T.Text -> ActionCtxT ctx m a
simpleText x = lucid (simplePage x)
---------------------- Persistent ------------------------
-- TODO: move out of Main.hs
runDB :: (HasSpock m, SpockConn m ~ SqlBackend) =>
         SqlPersistM a -> m a
runDB action = runQuery $ \conn ->
    runResourceT $ runNoLoggingT $ runSqlConn action conn

--------------- Hex / UnHex --------------------------
-- TODO: move out of Main.hs
makeHex :: BS.ByteString -> T.Text
makeHex = T.decodeUtf8 . B16.encode
{-# INLINE makeHex #-}

decodeHex :: T.Text -> BS.ByteString
decodeHex = fst . B16.decode . T.encodeUtf8
{-# INLINE decodeHex #-}

--------------------- Crypto -----------------------------
-- TODO: move out of Main.hs
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

