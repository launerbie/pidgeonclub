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

import qualified Crypto.Hash.SHA512 as SHA

import qualified Data.ByteString.Char8 as B
import qualified Data.Configurator as C
import Data.Monoid ((<>))
import qualified Data.Text as T
--import Data.UnixTime (getUnixTime, toClockTime, utSeconds)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import qualified Database.Persist as P
import Database.Persist.Postgresql ( ConnectionString, createPostgresqlPool
                                   , SqlPersistT)
import Database.Persist.Sql hiding (get)
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
        case email of
          Just e -> case password of
            Just p -> do
               runDB $ insert $ Person e p
               liftIO $ print ("Added: "++e)
               text("succes!")
            Nothing -> text("oops, password param missing from form")
          Nothing -> text("oops, email param missing from form")

    get "/allusers" $ do
        users <- runDB $ selectList [] [Asc PersonEmail] -- [Entity record]
        liftIO $ print $ map entityVal users
        lucid $ allUsersPage (map (\u -> let e = entityVal u
                                         in (personEmail e, personPassword e)) users)

    get ("/user" <//> var) $ \user -> (lucid $ profilePage user)

    get "/profile" $ do
        r <- readSession
        liftIO $ print r
        --mSid <- runDB $ get sid
        --liftIO $ print mSid
        text(T.pack $ show r)

    get "/login" $ lucid loginPage

    post "/login" $ do
        showRequest
        email <- param "email"
        password <- param "password"
        case email of
          Just e -> case password of
            Just p -> do
               mPerson <- runDB $ getBy (UniqueUsername e)
               mPass <- runDB $ getBy (UniqueUsername p)
               -- for now, don't check password
               -- just check if user exists
               case mPerson of
                   Just personEntity -> do
                       liftIO $ print personEntity
                       now <- liftIO getCurrentTime
                       let validTil = addUTCTime 3600 now
                       let person = entityKey personEntity
                       sid <- runDB $ insert (Sessie validTil person)
                       liftIO $ print sid
                       writeSession (Just sid)
                       text("Login succesful.")
                   Nothing -> text("Invalid email or password")
            Nothing -> text("oops, password param missing from form")
          Nothing -> text("oops, email param missing from form")
        redirect "/"

---------------------- Lucid stuff -----------------------
lucid :: MonadIO m => Html a1 -> ActionCtxT ctx m a
lucid = lazyBytes . renderBS

runDB :: (HasSpock m, SpockConn m ~ SqlBackend) =>
         SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runDB action = runQuery $ \conn ->
    runResourceT $ runNoLoggingT $ runSqlConn action conn
