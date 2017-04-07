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

import qualified Data.ByteString.Char8 as B
import qualified Data.Configurator as C
import Data.Monoid ((<>))
import qualified Data.Text as T
--import Data.UnixTime (getUnixTime, toClockTime, utSeconds)
import Data.Time (UTCTime)
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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Session
    validUntil UTCTime
    personId PersonId
  Person
    email String
    password String
    zipcode String Maybe
    UniqueUsername email
    deriving Eq Show
  BlogPost
    title String
    authorId PersonId
    deriving Eq Show
|]

main :: IO ()
main = do
  sitecfg <- parseConfig "siteconfig.cfg" --TODO: default to 'siteconfig.cfg',
  pool <- dbpool sitecfg
  cfg <- defaultSpockCfg (AppSession "SessionName") pool (AppState sitecfg)
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
        liftIO $ print sid
        (lucid $ homePage)

    get "/contact" $ do
        text("contact page")

    post "/contact" $ do
        showRequest

    get "/register" $ do
        text("registration \nform")

    post "/register" $ do
        email <- param "email"
        password <- param "password"
        case email of
          Just e -> case password of
            Just p -> do runDB $ insert $ Person e p Nothing
                         liftIO $ print ("Added: "++e)
                         text("succes!")
            Nothing -> text("oops, password param missing from form")
          Nothing -> text("oops, email param missing from form")

    get "/allusers" $ do
        users <- runDB $ selectList [] [Asc PersonEmail] -- [Entity record]
        liftIO $ print $ map entityVal users
        lucid $ allUsersPage (map (\u -> let e = entityVal u
                                         in (personEmail e, personPassword e)) users)

    get ("/profile" <//> var) $ \person -> (lucid $ profilePage person)

    get ("/room" <//> var) $ \r -> do
        text ("Welcome to room " <> (T.pack r))

    post ("/room" <//> var) $ \(r :: T.Text) -> do
        text (T.pack $ "room")

---------------------- Lucid stuff -----------------------
lucid :: MonadIO m => Html a1 -> ActionCtxT ctx m a
lucid = lazyBytes . renderBS

runDB :: (HasSpock m, SpockConn m ~ SqlBackend) =>
         SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runDB action = runQuery $ \conn ->
    runResourceT $ runNoLoggingT $ runSqlConn action conn
