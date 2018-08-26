{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module PidgeonClub.Types where

import qualified Data.Text as T
import Database.Persist.TH
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Network.Socket
import Database.Persist.Sql hiding (get)

----------------- Spock -----------------------
import Web.Spock ( get, post, HasSpock, lazyBytes, middleware
                 , redirect, runSpock, spockAsApp, spock, SpockCtxM, SpockConn, ActionCtxT
                 , SpockActionCtx, root, runQuery, text, var
                 , (<//>) )

import Web.Spock.Config  ( defaultSpockCfg, PoolOrConn (PCNoDatabase, PCPool)
                         , SpockCfg )
import Web.Spock.Action  ( request, params, param, param')
import Web.Spock.SessionActions (getSessionId, readSession, writeSession)


----------------------- Database Schema ---------------------------------
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Sessie
    validUntil UTCTime
    personId PersonId
    ipaddress T.Text
    deriving Show
  Person
    email T.Text
    password T.Text
    salt T.Text
    username T.Text Maybe
    UniqueUsername email
    deriving Show
  Package
    receiver PersonId
    sender PersonId
    destination LocationId
    source LocationId
    deriving Show
  Location
    latitude Double
    longitude Double
    deriving Show
  Message
    message T.Text
    creator PersonId
    time UTCTime
    package PackageId
    deriving Show
  Login
    time UTCTime
    ipaddress T.Text
    personId PersonId
    deriving Show
|]

data PidgeonConfig = PidgeonConfig
    { dbHost :: HostName
    , dbPort :: Int
    , dbName :: String
    , dbUser :: String
    , dbPass :: String
    } deriving (Show)

data AppState = AppState {getCfg :: PidgeonConfig}
type AppSession = Maybe SessieId
type PidgeonApp ctx a = SpockCtxM ctx SqlBackend AppSession AppState a
type PidgeonAction = SpockActionCtx () SqlBackend AppSession AppState

