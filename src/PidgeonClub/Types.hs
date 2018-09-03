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
import Data.Time (UTCTime)
import Network.Socket
import Database.Persist.Sql

----------------- Spock -----------------------
import Web.Spock

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
  Pidgeon
    name T.Text
    height Double
    weight Double
    color T.Text
    addedby PersonId
    deriving Show
  Imagepidgeon
    filehash T.Text
    filepath T.Text
    rank Int
    pidgeon PidgeonId
    deriving Show
|]

data PidgeonConfig = PidgeonConfig
    { dbHost :: HostName
    , dbPort :: Int
    , dbName :: String
    , dbUser :: String
    , dbPass :: String
    } deriving (Show)

newtype AppState = AppState {getCfg :: PidgeonConfig}
type AppSession = Maybe SessieId
type PidgeonApp ctx a = SpockCtxM ctx SqlBackend AppSession AppState a
type PidgeonAction = SpockActionCtx () SqlBackend AppSession AppState

