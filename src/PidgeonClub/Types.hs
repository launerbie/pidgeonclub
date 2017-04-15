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


type SignupInput = (T.Text, T.Text, T.Text)

-------------------------------- Forms -------------------------------------
data SignupError = SignupError
    { usernameError :: [String]
    , passwordError :: [String]
    , passwordErrorConfirm :: [String]
    }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Sessie
    validUntil UTCTime
    personId PersonId
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
|]


