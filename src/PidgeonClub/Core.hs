{-# LANGUAGE OverloadedStrings #-}

module PidgeonClub.Core where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad
import Database.Persist.Sql

import qualified Data.Text as T
import Text.Pretty.Simple (pPrint)
import Network.Socket (HostName)
import Lucid
import Web.Spock hiding (SessionId)
import Web.Spock.Action

--------------------- PidgeonClub ----------------------
--import PidgeonClub.Actions
import PidgeonClub.Types
import PidgeonClub.Views

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

-- | Prints the HTTP request
showRequest :: PidgeonAction ()
showRequest = do
    r <- request
    p <- params
    liftIO $ do pPrint r
                print p


validUsername :: T.Text -> Bool
validUsername = T.all validChar

validPassword :: T.Text -> Bool
validPassword = (>7) . T.length

validEmail :: T.Text -> Bool
validEmail = undefined

validChar :: Char -> Bool
validChar x = x `elem` ['-', '_']++['a'..'z']++['A'..'Z']++['0'..'9']

isFreeUsername = undefined

