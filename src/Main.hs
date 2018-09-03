{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where

import Control.Monad.Logger    (runNoLoggingT)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as B
import qualified Data.Configurator as C

import Text.Pretty.Simple (pPrint)
import Data.HashMap.Strict (toList)
import Control.Monad.Trans
import System.Directory


----------------- Persistence -----------------
import Database.Persist.Postgresql ( ConnectionString, createPostgresqlPool)
import Database.Persist.Sql hiding (get)
import qualified Database.Persist.Sql as PSQL

------------------ Network------------------------
import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static (staticPolicy, addBase)

----------------- Spock -----------------------
import Web.Spock hiding (head)
import Web.Spock.Action
import Web.Spock.Config  ( defaultSpockCfg, PoolOrConn (PCNoDatabase, PCPool)
                         , SpockCfg )
import Web.Spock.Action  ( request, params, param, param')
import Web.Spock.SessionActions (getSessionId, readSession, writeSession)

--------------- PidgeonClub ------------------
import PidgeonClub.Views
import PidgeonClub.Types
import PidgeonClub.Forms

main :: IO ()
main = do
  sitecfg <- parseConfig "siteconfig.cfg" --TODO: default to 'siteconfig.cfg',
  pool <- dbpool sitecfg
  -- defaultSpockCfg :: sess -> PoolOrConn conn -> st -> IO (SpockCfg conn sess st)
  cfg <- defaultSpockCfg Nothing pool (AppState sitecfg)
  -- TODO: get port from cmd argument
  application <- spockAsApp $ spock cfg app
  runSpock 8080 (spock cfg app)

  --let certpath = "certs/live/homesecurity.fun/fullchain.pem"
  --let keypath = "certs/live/homesecurity.fun/privkey.pem"
  --let tls = tlsSettings certpath keypath
  --let settings = setPort 443 defaultSettings
  --runTLS tls settings application

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
getConnString p = B.pack $ concat [ "host=", dbHost p
                                  , " dbname=", dbName p
                                  , " user=", dbUser p
                                  , " password=", dbPass p
                                  , " port=", show (dbPort p)
                                  ]

app :: PidgeonApp () ()
app =  do
    middleware (staticPolicy (addBase "static"))

    -- ###################   Public endpoints   #########################
    get "/test" $ lucid testPage

    get "/" $ lucid homePage

    get "/signup" $ lucid (signupPage Nothing)

    post "/signup" $ do
        sr <- validSignupRequest
        p  <- mkPerson sr
        _  <- insertPerson p
        lucid $ signupSuccessPage (srEmail sr)

    get "/login" $ do
        r <- readSession
        case r of
            Nothing -> lucid $ loginPage Nothing
            Just sess -> requireUser $ \u -> do -- sess unused
                mPerson <- runDB $ PSQL.get u
                case mPerson of
                    Just p -> lucid $ loginPage (Just p)
                    Nothing -> simpleText "user doesn't exist anymore"

    post "/login" $ do
        lr                 <- loginRequest
        (personId, person) <- getPersonFromRequest lr
        let (salt, hash) = (personSalt person, personPassword person)
        if hash == makeHex (hashPassword (lrPassword lr) (decodeHex salt))
            then loginUser personId
            else simpleText "Invalid email or password"

    get "/reset" $ lucid resetPage

    post "/reset" $ do
        mEmail <- param "email"
        case mEmail of
            Just e -> simpleText ("A password reset mail has been sent to: " <> e)
              --TODO: actually send password reset mail
            Nothing -> text "Oops, something went wrong with your request!"


    -- ###################   Private endpoints  #########################

    get "/allusers" $ requireUser $ \_ -> do
    -- TODO: Make accessible only for admins
        users <- runDB $ selectList [] [Asc PersonEmail] -- [Entity record]
        let persons = map entityVal users
        lucid $ allUsersPage persons

    get "/newpidgeon" $ requireUser $ \u -> do
         lucid (addNewPidgeonPage Nothing)
         lucid (addNewPidgeonPage Nothing)

    post "/newpidgeon" $ requireUser $ \u -> do
         showRequest
         h <- files
         liftIO $ pPrint h
         liftIO $ do
           let (t, uf) = head (toList h)
           copyFile (uf_tempLocation uf) "/tmp/test"
         simpleText "Pidgeon has been submitted."

    get "/pidgeons" $ requireUser $ \u ->
        simpleText "List all pidgeons here."

    -- The user's settings page
    get "/settings" $ requireUser $ \u ->
        redirect "/settings/profile"

    get "/settings/profile" $ requireUser $ \u -> do
        lucid $ settingsPage SettingsProfile

    get "/settings/account" $ requireUser $ \u -> do
        lucid $ settingsPage SettingsAccount

    post "settings/account" $ requireUser $ \u -> do
        (currentpass, newpass, newpassconfirm) <- changePasswordRequest
        checkPassword u currentpass
        updatePassword u newpass newpassconfirm
        simpleText "Your password has been changed."

    get "/settings/security" $ requireUser $ \u -> do
        lucid $ settingsPage SettingsSecurity

    get "/loginhistory" $ requireUser $ \u -> do
        logins <- runDB $ selectList [LoginPersonId ==. u] []
        lucid $ loginHistoryPage (map entityVal logins)

    get "/logout" $ requireUser $ \u -> do
        killSessions u
        writeSession Nothing
        redirect "/"

    --get "/testrequirelogin" $ requireUser $ \_ -> do
    --    (runReaderT $ testPage) "x" >>= renderPage

    -- The user's public page
    -- Display some publicly available information on the user on this page
    --get ("/user" <//> var) $ \user -> lucid $ userPage user
