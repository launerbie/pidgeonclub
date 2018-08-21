{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where

import Control.Monad        (liftM, when, unless, guard)
import Control.Monad.Trans  (lift, MonadIO, liftIO)
import Control.Monad.Trans.Resource  (ResourceT, runResourceT)
import Control.Monad.Trans.Maybe
import Control.Monad.Logger    (runNoLoggingT, runStderrLoggingT, NoLoggingT)
import Data.Char (toLower)
import Data.Word8 hiding (toLower)
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B
import qualified Data.Configurator as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UnixTime (getUnixTime, toClockTime, utSeconds)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Lucid
import Network.Socket
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.Wai
import System.Random
import Text.Pretty.Simple (pPrint)

----------------- Persistence -----------------
import qualified Database.Persist as P
import Database.Persist.Postgresql ( ConnectionString, createPostgresqlPool
                                   , SqlPersistT)
import Database.Persist.Sql hiding (get)
import qualified Database.Persist.Sql as PSQL
import Database.Persist.TH

import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.Warp

----------------- Spock -----------------------
import Web.Spock ( get, post, HasSpock, lazyBytes, middleware
                 , redirect, runSpock, spockAsApp, spock, SpockCtxM, SpockConn, ActionCtxT
                 , SpockActionCtx, root, runQuery, text, var
                 , (<//>) )

import Web.Spock.Config  ( defaultSpockCfg, PoolOrConn (PCNoDatabase, PCPool)
                         , SpockCfg )
import Web.Spock.Action  ( request, params, param, param')
import Web.Spock.SessionActions (getSessionId, readSession, writeSession)

--------------- PidgeonClub ------------------
import PidgeonClub.Views
import PidgeonClub.Types
import PidgeonClub.Forms

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

main :: IO ()
main = do
  sitecfg <- parseConfig "siteconfig.cfg" --TODO: default to 'siteconfig.cfg',
  pool <- dbpool sitecfg
  -- defaultSpockCfg :: sess -> PoolOrConn conn -> st -> IO (SpockCfg conn sess st)
  cfg <- defaultSpockCfg (Nothing) pool (AppState sitecfg)
  -- TODO: get port from cmd argument
  --runSpock 8080 (spock cfg app)
  application <- spockAsApp $ spock cfg app
  let tls = (tlsSettings "certificate.pem" "key.pem")
  let settings = (setPort 443 defaultSettings)
  runTLS tls settings application

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
        r <- readSession
        case r of
            Just sess -> lucid $ homePage LoggedIn
            Nothing -> lucid $ homePage LoggedOut

    get "/signup" $ do
        r <- readSession
        case r of
            Just sess -> lucid (signupPage Nothing LoggedIn)
            Nothing -> lucid (signupPage Nothing LoggedOut)

    post "/signup" $ do
        sr <- validSignupRequest
        p  <- mkPerson sr
        insertPerson p
        lucid $ signupSuccessPage (srEmail sr) LoggedOut

    -- TODO: Make accessible only for admins
    get "/allusers" $ requireUser $ \_ -> do
        users <- runDB $ selectList [] [Asc PersonEmail] -- [Entity record]
        let persons = map entityVal users
        lucid $ allUsersPage persons LoggedIn

    -- The user's public page
    -- Display some publicly available information on the user on this page
    get ("/user" <//> var) $ \user -> (lucid $ userPage user LoggedOut )

    -- The user's settings page
    get "/settings" $ requireUser $ \u -> do
        redirect "/settings/profile"

    get "/settings/profile" $ requireUser $ \u -> do
        person <- getPerson u
        lucid $ settingsPage person SettingsProfile LoggedIn

    get "/settings/account" $ requireUser $ \u -> do
        person <- getPerson u
        lucid $ settingsPage person SettingsAccount LoggedIn

    post "settings/account" $ requireUser $ \u -> do
        (currentpass, newpass, newpassconfirm) <- changePasswordRequest
        checkPassword u currentpass
        updatePassword u newpass newpassconfirm
        simpleText "Your password has been changed."

    get "/settings/security" $ requireUser $ \u -> do
        person <- getPerson u
        lucid $ settingsPage person SettingsSecurity LoggedIn

    get "/loginhistory" $ requireUser $ \u -> do
        person <- getPerson u
        logins <- runDB $ selectList [LoginPersonId ==. u] []
        lucid $ loginHistoryPage (map entityVal logins) LoggedIn

    get "/login" $ do
        r <- readSession
        case r of
            Nothing -> lucid $ loginPage Nothing LoggedOut
            Just sess -> requireUser $ \u -> do
                mPerson <- runDB $ PSQL.get u
                case mPerson of
                    Just u -> lucid $ loginPage (Just u) LoggedIn
                    Nothing -> simpleText "user doesn't exist anymore"

    post "/login" $ do
        lr                 <- loginRequest
        (personId, person) <- getPersonFromRequest lr
        let (salt, hash) = (personSalt person, personPassword person)
        if hash == (makeHex $ hashPassword (lrPassword lr) (decodeHex $ salt))
            then loginUser personId
            else simpleText ("Invalid email or password")

    get "/logout" $ requireUser $ \u -> do
        killSessions u
        writeSession Nothing
        redirect "/"

    get "/reset" $ do
        lucid $ resetPage LoggedOut

    post "/reset" $ do
        mEmail <- param "email"
        case mEmail of
            Just e -> do
              simpleText ("A password reset mail has been sent to: " <> e)
              --TODO: actually send password reset mail
            Nothing -> text ("Oops, something went wrong with your request!")

loginUser :: PersonId -> PidgeonAction ()
loginUser personId = do
  ip       <- fmap (T.pack . getIP4 . remoteHost) request
  utctime  <- liftIO $ getCurrentTime
  validTil <- liftIO $ liftM (addUTCTime 3600) getCurrentTime
  sid      <- runDB $ do deleteWhere [ SessiePersonId ==. personId ]
                         insert (Login utctime ip personId)
                         insert (Sessie validTil personId ip)
                         --TODO: save unixtime
  writeSession (Just sid)
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
    Nothing -> simpleText "Not logged in."

getUserFromSession :: SessieId -> PidgeonAction (Maybe PersonId)
getUserFromSession sid = do
  mSid <- runDB $ PSQL.get sid -- :: Maybe Sessie
  case mSid of
      Just sess -> do
          now <- liftIO getCurrentTime
          if sessieValidUntil sess > now
            then return $ Just (sessiePersonId sess)
            else simpleText ("Session has expired")
      Nothing -> do
          writeSession Nothing
          simpleText ("Invalid session")

getPerson :: PersonId -> PidgeonAction Person
getPerson u = do
   mPerson <- runDB $ PSQL.get u
   case mPerson of
       Just p -> return p
       Nothing -> simpleText "user doesn't exist anymore"

killSessions :: PersonId -> PidgeonAction ()
killSessions personId = runDB $ deleteWhere [ SessiePersonId ==. personId ]

insertPerson :: Person -> PidgeonAction (Key Person)
insertPerson p = runDB $ insert p

getSignupRequest :: PidgeonAction SignupRequest
getSignupRequest = do
    ps <- params
    let mSr = SignupRequest <$> lookup "email" ps
                            <*> lookup "password" ps
                            <*> lookup "passwordConfirm" ps
    case mSr of
      Just sr -> return sr
      Nothing -> text ("Oops, something went wrong with your request!")

loginRequest :: PidgeonAction LoginRequest
loginRequest = do
    ps <- params
    let mLR = LoginRequest <$> lookup "email" ps
                           <*> lookup "password" ps
    case mLR of
      Just lr -> return lr
      Nothing -> text ("Oops, something went wrong with your request!")

getPersonFromRequest :: LoginRequest -> PidgeonAction ((Key Person, Person))
getPersonFromRequest lr = do
  mPersonEnt <- runDB $ getBy (UniqueUsername $ T.toLower (lrEmail lr))
  case mPersonEnt of
      Just ent -> do
          liftIO $ pPrint ent
          return (entityKey ent, entityVal ent)
      Nothing -> simpleText ("Invalid email or password")

mkPerson :: SignupRequest -> PidgeonAction Person
mkPerson sr = do
  (salt, hash) <- mkNewSaltAndHash (srPassword sr)
  return $ Person mail (makeHex hash) (makeHex salt) Nothing
  where mail = T.toLower (srEmail sr)

validSignupRequest :: PidgeonAction SignupRequest
validSignupRequest = do
    sr      <- getSignupRequest
    mPerson <- runDB $ getBy (UniqueUsername $ T.toLower (srEmail sr))

    let se = [ if isNothing mPerson
                 then Nothing
                 else Just EmailAddressTaken
             , if isValidEmail (srEmail sr)
                 then Nothing
                 else Just InvalidEmailAddress
             , if validPasswordLength (srPassword sr)
                 then Nothing
                 else Just PasswordTooShort
             , if (srPassword sr) == (srPasswordConfirm sr)
                 then Nothing
                 else Just PasswordsDontMatch
             ]

    if null (catMaybes se)
        then return sr
        else do liftIO $ pPrint se
                lucid (signupPage (Just $ catMaybes se) LoggedOut)

changePasswordRequest ::PidgeonAction (T.Text, T.Text, T.Text)
changePasswordRequest = do
  ps <- params
  let mReq = (,,) <$> lookup "currentpassword" ps
                  <*> lookup "newpassword" ps
                  <*> lookup "newpasswordC" ps
  case mReq of
      Just r -> return r
      Nothing -> text ("Oops, something went wrong with your request!")

checkPassword :: PersonId -> T.Text -> PidgeonAction ()
checkPassword u p = do
    person        <- getPerson u
    let (salt, hash) = (personSalt person, personPassword person)
    if hash == (makeHex $ hashPassword p (decodeHex $ salt))
        then return ()
        else simpleText ("Incorrect current password")

updatePassword :: PersonId -> T.Text -> T.Text -> PidgeonAction ()
updatePassword u p1 p2 = do
  (salt, hash) <- mkNewSaltAndHash p1
  if p1 == p2
      then runDB $ update u [ PersonPassword =. (makeHex hash)
                            , PersonSalt =. (makeHex salt)]
      else simpleText "Passwords don't match"

mkNewSaltAndHash :: T.Text -> PidgeonAction (BS.ByteString, BS.ByteString)
mkNewSaltAndHash password = do
  g <- liftIO $ newStdGen
  let salt = randomBS 16 g
      hash = hashPassword password salt
  return (salt, hash)

showRequest :: PidgeonAction ()
showRequest = do
    r <- request
    p <- params
    liftIO $ do pPrint r
                print p

-- TODO: Move out of Main.hs
getIP4 :: SockAddr -> String
getIP4 ipport = let s = show ipport
               in takeWhile (/= ':') s


---------------------- Lucid stuff -----------------------
-- TODO: move out of Main.hs
lucid :: Html a1 -> PidgeonAction a
lucid = lazyBytes . renderBS

simpleText :: T.Text -> PidgeonAction a
simpleText x = do
        r <- readSession
        case r of
            Nothing -> lucid (simplePage x LoggedOut)  -- TODO: Handle logged out case.
            Just sess -> lucid (simplePage x LoggedIn)
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
