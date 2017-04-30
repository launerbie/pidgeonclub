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

----------------- Spock -----------------------
import Web.Spock ( get, post, HasSpock, lazyBytes, middleware
                 , redirect, runSpock, spock, SpockCtxM, SpockConn, ActionCtxT
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
        sr          <- getSignupRequest
        validatedSR <- validateSignupRequest sr
        p           <- mkPerson validatedSR
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
    get "/profile" $ requireUser $ \u -> do
        liftIO $ pPrint u
        mPerson <- runDB $ PSQL.get u
        case mPerson of
            Just p -> lucid $ profilePage p LoggedIn
            Nothing -> simpleText "user doesn't exist anymore"

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
        (salt, hash)       <- return (personSalt person, personPassword person)
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
  validTil <- liftIO $ liftM (addUTCTime 3600) getCurrentTime
  sid      <- runDB $ do deleteWhere [ SessiePersonId ==. personId ]
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
  liftIO $ pPrint mSid
  case mSid of
      Just sess -> do
          liftIO $ pPrint sess
          now <- liftIO getCurrentTime
          if sessieValidUntil sess > now
            then return $ Just (sessiePersonId sess)
            else simpleText ("Session has expired")
      Nothing -> do
          writeSession Nothing
          simpleText ("Invalid session")

killSessions :: PersonId -> PidgeonAction ()
killSessions personId = runDB $ deleteWhere [ SessiePersonId ==. personId ]

insertPerson :: Person -> PidgeonAction (Key Person)
insertPerson p = runDB $ insert p

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

getSignupRequest :: PidgeonAction SignupRequest
getSignupRequest = do
    mEmail <- param "email"
    mPassword <- param "password"
    mPasswordConf <- param "passwordConfirm"
    let mSr = SignupRequest <$> mEmail <*> mPassword <*> mPasswordConf
    case mSr of
      Just sr -> return sr
      Nothing -> text ("Oops, something went wrong with your request!")

loginRequest :: PidgeonAction LoginRequest
loginRequest = do
    mEmail <- param "email"
    mPassword <- param "password"
    let lr = LoginRequest <$> mEmail <*> mPassword
    case lr of
      Just r -> return r
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
  g <- liftIO $ newStdGen
  let salt = randomBS 16 g
      hash = hashPassword (srPassword sr) salt
      mail = T.toLower (srEmail sr)
  return $ Person mail (makeHex hash) (makeHex salt) Nothing

validateSignupRequest :: SignupRequest -> PidgeonAction SignupRequest
validateSignupRequest sr@(SignupRequest email pass1 pass2) = do
    mPerson <- runDB $ getBy (UniqueUsername $ T.toLower email)
    let passwordsMatch = if pass1 == pass2
                         then Nothing
                         else Just "passwords do not match"
        emailAddressTaken = case mPerson of
                                Just p ->  Just "email address already taken"
                                Nothing -> Nothing
        se = SignupError { usernameError = catMaybes [ check isValidEmail email "not valid email"
                                                     , emailAddressTaken ]
                         , passwordError = catMaybes [check validPasswordLength pass1 "Password is too short"]
                         , passwordErrorConfirm = catMaybes [passwordsMatch]
                         }

    case maybeNoErrors se of
      Nothing      -> return sr
      Just serrors -> do liftIO $ pPrint se
                         lucid (signupPage (Just serrors) LoggedOut)



---------------------- Lucid stuff -----------------------
-- TODO: move out of Main.hs
lucid :: MonadIO m => Html a1 -> ActionCtxT ctx m a
lucid = lazyBytes . renderBS

simpleText :: MonadIO m => T.Text -> ActionCtxT ctx m a
simpleText x = lucid (simplePage x LoggedOut)
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
