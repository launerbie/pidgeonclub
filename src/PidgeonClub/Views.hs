{-# LANGUAGE OverloadedStrings #-}
--ExtendedDefaultRules needed for script_?
{-# LANGUAGE ExtendedDefaultRules #-}
module PidgeonClub.Views where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)

import PidgeonClub.Forms
import PidgeonClub.Types
import PidgeonClub.Lorem

import Web.Spock ( get, post, HasSpock, lazyBytes, middleware
                 , redirect, runSpock, spockAsApp, spock, SpockCtxM, SpockConn, ActionCtxT
                 , SpockActionCtx, root, runQuery, text, var
                 , (<//>) )

import Web.Spock.Config  ( defaultSpockCfg, PoolOrConn (PCNoDatabase, PCPool)
                         , SpockCfg )
import Web.Spock.Action  ( request, params, param, param')
import Web.Spock.SessionActions (getSessionId, readSession, writeSession)



data LogStatus = LoggedOut | LoggedIn deriving (Eq,Show)

data SettingsPage = SettingsAccount
                  | SettingsProfile
                  | SettingsSecurity
                  | SettingsLoginHistory
                  deriving (Show)

data NavEntry = NavEntry
  { navHref :: T.Text
  , navText :: T.Text
  } deriving (Eq, Show)

--type HtmlHandler input = ReaderT input PidgeonAction (Html ())
type Handler = HtmlT PidgeonAction ()

homeNav      = NavEntry "/" "Home"
loginNav     = NavEntry "/login" "Login"
logoutNav    = NavEntry "/logout" "Logout"
settingsNav  = NavEntry "/settings" "Settings"
signupNav    = NavEntry "/signup" "Signup"
loginHistNav = NavEntry "/loginhistory" "Login History"
allUsersNav  = NavEntry "/allusers" "Users"
pidgeonsNav  = NavEntry "/pidgeons" "Pidgeons"
newPidgeonNav  = NavEntry "/newpidgeon" "Add a pidgeon"
testNav      = NavEntry "/test" "Test page"

data NavMenu = NavMenu [NavEntry] NavEntry deriving Show

getNavMenu :: NavEntry -> PidgeonAction NavMenu
getNavMenu active = do
  s      <- logState
  if s == LoggedIn
  then return $ NavMenu [ homeNav
               , newPidgeonNav
               , pidgeonsNav
               , allUsersNav
               , loginHistNav
               , settingsNav
               , logoutNav
               , testNav] active
  else return $ NavMenu [ homeNav, signupNav, loginNav] active

-- ################ Common for all pages #######################
basePage :: NavEntry -> Handler -> HtmlT PidgeonAction ()
basePage active content =
  html_ [lang_ "en"] $ do
      head_ $ do
          title_ (toHtml $ navText active <> " | Pidgeon Club")
          meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
          link_ [rel_ "stylesheet",type_ "text/css",href_ "/css/bootstrap.min.css"]
      body_ $ do
          navigation active
          content
          footer2
          scripts2

--navigation :: NavMenu -> Html ()
navigation :: NavEntry -> HtmlT PidgeonAction ()
navigation active = do
  (NavMenu xs active) <- lift $ getNavMenu active
  nav_ [class_ "navbar navbar-inverse"] $ do
     div_ [class_ "container-fluid"] $ do
        div_ [class_ "navbar-header"] $ do
           button_ [class_ "navbar-toggle collapsed", type_ "button", data_ "toggle" "collapse", data_ "target" "#navbar"] $ do
                 span_ [class_ "sr-only"] "Toggle Navigation"
                 span_ [class_ "icon-bar"] ""
                 span_ [class_ "icon-bar"] ""
                 span_ [class_ "icon-bar"] ""
           a_ [class_ "navbar-brand"] "Pidgeon Club"
        div_ [class_ "collapse navbar-collapse", id_ "navbar"] $ do
           ul_ [class_ "nav navbar-nav"] $ do
             mapM_ (`createNavEntry` active) xs
             where createNavEntry :: NavEntry -> NavEntry -> HtmlT PidgeonAction ()
                   createNavEntry e a =
                     if e == a
                     then li_ [class_ "active"] (a_ [href_ (navHref e)] (toHtml $ navText e))
                     else li_ (a_ [href_ (navHref e)] (toHtml $ navText e))

logState :: PidgeonAction LogStatus
logState = do
  r <- readSession
  case r of
      Just _ -> return LoggedIn
      Nothing -> return LoggedOut

testPage :: Handler
testPage = do
    basePage testNav $ do
      div_ [class_ "container"] $ do
          p_ $ toHtml "test"

--  ################## Pages #####################################
homePage :: Handler
homePage = do
    basePage homeNav $ do
      div_ [class_ "container"] $ do
          h1_ "Lorem Ipsum"
          p_ $ toHtml lorem1

          h1_ "Paketus Directus"
          p_ $ toHtml lorem2

          h1_ "Facilus maximus"
          p_ $ toHtml lorem3

          h1_ "Minem fringilla"
          p_ $ toHtml lorem4

          h1_ "Pidgem Pidgus"
          p_ $ toHtml lorem5

simplePage :: T.Text -> Handler
simplePage x = basePage homeNav $ do
  div_ [class_ "container"] $ do
      (p_ $ toHtml x)

alreadyLoggedInPage :: Person -> Handler
alreadyLoggedInPage p = do
  div_ [class_ "container"] $ do
     div_ [class_ "panel panel-default"] $ do
        div_ [class_ "panel-heading"] "Log in to the Pidgeon Club"
        div_ [class_ "panel-body"] $ do
           p_ $ toHtml $ "Already logged in as: " <> personEmail p
           p_ $ toHtml "Would you like to log out?"
           a_ [class_ "btn btn-success", href_ "/logout", role_ "button"] "Log out"

loginPage :: Maybe Person -> Handler
loginPage (Just p) = basePage loginNav (alreadyLoggedInPage p)
loginPage Nothing = basePage loginNav loginForm

resetPage :: Handler
resetPage = basePage homeNav resetPassForm

signupPage :: Maybe [SignupFormError] -> Handler
signupPage mErr = do
  basePage signupNav (signupForm mErr)

signupSuccessPage :: T.Text -> Handler
signupSuccessPage email = do
    basePage signupNav $ do
      div_ [class_ "container"] $ do
         p_ $ toHtml $ "An activation mail has been sent to: " <> email
         p_ $ a_ [href_ "/"] "Click here to go back"

settingsPage :: Person -> SettingsPage -> Handler
settingsPage p settings = basePage settingsNav $ do
  div_ [class_ "container-fluid"] $ do
     div_ [class_ "row"] $ do
        div_ [class_ "col-sm-2"] $ do
           div_ [class_ "panel panel-default"] $ do
              div_ [class_ "panel-heading"] "Settings"
              div_ [class_ "panel-body"] $ a_ [href_ "/settings/profile"] "Profile"
              div_ [class_ "panel-body"] $ a_ [href_ "/settings/account"] "Account"
              div_ [class_ "panel-body"] $ a_ [href_ "/settings/security"] "Security"
        div_ [class_ "col-sm-6"] $ do
           case settings of
               SettingsProfile  -> settingsProfilePage p
               SettingsAccount  -> settingsAccountPage p
               SettingsSecurity -> settingsSecurityPage p

settingsProfilePage :: Person -> Handler
settingsProfilePage p = do
     p_ $ toHtml $ personEmail p
     p_ $ toHtml $ personPassword p
     p_ $ toHtml $ personSalt p

settingsAccountPage :: Person -> Handler
settingsAccountPage p = do
    h3_ "Change Password"
    hr_ []
    div_ [class_ "container"] $ do
       form_ [class_ "form-horizontal", method_ "post", action_ "/settings/account"] $ do
           div_ [class_ "form-group"] $ do
              label_ [for_ "currentpassword", class_ "control-label"] "Current password: "
              input_ [type_ "password", class_ "form-control", name_ "currentpassword"]
           div_ [class_ "form-group"] $ do
              label_ [for_ "newpassword", class_ "control-label"] "New password: "
              input_ [type_ "password", class_ "form-control", name_ "newpassword"]
           div_ [class_ "form-group"] $ do
              label_ [for_ "newpasswordC", class_ "control-label"] "Confirm new password: "
              input_ [type_ "password", class_ "form-control", name_ "newpasswordC"]
           div_ [class_ "form-group"] $ do
              button_ [type_ "submit", class_ "btn btn-success"] "Change password"

settingsSecurityPage :: Person -> Handler
settingsSecurityPage p = undefined

loginHistoryPage :: [Login] -> Handler
loginHistoryPage ls = basePage loginHistNav $ do
  let loginToRow l = (T.pack . show $ loginTime l, loginIpaddress l)
  div_ [class_ "container"] $ do
     table_ [class_ "table table-bordered"] $ do
          tr_ $ do
            th_ "Time"
            th_ "IP Address"
          mapM_ (makeRow2 . loginToRow) ls

userPage :: String -> Handler
userPage email = basePage homeNav $ do
  div_ [class_ "container"] $ do
     p_ $ toHtml email

allUsersPage :: [Person] -> Handler
allUsersPage xs = basePage allUsersNav $ do
  let personToRow p = (personEmail p, personPassword p, personSalt p)
  div_ [class_ "container"] $ do
     table_ [class_ "table table-bordered"] $ do
          tr_ $ do
            th_ "Email"
            th_ "Salted SHA256"
            th_ "Salt"
          mapM_ (makeRow3 . personToRow) xs

-- ######################## Forms #################################
signupForm :: Maybe [SignupFormError] -> Handler
signupForm mErr = do
  div_ [class_ "container"] $ do
     div_ [class_ "panel panel-default"] $ do
        div_ [class_ "panel-heading"] "Become a Pidgeon Club member!"
        div_ [class_ "panel-body"] $ do
           form_ [class_ "form-horizontal", method_ "post", action_ "/signup"] $ do
               div_ [class_ "form-group"] $ do
                  label_ [for_ "email", class_ "col-sm-2 control-label"] "email: "
                  div_ [class_ "col-sm-4"] $ do
                     input_ [type_ "email", class_ "form-control", name_ "email", placeholder_ "pieterpost@mail.com"]
               div_ [class_ "form-group"] $ do
                  label_ [for_ "password", class_ "col-sm-2 control-label"] "password: "
                  div_ [class_ "col-sm-4"]$ do
                     input_ [type_ "password",  class_ "form-control", name_ "password", placeholder_ "supersecret123"]
               div_ [class_ "form-group"] $ do
                  label_ [for_ "passwordConfirm", class_ "col-sm-2 control-label"] "confirm password: "
                  div_ [class_ "col-sm-4"]$ do
                     input_ [type_ "password",  class_ "form-control", name_ "passwordConfirm", placeholder_ "supersecret123"]
               div_ [class_ "form-group"] $ do
                  div_ [class_ "col-sm-offset-2 col-sm-4"] $ do
                     button_ [type_ "submit", class_ "btn btn-success"] "Register"
                     -- Form errors
                     case mErr of
                         Just e -> alert $ map show e
                         Nothing -> mempty
           p_ $ do "Already a member? "
                   a_ [href_ "/login" ] "Click here to login"

loginForm :: Handler
loginForm = do
  div_ [class_ "container"] $ do
     div_ [class_ "panel panel-default"] $ do
        div_ [class_ "panel-heading"] "Log in to the Pidgeon Club"
        div_ [class_ "panel-body"] $ do
           form_ [class_ "form-horizontal", method_ "post", action_ "/login"] $ do
               div_ [class_ "form-group"] $ do
                  label_ [for_ "inputEmail", class_ "col-sm-2 control-label"] "email: "
                  div_ [class_ "col-sm-4"]$ do
                     input_ [type_ "email", class_ "form-control", name_ "email", placeholder_ ""]
               div_ [class_ "form-group"] $ do
                  label_ [for_ "password", class_ "col-sm-2 control-label"] "password: "
                  div_ [class_ "col-sm-4"]$ do
                     input_ [type_ "password",  class_ "form-control", name_ "password", placeholder_ ""]
                     a_ [href_ "/reset" ] "Forgot your password?"
               div_ [class_ "form-group"] $ do
                  div_ [class_ "col-sm-offset-2 col-sm-4"] $ do
                     button_ [type_ "submit", class_ "btn btn-success"] "Sign in"
           div_ [class_ "container"] $ do
              p_ $ do "Don't have an account? "
                      a_ [href_ "/signup" ] "Click here to become a pidgeon!"

addNewPidgeonForm :: Maybe [NewPidgeonFormError] -> Handler
addNewPidgeonForm mErr = do
  div_ [class_ "container"] $ do
     div_ [class_ "panel panel-default"] $ do
        div_ [class_ "panel-heading"] "Add new Pidgeon!"
        div_ [class_ "panel-body"] $ do
           form_ [class_ "form-horizontal", method_ "post", action_ "/newpidgeon"] $ do
               div_ [class_ "form-group"] $ do
                  label_ [for_ "name", class_ "col-sm-2 control-label"] "Name: "
                  div_ [class_ "col-sm-4"] $ do
                     input_ [type_ "text", class_ "form-control", name_ "name"]
               div_ [class_ "form-group"] $ do
                  label_ [for_ "height", class_ "col-sm-2 control-label"] "Height: "
                  div_ [class_ "col-sm-4"] $ do
                     input_ [type_ "text", class_ "form-control", name_ "height"]
               div_ [class_ "form-group"] $ do
                  label_ [for_ "color", class_ "col-sm-2 control-label"] "Color: "
                  div_ [class_ "col-sm-4"] $ do
                     input_ [type_ "text", class_ "form-control", name_ "color"]
               div_ [class_ "form-group"] $ do
                  label_ [for_ "", class_ "col-sm-2 control-label"] "Weight: "
                  div_ [class_ "col-sm-4"] $ do
                     input_ [type_ "text", class_ "form-control", name_ "weight"]
               div_ [class_ "form-group"] $ do
                  div_ [class_ "col-sm-offset-2 col-sm-4"] $ do
                     button_ [type_ "submit", class_ "btn btn-success"] "Register pidgeon"
                     -- Form errors
                     case mErr of
                         Just e -> alert $ map show e
                         Nothing -> mempty

resetPassForm :: Handler
resetPassForm = do
  div_ [class_ "container"] $ do
     div_ [class_ "panel panel-default"] $ do
        div_ [class_ "panel-heading"] "Reset your password"
        div_ [class_ "panel-body"] $ do
           p_ "Forgot your password? Enter your email address below and request a password reset mail"
           form_ [class_ "form-horizontal", method_ "post", action_ "/reset"] $ do
               div_ [class_ "form-group"] $ do
                  label_ [for_ "inputEmail", class_ "col-sm-2 control-label"] "email: "
                  div_ [class_ "col-sm-4"]$ do
                     input_ [type_ "email", class_ "form-control", name_ "email", placeholder_ ""]
               div_ [class_ "form-group"] $ do
                  div_ [class_ "col-sm-offset-2 col-sm-4"] $ do
                     button_ [type_ "submit", class_ "btn btn-success"] "Send password reset mail"

addNewPidgeonPage :: Maybe [NewPidgeonFormError] -> Handler
addNewPidgeonPage mErr = basePage newPidgeonNav (addNewPidgeonForm mErr)

footer :: Html ()
footer = do
  footer_ [style_ "background-color: gray"] $ do
        div_ [class_ "container", style_ "color: white"] $ do
           hr_ []
           p_ "Space reserved for footer"

scripts :: Html ()
scripts = do
  script_ [src_ "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"] ""
  script_ [src_ "/js/bootstrap.min.js"] ""

footer2 :: Handler
footer2 = do
  footer_ [style_ "background-color: gray"] $ do
        div_ [class_ "container", style_ "color: white"] $ do
           hr_ []
           p_ "Space reserved for footer"

scripts2 :: Handler
scripts2 = do
  script_ [src_ "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"] ""
  script_ [src_ "/js/bootstrap.min.js"] ""

alert :: [String] -> Handler
alert [] = mempty
alert xs = div_ [class_ "alert alert-danger alert-dismissable"] $ do
              ul_ $ do
                mapM_ (li_ . toHtml) xs

-- ### Additional Attributes
-- border: A table attribute
border_ :: T.Text -> Attribute
border_ = makeAttribute "border"

cellspacing_ :: T.Text -> Attribute
cellspacing_ = makeAttribute "cellspacing"

cellpadding_ :: T.Text -> Attribute
cellpadding_ = makeAttribute "cellpadding_"

-- Generalize to makeRow :: nrRows -> [T.Text] -> Handler ?
makeRow3 :: (T.Text, T.Text, T.Text) -> Handler
makeRow3 (a,b,c) = tr_ $ do
                  td_ (toHtml a)
                  td_ (toHtml b)
                  td_ (toHtml c)

makeRow2 :: (T.Text, T.Text) -> Handler
makeRow2 (a,b) = tr_ $ do
                  td_ (toHtml a)
                  td_ (toHtml b)

