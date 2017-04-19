{-# LANGUAGE OverloadedStrings #-}
--ExtendedDefaultRules needed for script_?
{-# LANGUAGE ExtendedDefaultRules #-}
module PidgeonClub.Views where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)

import PidgeonClub.Forms
import PidgeonClub.Types
import PidgeonClub.Lorem

data LogStatus = LoggedOut | LoggedIn deriving (Eq,Show)

data NavEntry = NavEntry
  { navHref :: T.Text
  , navText :: T.Text
  } deriving (Eq, Show)

homeNav    = NavEntry "/" "Home"
loginNav   = NavEntry "/login" "Login"
logoutNav  = NavEntry "/logout" "Logout"
profileNav = NavEntry "/profile" "Profile"
signupNav  = NavEntry "/signup" "Signup"

data NavMenu = NavMenu [NavEntry] NavEntry deriving Show

defaultNavMenu :: NavEntry -> NavMenu
defaultNavMenu active = NavMenu [homeNav, signupNav, loginNav] active

userNavMenu :: NavEntry -> NavMenu
userNavMenu active = NavMenu [homeNav, profileNav, logoutNav] active

getNavMenu :: LogStatus -> NavEntry -> NavMenu
getNavMenu s e = if s == LoggedIn
                 then userNavMenu e
                 else defaultNavMenu e

-- ################ Common for all pages #######################
basePage :: NavMenu -> Html () -> Html ()
basePage nm@(NavMenu xs active) content=
  html_ [lang_ "en"] $ do
      head_ $ do
          title_ (toHtml $ (navText active) <> " | Pidgeon Club")
          meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
          link_ [rel_ "stylesheet",type_ "text/css",href_ "/css/bootstrap.min.css"]
      body_ $ do
          navigation nm
          content
          footer
          scripts

navigation :: NavMenu -> Html ()
navigation (NavMenu xs active) = do
  nav_ [class_ "navbar navbar-inverse"] $ do
     div_ [class_ "container"] $ do
        div_ [class_ "navbar-header"] $ do
           button_ [class_ "navbar-toggle collapsed", type_ "button", data_ "toggle" "collapse", data_ "target" "#navbar"] $ do
                 span_ [class_ "sr-only"] "Toggle Navigation"
                 span_ [class_ "icon-bar"] ""
                 span_ [class_ "icon-bar"] ""
                 span_ [class_ "icon-bar"] ""
           a_ [class_ "navbar-brand"] "Pidgeon Club"
        div_ [class_ "collapse navbar-collapse", id_ "navbar"] $ do
           ul_ [class_ "nav navbar-nav"] $ do
             mapM_ (\x -> createNavEntry x active) xs
             where createNavEntry :: NavEntry -> NavEntry -> Html ()
                   createNavEntry e a =
                     if e == a
                     then li_ [class_ "active"] (a_ [href_ (navHref e)] (toHtml $ navText e))
                     else li_ (a_ [href_ (navHref e)] (toHtml $ navText e))


--  ################## Pages #####################################
homePage :: LogStatus -> Html ()
homePage s = basePage (getNavMenu s homeNav) $ do
  div_ [class_ "container"] $ do
      (h1_ "Lorem Ipsum")
      (p_ $ toHtml lorem1)

      (h1_ "Paketus Directus")
      (p_ $ toHtml lorem2)

      (h1_ "Facilus maximus")
      (p_ $ toHtml lorem3)

      (h1_ "Minem fringilla")
      (p_ $ toHtml lorem4)

      (h1_ "Pidgem Pidgus")
      (p_ $ toHtml lorem5)

simplePage :: T.Text -> LogStatus -> Html ()
simplePage x s = basePage (getNavMenu s homeNav) $ do
  div_ [class_ "container"] $ do
      (p_ $ toHtml x)

alreadyLoggedInPage :: Person -> Html ()
alreadyLoggedInPage p = do
  div_ [class_ "container"] $ do
      (p_ $ toHtml $ "Already logged in as: " <> personEmail p)
      (p_ $ toHtml $ "Would you like to log out?")
      a_ [class_ "btn btn-default", href_ "/logout", role_ "button"] "logout"


loginPage :: Maybe Person -> LogStatus -> Html ()
loginPage (Just p) s = basePage (getNavMenu s loginNav) (alreadyLoggedInPage p)
loginPage Nothing s = basePage (getNavMenu s loginNav) suchHorizontalLoginForm

signupPage :: Maybe SignupError -> LogStatus -> Html ()
signupPage e s = basePage (getNavMenu s signupNav) (signupForm e)

profilePage :: Person -> LogStatus -> Html ()
profilePage p s = basePage (getNavMenu s profileNav) $ do
  div_ [class_ "container"] $ do
     p_ $ toHtml $ personEmail p
     p_ $ toHtml $ personPassword p
     p_ $ toHtml $ personSalt p

userPage :: String -> LogStatus -> Html ()
userPage email s = basePage (getNavMenu s profileNav) $ do
  div_ [class_ "container"] $ do
     p_ $ toHtml email

allUsersPage :: [(T.Text, T.Text, T.Text)] -> LogStatus -> Html ()
allUsersPage xs s = basePage (getNavMenu s homeNav) $ do
  div_ [class_ "container"] $ do
     table_ [class_ "table table-bordered"] $ do
          tr_ $ do
            th_ "Email"
            th_ "Salted SHA256"
            th_ "Salt"
          mapM_ makeRow3 xs

-- Generalize to makeRow :: nrRows -> [T.Text] -> Html () ?
makeRow3 :: (T.Text, T.Text, T.Text) -> Html ()
makeRow3 (a,b,c) = tr_ $ do
                  td_ (toHtml a)
                  td_ (toHtml b)
                  td_ (toHtml c)

emptyDiv :: String -> Html ()
emptyDiv t = div_ [class_ "container"] (p_ $ toHtml t)

-- ######################## Forms #################################
signupForm :: Maybe SignupError -> Html ()
signupForm mErr = do
  div_ [class_ "container"] $ do
     form_ [class_ "form-horizontal", method_ "post", action_ "/signup"] $ do
         div_ [class_ "form-group"] $ do
            label_ [for_ "email", class_ "col-sm-2 control-label"] "email: "
            div_ [class_ "col-sm-4"] $ do
               input_ [type_ "email", class_ "form-control", name_ "email", placeholder_ "pieterpost@mail.com"]
               {- formErrors -}
               case mErr of
                   Just e -> alert $ usernameError e
                   Nothing -> mempty
         div_ [class_ "form-group"] $ do
            label_ [for_ "password", class_ "col-sm-2 control-label"] "password: "
            div_ [class_ "col-sm-4"]$ do
               input_ [type_ "password",  class_ "form-control", name_ "password", placeholder_ "supersecret123"]
               {- formErrors -}
               case mErr of
                   Just e -> alert $ passwordError e
                   Nothing -> mempty
         div_ [class_ "form-group"] $ do
            label_ [for_ "passwordConfirm", class_ "col-sm-2 control-label"] "confirm password: "
            div_ [class_ "col-sm-4"]$ do
               input_ [type_ "password",  class_ "form-control", name_ "passwordConfirm", placeholder_ "supersecret123"]
               {- formErrors -}
               case mErr of
                   Just e -> alert $ passwordErrorConfirm e
                   Nothing -> mempty
         div_ [class_ "form-group"] $ do
            div_ [class_ "col-sm-offset-2 col-sm-4"] $ do
               button_ [type_ "submit", class_ "btn btn-default"] "Register"
  div_ [class_ "container"] $ do
     a_ [href_ "/reset" ] "Forgot your password?"

suchHorizontalLoginForm :: Html ()
suchHorizontalLoginForm = do
  div_ [class_ "container"] $ do
     form_ [class_ "form-horizontal", method_ "post", action_ "/login"] $ do
         div_ [class_ "form-group"] $ do
            label_ [for_ "inputEmail", class_ "col-sm-2 control-label"] "email: "
            div_ [class_ "col-sm-4"]$ do
               input_ [type_ "email", class_ "form-control", name_ "email", placeholder_ ""]
         div_ [class_ "form-group"] $ do
            label_ [for_ "password", class_ "col-sm-2 control-label"] "password: "
            div_ [class_ "col-sm-4"]$ do
               input_ [type_ "password",  class_ "form-control", name_ "password", placeholder_ ""]
         div_ [class_ "form-group"] $ do
            div_ [class_ "col-sm-offset-2 col-sm-4"] $ do
               button_ [type_ "submit", class_ "btn btn-default"] "Sign in"
  div_ [class_ "container"] $ do
     p_ $ do "Don't have an account? "
             a_ [href_ "/signup" ] "Become a pidgeon!"

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


alert :: [String] -> Html ()
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

