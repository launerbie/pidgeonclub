{-# LANGUAGE OverloadedStrings #-}
--ExtendedDefaultRules needed for script_?
{-# LANGUAGE ExtendedDefaultRules #-}
module PidgeonClub.Views where

import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)

import PidgeonClub.Actions
import PidgeonClub.Types

-- TODO: Login dependent menu, i.e. if not logged in, don't show the profile url in nav

data Page = Home
          | About
          | AllUsers
          | Contact
          | Profile
          | Login
          | Signup
          deriving (Eq, Show)

pageTitle_ :: Page -> Html ()
pageTitle_ p = toHtml $ (show p)++" | Pidgeon Club"

basePage :: Page -> Html () -> Html ()
basePage p content=
  html_ [lang_ "en"] $ do
      head_ $ do
          title_ (pageTitle_ p)
          meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
          link_ [rel_ "stylesheet",type_ "text/css",href_ "/css/bootstrap.min.css"]
      body_ $ do
          navigation p
          content
          footer
          scripts

navigation :: Page -> Html ()
navigation p = do
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
             case p of
                 Home   -> li_ [class_ "active"] (a_ [href_ "/"] "Home")
                 _      -> li_ (a_ [href_ "/"] "Home")
             case p of
                 AllUsers  -> li_ [class_ "active"] (a_ [href_ "/"] "AllUsers")
                 _      -> li_ (a_ [href_ "/allusers"] "AllUsers")
             case p of
                 Profile  -> li_ [class_ "active"] (a_ [href_ "/"] "Profile")
                 _      -> li_ (a_ [href_ "/profile"] "Profile")
             case p of
                 Signup -> li_ [class_ "active"] (a_ [href_ "/signup"] "Sign Up")
                 _      -> li_ (a_ [href_ "/signup"]  "Sign Up")
             case p of
                 Login  -> li_ [class_ "active"] (a_ [href_ "/login"] "Login")
                 _      -> li_ (a_ [href_ "/login"]   "Login")

homePage :: Html ()
homePage = basePage Home $ do
  div_ [class_ "container"] $ do
      (h1_ "Het probleem")
      (p_ hetprobleem)

simplePage :: T.Text -> Html ()
simplePage x = basePage Home $ do
  div_ [class_ "container"] $ do
      (p_ $ toHtml x)

loginPage :: Html ()
loginPage = basePage Login suchHorizontalLoginForm

signupPage :: Maybe SignupError -> Html ()
signupPage e = basePage Signup (signupForm e)

contactPage :: Html ()
contactPage = basePage Contact $ do
  div_ [class_ "container"] $ do
     p_ "contact page"

profilePage :: String -> Html ()
profilePage email = basePage Profile $ do
  div_ [class_ "container"] $ do
     p_ $ toHtml email


-- ### Additional Attributes
-- border: A table attribute
border_ :: T.Text -> Attribute
border_ = makeAttribute "border"

cellspacing_ :: T.Text -> Attribute
cellspacing_ = makeAttribute "cellspacing"

cellpadding_ :: T.Text -> Attribute
cellpadding_ = makeAttribute "cellpadding_"

allUsersPage :: [(T.Text, T.Text, T.Text)] -> Html ()
allUsersPage xs = basePage AllUsers $ do
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


hetprobleem = "Na een drukke dag komt u 's avonds thuis, er ligt een briefje op de mat.\n\"Helaas hebben wij u vandaag niet thuis aangetroffen, wij proberen het morgen nogmaals.....\"\nHet pakketje, dat u gisteren via uw favoriete webshop heeft besteld, wordt morgen bezorgd. Maar ook morgen moet u werken en zal de postbode u weer niet thuis aantreffen.....\nU heeft uw pakketje echt nodig, erg onhandig en niet klantvriendelijk!!\n2 De lamp die jullie op het oog hebben, kan alleen via internet besteld worden.\nHelaas zijn jullie deze week alle twee overdag niet thuis.....\nWaar en wanneer moet u uw lamp nu laten bezorgen?\n3 De nieuwe schoenen, die u online heeft besteld, passen niet, u kunt ze kosteloos retourneren. Maar waar vind u de tijd om naar het postkantoor te gaan?\nDit zijn enkele voorbeelden die iedereen herkent. U heeft iets via internet besteld maar u moet zich aanpassen aan de levertijden van de bezorger."

emptyDiv :: String -> Html ()
emptyDiv t = div_ [class_ "container"] (p_ $ toHtml t)

signupForm :: Maybe SignupError -> Html ()
signupForm mErr = do
  div_ [class_ "container"] $ do
     (p_ "Become a pidgeon!")
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
