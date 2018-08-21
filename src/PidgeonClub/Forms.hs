module PidgeonClub.Forms where

import qualified Data.Text as T

--TODO, validate with isValid :: Bytestring -> Bool
import Text.Email.Validate (isValid)
import Data.Text.Encoding (encodeUtf8)

-------------------------------- Forms -------------------------------------
data NewPidgeonFormError = InvalidSome
                         | InvalidThing
                         deriving (Show)

data SignupFormError = EmailAddressTaken
                     | InvalidEmailAddress
                     | PasswordTooShort
                     | PasswordsDontMatch

instance Show SignupFormError where
  show EmailAddressTaken = "Email address is already taken"
  show InvalidEmailAddress = "Email address is not valid"
  show PasswordTooShort = "Password is too short"
  show PasswordsDontMatch = "Passwords do not match"

data PidgeonError = MissingParams deriving (Eq, Show)

data SignupRequest = SignupRequest
    { srEmail :: T.Text
    , srPassword :: T.Text
    , srPasswordConfirm :: T.Text
    } deriving (Eq, Show)

data LoginRequest = LoginRequest
    { lrEmail :: T.Text
    , lrPassword :: T.Text
    } deriving (Eq, Show)

------------------- Validation -----------------------------

validUsername :: T.Text -> Bool
validUsername = T.all validChar

validPassword :: T.Text -> Bool
validPassword t = and $ map (\v -> v t) [validPasswordLength]

validPasswordLength :: T.Text -> Bool
validPasswordLength = (>7) . T.length

isValidEmail :: T.Text -> Bool
isValidEmail t = case T.find (=='@') t of
                Just _ -> True
                Nothing -> False

validChar :: Char -> Bool
validChar x = x `elem` ['-', '_']++['a'..'z']++['A'..'Z']++['0'..'9']

