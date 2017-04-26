module PidgeonClub.Forms where

import qualified Data.Text as T

--TODO, validate with isValid :: Bytestring -> Bool
import Text.Email.Validate (isValid)
import Data.Text.Encoding (encodeUtf8)

-------------------------------- Forms -------------------------------------
data SignupFormError = EmailAddressExists
                     | InvalidEmailAddress
                     | PasswordTooShort
                     | PasswordsDontMatch
                     deriving (Eq, Show)

data PidgeonError = MissingParams deriving (Eq, Show)

data SignupError = SignupError
    { usernameError :: [String]
    , passwordError :: [String]
    , passwordErrorConfirm :: [String]
    } deriving (Eq, Show)

data SignupRequest = SignupRequest
    { srEmail :: T.Text
    , srPassword :: T.Text
    , srPasswordConfirm :: T.Text
    } deriving (Eq, Show)

------------------- Validation -----------------------------
type ErrorMsg = String

maybeNoErrors :: SignupError -> Maybe SignupError
maybeNoErrors se = let x = length (usernameError se)
                       y = length (passwordError se)
                       z = length (passwordErrorConfirm se)
                   in case x+y+z of
                          0 -> Nothing
                          _ -> Just se

check :: (T.Text -> Bool) -> T.Text -> ErrorMsg -> Maybe ErrorMsg
check f t m = if f t
              then Nothing
              else Just m

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

