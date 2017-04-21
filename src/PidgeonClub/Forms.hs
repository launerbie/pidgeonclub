module PidgeonClub.Forms where

import qualified Data.Text as T

-------------------------------- Forms -------------------------------------
data SignupError = SignupError
    { usernameError :: [String]
    , passwordError :: [String]
    , passwordErrorConfirm :: [String]
    }

--data SignupRequest = SignupRequest T.Text T.Text T.Text deriving (Eq, Show)
data SignupRequest = SignupRequest
    { srEmail :: T.Text
    , srPassword :: T.Text
    , srPasswordConfirm :: T.Text
    } deriving (Eq, Show)

------------------- Validation -----------------------------
validUsername :: T.Text -> Bool
validUsername = T.all validChar

validPassword :: T.Text -> Bool
validPassword = (>7) . T.length

validEmail :: T.Text -> Bool
validEmail = undefined

validChar :: Char -> Bool
validChar x = x `elem` ['-', '_']++['a'..'z']++['A'..'Z']++['0'..'9']

