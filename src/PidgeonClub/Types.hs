module PidgeonClub.Types where

import qualified Data.Text as T

type SignupInput = (T.Text, T.Text, T.Text)

-------------------------------- Forms -------------------------------------
data SignupError = SignupError
    { usernameError :: [String]
    , passwordError :: [String]
    , passwordErrorConfirm :: [String]
    }
