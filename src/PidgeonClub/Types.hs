module PidgeonClub.Types where

import qualified Data.Text as T
import Data.Int

type UserName = T.Text
type Password = T.Text
type Email = T.Text

type SignupInput = (T.Text, T.Text, T.Text)

-------------------------------- Forms -------------------------------------
data SignupError = SignupError
    { usernameError :: [String]
    , passwordError :: [String]
    , passwordErrorConfirm :: [String]
    }
