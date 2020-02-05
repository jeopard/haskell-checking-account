module Models.Account (
  Account (..)
  ) where

import Data.UUID

-- represents a bank account
data Account = Account { accountId  :: UUID
                       , owner      :: String
                       } deriving (Show)

instance Eq Account where
  x == y = accountId x == accountId y

