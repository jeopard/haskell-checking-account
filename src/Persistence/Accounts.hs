module Persistence.Accounts (
  storeAccount,
  getAccount
  ) where

import Data.UUID

import qualified Data.Map as Map

import Models.Account

import qualified Persistence.Database as DB


-- stores an account to the DB
storeAccount :: Account -> DB.Database -> DB.Database
storeAccount newAccount db = let oldMap = DB.accountsMap db
                                 newAccountId = accountId newAccount
                                 newMap = Map.insert newAccountId newAccount oldMap
                             in db { DB.accountsMap = newMap }


-- gets an account from the DB
getAccount :: UUID -> DB.Database -> Maybe Account
getAccount accId db = let accMap = DB.accountsMap db
                      in Map.lookup accId accMap
