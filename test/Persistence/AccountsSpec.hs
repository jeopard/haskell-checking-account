module Persistence.AccountsSpec (spec) where

import qualified Data.Map as Map
import Data.UUID
import Test.Hspec

import Models.Account
import Persistence.Accounts

import qualified Persistence.Database as DB


spec :: Spec
spec = do
  storeSpec
  getSpec


storeSpec :: Spec
storeSpec = do
  describe "storeAccount" $ do
    it "should put the Account in the Database" $ do
      let newId = read "58dcefed-c42f-4ff4-8f05-6b277a09ae8c" :: UUID
          newAccount = Account { accountId = newId, owner = "Kostis" }

      let newDb = storeAccount newAccount existingDb
          newAccountsMap = DB.accountsMap newDb

      (Map.lookup newId newAccountsMap) `shouldBe` (Just newAccount)

      let result = getAccount existingId existingDb
      result `shouldBe` result

getSpec :: Spec
getSpec = do
  describe "getAccount" $ do
    context "when account with given ID exists in the DB" $ do
      it "returns Just that account" $ do
        let result = getAccount existingId existingDb

        result `shouldBe` (Just existingAccount)
      
    context "when account with given ID does not exist in the DB" $ do
      it "returns Nothing" $ do
        let anotherId = read "3b09e842-ef74-4c30-be49-4984a6691584" :: UUID

        let result = getAccount anotherId existingDb
      
        result `shouldBe` Nothing

-- helpers to create a DB with an existing account
existingDb :: DB.Database
existingDb = let accMap = Map.insert existingId existingAccount Map.empty
             in DB.Database { DB.accountsMap = accMap, DB.operationsMap = Map.empty }

existingAccount :: Account
existingAccount = Account { accountId = existingId, owner = "John" }

existingId :: UUID
existingId = read "c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: UUID
