module Persistence.OperationsSpec (spec) where

import Data.Scientific
import Data.Time.Calendar
import Data.UUID
import Test.Hspec

import qualified Data.Map as Map
import qualified Data.Set as Set

import Models.Operation
import Persistence.Operations

import qualified Models.Account as A
import qualified Persistence.Database as DB


spec :: Spec
spec = do
  storeSpec
  getSpec


storeSpec :: Spec
storeSpec = do
  describe "storeOperation" $ do
    context "when there is no other operation stored for that account" $ do
      it "creates the Operation SortedList and inserts the operation" $ do

        let initialDB = DB.initDB
            newDb = storeOperation operationA initialDB
            newOperationsMap = DB.operationsMap newDb
            maybeOperations = Map.lookup accId newOperationsMap

        let expectation = Set.fromList [operationA]

        maybeOperations `shouldBe` (Just expectation)

    context "when there is another operation stored for that account" $ do
      it "inserts the operation to the existing Operation SortedList" $ do
        
        let initialOperationsMap = Map.fromList [(accId, Set.fromList [operationB])]
            initialDB = DB.initDB { DB.operationsMap = initialOperationsMap }

        let newDb = storeOperation operationA initialDB
            newOperationsMap = DB.operationsMap newDb
            maybeOperations = Map.lookup accId newOperationsMap

        let expectation = Set.fromList [operationA, operationB]

        maybeOperations `shouldBe` (Just expectation)


getSpec :: Spec
getSpec = do
  describe "getOperations" $ do
    context "when there are no operations for the account ID" $ do
      it "returns an empty list" $ do
        let db = DB.initDB

        let result = getOperations accId db

        result `shouldBe` []

    context "when there are operations for the account ID" $ do
      it "returns the list with the operations in ascending order" $ do
        
        let existingOps = Set.fromList [operationA, operationB, operationC, operationD]
            opsMap = Map.fromList [(accId, existingOps)]
            db = DB.initDB { DB.operationsMap = opsMap }

        let result = getOperations accId db

        result `shouldBe` [operationC, operationD, operationA, operationB]
        

operationA :: Operation
operationA = let opId = read  "c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: UUID
             in Operation { operationId   = opId
                          , accountId     = accId
                          , operationType = Credit
                          , date          = fromGregorian 2018 7 14
                          , amount        = scientific 1512 (-2)
                          , description   = "Amazon transaction" }

operationB :: Operation
operationB = let opId  = read "6d877c2d-90b6-4d3e-9caa-48f8eea4704e" :: UUID
             in Operation { operationId   = opId
                          , accountId     = accId
                          , operationType = Debit
                          , date          = fromGregorian 2019 10 4
                          , amount        = scientific 200 0
                          , description   = "ATM withdrawal" }

operationC :: Operation
operationC = let opId  = read "abeb4f11-c508-46e9-be57-8f2e3fe6f047" :: UUID
             in Operation { operationId   = opId
                          , accountId     = accId
                          , operationType = Debit
                          , date          = fromGregorian 2017 1 4
                          , amount        = scientific 10 0
                          , description   = "ATM withdrawal" }

operationD :: Operation
operationD = let opId  = read "318cb4d4-7d51-45b2-ac5a-1f7212f35d35" :: UUID
             in Operation { operationId   = opId
                          , accountId     = accId
                          , operationType = Debit
                          , date          = fromGregorian 2018 2 4
                          , amount        = scientific 10 0
                          , description   = "ATM withdrawal" }


accId :: UUID
accId = read "ad5efce7-ae5e-4efd-80d1-fe5eac9c0b54" :: UUID
