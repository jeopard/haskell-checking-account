module Models.StatementDateSpec (spec) where

import Data.Scientific
import Data.Time.Calendar
import Data.UUID
import Test.Hspec

import Models.StatementDate

import qualified Models.Operation as O

spec :: Spec
spec = do
  fromOperationSpec
  insertOperationSpec

fromOperationSpec :: Spec
fromOperationSpec = do
  describe "fromOperation" $ do
    context "when it's a credit operation" $ do
      it "creates a StatementDate, adding the op amount" $ do
        let previousBalance = scientific (-1012) (-2) 

        let result = fromOperation operationA previousBalance

        (date result)       `shouldBe` (O.date operationA)
        (operations result) `shouldBe` [operationA]
        -- 15.12 - 10.12 = 5.00
        (endOfDayBalance result) `shouldBe` (scientific 5 0)
        
    context "when it's a debit operation" $ do
      it "creates a StatementDate, subtracting the op amount" $ do
        let previousBalance = scientific (-1012) (-2)

        let result = fromOperation operationB previousBalance

        (date result)       `shouldBe` (O.date operationB)
        (operations result) `shouldBe` [operationB]
        -- -200 - 10.12 = - 210.12
        (endOfDayBalance result) `shouldBe` (scientific (-21012) (-2))
        

insertOperationSpec :: Spec
insertOperationSpec = do
  describe "insertOperation" $ do
    it "returns a StatementDate with the operation appended" $ do
      let result = insertOperation operationB sDate

      (date result)       `shouldBe` (O.date operationA)
      (operations result) `shouldBe` [operationA, operationB]
      -- 15.12 - 200 = -184.88
      (endOfDayBalance result) `shouldBe` (scientific (-18488) (-2))
      


sDate :: StatementDate
sDate = StatementDate (O.date operationA) [operationA] (O.amount operationA)
    
        
operationA :: O.Operation
operationA = let opId = read  "c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: UUID
             in O.Operation { O.operationId   = opId
                            , O.accountId     = accId
                            , O.operationType = O.Credit
                            , O.date          = fromGregorian 2019 7 14
                            , O.amount        = scientific 1512 (-2)
                            , O.description   = "Amazon transaction" }

operationB :: O.Operation
operationB = let opId  = read "6d877c2d-90b6-4d3e-9caa-48f8eea4704e" :: UUID
             in O.Operation { O.operationId   = opId
                            , O.accountId     = accId
                            , O.operationType = O.Debit
                            , O.date          = fromGregorian 2019 7 14
                            , O.amount        = scientific 200 0
                            , O.description   = "ATM withdrawal" }

accId :: UUID
accId = read "2ad34674-7825-49cc-b985-342eb48285c4" :: UUID
