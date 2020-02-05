module Models.OperationSpec (spec) where

import Data.Scientific
import Data.Time.Calendar
import Data.UUID
import Test.Hspec

import Models.Operation


spec :: Spec
spec = do
  equalitySpec
  ordSpec
  amountWithSignSpec
  calculateBalanceSpec


equalitySpec :: Spec
equalitySpec = do
  describe "==" $ do
    context "when operations have the same ID" $ do
      it "returns True" $ do
        let operationE = operationB { operationId = (operationId operationA) }

        operationA == operationE `shouldBe` True

    context "when operations have the different ID" $ do
      it "returns False" $ do

        operationA == operationB `shouldBe` False


ordSpec :: Spec
ordSpec = do
  describe "compare" $ do
    context "when operations have the same ID" $ do
      it "returns True" $ do
        let operationE = operationB { operationId = (operationId operationA) }

        operationA `compare` operationE `shouldBe` EQ

    context "when first argument has greater (lexicographically) ID" $ do
      it "returns False" $ do

        operationA `compare` operationB `shouldBe` GT

    context "when first argument has smaller (lexicographically) ID" $ do
      it "returns False" $ do

        operationB `compare` operationA `shouldBe` LT


amountWithSignSpec :: Spec
amountWithSignSpec = do
  describe "amountWithSign" $ do
    context "when the amount is positive" $ do
      context "and the operation type is credit" $ do
        it "returns the amount unchanged" $ do
          let am = scientific 15213 (-2)
              op = operationA { amount = am, operationType = Credit }

          (amountWithSign op) `shouldBe` am

      context "and the operation type is debit" $ do
        it "returns the opposite amount" $ do
          let am = scientific 15213 (-2)
              op = operationA { amount = am, operationType = Debit }

          (amountWithSign op) `shouldBe` (scientific (-15213) (-2))

    context "when the amount is zero" $ do
      context "and the operation type is credit" $ do
        it "returns zero" $ do
          let am = scientific 0 0
              op = operationA { amount = am, operationType = Credit }

          (amountWithSign op) `shouldBe` am

      context "and the operation type is debit" $ do
        it "returns zero" $ do
          let am = scientific 0 0 
              op = operationA { amount = am, operationType = Debit }

          (amountWithSign op) `shouldBe` am


calculateBalanceSpec :: Spec
calculateBalanceSpec = do
  describe "calculateBalance" $ do
    context "when list is empty" $ do
      it "returns zero" $ do
        (calculateBalance []) `shouldBe` (scientific 0 0)

    context "when list contains one credit operation" $ do
      it "returns that operation's amount" $ do
        (calculateBalance [operationA]) `shouldBe` (amount operationA)

    context "when list contains one debit operation" $ do
      it "returns the oposite of that operation's amount" $ do
        (calculateBalance [operationB]) `shouldBe` (-(amount operationB))

    context "when list contains multiple operations" $ do
      it "adds the credits and subtracts the debits" $ do
        let list = [operationA, operationB, operationC]

        -- 15.12 - 200 + 185.4 = 0.52
        (calculateBalance list) `shouldBe` (scientific 52 (-2))
        
operationA :: Operation
operationA = let opId = read  "c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: UUID
                 accId = read "2ad34674-7825-49cc-b985-342eb48285c4" :: UUID
             in Operation { operationId   = opId
                          , accountId     = accId
                          , operationType = Credit
                          , date          = fromGregorian 2019 7 14
                          , amount        = scientific 1512 (-2)
                          , description   = "Amazon transaction" }

operationB :: Operation
operationB = let opId  = read "6d877c2d-90b6-4d3e-9caa-48f8eea4704e" :: UUID
                 accId = read "ad5efce7-ae5e-4efd-80d1-fe5eac9c0b54" :: UUID
             in Operation { operationId   = opId
                          , accountId     = accId
                          , operationType = Debit
                          , date          = fromGregorian 2018 10 4
                          , amount        = scientific 200 0
                          , description   = "ATM withdrawal" }

operationC :: Operation
operationC = operationA { amount = scientific 1854 (-1) }
