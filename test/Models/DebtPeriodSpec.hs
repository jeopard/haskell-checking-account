module Models.DebtPeriodSpec (spec) where

import Data.Scientific
import Data.Time.Calendar
import Data.UUID
import Test.Hspec

import Models.DebtPeriod

import qualified Models.Operation as O

spec :: Spec
spec = do
  fromOperationsSpec

fromOperationsSpec :: Spec
fromOperationsSpec = do
  describe "fromOperations" $ do
    context "when multiple operations on the same dates" $ do
      it "returns the correct periods" $ do
        let ops = [opA, opB, opC, opD, opE, opF, opG, opH]

        let expectedFirstPeriod = DebtPeriod (fromGregorian 2019 7 16) (Just $ fromGregorian 2019 7 19) (scientific 255 (-1))
            expectedSecondPeriod = DebtPeriod (fromGregorian 2019 7 20) (Just $ fromGregorian 2019 7 22) (scientific 475 (-1))
            expectedThirdPeriod = DebtPeriod (fromGregorian 2019 7 28) (Just $ fromGregorian 2019 7 30) (scientific 43 0)

        (fromOperations ops) `shouldBe` [expectedFirstPeriod, expectedSecondPeriod,
                                         expectedThirdPeriod]

    context "when there are periods with zero balance" $ do
      it "does not include them" $ do
        let newOp = op { O.operationType = O.Credit, O.amount = scientific 43 0, O.date = fromGregorian 2019 7 28 }
            ops = [opA, opB, opC, opD, opE, opF, opG, newOp, opH]

        let result = fromOperations ops

        let expectedFirstPeriod = DebtPeriod (fromGregorian 2019 7 16) (Just $ fromGregorian 2019 7 19) (scientific 255 (-1))
            expectedSecondPeriod = DebtPeriod (fromGregorian 2019 7 20) (Just $ fromGregorian 2019 7 22) (scientific 475 (-1))

        (fromOperations ops) `shouldBe` [expectedFirstPeriod, expectedSecondPeriod]

    context "when operations start with Debit" $ do
      it "returns the first period starting on the date the first operation" $ do
        let newOp = op { O.operationType = O.Debit, O.amount = scientific 1 0, O.date = fromGregorian 2019 7 12 }
            ops = [newOp, opA, opB, opC, opD, opE, opF, opG, opH]

        let expectedFirstPeriod = DebtPeriod (fromGregorian 2019 7 12) (Just $ fromGregorian 2019 7 13) (scientific 1 0)
            expectedSecondPeriod = DebtPeriod (fromGregorian 2019 7 16) (Just $ fromGregorian 2019 7 19) (scientific 265 (-1))
            expectedThirdPeriod = DebtPeriod (fromGregorian 2019 7 20) (Just $ fromGregorian 2019 7 22) (scientific 485 (-1))
            expectedFourthPeriod = DebtPeriod (fromGregorian 2019 7 28) (Just $ fromGregorian 2019 7 30) (scientific 44 0)

        (fromOperations ops) `shouldBe` [expectedFirstPeriod, expectedSecondPeriod,
                                         expectedThirdPeriod, expectedFourthPeriod]

    context "when operations end with negative balance" $ do
      it "returns the last period ending on Nothing" $ do
        let ops = [opA, opB, opC, opD, opE, opF, opG] -- missing H

        let expectedFirstPeriod = DebtPeriod (fromGregorian 2019 7 16) (Just $ fromGregorian 2019 7 19) (scientific 255 (-1))
            expectedSecondPeriod = DebtPeriod (fromGregorian 2019 7 20) (Just $ fromGregorian 2019 7 22) (scientific 475 (-1))
            expectedThirdPeriod = DebtPeriod (fromGregorian 2019 7 28) Nothing (scientific 43 0)

        (fromOperations ops) `shouldBe` [expectedFirstPeriod, expectedSecondPeriod, expectedThirdPeriod]

    context "when only debt periods" $ do
      it "returns debt periods for all the operations" $ do
        -- big debit to make all next balances negative
        let newOp = op { O.operationType = O.Debit, O.amount = scientific 1000 0, O.date = fromGregorian 2019 7 12 }
        let ops = [newOp, opA, opB, opC]

        let expectedFirstPeriod = DebtPeriod (fromGregorian 2019 7 12) (Just $ fromGregorian 2019 7 13) (scientific 1000 0)
            expectedSecondPeriod = DebtPeriod (fromGregorian 2019 7 14) (Just $ fromGregorian 2019 7 15) (scientific 990 0)
            expectedThirdPeriod = DebtPeriod (fromGregorian 2019 7 16) Nothing (scientific 10255 (-1))

        (fromOperations ops) `shouldBe` [expectedFirstPeriod, expectedSecondPeriod, expectedThirdPeriod]

    context "when no debt periods" $ do
      it "returns an empty list" $ do
        let newOp = op { O.operationType = O.Credit, O.amount = scientific 1000 0, O.date = fromGregorian 2019 7 12 }
        let ops = [newOp, opA, opB, opC]

        (fromOperations ops) `shouldBe` []

    context "when no operations" $ do
      it "returns an empty list" $ do
        (fromOperations []) `shouldBe` []

    context "when one debit operation" $ do
      it "returns one debt period" $ do
        let ops = [opB]
            expectation = [DebtPeriod (fromGregorian 2019 7 16) Nothing (scientific 455 (-1))]

        (fromOperations ops) `shouldBe` expectation

    context "when one credit operation" $ do
      it "returns an empty list" $ do
        (fromOperations [opA]) `shouldBe` []


-- the only Operation data that matter here are the operation type, date, and amount,
-- so we'll just create one operation and use it as a default for creating others
op :: O.Operation
op = let opId = read  "c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: UUID
     in O.Operation { O.operationId   = opId
                    , O.accountId     = accId
                    , O.operationType = O.Credit
                    , O.date          = fromGregorian 2019 7 14
                    , O.amount        = scientific 10 0
                    , O.description   = "Amazon transaction" }

opA :: O.Operation
opA = op

-- +10

opB :: O.Operation
opB = op { O.operationId   = read  "df9abf28-78ce-483d-8de5-368d67763cb8" :: UUID
         , O.operationType = O.Debit
         , O.date          = fromGregorian 2019 7 16
         , O.amount        = scientific 455 (-1) }

opC :: O.Operation
opC = op { O.operationId   = read  "7ce81ac6-1abb-4008-9009-dec28f95629c" :: UUID
         , O.operationType = O.Credit
         , O.date          = fromGregorian 2019 7 16
         , O.amount        = scientific 10 0 }

-- -25.50

opD :: O.Operation
opD = op { O.operationId   = read  "3657d672-92ed-430d-aed8-0bcd54b0a4d8" :: UUID
         , O.operationType = O.Debit
         , O.date          = fromGregorian 2019 7 20
         , O.amount        = scientific 22 0 }

-- -47.50

opE :: O.Operation
opE = op { O.operationId   = read  "ca681fe0-1561-4604-b3a9-941c86c6916c" :: UUID
         , O.operationType = O.Credit
         , O.date          = fromGregorian 2019 7 23
         , O.amount        = scientific 1405 (-1) }

opF :: O.Operation
opF = op { O.operationId   = read  "62814544-4aba-47b1-b36c-87d0d03e39b7" :: UUID
         , O.operationType = O.Debit
         , O.date          = fromGregorian 2019 7 23
         , O.amount        = scientific 54 0 }

-- +39

opG :: O.Operation
opG = op { O.operationId   = read  "6c181235-9d51-4a17-81c1-95fbae0e7c70" :: UUID
         , O.operationType = O.Debit
         , O.date          = fromGregorian 2019 7 28
         , O.amount        = scientific 82 0 }

-- -43

opH :: O.Operation
opH = op { O.operationId   = read  "28f06237-a873-4311-b97f-7ead932896f5" :: UUID
         , O.operationType = O.Credit
         , O.date          = fromGregorian 2019 7 31
         , O.amount        = scientific 50 0 }

-- +7

accId :: UUID
accId = read "2ad34674-7825-49cc-b985-342eb48285c4" :: UUID
