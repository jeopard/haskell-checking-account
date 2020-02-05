module Models.StatementSpec (spec) where

import Data.Scientific
import Data.Time.Calendar
import Data.UUID
import Test.Hspec

import Models.Statement
import Models.StatementDate

import qualified Models.Operation as O

spec :: Spec
spec = do
  fromOperationsSpec

fromOperationsSpec :: Spec
fromOperationsSpec = do
  describe "fromOperations" $ do
    context "when all operations are between the dates" $ do
      it "creates a Statement with the correct data" $ do
        let startDate = fromGregorian 2018 5 1
            endDate   = fromGregorian 2020 1 1
            result = fromOperations allOps startDate endDate 

        (fromDate result)       `shouldBe` startDate
        (toDate result)         `shouldBe` endDate
        (statementDates result) `shouldBe` [sDateA, sDateBC, sDateD, sDateEF, sDateG, sDateH]

    context "when all operations are before the dates" $ do
      it "creates a Statement with the correct data" $ do
        let startDate = fromGregorian 2020 5 1
            endDate   = fromGregorian 2021 1 1
            result = fromOperations allOps startDate endDate 

        (fromDate result)       `shouldBe` startDate
        (toDate result)         `shouldBe` endDate
        (statementDates result) `shouldBe` []

    context "when all operations are after the dates" $ do
      it "creates a Statement with the correct data" $ do
        let startDate = fromGregorian 2010 5 1
            endDate   = fromGregorian 2011 1 1
            result = fromOperations allOps startDate endDate 

        (fromDate result)       `shouldBe` startDate
        (toDate result)         `shouldBe` endDate
        (statementDates result) `shouldBe` []

    context "when some operations are on the dates" $ do
      it "creates a Statement including these operations" $ do
        let startDate = fromGregorian 2019 7 16
            endDate   = fromGregorian 2019 7 23
            result = fromOperations allOps startDate endDate 

        (fromDate result)       `shouldBe` startDate
        (toDate result)         `shouldBe` endDate
        (statementDates result) `shouldBe` [sDateBC, sDateD, sDateEF]

    context "when fromDate and toDate are on the same day" $ do
      it "creates a Statement including the operations of the day" $ do
        let startDate = fromGregorian 2019 7 16
            endDate   = fromGregorian 2019 7 16
            result = fromOperations allOps startDate endDate 

        (fromDate result)       `shouldBe` startDate
        (toDate result)         `shouldBe` endDate
        (statementDates result) `shouldBe` [sDateBC]


allOps :: [O.Operation]
allOps = [opA, opB, opC, opD, opE, opF, opG, opH]

-- the only Operation data that matter here are the operation type, date, and amount,
-- so we'll just create one operation and use it as a default for creating others
op :: O.Operation
op = let opId = read  "c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: UUID
     in O.Operation { O.operationId   = opId
                    , O.accountId     = accId
                    , O.operationType = O.Credit
                    , O.date          = fromGregorian 2019 7 14
                    , O.amount        = scientific 100 0
                    , O.description   = "Amazon transaction" }

opA :: O.Operation
opA = op

sDateA :: StatementDate
sDateA = StatementDate (O.date opA) [opA] (scientific 100 0)

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

-- 100 - 45.50 + 10 = 64.50
sDateBC :: StatementDate
sDateBC = StatementDate { date = (O.date opB), operations = [opB, opC], endOfDayBalance= (scientific 645 (-1))}

opD :: O.Operation
opD = op { O.operationId   = read  "3657d672-92ed-430d-aed8-0bcd54b0a4d8" :: UUID
         , O.operationType = O.Debit
         , O.date          = fromGregorian 2019 7 20
         , O.amount        = scientific 24 0 }

-- 64.50 - 24 = 40.50
sDateD :: StatementDate
sDateD = StatementDate (O.date opD) [opD] (scientific 405 (-1))

opE :: O.Operation
opE = op { O.operationId   = read  "ca681fe0-1561-4604-b3a9-941c86c6916c" :: UUID
         , O.operationType = O.Credit
         , O.date          = fromGregorian 2019 7 23
         , O.amount        = scientific 1405 (-1) }

opF :: O.Operation
opF = op { O.operationId   = read  "62814544-4aba-47b1-b36c-87d0d03e39b7" :: UUID
         , O.operationType = O.Debit
         , O.date          = fromGregorian 2019 7 23
         , O.amount        = scientific 53 0 }

-- 40.50 + 140.50 - 53 = 128.00
sDateEF :: StatementDate
sDateEF = StatementDate (O.date opE) [opE, opF] (scientific 128 0)

opG :: O.Operation
opG = op { O.operationId   = read  "6c181235-9d51-4a17-81c1-95fbae0e7c70" :: UUID
         , O.operationType = O.Debit
         , O.date          = fromGregorian 2019 7 28
         , O.amount        = scientific 22 0 }

-- 128 - 22 = 106
sDateG :: StatementDate
sDateG = StatementDate (O.date opG) [opG] (scientific 106 0)

opH :: O.Operation
opH = op { O.operationId   = read  "28f06237-a873-4311-b97f-7ead932896f5" :: UUID
         , O.operationType = O.Credit
         , O.date          = fromGregorian 2019 7 31
         , O.amount        = scientific 5 (-1) }

-- 106 + 0.50 = 106.50
sDateH :: StatementDate
sDateH = StatementDate (O.date opH) [opH] (scientific 1065 (-1))
  

accId :: UUID
accId = read "2ad34674-7825-49cc-b985-342eb48285c4" :: UUID
