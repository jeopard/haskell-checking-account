{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Controllers.AccountResourcesSpec (spec) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Scientific
import Data.Time.Calendar
import Data.UUID
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai as THW
import Test.Hspec.Wai.JSON

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Web.Scotty.Trans as S

import Models.Operation

import qualified Controllers.AccountResources
import qualified Models.Account as A
import qualified Persistence.Database as DB


spec :: Spec
spec = do
  balanceSpec
  debtPeriodsSpec
  statementSpec


balanceSpec :: Spec
balanceSpec = with service $ do
  describe "GET /accounts/:accId/balance" $ do
    context "when account with ID exists" $ do
      context "and it has no stored operations" $ do
        it "returns 0" $ do
          -- the ID of the account without operations
          let call = get "/accounts/2ad34674-7825-49cc-b985-342eb48285c4/balance"

          let expectedBody = [json|{status: "success", data: 0.0}|]

          call `shouldRespondWith` expectedBody

      context "and it has stored operations" $ do
        it "returns the sum of the amounts of the operations" $ do
          -- the ID of the account with operations
          let call = get "/accounts/c2cc10e1-57d6-4b6f-9899-38d972112d8c/balance"

          -- balance is (2000 - 51.84 - 0 - 15.19) = 1932.97
          let expectedBody = [json|{status: "success", data: -189.03}|]

          call `shouldRespondWith` expectedBody

    context "when account with ID does not exist" $ do
      it "returns an error" $ do
          -- random ID
        let call = get "/accounts/57402958-47ab-4ad6-b30a-c38205f720ce/balance"

        let expectedBody = [json|{status: "error", message: "Account not found"}|]

        call `shouldRespondWith` expectedBody { matchStatus = 404 }

    context "when a non-UUID ID is given" $ do
      it "returns an error" $ do
        let call = get "/accounts/non-uuid-id/balance"

        let expectedBody = [json|{status: "error", message: "Account not found"}|]

        call `shouldRespondWith` expectedBody { matchStatus = 404 }


debtPeriodsSpec :: Spec
debtPeriodsSpec = with service $ do
  describe "GET /accounts/:accId/debtPeriods" $ do
    context "when account with ID exists" $ do
      context "and it has no stored operations" $ do
        it "returns 0" $ do
          -- the ID of the account without operations
          let call = get "/accounts/2ad34674-7825-49cc-b985-342eb48285c4/debtPeriods"

          let expectedBody = [json|{status: "success", data: []}|]

          call `shouldRespondWith` expectedBody

      context "and it has stored operations" $ do
        it "returns the sum of the amounts of the operations" $ do
          -- the ID of the account with operations
          let call = get "/accounts/c2cc10e1-57d6-4b6f-9899-38d972112d8c/debtPeriods"

          let expectedBody = [json|{status: "success", data: [{type: "DebtPeriod", fromDate: "2019-07-12", toDate: "2019-07-21", principal: 322}, {type: "DebtPeriod", fromDate: "2019-07-22", toDate: "2019-08-09", principal: 173.84}, {type: "DebtPeriod", fromDate: "2019-08-10", toDate: null, principal: 189.03}]}|]

          call `shouldRespondWith` expectedBody

    context "when account with ID does not exist" $ do
      it "returns an error" $ do
          -- random ID
        let call = get "/accounts/57402958-47ab-4ad6-b30a-c38205f720ce/debtPeriods"

        let expectedBody = [json|{status: "error", message: "Account not found"}|]

        call `shouldRespondWith` expectedBody { matchStatus = 404 }

    context "when a non-UUID ID is given" $ do
      it "returns an error" $ do
        let call = get "/accounts/non-uuid-id/debtPeriods"

        let expectedBody = [json|{status: "error", message: "Account not found"}|]

        call `shouldRespondWith` expectedBody { matchStatus = 404 }


statementSpec :: Spec
statementSpec = with service $ do
  describe "GET /accounts/:accId/statement" $ do
    context "when account with ID exists" $ do
      context "and it has no stored operations" $ do
        it "returns 0" $ do
          -- the ID of the account without operations
          let call = get "/accounts/2ad34674-7825-49cc-b985-342eb48285c4/statement?fromDate=2018-01-01&toDate=2018-01-31"

          let expectedBody = [json|{status: "success", data: {type: "Statement", fromDate: "2018-01-01", toDate: "2018-01-31", statementDates: []}}|]

          call `shouldRespondWith` expectedBody

      context "and it has stored operations" $ do
        it "returns the sum of the amounts of the operations" $ do
          -- the ID of the account with operations
          let call = get "/accounts/c2cc10e1-57d6-4b6f-9899-38d972112d8c/statement?fromDate=2019-07-22&toDate=2019-08-21"

          -- we could have written a matcher again, but we only need it for one example
          -- and having the exact JSON here is also a bit safer
          let expectedBody = [json|{status: "success", data: {type: "Statement", fromDate: "2019-07-22", toDate: "2019-08-21", statementDates: [{type: "StatementDate", date: "2019-07-22", endOfDayBalance: -173.84, operations: [{type: "Operation", id: "3b09e842-ef74-4c30-be49-4984a6691584", accountId: "c2cc10e1-57d6-4b6f-9899-38d972112d8c", operationType: "credit", date: "2019-07-22", amount: 200.0, description: "Gift from parents"}, {type: "Operation", id: "6d877c2d-90b6-4d3e-9caa-48f8eea4704e", accountId: "c2cc10e1-57d6-4b6f-9899-38d972112d8c", operationType: "debit", date: "2019-07-22", amount: 51.84, description: "Amazon purchase"}]},{type: "StatementDate", date: "2019-08-10", endOfDayBalance: -189.03, operations: [{type: "Operation", id: "db6cc62b-e8c8-4b13-83ef-852e6bc9476a", accountId: "c2cc10e1-57d6-4b6f-9899-38d972112d8c", operationType: "debit", date: "2019-08-10", amount: 15.19, description: "Uber ride"}]}]}}|]

          call `shouldRespondWith` expectedBody

    context "when there is no fromDate" $ do
      it "returns an error" $ do
          -- the ID of the account with operations
          let call = get "/accounts/c2cc10e1-57d6-4b6f-9899-38d972112d8c/statement?notFromDate=2019-07-22&toDate=2019-08-21"

          let expectedBody = [json|{status: "error", message: "did not find fromDate query param"}|]

          call `shouldRespondWith` expectedBody { matchStatus = 400 }

    context "when there is no toDate" $ do
      it "returns an error" $ do
          -- the ID of the account with operations
          let call = get "/accounts/c2cc10e1-57d6-4b6f-9899-38d972112d8c/statement?fromDate=2019-07-22&notToDate=2019-08-21"

          let expectedBody = [json|{status: "error", message: "did not find toDate query param"}|]

          call `shouldRespondWith` expectedBody { matchStatus = 400 }

    context "when fromDate is not an ISO8601 date" $ do
      it "returns an error" $ do
          -- the ID of the account with operations
          let call = get "/accounts/c2cc10e1-57d6-4b6f-9899-38d972112d8c/statement?fromDate=januaryFirst2019&toDate=2019-08-21"

          let expectedBody = [json|{status: "error", message: "could not parse fromDate"}|]

          call `shouldRespondWith` expectedBody { matchStatus = 400 }

    context "when toDate is not an ISO8601 date" $ do
      it "returns an error" $ do
          -- the ID of the account with operations
          let call = get "/accounts/c2cc10e1-57d6-4b6f-9899-38d972112d8c/statement?fromDate=2019-08-22&toDate=januaryFirst2019"

          let expectedBody = [json|{status: "error", message: "could not parse toDate"}|]

          call `shouldRespondWith` expectedBody { matchStatus = 400 }

    context "when fromDate is later than toDate" $ do
      it "returns an error" $ do
          -- the ID of the account with operations
          let call = get "/accounts/c2cc10e1-57d6-4b6f-9899-38d972112d8c/statement?fromDate=2019-08-22&toDate=2019-08-20"

          let expectedBody = [json|{status: "error", message: "fromDate cannot be greater than toDate"}|]

          call `shouldRespondWith` expectedBody { matchStatus = 400 }

    context "when account with ID does not exist" $ do
      it "returns an error" $ do
          -- random ID
        let call = get "/accounts/57402958-47ab-4ad6-b30a-c38205f720ce/statement?fromDate=2019-08-22&toDate=2019-08-30"

        let expectedBody = [json|{status: "error", message: "Account not found"}|]

        call `shouldRespondWith` expectedBody { matchStatus = 404 }

    context "when a non-UUID ID is given" $ do
      it "returns an error" $ do
        let call = get "/accounts/non-uuid-id/statement?fromDate=2019-08-22&toDate=2019-08-30"

        let expectedBody = [json|{status: "error", message: "Account not found"}|]

        call `shouldRespondWith` expectedBody { matchStatus = 404 }


-- set up a simple service to test the requests
service :: IO Application
service = do
  dbTvar <- newTVarIO db

  return S.scottyAppT 8888 (\f -> runReaderT f dbTvar) Controllers.AccountResources.routes


-- setting up a DB with 2 accounts, one of which has operations
db :: DB.Database
db = let accMap = Map.fromList [(accWithoutOperationsId, accWithoutOperations),
                                (accWithOperationsId,    accWithOperations)]

         opsMap = Map.fromList [(accWithOperationsId, accOperations)]

     in DB.Database { DB.accountsMap = accMap, DB.operationsMap = opsMap }

accWithoutOperations :: A.Account
accWithoutOperations = A.Account { A.accountId = accWithoutOperationsId, A.owner = "Mary" }

accWithoutOperationsId :: UUID
accWithoutOperationsId = read "2ad34674-7825-49cc-b985-342eb48285c4" :: UUID

accWithOperations :: A.Account
accWithOperations = A.Account { A.accountId = accWithoutOperationsId, A.owner = "John" }

accWithOperationsId :: UUID
accWithOperationsId = read "c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: UUID


accOperations :: Set.Set Operation
accOperations = Set.fromList [operationA, operationB, operationC, operationD, operationE]
  

operationA :: Operation
operationA = let opId = read  "c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: UUID
             in Operation { operationId   = opId
                          , accountId     = accWithOperationsId
                          , operationType = Credit
                          , date          = fromGregorian 2019 7 2
                          , amount        = scientific 1000 0
                          , description   = "Payment from work" }
  

operationB :: Operation
operationB = let opId = read  "459b0919-f481-49fa-abdd-bc1750215139" :: UUID
             in Operation { operationId   = opId
                          , accountId     = accWithOperationsId
                          , operationType = Debit
                          , date          = fromGregorian 2019 7 12
                          , amount        = scientific 1322 0
                          , description   = "Tickets to China" }

operationC :: Operation
operationC = let opId  = read "6d877c2d-90b6-4d3e-9caa-48f8eea4704e" :: UUID
             in Operation { operationId   = opId
                          , accountId     = accWithOperationsId
                          , operationType = Debit
                          , date          = fromGregorian 2019 7 22
                          , amount        = scientific 5184 (-2)
                          , description   = "Amazon purchase" }

operationD :: Operation
operationD = let opId  = read "3b09e842-ef74-4c30-be49-4984a6691584" :: UUID
             in Operation { operationId   = opId
                          , accountId     = accWithOperationsId
                          , operationType = Credit
                          , date          = fromGregorian 2019 7 22
                          , amount        = scientific 200 0 
                          , description   = "Gift from parents" }

operationE :: Operation
operationE = let opId  = read "db6cc62b-e8c8-4b13-83ef-852e6bc9476a" :: UUID
             in Operation { operationId   = opId
                          , accountId     = accWithOperationsId
                          , operationType = Debit
                          , date          = fromGregorian 2019 8 10
                          , amount        = scientific 1519 (-2) 
                          , description   = "Uber ride" }

