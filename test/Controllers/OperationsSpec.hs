{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Controllers.OperationsSpec (spec) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson (object, encode, (.=), eitherDecode)
import Data.Scientific
import Data.Time.Calendar
import Data.UUID
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import qualified Data.Map as Map
import qualified Web.Scotty.Trans as S

import Models.Operation
import Serializers.Response
import SpecHelpers

import qualified Controllers.Operations
import qualified Models.Account as A
import qualified Persistence.Database as DB


spec :: Spec
spec = do
  postOperationsSpec


postOperationsSpec :: Spec
postOperationsSpec = with service $ do
  describe "POST /operations" $ do
    context "when the JSON request body contains the correct attributes" $ do
      context "and account with ID exists" $ do
        context "and the operation type is credit" $ do
          it "creates an operation" $ do
            -- account ID matches the existing one in the DB
            let payload = object ["accountId" .= ("c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: String), "operationType" .= ("credit" :: String), "date" .= ("2018-01-10" :: String), "amount" .= (123.84 :: Float), "description" .= ("Mercado Livre" :: String)]

            let call = post "/operations" (encode payload)

            let bodyMatcher = operationMatcher existingAccId Credit (fromGregorian 2018 1 10) (scientific 2 12384) "Mercado Livre"

            call `shouldRespondWith` 200 { matchBody = MatchBody bodyMatcher }

        context "and the operation type is debit" $ do
          it "creates an operation" $ do
            -- account ID matches the existing one in the DB
            let payload = object ["accountId" .= ("c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: String), "operationType" .= ("debit" :: String), "date" .= ("2018-01-10" :: String), "amount" .= (123.84 :: Float), "description" .= ("Mercado Livre" :: String)]

            let call = post "/operations" (encode payload)

            let bodyMatcher = operationMatcher existingAccId Debit (fromGregorian 2018 1 10) (scientific 2 12384) "Mercado Livre"

            call `shouldRespondWith` 200 { matchBody = MatchBody bodyMatcher }
            
        context "and the operation type is neither credit or debit" $ do
          it "returns an error" $ do
            -- account ID matches the existing one in the DB
            let payload = object ["accountId" .= ("c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: String), "operationType" .= ("othertype" :: String), "date" .= ("2018-01-10" :: String), "amount" .= (123.84 :: Float), "description" .= ("Mercado Livre" :: String)]

            let call = post "/operations" (encode payload)

            let expectedBody = [json|{status: "error", message: "Error in $: operationType can be either credit or debit"}|]
            call `shouldRespondWith`  expectedBody { matchStatus = 400 }
            
        context "and the amount is negative" $ do
          it "returns an error" $ do
            -- account ID matches the existing one in the DB
            let payload = object ["accountId" .= ("c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: String), "operationType" .= ("debit" :: String), "date" .= ("2018-01-10" :: String), "amount" .= (-123.84 :: Float), "description" .= ("Mercado Livre" :: String)]

            let call = post "/operations" (encode payload)

            let expectedBody = [json|{status: "error", message: "Error in $: amount cannot be negative"}|]
            call `shouldRespondWith`  expectedBody { matchStatus = 400 }

      context "and account with ID does not exist" $ do
        it "returns an error" $ do
          -- account ID does not match the existing one in the DB
          let payload = object ["accountId" .= ("78859120-35e3-4e00-8b6a-2fafe6470f69" :: String), "operationType" .= ("credit" :: String), "date" .= ("2018-01-10" :: String), "amount" .= (123.84 :: Float), "description" .= ("Mercado Livre" :: String)]

          let call = post "/operations" (encode payload)

          let expectedBody = [json|{status: "error", message: "Account with given ID does not exist"}|]
          call `shouldRespondWith`  expectedBody { matchStatus = 400 }
        
    context "when the JSON body does not contain the account ID" $ do
      it "returns an error" $ do
        let payload = object ["operationType" .= ("credit" :: String), "date" .= ("2018-01-10" :: String), "amount" .= (123.84 :: Float), "description" .= ("Mercado Livre" :: String)]
            call = post "/operations" (encode payload)

        let expectedBody = [json|{status: "error", message: "Error in $: key \"accountId\" not present"}|]
        call `shouldRespondWith` expectedBody { matchStatus = 400 }

    context "when the body is not JSON" $ do
      it "returns an error" $ do
        let call = post "/operations" "a non-JSON body"

        let expectedBody = [json|{status: "error", message: "Error in $: Failed reading: not a valid json value"}|]
        call `shouldRespondWith` expectedBody { matchStatus = 400 }


-- set up a simple service to test the requests
service :: IO Application
service = do
  db <- newTVarIO existingDb

  return S.scottyAppT 8888 (\f -> runReaderT f db) Controllers.Operations.routes


-- matches a success response with an Operation in the payload
operationMatcher :: UUID -> OperationType -> Day -> Scientific -> String -> [Header] -> Body -> Maybe String
operationMatcher accId opType opDate opAmount opDescription _ body =
  case (eitherDecode body :: Either String (Success Operation)) of
    Left e -> Just e
    Right (Success op) ->
      if (accountId op) /= accId && (operationType op) /= opType && (date op) /= opDate &&
         (amount op) /= opAmount && (description op) /= opDescription
      then Just $ "operations do not match"
      else Nothing -- all good, we have a match


-- helpers to create a DB with an existing account
existingDb :: DB.Database
existingDb = let accMap = Map.insert existingAccId existingAccount Map.empty
             in DB.Database { DB.accountsMap = accMap, DB.operationsMap = Map.empty }

existingAccount :: A.Account
existingAccount = A.Account { A.accountId = existingAccId, A.owner = "John" }

existingAccId :: UUID
existingAccId = read "c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: UUID

