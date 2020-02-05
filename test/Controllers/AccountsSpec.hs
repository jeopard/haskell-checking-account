{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Controllers.AccountsSpec (spec) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson (object, encode, (.=), eitherDecode)
import Data.ByteString.Char8
import Data.UUID
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import qualified Data.Map as Map
import qualified Web.Scotty.Trans as S

import Models.Account
import Serializers.Response
import SpecHelpers

import qualified Controllers.Accounts
import qualified Persistence.Database as DB


spec :: Spec
spec = do
  getAccountsSpec
  postAccountsSpec


getAccountsSpec :: Spec
getAccountsSpec = with service $ do
  describe "GET /accounts/:id" $ do
    context "when an account with the ID exists" $ do
      it "returns the account" $ do
        -- convert to Bytestring with pack
        let path = pack $ "/accounts/" ++ (show existingId)
            call = get path

        let expectedBody = [json|{status: "success", data: {type: "Account", id: "c2cc10e1-57d6-4b6f-9899-38d972112d8c", owner: "John"}}|]
        call `shouldRespondWith` expectedBody

    context "when an account with the ID does not exist" $ do
      it "returns an error" $ do
        -- this ID does not exist in the DB
        let call = get "/accounts/ad5efce7-ae5e-4efd-80d1-fe5eac9c0b54"

        let expectedBody = [json|{status: "error", message: "Account not found"}|]
        call `shouldRespondWith` expectedBody { matchStatus = 404 }

    context "when the ID is not a valid UUID" $ do
      it "returns an error" $ do
        let call = get "/accounts/someId"

        let expectedBody = [json|{status: "error", message: "Account not found"}|]
        call `shouldRespondWith` expectedBody { matchStatus = 404 }


postAccountsSpec :: Spec
postAccountsSpec = with service $ do
  describe "POST /accounts" $ do
    context "when the JSON body contains exactly one owner attribute" $ do
      it "creates an account" $ do
        let payload = object ["owner" .= ("Mary" :: String)]
            call = post "/accounts" (encode payload)

        let bodyMatcher = accountOwnerMatcher "Mary"

        call `shouldRespondWith` 200 { matchBody = MatchBody bodyMatcher }

    context "when the JSON body contains an owner attribute and others" $ do
      it "creates an account, ignoring the extra attributes" $ do
        let payload = object ["owner" .= ("Mary" :: String), "dob" .= ("1985-09-16" :: String)]
            call = post "/accounts" (encode payload)

        let bodyMatcher = accountOwnerMatcher "Mary"

        call `shouldRespondWith` 200 { matchBody = MatchBody bodyMatcher }

    context "when the JSON body does not contain an owner attribute" $ do
      it "returns an error" $ do
        let payload = object ["accountHolder" .= ("Mary" :: String)]
            call = post "/accounts" (encode payload)

        let expectedBody = [json|{status: "error", message: "Error in $: key \"owner\" not present"}|]
        call `shouldRespondWith` expectedBody { matchStatus = 400 }

    context "when the body is not JSON" $ do
      it "returns an error" $ do
        let call = post "/accounts" "a non-JSON body"

        let expectedBody = [json|{status: "error", message: "Error in $: Failed reading: not a valid json value"}|]
        call `shouldRespondWith` expectedBody { matchStatus = 400 }


-- set up a simple service to test the requests
service :: IO Application
service = do
  db <- newTVarIO existingDb

  return S.scottyAppT 8888 (\f -> runReaderT f db) Controllers.Accounts.routes


-- matches a success response with an Account in the payload and the owner String o
accountOwnerMatcher :: String -> [Header] -> Body -> Maybe String
accountOwnerMatcher o _ body =
  case (eitherDecode body :: Either String (Success Account)) of
    Left e -> Just e
    Right (Success acc) ->
      if (owner acc) /= o
      then Just $ "owner " ++ (owner acc) ++ " does not match " ++  o
      else Nothing -- all good, we have a match


-- helpers to create a DB with an existing account
existingDb :: DB.Database
existingDb = let accMap = Map.insert existingId existingAccount Map.empty
             in DB.Database { DB.accountsMap = accMap, DB.operationsMap = Map.empty }

existingAccount :: Account
existingAccount = Account { accountId = existingId, owner = "John" }

existingId :: UUID
existingId = read "c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: UUID

