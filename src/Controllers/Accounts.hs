{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Controllers.Accounts (routes) where

import Control.Monad.Trans
import Data.Aeson ( FromJSON, eitherDecode )
import Data.UUID
import Data.UUID.V4
import GHC.Generics
import Network.HTTP.Types.Status
import Web.Scotty.Trans

import Core
import Persistence.Accounts
import qualified Models.Account as A
import Serializers.Account
import Serializers.Response

routes :: AppScottyM ()
routes = do

  post "/accounts" $ do
    b <- body
    let parsedBody = eitherDecode b :: Either String CreateAccountBody

    -- fail with 400 if the request body is not correct
    case parsedBody of
      Left e -> do
        status badRequest400
        json $ Error e
      Right correctlyParsedBody -> do
        account <- createAccount correctlyParsedBody
        
        json $ Success $ AccountSerializer account

  get "/accounts/:accId" $ do
    accId <- param "accId"

    -- if not UUID, it's not an account ID, so 404
    let maybeUuid = fromString accId
    case maybeUuid of
      Nothing -> accountNotFound
      Just accUuid -> do

        -- returns the account if it exists
        maybeAccount <- fetchDb $ getAccount accUuid

        case maybeAccount of
          Nothing -> accountNotFound
          Just acc -> json $ Success $ AccountSerializer acc


accountNotFound :: AppActionM ()
accountNotFound = do
  status notFound404
  json $ Error "Account not found"

createAccount :: CreateAccountBody -> AppActionM A.Account
createAccount body = do
  newId <- liftIO nextRandom
  let account = A.Account { A.accountId    = newId,
                            A.owner        = owner body }
  updateDb $ storeAccount account

  return account

data CreateAccountBody = CreateAccountBody { owner :: String } deriving (Generic)
instance FromJSON CreateAccountBody
