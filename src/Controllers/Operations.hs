{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Controllers.Operations (routes) where

import Control.Monad.Reader
import Control.Monad.Trans
import Data.Aeson ( FromJSON, eitherDecode, parseJSON, withObject, (.:) )
import Data.Scientific
import Data.Time.Calendar
import Data.UUID
import Data.UUID.V4
import GHC.Generics
import Network.HTTP.Types.Status
import Web.Scotty.Trans

import Core
import Persistence.Accounts
import Persistence.Operations
import Serializers.Operation
import Serializers.Response

import qualified Models.Operation as O
import qualified Persistence.Database as DB


routes :: AppScottyM ()
routes = do
  post "/operations" $ do
    b <- body
    let parsedBody = eitherDecode b :: Either String CreateOperationBody

    -- fail with 400 if we can't parse the body
    case parsedBody of
      Left e -> do
        status badRequest400
        json $ Error e
      Right correctlyParsedBody -> do
        op <- bodyToOperation correctlyParsedBody

        result <- transaction $ createOperationTransaction op

        -- if Just String, that's the error message, account does not exist
        case result of
          Just e -> do
            status badRequest400
            json $ Error e
          Nothing -> json $ Success $ OperationSerializer op


-- if there is an account, it stores the operation and returns the new DB and the operation,
-- otherwise it returns the DB unchanged and the error message
createOperationTransaction :: O.Operation -> DB.Database -> (DB.Database, Maybe String)
createOperationTransaction op oldDb =
  let maybeAccount = getAccount (O.accountId op) oldDb
      -- gets used only if the account is found
      newDb = storeOperation op oldDb
  in case maybeAccount of
       Nothing -> (oldDb, Just "Account with given ID does not exist")
       Just _  -> (newDb, Nothing) -- success


bodyToOperation :: CreateOperationBody -> AppActionM O.Operation
bodyToOperation body = do
  newId <- liftIO nextRandom

  return O.Operation { O.operationId   = newId
                     , O.accountId     = accountId body
                     , O.operationType = operationType body
                     , O.date          = date body
                     , O.amount        = amount body
                     , O.description   = description body }

data CreateOperationBody = CreateOperationBody { accountId     :: UUID
                                               , operationType :: O.OperationType
                                               , date          :: Day
                                               , amount        :: Scientific
                                               , description   :: String
                                               }

instance FromJSON CreateOperationBody where
  parseJSON = withObject "createAccountBody" $ \cab -> do
    accId <- cab .: "accountId"

    opTypeString :: String <- cab .: "operationType"

    opType <- do case opTypeString of
                   "credit" -> return O.Credit
                   "debit"  -> return O.Debit
                   _        -> fail "operationType can be either credit or debit"

    -- we could check if date <= today, but it's probably unnecessary for this exercise.
    -- what is today? in what timezone? 
    -- in a production service, we should be be storing the timestamp anyway
    opDate <- cab .: "date"

    opAmount <- cab .: "amount"
    when (opAmount < 0) $ fail "amount cannot be negative"
    
    opDescription <- cab .: "description"

    return CreateOperationBody { accountId     = accId,
                                 operationType = opType,
                                 date          = opDate,
                                 amount        = opAmount,
                                 description   = opDescription }
