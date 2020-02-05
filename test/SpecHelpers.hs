{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module SpecHelpers () where

import Control.Monad.Reader
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

import Serializers.Response

import qualified Models.Account as A
import qualified Models.Operation as O


-- helpful functions for our specs

-- allows us to build account body matchers
instance FromJSON A.Account where
  parseJSON = withObject "account" $ \a -> do
    dataType <- a .: "type"
    when (dataType /= ("Account" :: String)) $ fail "type is not Account"
    
    accId <- a .: "id"
    accOwner <- a .: "owner"
    return A.Account { A.accountId = accId, A.owner = accOwner }


-- allows us to build operation body matchers
instance FromJSON O.Operation where
  parseJSON = withObject "operation" $ \o -> do
    dataType <- o .: "type"
    when (dataType /= ("Operation" :: String)) $ fail "type is not Operation"

    opId <- o .: "id"
    accId <- o .: "accountId"

    opTypeString :: String <- o .: "operationType"

    opType <- do case opTypeString of
                   "credit" -> return O.Credit
                   "debit"  -> return O.Debit
                   _        -> fail "operationType can be either credit or debit"

    opDate         <- o .: "date"
    opAmount       <- o .: "amount"
    opDescription  <- o .: "description"

    return O.Operation { O.operationId   = opId,
                         O.accountId     = accId,
                         O.operationType = opType,
                         O.date          = opDate,
                         O.amount        = opAmount,
                         O.description   = opDescription }


-- allows us to build successful response matchers
instance (FromJSON a) => FromJSON (Success a) where
  parseJSON = withObject "success" $ \s -> do

    status <- s .: "status"
    when (status /= ("success" :: String)) $ fail "status is not success"

    payload <- s .: "data"
    return $ Success payload


instance Eq O.OperationType where
  x == y = case (x, y) of
             (O.Credit, O.Credit) -> True
             (O.Debit, O.Debit)   -> True
             _                    -> False
