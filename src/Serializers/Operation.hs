{-# LANGUAGE OverloadedStrings #-}

module Serializers.Operation (
  OperationSerializer (OperationSerializer),
  toJSON
  ) where

import Data.Aeson ( ToJSON, toJSON, object, (.=) )

import Models.Operation


-- transforms an Operation to JSON
data OperationSerializer = OperationSerializer Operation
instance ToJSON OperationSerializer where
  toJSON (OperationSerializer o) =
    let presentedAmount = read (show (amount o)) :: Float
        opType = case operationType o of Credit -> ("credit" :: String)
                                         Debit  -> ("debit"  :: String)
    in object [
      "type"          .= ("Operation" :: String),
      "id"            .= operationId o,
      "accountId"     .= accountId o,
      "operationType" .= opType,
      "date"          .= date o,
      "amount"        .= presentedAmount,
      "description"   .= description o
      ]
