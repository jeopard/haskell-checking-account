{-# LANGUAGE OverloadedStrings #-}

module Serializers.StatementDate (
  StatementDateSerializer (..),
  toJSON
  ) where

import Data.Aeson ( ToJSON, Value, toJSON, object, (.=) )

import Models.StatementDate
import Serializers.Operation

import qualified Models.Operation as O


-- transforms a StatementDate to JSON
data StatementDateSerializer = StatementDateSerializer StatementDate
instance ToJSON StatementDateSerializer where
  toJSON (StatementDateSerializer sd) =
    let jsonOps = opsSetToJsonList (operations sd)
    in object [
      "type"            .= ("StatementDate" :: String),
      "date"            .= date sd,
      "operations"      .= jsonOps,
      "endOfDayBalance" .= endOfDayBalance sd
      ]


-- helper function to serialize the operations list
opsSetToJsonList :: [O.Operation] -> [Value]
opsSetToJsonList ops = let serializeFunction = (\op -> toJSON $ OperationSerializer op)
                       in map serializeFunction ops
