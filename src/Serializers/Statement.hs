{-# LANGUAGE OverloadedStrings #-}

module Serializers.Statement (
  StatementSerializer (..),
  toJSON
  ) where

import Data.Aeson ( ToJSON, Value, toJSON, object, (.=) )

import Models.Statement
import Models.StatementDate
import Serializers.StatementDate


-- transforms a Statement to JSON
data StatementSerializer = StatementSerializer Statement
instance ToJSON StatementSerializer where
  toJSON (StatementSerializer s) =
    let jsonSDates = sDatesToJson (statementDates s)
    in object [
      "type"           .= ("Statement" :: String),
      "fromDate"       .= fromDate s,
      "toDate"         .= toDate s,
      "statementDates" .= jsonSDates
      ]


-- helper function to serialize the statementDates list
sDatesToJson :: [StatementDate] -> [Value]
sDatesToJson sDates =
  let serializeFunction = (\sDate -> toJSON $ StatementDateSerializer sDate)
  in map serializeFunction sDates
