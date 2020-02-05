{-# LANGUAGE OverloadedStrings #-}

module Serializers.DebtPeriod (
  DebtPeriodSerializer (..),
  toJSON
  ) where

import Data.Aeson ( ToJSON, toJSON, object, (.=) )

import Models.DebtPeriod


-- transforms a DebtPeriod to JSON
data DebtPeriodSerializer = DebtPeriodSerializer DebtPeriod
instance ToJSON DebtPeriodSerializer where
  toJSON (DebtPeriodSerializer dp) =
    object [
      "type"      .= ("DebtPeriod" :: String),
      "fromDate"  .= fromDate dp,
      "toDate"    .= toDate dp,
      "principal" .= principal dp
      ]
