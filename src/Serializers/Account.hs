{-# LANGUAGE OverloadedStrings #-}

module Serializers.Account (
  AccountSerializer (AccountSerializer),
  toJSON
  ) where

import Data.Aeson ( ToJSON, toJSON, object, (.=) )

import Models.Account


-- transforms an Account to JSON
data AccountSerializer = AccountSerializer Account
instance ToJSON AccountSerializer where
  toJSON (AccountSerializer a) = object ["type" .= ("Account" :: String),
                                         "id" .= accountId a,
                                         "owner" .= owner a ]
