{-# LANGUAGE OverloadedStrings #-}

module Serializers.Response (
  Success (Success),
  Error (Error)
  ) where

import Data.Aeson ( ToJSON, toJSON, object, (.=) )


-- wrapper for rendering successful responses
data Success a = Success a
instance (ToJSON a) => ToJSON (Success a) where
  toJSON (Success s) = object [ "status" .= ("success" :: String),
                                "data"   .= s ]


-- wrapper for rendering error responses
data Error = Error String
instance ToJSON Error where
  toJSON (Error e) = object [ "status"  .= ("error" :: String),
                              "message" .= e ]
