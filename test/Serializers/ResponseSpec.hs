{-# LANGUAGE DeriveGeneric #-}

module Serializers.ResponseSpec (spec) where

import Data.Aeson (ToJSON, encode, toEncoding, genericToEncoding, defaultOptions)
import Data.ByteString.Lazy.Char8
import GHC.Generics
import Test.Hspec

import Serializers.Response


spec :: Spec
spec = do
  successSpec
  errorSpec

successSpec :: Spec
successSpec = do
  describe "Success" $ do
    successToJSONSpec

successToJSONSpec :: Spec
successToJSONSpec = do
  describe "toJSON" $ do
    it "renders the data correctly" $ do
      let payload = SampleData { key1 = 35, key2 = "some value" }
          serializer = Success payload

      let result = unpack $ encode serializer
          expectation = "{\"status\":\"success\",\"data\":{\"key2\":\"some value\",\"key1\":35}}"

      result `shouldBe` expectation

-- sample data structure to use in our spec
data SampleData = SampleData { key1 :: Int, key2 :: String } deriving (Generic)
instance ToJSON SampleData where
  toEncoding = genericToEncoding defaultOptions

errorSpec :: Spec
errorSpec = do
  describe "Success" $ do
    errorToJSONSpec

errorToJSONSpec :: Spec
errorToJSONSpec = do
  describe "toJSON" $ do
    it "renders the data correctly" $ do
      let serializer = Error "some error message"

      let result = unpack $ encode serializer
          expectation = "{\"status\":\"error\",\"message\":\"some error message\"}"

      result `shouldBe` expectation
