{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Serializers.AccountSpec (spec) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8
import Data.UUID
import Test.Hspec

import Models.Account
import Serializers.Account


spec :: Spec
spec = do
  toJSONSpec


toJSONSpec :: Spec
toJSONSpec = do
  describe "toJSON" $ do
    it "renders the data correctly" $ do
      let accId = (read "c2cc10e1-57d6-4b6f-9899-38d972112d8c") :: UUID
          acc = Account { accountId = accId, owner = "John" }
          serializer = AccountSerializer acc

      let result = unpack $ encode serializer
          expectation = "{\"owner\":\"John\",\"id\":\"c2cc10e1-57d6-4b6f-9899-38d972112d8c\",\"type\":\"Account\"}"

      result `shouldBe` expectation
