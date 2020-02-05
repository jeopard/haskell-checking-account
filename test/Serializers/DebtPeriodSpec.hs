{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Serializers.DebtPeriodSpec (spec) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8
import Data.Scientific
import Data.Time.Calendar
import Test.Hspec

import Models.DebtPeriod
import Serializers.DebtPeriod


spec :: Spec
spec = do
  toJSONSpec


toJSONSpec :: Spec
toJSONSpec = do
  describe "toJSON" $ do
    context "when toDate exists" $ do
      it "renders an empty operations array" $ do
        let startDate = fromGregorian 2019 8 10
            endDate = Just $ fromGregorian 2019 8 14
            p = scientific 20105 (-1)

            dp = DebtPeriod startDate endDate p
            serializer = DebtPeriodSerializer dp

        let result = unpack $ encode serializer
            expectation = "{\"fromDate\":\"2019-08-10\",\"toDate\":\"2019-08-14\",\"principal\":2010.5,\"type\":\"DebtPeriod\"}"

        result `shouldBe` expectation

    context "when toDate does not exist" $ do
      it "renders an empty operations array" $ do
        let startDate = fromGregorian 2019 8 10
            p = scientific 20105 (-1)

            dp = DebtPeriod startDate Nothing p
            serializer = DebtPeriodSerializer dp

        let result = unpack $ encode serializer
            expectation = "{\"fromDate\":\"2019-08-10\",\"toDate\":null,\"principal\":2010.5,\"type\":\"DebtPeriod\"}"

        result `shouldBe` expectation
