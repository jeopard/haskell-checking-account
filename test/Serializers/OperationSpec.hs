{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Serializers.OperationSpec (spec) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8
import Data.Scientific
import Data.Time.Calendar
import Data.UUID
import Test.Hspec

import Models.Operation
import Serializers.Operation


spec :: Spec
spec = do
  toJSONSpec


toJSONSpec :: Spec
toJSONSpec = do
  describe "toJSON" $ do
    context "when it's a debit operation" $ do
      it "renders the data correctly" $ do
        let serializer = OperationSerializer operation

        let result = unpack $ encode serializer
            expectation = "{\"amount\":22.32,\"date\":\"2019-10-04\",\"accountId\":\"ad5efce7-ae5e-4efd-80d1-fe5eac9c0b54\",\"id\":\"6d877c2d-90b6-4d3e-9caa-48f8eea4704e\",\"operationType\":\"debit\",\"type\":\"Operation\",\"description\":\"Some purchase\"}"

        result `shouldBe` expectation

    context "when it's a credit operation" $ do
      it "renders the data correctly" $ do
        let creditOperation = operation { operationType = Credit }
            serializer = OperationSerializer creditOperation

        let result = unpack $ encode serializer
            expectation = "{\"amount\":22.32,\"date\":\"2019-10-04\",\"accountId\":\"ad5efce7-ae5e-4efd-80d1-fe5eac9c0b54\",\"id\":\"6d877c2d-90b6-4d3e-9caa-48f8eea4704e\",\"operationType\":\"credit\",\"type\":\"Operation\",\"description\":\"Some purchase\"}"

        result `shouldBe` expectation


operation :: Operation
operation = let opId  = read "6d877c2d-90b6-4d3e-9caa-48f8eea4704e" :: UUID
                accId = read "ad5efce7-ae5e-4efd-80d1-fe5eac9c0b54" :: UUID
            in Operation { operationId   = opId
                         , accountId     = accId
                         , operationType = Debit
                         , date          = fromGregorian 2019 10 4
                         , amount        = scientific 2232 (-2)
                         , description   = "Some purchase" }
