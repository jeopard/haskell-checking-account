{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Serializers.StatementDateSpec (spec) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8
import Data.Scientific
import Data.Time.Calendar
import Data.UUID
import Test.Hspec

import Models.StatementDate
import Serializers.StatementDate

import qualified Models.Operation as O

spec :: Spec
spec = do
  toJSONSpec


toJSONSpec :: Spec
toJSONSpec = do
  describe "toJSON" $ do
    context "when no operations" $ do
      it "renders an empty operations array" $ do
        let sDate = StatementDate (fromGregorian 2019 8 10) [] (scientific 20105 (-1))
            serializer = StatementDateSerializer sDate

        let result = unpack $ encode serializer
            expectation = "{\"date\":\"2019-08-10\",\"endOfDayBalance\":2010.5,\"type\":\"StatementDate\",\"operations\":[]}"

        result `shouldBe` expectation

    context "when there are operations" $ do
      it "renders the StatementDate correctly" $ do
        let d = fromGregorian 2019 7 22
            ops = [operationB, operationC]
            a = scientific 20105 (-1)
            serializer = StatementDateSerializer $ StatementDate d ops a

        let result = unpack $ encode serializer
            expectation = "{\"date\":\"2019-07-22\",\"endOfDayBalance\":2010.5,\"type\":\"StatementDate\",\"operations\":[{\"amount\":51.84,\"date\":\"2019-07-22\",\"accountId\":\"c2cc10e1-57d6-4b6f-9899-38d972112d8c\",\"id\":\"6d877c2d-90b6-4d3e-9caa-48f8eea4704e\",\"operationType\":\"debit\",\"type\":\"Operation\",\"description\":\"Amazon purchase\"},{\"amount\":0,\"date\":\"2019-07-22\",\"accountId\":\"c2cc10e1-57d6-4b6f-9899-38d972112d8c\",\"id\":\"3b09e842-ef74-4c30-be49-4984a6691584\",\"operationType\":\"debit\",\"type\":\"Operation\",\"description\":\"Canceled transaction\"}]}"

        result `shouldBe` expectation

operationB :: O.Operation
operationB = let opId  = read "6d877c2d-90b6-4d3e-9caa-48f8eea4704e" :: UUID
             in O.Operation { O.operationId   = opId
                            , O.accountId     = accId
                            , O.operationType = O.Debit
                            , O.date          = fromGregorian 2019 7 22
                            , O.amount        = scientific 5184 (-2)
                            , O.description   = "Amazon purchase" }

operationC :: O.Operation
operationC = let opId  = read "3b09e842-ef74-4c30-be49-4984a6691584" :: UUID
             in O.Operation { O.operationId   = opId
                            , O.accountId     = accId
                            , O.operationType = O.Debit
                            , O.date          = fromGregorian 2019 7 22
                            , O.amount        = scientific 0 0 
                            , O.description   = "Canceled transaction" }

accId :: UUID
accId = read "c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: UUID
