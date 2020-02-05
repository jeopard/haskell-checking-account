{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Serializers.StatementSpec (spec) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8
import Data.Scientific
import Data.Time.Calendar
import Data.UUID
import Test.Hspec

import Models.Statement
import Models.StatementDate
import Serializers.Statement

import qualified Models.Operation as O

spec :: Spec
spec = do
  toJSONSpec


toJSONSpec :: Spec
toJSONSpec = do
  describe "toJSON" $ do
    context "when no statement dates" $ do
      it "renders an empty statementDates array" $ do
        let startDate = fromGregorian 2019 5 1
            endDate   = fromGregorian 2019 5 30
            statement = Statement startDate endDate []
            serializer = StatementSerializer statement

        let result = unpack $ encode serializer
            expectation = "{\"fromDate\":\"2019-05-01\",\"toDate\":\"2019-05-30\",\"statementDates\":[],\"type\":\"Statement\"}"

        result `shouldBe` expectation

    context "when there are statement dates" $ do
      it "renders the Statement correctly" $ do
        let startDate = fromGregorian 2019 7 1
            endDate   = fromGregorian 2019 7 31
            statement = Statement startDate endDate [sDateB, sDateC]
            serializer = StatementSerializer statement

        let result = unpack $ encode serializer
            expectation = "{\"fromDate\":\"2019-07-01\",\"toDate\":\"2019-07-31\",\"statementDates\":[{\"date\":\"2019-07-22\",\"endOfDayBalance\":2010.5,\"type\":\"StatementDate\",\"operations\":[{\"amount\":51.84,\"date\":\"2019-07-22\",\"accountId\":\"c2cc10e1-57d6-4b6f-9899-38d972112d8c\",\"id\":\"6d877c2d-90b6-4d3e-9caa-48f8eea4704e\",\"operationType\":\"debit\",\"type\":\"Operation\",\"description\":\"Amazon purchase\"}]},{\"date\":\"2019-07-23\",\"endOfDayBalance\":2010.5,\"type\":\"StatementDate\",\"operations\":[{\"amount\":0,\"date\":\"2019-07-23\",\"accountId\":\"c2cc10e1-57d6-4b6f-9899-38d972112d8c\",\"id\":\"3b09e842-ef74-4c30-be49-4984a6691584\",\"operationType\":\"debit\",\"type\":\"Operation\",\"description\":\"Canceled transaction\"}]}],\"type\":\"Statement\"}"

        result `shouldBe` expectation

sDateB :: StatementDate
sDateB = StatementDate (fromGregorian 2019 7 22) [operationB] (scientific 20105 (-1))

operationB :: O.Operation
operationB = let opId  = read "6d877c2d-90b6-4d3e-9caa-48f8eea4704e" :: UUID
             in O.Operation { O.operationId   = opId
                            , O.accountId     = accId
                            , O.operationType = O.Debit
                            , O.date          = fromGregorian 2019 7 22
                            , O.amount        = scientific 5184 (-2)
                            , O.description   = "Amazon purchase" }


sDateC :: StatementDate
sDateC = StatementDate (fromGregorian 2019 7 23) [operationC] (scientific 20105 (-1))

operationC :: O.Operation
operationC = let opId  = read "3b09e842-ef74-4c30-be49-4984a6691584" :: UUID
             in O.Operation { O.operationId   = opId
                            , O.accountId     = accId
                            , O.operationType = O.Debit
                            , O.date          = fromGregorian 2019 7 23
                            , O.amount        = scientific 0 0 
                            , O.description   = "Canceled transaction" }

accId :: UUID
accId = read "c2cc10e1-57d6-4b6f-9899-38d972112d8c" :: UUID
