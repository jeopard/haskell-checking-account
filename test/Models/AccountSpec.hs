module Models.AccountSpec (spec) where

import Data.UUID.V4
import Test.Hspec

import Models.Account


spec :: Spec
spec = do
  equalitySpec


equalitySpec :: Spec
equalitySpec = do
  describe "==" $ do
    context "when accounts have the same ID" $ do
      it "returns True" $ do
        theId <- nextRandom

        let a = Account { accountId = theId, owner = "John" }
            b = Account { accountId = theId, owner = "Mary" }

        a == b `shouldBe` True

    context "when accounts have the different ID" $ do
      it "returns False" $ do
        aId <- nextRandom
        bId <- nextRandom

        let a = Account { accountId = aId, owner = "John" }
            b = Account { accountId = bId, owner = "Mary" }

        a == b `shouldBe` False
