{-# LANGUAGE OverloadedStrings #-}

module CoreSpec (spec) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.UUID
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai as T

import qualified Data.Map as Map
import qualified Web.Scotty.Trans as S

import Core
import Models.Account

import qualified Persistence.Database as DB


spec :: Spec
spec = do
  fetchUpdateDbSpec
  transactionSpec


fetchUpdateDbSpec :: Spec
fetchUpdateDbSpec = with service $ do
  describe "fetching from and updating the DB in a Scotty Action" $ do
    it "reads and updates the DB" $ do
      T.get  "/johnAccountExists" `shouldRespondWith` "false"
      T.post "/johnAccount" ""    `shouldRespondWith` 200
      T.get  "/johnAccountExists" `shouldRespondWith` "true"


transactionSpec :: Spec
transactionSpec = with service $ do
  describe "transactions" $ do
    it "reads from and writes to the DB atomically" $ do
      T.post "/maryAccountIfJohnExists" "" `shouldRespondWith` "false"
      T.post "/johnAccount" ""             `shouldRespondWith` 200
      T.post "/maryAccountIfJohnExists" "" `shouldRespondWith` "true"


-- set up a simple service that demostrates the use of updatedDb and fetchDb
service :: IO Application
service = do
  db <- newTVarIO DB.initDB

  return S.scottyAppT 8888  (\f -> runReaderT f db) $ do
    S.post "/johnAccount" $ do
      updateDb insertJohnAccount

    S.get "/johnAccountExists" $ do
      maybeAccount <- fetchDb getJohnAccount
      case maybeAccount of Nothing -> S.json False
                           Just _  -> S.json True

    S.post "/maryAccountIfJohnExists" $ do
      success <- transaction insertMaryIfJohnExists

      S.json success
      

insertMaryIfJohnExists :: DB.Database -> (DB.Database, Bool)
insertMaryIfJohnExists oldDb = let accMap = DB.accountsMap oldDb
                                   johnId = accountId johnAccount
                                   -- these are used only if john exists
                                   maryId = accountId maryAccount
                                   newAccMap = Map.insert maryId maryAccount accMap
                                   newDb = oldDb { DB.accountsMap = newAccMap }

                               in if Map.member johnId accMap
                                  then (newDb, True)
                                  else (oldDb, False)


insertJohnAccount :: DB.Database -> DB.Database
insertJohnAccount oldDb = let accMap = DB.accountsMap oldDb
                              newAccMap = Map.insert (accountId johnAccount) johnAccount accMap
                          in oldDb { DB.accountsMap = newAccMap }

getJohnAccount :: DB.Database -> Maybe Account
getJohnAccount db = let accMap = DB.accountsMap db
                    in Map.lookup (accountId johnAccount) accMap

johnAccount :: Account
johnAccount = let accId = (read "c2cc10e1-57d6-4b6f-9899-38d972112d8c") :: UUID
              in Account { accountId = accId, owner = "John" }

maryAccount :: Account
maryAccount = let accId = (read "db6cc62b-e8c8-4b13-83ef-852e6bc9476a") :: UUID
              in Account { accountId = accId, owner = "Mary" } 
