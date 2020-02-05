module Lib (main) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Network.Wai.Middleware.RequestLogger
import System.Environment
import Web.Scotty.Trans

import Core

import qualified Controllers.Accounts
import qualified Controllers.AccountResources
import qualified Controllers.Operations
import qualified Persistence.Database as DB

-- start the server at the $PORT env var, and load the routes from the controllers
main :: IO ()
main = do
  port <- getEnv "PORT"
  db <- newTVarIO DB.initDB

  scottyT (read port) (\f -> runReaderT f db) routes


routes :: AppScottyM ()
routes = do
  middleware logStdoutDev

  Controllers.Accounts.routes
  Controllers.AccountResources.routes
  Controllers.Operations.routes
