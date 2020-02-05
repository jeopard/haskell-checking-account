module Persistence.Database (
  Database (..),
  initDB
  ) where

import Data.UUID
import Models.Account
import Models.Operation

import qualified Data.Map as Map
import qualified Data.Set as Set

-- our schema
-- 
-- Note: we index operations based on their accountId rather than their own ID
--       because it's more convenient in this exercise. It should be good enough
data Database = Database { accountsMap   :: Map.Map UUID Account,
                           operationsMap :: Map.Map UUID (Set.Set Operation) }


-- an empty DB, used on server startup
initDB :: Database
initDB = Database { accountsMap = Map.empty, operationsMap = Map.empty }

