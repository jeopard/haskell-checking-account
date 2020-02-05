module Persistence.Operations (
  storeOperation,
  getOperations) where

import Data.List
import Data.UUID

import qualified Data.Map as Map
import qualified Data.Set as Set

import Models.Operation

import qualified Persistence.Database as DB


-- stores an operation to the DB
storeOperation :: Operation -> DB.Database -> DB.Database
storeOperation newOperation db = let oldMap = DB.operationsMap db
                                     accId = accountId newOperation

                                     -- create the operation Set if it does not exist
                                     maybeOldOpsSet = Map.lookup accId oldMap
                                     oldOpsSet = case maybeOldOpsSet of
                                       Nothing -> Set.empty
                                       Just opsSet -> opsSet

                                     newOpsSet = Set.insert newOperation oldOpsSet
                                     newMap = Map.insert accId newOpsSet oldMap

                                 in db { DB.operationsMap = newMap }


-- returns operations sorted by date
getOperations :: UUID -> DB.Database -> [Operation]
getOperations accId db =
  let operationsMap = DB.operationsMap db
      maybeOperationsSet = Map.lookup accId operationsMap
  -- return an empty list if no operations are found
  in case maybeOperationsSet of Nothing  -> []
                                Just opsSet -> sortOperationsByDate $ Set.toList opsSet

sortOperationsByDate :: [Operation] -> [Operation]
sortOperationsByDate ops = let sortFunc = (\x y -> compare (date x) (date y))
                           in sortBy sortFunc ops
