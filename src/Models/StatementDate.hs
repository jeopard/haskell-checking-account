module Models.StatementDate (
  StatementDate (..),
  fromOperation,
  insertOperation
  ) where

import Data.Scientific
import Data.Time.Calendar

import qualified Models.Operation as O


-- represents a date line in a bank statement
data StatementDate = StatementDate { date            :: Day
                                   , operations      :: [O.Operation]
                                   , endOfDayBalance :: Scientific } deriving (Show, Eq)


-- creates a StatementDate using an operation and the balance at the end of the previous day
fromOperation :: O.Operation -> Scientific -> StatementDate
fromOperation op previousDayBalance =
  let eODBalance = previousDayBalance + (O.amountWithSign op)
  in StatementDate (O.date op) [op] eODBalance


-- Inserts a new operation to an existing StatementDate.
-- There is no check if they fall on the same date 
insertOperation :: O.Operation -> StatementDate -> StatementDate
insertOperation op oldSDate =
  let newOps = (operations oldSDate) ++ [op]
      newEODBalance = (endOfDayBalance oldSDate) + (O.amountWithSign op)
  in oldSDate { operations = newOps, endOfDayBalance = newEODBalance }
