module Models.Statement (
  Statement (..),
  fromOperations
  ) where

import Data.List
import Data.Time.Calendar
import Data.Scientific

import qualified Models.Operation as O
import qualified Models.StatementDate as SD


-- represents a bank statement for an account
data Statement = Statement { fromDate       :: Day
                           , toDate         :: Day
                           , statementDates :: [SD.StatementDate] } deriving (Show, Eq)


-- operation list is assumed to be sorted
fromOperations :: [O.Operation] -> Day -> Day -> Statement
fromOperations ops fromDate toDate =
      -- ops after toDate are useless
  let opsToDate = dropWhileEnd (\op -> (O.date op) > toDate) ops

      -- use the ops before fromDate to calculate the balance up to that day
      (opsBeforeFrom, opsFromDateToDate) = span (\op -> (O.date op) < fromDate) opsToDate

      startingBalance = O.calculateBalance opsBeforeFrom

      -- create the statement dates using the ops between the dates
      (sDates, _) = foldl populateStatementDates ([], startingBalance) opsFromDateToDate
      
  in Statement fromDate toDate sDates

-- helper function called by a foldl to build the statement dates for a statement.
-- The tuple holds the statement dates and the balance so far in our calculations
populateStatementDates :: ([SD.StatementDate], Scientific) -> O.Operation -> ([SD.StatementDate], Scientific)
populateStatementDates (sDates, balance) op =
  let newBalance = balance + (O.amountWithSign op)
      -- used only if list not empty
      lastSDate = last sDates
      -- if sDates is empty or last element is not of the same date, it appends,
      -- otherwise it edits the last element
      newSDates = if (null sDates) || (SD.date lastSDate /= O.date op)
                  then sDates ++ [SD.fromOperation op balance]
                  else (init sDates) ++ [SD.insertOperation op lastSDate]
  in (newSDates, newBalance)

