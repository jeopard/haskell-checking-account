module Models.DebtPeriod (
  DebtPeriod (..),
  fromOperations
  ) where

import Data.Scientific
import Data.Time.Calendar

import qualified Models.Operation as O


-- represents debt periods for a bank account
data DebtPeriod = DebtPeriod { fromDate  :: Day
                             , toDate    :: Maybe Day
                             , principal :: Scientific } deriving (Show, Eq)


-- assuming ops are sorted and balance before first op is zero
fromOperations :: [O.Operation] -> [DebtPeriod]
fromOperations ops = let dateBalances = foldl balanceOnDates [] ops
                     in getPeriods dateBalances

-- helper structure to calculate the balances
data DateBalance = DateBalance { date :: Day, balance :: Scientific}

-- similar to the statement creation functions, but simpler.
-- it's used by foldl to build the balances per date
balanceOnDates :: [DateBalance] -> O.Operation -> [DateBalance]
balanceOnDates dateBalances op
  -- when the accumulator is empty, we insert the first DateBalance
  | null dateBalances = [DateBalance (O.date op) (O.amountWithSign op)]
  -- otherwise, we check if the last DateBalance is on the same date as op.
  -- if it is, we update its balance. Otherwise we append a new DateBalance
  | otherwise =
      let lastDateBalance = last dateBalances
          newBalance = (balance lastDateBalance) + (O.amountWithSign op)
          newDateBalance = DateBalance (O.date op) newBalance
      in if date lastDateBalance == O.date op
         then (init dateBalances) ++ [newDateBalance]
         else dateBalances ++ [newDateBalance]

-- recursive function that constructs the debt periods given the balances per day
getPeriods :: [DateBalance] -> [DebtPeriod]
-- when no DateBalances, no debt
getPeriods [] = []
-- when one DateBalance, and it's negative, we add one period with no toDate
getPeriods (db:[]) = 
  let b = balance db
      period = DebtPeriod (date db) Nothing (abs b)
  in if b >= 0
     then []
     else [period]
-- when two or more DateBalances, and first is negative, we add one period with end
-- end set to the day before the second DateBalance. After that, we continue looking
-- by calling getPeriods again and append its results
getPeriods (db1:db2:dbs) =
  let b = balance db1
      -- period ends the date before the next starts
      endDate = pred (date db2)
      period = DebtPeriod (date db1) (Just endDate) (abs b)
  in if b >= 0
     then getPeriods (db2:dbs)
     else [period] ++ (getPeriods (db2:dbs))
