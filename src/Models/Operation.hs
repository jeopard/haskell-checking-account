module Models.Operation (
  Operation (..),
  OperationType (..),
  amountWithSign,
  calculateBalance
  ) where

import Data.Scientific
import Data.Time.Calendar
import Data.UUID


-- represents credit and debit operations performed in a bank account
data Operation = Operation { operationId   :: UUID
                           , accountId     :: UUID
                           , operationType :: OperationType
                           , date          :: Day
                           , amount        :: Scientific
                           , description   :: String
                           } deriving (Show)


data OperationType = Credit | Debit deriving (Show)


instance Eq Operation where
  x == y = operationId x == operationId y


-- implementing this instance so that we can use Operations inside Sets
instance Ord Operation where
  compare x y = compare (operationId x) (operationId y)


-- return a negative amount if the operation was debit
amountWithSign :: Operation -> Scientific
amountWithSign op = let a = amount op
                    in case (operationType op) of Credit -> a
                                                  Debit -> -a


-- calculate the sum of the amounts of a list of operations
calculateBalance :: [Operation] -> Scientific
calculateBalance ops = let startingValue = (scientific 0 0)
                           sumFunction = (\op acc -> acc + (amountWithSign op))
                       in foldr sumFunction startingValue ops
