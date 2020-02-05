{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Controllers.AccountResources (routes) where

import Data.Time.Calendar
import Data.UUID
import Network.HTTP.Types.Status
import Text.Read (readMaybe)
import Web.Scotty.Trans

import Core
import Models.Operation
import Persistence.Accounts
import Persistence.Operations
import Serializers.DebtPeriod
import Serializers.Response
import Serializers.Statement

import qualified Models.DebtPeriod as DP
import qualified Models.Statement as S
import qualified Persistence.Database as DB

-- These are the nested routes under /accounts/:id/.
-- They provide functionality related to a given account
routes :: AppScottyM ()
routes = do

  get "/accounts/:accId/balance" $ do
    accId <- param "accId"

    -- if not UUID, it's not an account ID, so 404
    let maybeUuid = fromString accId
    case maybeUuid of
      Nothing -> accountNotFound
      Just accUuid -> do

        -- returns the transations or Nothing when the acocunt is not found
        maybeOperations <- transaction $ getOperationsTransaction accUuid

        case maybeOperations of
          Nothing -> accountNotFound
          Just operations -> do
            let balance = calculateBalance operations
            json $ Success balance

  get "/accounts/:accId/debtPeriods" $ do
    accId <- param "accId"

    -- if not UUID, it's not an account ID, so 404
    let maybeUuid = fromString accId
    case maybeUuid of
      Nothing -> accountNotFound
      Just accUuid -> do

        -- returns the transations or Nothing when the acocunt is not found
        maybeOperations <- transaction $ getOperationsTransaction accUuid

        case maybeOperations of
          Nothing -> accountNotFound
          Just operations -> do
            let debtPeriods = DP.fromOperations operations
                serializers = map (\dp -> DebtPeriodSerializer dp) debtPeriods
            json $ Success serializers

  get "/accounts/:accId/statement" $ do
    eitherDates <- eitherFromToDates
    -- first try to parse dates, and fail with 400 if there's an issue
    case eitherDates of
      Left e -> do
        status badRequest400
        json $ Error e
      Right (fromDate, toDate) -> do

        -- make sure the account ID is UUID and the account exists
        accId <- param "accId"
        let maybeUuid = fromString accId
        case maybeUuid of
          Nothing -> accountNotFound
          Just accUuid -> do

            -- returns the transations or Nothing when the acocunt is not found
            maybeOperations <- transaction $ getOperationsTransaction accUuid

            case maybeOperations of
              Nothing -> accountNotFound
              Just operations -> do
                let statement = S.fromOperations operations fromDate toDate
                json $ Success $ StatementSerializer statement


-- returns the fromDate and toDate query params or error if we can't extract
eitherFromToDates :: AppActionM (Either String (Day, Day))
eitherFromToDates = do
  maybeFromDateString <- (Just <$> param "fromDate") `rescue` const (return Nothing)
  maybeToDateString <- (Just <$> param "toDate") `rescue` const (return Nothing)

  case (maybeFromDateString, maybeToDateString) of
    (Nothing, _) -> return $ Left "did not find fromDate query param"
    (_, Nothing) -> return $ Left "did not find toDate query param"
    (Just fromDateString, Just toDateString) -> do
      let maybeFromDate = readMaybe fromDateString
          maybeToDate   = readMaybe toDateString

      case (maybeFromDate, maybeToDate) of
        (Nothing, _) -> return $ Left $ "could not parse fromDate"
        (_, Nothing) -> return $ Left $ "could not parse toDate"
        (Just fromDate, Just toDate) -> do
          if fromDate > toDate
            then return $ Left "fromDate cannot be greater than toDate"
            else return $ Right (fromDate, toDate)
  

accountNotFound :: AppActionM ()
accountNotFound = do
  status notFound404
  json $ Error "Account not found"


-- if there is an account, it returns its operations, otherwise it returns an error message
getOperationsTransaction :: UUID -> DB.Database -> (DB.Database, Maybe [Operation])
getOperationsTransaction accId db =
  let maybeAccount = getAccount accId db
      -- this gets used only if the account is found
      operations = getOperations accId db
  in case maybeAccount of
       Nothing -> (db, Nothing)
       Just _  -> (db, Just operations)
