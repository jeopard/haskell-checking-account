module Core (
  AppScottyM,
  AppActionM,
  transaction,
  updateDb,
  fetchDb
  ) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Text.Lazy
import Web.Scotty.Trans

import qualified Persistence.Database as DB

type AppScottyM = ScottyT Text (ReaderT (TVar DB.Database) IO)
type AppActionM = ActionT Text (ReaderT (TVar DB.Database) IO)


-- the function passed receives the current DB as an argument, processes its data,
-- and returns the next state of the DB + a value extracted from the DB. That
-- value is returned back to the caller
transaction :: (DB.Database -> (DB.Database, a)) -> AppActionM a
transaction f = do
  tvar <- lift ask
  liftIO $ atomically $ do
    currentDbState <- readTVar tvar
    let (nextDbState, result) = f currentDbState
    
    modifyTVar' tvar (\_ -> nextDbState)

    return result


-- similar to transaction above, but it only updates the DB without reading any data
updateDb :: (DB.Database -> DB.Database) -> AppActionM ()
updateDb f = do
  tvar <- lift ask
  liftIO $ atomically $ do

    modifyTVar' tvar f
    return ()


-- similar to transaction above, but it only returns a value from the DB without updating
fetchDb :: (DB.Database -> a) -> AppActionM a
fetchDb f = do
  tvar <- lift ask
  liftIO $ atomically $ do
    currentDbState <- readTVar tvar
    let result = f currentDbState
    
    return result
