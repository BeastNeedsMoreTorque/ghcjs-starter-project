{-# LANGUAGE ConstraintKinds #-}

module Handler.API where

import Import

import Control.Monad.Logger
import Database.Persist.Sql
import Database.Persist.Sqlite
import Model

type ControlIO m = (MonadIO m, MonadBaseControl IO m)

type DBM m a =
  (ControlIO m, MonadThrow m, Applicative m, Functor m) => SqlPersistT m a

type DB a = forall m . DBM m a

runDevDB :: DB a -> IO a
runDevDB action = runNoLoggingT $
  withSqlitePool "backend.sqlite3" 1
    $ \pool -> liftIO $ runSqlPool action pool

getCountByKeyName :: Text -> DB (Maybe (Entity Count))
getCountByKeyName keyName = selectFirst ([CountKeyName ==. keyName]) []

incrementCount :: Text -> DB (Entity Count)
incrementCount keyName = do
  existingCount <- getCountByKeyName keyName
  case existingCount of
    Nothing -> do
      let countVal = Count keyName 1
      countKey <- insert countVal
      return (Entity countKey countVal)
    (Just eCount@(Entity countKey Count{..})) -> do
      update countKey [CountKeyCount +=. 1]
      return (Entity countKey (Count countKeyName (countKeyCount + 1)))

getCountR :: Handler Value
getCountR = do
  mKeyName <- lookupGetParam "key"
  case mKeyName of
    Nothing -> sendResponseStatus status400
               ("Invalid request, must include key name in get parameters." :: Text)
    (Just keyName) -> do
      maybeCount <- runDB $ getCountByKeyName keyName
      case maybeCount of
        Nothing -> notFound
        (Just (Entity _ Count{..})) ->
          return $ object $ [keyName .= countKeyCount]

data CountIncrementRequest =
  CountIncrementRequest {
    incrementKeyName :: Text
  } deriving (Eq, Generic, Show)

-- {"incrementKeyName": "chris"}
instance FromJSON CountIncrementRequest

-- curl -XPOST -d "{\"incrementKeyName\": \"chris\"}" 'http://localhost:3000/api/v1/count'

postCountR :: Handler Value
postCountR = do
  CountIncrementRequest{..} <- requireJsonBody
  (Entity _ Count{..}) <- runDB $ incrementCount incrementKeyName
  return $ object $ [incrementKeyName .= countKeyCount]
