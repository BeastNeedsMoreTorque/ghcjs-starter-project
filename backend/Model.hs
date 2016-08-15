{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
-- import Database.Persist
-- import Database.Persist.Sqlite
-- import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Count
    keyName  Text
    keyCount Int
    UniqueKeyName keyName
    deriving Show
|]
