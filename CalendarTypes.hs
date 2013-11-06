{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module CalendarTypes where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time
import Data.Int
import Database.Persist.Sql

-- * In routing

type TargetUID = Text

data TargetType = TargetTask
                | TargetEvent
                | TargetNote
                deriving (Show, Eq, Read)

instance PathPiece TargetType where
        toPathPiece TargetTask = "task"
        toPathPiece TargetEvent = "event"
        toPathPiece TargetNote = "note"

        fromPathPiece "task"  = Just TargetTask
        fromPathPiece "event" = Just TargetEvent
        fromPathPiece "note"  = Just TargetNote
        fromPathPiece     _   = Nothing

instance PersistField DiffTime where
        toPersistValue dt  = toPersistValue (floor dt :: Int64)
        fromPersistValue v = fmap fromIntegral ( fromPersistValue v :: Either Text Int64 )

instance PersistFieldSql DiffTime where
        sqlType _ = SqlInt64

-- * In database

-- TODO not implemented
newtype Repeat = Repeat Text    deriving (PersistField, PersistFieldSql)
newtype Alarm = Alarm Text      deriving (PersistField, PersistFieldSql)
newtype Urgency = Urgency Int   deriving (PersistField, PersistFieldSql)
