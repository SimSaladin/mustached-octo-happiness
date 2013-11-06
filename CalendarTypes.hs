{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CalendarTypes where

import Prelude
import Yesod
import Data.Text (Text)
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

-- * In database

-- TODO not implemented
newtype Repeat = Repeat Text deriving (PersistField, PersistFieldSql)
newtype Alarm = Alarm Text deriving (PersistField, PersistFieldSql)
newtype Urgency = Urgency Int deriving (PersistField, PersistFieldSql)
