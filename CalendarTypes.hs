{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module CalendarTypes where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time
import Database.Persist.Sql

-- * In routing

-- FIXME should be an UUID for partability
type TargetUID = Int

data TargetType = TargetTodo
                | TargetEvent
                | TargetNote
                deriving (Show, Eq, Read)

instance PathPiece TargetType where
        toPathPiece TargetTodo  = "todo"
        toPathPiece TargetEvent = "event"
        toPathPiece TargetNote  = "note"
        fromPathPiece "todo"  = Just TargetTodo
        fromPathPiece "event" = Just TargetEvent
        fromPathPiece "note"  = Just TargetNote
        fromPathPiece     _   = Nothing

-- * In db

type DayOfWeek = Int

data RepeatTime = Weekly [DayOfWeek]
        deriving (Show, Read)

data Repeat = Repeat
        { repeatWhen  :: RepeatTime
        , repeatStart :: TimeOfDay
        , repeatEnd   :: TimeOfDay
        } deriving (Show, Read)
instance PersistField Repeat where
        toPersistValue   = toPersistValue . show
        fromPersistValue = fmap read . fromPersistValue
instance PersistFieldSql Repeat where
        sqlType _ = SqlString

newtype Alarm = Alarm Text      deriving (Eq, PersistField, PersistFieldSql)
newtype Urgency = Urgency Int   deriving (Eq, PersistField, PersistFieldSql)

-- instance PersistField DiffTime where
--         toPersistValue dt  = toPersistValue (floor dt :: Int64)
--         fromPersistValue v = fmap fromIntegral ( fromPersistValue v :: Either Text Int64 )
-- 
-- instance PersistFieldSql DiffTime where
--         sqlType _ = SqlInt64
