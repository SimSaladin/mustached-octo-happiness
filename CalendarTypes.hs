{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Here we define the foundational types used by rest of the application.
module CalendarTypes where

import ClassyPrelude
import Data.Time
import Data.Maybe
import Data.Time.Calendar.WeekDate
import Database.Persist.Sql
import Utils
import Control.Monad
import Control.Applicative
import System.Locale
import Yesod

-- * Application routes

-- FIXME should be an UUID for portability
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

-- * Calendar objects

type DayOfWeek = Int

data RepeatTime = Weekly [DayOfWeek]
        deriving (Show, Read)

unWeekly :: RepeatTime -> [DayOfWeek]
unWeekly (Weekly x) = x

data Repeat = Repeat
        { repeatWhen  :: RepeatTime
        , repeatStart :: TimeOfDay
        , repeatEnd   :: TimeOfDay
        } deriving (Show, Read)
instance PersistField Repeat where
        toPersistValue   = toPersistValue . show
        fromPersistValue = fmap (fromJust . readMay :: Text -> Repeat) . fromPersistValue
instance PersistFieldSql Repeat where
        sqlType _ = SqlString

newtype Alarm = Alarm Text      deriving (Eq, PersistField, PersistFieldSql, Show)
newtype Urgency = Urgency Int   deriving (Eq, PersistField, PersistFieldSql, Show)

-- * Functions

-- | Calculate next occurances of a Repeat at given days. Just don't call
-- it with infinite days and finite range; it won't return when evaluated.
nextRepeatsAt :: [Day] -> Day -> Maybe Day -> Repeat -> [(LocalTime, LocalTime)]
nextRepeatsAt xs lower upper rep = concatMap f xs
    where
        f d | dayLimits d && weekDayLimits d = [toframe d]
            | otherwise                      = []

        dayLimits = case upper of Nothing -> (>= lower)
                                  Just u  -> liftA2 (&&) (>= lower) (<= u)

        toframe = (,) <$> flip LocalTime (repeatStart rep)
                      <*> flip LocalTime (repeatEnd rep)

        weekDayLimits d = let (_,_,d') = toWeekDate d in d' `elem` weekDays
        weekDays        = unWeekly $ repeatWhen rep

dayDefault :: Maybe Day -> HandlerT m IO Day
dayDefault mday = case mday of
        Just day -> return day
        Nothing  -> liftM (localDay . zonedTimeToLocalTime) lookupTimeAt

-- | Lookup "at" get param, or current (server) time
lookupTimeAt :: HandlerT m IO ZonedTime
lookupTimeAt = do
    mt <- lookupGetParam "at"
    case mt of
        Nothing -> 
            -- XXX: user specified time zone instead of server time zone?
            liftIO $ liftM2 utcToZonedTime getCurrentTimeZone getCurrentTime
        Just t  -> return $ read' t

-- | Lookup the weekstart get parameter and use that as first day in view.
-- Otherwise use the current day and next 6 days.
getViewTimeframe :: HandlerT m IO (Day, Day)
getViewTimeframe = liftM (liftA2 (,) id (addDays 6)) $ do
    mbegin <- lookupGetParam "weekstart"
    case mbegin of
        Just begin ->
            maybe (invalidArgs []) return $ readMay begin
        Nothing    -> liftM utctDay $ liftIO getCurrentTime

-- | Week day names
days :: [Text]
days = [ "Ma", "Ti", "Ke", "To", "Pe", "La", "Su" ]

myLocale :: TimeLocale
myLocale = defaultTimeLocale -- TODO finnish hacks

formatWeekday :: FormatTime t => t -> String
formatWeekday = formatTime myLocale "%d.%m %a"

cellTimeFormat :: FormatTime t => t -> String
cellTimeFormat = formatTime myLocale "%H:%M"

formatTimeFrame :: FormatTime t => t -> t -> String
formatTimeFrame b e = formatTime myLocale "%d.%m klo. %H:%M - " b <>
                      formatTime myLocale "%H:%M" e

formatWeekNumber :: FormatTime t => t -> String
formatWeekNumber = formatTime myLocale "%V"
