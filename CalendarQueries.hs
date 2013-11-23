-- | This module provides calendar queries and some relevent session
-- helpers.
module CalendarQueries where

import Prelude
import Control.Monad
import Control.Applicative
import Data.Time
import Data.Typeable        (Typeable)
import Database.Esqueleto
import Yesod                (HandlerT, runDB, cached, lookupSession, setSession)
import Yesod.Auth           (requireAuthId, YesodAuth, AuthId)
import Yesod.Persist.Core   (YesodPersist, YesodPersistBackend)
import qualified Data.Text as T
import Model

-- * Calendar

insertCalendar :: (YesodPersist site, SqlPersistT ~ YesodPersistBackend site, YesodAuth site, AuthId site ~ UserId)
               => Calendar -> HandlerT site IO ()
insertCalendar cal = runDB (insert cal)
    >>= setActiveCalendar >> updateViewCalendars >> return ()

-- * Info

-- | Calendar info wrapper. Memoized.
newtype CalendarInfo = CalendarInfo
                       { unCalendarInfo :: [(Entity Calendar, Bool, Int)] -- ^ Calendar, is active, num of entries
                       } deriving (Typeable)

queryCalendarInfo :: (YesodPersist site, SqlPersistT ~ YesodPersistBackend site, YesodAuth site, AuthId site ~ UserId)
                  => HandlerT site IO CalendarInfo
queryCalendarInfo = do
    uid <- requireAuthId
    xs  <- getViewCalendars
    let f (ent, Value n) = (ent, entityKey ent `elem` xs, n)

    cached . runDB . fmap (CalendarInfo . map f) . select . from $
        \(c `LeftOuterJoin` mt) -> do
            on      $ just (c ^. CalendarId) ==. (mt ?. CalTargetCalendar)
            groupBy $ c ^. CalendarId
            where_  $ c ^. CalendarOwner ==. val uid
            orderBy [asc $ c ^. CalendarName]
            return (c, count $ mt ?. CalTargetId)

getViewCalendars :: (YesodPersist site, SqlPersistT ~ YesodPersistBackend site, YesodAuth site, AuthId site ~ UserId)
                 => HandlerT site IO [CalendarId]
getViewCalendars = do
        mcals <- lookupSession "active_calendars"
        case mcals of
            Just cals -> return $ read $ T.unpack cals
            Nothing   -> updateViewCalendars

updateViewCalendars :: (YesodPersist site, SqlPersistT ~ YesodPersistBackend site, YesodAuth site, AuthId site ~ UserId)
                    => HandlerT site IO [CalendarId]
updateViewCalendars = do
    uid <- requireAuthId
    cals <- liftM (map unValue) . runDB . select . from $ \c -> do
        where_ $ c ^. CalendarOwner ==. val uid
        orderBy [ asc $ c ^. CalendarId ]
        return $ c ^. CalendarId

    setSession "active_calendars" (T.pack $ show cals)
    return cals
    where
        unValue (Value x) = x

-- * Active one

-- | Lookup active calendar. In most cases only looks up the session key
-- "calendar". Result is Nothing if user has no calendars
activeCalendar :: (YesodPersist site, SqlPersistT ~ YesodPersistBackend site, YesodAuth site, AuthId site ~ UserId)
               => HandlerT site IO (Maybe CalendarId)
activeCalendar =
        lookupSession "calendar" >>= maybe defcal (return . Just . read . T.unpack)
    where
        -- just select first calendar id.
        defcal = do
            uid <- requireAuthId
            xs <- runDB $ select $ from $ \c -> do
                where_ $ c ^.CalendarOwner ==. val uid
                orderBy [asc $ c ^. CalendarId]
                return (c ^. CalendarId)
            case xs of
                (Value x : _) -> setActiveCalendar x >> return (Just x)
                _             -> return Nothing

setActiveCalendar :: CalendarId -> HandlerT site IO ()
setActiveCalendar = setSession "calendar" . T.pack . show
    

-- * Target

queryCalendarObjects :: (YesodPersist site, SqlPersistT ~ YesodPersistBackend site, YesodAuth site, AuthId site ~ UserId)
                     => [CalendarId] -- ^ Calendars to query
                     -> Day     -- ^ Timeframe start (inclusive)
                     -> Day     -- ^ Timeframe end  (inclusive)
                     -> HandlerT site IO ([(Entity Target, Entity Event)], [(Entity Target, Entity Todo)])
queryCalendarObjects calendars begin end =
        runDB $ liftM2 (,) queryEvents queryTodos
    where
        queryEvents = select $ from $ \(ct `RightOuterJoin` target `RightOuterJoin` event) -> do
            on $ target ^. TargetId ==. event ^. EventTarget
            on $ target ^. TargetId ==. ct ^. CalTargetTarget
            where_ $ ct ^. CalTargetCalendar `in_` valList calendars
            where_ $ event ^. EventBegin >=. val begin
            where_ $ event ^. EventEnd   <=. just (val end)
            return (target, event)

        queryTodos = select $ from $ \(ct `RightOuterJoin` target `RightOuterJoin` todo) -> do
            on $ target ^. TargetId ==. todo ^. TodoTarget
            on $ target ^. TargetId ==. ct ^. CalTargetTarget
            where_ $ ct ^. CalTargetCalendar `in_` valList calendars
            where_ $ todo ^. TodoBegin >=. val begin
            where_ $ todo ^. TodoEnd   <=. just (val end)
            return (target, todo)

queryCalendarNotes :: (YesodPersist site, SqlPersistT ~ YesodPersistBackend site)
                   => [CalendarId] -> HandlerT site IO [ (Entity Target, Entity Note) ]
queryCalendarNotes calendars = runDB $
    select $ from $ \(ct `RightOuterJoin` target `RightOuterJoin` note) -> do
        on $ target ^. TargetId ==. note ^. NoteTarget
        on $ target ^. TargetId ==. ct ^. CalTargetTarget
        where_ $ ct ^. CalTargetCalendar `in_` valList calendars
        orderBy [ asc $ note ^. NoteTarget ]
        return (target, note)

-- | Create a new target given values.
queryAddTarget :: (YesodPersist site, SqlPersistT ~ YesodPersistBackend site,
                  PersistEntity a, PersistEntityBackend a ~ SqlBackend)
                  => CalendarId -> (Target, TargetId -> a)
                  -> HandlerT site IO CalTargetId
queryAddTarget cid (t,f) = runDB $
    insert t >>= liftA2 (>>) (insert . f) (insert . CalTarget cid)

-- | Update values of a target.
queryModifyTarget :: (YesodPersist site, SqlPersistT ~ YesodPersistBackend site,
                     PersistEntity a, PersistEntityBackend a ~ SqlBackend)
                  => (a -> TargetId) -> (TargetId -> Unique a) -> a
                  -> HandlerT site IO ()
queryModifyTarget targ uniq new = runDB $ do
    -- NOTE This could be optimized to one query, with the cost of
    -- verbosity: list out all fields and values in a "update...where
    -- target = TID"  query.
    mt <- getBy $ uniq $ targ new
    case mt of
        Nothing -> error "queryModifyTarget: non-existing target id"
        Just (Entity oid _) -> replace oid new

