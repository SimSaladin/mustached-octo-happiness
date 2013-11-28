-- | This module provides calendar queries and some relevent session
-- helpers.
module CalendarQueries where

import Prelude
import Control.Monad
import Control.Applicative
import Data.Time
import Data.Typeable        (Typeable)
import Database.Esqueleto
import Yesod                (HandlerT, runDB, cached, lookupSession, setSession, get404, getBy404, notFound, permissionDenied)
import Yesod.Auth           (requireAuthId, YesodAuth, AuthId)
import Yesod.Persist.Core   (YesodPersist, YesodPersistBackend)
import qualified Data.Text as T
import Model

-- * Types

-- | We do not have "App" in scope here, but have to convince the type
-- system that we can do database and auth operations.
type QueryHandler a =
        ( YesodPersist site
        , SqlPersistT ~ YesodPersistBackend site
        , YesodAuth site, AuthId site ~ UserId
        ) => HandlerT site IO a

-- | Target query reply has to quantify over different target types, hence
-- this sum type.
data TW = T1 (Entity Event) | T2 (Entity Todo) | T3 (Entity Note)


-- * Calendar

-- ** Select

-- *** Info

-- | Calendar info wrapper. Memoized.
newtype CalendarInfo = CalendarInfo
                       { unCalendarInfo :: [(Entity Calendar, Bool, Int)] -- ^ Calendar, is active, num of entries
                       } deriving (Typeable)

queryCalendarInfo :: QueryHandler CalendarInfo
queryCalendarInfo = do
    uid <- requireAuthId
    xs  <- getViewCalendars
    let f (ent, Value n) = (ent, entityKey ent `elem` xs, n)

    cached . runDB . fmap (CalendarInfo . map f) . select . from $
        \(c `LeftOuterJoin` mt) -> do
            on      (just (c ^. CalendarId) ==. mt ?. CalTargetCalendar)
            groupBy (c ^. CalendarId)
            where_  (c ^. CalendarOwner ==. val uid)
            orderBy [asc $ c ^. CalendarName]
            return (c, count $ mt ?. CalTargetId)

-- *** Individual

-- | Documentation for 'queryCalendar'
queryCalendar :: CalendarId -> QueryHandler Calendar
-- XXX: use above cached calendarinfo?
queryCalendar cid = runDB $ get404 cid

-- *** Viewed

getViewCalendars :: QueryHandler [CalendarId]
getViewCalendars = do
        mcals <- lookupSession "active_calendars"
        case mcals of
            Just cals -> return $ read $ T.unpack cals
            Nothing   -> updateViewSession

updateViewSession :: QueryHandler [CalendarId]
updateViewSession = do
    uid <- requireAuthId
    cals <- liftM (map unValue) . runDB . select . from $ \c -> do
        where_ $ c ^. CalendarOwner ==. val uid
        orderBy [ asc $ c ^. CalendarId ]
        return $ c ^. CalendarId

    setSession "active_calendars" (T.pack $ show cals)
    return cals

-- *** Active

-- | Lookup active calendar. In most cases only looks up the session key
-- "calendar". Result is Nothing if user has no calendars
activeCalendar :: QueryHandler (Maybe CalendarId)
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

setActiveCalendar :: CalendarId -> QueryHandler ()
setActiveCalendar = setSession "calendar" . T.pack . show

-- ** Insert

queryInsertCalendar :: Calendar -> QueryHandler ()
queryInsertCalendar cal = runDB (insert cal)
    >>= setActiveCalendar >> updateViewSession >> return ()

-- ** Delete

-- | delete a calendar by id, along with its target associations following
-- with orphan calendar targets.
queryDeleteCalendar :: CalendarId -> QueryHandler ()
queryDeleteCalendar cid = do
        runDB $ do
            -- caltarget relations
            delete $ from $ \ct -> where_ (ct ^. CalTargetCalendar ==. val cid)

            -- find orphan targets' ids.
            let orphansQ = subList_select . from $ \target -> do
                    where_ . notExists .  from
                        $ \ct -> where_ (ct ^. CalTargetTarget ==. target ^. TargetId)
                    return (target ^. TargetId)

            -- orphan specializations
            delete $ from $ \event -> where_ (event ^. EventTarget `in_` orphansQ)
            delete $ from $ \todo  -> where_ (todo ^. TodoTarget   `in_` orphansQ)
            delete $ from $ \note  -> where_ (note ^. NoteTarget   `in_` orphansQ)

            -- orphan targets
            delete $ from $ \target -> where_ (target ^. TargetId `in_` orphansQ)

            -- the calendar
            deleteKey cid

-- | Update a calendar's settings.
queryUpdateCalendar :: CalendarId -> Calendar -> QueryHandler ()
queryUpdateCalendar cid = runDB . replace cid


-- * Target

-- ** Select many

queryCalendarObjects ::  [CalendarId] -- ^ Calendars to query
                     -> Day     -- ^ Timeframe start (inclusive)
                     -> Day     -- ^ Timeframe end  (inclusive)
                     -> QueryHandler ([(Entity Target, Entity Event)], [(Entity Target, Entity Todo)])
queryCalendarObjects calendars begin end =
        runDB $ liftM2 (,) queryEvents queryTodos
    where
        queryEvents = select $ from $ \(ct `InnerJoin` target `InnerJoin` event) -> do

            let onTid = on . (==. target ^. TargetId)
            onTid (event ^. EventTarget)
            onTid (ct    ^. CalTargetTarget)

            where_ $ ct ^. CalTargetCalendar `in_` valList calendars
            where_ $ event ^. EventBegin >=. val begin      -- event-specific
            where_ $ event ^. EventEnd   <=. just (val end)
            return (target, event)

        queryTodos = select $ from $ \(ct `InnerJoin` target `InnerJoin` todo) -> do

            let onTid = on . (==. target ^. TargetId)
            onTid (todo ^. TodoTarget)
            onTid (ct   ^. CalTargetTarget)

            where_ $ ct ^. CalTargetCalendar `in_` valList calendars
            where_ $ todo ^. TodoBegin >=. val begin        -- todo-specific
            where_ $ todo ^. TodoEnd   <=. just (val end)
            return (target, todo)

queryCalendarNotes :: [CalendarId] -> QueryHandler [ (Entity Target, Entity Note) ]
queryCalendarNotes calendars = runDB $
    select $ from $ \(ct `InnerJoin` target `InnerJoin` note) -> do

        let onTid = on . (==. target ^. TargetId)
        onTid  (note ^. NoteTarget)
        onTid  (ct   ^. CalTargetTarget)

        where_ (ct   ^. CalTargetCalendar `in_` valList calendars)
        orderBy [ asc $ note ^. NoteTarget ]
        return (target, note)

-- ** Select single

-- | Get a single target by TargetId, along with calendars it resides in.
queryTarget :: TargetId -> QueryHandler (Target, [CalendarId], TW)
queryTarget tid = do
    res <- queryTarget' tid
    case res of
        ([]                    ,  _) -> notFound
        (x@(Entity _ t,_,_,_):_, cs) -> case x of
            (_,Just note,_,_)  -> return (t, cs, T3 note)
            (_,_,Just todo,_)  -> return (t, cs, T2 todo)
            (_,_,_,Just event) -> return (t, cs, T1 event)
            _                  -> error
                "queryTarget: this should not happen, as this indicates a orphan Target entry in db."

-- | Query target info. Second result list is always 0 or 1 in length, 0 indicates
-- no matches. We return only CalendarId's, as calendar info is already
-- cached on every page in CalendarInfo.
queryTarget' :: TargetId -> QueryHandler
            ( [ ( Entity Target
                , Maybe (Entity Note)
                , Maybe (Entity Todo)
                , Maybe (Entity Event)) ]
            , [ CalendarId ] )
queryTarget' tid = runDB $ do
    ts <- select . from $
        \(target `LeftOuterJoin` event `LeftOuterJoin` note `LeftOuterJoin` todo) -> do

            let onTid = on . (==. just (target ^. TargetId))

            onTid $ todo  ?. TodoTarget
            onTid $ note  ?. NoteTarget
            onTid $ event ?. EventTarget

            where_ $ target ^. TargetId ==. val tid
            return (target, note, todo, event)

    cs <- select . from $
        \ct -> do
            where_ (ct ^. CalTargetTarget ==. val tid)
            return (ct ^. CalTargetCalendar)

    return (ts, map unValue cs)

-- ** Insert

-- | Create a new target given values.
queryAddTarget :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend)
               => CalendarId
               -> (Target, TargetId -> a)
               -> QueryHandler CalTargetId
queryAddTarget cid (t,f) = runDB $
    -- insert target, then specialization, finally associate to the calendar
    insert t >>= liftA2 (>>) (insert . f) (insert . CalTarget cid)

-- ** Update (replace)

-- | Update values of a target.
queryModifyTarget :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend)
                  => (a -> TargetId)
                  -> (TargetId -> Unique a)
                  -> a
                  -> QueryHandler ()
queryModifyTarget toTid uniq new = let
    tid = toTid new
    in do runDB (get404 tid) >>= guardTarget "Ei oikeutta muokata toisen kohdetta."
          runDB $ getBy404 (uniq tid) >>= flip replace new . entityKey

-- ** Delete

-- | Delete a target and it's instances from calendars.
queryDeleteTarget :: TargetId -> QueryHandler ()
queryDeleteTarget tid = do
    runDB (get404 tid) >>= guardTarget "Ei oikeutta poistaa toisen kohdetta."
    runDB $ del CalTargetTarget
          >> del NoteTarget >> del TodoTarget >> del EventTarget
          >> del TargetId
    where
        -- | Delete where given targetId tid matches to field and table
        -- defined by argument.
        del = delete . from . (at . ) . flip (^.)

        -- combinators <3, haskell's monomorphism </3; above won't work
        -- without this general sig in this function.
        at :: Esqueleto query expr back => expr (Value TargetId) -> query ()
        at  = where_ . (==. val tid)


-- * Helpers

guardTarget :: T.Text -> Target -> QueryHandler ()
guardTarget msg t = do uid <- requireAuthId
                       if (targetOwner t /= uid)
                           then permissionDenied msg
                           else return ()

unValue :: Value a -> a
unValue (Value x) = x
