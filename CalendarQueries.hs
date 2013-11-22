{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | This module provides calendar queries and some relevent session
-- helpers.
module CalendarQueries where

import Prelude
import Control.Monad
import Control.Applicative
import Data.Typeable (Typeable)
import Database.Esqueleto
import Yesod (HandlerT, HandlerSite, runDB, cached, lookupSession, setSession)
import Yesod.Auth (requireAuthId)
import Yesod.Persist.Core (YesodPersist, YesodPersistBackend)
import qualified Data.Text as T

import Model

-- | Calendar info wrapper. Memoized.
newtype CalendarInfo = CalendarInfo
                       { unCalendarInfo :: [(Entity Calendar, Value Int)] }
                       deriving (Typeable)

-- queryCalendarInfo :: HandlerT App IO [(Entity Calendar, Value Int)]
queryCalendarInfo = do
    uid <- requireAuthId
    fmap unCalendarInfo . cached . fmap CalendarInfo .  runDB . select $
        from $ \(c `LeftOuterJoin` mt) -> do
            on      $ just (c ^. CalendarId) ==. (mt ?. CalTargetCalendar)
            groupBy $ c ^. CalendarId
            where_  $ c ^. CalendarOwner ==. val uid
            orderBy [asc $ c ^. CalendarName]
            return (c, count $ mt ?. CalTargetId)

queryCalendarTargets from to = do
    uid <- requireAuthId
    runDB $ select $ from $ \(c ) -> do
        return undefined

-- | Lookup active calendar. In most cases only looks up the session key
-- "calendar".
activeCalendar = -- activeCalendar :: HandlerT ... CalendarId
        lookupSession "calendar" >>= maybe defcal (return . read . T.unpack)
    where
        -- case select is empty should be impossible; calendar is
        -- not even loaded then (calendar creation form is shown).
        defcal = do
            (Value x : _) <- runDB $ select $ from $ \c -> do
                orderBy [asc $ c ^. CalendarId]
                return (c ^. CalendarId)
            setSession "calendar" (T.pack $ show x)
            return x


-- | Create a new target given values.
queryAddTarget :: (YesodPersist site,
                  PersistStore (YesodPersistBackend site (HandlerT site IO)),
                  PersistMonadBackend (YesodPersistBackend site (HandlerT site IO)) ~ SqlBackend,
                  PersistEntity a,
                  PersistEntityBackend a ~ SqlBackend)
                  => CalendarId -> (Target, TargetId -> a)
                  -> HandlerT site IO CalTargetId
queryAddTarget cid (t,f) = runDB $
    insert t >>= liftA2 (>>) (insert . f) (insert . CalTarget cid)

-- | Update values of a target.
queryModifyTarget targ uniq new = runDB $ do
    -- NOTE This could be optimized to one query, with the cost of
    -- verbosity: list out all fields and values in a "update...where
    -- target = TID"  query.
    mt <- getBy $ uniq $ targ new
    case mt of
        Nothing -> error "queryModifyTarget: non-existing target id"
        Just (Entity oid _) -> replace oid new
