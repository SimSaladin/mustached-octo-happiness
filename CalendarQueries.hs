{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CalendarQueries where

import Prelude
import Yesod (runDB, cached)
import Yesod.Auth (requireAuthId)
import Database.Esqueleto
import Data.Typeable (Typeable)

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


