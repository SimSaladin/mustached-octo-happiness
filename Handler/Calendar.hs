{-# LANGUAGE TupleSections #-}
module Handler.Calendar where

import Import
import Control.Monad
import Control.Arrow
import Data.Maybe
import Data.Time
import Data.List as L
import Data.Ord
import qualified Data.Text as T
import System.Locale

read' :: Read a => Text -> a
read' = read . T.unpack

-- * Calendar

-- Type synonyms to help reasoning

-- | For every hour in every day there is a cell which contains objects
-- starting that hour (on that day).
type Cell = (TimeRange, (Entity Target, Either Event Todo))

-- | Every hour (a row) is a collection of cells associated with a day.
type HourView = [ (Day, [Cell]) ]

-- | A week is a list of HourViews (the list of rows) associated with the
-- hour.
type WeekView = [ (Hour, HourView) ]

type Hour = Int
type TimeRange = (LocalTime, LocalTime)
type Unfold a b = a -> Maybe (b, a)

-- | Calculate next occurances of a Repeat at given days.
nextRepeatsAt :: [Day] -> Day -> Maybe Day -> Repeat -> [(LocalTime, LocalTime)]
nextRepeatsAt xs lower upper rep = concatMap f xs
    where
        f d | d >= lower && maybe True (d <=) upper = let
                t = LocalTime d (repeatStart rep)
                in [(t,t)]
            | otherwise = []

getCalendarR :: Handler Html
getCalendarR = do
    uid              <- requireAuthId
    myCalendars      <- getViewCalendars
    mcal             <- activeCalendar
    (fromDay, toDay) <- getViewTimeframe
    (events, todos)  <- liftM (map (second entityVal) *** map (second entityVal))
                             (queryCalendarObjects myCalendars fromDay toDay)

    notes <- queryCalendarNotes myCalendars

    timezone <- liftIO getCurrentTimeZone -- TODO user supplied?

    let dayRange     = [fromDay..toDay]
        numOfObjects = length events + length todos

        objectWeeks :: WeekView
        objectWeeks = map (second sepDays) $ sepHours $ sortBy (comparing fst)
            $ (go eventRF Left events ++ go todoRF Right todos)
            where
                go :: (b -> [TimeRange]) -> (b -> Either Event Todo) -> [(Entity Target, b)] -> [Cell]
                go rs vs = concatMap $ zip <$> rs . snd <*> repeat . second vs
                eventRF  = nextRepeatsAt dayRange <$> eventBegin <*> eventEnd <*> eventRepeat
                todoRF   = nextRepeatsAt dayRange <$> todoBegin <*> todoEnd <*> todoRepeat

        -- TODO do something for these spaghetti monsters...

        sepHours :: [Cell] -> [(Hour, [Cell])]
        sepDays :: [Cell] -> [(Day, [Cell])]

        sepHours xs = L.unfoldr f (0, map getHour xs)
        sepDays  xs = L.unfoldr g (fromDay, map getDay xs)

        f :: Unfold (Hour, [(Hour, Cell)]) (Hour, [Cell])
        g :: Unfold (Day, [(Day, Cell)]) (Day, [Cell])
        f (24, _)       = Nothing
        f (h, xs)       = Just $ (h,) . map snd *** (h + 1,) $ L.span ((== h) . fst) xs
        g (d, xs)
            | d > toDay = Nothing
            | otherwise = Just $ (d,) . map snd *** (addDays 1 d,) $ L.span ((== d) . fst) xs

        getHour x@((t,_),_) = (todHour $ localTimeOfDay t, x)
        getDay  x@((t,_),_) = (localDay t, x)
        
    let targetParams :: Day -> Hour -> [(Text, Text)]
        targetParams day hour = let
            zonedtime = ZonedTime (LocalTime day $ TimeOfDay hour 0 0) timezone
            in [("at", T.pack $ show zonedtime)]

        getType (Left _)  = "event" :: Html
        getType (Right _) = "todo"

    defaultLayout $ do
        setTitle "Calendar"
        $(widgetFile "calendar")

getCalendarSettingsR :: Handler Html
getCalendarSettingsR = do
    defaultLayout $ do
        setTitle "Kalenteriasetukset"
        $(widgetFile "calendarsettings")

postCalendarSettingsR :: Handler Html
postCalendarSettingsR = do
    ((res, _), _) <- runFormPost newCalendarForm
    case res of
        FormSuccess cal -> do
            insertCalendar cal
            setMessage "Kalenteri luotu."
            redirect CalendarR
        FormFailure _ -> getCalendarSettingsR
        FormMissing   -> setMessage "Tyhjä lomake." >> redirect CalendarSettingsR

newCalendarWidget :: Widget
newCalendarWidget = do
    ((_, w), _) <- liftHandlerT $ runFormPost newCalendarForm
    $(widgetFile "calendarWidgetAdd")

-- * Time helpers

dayDefault :: Maybe Day -> Handler Day
dayDefault mday = case mday of
        Just day -> return day
        Nothing  -> liftM (localDay . zonedTimeToLocalTime) lookupTimeAt

lookupTimeAt :: Handler ZonedTime
lookupTimeAt = do
    mt <- lookupGetParam "at"
    case mt of
        Nothing -> 
            -- XXX: user specified time zone?
            liftIO $ liftM2 utcToZonedTime getCurrentTimeZone getCurrentTime
        Just t  -> return $ read' t

getViewTimeframe :: Handler (Day, Day)
getViewTimeframe = liftM (liftA2 (,) id (addDays 6) . utctDay) $
    liftIO getCurrentTime

days :: [Text]
days = [ "Ma", "Ti", "Ke", "To", "Pe", "La", "Su" ]

myLocale :: TimeLocale
myLocale = defaultTimeLocale -- TODO finnish hacks

formatWeekday :: FormatTime t => t -> String
formatWeekday = formatTime myLocale "%d.%m %a"

cellTimeFormat :: FormatTime t => t -> String
cellTimeFormat = formatTime myLocale "%H:%M"

-- * Targets

-- | View new target form.
--
-- CalendarId is used as a dummy here: POST form posts by default to the
-- origin url which already contains the id.
getTargetR :: CalendarId -> TargetType -> Handler Html
getTargetR _ TargetNote  = runTargetForm Nothing noteForm  >>= layoutNote
getTargetR _ TargetEvent = runTargetForm Nothing eventForm >>= layoutEvent
getTargetR _ TargetTodo  = runTargetForm Nothing todoForm  >>= layoutTodo

-- | Create a new target.
postTargetR :: CalendarId -> TargetType -> Handler Html
postTargetR cid tt = case tt of
    TargetEvent -> f layoutEvent eventForm eventTarget UniqueEvent
    TargetTodo  -> f layoutTodo  todoForm  todoTarget  UniqueTodo
    TargetNote  -> f layoutNote  noteForm  noteTarget  UniqueNote
  where
      -- Damn GHC.. won't quantify over "a" here unless explicitly told to :/
      -- (it would indeed be shorter to just copy-paste f's def...)
      f :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend)
        => (TargetFormRes a -> Handler Html) -> CalTargetForm a
        -> (a -> TargetId) -> (TargetId -> Unique a) -> Handler Html
      f = targetPostHelper cid tt Nothing

-- | View a target, or edit when GET param edit is set.
getTargetThisR :: TargetId -> Handler Html
getTargetThisR tid = do
    (t, v) <- queryTarget tid
    defaultLayout $ do
        setTitle $ toHtml $ targetName t
        $(widgetFile "viewtarget")

-- | Update a target, or delete it.
postTargetThisR :: TargetId -> Handler Html
postTargetThisR = undefined

-- | Export a target as text.
getTargetTextR :: TargetId -> Handler Html
getTargetTextR = undefined

-- | Send a target to another user.
postTargetSendR :: TargetId -> Handler Html
postTargetSendR = undefined

noteFormStandalone :: CalendarId -> Widget
noteFormStandalone cid = do
    ((_,formw), enctype) <- liftHandlerT $ runFormPost . noteForm Nothing =<< requireAuthId
    [whamlet|
<form .forms .forms-basic .forms-90 method=post action=@{TargetR cid TargetNote} enctype=#{enctype}>
    ^{formw}
    <p>
        <input .btn.btn-green.unit-90 type=submit value="Lisää muistiinpano">
|]

-- ** Helpers

targetPostHelper :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend)
                 => CalendarId
                 -> TargetType
                 -> Maybe a                           -- ^ Possible initial value
                 -> (TargetFormRes a -> Handler Html) -- ^ Layout (on failed)
                 -> CalTargetForm a                   -- ^ Target form
                 -> (a -> TargetId)                   -- ^ Extract targetId
                 -> (TargetId -> Unique a)            -- ^ Access target
                 -> Handler Html
targetPostHelper cid tt initial layout form toTid fromTid = do
    x@((res,_),_) <- runTargetForm initial form
    case res of
        FormSuccess s -> handler s >> redirect CalendarR
        FormFailure _ -> layout x
        FormMissing   -> redirect (TargetR cid tt)
  where
    handler (Left modified) = queryModifyTarget toTid fromTid modified
                              >> setMessage "Kohteen tiedot päivitetty."
    handler (Right tinfo)   = queryAddTarget cid tinfo
                              >> setMessage "Kohde onnistuneesti lisätty."

-- XXX: editing others' targets is possible!
runTargetForm :: Maybe a -> CalTargetForm a -> Handler (TargetFormRes a)
runTargetForm ival theForm = runFormPost . theForm ival =<< requireAuthId

layoutNote :: TargetFormRes Note -> Handler Html
layoutEvent :: TargetFormRes Event -> Handler Html
layoutTodo :: TargetFormRes Todo -> Handler Html
layoutNote  = targetLayout "muistiinpano" "green"
layoutEvent = targetLayout "tapahtuma" "blue"
layoutTodo  = targetLayout "tehtävä" "yellow"

targetLayout :: Html -> Html -> TargetFormRes a -> Handler Html
targetLayout what col ((res, formw), enctype) = 
    defaultLayout $ do
        setTitle $ "Uusi " <> what
        $(widgetFile "newtarget")

-- * Forms

type CalTargetAt a   = Either a (Target, TargetId -> a)
type CalTargetForm a = Maybe a -> UserId -> Form (CalTargetAt a)
type TargetFormRes a = ((FormResult (CalTargetAt a), Widget), Enctype)

class GetTarget a where
        getTarget :: a -> TargetId

instance GetTarget Todo where getTarget = todoTarget
instance GetTarget Event where getTarget = eventTarget
instance GetTarget Note where getTarget = noteTarget

-- ** Calendar
newCalendarForm :: Form Calendar
newCalendarForm = renderKube $ Calendar
    <$> lift requireAuthId
    <*> areq textField "Nimi" Nothing
    <*> aopt textField "Kuvaus" Nothing
    <*> areq textField "Väri" Nothing
    <*> areq checkBoxField "Julkinen" Nothing
    <*> areq checkBoxField "Julkisesti muokattava" Nothing

-- ** Targets

noteForm :: CalTargetForm Note
noteForm = calTargetForm' $ \mn -> Note
    <$> areq textareaField "Sisältö" (noteContent <$> mn)

eventForm :: CalTargetForm Event
eventForm = calTargetForm' $ \me -> (\f t r -> Event r f t)
    <$> myDayFieldReq      "Päivästä"       (eventBegin     <$> me)
    <*> myDayField         "Päivään"        (eventEnd       <$> me)
    <*> repeatForm                          (eventRepeat    <$> me)
    <*> aopt textField     "Paikka"         (eventPlace     <$> me)
    <*> areq urgencyField  "Tärkeys" (Just $ maybe (Urgency 2) eventUrgency me)
    <*> aopt alarmField    "Muistutus"      (eventAlarm     <$> me)
    <*> (fromMaybe [] <$> aopt attendeeField "Osallistujat"   (Just $ eventAttendees <$> me))
    <*> aopt textareaField "Kommentit"      (eventComment   <$> me)
        where
            attendeeField = checkMMap
                -- hmm.. could the ambigous Left value be defaulted by ghc?
                (return . (Right :: [Text] -> Either Text [Text]) . T.words)
                T.unwords textField

todoForm :: CalTargetForm Todo
todoForm = calTargetForm' $ \mt -> Todo
    <$> areq checkBoxField "Valmis"    (todoDone    <$> mt)
    <*> repeatForm                     (todoRepeat  <$> mt)
    <*> myDayFieldReq      "Aloitus"   (todoBegin   <$> mt)
    <*> myDayField         "Lopetus"   (todoEnd     <$> mt)
    <*> areq alarmField    "Muistutus" (todoAlarm   <$> mt)
    <*> areq urgencyField  "Tärkeys"   (todoUrgency <$> mt)

-- *** Helpers

calTargetForm' :: GetTarget ct => (Maybe ct -> AForm Handler (TargetId -> ct)) -> CalTargetForm ct
calTargetForm' ctform Nothing uid = renderKube $ ((Right .) . (,)) <$> targetForm uid <*> ctform Nothing
calTargetForm' ctform (Just ct) _ = renderKube $ (Left . ($ getTarget ct)) <$> ctform (Just ct)

targetForm :: UserId -> AForm Handler Target
targetForm uid = Target
    <$> pure uid
    <*> areq textField "Nimike" Nothing

-- ** Fields

alarmField :: Field Handler Alarm
alarmField = radioFieldList $ map (\x -> (x <> " min", Alarm x))
        ["10", "20", "30", "45", "60", "120"]

urgencyField :: Field Handler Urgency
urgencyField = radioFieldList
    [ ("Joutava" :: Text, Urgency 1)
    , ("Normaali", Urgency 2)
    , ("Tärkeä", Urgency 3)
    , ("Kriittinen", Urgency 4) ]

weekDaysField :: Field Handler RepeatTime
weekDaysField = checkMMap (return . f) unWeekly $ multiSelectFieldList $ zip days [1..]
    where
        f :: [Int] -> Either Text RepeatTime
        f = Right . Weekly

myDayField :: FieldSettings App -> Maybe (Maybe Day) -> AForm Handler (Maybe Day)
myDayField opts mday = formToAForm $ do
    day <- lift $ dayDefault (join mday)
    (r, v) <- mopt dayField opts . Just $ Just day
    return (r, [v])

myDayFieldReq :: FieldSettings App -> Maybe Day -> AForm Handler Day
myDayFieldReq opts mday = formToAForm $ do
    day <- lift $ dayDefault mday
    (r, v) <- mreq dayField opts $ Just day
    return (r, [v])

repeatForm :: Maybe Repeat -> AForm Handler Repeat
repeatForm info = formToAForm $ do
    (wd, sd, ed) <- case info of
        Nothing  -> do
            zd <- lift lookupTimeAt
            let tod = localTimeOfDay $ zonedTimeToLocalTime zd
            return (Weekly [1..7], tod, tod) -- TODO _3 +1h
        Just rep ->
            return $ liftA3 (,,) repeatWhen repeatStart repeatEnd $ rep

    (wr, wv) <- mreq weekDaysField "Toisto"  $ Just wd
    (sr, sv) <- mreq timeField "Alkaa klo."  $ Just sd
    (er, ev) <- mreq timeField "Loppuu klo." $ Just ed
    let myView = FieldView -- TODO fields not complete
            { fvErrors   = Nothing
            , fvId       = "repeat"
            , fvLabel    = "Klo."
            , fvRequired = True
            , fvTooltip  = Nothing
            , fvInput    = [whamlet|
<p>^{fvInput sv} - ^{fvInput ev}
|] }
    return (Repeat <$> wr <*> sr <*> er, [wv, myView])
