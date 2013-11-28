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

-- * Calendar

-- ** Types

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

-- ** CRUD

-- *** GET

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
        --
        objectWeeks :: WeekView
        objectWeeks = map (second sepDays) $ sepHours $ sortBy (comparing fst)
            $ (go eventRF Left events ++ go todoRF Right todos)
            where
                go :: (b -> [TimeRange]) -> (b -> Either Event Todo) -> [(Entity Target, b)] -> [Cell]
                go rs vs = concatMap $ zip <$> rs . snd <*> repeat . second vs
                eventRF  = nextRepeatsAt dayRange <$> eventBegin <*> eventEnd <*> eventRepeat
                todoRF   = nextRepeatsAt dayRange <$> todoBegin <*> todoEnd <*> todoRepeat

        -- TODO do something for these spaghetti monsters...
        --
        sepHours :: [Cell] -> [(Hour, [Cell])]
        sepDays :: [Cell] -> [(Day, [Cell])]
        sepHours xs = L.unfoldr f (0, map getHour xs)
        sepDays  xs = L.unfoldr g (fromDay, map getDay xs)
        --
        f :: Unfold (Hour, [(Hour, Cell)]) (Hour, [Cell])
        g :: Unfold (Day, [(Day, Cell)]) (Day, [Cell])
        f (24, _)       = Nothing
        f (h, xs)       = Just $ (h,) . map snd *** (h + 1,) $ L.span ((== h) . fst) xs
        g (d, xs)
            | d > toDay = Nothing
            | otherwise = Just $ (d,) . map snd *** (addDays 1 d,) $ L.span ((== d) . fst) xs
        --
        getHour x@((t,_),_) = (todHour $ localTimeOfDay t, x)
        getDay  x@((t,_),_) = (localDay t, x)
    --
    let targetParams :: Day -> Hour -> [(Text, Text)]
        targetParams day hour = let
            zonedtime = ZonedTime (LocalTime day $ TimeOfDay hour 0 0) timezone
            in [("at", T.pack $ show zonedtime)]
        --
        getType (Left _)  = "event" :: Html
        getType (Right _) = "todo"

    defaultLayout $ do
        setTitle "Calendar"
        $(widgetFile "calendar")

getCalendarSettingsR :: Handler Html
getCalendarSettingsR = do
    defaultLayout $ do
        setTitle "Kalenteriasetukset"
        $(widgetFile "calendar_settings")

-- | Documentation for 'getCalendarUpdateR'
getCalendarUpdateR :: CalendarId -> Handler Html
getCalendarUpdateR cid = do
    ((res,formw),enctype) <- runFormPost . calendarForm . Just =<< queryCalendar cid
    defaultLayout $ do
        setTitle "Muokataan kalenteria"
        $(widgetFile "calendar_update")

-- *** POST

postCalendarCreateR :: Handler Html
postCalendarCreateR = do
    ((res, _), _) <- runFormPost $ calendarForm Nothing
    case res of
        FormSuccess cal -> do
            queryInsertCalendar cal
            setMessage "Kalenteri luotu."
            redirect CalendarR
        FormFailure _ -> getCalendarSettingsR
        FormMissing   -> setMessage "Tyhjä lomake." >> redirect CalendarSettingsR

postCalendarUpdateR :: CalendarId -> Handler Html
postCalendarUpdateR cid = do
        cal <- queryCalendar cid
        ((res,_),_) <- runFormPost $ calendarForm $ Just cal
        case res of
            FormSuccess newcal -> do
                queryUpdateCalendar cid newcal
                setMessage "Kalenterin tiedot päivitetty"
                redirect CalendarR
            FormFailure _ -> getCalendarUpdateR cid
            FormMissing   -> setMessage "Tyhjä lomake." >> redirect (CalendarUpdateR cid)

-- | Delete a calendar by id.
postCalendarDeleteR :: CalendarId -> Handler Html 
postCalendarDeleteR cid = do
        queryDeleteCalendar cid
        setMessage "Kalenteri poistettu"
        redirect CalendarR

-- ** Pieces

calendarForm :: Maybe Calendar -> Form Calendar
calendarForm mcal = renderKube $ Calendar
    <$> maybe (lift requireAuthId) (pure . calendarOwner) mcal
    <*> areq textField "Nimi"           (calendarName <$> mcal)
    <*> aopt textField "Kuvaus"         (calendarDesc <$> mcal)
    <*> areq colorField "Väri"          (Just $ maybe "green" calendarColor mcal)
    <*> areq checkBoxField "Julkinen"   (calendarPublic <$> mcal)
    <*> areq checkBoxField "Julkisesti muokattava" (calendarPublicedit <$> mcal)

calendarListing :: Widget
calendarListing = do
    cinfo <- liftHandlerT queryCalendarInfo
    $(widgetFile "calendar_listing")

newCalendarWidget :: Widget
newCalendarWidget = do
    ((res, w), enctype) <- liftHandlerT $ runFormPost $ calendarForm Nothing
    $(widgetFile "calendar_form")


-- * Targets

-- ** Types

-- | Calendar target forms take possible initial value and userid as
-- paramaters.
type CalTargetForm a = Maybe a -> UserId -> Form (CalTargetAt a)

-- | Result from running a calendar target form.
type TargetFormRes a = ((FormResult (CalTargetAt a), Widget), Enctype)

-- | Calendar target form result is either the updated value of a existing
-- target (if the initial value was provided) or the target and a function
-- which takes targetid to a target specialization.
type CalTargetAt a = Either a (Target, TargetId -> a)

-- ** CRUD

-- | View new target form.
--
-- CalendarId is used as a dummy here: POST form posts by default to the
-- origin url which already contains the id.
getTargetCreateR :: CalendarId -> TargetType -> Handler Html
getTargetCreateR cid tt = case tt of
    TargetNote  -> go (getTargetForm :: CalTargetForm Note)
    TargetEvent -> go (getTargetForm :: CalTargetForm Event)
    TargetTodo  -> go (getTargetForm :: CalTargetForm Todo)
    where
        go a = runTargetForm Nothing a >>= getTargetFormLayout Nothing

-- | View a target, or update form when GET param edit is set.
getTargetReadR :: TargetId -> Handler Html
getTargetReadR tid = do
    (t, cs, v) <- queryTarget tid

    today <- liftM utctDay (liftIO getCurrentTime) -- FIXME zoned instead
    let getRepeats start end = take 7 . nextRepeatsAt [today..addDays 30 today] start end

    defaultLayout $ do
        setTitle $ toHtml $ targetName t
        $(widgetFile "target_single")

getTargetUpdateR :: TargetId -> Handler Html
getTargetUpdateR tid = do
    (t, _cs, v) <- queryTarget tid           -- TODO modify calendars?

    let -- don't remove sig, ghc specializes otherwise
        go :: GetTarget a => a -> Handler Html
        go a = runTargetForm (Just a) getTargetForm >>= getTargetFormLayout (Just $ targetName t)

    case v of
        T1 (Entity _ event) -> go event
        T2 (Entity _ todo)  -> go todo
        T3 (Entity _ note)  -> go note

-- *** POST

-- | Create the new target of given type from submitted form data.
postTargetCreateR :: CalendarId -> TargetType -> Handler Html
postTargetCreateR cid ttype =
    -- Actual work is delegated to target-type specific handlers which in
    -- turn delegate to the general handler.  Here we do one thing:
    -- quantify over correct type to choose correct handler (the magic is
    -- in the Nothing constructors).
    case ttype of
        TargetEvent -> go (Nothing :: Maybe (Target, Event))
        TargetTodo  -> go (Nothing :: Maybe (Target, Todo))
        TargetNote  -> go (Nothing :: Maybe (Target, Note))
    where
        -- don't remove the sig, ghc specializes otherwise
        go :: GetTarget a => Maybe (Target, a) -> Handler Html
        go = targetPostHelper cid ttype

postTargetUpdateR :: TargetId -> Handler Html
postTargetUpdateR tid = do
    -- TODO modify target's calendars?
    (target, _cs, v) <- queryTarget tid

    let -- don't remove sig, ghc specializes otherwise.
        go :: GetTarget a => TargetType -> a -> Handler Html
        go tt a = targetPostHelper undefined tt (Just (target, a))

    case v of
        T1 (Entity _ event) -> go TargetEvent event
        T2 (Entity _ todo)  -> go TargetTodo  todo
        T3 (Entity _ note)  -> go TargetNote  note

-- | Delete a target.
postTargetDeleteR :: TargetId -> Handler Html
postTargetDeleteR tid = do
        queryDeleteTarget tid
        setMessage "Kohde poistettu."
        redirect CalendarR

-- ** Other

-- | Export a target as text.
getTargetTextR :: TargetId -> Handler Html
getTargetTextR = error "Tulossa pian :)"

-- | Send a target to another user.
postTargetSendR :: TargetId -> Handler Html
postTargetSendR = error "Tulossa pian :)"

-- | A standalone widget for a new note.
noteFormStandalone :: CalendarId -> Widget
noteFormStandalone cid = do
    ((_,formw), enctype) <- liftHandlerT $ runFormPost . (getTargetForm :: CalTargetForm Note) Nothing =<< requireAuthId
    -- Just a simple 4-line form; no real reason to put it in its own template file.
    [whamlet|
<form .forms .forms-basic .forms-90 method=post action=@{TargetCreateR cid TargetNote} enctype=#{enctype}>
    ^{formw}
    <p>
        <input .btn.btn-green.unit-90 type=submit value="Lisää muistiinpano">
|]

-- ** GetTarget interface

-- | Target GET/POST magic specializations are implemented here.
class (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => GetTarget a where
        getTarget              :: a -> TargetId
        getTargetUnique        :: TargetId -> Unique a
        getTargetFormLayout    :: Maybe Text -> TargetFormRes a -> Handler Html
--        getTargetPostHandler   :: CalendarId -> Maybe a -> Handler Html
        getTargetForm          :: CalTargetForm a

instance GetTarget Todo where
    getTarget           = todoTarget
    getTargetUnique     = UniqueTodo
    getTargetFormLayout = targetFormLayout "tehtävä" "yellow"
    getTargetForm       = calTargetForm $ \mt -> Todo
        <$> maybe (pure False) (areq checkBoxField "Valmis" . Just . todoDone) mt
        <*> repeatForm                     (todoRepeat  <$> mt)
        <*> myDayFieldReq      "Aloitus"   (todoBegin   <$> mt)
        <*> myDayField         "Lopetus"   (todoEnd     <$> mt)
        <*> aopt alarmField    "Muistutus" (todoAlarm   <$> mt)
        <*> areq urgencyField  "Tärkeys"   (Just $ maybe (Urgency 2) todoUrgency mt)

instance GetTarget Event where
    getTarget           = eventTarget
    getTargetUnique     = UniqueEvent
    getTargetFormLayout = targetFormLayout "tapahtuma" "blue"
    getTargetForm       = calTargetForm $ \me -> (\f t r -> Event r f t)
        <$> myDayFieldReq      "Päivästä"       (eventBegin     <$> me)
        <*> myDayField         "Päivään"        (eventEnd       <$> me)
        <*> repeatForm                          (eventRepeat    <$> me)
        <*> aopt textField     "Paikka"         (eventPlace     <$> me)
        <*> areq urgencyField  "Tärkeys"        (Just $ maybe (Urgency 2) eventUrgency me)
        <*> aopt alarmField    "Muistutus"      (eventAlarm     <$> me)
        <*> (fromMaybe [] <$> aopt attendeeField "Osallistujat"   (Just $ eventAttendees <$> me))
        <*> aopt textareaField "Kommentit"      (eventComment   <$> me)

instance GetTarget Note where
    getTarget           = noteTarget
    getTargetUnique     = UniqueNote
    getTargetFormLayout = targetFormLayout "muistiinpano" "green"
    getTargetForm       = calTargetForm $ \mn -> Note
        <$> areq textareaField ("Sisältö"{fsAttrs=[("required","")]}) (noteContent <$> mn)

-- ** Targets general

-- | Generic target POST handler. Can be used for creates and updates.
-- (in updates calendarId may be just bottom, it is not used.)
targetPostHelper :: GetTarget a => CalendarId -> TargetType -> Maybe (Target, a) -> Handler Html
targetPostHelper cid tt initial = do
    x@((res,_),_) <- runTargetForm (snd <$> initial) getTargetForm
    case res of
        FormSuccess s -> handler s >> redirect CalendarR
        FormFailure _ -> getTargetFormLayout (targetName . fst <$> initial) x
        FormMissing   -> do
            setMessage "Tyhjä lomake!"
            redirect $ case initial of
                           Just (_, v) -> TargetUpdateR (getTarget v)
                           Nothing     -> TargetCreateR cid tt
  where
    handler (Left modified) = queryModifyTarget getTarget getTargetUnique modified
                              >> setMessage "Kohteen tiedot päivitetty."
    handler (Right tinfo)   = queryAddTarget cid tinfo
                              >> setMessage "Kohde onnistuneesti lisätty."

-- | The generic form layout handler.
targetFormLayout :: Html -- ^ Display name
                 -> Html -- ^ Button color name
                 -> Maybe Text -- ^ Maybe modifying target with Just name.
                 -> TargetFormRes a -- ^ Target form
                 -> Handler Html
targetFormLayout what col modifyThis ((res, formw), enctype) = 
    defaultLayout $ do
        setTitle $ "Uusi " <> what
        $(widgetFile "target_form")

-- | Construct a target form given a function from initial value to a form
-- whose result constructs the target given a targetid.
calTargetForm :: GetTarget t => (Maybe t -> AForm Handler (TargetId -> t)) -> CalTargetForm t
calTargetForm ctform Nothing uid = renderKube $ ((Right .) . (,)             ) <$> targetForm uid <*> ctform Nothing
calTargetForm ctform (Just ct) _ = renderKube $ ( Left     . ($ getTarget ct)) <$> ctform (Just ct)

-- | Take calendar target form to its result.
runTargetForm :: Maybe a -> CalTargetForm a -> Handler (TargetFormRes a)
-- XXX: editing others' targets is possible!?
runTargetForm initial calForm = runFormPost . calForm initial =<< requireAuthId

-- | This is a form part embedded to (the beginning of) every specialization.
targetForm :: UserId -> AForm Handler Target
targetForm uid = Target
    <$> pure uid
    <*> areq textField "Nimike" Nothing


-- * Time helpers

-- | Calculate next occurances of a Repeat at given days. Just don't call
-- it with infinite days and finite range; it won't return when evaluated.
nextRepeatsAt :: [Day] -> Day -> Maybe Day -> Repeat -> [(LocalTime, LocalTime)]
nextRepeatsAt xs lower upper rep = concatMap f xs
    where
        f d | cond d    = [toframe d]
            | otherwise = []
        cond = case upper of Nothing -> (>= lower)
                             Just u  -> liftA2 (&&) (>= lower) (<= u)
        toframe = (,) <$> flip LocalTime (repeatStart rep)
                      <*> flip LocalTime (repeatEnd rep)

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

formatTimeFrame :: FormatTime t => t -> t -> String
formatTimeFrame b e = formatTime myLocale "%d.%m klo. %H:%M - " b <>
                      formatTime myLocale "%H:%M" e


-- * Fields

alarmField :: Field Handler Alarm
alarmField = radioFieldList $
    ("10 min. ennen", Alarm "10") : map (liftA2 (,) (<> " min") Alarm)
                                    ["20", "30", "45", "60", "120"]

attendeeField :: Field Handler [Text]
attendeeField = checkMMap
    -- hmm.. could the ambigous Left value be defaulted by ghc?
    (return . (Right :: [Text] -> Either Text [Text]) . T.words)
    T.unwords textField

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

-- | It is a (monadic form converted to applicative) form because it needs
-- many fields and extra logic between them
repeatForm :: Maybe Repeat -> AForm Handler Repeat
repeatForm info = formToAForm $ do
    (wd, sd, ed) <- case info of
        Nothing  -> do
            tod <- liftM (localTimeOfDay . zonedTimeToLocalTime) $ lift lookupTimeAt
            let tod'  = TimeOfDay (todHour tod) (todMin tod) 0
                tod'' = TimeOfDay (todHour tod + 1) (todMin tod) 0
            return (Weekly [1..7], tod', tod'')
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
^{fvInput sv} - ^{fvInput ev}
|] }
    -- TODO implement weekday repeat?
    return (Repeat <$> wr <*> sr <*> er, [wv, myView])

colorField :: Field Handler Text
colorField = radioFieldList
    [("Vihreä" :: Text, "green")
    ,("Sininen",    "blue")
    ,("Musta",      "black")
    ,("Punainen",   "red")
    ,("Oranssi",    "orange")
    ,("Keltainen",  "yellow") ]

-- XXX use this instead of implicit calendar in targets
targetCalendarsField :: Maybe TargetId -> Field Handler [CalendarId]
targetCalendarsField = undefined
