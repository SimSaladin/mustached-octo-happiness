------------------------------------------------------------------------------
-- File:          Handler/Tasks.hs
-- Creation Date:
-- Last Modified: Mar 31 2014 [03:00:53]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

-- | Targets and so on.
module Handler.Target where

import Import
import Yesod.Form.Bootstrap3 (withPlaceholder)

-- * CRUD

-- | View new target form.
--
-- CalendarId is used as a dummy here: POST form posts by default to the
-- origin url which already contains the id.
getTargetCreateR :: CalendarId -> TargetType -> Handler Html
-- XXX: use cid?
getTargetCreateR _cid tt = case tt of
    TargetNote  -> go (calTargetForm getTargetForm :: CalTargetForm Note)
    TargetEvent -> go (calTargetForm getTargetForm :: CalTargetForm Event)
    TargetTodo  -> go (calTargetForm getTargetForm :: CalTargetForm Todo)
    where
        go a = runTargetForm Nothing Nothing a >>= getTargetFormLayout Nothing

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
    (target, _cals, v) <- queryTarget tid -- TODO modify calendars?

    let -- don't remove sig, ghc specializes otherwise
        go :: GetTarget a => a -> Handler Html
        go a = runTargetForm (Just target) (Just a) (calTargetForm getTargetForm)
                >>= getTargetFormLayout (Just $ targetName target)
    case v of
        T1 (Entity _ event) -> go event
        T2 (Entity _ todo)  -> go todo
        T3 (Entity _ note)  -> go note

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

getTargetDeleteR :: TargetId -> Handler Html
getTargetDeleteR tid = do
    defaultLayout $ do
        $(widgetFile "target_delete_confirm")

-- | Delete a target.
postTargetDeleteR :: TargetId -> Handler Html
postTargetDeleteR tid = do
    queryDeleteTarget tid
    setMessage "Kohde poistettu."
    redirect CalendarR

-- * Other

-- | Export a target as text.
getTargetTextR :: TargetId -> Handler Html
getTargetTextR = error "Tulossa pian :)"

-- | Send a target to another user.
postTargetSendR :: TargetId -> Handler Html
postTargetSendR = error "Tulossa pian :)"

-- * Embeddable

-- | A standalone widget for a new note.
noteFormStandalone :: CalendarId -> Widget
noteFormStandalone cid = do
    uid <- liftHandlerT requireAuthId
    ((_,widget), encType) <- liftHandlerT $ runFormPost $
            (calTargetForm' getTargetForm :: CalTargetForm Note) Nothing Nothing uid
    [whamlet|
<form .forms method=post action=@{TargetCreateR cid TargetNote} enctype=#{encType}>
    ^{widget}
    <p>
        <input .btn.btn-green.unit-90 type=submit value="Lisää muistiinpano">
|]

-- * Fields

alarmField :: Field Handler Alarm
alarmField = radioFieldList $
    ("10 min. ennen", Alarm "10") : map (liftA2 (,) (<> " min") Alarm)
                                    ["20", "30", "45", "60", "120"]

attendeeField :: Field Handler [Text]
attendeeField = checkMMap
    -- hmm.. could the ambigous Left value be defaulted by ghc?
    (return . (Right :: [Text] -> Either Text [Text]) . words)
    unwords textField

urgencyField :: Field Handler Urgency
urgencyField = radioFieldList
    [ ("Joutava" :: Text, Urgency 1)
    , ("Normaali", Urgency 2)
    , ("Tärkeä", Urgency 3)
    , ("Kriittinen", Urgency 4) ]

weekDaysField :: Field Handler RepeatTime
weekDaysField = checkMMap (return . f) unWeekly $ multiSelectFieldList $ zip days [1..]
    where
        -- type inferer won't default the Left type
        f :: [Int] -> Either Text RepeatTime
        f = Right . Weekly

myDayField :: FieldSettings App -> Maybe (Maybe Day) -> AForm Handler (Maybe Day)
myDayField opts mday = formToAForm $ do
    theDay <- case mday of
                  Nothing -> liftHandlerT $ liftM Just $ dayDefault Nothing
                  Just md -> return md
    (r, v) <- mopt dayField opts (Just theDay)
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

    (wdr, wdv) <- mreq weekDaysField "Toisto viikonpäivinä" $ Just wd
    (sr, sv) <- mreq timeField ""  $ Just sd
    (er, ev) <- mreq timeField "" $ Just ed
    let timeOfDayView = FieldView -- TODO fields not complete
            { fvErrors   = Nothing
            , fvId       = "repeat"
            , fvLabel    = "Kesto"
            , fvRequired = True
            , fvTooltip  = Nothing
            , fvInput    = [whamlet|
^{fvInput sv} - ^{fvInput ev}
|] }
    -- TODO implement weekday repeat?
    return (Repeat <$> wdr <*> sr <*> er, [wdv, timeOfDayView])

colorField :: Field Handler Text
colorField = radioField colorList

-- XXX use this instead of implicit calendar in targets
targetCalendarsField :: Maybe TargetId -> Field Handler [CalendarId]
targetCalendarsField = undefined

colorList :: Handler (OptionList Text)
colorList = optionsPairs
    [("Vihreä" :: Text, "green")
    ,("Sininen",    "blue")
    ,("Musta",      "black")
    ,("Punainen",   "red")
    ,("Oranssi",    "orange")
    ,("Keltainen",  "yellow") ]

-- * (Overengineered) target forms

-- | This is a form part embedded to (the beginning of) every specialization.
targetForm :: Maybe Target -> AForm Handler Target
targetForm mtarget = Target
    <$> maybe (lift requireAuthId) (pure . targetOwner) mtarget
    <*> areq textField (withPlaceholder "Otsikko" "") (targetName <$> mtarget)

-- | Generic target POST handler. Can be used for creates and updates.
-- (in updates calendarId may be just bottom, it is not used.)
targetPostHelper :: GetTarget a
                 => CalendarId -> TargetType -> Maybe (Target, a) -> Handler Html
targetPostHelper cid tt initial = do
    x@((res,_),_) <- runTargetForm (fst <$> initial) (snd <$> initial)
                                  (calTargetForm getTargetForm)
    case res of
        FormSuccess s -> handler s >> redirect CalendarR
        FormFailure _ -> getTargetFormLayout (targetName . fst <$> initial) x
        FormMissing   -> do
            setMessage "Tyhjä lomake!"
            redirect $ case initial of
                           Just (_, v) -> TargetUpdateR (getTarget v)
                           Nothing     -> TargetCreateR cid tt
  where
    handler (target, Left modified) =
        queryModifyTarget getTarget getTargetUnique target modified
        >> setMessage "Kohteen tiedot päivitetty."

    handler (target, Right f) =
        queryAddTarget cid target f
        >> setMessage "Kohde onnistuneesti lisätty."

-- | The generic form layout handler.
targetFormLayout :: Html -- ^ Display name
                 -> Html -- ^ Button color name
                 -> Maybe Text -- ^ Maybe modifying target with Just name.
                 -> TargetFormRes a -- ^ Target form
                 -> Handler Html
targetFormLayout what col modifyThis ((res, formw), enctype) = 
    defaultLayout $ do
        setTitle $ "Uusi " `mappend` what
        $(widgetFile "layout_target")

-- | Construct a target form given a function from initial value to a form
-- whose result constructs the target given a targetid.
calTargetForm :: GetTarget a => (Maybe a -> AForm Handler (TargetId -> a)) -> CalTargetForm a
-- XXX use uid?
calTargetForm spec mtarget ma _uid = renderKube $ (\target -> (,) target . f)
    <$> targetForm mtarget
    <*> spec ma
    where
        f a = maybe (Right a) (Left . a . getTarget) ma

-- | calTargetForm but with renderKube'
calTargetForm' :: GetTarget a => (Maybe a -> AForm Handler (TargetId -> a)) -> CalTargetForm a
calTargetForm' spec mtarget ma _uid = renderKubeNoLabel $ (\target -> (,) target . f)
    <$> targetForm mtarget
    <*> spec ma
    where
        f a = maybe (Right a) (Left . a . getTarget) ma

-- | Take calendar target form to its result.
runTargetForm :: Maybe Target -> Maybe a -> CalTargetForm a -> Handler (TargetFormRes a)
-- XXX: editing others' targets is possible!?
runTargetForm mt initial calForm = runFormPost . calForm mt initial =<< requireAuthId

-- ** Types

-- | Calendar target forms take possible initial value and userid as
-- paramaters.
type CalTargetForm a = Maybe Target -> Maybe a -> UserId -> Form (CalTargetAt a)

-- | Result from running a calendar target form.
type TargetFormRes a = ((FormResult (CalTargetAt a), Widget), Enctype)

-- | Calendar target form result is either the updated value of an existing
-- target and specialization (if the initial value was provided), or
-- a target and a function which takes targetid to the specialization.
type CalTargetAt a = (Target, Either a (TargetId -> a))

-- | Target GET/POST magic.
class (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => GetTarget a where
        getTarget              :: a -> TargetId
        getTargetUnique        :: TargetId -> Unique a
        getTargetFormLayout    :: Maybe Text -> TargetFormRes a -> Handler Html
        getTargetForm          :: Maybe a -> AForm Handler (TargetId -> a)

instance GetTarget Todo where
    getTarget           = todoTarget
    getTargetUnique     = UniqueTodo
    getTargetFormLayout = targetFormLayout "tehtävä" "yellow"
    getTargetForm mt    = (\done f t r -> Todo done r f t)
        <$> maybe (pure False) (areq checkBoxField "Valmis" . Just . todoDone) mt
        <*> myDayFieldReq "Päivittäin alkaen päivästä" (todoBegin <$> mt)
        <*> myDayField    "Päättyen päivään" (todoEnd <$> mt)
        <*> repeatForm (todoRepeat <$> mt)
        <*> aopt alarmField "Muistutus" (todoAlarm <$> mt)
        <*> areq urgencyField "Tärkeys" (Just $ maybe (Urgency 2) todoUrgency mt)

instance GetTarget Event where
    getTarget           = eventTarget
    getTargetUnique     = UniqueEvent
    getTargetFormLayout = targetFormLayout "tapahtuma" "blue"
    getTargetForm me    = (\f t r -> Event r f t)
        <$> myDayFieldReq "Päivittäin alkaen päivästä" (eventBegin <$> me)
        <*> myDayField    "Pättyen päivään" (eventEnd <$> me)
        <*> repeatForm (eventRepeat <$> me)
        <*> aopt textField "Paikka" (eventPlace <$> me)
        <*> areq urgencyField "Tärkeys" (Just $ maybe (Urgency 2) eventUrgency me)
        <*> aopt alarmField "Muistutus" (eventAlarm <$> me)
        <*> (fromMaybe [] <$> aopt attendeeField "Osallistujat" (Just $ eventAttendees <$> me))
        <*> aopt textareaField "Kommentit" (eventComment <$> me)

instance GetTarget Note where
    getTarget           = noteTarget
    getTargetUnique     = UniqueNote
    getTargetFormLayout = targetFormLayout "muistiinpano" "green"
    getTargetForm       = \mn -> Note
        <$> areq textareaField ("Sisältö"{fsAttrs=[("required","")]}) (noteContent <$> mn)
