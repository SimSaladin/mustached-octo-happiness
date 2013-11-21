module Handler.Calendar where

import Import
import Control.Monad
import Data.Time
import qualified Data.Text as T

days :: [Text]
days = [ "Ma", "Ti", "Ke", "To", "Pe", "La", "Su" ]

-- * Calendar

getCalendarR :: Handler Html
getCalendarR = do
    let times = map (\x -> T.pack $ show x ++ ".00") ([0..23] :: [Int])

    -- TODO calendar content!
    cals <- queryCalendarInfo

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
    ((resNewCal,_),_) <- runFormPost newCalendarForm
    case resNewCal of
        FormSuccess newcal -> do
            _ <- runDB $ insert newcal
            setMessage "Kalenteri luotu."
            redirect CalendarR
        FormFailure _ -> getCalendarSettingsR
        _ -> do
            undefined

newCalendarWidget :: Widget
newCalendarWidget = do
    ((_, w), _) <- liftHandlerT $ runFormPost newCalendarForm
    $(widgetFile "calendarWidgetAdd")

-- * Targets

-- ** Create

getTargetR :: TargetType -> Handler Html
getTargetR TargetNote  = runTargetForm noteForm  >>= layoutNote
getTargetR TargetEvent = runTargetForm eventForm >>= layoutEvent
getTargetR TargetTodo  = runTargetForm todoForm  >>= layoutTodo

layoutNote  = targetLayout "muistiinpano"
layoutEvent = targetLayout "tapahtuma"
layoutTodo  = targetLayout "to-do"

targetLayout :: Html -> ((FormResult a, Widget), Enctype) -> Handler Html
targetLayout what ((res, formw), enctype) = 
    defaultLayout $ do
        setTitle $ "Uusi " <> what
        $(widgetFile "newtarget")

-- (real sig would be three lines...)
-- runTargetForm :: form -> Handler ((,),)
runTargetForm theForm = runFormPost . theForm Nothing =<< requireAuthId

-- ** Update, Delete

-- | Create or update a target.
postTargetR :: TargetType -> Handler Html
postTargetR TargetEvent = do
        runres@((res,_),_) <- runTargetForm eventForm
        case res of
            FormSuccess e -> do
                -- runDB $ insert Event
                undefined
                setMessage "Tapahtuma lisätty"
                redirect CalendarR
            FormFailure _ -> layoutEvent runres
            FormMissing   -> redirect $ TargetR TargetEvent

-- ** Read

getTargetThisR :: TargetUID -> Handler Html
getTargetThisR = undefined

postTargetThisR :: TargetUID -> Handler Html
postTargetThisR = undefined

-- ** Export

getTargetTextR :: TargetUID -> Handler Html
getTargetTextR = undefined

postTargetSendR :: TargetUID -> Handler Html
postTargetSendR = undefined

-- * Forms

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
eventForm = calTargetForm' $ \me -> Event
    <$> repeatForm
    <*> myDayFieldReq      "Päivästä"       (eventBegin     <$> me)
    <*> myDayField         "Päivään"        (eventEnd       <$> me)
    <*> aopt textField     "Paikka"         (eventPlace     <$> me)
    <*> areq urgencyField  "Tärkeys" (Just $ maybe (Urgency 2) eventUrgency me)
    <*> aopt alarmField    "Muistutus"      (eventAlarm     <$> me)
    <*> areq attendeeField "Osallistujat"   (Just $ maybe [] eventAttendees me)
    <*> aopt textareaField "Kommentit"      (eventComment   <$> me)
        where
            attendeeField = checkMMap
                -- hmm.. could the ambigous Left value be defaulted by ghc?
                (return . (Right :: [Text] -> Either Text [Text]) . T.words)
                T.unwords textField

todoForm :: CalTargetForm Todo
todoForm = calTargetForm' $ \mt -> Todo
    <$> areq checkBoxField "Valmis"    (todoDone    <$> mt)
    <*> repeatForm
    <*> myDayFieldReq      "Aloitus"   (todoBegin   <$> mt)
    <*> myDayField         "Lopetus"   (todoEnd     <$> mt)
    <*> areq alarmField    "Muistutus" (todoAlarm   <$> mt)
    <*> areq urgencyField  "Tärkeys"   (todoUrgency <$> mt)

-- *** Helpers

type CalTargetForm ct = Maybe ct -> UserId -> Form (Either ct (Target, TargetId -> ct))

class GetTarget a where
        getTarget :: a -> TargetId

instance GetTarget Todo where getTarget = todoTarget
instance GetTarget Event where getTarget = eventTarget
instance GetTarget Note where getTarget = noteTarget

calTargetForm' :: GetTarget ct => (Maybe ct -> AForm Handler (TargetId -> ct)) -> CalTargetForm ct
calTargetForm' ctform Nothing uid = renderKube $ ((Right .) . (,)) <$> targetForm uid <*> ctform Nothing
calTargetForm' ctform (Just ct) _ = renderKube $ (Left . ($ getTarget ct)) <$> ctform (Just ct)

targetForm :: UserId -> AForm Handler Target
targetForm uid = Target
    <$> pure uid
    <*> areq textField "Nimike" Nothing

myDayField :: FieldSettings App -> Maybe (Maybe Day) -> AForm Handler (Maybe Day)
myDayField opts mday = formToAForm $ do
    (r, v) <- mopt dayField opts . Just . Just =<< dayDefault (join mday)
    return (r, [v])

myDayFieldReq :: FieldSettings App -> Maybe Day -> AForm Handler Day
myDayFieldReq opts mday = formToAForm $ do
    (r, v) <- mreq dayField opts . Just =<< dayDefault mday
    return (r, [v])

dayDefault :: MonadIO m => Maybe Day -> m Day
dayDefault mday = case mday of
        Just day -> return day
        -- FIXME users most likely expect zoned time..
        Nothing  -> liftM utctDay (liftIO getCurrentTime)

-- ** Fields

urgencyField :: Field Handler Urgency
urgencyField = radioFieldList
    [ ("Joutava" :: Text, Urgency 1)
    , ("Normaali", Urgency 2)
    , ("Tärkeä", Urgency 3)
    , ("Kriittinen", Urgency 4) ]

weekDaysField :: Field Handler [Int]
weekDaysField = multiSelectFieldList $ zip days [1..]

repeatForm :: AForm Handler Repeat
repeatForm = formToAForm $ do
    (wr, wv) <- mreq weekDaysField "Toisto" Nothing
    (sr, sv) <- mreq timeField "Alkaa klo." Nothing
    (er, ev) <- mreq timeField "Loppuu klo." Nothing
    let myView = FieldView -- TODO fields not complete
            { fvErrors   = Nothing
            , fvId       = "repeat"
            , fvLabel    = "Klo."
            , fvRequired = True
            , fvTooltip  = Nothing
            , fvInput    = [whamlet|
<p>^{fvInput sv} - ^{fvInput ev}
|] }
    return (Repeat <$> (Weekly <$> wr) <*> sr <*> er, [wv, myView]) -- [sv, ev, wv])

-- daysRangeForm :: Maybe Day -> (Maybe (Maybe Day)) -> AForm Handler (Day,... ?????)

alarmField :: Field Handler Alarm
alarmField = radioFieldList $ map (\x -> (x <> " min", Alarm x))
        ["10", "20", "30", "45", "60", "120"]
