module Handler.Calendar where

import Import
import Data.Time
import qualified Data.Text as T

-- * Calendar

getCalendarR :: Handler Html
getCalendarR = do
    let days = [ "Ma", "Ti", "Ke", "To", "Pe", "La", "Su" ] :: [Text]
        times = map (\x -> T.pack $ show x ++ ".00") [0..23]
    uid <- requireAuthId
    cals <- queryCalendarInfo uid
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
    undefined

newCalendarWidget :: Widget
newCalendarWidget = do
    ((_, w), _) <- liftHandlerT $ runFormPost newCalendarForm
    $(widgetFile "calendarWidgetAdd")

-- * Targets

-- ** Create

getTargetR :: TargetType -> Handler Html
getTargetR TargetNote = do
    form <- generateFormPost $ noteForm Nothing undefined
    targetLayout "muistiinpano" form

getTargetR TargetEvent = do
    form <- generateFormPost $ eventForm Nothing undefined
    targetLayout "tapahtuma" form

getTargetR TargetTodo = do
    form <- generateFormPost $ todoForm Nothing undefined
    targetLayout "to-do" form

targetLayout :: Html -> (Widget, Enctype) -> Handler Html
targetLayout what (formw, enctype) = 
    defaultLayout $ do
        setTitle $ "Uusi " <> what
        $(widgetFile "newtarget")

-- ** Update, Delete

-- | Create or update a target.
postTargetR :: TargetType -> Handler Html
postTargetR = undefined

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

newCalendarForm :: Form Calendar
newCalendarForm = renderKube $ Calendar
    <$> lift requireAuthId
    <*> areq textField "Nimi" Nothing
    <*> aopt textField "Kuvaus" Nothing
    <*> areq textField "Väri" Nothing
    <*> areq checkBoxField "Julkinen" Nothing
    <*> areq checkBoxField "Julkisesti muokattava" Nothing

type CalTargetForm ct = Maybe ct -> UserId -> Form (Either ct (Target, TargetId -> ct))

class GetTarget a where
        getTarget :: a -> TargetId

instance GetTarget Todo where getTarget = todoTarget
instance GetTarget Event where getTarget = eventTarget
instance GetTarget Note where getTarget = noteTarget

calTargetForm' :: GetTarget ct => (Maybe ct -> AForm Handler (TargetId -> ct)) -> CalTargetForm ct
calTargetForm' ctform Nothing uid = renderKube $ ((Right .) . (,)) <$> targetForm uid <*> ctform Nothing
calTargetForm' ctform (Just ct) _ = renderKube $ (flip Left $ getTarget ct) <$> ctform (Just mc)

noteForm :: CalTargetForm Note
noteForm = calTargetForm' $ \mn -> Note
    <$> areq textareaField "Sisältö" Nothing

eventForm :: CalTargetForm Event
eventForm = calTargetForm' $ \me -> Event
    <$> areq repeatField   "Milloin" Nothing
    <*> areq timedateField "Aloitus" Nothing
    <*> aopt timedateField "Lopetus" Nothing
    <*> aopt textField     "Paikka" Nothing
    <*> areq urgencyField  "Tärkeys" Nothing
    <*> areq alarmField    "Muistutus" Nothing
    <*> (T.words <$> areq textField "Osallistujat" Nothing)
    <*> aopt textareaField "Kommentit" Nothing

todoForm :: CalTargetForm Todo
todoForm = calTargetForm' $ \mt -> Todo
    <$> areq checkBoxField "Valmis" Nothing
    <*> areq repeatField "Milloin" Nothing
    <*> areq timedateField "Aloitus" Nothing
    <*> aopt timedateField "Lopetus" Nothing
    <*> areq alarmField    "Muistutus" Nothing
    <*> areq urgencyField  "Tärkeys" Nothing

-- ** Fields

targetForm :: UserId -> AForm Handler Target
targetForm uid = Target
    <$> pure uid
    <*> areq textField "Nimike" Nothing

urgencyField :: Field Handler Urgency
urgencyField = radioFieldList
    [ ("Joutava" :: Text, Urgency 1)
    , ("Normaali", Urgency 2)
    , ("Tärkeä", Urgency 3)
    , ("Kriittinen", Urgency 4) ]

timedateField :: Field Handler UTCTime
timedateField = error "undefined: `timedateField`"

repeatField :: Field Handler Repeat
repeatField = error "undefined: `repeatField`"

alarmField :: Field Handler Alarm
alarmField  = error "undefined: `alarmField' in Handler/Calendar.hs"


