module Handler.Calendar where

import Import
import Data.Time
import qualified Data.Text as T

-- * Calendar

getCalendarR :: Handler Html
getCalendarR = do
    let days = [ "Ma", "Ti", "Ke", "To", "Pe", "La", "Su" ] :: [Text]
        times = map (\x -> T.pack $ show x ++ ".00") [0..23]
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

-- * Targets

-- ** Create

getTargetR :: TargetType -> Handler Html
getTargetR TargetNote = do
    form <- generateFormPost $ noteForm undefined Nothing
    targetLayout "muistiinpano" form

getTargetR TargetEvent = do
    form <- generateFormPost $ eventForm undefined Nothing
    targetLayout "tapahtuma" form

getTargetR TargetTodo = do
    form <- generateFormPost $ todoForm undefined Nothing
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

type CalForm a = UserId -> Maybe a -> Form (Target, TargetId -> a)

noteForm :: CalForm Note
noteForm uid x = renderDivs $ (\target -> (,) target . Note)
    <$> targetForm uid (noteTarget <$> x)
    <*> areq textareaField "Sisältö" Nothing

eventForm :: CalForm Event
eventForm uid me = renderDivs $ (,)
    <$> targetForm uid Nothing <*> event
  where
      event = Event
        <$> areq repeatField   "Milloin" Nothing
        <*> areq timedateField "Aloitus" Nothing
        <*> aopt timedateField "Lopetus" Nothing
        <*> aopt textField     "Paikka" Nothing
        <*> areq urgencyField  "Tärkeys" Nothing
        <*> areq alarmField    "Muistutus" Nothing
        <*> (T.words <$> areq textField "Osallistujat" Nothing)
        <*> aopt textareaField "Kommentit" Nothing

todoForm :: CalForm Todo
todoForm uid mtodo = undefined

calForm :: UserId -> TargetId
        -> AForm Handler (TargetId -> a)
        -> Form (Target, TargetId -> a)
calForm uid tid e = renderDivs $ (,) <$> targetForm uid Nothing
                                     <*> e

-- ** Fields

targetForm :: UserId -> Maybe TargetId -> AForm Handler Target
targetForm uid mtid = Target
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


