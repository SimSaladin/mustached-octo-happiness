module Handler.Calendar where

import Import
import Data.Time
import qualified Data.Text as T

-- * Calendar

getCalendarR :: Handler Html
getCalendarR = do
    let days = [ "Ma", "Ti", "Ke", "To", "Pe", "La", "Su" ] :: [Text]
        times = map (\x -> show x ++ ".00") [0..23]
    defaultLayout $ do
        setTitle "Calendar"
        $(widgetFile "calendar")

getCalendarSettingsR :: Handler Html
getCalendarSettingsR = undefined

postCalendarSettingsR :: Handler Html
postCalendarSettingsR = undefined

-- * Target create

getTargetR :: TargetType -> Handler Html
getTargetR TargetNote = do
        let ttype = "muistiinpano" :: Text -- could be i18ned
        (formw, enctype) <- generateFormPost $ noteForm undefined Nothing
        defaultLayout $ do
            setTitle "Uusi muistiipano"
            $(widgetFile "newtarget")

getTargetR _ = undefined

postTargetR :: TargetType -> Handler Html
postTargetR = undefined

-- ** Particular Target

getTargetThisR :: TargetUID -> Handler Html
getTargetThisR = undefined

postTargetThisR :: TargetUID -> Handler Html
postTargetThisR = undefined

-- * Forms

type CalForm a = UserId -> Maybe a -> Form (Target, TargetId -> a)

calForm :: UserId -> TargetId
        -> AForm Handler (TargetId -> a)
        -> Form (Target, TargetId -> a)
calForm uid tid e = renderDivs $ (,) <$> targetForm uid Nothing
                                     <*> e

noteForm :: UserId -> Maybe Note -> Form (Target, TargetId -> Note)
noteForm uid x = renderDivs $ (\target -> (,) target . Note)
    <$> targetForm uid (noteTarget <$> x)
    <*> areq textareaField "Sisältö" Nothing

eventForm :: UserId -> Form (Target, TargetId -> Event)
eventForm uid = renderDivs $ (,)
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
timedateField = undefined

repeatField :: Field Handler Repeat
repeatField = undefined

alarmField :: Field Handler Alarm
alarmField  = error "undefined: `alarmField' in Handler/Calendar.hs"

-- ** Export

getTargetTextR :: TargetUID -> Handler Html
getTargetTextR = undefined

postTargetSendR :: TargetUID -> Handler Html
postTargetSendR = undefined


