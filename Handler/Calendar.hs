module Handler.Calendar where

import Import

-- * Calendar

getCalendarR :: Handler Html
getCalendarR = do
    defaultLayout $ do
        setTitle "Calendar"
        $(widgetFile "calendar")

getCalendarSettingsR :: Handler Html
getCalendarSettingsR = undefined

postCalendarSettingsR :: Handler Html
postCalendarSettingsR = undefined

-- * Targets

getTargetR :: TargetType -> Handler Html
getTargetR = undefined

postTargetR :: TargetType -> Handler Html
postTargetR = undefined

-- ** Particular Target

getTargetThisR :: TargetUID -> Handler Html
getTargetThisR = undefined

postTargetThisR :: TargetUID -> Handler Html
postTargetThisR = undefined

-- ** Generic



-- ** Export

getTargetTextR :: TargetUID -> Handler Html
getTargetTextR = undefined

postTargetSendR :: TargetUID -> Handler Html
postTargetSendR = undefined
