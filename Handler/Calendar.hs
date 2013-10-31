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

-- * Objects

getObjectR :: ObjectType -> Handler Html
getObjectR = undefined

postObjectR :: ObjectType -> Handler Html
postObjectR = undefined

-- ** Particular object

getObjectThisR :: ObjectUID -> Handler Html
getObjectThisR = undefined

postObjectThisR :: ObjectUID -> Handler Html
postObjectThisR = undefined

-- ** Generic



-- ** Export

getObjectTextR :: ObjectUID -> Handler Html
getObjectTextR = undefined

postObjectSendR :: ObjectUID -> Handler Html
postObjectSendR = undefined
