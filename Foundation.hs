{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Auth.HashDB (authHashDB, HashDBUser(..))
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)
import Yesod.Fay

import CalendarTypes
import CalendarQueries
import Utils

data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
--    , fayCommandHandler :: CommandHandler App
    , appLogger :: Logger
    }

mkMessage "App" "messages" "en"

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        pc <- widgetToPageContent' $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    addStaticContent =
        addStaticContentExternal Right genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

instance YesodJquery App

--instance YesodFay App where
--
--    fayRoute = FaySiteR
--
--    yesodFayCommand render command = do
--        master <- getYesod
--        fayCommandHandler master render command

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = CalendarR
    -- Where to send a user after logout
    logoutDest _ = FrontPageR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> fmap Just $ insert $ User (credsIdent creds) Nothing Nothing

    authPlugins _ = [ authBrowserId def
                    , authGoogleEmail
                    , authHashDB' ]

    authHttpManager = httpManager

    loginHandler = lift $ redirect FrontPageR

instance HashDBUser User where
        userPasswordHash = userPassword
        userPasswordSalt = userSalt
        setSaltAndPasswordHash salt pass user =
            user { userSalt = Just salt, userPassword = Just pass }

instance RenderMessage App FormMessage where
    renderMessage _ _ = finnishFormMessage

-- * Common views

-- | Specialized login page.
authHashDB' :: AuthPlugin App
authHashDB' = (authHashDB $ Just . UniqueUser) { apLogin = \tm -> $(widgetFile "hashdblogin") }
    where login = PluginR "hashdb" ["login"]

widgetToPageContent' :: Widget -> Handler (PageContent (Route App))
widgetToPageContent' w = widgetToPageContent $ do
    getYesod >>= addScriptEither . urlJqueryJs
    $(combineStylesheets 'StaticR
        [ css_kube_min_css
        , css_glyphicons_css
        , css_style_css
        ])
    w

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

calendars :: Widget
calendars = do
    CalendarInfo cals <- liftHandlerT queryCalendarInfo
    mcal <- liftHandlerT activeCalendar
    $(widgetFile "calendar_nav")

publicCalendars :: Widget
publicCalendars = do
    cals <- liftHandlerT queryPublicCalendars
    mcal <- liftHandlerT activeCalendar
    $(widgetFile "calendar_nav")

smallLogo :: Widget
smallLogo = [whamlet|
<img .small-logo src=@{StaticR img_moh_plain_svg}>
|]
