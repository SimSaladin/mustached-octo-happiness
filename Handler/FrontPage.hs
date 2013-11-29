module Handler.FrontPage where

import Import
import Text.Hamlet (hamletFile)
import Yesod.Auth.HashDB (setPassword)

getFrontPageR :: Handler Html
getFrontPageR = do
    muid <- maybeAuthId
    case muid of
        Just _ -> redirect CalendarR
        Nothing -> do
            master <- getYesod
            mmsg <- getMessage

            -- auth
            ((res, rw), _) <- runFormPost registerForm
            let [persona, google, hashdb]  = authPlugins master
                aRender                    = flip apLogin AuthR

            pc <- widgetToPageContent' $ do
                setTitle "Mustached Octo Happiness"
                $(widgetFile "frontpage")
            giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

postFrontPageR :: Handler Html
postFrontPageR = do
    ((res,_),_) <- runFormPost registerForm
    case res of
        -- success
        FormSuccess (ident, pw) -> do
            mu <- runDB $ getBy $ UniqueUser ident
            setMessage . toHtml =<< case mu of
                Just _  -> return $ "Käyttäjänimi " <> ident <> " on valitettavasti jo käytössä."
                Nothing -> do
                    -- insert into "user" values (...)
                    _ <- runDB . insert =<< setPassword pw (User ident Nothing Nothing)
                    return $ "Tervetuloa, " <> ident <> ". Voit nyt kirjautua sisään."
            redirect FrontPageR

        -- failure
        FormMissing -> setMessage "Tyhjä lomake." >> redirect FrontPageR
        _           -> getFrontPageR

registerForm :: Form (Text, Text)
registerForm = renderKube $ (,)
    <$> areq textField "Käyttäjänimi" Nothing
    <*> passwordConfirmForm
