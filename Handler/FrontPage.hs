module Handler.FrontPage where

import Import
import Text.Hamlet (hamletFile)
import Yesod.Auth
import Yesod.Auth.HashDB (setPassword)

postFrontPageR :: Handler Html
postFrontPageR = do
    ((res, rw), _) <- runFormPost registerForm
    case res of
        FormSuccess (ident, pw) -> do
            mu <- runDB $ getBy $ UniqueUser ident
            setMessage . toHtml =<< case mu of
                Nothing -> do
                    _ <- runDB . insert =<< setPassword pw (User ident Nothing Nothing) -- insert into "user" values (...)
                    return $ "Tervetuloa, " <> ident <> ". Voit nyt kirjautua sisään."
                Just _ -> return $ "Käyttäjänimi " <> ident <> " on valitettavasti jo käytössä."
            redirect FrontPageR
        FormMissing -> redirect FrontPageR
        _           -> getFrontPageR

getFrontPageR :: Handler Html
getFrontPageR = do
    master <- getYesod
    mmsg <- getMessage
    ((res, rw), _) <- runFormPost registerForm

    let [persona, google, hashdb]  = authPlugins master
        aRender                    = flip apLogin AuthR

    pc <- widgetToPageContent' $ do
        setTitle "Mustached Octo Happiness"
        $(widgetFile "frontpage")

    giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

registerForm :: Form (Text, Text)
registerForm = renderKube $ (,)
    <$> areq textField "Käyttäjänimi" Nothing
    <*> passwordConfirmForm

passwordConfirmForm :: AForm Handler Text
passwordConfirmForm = formToAForm $ do
        (ra, va) <- mreq passwordField "Salasana" Nothing
        (rb, vb) <- mreq (passwordConfirmField ra) "Salasana uudelleen" Nothing
        return (rb, [va, vb])

passwordConfirmField :: FormResult Text -> Field Handler Text
passwordConfirmField r = check (f r) passwordField
    where
        f (FormSuccess p) pc | p == pc = Right p
                             | otherwise = Left ("Salasanat eivät täsmää" :: Text)
        f _ _ = Left "Salasana puuttuu"
