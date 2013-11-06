module Handler.FrontPage where

import Import
import Text.Hamlet (hamletFile)
import Yesod.Auth (Route(LoginR))

getFrontPageR :: Handler Html
getFrontPageR = do
    master <- getYesod
    mmsg <- getMessage
    pc <- widgetToPageContent $ do
        setTitle "Welcome to Mustached-Octo-Happiness!"
        $(combineStylesheets 'StaticR [ css_kube_min_css ])
        $(widgetFile "frontpage")
    giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
