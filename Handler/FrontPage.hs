module Handler.FrontPage where

import Import
import Language.Haskell.TH ( Exp(..) )

getFrontPageR :: Handler Html
getFrontPageR = do
    defaultLayout $ do
        setTitle "Welcome to Mustached-Octo-Happiness!"
        $(widgetFile "frontpage")

        -- TODO fay fay fay fay..
        let handlerName = "getFrontPageR" :: Text
        $(fayFile' (ConE 'StaticR) "Home")
