module Utils where

import Prelude
import Data.Text (Text)
import Data.Monoid (mappend)
import Yesod.Form
import qualified Data.Text as T
import Yesod

read' :: Read a => Text -> a
read' = read . T.unpack

renderKube :: Monad master => FormRender master a
renderKube aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
    -- <p :fvRequired view:.required :not (fvRequired view):.optional :has (fvErrors view):.error>
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <label for=#{fvId view}>
        #{fvLabel view}
        $if fvRequired view
            <span .req>*
        $maybe desc <- fvTooltip view
            <span .forms-desc>#{desc}
        $maybe err <- fvErrors view
            <span .error>#{err}
        ^{fvInput view}
|]
    return (res, widget)

passwordConfirmForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
                    => AForm m Text
passwordConfirmForm = formToAForm $ do
        (ra, va) <- mreq passwordField "Salasana" Nothing
        (rb, vb) <- mreq (passwordConfirmField ra) "Salasana uudelleen" Nothing
        return (rb, [va, vb])

passwordConfirmField :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
                     => FormResult Text -> Field m Text
passwordConfirmField r = check (f r) passwordField
    where
        f (FormSuccess p) pc | p == pc = Right p
                             | otherwise = Left ("Salasanat eivät täsmää" :: Text)
        f _ _ = Left "Salasana puuttuu"

finnishFormMessage :: FormMessage -> Text
finnishFormMessage (MsgInvalidInteger t) = "Kelpaamaton kokonaisluku: " `mappend` t
finnishFormMessage (MsgInvalidNumber t) = "Kelpaamaton luku: " `mappend` t
finnishFormMessage (MsgInvalidEntry t) = "Kelpaamaton syöte: " `mappend` t
finnishFormMessage MsgInvalidTimeFormat = "Kelpaamaton aika, formaatti on HH:MM[:SS]"
finnishFormMessage MsgInvalidDay = "Kelpaamaton päivä, formaatti on VVVV-KK-PP"
finnishFormMessage (MsgInvalidUrl t) = "Kelpaamaton URL: " `mappend` t
finnishFormMessage (MsgInvalidEmail t) = "Kelpaamaton sähköpostiosoite: " `mappend` t
finnishFormMessage (MsgInvalidHour t) = "Kelpaamaton tunti: " `mappend` t
finnishFormMessage (MsgInvalidMinute t) = "Kelpaamaton minuutti: " `mappend` t
finnishFormMessage (MsgInvalidSecond t) = "Kelpaamaton sekunti: " `mappend` t
finnishFormMessage MsgCsrfWarning = "Ole hyvä ja vahvista lomakkeen lähetys suojauksena CSRF-hyökkäyksiä vastaan."
finnishFormMessage MsgValueRequired = "Arvo vaaditaan"
finnishFormMessage (MsgInputNotFound t) = "Syötettä ei löytynyt: " `mappend` t
finnishFormMessage MsgSelectNone = "<Tyhjä>"
finnishFormMessage (MsgInvalidBool t) = "Kelpaamaton totuusarvo: " `mappend` t
finnishFormMessage MsgBoolYes = "Kyllä"
finnishFormMessage MsgBoolNo = "Ei"
finnishFormMessage MsgDelete = "Poista?"
