module Utils where

import ClassyPrelude
import Yesod
import Yesod.Form

read' :: Read a => Text -> a
read' = fromMaybe (error "Could not parse!") . readMay . unpack

renderKube :: Monad master => FormRender master a
renderKube aform fragment = do
    (res, views') <- aFormToForm aform
    let views   = views' []

    -- <p :fvRequired view:.required :not (fvRequired view):.optional :isJust (fvErrors view):.error>
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <label for=#{fvId view}>
        #{fvLabel view}
        $maybe desc <- fvTooltip view
            <span .forms-desc>#{desc}
        $maybe err <- fvErrors view
            <span .error>#{err}
        ^{fvInput view}
|]
    return (res, widget)

renderKubeNoLabel :: Monad master => FormRender master a
renderKubeNoLabel aform fragment = do
    (res, views') <- aFormToForm aform
    let views  = views' []
        widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <label for=#{fvId view}>
        $maybe desc <- fvTooltip view
            <span .forms-desc>#{desc}
        $maybe err <- fvErrors view
            <span .error>#{err}
        ^{fvInput view}
|]
    return (res, widget)

-- | Password and confirmation fields conveniently together as a form.
passwordConfirmForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
                    => AForm m Text
passwordConfirmForm = formToAForm $ do
        (ra, va) <- mreq passwordField "Salasana" Nothing
        (rb, vb) <- mreq (passwordConfirmField ra) "Salasana uudelleen" Nothing
        return (rb, [va, vb])

-- | Password confirmation field given the result from the password field.
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
