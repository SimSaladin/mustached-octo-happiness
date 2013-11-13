module Utils where

import Prelude
import Data.Text (Text)
import Yesod

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

        $maybe err <- fvErrors view
            <span .error>#{err}

        ^{fvInput view}

        $maybe desc <- fvTooltip view
            <span .forms-desc>#{desc}
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
