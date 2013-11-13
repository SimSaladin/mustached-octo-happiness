module Utils where

import Prelude
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
