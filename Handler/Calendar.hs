{-# LANGUAGE TupleSections, RecordWildCards #-}
module Handler.Calendar where

import Import
import Control.Lens
import qualified Data.List as L
import qualified Data.Text as T

import Handler.Target

-- * Types

type Hour = Int

type TimeRange = (LocalTime, LocalTime)

data ViewSettings = ViewWeekRange
                  { viewStartDay  :: Day
                  , viewEndDay    :: Day
                  , viewActiveCal :: CalendarId
                  , viewCalendars :: [CalendarId]
                  }

type Event' = (Entity Target, Event)
type Todo'  = (Entity Target, Todo)

data Cell = Cell
           { _cellRange    :: TimeRange
           , _cellTarget   :: Entity Target
           , _cellCalColor :: Text
           , _cellContent  :: Either Event Todo
           }

makeLenses ''ViewSettings
makeLenses ''Cell

class IsCell a where
    toCell :: a -> TimeRange -> Entity Target -> Cell
    getRepeats :: [Day] -> a -> [TimeRange]
instance IsCell Event where
    toCell event timerange target = Cell timerange target "" (Left event)
    getRepeats days = nextRepeatsAt days <$> eventBegin <*> eventEnd <*> eventRepeat
instance IsCell Todo where
    toCell todo timerange target = Cell timerange target "" (Right todo)
    getRepeats days = nextRepeatsAt days <$> todoBegin <*> todoEnd <*> todoRepeat

-- | A unfolding function type used by the black magic parts in cell
-- construction.
type Unfold a b = (a, [(a, b)]) -> Maybe ((a, [b]), (a, [(a, b)]))

-- * Views

calendarView :: Handler Html
calendarView = do
    msettings <- getViewSettings
    case msettings of
        Nothing -> defaultLayout noCalendarsWidget
        Just settings -> do
            notes <- queryCalendarNotes (viewCalendars settings)
            defaultLayout $ do
                setTitle "Calendar"
                $(widgetFile "calendar")

renderNotes :: ViewSettings -> [(Entity Target, Entity Note)] -> Widget
renderNotes settings notes = [whamlet|
$case notes
    $of []
        <p>
            <i>Tyhjä! Voit luoda muistiinpanon alla.
    $of xs
        $forall (Entity k t, Entity _ note) <- xs
            <a href=@{TargetReadR k}> #{targetName t}
            p>#{noteContent note}
<hr>
^{noteFormStandalone $ viewActiveCal settings}
|]

renderView :: ViewSettings -> Widget
renderView ViewWeekRange{..} = do
    targetParams <- liftHandlerT getTargetRouteParams

    (events, todos) <- liftM (map (second entityVal) *** map (second entityVal))
            . liftHandlerT
            $ queryCalendarObjects viewCalendars viewStartDay viewEndDay

    let dayRange = [viewStartDay .. viewEndDay]
        cellsByDay = groupByDay viewStartDay viewEndDay $ cells dayRange events todos
        hours = [0..23] :: [Hour]
        prevWeek = addDays (-7) viewStartDay
        nextWeek = addDays 1 viewEndDay

        -- old
        objectWeeks = constructWeekView viewStartDay viewEndDay events todos
        getType (Left _)  = "event" :: Html
        getType (Right _) = "todo"

    $(widgetFile "view_week")

renderTodos :: [Todo'] -> Widget
renderTodos todos = [whamlet|
   <section .todos>
      <h2>Tulevat tehtävät
      $case todos
         $of []
            <i>Ei tehtäviä
         $of _
            <ul>
               $forall (Entity k t, todo) <- todos
                  <li>
                     <a href=@{TargetReadR k}>#{targetName t}
|]

noCalendarsWidget :: Widget
noCalendarsWidget = do
    setTitle "No calendars"
    [whamlet|
<div .units-row>
  <div .unit-centered .unit-60>
     <h1>Tyhjää!
     <h1 .subheader>
        Näyttäisi siltä että sinulla ei ole yhtään kalenteria. #
        Luo kalenteri alla.
     ^{newCalendarWidget}
|]

constructWeekView :: Day -> Day -> [Event'] -> [Todo'] -> [(Hour, [(Day, [Cell])])]
constructWeekView fromDay toDay c d =
    map (second $ groupByDay fromDay toDay)
        . groupByHour $ cells [fromDay .. toDay] c d

-- * Settings

getViewSettings :: Handler (Maybe ViewSettings)
getViewSettings = do
    mcal <- activeCalendar
    flip (maybe $ return Nothing) mcal $ \cal -> do
        (fromDay, toDay) <- liftHandlerT getViewTimeframe
        viewCals <- getViewCalendars

        return $ Just $ ViewWeekRange fromDay toDay cal viewCals

getTargetRouteParams :: Handler (Day -> Hour -> [(Text, Text)])
getTargetRouteParams = do
    timezone <- liftIO getCurrentTimeZone -- TODO user supplied?
    return $ \day hour -> let
        zonedtime = ZonedTime (LocalTime day $ TimeOfDay hour 0 0) timezone
        in [("at", T.pack $ show zonedtime)]

-- * Cells

cells :: [Day] -> [(Entity Target, Event)] -> [(Entity Target, Todo)] -> [Cell]
cells dayRange events todos = go events ++ go todos
  where
    go :: IsCell b => [(Entity Target, b)] -> [Cell]
    go = concatMap $ zipWith (\range (target, b) -> toCell b range target)
            <$> getRepeats dayRange . snd <*> repeat

-- Cell placement in absolute/column context.
cellStyle :: Cell -> Text
cellStyle cell = "top:" <> tshow top <> "em;height:" <> tshow height <> "em"
    where
        (start, stop) = over both (todHour . localTimeOfDay) $ _cellRange cell
        top     = start * 2
        height  = (stop - start) * 2

groupByDay :: Day -> Day -> [Cell] -> [(Day, [Cell])]
groupByDay fromDay toDay = groupUnfold (unfoldDay toDay) fromDay cellDay

groupByHour :: [Cell] -> [(Int, [Cell])]
groupByHour = groupUnfold unfoldHour 0 cellHour

cellHour :: Cell -> Int
cellHour = todHour . localTimeOfDay . view (cellRange._1)

cellDay :: Cell -> Day
cellDay = localDay . view (cellRange._1)

-- | groupUnfold f a g xs sorts xs comparing by g, and unfolds on the list
-- of sorted elements using f with starting value a.
--
-- (This is the most incomprehensible part of the logic)
groupUnfold :: Ord a => Unfold a b -> a -> (b -> a) -> [b] -> [(a, [b])]
groupUnfold f a g = L.unfoldr f . (a,) . sortBy (comparing fst) . map (liftA2 (,) g id)

unfoldHour :: Unfold Hour b
unfoldHour (24, _) = Nothing
unfoldHour (h, xs) = Just $ (h, ) . map snd *** (h + 1, ) $ L.span ((== h) . fst) xs

unfoldDay :: Day -> Unfold Day b
unfoldDay toDay (d, xs)
    | d > toDay = Nothing
    | otherwise = Just $ (d,) . map snd *** (addDays 1 d,) $ L.span ((== d) . fst) xs

-- * Handlers

getDefaultViewR :: Handler Html
getDefaultViewR =
        undefined

-- * CRUD

getCalendarR :: Handler Html
getCalendarR = calendarView

getCalendarSettingsR :: Handler Html
getCalendarSettingsR = do
    defaultLayout $ do
        setTitle "Kalenteriasetukset"
        $(widgetFile "calendar_settings")

-- | Documentation for 'getCalendarUpdateR'
getCalendarUpdateR :: CalendarId -> Handler Html
getCalendarUpdateR cid = do
    ((res,formw),enctype) <- runFormPost . calendarForm . Just =<< queryCalendar cid
    defaultLayout $ do
        setTitle "Muokataan kalenteria"
        $(widgetFile "calendar_update")

getCalendarDeleteR :: CalendarId -> Handler Html
getCalendarDeleteR cid = do
    undefined

postCalendarCreateR :: Handler Html
postCalendarCreateR = do
    ((res, _), _) <- runFormPost $ calendarForm Nothing
    case res of
        FormSuccess cal -> do
            queryInsertCalendar cal
            setMessage "Kalenteri luotu."
            redirect CalendarR
        FormFailure _ -> getCalendarSettingsR
        FormMissing   -> setMessage "Tyhjä lomake." >> redirect CalendarSettingsR

postCalendarUpdateR :: CalendarId -> Handler Html
postCalendarUpdateR cid = do
        cal <- queryCalendar cid
        ((res,_),_) <- runFormPost $ calendarForm $ Just cal
        case res of
            FormSuccess newcal -> do
                queryUpdateCalendar cid newcal
                setMessage "Kalenterin tiedot päivitetty"
                redirect CalendarR
            FormFailure _ -> getCalendarUpdateR cid
            FormMissing   -> setMessage "Tyhjä lomake." >> redirect (CalendarUpdateR cid)

-- | Delete a calendar by id.
postCalendarDeleteR :: CalendarId -> Handler Html 
postCalendarDeleteR cid = do
        queryDeleteCalendar cid
        setMessage "Kalenteri poistettu"
        redirect CalendarR

-- * View settings

getCalendarViewR :: CalendarId -> String -> Handler Html
getCalendarViewR cid act = do
    ($ cid) $ case act of
        "view" -> addViewCalendar
        _      -> deleteViewCalendar
    redirect CalendarR

getCalendarActiveR :: CalendarId -> Handler Html
getCalendarActiveR cid = setActiveCalendar cid >> redirect CalendarR

-- * Pieces

calendarForm :: Maybe Calendar -> Form Calendar
calendarForm mcal = renderKube $ Calendar
    <$> maybe (lift requireAuthId) (pure . calendarOwner) mcal
    <*> areq nameField "Nimi"           (calendarName <$> mcal)
    <*> aopt textField "Kuvaus"         (calendarDesc <$> mcal)
    <*> areq colorField "Väri"          (Just $ maybe "green" calendarColor mcal)
    <*> areq checkBoxField "Julkinen"   (calendarPublic <$> mcal)
    <*> areq checkBoxField "Julkisesti muokattava" (calendarPublicedit <$> mcal)
    where
        nameField = checkM (calendarIsUnique $ calendarName <$> mcal) textField

calendarIsUnique :: Maybe Text -> Text -> Handler (Either Text Text)
calendarIsUnique mold new = do
        uid <- requireAuthId
        mdb <- runDB . getBy $ UniqueCalendar uid new
        return $ if isJust mdb && maybe True (new /=) mold
            then Left "Samanniminen kalenteri on jo olemassa."
            else Right new

calendarListing :: Widget
calendarListing = do
    cinfo <- liftHandlerT queryCalendarInfo
    $(widgetFile "calendar_listing")

newCalendarWidget :: Widget
newCalendarWidget = do
    ((res, w), enctype) <- liftHandlerT $ runFormPost $ calendarForm Nothing
    $(widgetFile "calendar_form")

