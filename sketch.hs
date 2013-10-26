{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, GADTs,
  OverloadedStrings, FlexibleContexts #-}
import           Yesod
import           Database.Esqueleto
import           Database.Persist.Postgresql
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad.Logger (runStderrLoggingT)
import           Control.Applicative

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Hello
    message String
    number Int
    deriving Show
|]

data HelloWorld = HelloWorld ConnectionPool
mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET POST OPTIONS
|]
instance Yesod HelloWorld
instance YesodPersist HelloWorld where
    type YesodPersistBackend HelloWorld = SqlPersistT
    runDB action = do
        HelloWorld pool <- getYesod
        runSqlPool action pool

getHomeR :: Handler Html
getHomeR = do
    defaultLayout [whamlet|
<h1>Hello World!
<form method=post>
    <input type=submit value=submit>
|]

postHomeR :: Handler Html
postHomeR = do
    per <- runDB $ selectList [] [Asc HelloNumber]

    esq <- runDB $ select $ from $ \h -> orderBy [asc (h ^. HelloNumber)] *> return h

    raw <- runDB $ rawSql "SELECT ?? FROM hello ORDER BY number ASC" [] :: Handler [Entity Hello]

    liftIO $ mapM_ print $ per ++ esq ++ raw
    redirect HomeR

optionsHomeR :: Handler Html
optionsHomeR = undefined

main :: IO ()
main = withPostgresqlPool "host=localhost port=5432 user=tikaha dbname=tikaha" 20 $ \pool -> do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll

    warp 3000 $ HelloWorld pool
