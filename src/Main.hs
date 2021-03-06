{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections, ScopedTypeVariables, PartialTypeSignatures     #-}

module Main where

import Control.Monad.Trans (liftIO)
import Text.Parsec.Char
import Text.Parsec
import Data.Time.Calendar
import Control.Concurrent (forkIO, threadDelay)
import Data.List (sort)
import           Control.Monad
import qualified Data.Map                           as M
import           Data.Maybe
import           Data.Monoid
import           Data.Pool                          (Pool, createPool,
                                                     withResource)
import           Data.String                        (fromString)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import           Data.Traversable
import           Database.PostgreSQL.Simple         (ConnectInfo (..),
                                                     Connection, Only (..),
                                                     close, connectPostgreSQL,
                                                     execute, execute_, query, query_)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           GHC.IO.Encoding                    (setLocaleEncoding, utf8)
import           Network.Wai                        (Response, pathInfo)
import           Network.Wai.Handler.Warp           (run)
import           Network.Wai.Middleware.Rollbar
import           System.Environment                 (lookupEnv, getEnv)
import           System.IO.Unsafe                   (unsafePerformIO)
import           Text.Digestive.Form
import           Text.Digestive.Larceny
import           Web.Fn
import           Web.Fn.Extra.Digestive
import           Web.Heroku                         (parseDatabaseUrl)
import qualified Web.Larceny                        as L
import Control.Logging
import System.Directory (listDirectory)
import Configuration.Dotenv
import qualified Network.Pushover as Push

import Context
import State


render :: Ctxt -> Text -> IO (Maybe Response)
render ctxt = renderWith ctxt mempty

renderWith :: Ctxt -> Substitutions -> Text -> IO (Maybe Response)
renderWith ctxt subs tpl =
  do t <- L.renderWith (library ctxt) subs () (T.splitOn "/" tpl)
     case t of
       Nothing -> return Nothing
       Just t' -> okHtml t'

notify :: Ctxt -> Push.Message -> IO ()
notify ctxt msg =
  case pushover ctxt of
    Nothing -> return ()
    Just (apik, userk) -> 
      void $ Push.sendMessage apik userk msg

initializer :: IO Ctxt
initializer =
  do onMissingFile (loadFile False ".env") (return ())
     lib <- L.loadTemplates "templates" L.defaultOverrides
     u <- fmap parseDatabaseUrl <$> lookupEnv "DATABASE_URL"
     let ps = fromMaybe [("host", "localhost")
                        ,("port", "5432")
                        ,("user", "todo_user")
                        ,("password", "111")
                        ,("dbname", "todo_devel")]
              u
     pgpool <- createPool (connectPostgreSQL $ T.encodeUtf8 $ T.intercalate " " $ map (\(k,v) -> k <> "=" <> v) ps)
                        close 1 60 20
     ms <- sort <$> listDirectory "migrations"
     withResource pgpool $ \conn -> execute_ conn "CREATE TABLE IF NOT EXISTS migrations (name text not null)"
     mapM_ (\m -> do sql <- readFile $ "migrations/" <> m
                     let name = takeWhile (/= '.') m
                     withResource pgpool $ \conn -> do
                       Just (Only (n :: Integer)) <- listToMaybe <$> query conn "SELECT count(*) FROM migrations WHERE name = ?" (Only name)
                       if n == 0 then
                         do execute_ conn (fromString sql)
                            execute conn "INSERT INTO migrations (name) VALUES (?)" (Only name)
                            return ()
                         else return ())
           ms
     push <- do mapi <- lookupEnv "PUSHOVER_API"
                muser <- lookupEnv "PUSHOVER_USER"
                case (mapi,muser) of
                  (Just api, Just user) ->
                    let (Right apik) = Push.makeToken (T.pack api) in
                      let (Right userk) = Push.makeToken (T.pack user) in
                        return $ Just (apik, userk)
                  _ -> return Nothing
     url <- T.pack <$> fromMaybe "http://localhost:3000" <$> lookupEnv "SITE_URL"
     return (Ctxt defaultFnRequest pgpool lib push url)

main :: IO ()
main = withStderrLogging $ do
  setLocaleEncoding utf8
  ctxt <- initializer
  port <- maybe 3000 read <$> lookupEnv "PORT"
  log' $ "Listening on port " <> tshow port <>  "..."
  rb_token <- lookupEnv "ROLLBAR_ACCESS_TOKEN"
  let rb = case rb_token of
             Nothing -> id
             Just tok -> exceptions (Settings (fromString tok) "production" :: Settings '[])
  forkIO $ notifyThread ctxt
  run port $ rb $ toWAI ctxt site

larcenyServe :: Ctxt -> IO (Maybe Response)
larcenyServe ctxt = do
  let pth' = pathInfo (fst . getRequest $ ctxt)
  let pth = T.intercalate "/" pth'
  let idx = if T.length pth > 0 then pth <> "/index" else "index"
  if ((length pth' > 1) && "_" `T.isPrefixOf` (last pth')) || ".." `T.isInfixOf` pth
     then return Nothing
     else route ctxt [ anything ==> \ctxt -> render ctxt pth
                     , anything ==> \ctxt -> render ctxt idx
                     ]

data TodoData = TodoData Text (Maybe UTCTime) (Maybe Repeat)

timezone :: TimeZone
timezone = unsafePerformIO getCurrentTimeZone

todoForm :: Maybe Todo -> Form Text IO TodoData
todoForm mt = TodoData <$> "description" .: check "Can't be empty" (not . T.null) (text (tDescription <$> mt))
                       <*> "deadline_at" .: optionalUtcTimeFormlet "%F" "%I:%M %p" timezone (tDeadlineAt =<< mt)
                       <*> "repeat_at" .: optionalStringRead "Must be in form 10D (for every 10 days) or 2M (for every 2 months)" (join (tRepeatAt <$> mt))

mkTimestamp :: UTCTime -> Text
mkTimestamp = T.pack . formatTime defaultTimeLocale "%F %I:%M%P"

todoSubs :: Todo -> Substitutions
todoSubs t = L.subs
  [("id", L.textFill $ tshow $ tId t)
  ,("description", L.textFill $ tDescription t)
  ,("is-done", if isJust (tDoneAt t) then L.fillChildren else L.textFill "")
  ,("not-done", if isNothing (tDoneAt t) then L.fillChildren else L.textFill "")
  ,("deadline", if isNothing (tDeadlineAt t) then L.textFill "" else L.fillChildrenWith (L.subs [("timestamp", L.textFill $ mkTimestamp (fromJust (tDeadlineAt t)))]))
  ,("done_at", if isNothing (tDoneAt t) then L.textFill "" else L.fillChildrenWith (L.subs [("timestamp", L.textFill $ mkTimestamp (fromJust (tDoneAt t)))]))
  ,("snooze_till", L.Fill $ \_ (pth, L.Template tpl) l ->
                              do now <- liftIO getCurrentTime
                                 case tSnoozeTill t of
                                   Just time | time > now -> tpl pth (L.subs [("timestamp", L.textFill $ mkTimestamp time)]) l
                                   _ -> return $ "")
  ,("not_snooze_till",
    L.Fill $ \_ (pth, L.Template tpl) l ->
                              do now <- liftIO getCurrentTime
                                 case tSnoozeTill t of
                                   Just time | time > now -> return $ ""
                                   _ -> tpl pth (L.subs []) l)
  ,("repeat_at", if isNothing (tRepeatAt t) then L.textFill "" else L.fillChildrenWith (L.subs [("interval", L.textFill $ tshow (fromJust $ tRepeatAt t))]))
  ]


getModes :: Ctxt -> IO [Mode]
getModes ctxt =
  withResource (db ctxt) $ \c -> query_ c "select id, name, account from modes"

getMode :: Ctxt -> Int -> IO (Maybe Mode)
getMode ctxt id =
  withResource (db ctxt) $ \c -> listToMaybe <$> query c "select id, name, account from modes where id = ?" (Only id)

getModeByAccount :: Ctxt -> Text -> IO (Maybe Mode)
getModeByAccount ctxt account =
  withResource (db ctxt) $ \c -> listToMaybe <$> query c "select id, name, account from modes where account = ?" (Only account)

getOrCreateMode :: Ctxt -> Text -> Text -> IO Mode
getOrCreateMode ctxt account mode =
  withResource (db ctxt) $ \c -> do res <- query c "select id, name, account from modes where name = ? and account = ?" (mode, account)
                                    case res of
                                      (x:_) -> return x :: IO Mode
                                      [] -> head <$> query c "insert into modes (name, account) values (?,?) returning id, name, account" (mode, account)

newTodo :: Ctxt -> Mode -> Text -> Maybe UTCTime -> Maybe Repeat -> IO (Maybe Todo)
newTodo ctxt mode description deadline_at repeat_at = do
  withResource (db ctxt) $ \c -> listToMaybe <$> query c "insert into todos (description, mode_id, deadline_at, repeat_at) values (?,?,?,?) returning id, description, created_at, live_at, mode_id, snooze_till, deadline_at, repeat_at, repeat_times, magnitude, NULL as done_at" (description, mId mode, deadline_at, repeat_at)

getTodos :: Ctxt -> Mode -> IO [Todo]
getTodos ctxt mode =
  runQuery ctxt (activeTodos (mId mode))
  
getSnoozed :: Ctxt -> Mode -> IO [Todo]
getSnoozed ctxt mode =
  runQuery ctxt (snoozedTodos (mId mode))

getDones :: Ctxt -> Mode -> IO [Todo]
getDones ctxt mode =
  runQuery ctxt (doneTodos (mId mode))

getDonesForTodo :: Ctxt -> Todo -> IO [UTCTime]
getDonesForTodo ctxt todo =
  withResource (db ctxt) $ \c -> map (\(Only x) -> x) <$> query c "select created_at from dones where todo_id = ? order by created_at desc" (Only (tId todo))

getTodoById :: Ctxt -> Int -> Int -> IO (Maybe Todo)
getTodoById ctxt mode_id id =
  listToMaybe <$> runQuery ctxt (todoById mode_id id)

getTodo :: Ctxt -> Text -> Int -> IO (Maybe Todo)
getTodo ctxt account id = do
  m <- getModeByAccount ctxt account
  case m of
    Nothing -> return Nothing
    Just mode -> getTodoById ctxt (mId mode) id

getNotifications :: Ctxt -> Todo -> IO [Notification]
getNotifications ctxt todo =
  withResource (db ctxt) $ \c -> query c "select id, todo_id, created_at from notifications where todo_id = ? order by created_at desc" (Only $ tId todo)

newNotification :: Ctxt -> UTCTime -> Todo -> IO ()
newNotification ctxt lastsent todo =
  withResource (db ctxt) $ \c -> void $ execute c "insert into notifications (todo_id, created_at) (select ?, now() where not exists (select id from notifications where created_at > ? and todo_id = ?))" (tId todo, lastsent, tId todo)

markDone :: Ctxt -> Text -> Int -> IO ()
markDone ctxt account id = do
  mt <- getTodo ctxt account id
  case mt of
    Nothing -> return ()
    Just todo -> do
      withResource (db ctxt) $ \c -> void $ execute c "insert into dones (todo_id) (select T.id from todos as T join modes as M on M.id = T.mode_id where M.account = ? and T.id = ?)" (account, id)
      case (tDeadlineAt todo, tRepeatAt todo) of
        (Just dead, Just repeat) ->
          -- NOTE(dbp 2017-11-03): If it had been snoozed (which we determine
          -- based on snooze_till being more recent than the previous
          -- repetition, if it had existed), we will replicate the same period.
          -- If there was no snooze, we just snooze till the current deadline.
          let snooze = case tSnoozeTill todo of
                         Nothing -> dead
                         Just old_snooze -> if old_snooze > subRepeat dead repeat then addUTCTime (diffUTCTime old_snooze (subRepeat dead repeat)) dead else dead in
          updateTodo ctxt (todo { tDeadlineAt = Just $ addRepeat dead repeat, tSnoozeTill = Just snooze, tLiveAt = dead })
        _ -> return ()

markUndone :: Ctxt -> Text -> Int -> IO ()
markUndone ctxt account id =
  withResource (db ctxt) $ \c -> void $ execute c "delete from dones where id in (select D.id from dones as D join todos as T on T.id = D.todo_id join modes as M on M.id = T.mode_id where M.account = ? and T.id = ? order by D.created_at desc limit 1)" (account, id)

updateTodo :: Ctxt -> Todo -> IO ()
updateTodo ctxt todo =
  withResource (db ctxt) $ \c -> void $ execute c "update todos set description = ?, live_at = ?, deadline_at = ?, snooze_till = ?, repeat_at = ? where id = ?" (tDescription todo, tLiveAt todo, tDeadlineAt todo, tSnoozeTill todo, tRepeatAt todo, tId todo)


redirectIndex :: Text -> IO (Maybe Response)
redirectIndex account = redirect $ "/?acnt=" <> account

redirectTodo :: Text -> Todo -> IO (Maybe Response)
redirectTodo account todo = redirect $ "/todos/" <> tshow (tId todo) <> "/edit?acnt=" <> account

indexH :: Ctxt -> Text -> IO (Maybe Response)
indexH ctxt account = do
  defmode <- getOrCreateMode ctxt account "default"
  runForm ctxt "todo" (todoForm Nothing) $ \td ->
    case td of
      (_, Just (TodoData desc deadline repeat)) -> do mt <- newTodo ctxt defmode desc deadline repeat
                                                      case mt of
                                                        Nothing -> redirectIndex account
                                                        Just todo -> redirectTodo account todo
      (v, Nothing) -> do
        todos <- getTodos ctxt defmode
        renderWith ctxt (formFills v <> L.subs [("todos", L.mapSubs todoSubs todos)
                                               ,("account", L.textFill account)])
          "index"

archiveH :: Ctxt -> Text -> IO (Maybe Response)
archiveH ctxt account = do
  defmode <- getOrCreateMode ctxt account "default"
  dones <- getDones ctxt defmode
  renderWith ctxt (L.subs [("todos", L.mapSubs todoSubs dones)
                          ,("account", L.textFill account)])
    "archive"


snoozedH :: Ctxt -> Text -> IO (Maybe Response)
snoozedH ctxt account = do
  defmode <- getOrCreateMode ctxt account "default"
  dones <- getSnoozed ctxt defmode
  renderWith ctxt (L.subs [("todos", L.mapSubs todoSubs dones)
                          ,("account", L.textFill account)])
    "snoozed"


editH :: Ctxt -> Text -> Int -> IO (Maybe Response)
editH ctxt account id = do
  mtodo <- getTodo ctxt account id
  case mtodo of
    Nothing -> return Nothing
    Just todo ->
      runForm ctxt "todo" (todoForm $ Just todo) $ \td ->
        case td of
          (_, Just (TodoData desc deadline repeat)) -> do updateTodo ctxt (todo { tDescription = desc, tDeadlineAt = deadline, tRepeatAt = repeat})
                                                          redirectIndex account
          (v, Nothing) -> do
            ts <- getDonesForTodo ctxt todo
            renderWith ctxt (formFills v <> L.subs [("todo", L.fillChildrenWith $ todoSubs todo)
                                                   ,("account", L.textFill account)
                                                   ,("dones", if length ts > 0 then L.mapSubs (\t -> L.subs [("timestamp", L.textFill (mkTimestamp t))]) ts else L.textFill "")
                                                   ]) "edit"

doneH :: Ctxt -> Text -> Int -> IO (Maybe Response)
doneH ctxt account id = do
  markDone ctxt account id
  redirectIndex account

undoneH :: Ctxt -> Text -> Int -> IO (Maybe Response)
undoneH ctxt account id = do
  markUndone ctxt account id
  redirectIndex account

snoozeH :: Ctxt -> Text -> Int -> Text -> IO (Maybe Response)
snoozeH ctxt account id t = do
  mtodo <- getTodo ctxt account id
  case mtodo of
    Nothing -> return Nothing
    Just todo -> do
      UTCTime day time <- getCurrentTime
      let t' = case t of
                 "D" -> addDays 1 day
                 "W" -> addDays 7 day
                 "M" -> addGregorianMonthsClip 1 day
      updateTodo ctxt (todo { tSnoozeTill = Just (UTCTime t' time)})
      redirectIndex account

unSnoozeH :: Ctxt -> Text -> Int -> IO (Maybe Response)
unSnoozeH ctxt account id = do
  mtodo <- getTodo ctxt account id
  case mtodo of
    Nothing -> return Nothing
    Just todo -> do
      updateTodo ctxt (todo { tSnoozeTill = Nothing})
      redirectTodo account todo


updateH :: Ctxt -> Text -> Int -> Text -> IO (Maybe Response)
updateH ctxt account id txt = do
  mtodo <- getTodo ctxt account id
  case mtodo of
    Nothing -> return Nothing
    Just todo -> do
      updateTodo ctxt (todo { tDescription = txt})
      redirectIndex account

authed :: Ctxt -> Text -> IO (Maybe Response)
authed ctxt account =
  route ctxt [ end ==> flip indexH account
             , path "archive" ==> flip archiveH account
             , path "snoozed" ==> flip snoozedH account
             , path "todos" // segment // path "done" ==> flip doneH account
             , path "todos" // segment // path "undone" ==> flip undoneH account
             , path "todos" // segment // path "snooze" // param "t" ==> flip snoozeH account
             , path "todos" // segment // path "unsnooze" ==> flip unSnoozeH account
             , path "todos" // segment // path "edit" ==> flip editH account
             , path "todos" // segment // path "update" // param "txt" !=> flip updateH account
             ]

withAccount :: Ctxt -> Text -> IO (Maybe Response)
withAccount = authed

withMode :: Ctxt -> Int -> IO (Maybe Response)
withMode ctxt id = do m <- getMode ctxt id
                      case m of
                        Nothing -> return Nothing
                        Just mode -> authed ctxt (mAccount mode)

site :: Ctxt -> IO Response
site ctxt = route ctxt [ path "static" ==> staticServe "static"
                       -- cache busting version
                       , path "static" // segment ==> \c (_ :: Text) ->
                           staticServe "static" c
                       , param "acnt" ==> withAccount 
                       , param "mode_id" ==> withMode
                       ]
            `fallthrough` do r <- render ctxt "404"
                             case r of
                               Just r' -> return r'
                               Nothing -> notFoundText "Page not found"

notifyThread :: Ctxt -> IO ()
notifyThread ctxt = do
  modes <- getModes ctxt
  mapM_ (\mode -> do
            todos <- getTodos ctxt mode
            mapM_ (notifyTodo ctxt) todos) modes
  threadDelay 300000000 -- 5 minutes
  notifyThread ctxt

notifyTodo :: Ctxt -> Todo -> IO ()
notifyTodo ctxt todo = do
  case tDeadlineAt todo of
    Nothing -> return ()
    Just deadline -> do
      lastNotification <- listToMaybe <$> getNotifications ctxt todo
      let lastSent = case (nCreatedAt <$> lastNotification) of
                       Nothing -> tCreatedAt todo
                       Just not -> case tSnoozeTill todo of
                                     Nothing -> not
                                     Just snooze -> max snooze not
      now <- getCurrentTime
      if diffUTCTime now lastSent > diffUTCTime deadline now &&
        diffUTCTime now lastSent > 3600 -- never send more frequently than once per hour
        then do
          log' $ "Notification for todo ID " <> tshow (tId todo) <> " \"" <> tDescription todo <> "\""
          notify ctxt (Push.message [Push.text (tDescription todo), Push.text $ "\n by " <> (T.pack $ formatTime defaultTimeLocale "%F %I:%M%P" deadline) <> "\n", Push.link (siteurl ctxt <> "/todos/" <> (tshow (tId todo)) <> "/done?mode_id=" <> (tshow (tModeId todo))) [Push.text "Mark Done"]])
          newNotification ctxt lastSent todo
        else return ()
