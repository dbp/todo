{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections, ScopedTypeVariables, PartialTypeSignatures     #-}

module Main where

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
import           GHC.IO.Encoding                    (setLocaleEncoding, utf8)
import           Network.Wai                        (Response, pathInfo)
import           Network.Wai.Handler.Warp           (run)
import           Network.Wai.Middleware.Rollbar
import           System.Environment                 (lookupEnv)
import           System.Environment                 (lookupEnv)
import           System.IO.Unsafe                   (unsafePerformIO)
import           Text.Digestive.Form
import           Text.Digestive.Larceny
import           Web.Fn
import           Web.Fn.Extra.Digestive
import           Web.Heroku                         (parseDatabaseUrl)
import qualified Web.Larceny                        as L
import Control.Logging
import System.Directory (listDirectory)


data Ctxt = Ctxt { _req    :: FnRequest
                 , db      :: Pool Connection
                 , library :: Library
                 }

instance RequestContext Ctxt where
  getRequest = _req
  setRequest c r = c { _req = r }

render :: Ctxt -> Text -> IO (Maybe Response)
render ctxt = renderWith ctxt mempty

renderWith :: Ctxt -> Substitutions -> Text -> IO (Maybe Response)
renderWith ctxt subs tpl =
  do t <- L.renderWith (library ctxt) subs () (T.splitOn "/" tpl)
     case t of
       Nothing -> return Nothing
       Just t' -> okHtml t'


initializer :: IO Ctxt
initializer =
  do lib <- L.loadTemplates "templates" L.defaultOverrides
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
     return (Ctxt defaultFnRequest pgpool lib)

main :: IO ()
main = do
  setLocaleEncoding utf8
  ctxt <- initializer
  port <- maybe 3000 read <$> lookupEnv "PORT"
  putStrLn $ "Listening on port " <> show port <>  "..."
  rb_token <- lookupEnv "ROLLBAR_ACCESS_TOKEN"
  let rb = case rb_token of
             Nothing -> id
             Just tok -> exceptions (Settings (fromString tok) "production" :: Settings '[])
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

data TodoData = TodoData Text (Maybe UTCTime)

data Todo = Todo { tId :: Int
                 , tDescription :: Text
                 , tCreatedAt :: UTCTime
                 , tModeId :: Int
                 , tSnoozeTill :: Maybe UTCTime
                 , tDeadlineAt :: Maybe UTCTime
                 , tRepeatAt :: Maybe Int
                 , tRepeatTimes :: Maybe Int
                 , tMagnitude :: Maybe Int
                 , tDoneAt :: Maybe UTCTime
                 }

instance FromRow Todo where
  fromRow = Todo <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field


data Mode = Mode { mId :: Int
                 , mName :: Text
                 , mAccount :: Text
                 }

instance FromRow Mode where
  fromRow = Mode <$> field
                 <*> field
                 <*> field

timezone :: TimeZone
timezone = unsafePerformIO getCurrentTimeZone

todoForm :: Maybe Todo -> Form Text IO TodoData
todoForm mt = TodoData <$> "description" .: text (tDescription <$> mt)
                       <*> "deadline_at" .: (fmap (\d -> UTCTime d 0) <$> (optionalDateFormlet "%F" (utctDay <$> (tDeadlineAt =<< mt))))

todoSubs :: Todo -> Substitutions
todoSubs t = L.subs
  [("id", L.textFill $ tshow $ tId t)
  ,("description", L.textFill $ tDescription t)
  ,("is-done", if isJust (tDoneAt t) then L.fillChildren else L.textFill "")
  ,("not-done", if isNothing (tDoneAt t) then L.fillChildren else L.textFill "")
  ,("deadline", if isNothing (tDeadlineAt t) then L.textFill "" else L.fillChildrenWith (L.subs [("timestamp", L.textFill $ T.pack $ formatTime defaultTimeLocale "%F" (fromJust (tDeadlineAt t)))]))
  ]


getMode :: Ctxt -> Text -> Text -> IO Mode
getMode ctxt account mode =
  withResource (db ctxt) $ \c -> do res <- query c "select id, name, account from modes where name = ? and account = ?" (mode, account)
                                    case res of
                                      (x:_) -> return x :: IO Mode
                                      [] -> head <$> query c "insert into modes (name, account) values (?,?) returning id, name, account" (mode, account)

newTodo :: Ctxt -> Mode -> Text -> Maybe UTCTime -> IO (Maybe Todo)
newTodo ctxt mode description deadline_at = do
  withResource (db ctxt) $ \c -> listToMaybe <$> query c "insert into todos (description, mode_id, deadline_at) values (?,?, ?) returning id, description, created_at, mode_id, snooze_till, deadline_at, repeat_at, repeat_times, magnitude, done_at" (description, mId mode, deadline_at)

getTodos :: Ctxt -> Mode -> IO [Todo]
getTodos ctxt mode =
  withResource (db ctxt) $ \c -> query c "select id, description, created_at, mode_id, snooze_till, deadline_at, repeat_at, repeat_times, magnitude, done_at from todos where mode_id = ? and done_at is null order by deadline_at desc, created_at asc" (Only $ mId mode)

getDones :: Ctxt -> Mode -> IO [Todo]
getDones ctxt mode =
  withResource (db ctxt) $ \c -> query c "select id, description, created_at, mode_id, snooze_till, deadline_at, repeat_at, repeat_times, magnitude, done_at from todos where mode_id = ? and done_at is not null order by deadline_at desc, created_at asc" (Only $ mId mode)

getTodo :: Ctxt -> Text -> Int -> IO (Maybe Todo)
getTodo ctxt account id =
  withResource (db ctxt) $ \c -> listToMaybe <$> query c "select T.id, description, created_at, mode_id, snooze_till, deadline_at, repeat_at, repeat_times, magnitude, done_at from todos as T join modes as M on M.id = T.mode_id where M.account = ? and T.id = ?" (account, id)

markDone :: Ctxt -> Text -> Int -> IO ()
markDone ctxt account id =
  withResource (db ctxt) $ \c -> void $ execute c "update todos set done_at = now() where id in (select T.id from todos as T join modes as M on M.id = T.mode_id where M.account = ? and T.id = ?)" (account, id)

markUndone :: Ctxt -> Text -> Int -> IO ()
markUndone ctxt account id =
  withResource (db ctxt) $ \c -> void $ execute c "update todos set done_at = NULL where id in (select T.id from todos as T join modes as M on M.id = T.mode_id where M.account = ? and T.id = ?)" (account, id)

updateTodo :: Ctxt -> Todo -> IO ()
updateTodo ctxt todo =
  withResource (db ctxt) $ \c -> void $ execute c "update todos set description = ?, deadline_at = ? where id = ?" (tDescription todo, tDeadlineAt todo, tId todo)


redirectIndex :: Text -> IO (Maybe Response)
redirectIndex account = redirect $ "/?acnt=" <> account

redirectEdit :: Text -> Todo -> IO (Maybe Response)
redirectEdit account todo = redirect $ "/todos/" <> tshow (tId todo) <> "/edit?acnt=" <> account

indexH :: Ctxt -> Text -> IO (Maybe Response)
indexH ctxt account = do
  defmode <- getMode ctxt account "default"
  runForm ctxt "todo" (todoForm Nothing) $ \td ->
    case td of
      (_, Just (TodoData desc deadline)) -> do mt <- newTodo ctxt defmode desc deadline
                                               case mt of
                                                 Nothing -> redirectIndex account
                                                 Just todo -> redirectEdit account todo
      (v, Nothing) -> do
        todos <- getTodos ctxt defmode
        dones <- getDones ctxt defmode
        renderWith ctxt (formFills v <> L.subs [("todos", L.mapSubs todoSubs $ todos <> dones)
                                               ,("account", L.textFill account)]) "index"

editH :: Ctxt -> Text -> Int -> IO (Maybe Response)
editH ctxt account id = do
  mtodo <- getTodo ctxt account id
  case mtodo of
    Nothing -> return Nothing
    Just todo ->
      runForm ctxt "todo" (todoForm $ Just todo) $ \td ->
        case td of
          (_, Just (TodoData desc deadline)) -> do updateTodo ctxt (todo { tDescription = desc, tDeadlineAt = deadline})
                                                   redirectIndex account
          (v, Nothing) -> do
            renderWith ctxt (formFills v <> L.subs [("todo", L.fillChildrenWith $ todoSubs todo)
                                                   ,("account", L.textFill account)]) "edit"

doneH :: Ctxt -> Text -> Int -> IO (Maybe Response)
doneH ctxt account id = do
  markDone ctxt account id
  redirectIndex account

undoneH :: Ctxt -> Text -> Int -> IO (Maybe Response)
undoneH ctxt account id = do
  markUndone ctxt account id
  redirectIndex account


updateH :: Ctxt -> Text -> Int -> Text -> IO (Maybe Response)
updateH ctxt account id txt = do
  mtodo <- getTodo ctxt account id
  case mtodo of
    Nothing -> return Nothing
    Just todo -> do
      updateTodo ctxt (todo { tDescription = txt})
      redirectIndex account

site :: Ctxt -> IO Response
site ctxt = route ctxt [ path "static" ==> staticServe "static"
                       , param "acnt" // end ==> indexH
                       , param "acnt" // path "todos" // segment // path "done" ==> doneH
                       , param "acnt" // path "todos" // segment // path "undone" ==> undoneH
                       , param "acnt" // path "todos" // segment // path "edit" ==> editH
                       , param "acnt" // path "todos" // segment // path "update" // param "txt" !=> updateH
                       , anything ==> larcenyServe
                       ] 
            `fallthrough` do r <- render ctxt "404"
                             case r of
                               Just r' -> return r'
                               Nothing -> notFoundText "Page not found"
