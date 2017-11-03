{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module State where

import Control.Arrow (returnA, (>>>))
import Data.Monoid
import           Prelude hiding (sum, null, not)
import           Opaleye (Column, Nullable, matchNullable, isNull,
                          Table, queryTable, orderBy, desc, asc,
                           Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                           (.===), (.||), (.&&), (.<), (.>), not,
                           (.++), ifThenElse, pgString, aggregate, groupBy,
                           count, avg, sum, leftJoin, leftJoin,
                           showSqlForPostgres, Unpackspec, constant,
                           PGInt4, PGInt8, PGTimestamptz, PGText, PGDate, PGFloat8, PGBool)
import qualified Opaleye as O
import Opaleye.Table (table, tableColumn)
import Opaleye.Column (null, isNull)
import           Data.Profunctor.Product (p2, p3, p10)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Calendar
import Text.Parsec.Char
import Text.Parsec hiding (Column)

import Opaleye.Internal.Column (Column(..))
import Opaleye.Internal.HaskellDB.PrimQuery (PrimExpr(..))
import Opaleye.Internal.RunQuery (QueryRunnerColumnDefault(..), fieldQueryRunnerColumn)

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.FromField hiding (tableColumn)
import           Database.PostgreSQL.Simple.ToField
import Data.Pool

import Context

runQuery ctxt q = withResource (db ctxt) $ \c -> O.runQuery c q

pgNow :: Column PGTimestamptz
pgNow = Column (FunExpr "now" [])


data Todo' a b c d e f g h i j k = Todo { tId :: a
                                       , tDescription :: b
                                       , tCreatedAt :: c
                                       , tLiveAt :: d
                                       , tModeId :: e
                                       , tSnoozeTill :: f
                                       , tDeadlineAt :: g
                                       , tRepeatAt :: h
                                       , tRepeatTimes :: i
                                       , tMagnitude :: j
                                       , tDoneAt :: k
                                       }

type Todo = Todo' Int
                  Text
                  UTCTime
                  UTCTime
                  Int
                  (Maybe UTCTime)
                  (Maybe UTCTime)
                  (Maybe Repeat)
                  (Maybe Int)
                  (Maybe Int)
                  (Maybe UTCTime)

type TodoColumn = Todo'
                  (Column PGInt4)
                  (Column PGText)
                  (Column PGTimestamptz)
                  (Column PGTimestamptz)
                  (Column PGInt4)
                  (Column (Nullable PGTimestamptz))
                  (Column (Nullable PGTimestamptz))
                  (Column (Nullable PGText))
                  (Column (Nullable PGInt4))
                  (Column (Nullable PGInt4))
                  (Column (Nullable PGTimestamptz))

type TodoTable = (Column PGInt4,
                  Column PGText,
                  Column PGTimestamptz,
                  Column PGTimestamptz,
                  Column PGInt4,
                  Column (Nullable PGTimestamptz),
                  Column (Nullable PGTimestamptz),
                  Column (Nullable PGText),
                  Column (Nullable PGInt4),
                  Column (Nullable PGInt4))

deriving instance Show Todo

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
                 <*> field

data Repeat = RepeatDays Integer | RepeatMonths Integer

instance Show Repeat where
  show (RepeatDays n) = show n <> "D"
  show (RepeatMonths m) = show m <> "M"

instance Read Repeat where
  readsPrec _ s = case parseRepeat s of
                    Nothing -> []
                    Just r -> [(r, "")]

parseRepeat :: String -> Maybe Repeat
parseRepeat s = either (const Nothing) Just (parse repeatParser "" s)
    where repeatParser = do n <- read <$> many1 digit
                            res <- (oneOf "Dd" *> return (RepeatDays n)) <|>
                                   (oneOf "Mm" *> return (RepeatMonths n))
                            eof
                            return res


instance FromField Repeat where
  fromField f x = do s :: String <- fromField f x
                     maybe (returnError ConversionFailed f "Failed to parse field") return (parseRepeat s)

instance ToField Repeat where
  toField r = toField $ show r

instance QueryRunnerColumnDefault PGText Repeat where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

addRepeat :: UTCTime -> Repeat -> UTCTime
addRepeat (UTCTime day time) (RepeatDays n) = UTCTime (addDays n day) time
addRepeat (UTCTime day time) (RepeatMonths m) = UTCTime (addGregorianMonthsClip m day) time

subRepeat :: UTCTime -> Repeat -> UTCTime
subRepeat t (RepeatDays n) = addRepeat t (RepeatDays (-n))
subRepeat t (RepeatMonths n) = addRepeat t (RepeatMonths (-n))

data Mode = Mode { mId :: Int
                 , mName :: Text
                 , mAccount :: Text
                 }

instance FromRow Mode where
  fromRow = Mode <$> field
                 <*> field
                 <*> field

data Notification = Notification { nId :: Int
                                 , nTodoId :: Int
                                 , nCreatedAt :: UTCTime
                                 }

instance FromRow Notification where
  fromRow = Notification <$> field
                         <*> field
                         <*> field


$(makeAdaptorAndInstance "pTodo" ''Todo')

todoTable :: Table TodoTable TodoTable
todoTable = table "todos"
                (p10 ( tableColumn "id"
                     , tableColumn "description"
                     , tableColumn "created_at"
                     , tableColumn "live_at"
                     , tableColumn "mode_id"
                     , tableColumn "snooze_till"
                     , tableColumn "deadline_at"
                     , tableColumn "repeat_at"
                     , tableColumn "repeat_times"
                     , tableColumn "magnitude"))

doneTable :: Table (Column PGInt4, Column PGInt4, Column PGTimestamptz)
                   (Column PGInt4, Column PGInt4, Column PGTimestamptz)
doneTable = table "dones"
  (p3 (tableColumn "id", tableColumn "todo_id", tableColumn "created_at"))

allDones :: Query (Column PGInt4, Column PGInt4, Column PGTimestamptz)
allDones = queryTable doneTable

relevantDones :: Query (Column PGInt4, Column PGInt4, Column PGTimestamptz)
relevantDones = proc () -> do
  done@(_, todo_id', created_at') <- allDones -< ()
  (id,description,created_at,live_at,mode_id,snooze_till,deadline_at,repeat_at,repeat_times,magnitude) <- queryTable todoTable -< ()
  restrict -< id .== todo_id'
  restrict -< isNull deadline_at .|| (created_at' .> live_at)
  returnA -< done

allTodos :: Query (Column PGInt4, Column PGInt4, Column PGTimestamptz) -> Int -> Query (TodoTable, Column (Nullable PGTimestamptz))
allTodos dones mode_id' = proc () -> do
  ((id,description,created_at,live_at,mode_id,snooze_till,deadline_at,repeat_at,repeat_times,magnitude), (_, todo_id, created_at')) <- (leftJoin (queryTable todoTable)
                     dones
            (\((id,description,created_at,live_at,mode_id,snooze_till,deadline_at,repeat_at,repeat_times,magnitude), (_, todo_id, created_at')) -> todo_id .== id) :: Query (TodoTable, (Column (Nullable PGInt4), Column (Nullable PGInt4), Column (Nullable PGTimestamptz)))) -< ()
  restrict -< mode_id .== constant mode_id'
  returnA -< ((id,description,created_at,live_at,mode_id,snooze_till,deadline_at,repeat_at,repeat_times,magnitude), created_at')

activeTodos :: Int -> Query TodoColumn
activeTodos mode_id' =
    orderBy (desc tDeadlineAt <> asc tCreatedAt) $ proc () -> do
  ((id,description,created_at,live_at,mode_id,snooze_till,deadline_at,repeat_at,repeat_times,magnitude), created_at') <- allTodos relevantDones mode_id' -< ()
  restrict -< isNull created_at'
  restrict -< matchNullable (constant True) (\s -> s .<= pgNow) snooze_till
  returnA -< Todo id description created_at live_at mode_id snooze_till deadline_at repeat_at repeat_times magnitude created_at'

snoozedTodos :: Int -> Query TodoColumn
snoozedTodos mode_id' =
  orderBy (desc tDeadlineAt <> asc tCreatedAt) $ proc () -> do
  ((id,description,created_at,live_at,mode_id,snooze_till,deadline_at,repeat_at,repeat_times,magnitude), created_at') <- allTodos relevantDones mode_id' -< ()
  restrict -< isNull created_at'
  restrict -< matchNullable (constant False) (\s -> s .> pgNow) snooze_till
  returnA -< Todo id description created_at live_at mode_id snooze_till deadline_at repeat_at repeat_times magnitude created_at'


doneTodos :: Int -> Query TodoColumn
doneTodos mode_id' =
  orderBy (desc tDoneAt) $ proc () -> do
  ((id,description,created_at,live_at,mode_id,snooze_till,deadline_at,repeat_at,repeat_times,magnitude), created_at') <- allTodos allDones mode_id' -< ()
  restrict -< not (isNull created_at')
  returnA -< Todo id description created_at live_at mode_id snooze_till deadline_at repeat_at repeat_times magnitude created_at'

todoById :: Int -> Int -> Query TodoColumn
todoById mode_id' id' = proc () -> do
  ((id,description,created_at,live_at,mode_id,snooze_till,deadline_at,repeat_at,repeat_times,magnitude), created_at') <- allTodos relevantDones mode_id' -< ()
  restrict -< constant id' .== id
  restrict -< constant mode_id' .== mode_id
  returnA -< Todo id description created_at live_at mode_id snooze_till deadline_at repeat_at repeat_times magnitude created_at'
