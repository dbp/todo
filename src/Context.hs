module Context where

import Web.Fn
import Data.Pool
import Database.PostgreSQL.Simple
import Text.Digestive.Larceny
import qualified Network.Pushover as Push
import Data.Text (Text)

data Ctxt = Ctxt { _req     :: FnRequest
                 , db       :: Pool Connection
                 , library  :: Library
                 , pushover :: Maybe (Push.APIToken, Push.UserKey)
                 , siteurl :: Text
                 }

instance RequestContext Ctxt where
  getRequest = _req
  setRequest c r = c { _req = r }
