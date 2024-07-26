module Adapter.HTTP.API.Common where


import Reexport
import ClassyPrelude
import Data.Aeson
import qualified Domain.Server as D
import Web.Scotty.Trans
import Adapter.HTTP.Common

-- * Error responce

errorResponce :: (ToJSON a) => a -> Value
errorResponce val = object [ "error" .= val]


-- * Session

reqCurrentUserId :: (D.SessionRepo m) => ActionT m (D.SessionId, D.UserId)
reqCurrentUserId = do
  maySessionIdUserId <- getCurrentUserId
  -- pure (D.SessionId 777, D.GuestUserId 555)
  case maySessionIdUserId of
    Nothing -> redirect "/auth/login"
    Just sessionIdUserId -> pure sessionIdUserId