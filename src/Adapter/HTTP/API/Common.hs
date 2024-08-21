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

reqCurrentUserId :: (D.SessionRepo m) => ActionT m (D.SessionId, D.UserId, Maybe D.Token)
reqCurrentUserId = do
  undefined
  -- maySessionIdUserId <- getCurrentUserId
  -- case maySessionIdUserId of
  --   Just (sId, uId) -> pure (sId, uId, Nothing)
  --   Nothing -> lift $ do
  --     (sId, uId, token) <- D.initNewGuestSession
  --     pure (sId, uId, Just token)
