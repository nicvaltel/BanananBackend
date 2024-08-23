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

reqCurrentUserId :: (MonadIO m, D.SessionRepo m) => ActionT m (D.SessionId, Maybe D.UserId)
reqCurrentUserId = do
  maySessionIdUserId <- getCurrentUserId
  case maySessionIdUserId of
    Just result@(sId,_) -> do
      maySD <- lift $ D.resolveSessionData sId
      case maySD of
        Nothing -> do
          sIdNew <- D.generateNewSessionId
          pure (sIdNew, Nothing)
        Just _ -> pure result
    Nothing -> do
      sId <- D.generateNewSessionId
      pure (sId, Nothing)
