{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.API.Main where


import ClassyPrelude
import qualified Domain.Server as D
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Middleware.Gzip
import qualified Adapter.HTTP.API.Auth as Auth
import Adapter.HTTP.API.Common

mainAPI :: 
  (MonadUnliftIO m, D.SessionRepo m) =>
  (m Response -> IO Response) -> IO Application
mainAPI runner = scottyAppT defaultOptions runner routesAPI

routesAPI :: 
  ( MonadUnliftIO m, D.SessionRepo m) =>
  ScottyT m ()
routesAPI = do
  middleware $ gzip $ def {gzipFiles = GzipCompress}

  Auth.routes

  notFound $ do
    status status404
    json $ errorResponce ("NotFound" :: Text)

  defaultHandler $ Handler $ \(e :: SomeException) -> do
    -- lift $ $(logTM) ErrorS $ "Unhandeled error: " <> ls (show e)
    status status500
    json ("InternalServerError" :: Text)