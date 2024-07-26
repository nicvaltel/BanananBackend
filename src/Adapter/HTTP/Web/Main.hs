{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.Web.Main where

import Domain.Server
import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Static (staticPolicy', CacheContainer, addBase, initCaching, CachingStrategy (..))
import qualified Adapter.HTTP.Web.Auth as WebAuth


mainWeb :: 
  (MonadUnliftIO m, SessionRepo m) =>
  (m Response -> IO Response) -> IO Application
mainWeb runner = do
  cacheContainer <- initCaching PublicStaticCaching
  scottyAppT defaultOptions runner $ routes cacheContainer

routes :: 
  ( MonadUnliftIO m, SessionRepo m) =>
  CacheContainer -> ScottyT m ()
routes cachingStrategy= do

  middleware $ gzip $ def {gzipFiles = GzipCompress}
  middleware $ staticPolicy' cachingStrategy (addBase "static")

  WebAuth.routes

  notFound $ do
    status status404
    text "Not found"

  defaultHandler $ Handler $ \(e :: SomeException) -> do
    -- lift $ $(logTM) ErrorS $ "Unhandeled error: " <> ls (show e)
    status status500
    text "InternalServerError"
    
