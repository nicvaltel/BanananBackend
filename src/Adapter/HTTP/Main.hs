{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.Main (main) where

import ClassyPrelude
import Network.Wai
import Domain.Server (SessionRepo)

import qualified Adapter.HTTP.API.Main as API
import qualified Adapter.HTTP.Web.Main as Web
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Vhost ( vhost )


main :: 
  (MonadUnliftIO m, SessionRepo m) =>
  Int -> (m Response -> IO Response) -> IO ()
main port runner = do
  web <- Web.mainWeb runner
  run port web
  -- api <- API.mainAPI runner
  -- run port $ vhost [(pathBeginsWith "api", api)] web
  where
    pathBeginsWith path req = headMay (pathInfo req) == Just path
