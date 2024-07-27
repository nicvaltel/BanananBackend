{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.API.Auth where



import Web.Scotty.Trans
import ClassyPrelude
import qualified Domain.Server as D
import Adapter.HTTP.API.Common
import Adapter.HTTP.Common
import Text.Printf (printf)



-- routes :: (MonadUnliftIO m) => ScottyT m ()
-- routes = do
--   -- register
--   post "/api/auth/register" $ do
--    pure ()

--   -- verify email
--   post "/api/auth/verifyEmail" $ do
--    pure ()

--   -- login 
--   post "/api/auth/login" $ do
--         pure ()

--   -- get user
--   get "/api/users" $ do
--     pure ()




mkJsonIntPairString :: (String, Int) -> String
mkJsonIntPairString (name, val) = "\"" ++ name ++ "\":" ++ show val

wrapJsonStrings :: [String] -> String
wrapJsonStrings strs = "{" ++ intercalate "," strs ++ "}"

routes :: (MonadUnliftIO m, D.SessionRepo m) => ScottyT m ()
routes = do

  get "/api/users" $ do
    (D.SessionId sId, D.UserId uId) <- reqCurrentUserId
    setSessionIdInCookie (D.SessionId sId)
    let strJson = wrapJsonStrings $ map mkJsonIntPairString [("uId", uId), ("sId", sId)]
    json strJson