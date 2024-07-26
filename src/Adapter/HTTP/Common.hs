{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.Common where


import Reexport
import Web.Scotty.Trans
import Web.Cookie
import Data.Text.Lazy (toStrict)
import ClassyPrelude (decodeUtf8, Utf8 (encodeUtf8), unpack)
import qualified Domain.Server as D
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Text.Encoding as T
import Data.Time


-- toResult :: Either e a -> DF.Result e a
-- toResult = either DF.Error DF.Success

setCookie :: MonadIO m => SetCookie -> ActionT m ()
setCookie = setHeader "Set-Cookie" . decodeUtf8 . toLazyByteString . renderSetCookie


getCookie :: Monad m => Text -> ActionT m (Maybe Text)
getCookie key = do
  mCookieStr <- header "Cookie"
  pure $ do
    cookie <- parseCookies . encodeUtf8 . toStrict <$> mCookieStr
    let bsKey = encodeUtf8 key
    val <- lookup bsKey cookie
    pure $ decodeUtf8 val



setSessionIdInCookie :: MonadIO m => D.SessionId -> ActionT m ()
setSessionIdInCookie (D.SessionId sId) = do
  let oneMonth = 24 * 60 * 60 * 30
  curTime <- liftIO getCurrentTime
  setCookie $ def{
      setCookieName = "sId"
    , setCookiePath = Just "/"
    , setCookieValue = T.encodeUtf8 $ tshow sId
    , setCookieExpires = Just $ addUTCTime oneMonth curTime
    , setCookieHttpOnly = True
    , setCookieSecure = False
    , setCookieSameSite = Just sameSiteLax
    }
  

getCurrentUserId :: D.SessionRepo m => ActionT m (Maybe D.UserId)
getCurrentUserId = do
  maySessionId <- getCookie "sId"
  case maySessionId of
    Nothing -> pure Nothing
    Just sIdText -> case read (unpack sIdText) of
      Nothing -> pure Nothing
      Just (sId :: Int) -> lift $ D.resolveSessionId  (D.SessionId sId)

