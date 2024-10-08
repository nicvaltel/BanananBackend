{-# LANGUAGE PartialTypeSignatures #-}
module Adapter.HTTP.Common where


import Reexport
import ClassyPrelude
import Web.Scotty.Trans
import Web.Cookie
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



setByteStringValueInCookie :: MonadIO m => ByteString -> ByteString -> ActionT m ()
setByteStringValueInCookie bsName bsValue = do
  let oneMonth = 24 * 60 * 60 * 30
  curTime <- liftIO getCurrentTime
  setCookie $ def{
      setCookieName = bsName
    , setCookiePath = Just "/"
    , setCookieValue = bsValue
    , setCookieExpires = Just $ addUTCTime oneMonth curTime
    , setCookieHttpOnly = True
    , setCookieSecure = False
    , setCookieSameSite = Just sameSiteLax
    }
  

getCurrentUserId :: (D.SessionRepo m) => ActionT m (Maybe (D.SessionId, Maybe D.UserId))
getCurrentUserId = do
  maySidText <- getCookie "sId"
  mayUidText <- getCookie "uId"
  case maySidText of
    Nothing -> pure Nothing
    Just sId -> do
      let uId = mayUidText >>= safeRead . unpack
      pure $ Just (D.SessionId sId, D.UserId <$> uId)