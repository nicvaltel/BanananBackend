module Adapter.HTTP.Web.Common where



import Reexport
import Web.Scotty.Trans
import Domain.Server
-- import qualified Text.Digestive.View as DF
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as H
import Adapter.HTTP.Common


-- * Views
renderHtml :: MonadIO m => H.Html -> ActionT m ()
renderHtml = html . H.renderHtml

-- mainLayout :: Text -> H.Html -> H.Html
-- mainLayout title content =
--   H.docTypeHtml $ do
--     H.head $ do
--       favicon "/static/images/logo.png"
--       H.title $ H.toHtml title
--     H.body $ do
--       H.div $ H.img ! A.src "/static/images/logo.png"
--       H.div content
--   where
--     favicon path =
--       H.link  ! A.rel   "icon" 
--               ! A.type_ "image/png"
--               ! A.href  path

-- formLayout :: DF.View a -> Text -> H.Html -> H.Html
-- formLayout view action =
--   H.form  ! A.method "POST"
--           ! A.enctype (H.toValue $ show $ DF.viewEncType view)
--           ! A.action (H.toValue action)

-- -- * Session

-- reqCurrentUserId :: SessionRepo m => ActionT m UserId
-- reqCurrentUserId = do
--   mUserId <- getCurrentUserId
--   case mUserId of
--     Nothing -> redirect "/auth/login"
--     Just userId -> pure userId