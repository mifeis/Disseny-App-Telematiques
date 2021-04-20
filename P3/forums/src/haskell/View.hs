
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module View
where
import           Found
import           Model

import           Develop.DatFw
import           Develop.DatFw.Widget
import           Develop.DatFw.Template
import           Text.Blaze

import           Control.Monad.IO.Class   -- imports liftIO
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Data.Time
import           Data.Semigroup
import           Language.Haskell.TH.Syntax
import           CMark

-- ---------------------------------------------------------------
-- Utilities for the view components
-- ---------------------------------------------------------------

instance ToMarkup Markdown where
    toMarkup (Markdown t) = preEscapedToMarkup $ commonmarkToHtml [optSafe] t


uidNameWidget :: UserId -> Widget ForumsApp
uidNameWidget uid = do
    uname <- maybe "???" udName <$> runDbAction (getUser uid)
    toWidget $ toMarkup uname

dateWidget :: UTCTime -> Widget ForumsApp
dateWidget time = do
    zt <- liftIO $ utcToLocalZonedTime time
    let locale = defaultTimeLocale
    toWidget $ toMarkup $ T.pack $ formatTime locale "%e %b %Y, %H:%M" zt

pidPostedWidget :: PostId -> Widget ForumsApp
pidPostedWidget pid = do
    mbpost <- runDbAction $ getPost pid
    maybe "???" (dateWidget . pdPosted) mbpost

-- ---------------------------------------------------------------
-- Views
-- ---------------------------------------------------------------

homeView :: Maybe (UserId, UserD) -> Widget ForumsApp -> Widget ForumsApp
homeView mbuser fformw = do
    let isAdmin = maybe False (udIsAdmin . snd) mbuser
    forums <- runDbAction getForumList
    $(widgetTemplFile $ "src/templates/home.html")

forumView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> Widget ForumsApp -> Widget ForumsApp -> Widget ForumsApp
forumView mbuser (fid, forum) tformw eformw = do
    let isMod = maybe False ((==) (fdModeratorId forum) . fst) mbuser
    topics <- runDbAction $ getTopicList fid
    $(widgetTemplFile $ "src/templates/forum.html")

topicView :: Maybe (UserId, UserD) -> (TopicId, TopicD) -> Widget ForumsApp -> Widget ForumsApp
topicView mbuser (tid, topic) rformw = do
    forum <- runDbAction (getForum (tdForumId topic)) >>= maybe notFound pure
    let isMod = maybe False ((==) (fdModeratorId forum) . fst) mbuser
    replies <- runDbAction $ getPostList tid
    $(widgetTemplFile $ "src/templates/topic.html")