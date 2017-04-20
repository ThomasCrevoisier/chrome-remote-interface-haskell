{-# LANGUAGE OverloadedStrings #-}

module Chrome.InspectablePage where

import Data.Aeson
import Data.Maybe

import Network.HTTP.Client (newManager, httpLbs, defaultManagerSettings, responseBody)
import Network.URL

import Chrome.DebuggingURL

data InspectablePage = InspectablePage { _pageId :: String
                                       , _pageTitle :: String
                                       , _pageWSDebuggerURL :: DebuggingURL
                                       } deriving Show

instance FromJSON InspectablePage where
  parseJSON = withObject "InspectablePage" $ \o -> InspectablePage
                <$> o .: "id"
                <*> o .: "title"
                <*> o .: "webSocketDebuggerUrl"

fetchInspectablePages :: IO (Maybe [InspectablePage])
fetchInspectablePages = do
  manager <- newManager defaultManagerSettings
  res <- httpLbs "http://localhost:9222/json" manager
  return . decode . responseBody $ res

type WSPageSettings = (String, Integer, String)

wsClientFromPage :: InspectablePage -> Maybe WSPageSettings
wsClientFromPage (InspectablePage _ _ url) = let h = debuggingURLHost url
                                                 domain = host <$> h
                                                 port' = fromMaybe 80 . port <$> h
                                             in
                                               (,,) <$> domain <*> port' <*> pure (debuggingURLPath url)

