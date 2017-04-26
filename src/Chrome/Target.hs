{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chrome.Target where

import Data.Aeson
import Data.Maybe

import Network.HTTP.Client (newManager, httpLbs, defaultManagerSettings, responseBody)
import Network.URL

import Chrome.DebuggingURL

data Target = Target { _targetId :: String
                     , _targetTitle :: String
                     , _targetWSDebuggerURL :: DebuggingURL
                     } deriving Show

instance FromJSON Target where
  parseJSON = withObject "target" $ \o -> Target
                <$> o .: "id"
                <*> o .: "title"
                <*> o .: "webSocketDebuggerUrl"

fetchTargets :: IO (Maybe [Target])
fetchTargets = do
  manager <- newManager defaultManagerSettings
  -- TODO : allow to pass the address of Chrome as a param
  res <- httpLbs "http://localhost:9222/json" manager
  return . decode . responseBody $ res

type WSTargetSettings = (String, Integer, String)

wsClientFromTarget :: Target -> Maybe WSTargetSettings
wsClientFromTarget Target{..} = let h = debuggingURLHost _targetWSDebuggerURL
                                    domain = host <$> h
                                    port' = fromMaybe 80 . port <$> h
                                    in
                                        (,,) <$> domain <*> port' <*> pure (debuggingURLPath _targetWSDebuggerURL)
