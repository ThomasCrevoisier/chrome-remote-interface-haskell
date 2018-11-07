{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Chrome.Target where

import           Data.Aeson
import           Data.Maybe

import           Network.HTTP.Client        (defaultManagerSettings, httpLbs,
                                             newManager, parseRequest,
                                             responseBody)
import           Network.URL

import           Chrome.Target.DebuggingURL

data Target = Target { _targetId            :: String
                     , _targetTitle         :: String
                     , _targetWSDebuggerURL :: DebuggingURL
                     } deriving Show

instance FromJSON Target where
  parseJSON = withObject "target" $ \o -> Target
                <$> o .: "id"
                <*> o .: "title"
                <*> o .: "webSocketDebuggerUrl"

fetchTargets :: String -> IO (Maybe [Target])
fetchTargets url = do
  req <- parseRequest $ url ++ "/json"
  manager <- newManager defaultManagerSettings
  res <- httpLbs req manager
  return . decode . responseBody $ res

type WSTargetSettings = (String, Integer, String)

wsClientFromTarget :: Target -> Maybe WSTargetSettings
wsClientFromTarget Target{..} = let h = debuggingURLHost _targetWSDebuggerURL
                                    domain = host <$> h
                                    port' = fromMaybe 80 . port <$> h
                                    in
                                        (,,) <$> domain <*> port' <*> pure (debuggingURLPath _targetWSDebuggerURL)
