{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.Network.Types where

import Data.Aeson
import Data.Map (Map)

data NetworkEnableParams = NetworkEnableParams
                           {
                             _netMaxTotalBufferSize :: Maybe Int
                           , _netMaxResourceBufferSize :: Maybe Int
                           } deriving Show

defaultEnableParams = NetworkEnableParams Nothing Nothing

instance ToJSON NetworkEnableParams where
  toJSON (NetworkEnableParams total resources) = object [ "maxTotalBufferSize" .= total
                                                        , "maxResourceBufferSize" .= resources
                                                        ]

data RequestEvent = RequestEvent { _eventRequest :: Request } deriving Show

instance FromJSON RequestEvent where
  parseJSON = withObject "response" $ \o -> RequestEvent <$> o .: "request"

data Request = Request { _reqUrl :: String } deriving Show

instance FromJSON Request where
  parseJSON = withObject "request" $ \o -> Request <$> o .: "url"

type Headers = Map String String

data ResponseBody = ResponseBody
                    {
                      _resBody :: String
                    , _resBase64Encoded :: Bool
                    } deriving Show

instance FromJSON ResponseBody where
  parseJSON = withObject "response" $ \o -> ResponseBody
                                           <$> o .: "body"
                                           <*> o .: "base64Encoded"

newtype CanClear = CanClear Bool
                   deriving Show

instance FromJSON CanClear where
  parseJSON = withObject "response" $ \o -> CanClear <$> o .: "result"

data NetworkConditionsParams = NetworkConditionsParams
                               {
                                 _netOffline :: Bool
                               , _netLatency :: Int
                               , _netDownloadThroughput :: Int
                               , _netUploadThroughput :: Int
                               , _netConnectionType :: Maybe String
                               } deriving Show

instance ToJSON NetworkConditionsParams where
  toJSON (NetworkConditionsParams offline latency dl ul connType) = object [ "offline" .= offline
                                                                           , "latency" .= latency
                                                                           , "downloadThroughput" .= dl
                                                                           , "uploadThroughput" .= ul
                                                                           , "connectionType" .= connType
                                                                           ]
