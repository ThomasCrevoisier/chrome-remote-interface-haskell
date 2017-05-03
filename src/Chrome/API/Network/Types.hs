{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Chrome.API.Network.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Map (Map)

data NetworkEnableParams = NetworkEnableParams
                           {
                             maxTotalBufferSize :: Maybe Int
                           , maxResourceBufferSize :: Maybe Int
                           } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''NetworkEnableParams)


defaultEnableParams = NetworkEnableParams Nothing Nothing

type Headers = Map String String

data ResponseBody = ResponseBody
                    {
                      body :: String
                    , base64Encoded :: Bool
                    } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''ResponseBody)

newtype CanClear = CanClear Bool
                   deriving Show

instance FromJSON CanClear where
  parseJSON = withObject "response" $ \o -> CanClear <$> o .: "result"

data NetworkConditionsParams = NetworkConditionsParams
                               {
                                 offline :: Bool
                               , latency :: Int
                               , downloadThroughput :: Int
                               , uploadThroughput :: Int
                               , connectionType :: Maybe String
                               } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''NetworkConditionsParams)

data Initiator = Initiator
                 {
                   _type :: String
                 , url :: Maybe String
                 , lineNumber :: Maybe Int
                 } deriving Show

instance FromJSON Initiator where
  parseJSON = withObject "initiator" $ \o -> Initiator
                                             <$> o .: "type"
                                             <*> o .:? "url"
                                             <*> o .:? "lineNumber"

data Request = Request { url :: String }
               deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''Request)

data Response = Response
                {
                  url :: String
                , status :: Int
                , statusText :: String
                , headers :: Headers
                , headersText :: Maybe String
                , mimeType :: String
                , requestHeaders :: Maybe Headers
                , requestHeadersText :: Maybe String
                , connectionReused :: Bool
                , connectionId :: Int
                , remoteIPAddress :: Maybe String
                , remotePort :: Maybe Int
                , fromDiskCache :: Maybe Bool
                , fromServiceWorker :: Maybe Bool
                , encodedDataLength :: Int
                , protocol :: Maybe String
                } deriving Show

$(deriveFromJSON defaultOptions{ omitNothingFields = True } ''Response)

data RequestEvent = RequestEvent
                    { requestId :: String
                    , frameId :: String
                    , loaderId :: String
                    , documentURL :: String
                    , request :: Request
                    , timestamp :: Double
                    , wallTime :: Double
                    , initiator :: Initiator
                    , redirectResponse :: Maybe Response
                    }
                    deriving Show

$(deriveFromJSON defaultOptions{ omitNothingFields = True } ''RequestEvent)

data RequestFromCacheEvent = RequestFromCacheEvent { requestId :: String }
                             deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''RequestFromCacheEvent)

data ResponseReceivedEvent = ResponseReceivedEvent
                             { requestId :: String
                             , frameId :: String
                             , loaderId :: String
                             , timestamp :: Double
                             , response :: Response
                             } deriving Show

$(deriveFromJSON defaultOptions{ omitNothingFields = True} ''ResponseReceivedEvent)

data DataReceivedEvent = DataReceivedEvent
                         { requestId :: String
                         , timestamp :: Double
                         , dataLength :: Int
                         , encodedDataLength :: Int
                         } deriving Show

$(deriveFromJSON defaultOptions{ omitNothingFields = True} ''DataReceivedEvent)

data LoadingFinishedEvent = LoadingFinishedEvent
                            { requestId :: String
                            , timestamp :: Double
                            , encodedDataLength :: Int
                            } deriving Show

$(deriveFromJSON defaultOptions{ omitNothingFields = True} ''LoadingFinishedEvent)

data LoadingFailedEvent = LoadingFailedEvent
                          { requestId :: String
                          , timestamp :: Double
                          , errorText :: String
                          , canceled :: Maybe Bool
                          } deriving Show

$(deriveFromJSON defaultOptions{ omitNothingFields = True } ''LoadingFailedEvent)
