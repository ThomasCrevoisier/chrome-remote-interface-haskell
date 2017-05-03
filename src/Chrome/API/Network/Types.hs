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

data Request = Request { url :: String }
               deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''Request)

data RequestEvent = RequestEvent { request :: Request }
                    deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''RequestEvent)

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
