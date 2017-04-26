{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.Network where

import Data.Aeson
import Data.Map (Map, empty)
import Chrome.Target.Message

enable :: Method (Map String String)
enable = Method "Network.enable" empty

disable :: Method (Map String String)
disable = Method "Network.disable" empty

eventRequestWillBeSent :: String
eventRequestWillBeSent = "Network.requestWillBeSent"

data RequestEvent = RequestEvent { _eventRequest :: Request } deriving Show

instance FromJSON RequestEvent where
  parseJSON = withObject "response" $ \o -> RequestEvent <$> o .: "request"

data Request = Request { _reqUrl :: String } deriving Show

instance FromJSON Request where
  parseJSON = withObject "request" $ \o -> Request <$> o .: "url"
