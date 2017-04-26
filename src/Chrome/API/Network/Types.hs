{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.Network.Types where

import Data.Aeson

data RequestEvent = RequestEvent { _eventRequest :: Request } deriving Show

instance FromJSON RequestEvent where
  parseJSON = withObject "response" $ \o -> RequestEvent <$> o .: "request"

data Request = Request { _reqUrl :: String } deriving Show

instance FromJSON Request where
  parseJSON = withObject "request" $ \o -> Request <$> o .: "url"
