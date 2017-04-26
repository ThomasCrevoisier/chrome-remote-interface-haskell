{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chrome.Target.Message where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy.Char8 as B8

import Data.Map

import System.Random (randomRIO)
import Control.Applicative ((<|>))

data Method a = Method { _cmdMethod :: String
                       , _cmdParams :: a
                       } deriving Show

data OutgoingMsg a = OutgoingMsg Int (Method a) deriving Show

msgId :: OutgoingMsg a -> Int
msgId (OutgoingMsg id' _) = id'

instance ToJSON a => ToJSON (OutgoingMsg a) where
  toJSON (OutgoingMsg msgId cmd) = object [ "id" .= msgId
                                          , "method" .= _cmdMethod cmd
                                          , "params" .= _cmdParams cmd
                                          ]

msgToText :: ToJSON a => OutgoingMsg a -> Text
msgToText = T.pack . B8.unpack . encode

methodToMsg :: Method a -> IO (OutgoingMsg a)
methodToMsg cmd = flip OutgoingMsg cmd <$> (abs <$> randomRIO (1, 2000000))

data IncomingMsg a
  = Event (EventResponse a)
  | Result (MethodResult a)
  deriving (Show)

instance FromJSON a => FromJSON (IncomingMsg a) where
  parseJSON v = (Event <$> parseJSON v) <|> (Result <$> parseJSON v)

data EventResponse a = EventResponse { _eventMethod :: String, _eventContent :: a } deriving Show

instance FromJSON a => FromJSON (EventResponse a) where
  parseJSON = withObject "response" $ \o -> EventResponse
                                            <$> o .: "method"
                                            <*> o .: "params"

data MethodResult a = MethodResult { _resId :: Int
                                   , _resResult :: a
                                   } deriving Show

instance FromJSON a => FromJSON (MethodResult a) where
  parseJSON = withObject "response" $ \o -> MethodResult
                                            <$> o .: "id"
                                            <*> o .: "result"
