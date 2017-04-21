{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Chrome.DebuggingMessage where

import GHC.Generics

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy.Char8 as B8

import Data.Map

import System.Random (randomRIO)
import Control.Applicative ((<|>))

data Command a = Command { _cmdMethod :: String
                         , _cmdParams :: a
                         } deriving Show

data DebuggingMsg a = DebuggingMsg Int (Command a) deriving Show

msgId :: DebuggingMsg a -> Int
msgId (DebuggingMsg id' _) = id'

instance ToJSON a => ToJSON (DebuggingMsg a) where
  toJSON (DebuggingMsg msgId cmd) = object [ "id" .= msgId
                                           , "method" .= _cmdMethod cmd
                                           , "params" .= _cmdParams cmd
                                           ]

msgToText :: ToJSON a => DebuggingMsg a -> Text
msgToText = T.pack . B8.unpack . encode

commandToMsg :: Command a -> IO (DebuggingMsg a)
commandToMsg cmd = flip DebuggingMsg cmd <$> (abs <$> randomRIO (1, 2000000))

data WSResponse a
  = Event EventResponse
  | Result (DebuggingResponse a)
  deriving (Show, Generic)

instance FromJSON a => FromJSON (WSResponse a) where
  parseJSON v = (Event <$> parseJSON v) <|> (Result <$> parseJSON v)

data EventResponse = EventResponse { _eventMethod :: String } deriving Show

instance FromJSON EventResponse where
  parseJSON = withObject "response" $ \o -> EventResponse
                                            <$> o .: "method"

data DebuggingResponse a = DebuggingResponse { _resId :: Int
                                             , _resResult :: a
                                             } deriving Show

instance FromJSON a => FromJSON (DebuggingResponse a) where
  parseJSON = withObject "response" $ \o -> DebuggingResponse
                                            <$> o .: "id"
                                            <*> o .: "result"
