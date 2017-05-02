{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chrome.Target.Message where

import Data.Aeson
import Data.Aeson.Types

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

noParam :: Value
noParam = emptyObject

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
  | Result (MethodResponse a)
  deriving (Show)

type AnyResult = Value

instance FromJSON a => FromJSON (IncomingMsg a) where
  parseJSON v = (Event <$> parseJSON v) <|> (Result <$> parseJSON v)

data EventResponse a = EventResponse { _eventMethod :: String, _eventContent :: MethodResult a } deriving Show

instance FromJSON a => FromJSON (EventResponse a) where
  parseJSON = withObject "response" $ \o -> EventResponse
                                            <$> o .: "method"
                                            <*> ((Right <$> o .: "params") <|> ((Left . ResponseMsgError) <$> o .: "error") <|> (pure $ Left ResponseParsingError))

data MethodResponse a = MethodResponse { _resId :: Int
                                       , _resResult :: MethodResult a
                                       } deriving Show

type MethodResult a = Either ResponseError a

data ResponseError
  = ResponseParsingError
  | ResponseMsgError ErrorMsg
  deriving Show

data ErrorMsg
  = ErrorMsg { _errCode :: Int
             , _errMessage :: String
             , _errData :: String
             } deriving Show

instance FromJSON ErrorMsg where
  parseJSON = withObject "error" $ \o -> ErrorMsg <$> o .: "code" <*> o .: "message" <*> o .: "data"

instance FromJSON a => FromJSON (MethodResponse a) where
  parseJSON = withObject "response" $ \o -> MethodResponse
                                            <$> o .: "id"
                                            <*> ((Right <$> o .: "result") <|> ((Left . ResponseMsgError) <$> o .: "error") <|> (pure $ Left ResponseParsingError))
