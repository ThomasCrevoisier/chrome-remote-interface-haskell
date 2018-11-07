{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Chrome.API.Input.Types where

import           Chrome.Target.Message.TH (deriveJSONMsg)

data KeyEvent = KeyEvent
                { _type                 :: String
                , modifiers             :: Maybe Int
                , timestamp             :: Maybe Double
                , text                  :: Maybe String
                , unmodifiedText        :: Maybe String
                , keyIdentifier         :: Maybe String
                , code                  :: Maybe String
                , key                   :: Maybe String
                , windowsVirtualKeyCode :: Maybe Int
                , nativeVirtualKeyCode  :: Maybe Int
                , autoRepeat            :: Maybe Bool
                , isKeypad              :: Maybe Bool
                , isSystemKey           :: Maybe Bool
                } deriving Show

$(deriveJSONMsg ''KeyEvent)

data MouseEvent = MouseEvent
                  { _type      :: String
                  , x          :: Int
                  , y          :: Int
                  , modifiers  :: Maybe Int
                  , timestamp  :: Maybe Double
                  , button     :: Maybe String
                  , clickCount :: Maybe Int
                  } deriving Show

$(deriveJSONMsg ''MouseEvent)
