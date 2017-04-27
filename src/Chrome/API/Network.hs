{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.Network (
    module Chrome.API.Network.Types
  , enable
  , disable
  , onRequestWillBeSent
  ) where

import Data.Aeson (Value)
import Data.Map (Map, empty)

import Chrome.Target.Message
import Chrome.Target.Client

import Chrome.API.Network.Types

enable :: TargetClient (Maybe Value)
enable = callMethod $ (Method "Network.enable" empty :: Method (Map String String))

disable :: TargetClient (Maybe Value)
disable = callMethod $ (Method "Network.disable" empty :: Method (Map String String))

onRequestWillBeSent :: (RequestEvent -> IO ()) -> TargetClient ()
onRequestWillBeSent = listenToMethod "Network.requestWillBeSent"
