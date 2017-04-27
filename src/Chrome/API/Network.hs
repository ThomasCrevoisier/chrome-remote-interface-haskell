{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.Network (
    module Chrome.API.Network.Types
  , enable
  , disable
  , onRequestWillBeSent
  ) where

import Data.Aeson (Value)

import Control.Concurrent.Async

import Chrome.Target.Message
import Chrome.Target.Client

import Chrome.API.Network.Types

enable :: TargetClient (Maybe NoResult)
enable = callMethod $ Method "Network.enable" noParam

disable :: TargetClient (Maybe NoResult)
disable = callMethod $ Method "Network.disable" noParam

onRequestWillBeSent :: TargetClientAsync RequestEvent
onRequestWillBeSent = listenToEventMethod "Network.requestWillBeSent"
