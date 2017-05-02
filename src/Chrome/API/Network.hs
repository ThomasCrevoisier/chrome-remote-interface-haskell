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

enable :: NetworkEnableParams -> TargetClientAsync (MethodResult AnyResult)
enable = callMethod . Method "Network.enable"

disable :: TargetClientAsync (MethodResult AnyResult)
disable = callMethod $ Method "Network.disable" noParam

onRequestWillBeSent :: TargetClientAsync (MethodResult RequestEvent)
onRequestWillBeSent = listenToEventMethod "Network.requestWillBeSent"
