module Chrome.API.Network (
    module Chrome.API.Network.Types
  , enable
  , disable
  , onRequestWillBeSent
  ) where

import Data.Aeson (Value)
import Data.Map (Map, empty, insert)

import Chrome.Target.Message
import Chrome.Target.Client

import Chrome.API.Network.Types

enable :: NetworkEnableParams -> TargetClientAsync (MethodResult AnyResult)
enable = callMethod . Method "Network.enable"

disable :: TargetClientAsync (MethodResult AnyResult)
disable = callMethod $ Method "Network.disable" noParam

setUserAgentOverride :: String -> TargetClientAsync (MethodResult AnyResult)
setUserAgentOverride userAgent = callMethod $ Method "Network.setUserAgentOverride" (insert "userAgent" userAgent empty)

onRequestWillBeSent :: TargetClientAsync (MethodResult RequestEvent)
onRequestWillBeSent = listenToEventMethod "Network.requestWillBeSent"
