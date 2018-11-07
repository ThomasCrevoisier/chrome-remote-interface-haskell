module Chrome.API.Schema where

import           Chrome.Target.Client
import           Chrome.Target.Message

import           Chrome.API.Schema.Types

getDomains :: TargetClientAsync (MethodResult DomainsResult)
getDomains = callMethod $ Method "Schema.getDomains" noParam

