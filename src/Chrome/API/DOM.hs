{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.DOM (
    module Chrome.API.DOM.Types
  , enable
  , disable
  , getDocument
  , querySelector
  , querySelectorAll
  ) where

import Data.Aeson

import Chrome.Target.Message
import Chrome.Target.Client (callMethod, TargetClientAsync)

import Chrome.API.DOM.Types

enable :: TargetClientAsync (Maybe NoResult)
enable = callMethod $ Method "DOM.enable" noParam

disable :: TargetClientAsync (Maybe NoResult)
disable = callMethod $ Method "DOM.disable" noParam

getDocument :: TargetClientAsync (Maybe GetDocumentResponse)
getDocument = callMethod $ Method "DOM.getDocument" noParam

querySelector :: QuerySelectorParam -> TargetClientAsync (Maybe QuerySelectorResponse)
querySelector = callMethod . Method "DOM.querySelector"

querySelectorAll :: QuerySelectorParam -> TargetClientAsync (Maybe QuerySelectorAllResponse)
querySelectorAll = callMethod . Method "DOM.querySelectorAll"
