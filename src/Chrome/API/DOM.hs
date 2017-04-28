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

enable :: TargetClientAsync (MethodResult ())
enable = callMethod $ Method "DOM.enable" noParam

disable :: TargetClientAsync (MethodResult ())
disable = callMethod $ Method "DOM.disable" noParam

getDocument :: TargetClientAsync (MethodResult GetDocumentResponse)
getDocument = callMethod $ Method "DOM.getDocument" noParam

querySelector :: QuerySelectorParam -> TargetClientAsync (MethodResult QuerySelectorResponse)
querySelector = callMethod . Method "DOM.querySelector"

querySelectorAll :: QuerySelectorParam -> TargetClientAsync (MethodResult QuerySelectorAllResponse)
querySelectorAll = callMethod . Method "DOM.querySelectorAll"
