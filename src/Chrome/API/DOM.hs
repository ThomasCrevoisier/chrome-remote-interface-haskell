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
import Chrome.Target.Client (callMethod, TargetClient)

import Chrome.API.DOM.Types

enable :: TargetClient (Maybe ())
enable = callMethod $ Method "DOM.enable" ()

disable :: TargetClient (Maybe ())
disable = callMethod $ Method "DOM.disable" ()

getDocument :: TargetClient (Maybe GetDocumentResponse)
getDocument = callMethod $ Method "DOM.getDocument" ()

querySelector :: QuerySelectorParam -> TargetClient (Maybe QuerySelectorResponse)
querySelector = callMethod . Method "DOM.querySelector"

querySelectorAll :: QuerySelectorParam -> TargetClient (Maybe QuerySelectorAllResponse)
querySelectorAll = callMethod . Method "DOM.querySelectorAll"
