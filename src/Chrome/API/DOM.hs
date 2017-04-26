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
import Chrome.Target.Client (sendCmd', TargetClient)

import Chrome.API.DOM.Types

enable :: TargetClient (Maybe ())
enable = sendCmd' $ Method "DOM.enable" ()

disable :: TargetClient (Maybe ())
disable = sendCmd' $ Method "DOM.disable" ()

getDocument :: TargetClient (Maybe GetDocumentResponse)
getDocument = sendCmd' $ Method "DOM.getDocument" ()

querySelector :: QuerySelectorParam -> TargetClient (Maybe QuerySelectorResponse)
querySelector = sendCmd' . Method "DOM.querySelector"

querySelectorAll :: QuerySelectorParam -> TargetClient (Maybe QuerySelectorAllResponse)
querySelectorAll = sendCmd' . Method "DOM.querySelectorAll"
