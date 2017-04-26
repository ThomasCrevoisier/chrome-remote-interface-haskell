{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.DOM (
    module Chrome.API.DOM.Types
  , enable
  , disable
  , getDocument
  , querySelector
  , querySelectorAll
  , querySelectorAll'
  ) where

import Data.Aeson

import Data.Map (empty, insert)
import Chrome.Target.Message

import Chrome.Target.Client (sendCmd', TargetClient)

import Chrome.API.DOM.Types

enable :: Method ()
enable = Method "DOM.enable" ()

disable :: Method ()
disable = Method "DOM.disable" ()

getDocument :: Method ()
getDocument = Method "DOM.getDocument" ()

querySelector :: Int -> String -> Method QuerySelectorParam
querySelector nId selector = Method "DOM.querySelector" $ QuerySelectorParam nId selector


querySelectorAll :: Int -> String -> Method QuerySelectorParam
querySelectorAll nId = Method "DOM.querySelectorAll" . QuerySelectorParam nId

querySelectorAll' :: QuerySelectorParam -> TargetClient (Maybe QuerySelectorAllResponse)
querySelectorAll' = sendCmd' . Method "DOM.querySelectorAll"
