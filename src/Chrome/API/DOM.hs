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

enable :: TargetClientAsync (MethodResult AnyResult)
enable = callMethod $ Method "DOM.enable" noParam

disable :: TargetClientAsync (MethodResult AnyResult)
disable = callMethod $ Method "DOM.disable" noParam

getDocument :: TargetClientAsync (MethodResult GetDocumentResponse)
getDocument = callMethod $ Method "DOM.getDocument" noParam

requestChildNodes :: RequestChildNodesParams -> TargetClientAsync (MethodResult AnyResult)
requestChildNodes = callMethod . Method "DOM.requestChildNodes"

querySelector :: QuerySelectorParam -> TargetClientAsync (MethodResult QuerySelectorResponse)
querySelector = callMethod . Method "DOM.querySelector"

querySelectorAll :: QuerySelectorParam -> TargetClientAsync (MethodResult QuerySelectorAllResponse)
querySelectorAll = callMethod . Method "DOM.querySelectorAll"

setNodeName :: NodeNameParams -> TargetClientAsync (MethodResult NodeNameResult)
setNodeName = callMethod . Method "DOM.setNodeName"

setNodeValue :: NodeValueParams -> TargetClientAsync (MethodResult AnyResult)
setNodeValue = callMethod . Method "DOM.setNodeValue"

removeNode :: RemoveNodeParams -> TargetClientAsync (MethodResult AnyResult)
removeNode = callMethod . Method "DOM.removeNode"

setAttributeValue :: SetAttributeParams -> TargetClientAsync (MethodResult AnyResult)
setAttributeValue = callMethod . Method "DOM.setAttributeValue"

setAttributesAsText :: SetAttributesAsTextParams -> TargetClientAsync (MethodResult AnyResult)
setAttributesAsText = callMethod . Method "DOM.setAttributesAsText"

removeAttribute :: RemoveAttributeParams -> TargetClientAsync (MethodResult AnyResult)
removeAttribute = callMethod . Method "DOM.removeAttribute"

getOuterHTML :: NodeIdParam -> TargetClientAsync (MethodResult OuterHTML)
getOuterHTML = callMethod . Method "DOM.getOuterHTML"

setOuterHTML :: SetOuterHTMLParams -> TargetClientAsync (MethodResult AnyResult)
setOuterHTML = callMethod . Method "DOM.setOuterHTML"

requestNode :: RequestNodeParams -> TargetClientAsync (MethodResult NodeIdResult)
requestNode = callMethod . Method "DOM.requestNode"

highlightRect :: HighlightRectParams -> TargetClientAsync (MethodResult AnyResult)
highlightRect = callMethod . Method "DOM.highlightRect"

highlightNode :: HighlightNodeParams -> TargetClientAsync (MethodResult AnyResult)
highlightNode = callMethod . Method "DOM.highlightNode"

hideHighlight :: TargetClientAsync (MethodResult AnyResult)
hideHighlight = callMethod $ Method "DOM.hideHighlight" noParam

resolveNode :: ResolveNodeParams -> TargetClientAsync (MethodResult ResolveNodeResult)
resolveNode = callMethod . Method "DOM.resolveNode"

getAttributes :: NodeIdParam -> TargetClientAsync (MethodResult AttributesResult)
getAttributes = callMethod . Method "DOM.getAttributes"

moveTo :: MoveToParams -> TargetClientAsync (MethodResult NodeIdResult)
moveTo = callMethod . Method "DOM.moveTo"
