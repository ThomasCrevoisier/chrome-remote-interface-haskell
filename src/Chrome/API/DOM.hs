module Chrome.API.DOM (
    module Chrome.API.DOM.Types
  , module Chrome.API.DOM
  ) where

import           Chrome.Target.Client  (TargetClientAsync, callMethod,
                                        listenToEventMethod)
import           Chrome.Target.Message

import           Chrome.API.DOM.Types

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

onDocumentUpdated :: TargetClientAsync (MethodResult AnyResult)
onDocumentUpdated = listenToEventMethod "DOM.documentUpdated"

onSetChildNodes :: TargetClientAsync (MethodResult SetChildNodesEvent)
onSetChildNodes = listenToEventMethod "DOM.setChildNodes"

onAttributeModified :: TargetClientAsync (MethodResult AttributeModifiedEvent)
onAttributeModified = listenToEventMethod "DOM.attributeModified"

onAttributeRemoved :: TargetClientAsync (MethodResult AttributeRemovedEvent)
onAttributeRemoved = listenToEventMethod "DOM.attributeRemoved"

onCharacterDataModified :: TargetClientAsync (MethodResult CharDataModifiedEvent)
onCharacterDataModified = listenToEventMethod "DOM.characterDataModified"

onChildNodeCountUpdated :: TargetClientAsync (MethodResult ChildNodeCountEvent)
onChildNodeCountUpdated = listenToEventMethod "DOM.childNodeCountUpdated"

onChildNodeInserted :: TargetClientAsync (MethodResult ChildNodeInsertedEvent)
onChildNodeInserted = listenToEventMethod "DOM.childNodeInserted"

onChildNodeRemoved :: TargetClientAsync (MethodResult ChildNodeRemovedEvent)
onChildNodeRemoved = listenToEventMethod "DOM.childNodeRemoved"
