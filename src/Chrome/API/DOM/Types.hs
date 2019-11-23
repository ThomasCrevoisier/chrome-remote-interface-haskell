{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Chrome.API.DOM.Types where

import GHC.Generics (Generic)
import           Data.Aeson
import           Data.Aeson.TH

import Chrome.Target.Message.Aeson (FromJSONMessage(..), ToJSONMessage(..))

data RequestChildNodesParams = RequestChildNodesParams
                               {
                                 nodeId :: Int
                               , depth  :: Maybe Int
                               } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''RequestChildNodesParams)

data QuerySelectorParam = QuerySelectorParam { nodeId   :: Int
                                             , selector :: String
                                             }
                                             deriving (Show, Generic)
                                             deriving FromJSON via (FromJSONMessage QuerySelectorParam)
                                             deriving ToJSON via (ToJSONMessage QuerySelectorParam)

data QuerySelectorResponse = QuerySelectorResponse { _rNodeId :: Int } deriving Show

instance FromJSON QuerySelectorResponse where
  parseJSON = withObject "response" $ \o -> QuerySelectorResponse
                                            <$> o .: "nodeId"


type QuerySelectorAllParam = QuerySelectorParam

data QuerySelectorAllResponse = QuerySelectorAllResponse
                                { nodeIds :: [Int] }
                                deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''QuerySelectorAllResponse)

data Node = Node
            { nodeId   :: Int
            , nodeName :: String
            } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''Node)

data GetDocumentResponse = GetDocumentResponse { root :: Node }
                           deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''GetDocumentResponse)

data NodeNameParams = NodeNameParams
                      { nodeId :: Int
                      , name   :: String
                      } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''NodeNameParams)

data NodeNameResult = NodeNameResult
                      { nodeId :: Int }
                      deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''NodeNameResult)

data NodeValueParams = NodeValueParams
                       { nodeId :: Int
                       , value  :: String
                       } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''NodeValueParams)

data RemoveNodeParams = RemoveNodeParams
                        { nodeId :: Int }
                        deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''RemoveNodeParams)

data SetAttributeParams = SetAttributeParams
                          { nodeId :: Int
                          , name   :: String
                          , value  :: String
                          } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''SetAttributeParams)

data SetAttributesAsTextParams = SetAttributesAsTextParams
                                 { nodeId :: Int
                                 , text   :: String
                                 , name   :: Maybe String
                                 } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''SetAttributesAsTextParams)

data RemoveAttributeParams = RemoveAttributeParams
                             { nodeId :: Int
                             , name   :: String
                             } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''RemoveAttributeParams)

newtype NodeIdParam = NodeIdParam Int

instance ToJSON NodeIdParam where
  toJSON (NodeIdParam nId) = object [ "nodeId" .= nId ]

data OuterHTML = OuterHTML
                 { outerHTML :: String }
                 deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''OuterHTML)

data SetOuterHTMLParams = SetOuterHTMLParams
                          { nodeId    :: Int
                          , outerHTML :: String
                          } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''SetOuterHTMLParams)

data RequestNodeParams = RequestNodeParams
                         { objectId :: String }
                         deriving Show

$(deriveJSON defaultOptions ''RequestNodeParams)

data NodeIdResult = NodeIdResult
                    { nodeId :: Int }
                    deriving Show

$(deriveJSON defaultOptions ''NodeIdResult)

data RGBA = RGBA
            { r :: Int
            , g :: Int
            , b :: Int
            , a :: Maybe Int
            } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''RGBA)

data HighlightRectParams = HighlightRectParams
                            { x            :: Int
                            , y            :: Int
                            , width        :: Int
                            , height       :: Int
                            , color        :: Maybe RGBA
                            , outlineColor :: Maybe RGBA
                            } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''HighlightRectParams)

data HighlightConfig = HighlightConfig
                       { showInfo           :: Maybe Bool
                       , showRulers         :: Maybe Bool
                       , showExtensionLines :: Maybe Bool
                       , displayAsMaterial  :: Maybe Bool
                       , contentColor       :: Maybe RGBA
                       , paddingColor       :: Maybe RGBA
                       , borderColor        :: Maybe RGBA
                       , marginColor        :: Maybe RGBA
                       , eventTargetColor   :: Maybe RGBA
                       , shapeColor         :: Maybe RGBA
                       , shapeMarginColor   :: Maybe RGBA
                       , selectorList       :: Maybe String
                       } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''HighlightConfig)

data HighlightNodeParams = HighlightNodeParams
                           { highlightConfig :: HighlightConfig
                           , nodeId          :: Maybe Int
                           , backendNodeId   :: Maybe Int
                           , objectId        :: Maybe String
                           } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''HighlightNodeParams)

data ResolveNodeParams = ResolveNodeParams
                         { nodeId      :: Int
                         , objectGroup :: Maybe String
                         } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''ResolveNodeParams)

data RemoteObject = RemoteObject
                    { _type               :: String
                    , subtype             :: String
                    , className           :: Maybe String
                    , value               :: Maybe Value
                    , unserializableValue :: Maybe String
                    , description         :: Maybe String
                    , objectId            :: Maybe String
                    } deriving Show


$(deriveJSON defaultOptions{
     omitNothingFields = True
   , fieldLabelModifier = let f "type" = "_type"
                              f str    = str
                              in
                            f
} ''RemoteObject)

data ResolveNodeResult = ResolveNodeResult
                         { object :: RemoteObject }
                         deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''ResolveNodeResult)

data AttributesResult = AttributesResult
                        { attributes :: [String] }
                        deriving Show

$(deriveJSON defaultOptions ''AttributesResult)

data MoveToParams = MoveToParams
                    { nodeId             :: Int
                    , targetNodeId       :: Int
                    , insertBeforeNodeId :: Maybe Int
                    } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''MoveToParams)

data SetChildNodesEvent = SetChildNodesEvent
                          { parentId :: Int
                          , nodes    :: [Node]
                          } deriving Show

$(deriveJSON defaultOptions ''SetChildNodesEvent)

data AttributeModifiedEvent = AttributeModifiedEvent
                              { nodeId :: Int
                              , name   :: String
                              , value  :: String
                              } deriving Show

$(deriveJSON defaultOptions ''AttributeModifiedEvent)

data AttributeRemovedEvent = AttributeRemovedEvent
                             { nodeId :: Int
                             , name   :: String
                             } deriving Show

$(deriveJSON defaultOptions ''AttributeRemovedEvent)

data CharDataModifiedEvent = CharDataModifiedEvent
                             { nodeId        :: Int
                             , characterData :: String
                             } deriving Show

$(deriveJSON defaultOptions ''CharDataModifiedEvent)

data ChildNodeCountEvent = ChildNodeCountEvent
                           { nodeId         :: Int
                           , childNodeCount :: Int
                           } deriving Show

$(deriveJSON defaultOptions ''ChildNodeCountEvent)

data ChildNodeInsertedEvent = ChildNodeInsertedEvent
                              { parentNodeId   :: Int
                              , previousNodeId :: Int
                              , node           :: Node
                              } deriving Show

$(deriveJSON defaultOptions ''ChildNodeInsertedEvent)

data ChildNodeRemovedEvent = ChildNodeRemovedEvent
                             { parentNodeId :: Int
                             , nodeId       :: Int
                             } deriving Show

$(deriveJSON defaultOptions ''ChildNodeRemovedEvent)
