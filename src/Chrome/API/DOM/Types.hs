{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Chrome.API.DOM.Types where

import Data.Aeson
import Data.Aeson.TH

data RequestChildNodesParams = RequestChildNodesParams
                               {
                                 nodeId :: Int
                               , depth :: Maybe Int
                               } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''RequestChildNodesParams)

data QuerySelectorParam = QuerySelectorParam { nodeId :: Int
                                             , selector :: String
                                             } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''QuerySelectorParam)

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
            { nodeId :: Int
            , nodeName :: String
            } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''Node)

data GetDocumentResponse = GetDocumentResponse { root :: Node }
                           deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''GetDocumentResponse)

data NodeNameParams = NodeNameParams
                      { nodeId :: Int
                      , name :: String
                      } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''NodeNameParams)

data NodeNameResult = NodeNameResult
                      { nodeId :: Int }
                      deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''NodeNameResult)

data NodeValueParams = NodeValueParams
                       { nodeId :: Int
                       , value :: String
                       } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''NodeValueParams)

data RemoveNodeParams = RemoveNodeParams
                        { nodeId :: Int }
                        deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''RemoveNodeParams)

data SetAttributeParams = SetAttributeParams
                          { nodeId :: Int
                          , name :: String
                          , value :: String
                          } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''SetAttributeParams)

data SetAttributesAsTextParams = SetAttributesAsTextParams
                                 { nodeId :: Int
                                 , text :: String
                                 , name :: Maybe String
                                 } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''SetAttributesAsTextParams)

data RemoveAttributeParams = RemoveAttributeParams
                             { nodeId :: Int
                             , name :: String
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
                          { nodeId :: Int
                          , outerHTML :: String
                          } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''SetOuterHTMLParams)
