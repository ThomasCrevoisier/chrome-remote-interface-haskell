{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.DOM.Types where

import GHC.Generics
import Data.Aeson

data QuerySelectorParam = QuerySelectorParam { _qNodeId :: Int
                                             , _qSelector :: String
                                             } deriving Show

instance ToJSON QuerySelectorParam where
  toJSON (QuerySelectorParam nId selector) = object [ "nodeId" .= nId
                                                    , "selector" .= selector
                                                    ]

data QuerySelectorResponse = QuerySelectorResponse { _rNodeId :: Int } deriving Show

instance FromJSON QuerySelectorResponse where
  parseJSON = withObject "response" $ \o -> QuerySelectorResponse
                                            <$> o .: "nodeId"


type QuerySelectorAllParam = QuerySelectorParam

data QuerySelectorAllResponse = QuerySelectorAllResponse { _rNodeIds :: [Int] } deriving Show

instance FromJSON QuerySelectorAllResponse where
  parseJSON = withObject "response" $ \o -> QuerySelectorAllResponse
                                            <$> o .: "nodeIds"


data GetDocumentResponse
  = GetDocumentResponse { root :: Node }
  deriving (Show, Generic)

instance FromJSON GetDocumentResponse

data Node
  = Node { nodeId :: Int
         , nodeName :: String
         }
  deriving (Show, Generic)

instance FromJSON Node
