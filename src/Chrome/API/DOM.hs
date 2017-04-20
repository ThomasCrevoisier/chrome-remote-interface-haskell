{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.DOM where

import Data.Aeson
import GHC.Generics

import Data.Map (empty, insert)
import Chrome.DebuggingMessage

enable :: Command ()
enable = Command "DOM.enable" ()

disable :: Command ()
disable = Command "DOM.disble" ()

getDocument :: Command ()
getDocument = Command "DOM.getDocument" ()

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

querySelector :: Int -> String -> Command QuerySelectorParam
querySelector nId selector = Command "DOM.querySelector" $ QuerySelectorParam nId selector

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
