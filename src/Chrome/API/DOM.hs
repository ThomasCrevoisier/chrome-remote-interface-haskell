{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.DOM where

import Data.Aeson
import GHC.Generics

import Data.Map (empty, insert)
import Chrome.Target.Message

import Chrome.Target.Client (sendCmd', TargetClient)

enable :: Method ()
enable = Method "DOM.enable" ()

disable :: Method ()
disable = Method "DOM.disable" ()

getDocument :: Method ()
getDocument = Method "DOM.getDocument" ()

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

querySelector :: Int -> String -> Method QuerySelectorParam
querySelector nId selector = Method "DOM.querySelector" $ QuerySelectorParam nId selector

data QuerySelectorAllResponse = QuerySelectorAllResponse { _rNodeIds :: [Int] } deriving Show

instance FromJSON QuerySelectorAllResponse where
  parseJSON = withObject "response" $ \o -> QuerySelectorAllResponse
                                            <$> o .: "nodeIds"

querySelectorAll :: Int -> String -> Method QuerySelectorParam
querySelectorAll nId = Method "DOM.querySelectorAll" . QuerySelectorParam nId

querySelectorAll' :: QuerySelectorParam -> TargetClient (Maybe QuerySelectorAllResponse)
querySelectorAll' = sendCmd' . Method "DOM.querySelectorAll"

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
