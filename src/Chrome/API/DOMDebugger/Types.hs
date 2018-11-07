{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Chrome.API.DOMDebugger.Types where

import           Data.Aeson
import           Data.Aeson.TH

data DOMBreakpointParams = DOMBreakpointParams
                              { nodeId :: Int
                              , _type  :: String
                              } deriving Show

instance ToJSON DOMBreakpointParams where
  toJSON (DOMBreakpointParams nId t) = object [ "nodeId" .= nId, "type" .= t ]

data EventBreakpointParams = EventBreakpointParams
                             { eventName  :: String
                             , targetName :: Maybe String
                             } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''EventBreakpointParams)

data XHRBreakpointParams = XHRBreakpointParams
                           { url :: String }
                           deriving Show

$(deriveJSON defaultOptions ''XHRBreakpointParams)
