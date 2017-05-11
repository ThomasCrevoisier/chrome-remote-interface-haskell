{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Chrome.API.Schema.Types where

import Data.Aeson
import Data.Aeson.TH

import Chrome.Target.Message.TH

data Domain = Domain
              { name :: String
              , version :: String
              } deriving Show

$(deriveJSONMsg ''Domain)

data DomainsResult = DomainsResult
                     { domains :: [Domain] }
                     deriving Show

$(deriveJSONMsg ''DomainsResult)
