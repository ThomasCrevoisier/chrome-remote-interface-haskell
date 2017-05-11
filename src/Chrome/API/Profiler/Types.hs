{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Chrome.API.Profiler.Types where

import Data.Aeson
import Data.Aeson.TH

import Chrome.Target.Message.TH (deriveJSONMsg)

import Chrome.API.Runtime.Types (CallFrame)

data SamplingIntervalParam = SamplingIntervalParam
                             { interval :: Int }
                             deriving Show

$(deriveJSONMsg ''SamplingIntervalParam)

data ProfileNode = ProfileNode
                   { id :: Int
                   , callFrame :: CallFrame
                   , hitCount :: Maybe Int
                   , children :: Maybe [Int]
                   , deoptReason :: Maybe String
                   } deriving Show

$(deriveJSONMsg ''ProfileNode)

data Profile = Profile
               { nodes :: [ProfileNode]
               , startTime :: Double
               , endTime :: Double
               , samples :: Maybe [Int]
               , timeDeltas :: Maybe [Int]
               } deriving Show

$(deriveJSONMsg ''Profile)

data ProfileResult = ProfileResult
               { profile :: Profile }
               deriving Show

$(deriveJSONMsg ''ProfileResult)
