{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Chrome.API.Profiler.Types where

import           Chrome.Target.Message.TH  (deriveJSONMsg)

import           Chrome.API.Debugger.Types (Location)
import           Chrome.API.Runtime.Types  (CallFrame)

data SamplingIntervalParam = SamplingIntervalParam
                             { interval :: Int }
                             deriving Show

$(deriveJSONMsg ''SamplingIntervalParam)

data ProfileNode = ProfileNode
                   { id          :: Int
                   , callFrame   :: CallFrame
                   , hitCount    :: Maybe Int
                   , children    :: Maybe [Int]
                   , deoptReason :: Maybe String
                   } deriving Show

$(deriveJSONMsg ''ProfileNode)

data Profile = Profile
               { nodes      :: [ProfileNode]
               , startTime  :: Double
               , endTime    :: Double
               , samples    :: Maybe [Int]
               , timeDeltas :: Maybe [Int]
               } deriving Show

$(deriveJSONMsg ''Profile)

data ProfileResult = ProfileResult
               { profile :: Profile }
               deriving Show

$(deriveJSONMsg ''ProfileResult)

data ProfileStartedEvent = ProfileStartedEvent
                           { id       :: String
                           , location :: Location
                           , title    :: Maybe String
                           } deriving Show

$(deriveJSONMsg ''ProfileStartedEvent)

data ProfileFinishedEvent = ProfileFinishedEvent
                            { id       :: String
                            , location :: Location
                            , profile  :: Profile
                            , title    :: Maybe String
                            } deriving Show

$(deriveJSONMsg ''ProfileFinishedEvent)
