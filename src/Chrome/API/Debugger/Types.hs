{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Chrome.API.Debugger.Types where

import Data.Aeson
import Data.Aeson.TH

import Chrome.API.Runtime.Types (RemoteObject, ExceptionDetails, CallArgument)

data BreakpointActiveParam = BreakpointActiveParam
                             { active :: Bool }
                             deriving Show

$(deriveJSON defaultOptions ''BreakpointActiveParam)

data SkipAllPausesParam = SkipAllPausesParam
                          { skip :: Bool }
                          deriving Show

$(deriveJSON defaultOptions ''SkipAllPausesParam)

data BreakpointByURLParams = BreakpointByURLParams
                             { lineNumber :: Int
                             , url :: Maybe String
                             , urlRegex :: Maybe String
                             , columnNumber :: Maybe Int
                             , condition :: Maybe String
                             } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''BreakpointByURLParams)

data Location = Location
                { scriptId :: String
                , lineNumber :: Int
                , columnNumber :: Maybe Int
                } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''Location)

data BreakpointByURLResult = BreakpointByURLResult
                             { breakpointId :: String
                             , locations :: [Location]
                             } deriving Show

$(deriveJSON defaultOptions ''BreakpointByURLResult)

data SetBreakpointParams = SetBreakpointParams
                           { location :: Location
                           , condition :: Maybe String
                           } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''SetBreakpointParams)

data SetBreakpointResult = SetBreakpointResult
                           { breakpointId :: String
                           , actualLocation :: Location
                           } deriving Show

$(deriveJSON defaultOptions ''SetBreakpointResult)

data RemoveBreakpointParams = RemoveBreakpointParams
                              { breakpointId :: String }
                              deriving Show

$(deriveJSON defaultOptions ''RemoveBreakpointParams)

data ContinueToLocationParams = ContinueToLocationParams
                                { location :: Location }
                                deriving Show

$(deriveJSON defaultOptions ''ContinueToLocationParams)

data SetScriptSourceParams = SetScriptSourceParams
                             { scriptId :: String
                             , scriptSource :: String
                             , dryRun :: Maybe Bool
                             } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''SetScriptSourceParams)

data RestartFrameParams = RestartFrameParams
                          { callFrameId :: String }
                          deriving Show

$(deriveJSON defaultOptions ''RestartFrameParams)

data GetScriptSourceParams = GetScriptSourceParams
                             { scriptId :: String }
                             deriving Show

$(deriveJSON defaultOptions ''GetScriptSourceParams)

data GetScriptSourceResult = GetScriptSourceResult
                             { scriptSource :: String }
                             deriving Show

$(deriveJSON defaultOptions ''GetScriptSourceResult)

data SetPauseOnExceptionsParams = SetPauseOnExceptionsParams
                                  { state :: String }
                                  deriving Show

$(deriveJSON defaultOptions ''SetPauseOnExceptionsParams)

data EvaluateOnCallFrameParams = EvaluateOnCallFrameParams
                                 { callFrameId :: String
                                 , expression :: String
                                 , objectGroup :: Maybe String
                                 , includeCommandLineAPI :: Maybe Bool
                                 , silent :: Maybe Bool
                                 , returnByValue :: Maybe Bool
                                 , generatePreview :: Maybe Bool
                                 } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''EvaluateOnCallFrameParams)

data EvaluateOnCallFrameResult = EvaluateOnCallFrameResult
                                 { result :: RemoteObject
                                 , exceptionDetails :: Maybe ExceptionDetails
                                 } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''EvaluateOnCallFrameResult)

data SetVariableValueParams = SetVariableValueParams
                              { scopeNumber :: Int
                              , variableName :: String
                              , newValue :: CallArgument
                              , callFrameId :: String
                              } deriving Show

$(deriveJSON defaultOptions ''SetVariableValueParams)

data SetAsyncCallStackDepthParams = SetAsyncCallStackDepthParams
                                    { maxDepth :: Int }
                                    deriving Show

$(deriveJSON defaultOptions ''SetAsyncCallStackDepthParams)
