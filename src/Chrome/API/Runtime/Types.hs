{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Chrome.API.Runtime.Types where

import Data.Aeson
import Data.Aeson.TH

import Chrome.Target.Message.TH

type UnserializableValue = String

type RemoteObjectId = String

data RemoteObject = RemoteObject
                    { _type :: String
                    , subtype :: Maybe String
                    , className :: Maybe String
                    , value :: Maybe Value
                    , unserializableValue :: Maybe UnserializableValue
                    , description :: Maybe String
                    , objectId :: Maybe RemoteObjectId
                    } deriving Show

$(deriveJSON defaultOptions{
     omitNothingFields = True,
     fieldLabelModifier = let f "type" = "_type"
                              f str = str
                              in
                            f
} ''RemoteObject)

type ScriptId = String

data CallFrame = CallFrame
                 { functionName :: String
                 , scriptId :: ScriptId
                 , url :: String
                 , lineNumber :: Int
                 , columnNumber :: Int
                 } deriving Show

$(deriveJSON defaultOptions ''CallFrame)

data StackTrace = StackTrace
                  { description :: Maybe String
                  , callFrames :: [CallFrame]
                  , parent :: Maybe StackTrace
                  } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True} ''StackTrace)

type ExecutionContextId = Int

data ExceptionDetails = ExceptionDetails
                        { exceptionId :: Int
                        , text :: String
                        , lineNumber :: Int
                        , columnNumber :: Int
                        , scriptId :: Maybe ScriptId
                        , url :: Maybe String
                        , stackTrace :: Maybe StackTrace
                        , exception :: Maybe RemoteObject
                        , executionContextId :: Maybe ExecutionContextId
                        } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''ExceptionDetails)

data CallArgument = CallArgument
                    { value :: Maybe Value
                    , unserializableValue :: Maybe UnserializableValue
                    , objectId :: Maybe RemoteObjectId
                    } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''CallArgument)

data EvaluateParams = EvaluateParams
                      { expression :: String
                      , objectGroup :: Maybe String
                      , includeCommandLineAPI :: Maybe Bool
                      , silent :: Maybe Bool
                      , contextId :: Maybe ExecutionContextId
                      , returnByValue :: Maybe Bool
                      , generatePreview :: Maybe Bool
                      , userGesture :: Maybe Bool
                      , awaitPromise :: Maybe Bool
                      } deriving Show

$(deriveJSONMsg ''EvaluateParams)

data EvaluateResult = EvaluateResult
                      { result :: RemoteObject
                      , exceptionDetails :: Maybe ExceptionDetails
                      } deriving Show

$(deriveJSONMsg ''EvaluateResult)

data AwaitPromiseParams = AwaitPromiseParams
                          { promiseObjectId :: RemoteObjectId
                          , returnByValue :: Maybe Bool
                          , generatePreview :: Maybe Bool
                          } deriving Show

$(deriveJSONMsg ''AwaitPromiseParams)

data CallFunctionOnParams = CallFunctionOnParams
                            { objectId :: RemoteObjectId
                            , functionDeclaration :: String
                            , arguments :: Maybe [CallArgument]
                            , silent :: Maybe Bool
                            , returnByValue :: Maybe Bool
                            , generatePreview :: Maybe Bool
                            , userGesture :: Maybe Bool
                            , awaitPromise :: Maybe Bool
                            } deriving Show

$(deriveJSONMsg ''CallFunctionOnParams)

data GetPropertiesParams = GetPropertiesParams
                           { objectId :: RemoteObjectId
                           , ownProperties :: Maybe Bool
                           , accessorPropertiesOnly :: Maybe Bool
                           , generatePreview :: Maybe Bool
                           } deriving Show

$(deriveJSONMsg ''GetPropertiesParams)

data PropertyDescriptor = PropertyDescriptor
                          { name :: String
                          , value :: Maybe RemoteObject
                          , writeable :: Maybe Bool
                          , get :: Maybe RemoteObject
                          , set :: Maybe RemoteObject
                          , configurable :: Bool
                          , enumerable :: Bool
                          , wasThrown :: Maybe Bool
                          , isOwn :: Maybe Bool
                          , symbol :: Maybe RemoteObject
                          } deriving Show

$(deriveJSONMsg ''PropertyDescriptor)

data InternalPropertyDescriptor = InternalPropertyDescriptor
                                  { name :: String
                                  , value :: Maybe RemoteObject
                                  } deriving Show

$(deriveJSONMsg ''InternalPropertyDescriptor)

data GetPropertiesResult = GetPropertiesResult
                           { result :: [PropertyDescriptor]
                           , internalProperties :: [InternalPropertyDescriptor]
                           , exceptionDetail :: Maybe ExceptionDetails
                           } deriving Show

$(deriveJSONMsg ''GetPropertiesResult)

data ReleaseObjectParams = ReleaseObjectParams
                           { objectId :: RemoteObjectId }
                           deriving Show

$(deriveJSONMsg ''ReleaseObjectParams)

data ReleaseObjectGroupParams = ReleaseObjectGroupParams
                                { objectGroup :: String }
                                deriving Show

$(deriveJSONMsg ''ReleaseObjectGroupParams)

data CompileScriptParams = CompileScriptParams
                           { expression :: String
                           , sourceURL :: String
                           , persistScript :: Bool
                           , executionContextId :: Maybe ExecutionContextId
                           } deriving Show

$(deriveJSONMsg ''CompileScriptParams)

data CompileScriptResult = CompileScriptResult
                           { scriptId :: Maybe ScriptId
                           , exceptionDetails :: Maybe ExceptionDetails
                           } deriving Show

$(deriveJSONMsg ''CompileScriptResult)

data RunScriptParams = RunScriptParams
                       { scriptId :: ScriptId
                       , executionContextId :: Maybe ExecutionContextId
                       , objectGroup :: Maybe String
                       , silent :: Maybe Bool
                       , includeCommandLineAPI :: Maybe Bool
                       , returnByValue :: Maybe Bool
                       , generatePreview :: Maybe Bool
                       , awaitPromise :: Maybe Bool
                       } deriving Show

$(deriveJSONMsg ''RunScriptParams)
