{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Chrome.API.Runtime.Types where

import Data.Aeson
import Data.Aeson.TH

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
