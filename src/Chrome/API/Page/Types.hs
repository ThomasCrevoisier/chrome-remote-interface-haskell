{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Chrome.API.Page.Types where

import Data.Aeson
import Data.Aeson.TH

data PageReloadParams = PageReloadParams
                        {
                          ignoreCache :: Bool
                        , scriptToEvaluateOnLoad :: Maybe String
                        } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''PageReloadParams)

data PageHandleDialogParams = PageHandleDialogParams
                              {
                                accept :: Bool
                              , promptText :: Maybe String
                              } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''PageHandleDialogParams)

newtype FrameId = FrameId String
                  deriving Show

instance FromJSON FrameId where
  parseJSON = withObject "frame" $ \o -> FrameId <$> o .: "frameId"

data CaptureScreenshotParams = CaptureScreenshotParams
                                {
                                  format :: String
                                , quality :: Int
                                , fromSurface :: Bool
                                } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''CaptureScreenshotParams)

data CaptureScreenshotResult = CaptureScreenshotResult
                                {
                                  _screenshotData :: String
                                } deriving Show

instance FromJSON CaptureScreenshotResult where
    parseJSON = withObject "screenshot" $ \o -> CaptureScreenshotResult <$> o .: "data"

newtype TimestampEvent = TimestampEvent Double
                         deriving Show

instance FromJSON TimestampEvent where
  parseJSON = withObject "timestamp" $ \o -> TimestampEvent <$> o .: "timestamp"

data FrameAttachedEvent = FrameAttachedEvent
                          {
                            frameId :: String
                          , parentFrameId :: String
                          } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''FrameAttachedEvent)

data Frame = Frame
             {
               id :: String
             , parentId :: Maybe String
             , loadedId :: String
             , name :: Maybe String
             , url :: String
             , securityOrigin :: String
             , mimeType :: String
             } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''Frame)

type DialogType = String

data DialogOpeningEvent = DialogOpeningEvent
                          {
                            _dialogMessage :: String
                          , _dialogType :: DialogType
                          } deriving Show

instance FromJSON DialogOpeningEvent where
  parseJSON = withObject "dialog" $ \o -> DialogOpeningEvent
                                          <$> o .: "message"
                                          <*> o .: "type"

newtype DialogClosingEvent = DialogClosingEvent Bool
                             deriving Show

instance FromJSON DialogClosingEvent where
  parseJSON = withObject "param" $ \o -> DialogClosingEvent <$> o .: "result"

data NavigationRequestEvent = NavigationRequestEvent
                              {
                                isInMainFrame :: Bool
                              , isRedirect :: Bool
                              , navigationId :: Int
                              , url :: String
                              } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''NavigationRequestEvent)
