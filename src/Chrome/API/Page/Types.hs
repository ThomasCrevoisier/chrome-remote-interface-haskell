{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.Page.Types where

import Data.Aeson

data PageReloadParams = PageReloadParams
                        {
                          _pIgnoreCache :: Bool
                        , _pScriptToEvaluateOnLoad :: Maybe String
                        } deriving Show

instance ToJSON PageReloadParams where
  toJSON (PageReloadParams ignoreCache script) = object [ "ignoreCache" .= ignoreCache
                                                        , "scriptToEvaluateOnLoad" .= script
                                                        ]

data PageHandleDialogParams = PageHandleDialogParams
                              {
                                _pAccept :: Bool
                              , _pPromptText :: Maybe String
                              } deriving Show

instance ToJSON PageHandleDialogParams where
  toJSON (PageHandleDialogParams accept promptText) = object [ "accept" .= accept
                                                             , "promptText" .= promptText
                                                             ]

data PageNavigateResult = PageNavigateResult
                          {
                            _pFrameId :: String
                          } deriving Show

instance FromJSON PageNavigateResult where
  parseJSON = withObject "frame" $ \o -> PageNavigateResult <$> o .: "frameId"

data CaptureScreenshotParams = CaptureScreenshotParams
                                {
                                  _pScreenshotFormat :: String
                                , _pScreenshotQuality :: Int
                                , _pScreenshotFromSurface :: Bool
                                } deriving Show

instance ToJSON CaptureScreenshotParams where
    toJSON (CaptureScreenshotParams format quality fromSurface) = object [ "format" .= format
                                                                         , "quality" .= quality
                                                                         , "fromSurface" .= fromSurface
                                                                         ]

data CaptureScreenshotResult = CaptureScreenshotResult
                                {
                                  _screenshotData :: String
                                } deriving Show

instance FromJSON CaptureScreenshotResult where
    parseJSON = withObject "screenshot" $ \o -> CaptureScreenshotResult <$> o .: "data"

newtype TimestampEvent = TimestampEvent Int
                         deriving Show

instance FromJSON TimestampEvent where
  parseJSON = withObject "timestamp" $ \o -> TimestampEvent <$> o .: "timestamp"
