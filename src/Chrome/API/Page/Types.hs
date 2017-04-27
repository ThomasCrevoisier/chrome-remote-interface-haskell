{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.Page.Types where

import Data.Aeson

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
