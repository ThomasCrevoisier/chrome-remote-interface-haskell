{-# LANGUAGE OverloadedStrings #-}

module Chrome.API.Page.Types where

import Data.Aeson

data CaptureScreenshotResult = CaptureScreenshotResult
                                {
                                  _screenshotData :: String
                                } deriving Show

instance FromJSON CaptureScreenshotResult where
    parseJSON = withObject "screenshot" $ \o -> CaptureScreenshotResult <$> o .: "data"
