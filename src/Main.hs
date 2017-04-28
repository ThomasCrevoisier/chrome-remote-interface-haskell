{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base64 as B64

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Control.Concurrent.Async

import Chrome.Target
import Chrome.Target.Client

import qualified Chrome.API.Page as Page
import qualified Chrome.API.DOM as DOM
import qualified Chrome.API.Network as Network

head' :: [a] -> Maybe a
head' (x:_) = Just x
head' _ = Nothing

sampleCommands :: TargetClient ()
sampleCommands = do

  waitFor Page.enable

  waitFor $ Page.navigate "http://gitlab.com"

  waitFor Page.onLoadEventFired

  saveScreenshotAs "gitlab.jpeg"

  waitFor $ Page.navigate "http://github.com"

  saveScreenshotAs "github.jpeg"

  waitFor Page.disable

  return ()

  -- waitFor Network.enable
  --
  -- forever $ do
  --     request <- waitFor Network.onRequestWillBeSent
  --     liftIO $ printRequest request
  where
    saveScreenshotAs filename = do
        screenshot <- waitFor $ Page.captureScreenshot (Page.CaptureScreenshotParams "jpeg" 100 True)
        case screenshot of
            Right (Page.CaptureScreenshotResult img) -> case B64.decode $ B8.pack img of
                                                         Right imgContent -> liftIO $ B.writeFile filename imgContent
                                                         Left _ -> liftIO $ putStrLn "A wild error occured :O"
            Left err -> liftIO $ print err

    printRequest :: Network.RequestEvent -> IO ()
    printRequest (Network.RequestEvent (Network.Request url)) = print url

main :: IO ()
main = do
  pages <- fetchTargets
  let firstPage = head' =<< pages
  case firstPage of
    Nothing -> putStrLn "No page found"
    Just p -> withTarget p sampleCommands
