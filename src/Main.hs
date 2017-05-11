{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.Foldable (traverse_)

import Control.Monad.Trans (liftIO)

import Chrome.Target
import Chrome.Target.Client
import Chrome.Target.Async (waitFor, onEvent, stopEventListener)

import qualified Chrome.API.Page as Page
import qualified Chrome.API.DOM as DOM
import qualified Chrome.API.Network as Network
import qualified Chrome.API.DOMDebugger as DOMDebugger
import qualified Chrome.API.Debugger as Debugger
import qualified Chrome.API.Emulation as Emulation
import qualified Chrome.API.Input as Input
import qualified Chrome.API.Profiler as Profiler
import qualified Chrome.API.Runtime as Runtime
import qualified Chrome.API.Schema as Schema

head' :: [a] -> Maybe a
head' (x:_) = Just x
head' _ = Nothing

sampleCommands :: TargetClient ()
sampleCommands = do
  traverse_ waitFor [Page.enable, Network.enable Network.defaultEnableParams]

  listener <- onEvent Network.onRequestWillBeSent (liftIO . printRequest)

  waitFor $ Page.navigate "http://gitlab.com"
  waitFor Page.onLoadEventFired

  stopEventListener listener

  traverse_ waitFor [Page.disable, Network.disable]

  where
    printRequest (Right event) = print event
    printRequest (Left err) = print err

main :: IO ()
main = do
  pages <- fetchTargets "http://localhost:9222"
  let firstPage = head' =<< pages
  case firstPage of
    Nothing -> putStrLn "No page found"
    Just p -> withTarget p sampleCommands
