{-# LANGUAGE OverloadedStrings #-}

-- TODO : make a Tutorial module
module Main where

import Data.Foldable (traverse_)

import Control.Monad.Trans (liftIO)

import Chrome.Target
import Chrome.Target.Client
import Chrome.Target.Async (waitFor, onEvent, stopEventListener)

import qualified Chrome.API.Page as Page
import qualified Chrome.API.Network as Network

-- TODO : use Safe.headMay
head' :: [a] -> Maybe a
head' (x:_) = Just x
head' _ = Nothing

sampleCommands :: TargetClient ()
sampleCommands = do
  traverse_ waitFor [Page.enable, Network.enable Network.defaultEnableParams]

  listener <- onEvent Network.onRequestWillBeSent (liftIO . printRequest)

  _ <- waitFor $ Page.navigate "http://gitlab.com"
  _ <- waitFor Page.onLoadEventFired

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
