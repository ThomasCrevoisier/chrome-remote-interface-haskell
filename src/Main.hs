{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Maybe
import Data.Foldable (traverse_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base64 as B64

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Chrome.Target
import Chrome.Target.Client
import Chrome.Target.Async (waitFor, onEvent, stopEventListener)

import qualified Chrome.API.Page as Page
import qualified Chrome.API.DOM as DOM
import qualified Chrome.API.Network as Network

head' :: [a] -> Maybe a
head' (x:_) = Just x
head' _ = Nothing

sampleCommands :: TargetClient ()
sampleCommands = do
  traverse waitFor [Page.enable, Network.enable]

  listener <- onEvent Network.onRequestWillBeSent (liftIO . printRequest)

  waitFor $ Page.navigate "http://gitlab.com"
  waitFor $ Page.onLoadEventFired

  stopEventListener listener

  traverse_ waitFor [Page.disable, Network.disable]

  where
    printRequest (Right (Network.RequestEvent (Network.Request url))) = print url
    printRequest _ = putStrLn "Oopsy"

main :: IO ()
main = do
  pages <- fetchTargets
  let firstPage = head' =<< pages
  case firstPage of
    Nothing -> putStrLn "No page found"
    Just p -> withTarget p sampleCommands
