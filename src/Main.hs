{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client (newManager, httpLbs, defaultManagerSettings, responseBody)
import Data.Aeson
import Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B8

import qualified Data.Text.IO as T
import Data.Maybe
import Data.Map as M

import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Network.URL

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, wait)
import Control.Monad (forever, unless)

import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Chrome.Target
import Chrome.Target.Message
import Chrome.Target.Client
import Chrome.API.Page (navigate)
import Chrome.API.DOM (
  getDocument
  , GetDocumentResponse(..)
  , querySelector
  , Node(..)
  , QuerySelectorResponse
  , QuerySelectorParam(..)
  , querySelectorAll
  , QuerySelectorAllParam
  , QuerySelectorAllResponse
  )

import qualified Chrome.API.Network as CN

head' :: [a] -> Maybe a
head' (x:_) = Just x
head' _ = Nothing

sampleCommands :: TargetClient ()
sampleCommands = do
  sendCmd' $ navigate "http://gitlab.com" :: TargetClient (Maybe Value)
  doc <- getDocument
  liftIO $ print doc

  case doc of
    Nothing -> liftIO $ putStrLn "No document found :/"
    Just doc' -> do
      nodes <- querySelectorAll (QuerySelectorParam (nodeId . root $ doc') "a")
      -- nodes <- sendCmd' $ querySelectorAll (nodeId . root $ doc') "a" :: TargetClient (Maybe QuerySelectorAllResponse)
      liftIO $ print nodes

  -- sendCmd' CN.enable :: TargetClient (Maybe Value)

  -- listenToMethod CN.eventRequestWillBeSent printRequest

  return ()
  where
    printRequest :: CN.RequestEvent -> IO ()
    printRequest (CN.RequestEvent (CN.Request url)) = liftIO $ print url

main :: IO ()
main = do
  pages <- fetchTargets
  let firstPage = head' =<< pages
  case firstPage of
    Nothing -> putStrLn "No page found"
    Just p -> do
      withTarget p sampleCommands
