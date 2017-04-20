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

import Chrome.DebuggingURL
import Chrome.InspectablePage
import Chrome.DebuggingMessage
import Chrome.WSClient
import Chrome.API.Page (navigate)
import Chrome.API.DOM (getDocument, GetDocumentResponse(..), querySelector, Node(..), QuerySelectorResponse)

head' :: [a] -> Maybe a
head' (x:_) = Just x
head' _ = Nothing

sampleCommands :: WSClient ()
sampleCommands = do
  sendCmd $ navigate "http://github.com" :: WSClient (Maybe ())
  doc <- sendCmd getDocument :: WSClient (Maybe GetDocumentResponse)
  case doc of
    Nothing -> liftIO $ putStrLn "No document found :/"
    Just doc' -> do
      node <- sendCmd $ querySelector (nodeId . root $ doc') "a" :: WSClient (Maybe QuerySelectorResponse)
      liftIO $ print node

  return ()

main :: IO ()
main = do
  pages <- fetchInspectablePages
  let firstPage = head' =<< pages
  case firstPage of
    Nothing -> putStrLn "No page found"
    Just p -> onPage p sampleCommands
