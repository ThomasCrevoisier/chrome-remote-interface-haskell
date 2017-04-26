{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Maybe

import Control.Monad.Trans
import Control.Monad.Trans.Reader

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
  Page.navigate "http://gitlab.com"
  Network.enable
  Network.onRequestWillBeSent printRequest

  return ()
  where
    printRequest :: Network.RequestEvent -> IO ()
    printRequest (Network.RequestEvent (Network.Request url)) = liftIO $ print url

main :: IO ()
main = do
  pages <- fetchTargets
  let firstPage = head' =<< pages
  case firstPage of
    Nothing -> putStrLn "No page found"
    Just p -> withTarget p sampleCommands
