{-# LANGUAGE OverloadedStrings #-}

module Chrome.WSClient where

import Network.Socket (withSocketsDo)
import Network.WebSockets as WS

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Concurrent.Async (async, wait)

import Data.Aeson
import Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as B8

import Chrome.InspectablePage
import Chrome.DebuggingMessage

type WSClient = ReaderT WS.Connection IO

sendCmd :: (ToJSON req, FromJSON res) => Command req -> WSClient (Maybe res)
sendCmd cmd = do
  conn <- ask

  res <- liftIO $ async $ do
    msgReceived <- WS.receiveData conn
    liftIO $ T.putStrLn msgReceived
    liftIO $ putStrLn "\n\n"
    let decodedMsg = decode . B8.pack . T.unpack $ msgReceived
    return $ _resResult <$> decodedMsg

  msg <- liftIO $ commandToMsg cmd

  liftIO $ T.putStrLn (msgToText msg)
  liftIO $ putStrLn "\n\n"

  liftIO $ do
    WS.sendTextData conn (msgToText msg)
    wait res

executeOnPage :: WSClient a -> WS.ClientApp ()
executeOnPage commands = \conn -> do
  putStrLn "Connexion on client !"

  runReaderT commands conn

  WS.sendClose conn ("Bye !" :: Text)

onPage :: InspectablePage -> WSClient a -> IO ()
onPage page commands = case wsClientFromPage page of
                   Nothing -> putStrLn "Page got a wrong config"
                   Just (domain', port', path') -> withSocketsDo $ WS.runClient domain' (fromInteger port') path' (executeOnPage commands)
