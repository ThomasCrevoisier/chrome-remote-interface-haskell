{-# LANGUAGE OverloadedStrings #-}

module Chrome.Target.Client where

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

import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan

import Chrome.Target
import Chrome.DebuggingMessage

socketClient :: (TChan T.Text, TChan T.Text) -> WS.ClientApp ()
socketClient (inChan, outChan) conn = do
  readProc <- async $ forever $ do
    msgReceived <- WS.receiveData conn
    -- T.putStrLn msgReceived >> putStrLn "\n\n"
    atomically $ writeTChan outChan msgReceived

  writeProc <- async $ forever $ do
    msg <- atomically $ readTChan inChan
    -- T.putStrLn msg >> putStrLn "\n\n"
    WS.sendTextData conn msg

  mapM_ wait [readProc, writeProc]

type TargetClientChannels = (TChan T.Text, TChan T.Text)

createWSChannels :: IO TargetClientChannels
createWSChannels = (,) <$> newBroadcastTChanIO <*> newBroadcastTChanIO

type TargetClient = ReaderT TargetClientChannels IO

dupWSChannels :: TargetClient TargetClientChannels
dupWSChannels = do
  (chanCmd, chanRes) <- ask
  liftIO . atomically $ (,) <$> dupTChan chanCmd <*> dupTChan chanRes

sendCmd' :: (ToJSON req, Show res, FromJSON res) => Command req -> TargetClient (Maybe res)
sendCmd' cmd = do
  (chanCmd, chanRes) <- dupWSChannels
  msg <- liftIO $ commandToMsg cmd
  liftIO $ atomically $ writeTChan chanCmd (msgToText msg)
  liftIO $ wait =<< async (waitResponse chanRes (msgId msg))

  where
    waitResponse :: (FromJSON res) => TChan T.Text -> Int -> IO (Maybe res)
    waitResponse chanRes' id' = do
      res <- atomically $ readTChan chanRes'
      let decodedMsg = decode . B8.pack . T.unpack $ res
      case decodedMsg of
        Just (Result result) -> if _resId result == id'
                                  then return $ Just $ _resResult result
                                  else waitResponse chanRes' id'
        _ -> waitResponse chanRes' id'

listenToMethod :: (Show res, FromJSON res) => String -> (res -> IO ()) -> TargetClient ()
listenToMethod method f = do
  (chanCmd, chanRes) <- dupWSChannels
  liftIO $ forever $ do
    res <- atomically $ readTChan chanRes
    let event = decode . B8.pack . T.unpack $ res
    case event of
      Just (Event event') -> if _eventMethod event' == method
                                 then f $ _eventContent event'
                                 else return ()
      _ -> return ()

wsServer :: Target -> TargetClient (Maybe ())
wsServer page = case wsClientFromTarget page of
  Nothing -> pure Nothing
  Just (domain', port', path') -> do
    (chanCmd, chanRes) <- dupWSChannels
    server <- liftIO . async $ WS.runClient domain' (fromInteger port') path' (socketClient (chanCmd, chanRes))

    -- liftIO $ wait server

    return $ Just ()

withTarget :: Target -> TargetClient a -> IO ()
withTarget page actions = do
  channels <- createWSChannels
  runReaderT actions' channels
  where
    actions' :: TargetClient ()
    actions' = do
      wsServer page
      actions
      return ()
