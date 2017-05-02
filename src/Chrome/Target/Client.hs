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
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan

import Chrome.Target
import Chrome.Target.Message

socketClient :: (TChan T.Text, TChan T.Text) -> WS.ClientApp ()
socketClient (inChan, outChan) conn = do
  readProc <- async $ forever $ do
    msgReceived <- WS.receiveData conn
    T.putStrLn msgReceived >> putStrLn "\n\n"
    atomically $ writeTChan outChan msgReceived

  writeProc <- async $ forever $ do
    msg <- atomically $ readTChan inChan
    T.putStrLn msg >> putStrLn "\n\n"
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

callMethod :: (ToJSON req, Show res, FromJSON res) => Method req -> TargetClientAsync (MethodResult res)
callMethod cmd = do
  (chanCmd, chanRes) <- dupWSChannels
  msg <- liftIO $ methodToMsg cmd
  liftIO $ do
      atomically $ writeTChan chanCmd (msgToText msg)
      async (waitResponse chanRes (msgId msg))

  where
    waitResponse :: (FromJSON res) => TChan T.Text -> Int -> IO (MethodResult res)
    waitResponse chanRes' id' = do
      res <- atomically $ readTChan chanRes'
      let decodedMsg = decode . B8.pack . T.unpack $ res
      case decodedMsg of
        Just (Result result) -> if _resId result == id'
                                  then return $ _resResult result
                                  else waitResponse chanRes' id'
        _ -> waitResponse chanRes' id'


type TargetClientAsync res = TargetClient (Async res)

listenToEventMethod :: (FromJSON res) => String -> TargetClientAsync (MethodResult res)
listenToEventMethod method = do
    (_, chanRes) <- dupWSChannels
    liftIO $ async $ waitForMsg method chanRes
    where
        waitForMsg :: (FromJSON res) => String -> TChan T.Text -> IO (MethodResult res)
        waitForMsg method inChan = do
            res <- atomically $ readTChan inChan
            let event = decode . B8.pack . T.unpack $ res
            case event of
                Just (Event event') -> if _eventMethod event' == method
                                            then pure $ _eventContent event'
                                            else waitForMsg method inChan
                _ -> waitForMsg method inChan

wsServer :: Target -> TargetClient (Maybe ())
wsServer page = case wsClientFromTarget page of
  Nothing -> pure Nothing
  Just (domain', port', path') -> do
    (chanCmd, chanRes) <- dupWSChannels
    server <- liftIO . async $ WS.runClient domain' (fromInteger port') path' (socketClient (chanCmd, chanRes))

    -- TODO : remove this or use Either to encode error
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
