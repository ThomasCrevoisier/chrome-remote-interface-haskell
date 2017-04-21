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

import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan

import Chrome.InspectablePage
import Chrome.DebuggingMessage
import Chrome.API.Page
import Chrome.API.DOM

type WSClient = ReaderT WS.Connection IO

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

sendCmd :: (ToJSON req, Show res, FromJSON res) => Command req -> WSClient (Maybe res)
sendCmd cmd = do
  conn <- ask
  msg <- liftIO $ commandToMsg cmd

  res <- liftIO $ async $ waitForMessage (msgId msg) conn

  liftIO $ T.putStrLn (msgToText msg) >> putStrLn "\n\n"

  liftIO $ do
    WS.sendTextData conn (msgToText msg)
    wait res
  where
    waitForMessage :: (Show res, FromJSON res) => Int -> WS.Connection -> IO (Maybe res)
    waitForMessage id' c = do
      msgReceived <- WS.receiveData c
      liftIO $ T.putStrLn msgReceived >> putStrLn "\n\n"
      let decodedMsg = decode . B8.pack . T.unpack $ msgReceived
      let decodedRes = _resResult <$> decodedMsg
      case _resId <$> decodedMsg of
        Nothing -> waitForMessage id' c
        Just msgId -> if msgId == id'
                         then pure decodedRes
                         else waitForMessage id' c

executeOnPage :: WSClient a -> WS.ClientApp ()
executeOnPage commands = \conn -> do
  putStrLn "Connexion on client !"

  runReaderT commands conn

  WS.sendClose conn ("Bye !" :: Text)

type WSChannels = (TChan T.Text, TChan T.Text)

createWSChannels :: IO WSChannels
createWSChannels = (,) <$> newBroadcastTChanIO <*> newBroadcastTChanIO

type WSChannelsT = ReaderT WSChannels IO

dupWSChannels :: WSChannelsT WSChannels
dupWSChannels = do
  (chanCmd, chanRes) <- ask
  liftIO . atomically $ (,) <$> dupTChan chanCmd <*> dupTChan chanRes

-- TODO : remove it
dupCmdChannel :: WSChannelsT WSChannels
dupCmdChannel = do
  (chanCmd, chanRes) <- ask
  chanCmd' <- liftIO . atomically $ dupTChan chanCmd
  return (chanCmd', chanRes)

-- TODO : remove it
dupResChannel :: WSChannelsT WSChannels
dupResChannel = do
  (chanCmd, chanRes) <- ask
  chanRes' <- liftIO . atomically $ dupTChan chanRes
  return (chanCmd, chanRes')

sendCmd' :: (ToJSON req, Show res, FromJSON res) => Command req -> WSChannelsT (Maybe res)
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

listenToMethod :: String -> WSChannelsT ()
listenToMethod method = do
  (chanCmd, chanRes) <- dupWSChannels
  liftIO $ forever $ do
    res <- atomically $ readTChan chanRes
    let event = decode . B8.pack . T.unpack $ res :: Maybe (WSResponse Value)
    case event of
      Just (Event content) -> if _eventMethod content == method
                                 then print content
                                 else return ()
      _ -> return ()

wsServer :: InspectablePage -> WSChannelsT (Maybe ())
wsServer page = case wsClientFromPage page of
  Nothing -> pure Nothing
  Just (domain', port', path') -> do
    (chanCmd, chanRes) <- dupWSChannels
    server <- liftIO . async $ WS.runClient domain' (fromInteger port') path' (socketClient (chanCmd, chanRes))

    -- liftIO $ wait server

    return $ Just ()

onPage' :: InspectablePage -> WSChannelsT a -> IO ()
onPage' page actions = do
  channels <- createWSChannels
  runReaderT actions' channels
  where
    actions' :: WSChannelsT ()
    actions' = do
      wsServer page
      actions
      return ()
      
onPage :: InspectablePage -> WSClient a -> IO ()
onPage page commands = case wsClientFromPage page of
                   Nothing -> putStrLn "Page got a wrong config"
                   Just (domain', port', path') -> withSocketsDo $ WS.runClient domain' (fromInteger port') path' (executeOnPage commands)
