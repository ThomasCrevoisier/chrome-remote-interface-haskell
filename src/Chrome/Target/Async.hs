module Chrome.Target.Async where

import Control.Monad (forever)
import Control.Monad.Trans (liftIO)

import Control.Concurrent.Async as Async
import Control.Concurrent.Async.Lifted as AsyncL

import Chrome.Target.Client

waitFor :: TargetClientAsync a -> TargetClient a
waitFor action = liftIO . Async.wait =<< action

onEvent :: TargetClientAsync a -> (a -> TargetClient ()) -> TargetClient (AsyncL.Async b)
onEvent event action = AsyncL.async . forever $ waitFor event >>= action

stopEventListener :: AsyncL.Async a -> TargetClient ()
stopEventListener = AsyncL.cancel
