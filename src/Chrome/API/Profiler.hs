module Chrome.API.Profiler where

import           Chrome.Target.Client
import           Chrome.Target.Message

import           Chrome.API.Profiler.Types

enable :: TargetClientAsync (MethodResult AnyResult)
enable = callMethod $ Method "Profiler.enable" noParam

disable :: TargetClientAsync (MethodResult AnyResult)
disable = callMethod $ Method "Profiler.disable" noParam

setSamplingInterval :: SamplingIntervalParam -> TargetClientAsync (MethodResult AnyResult)
setSamplingInterval = callMethod . Method "Profiler.setSamplingInterval"

start :: TargetClientAsync (MethodResult AnyResult)
start = callMethod $ Method "Profiler.start" noParam

stop :: TargetClientAsync (MethodResult ProfileResult)
stop = callMethod $ Method "Profiler.stop" noParam

onConsoleProfileStarted :: TargetClientAsync (MethodResult ProfileStartedEvent)
onConsoleProfileStarted = listenToEventMethod "Profiler.consoleProfileStarted"

onConsoleProfileFinished :: TargetClientAsync (MethodResult ProfileFinishedEvent)
onConsoleProfileFinished = listenToEventMethod "Profiler.consoleProfileFinished"
