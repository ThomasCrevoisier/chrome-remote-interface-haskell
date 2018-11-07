module Chrome.API.Runtime where

import           Chrome.Target.Client
import           Chrome.Target.Message

import           Chrome.API.Runtime.Types

evaluate :: EvaluateParams -> TargetClientAsync (MethodResult EvaluateResult)
evaluate = callMethod . Method "Runtime.evaluate"

awaitPromise :: AwaitPromiseParams -> TargetClientAsync (MethodResult EvaluateResult)
awaitPromise = callMethod . Method "Runtime.awaitPromise"

callFunctionOn :: CallFunctionOnParams -> TargetClientAsync (MethodResult EvaluateResult)
callFunctionOn = callMethod . Method "Runtime.callFunctionOn"

getProperties :: GetPropertiesParams -> TargetClientAsync (MethodResult GetPropertiesResult)
getProperties = callMethod . Method "Runtime.getProperties"

releaseObject :: ReleaseObjectParams -> TargetClientAsync (MethodResult AnyResult)
releaseObject = callMethod . Method "Runtime.releaseObject"

releaseObjectGroup :: ReleaseObjectGroupParams -> TargetClientAsync (MethodResult AnyResult)
releaseObjectGroup = callMethod . Method "Runtime.releaseObjectGroup"

runIfWaitingForDebugger :: TargetClientAsync (MethodResult AnyResult)
runIfWaitingForDebugger = callMethod $ Method "Runtime.runIfWaitingForDebugger" noParam

enable :: TargetClientAsync (MethodResult AnyResult)
enable = callMethod $ Method "Runtime.enable" noParam

disable :: TargetClientAsync (MethodResult AnyResult)
disable = callMethod $ Method "Runtime.disable" noParam

discardConsoleEntries :: TargetClientAsync (MethodResult AnyResult)
discardConsoleEntries = callMethod $ Method "Runtime.discardConsoleEntries" noParam

compileScript :: CompileScriptParams -> TargetClientAsync (MethodResult CompileScriptResult)
compileScript = callMethod . Method "Runtime.compileScript"

runScript :: RunScriptParams -> TargetClientAsync (MethodResult EvaluateResult)
runScript = callMethod . Method "Runtime.runScript"

onExecutionContextCreated :: TargetClientAsync (MethodResult ContextCreatedEvent)
onExecutionContextCreated = listenToEventMethod "Runtime.executionContextCreated"

onExecutionContextDestroyed :: TargetClientAsync (MethodResult ContextDestroyedEvent)
onExecutionContextDestroyed = listenToEventMethod "Runtime.executionContextDestroyed"

onExecutionContextsCleared :: TargetClientAsync (MethodResult AnyResult)
onExecutionContextsCleared = listenToEventMethod "Runtime.executionContextsCleared"

onExceptionThrown :: TargetClientAsync (MethodResult ExceptionThrownEvent)
onExceptionThrown = listenToEventMethod "Runtime.exceptionThrown"

onExceptionRevoked :: TargetClientAsync (MethodResult ExceptionRevokedEvent)
onExceptionRevoked = listenToEventMethod "Runtime.exceptionRevoked"

onConsoleAPICalled :: TargetClientAsync (MethodResult ConsoleAPICalledEvent)
onConsoleAPICalled = listenToEventMethod "Runtime.consoleAPICalled"

onInspectRequested :: TargetClientAsync (MethodResult InspectRequestedEvent)
onInspectRequested = listenToEventMethod "Runtime.inspectRequested"
