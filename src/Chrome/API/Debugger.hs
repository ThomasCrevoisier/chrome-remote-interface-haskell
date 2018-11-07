module Chrome.API.Debugger where

import           Chrome.Target.Client
import           Chrome.Target.Message

import           Chrome.API.Debugger.Types

enable :: TargetClientAsync (MethodResult AnyResult)
enable = callMethod $ Method "Debugger.enable" noParam

disable :: TargetClientAsync (MethodResult AnyResult)
disable = callMethod $ Method "Debugger.disable" noParam

setBreakpointActive :: BreakpointActiveParam -> TargetClientAsync (MethodResult AnyResult)
setBreakpointActive = callMethod . Method "Debugger.setBreakpointActive"

setSkipAllPauses :: SkipAllPausesParam -> TargetClientAsync (MethodResult AnyResult)
setSkipAllPauses = callMethod . Method "Debugger.setSkipAllPauses"

setBreakpointByUrl :: BreakpointByURLParams -> TargetClientAsync (MethodResult BreakpointByURLResult)
setBreakpointByUrl = callMethod . Method "Debugger.setBreakpointByUrl"

setBreakpoint :: SetBreakpointParams -> TargetClientAsync (MethodResult SetBreakpointResult)
setBreakpoint = callMethod . Method "Debugger.setBreakpoint"

removeBreakpoint :: RemoveBreakpointParams -> TargetClientAsync (MethodResult AnyResult)
removeBreakpoint = callMethod . Method "Debugger.removeBreakpoint"

continueToLocation :: ContinueToLocationParams -> TargetClientAsync (MethodResult AnyResult)
continueToLocation = callMethod . Method "Debugger.continueToLocation"

stepOver :: TargetClientAsync (MethodResult AnyResult)
stepOver = callMethod $ Method "Debugger.stepOver" noParam

stepInto :: TargetClientAsync (MethodResult AnyResult)
stepInto = callMethod $ Method "Debugger.stepInto" noParam

stepOut :: TargetClientAsync (MethodResult AnyResult)
stepOut = callMethod $ Method "Debugger.stepOut" noParam

pause :: TargetClientAsync (MethodResult AnyResult)
pause = callMethod $ Method "Debugger.pause" noParam

resume :: TargetClientAsync (MethodResult AnyResult)
resume = callMethod $ Method "Debugger.resume" noParam

-- TODO : Implement the return type
setScriptSource :: SetScriptSourceParams -> TargetClientAsync (MethodResult AnyResult)
setScriptSource = callMethod . Method "Debugger.setScriptSource"

-- TODO : Implement the return type
restartFrame :: RestartFrameParams -> TargetClientAsync (MethodResult AnyResult)
restartFrame = callMethod . Method "Debugger.restartFrame"

getScriptSource :: GetScriptSourceParams -> TargetClientAsync (MethodResult GetScriptSourceResult)
getScriptSource = callMethod . Method "Debugger.getScriptSource"

setPauseOnExceptions :: SetPauseOnExceptionsParams -> TargetClientAsync (MethodResult AnyResult)
setPauseOnExceptions = callMethod . Method "Debugger.setPauseOnExceptions"

evaluateOnCallFrame :: EvaluateOnCallFrameParams -> TargetClientAsync (MethodResult EvaluateOnCallFrameResult)
evaluateOnCallFrame = callMethod . Method "Debugger.evaluateOnCallFrame"

setVariableValue :: SetVariableValueParams -> TargetClientAsync (MethodResult AnyResult)
setVariableValue = callMethod . Method "Debugger.setVariableValue"

setAsyncCallStackDepth :: SetAsyncCallStackDepthParams -> TargetClientAsync (MethodResult AnyResult)
setAsyncCallStackDepth = callMethod . Method "Debugger.setAsyncCallStackDepth"

onScriptParsed :: TargetClientAsync (MethodResult ScriptParsedEvent)
onScriptParsed = listenToEventMethod "Debugger.scriptParsed"

onScriptFailedToParse :: TargetClientAsync (MethodResult ScriptParsedFailedEvent)
onScriptFailedToParse = listenToEventMethod "Debugger.scriptFailedToParsed"

onBreakpointResolved :: TargetClientAsync (MethodResult BreakpointResolvedEvent)
onBreakpointResolved = listenToEventMethod "Debugger.breakpointResolved"

onPaused :: TargetClientAsync (MethodResult PauseEvent)
onPaused = listenToEventMethod "Debugger.pause"

onResumed :: TargetClientAsync (MethodResult AnyResult)
onResumed = listenToEventMethod "Debugger.resumed"
