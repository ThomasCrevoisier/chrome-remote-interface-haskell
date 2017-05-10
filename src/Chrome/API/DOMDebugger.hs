module Chrome.API.DOMDebugger where

import Chrome.Target.Message
import Chrome.Target.Client

import Chrome.API.DOMDebugger.Types

setDOMBreakpoint :: DOMBreakpointParams -> TargetClientAsync (MethodResult AnyResult)
setDOMBreakpoint = callMethod . Method "DOMDebugger.setDOMBreakpoint"

removeDOMBreakpoint :: DOMBreakpointParams -> TargetClientAsync (MethodResult AnyResult)
removeDOMBreakpoint = callMethod . Method "DOMDebugger.removeDOMBreakpoint"

setEventListenerBreakpoint :: EventBreakpointParams -> TargetClientAsync (MethodResult AnyResult)
setEventListenerBreakpoint = callMethod . Method "DOMDebugger.setEventListenerBreakpoint"

removeEventListenerBreakpoint :: EventBreakpointParams -> TargetClientAsync (MethodResult AnyResult)
removeEventListenerBreakpoint = callMethod . Method "DOMDebugger.removeEventListenerBreakpoint"

setXHRBreakpoint :: XHRBreakpointParams -> TargetClientAsync (MethodResult AnyResult)
setXHRBreakpoint = callMethod . Method "DOMDebugger.setXHRBreakpoint"

removeXHRBreakpoint :: XHRBreakpointParams -> TargetClientAsync (MethodResult AnyResult)
removeXHRBreakpoint = callMethod . Method "DOMDebugger.removeXHRBreakpoint"
