module Chrome.API.Page where

import Data.Map (Map, empty, insert)

import Chrome.Target.Message
import Chrome.Target.Client

enable :: TargetClient (Maybe NoResult)
enable = callMethod $ Method "Page.enable" noParam

disable :: TargetClient (Maybe NoResult)
disable = callMethod $ Method "Page.disable" noParam

-- TODO : add optional parameters "ignoreCache" and "scriptEvaluatedOnLoad"
reload :: TargetClient (Maybe NoResult)
reload = callMethod $ Method "Page.reload" noParam

navigate :: String -> TargetClient (Maybe NoResult)
navigate url = callMethod $ Method "Page.navigate" (insert "url" url empty)

-- TODO : add optional parameter "promptText"
handleJavaScriptDialog :: Bool -> TargetClient (Maybe NoResult)
handleJavaScriptDialog accept = callMethod $ Method "Page.handleJavaScriptDialog" (insert "accept" accept empty)

-- TODO : onDomContentEventFired

-- TODO : onLoadEventFired

-- TODO : onFrameAttached

-- TODO : onFrameNavigated

-- TODO : onFrameDetached

-- TODO : onJavascriptDialogOpening

-- TODO : onJavascriptDialogClosed

-- TODO : onInterstitialShown

-- TODO : onInterstitialHidden

-- TODO : onNavigationRequested

-- TODO : types
-- - ResourceType
-- - FrameId
-- - Frame
