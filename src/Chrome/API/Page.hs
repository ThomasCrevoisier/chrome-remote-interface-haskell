module Chrome.API.Page where

import Data.Map (Map, empty, insert)
import Data.Aeson (Value)

import Chrome.Target.Message
import Chrome.Target.Client

enable :: TargetClient (Maybe Value)
enable = sendCmd' $ Method "Page.enable" (empty :: Map String String)

disable :: TargetClient (Maybe Value)
disable = sendCmd' $ Method "Page.disable" (empty :: Map String String)

-- TODO : add optional parameters "ignoreCache" and "scriptEvaluatedOnLoad"
reload :: TargetClient (Maybe Value)
reload = sendCmd' $ Method "Page.reload" (empty :: Map String String)

navigate :: String -> TargetClient (Maybe Value)
navigate url = sendCmd' $ Method "Page.navigate" (insert "url" url empty)

-- TODO : add optional parameter "promptText"
handleJavaScriptDialog :: Bool -> TargetClient (Maybe Value)
handleJavaScriptDialog accept = sendCmd' $ Method "Page.handleJavaScriptDialog" (insert "accept" accept empty)

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
