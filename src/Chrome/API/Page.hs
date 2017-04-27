module Chrome.API.Page (
      module Chrome.API.Page.Types
    , enable
    , disable
    , reload
    , navigate
    , handleJavaScriptDialog
    , captureScreenshot
    , onLoadEventFired
    ) where

import Data.Map (Map, empty, insert)

import Chrome.Target.Message
import Chrome.Target.Client

import Chrome.API.Page.Types

enable :: TargetClientAsync (Maybe NoResult)
enable = callMethod $ Method "Page.enable" noParam

disable :: TargetClientAsync (Maybe NoResult)
disable = callMethod $ Method "Page.disable" noParam

-- TODO : add optional parameters "ignoreCache" and "scriptEvaluatedOnLoad"
reload :: TargetClientAsync (Maybe NoResult)
reload = callMethod $ Method "Page.reload" noParam

navigate :: String -> TargetClientAsync (Maybe NoResult)
navigate url = callMethod $ Method "Page.navigate" (insert "url" url empty)

-- TODO : add optional parameter "promptText"
handleJavaScriptDialog :: Bool -> TargetClientAsync (Maybe NoResult)
handleJavaScriptDialog accept = callMethod $ Method "Page.handleJavaScriptDialog" (insert "accept" accept empty)

captureScreenshot :: CaptureScreenshotParams -> TargetClientAsync (Maybe CaptureScreenshotResult)
captureScreenshot params = callMethod $ Method "Page.captureScreenshot" params

onLoadEventFired :: TargetClientAsync (Maybe NoResult)
onLoadEventFired = listenToEventMethod "Page.loadEventFired"

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
