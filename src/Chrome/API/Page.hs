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

enable :: TargetClientAsync (MethodResult ())
enable = callMethod $ Method "Page.enable" noParam

disable :: TargetClientAsync (MethodResult ())
disable = callMethod $ Method "Page.disable" noParam

reload :: PageReloadParams -> TargetClientAsync (MethodResult ())
reload = callMethod . Method "Page.reload"

navigate :: String -> TargetClientAsync (MethodResult ())
navigate url = callMethod $ Method "Page.navigate" (insert "url" url empty)

handleJavaScriptDialog :: PageHandleDialogParams -> TargetClientAsync (MethodResult ())
handleJavaScriptDialog = callMethod . Method "Page.handleJavaScriptDialog"

captureScreenshot :: CaptureScreenshotParams -> TargetClientAsync (MethodResult CaptureScreenshotResult)
captureScreenshot = callMethod . Method "Page.captureScreenshot"

onLoadEventFired :: TargetClientAsync (MethodResult ())
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
