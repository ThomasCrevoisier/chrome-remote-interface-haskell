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

enable :: TargetClientAsync (MethodResult AnyResult)
enable = callMethod $ Method "Page.enable" noParam

disable :: TargetClientAsync (MethodResult AnyResult)
disable = callMethod $ Method "Page.disable" noParam

reload :: PageReloadParams -> TargetClientAsync (MethodResult AnyResult)
reload = callMethod . Method "Page.reload"

navigate :: String -> TargetClientAsync (MethodResult PageNavigateResult)
navigate url = callMethod $ Method "Page.navigate" (insert "url" url empty)

handleJavaScriptDialog :: PageHandleDialogParams -> TargetClientAsync (MethodResult AnyResult)
handleJavaScriptDialog = callMethod . Method "Page.handleJavaScriptDialog"

captureScreenshot :: CaptureScreenshotParams -> TargetClientAsync (MethodResult CaptureScreenshotResult)
captureScreenshot = callMethod . Method "Page.captureScreenshot"

onDomContentEventFired :: TargetClientAsync (MethodResult TimestampEvent)
onDomContentEventFired = listenToEventMethod "Page.domContentEventFired"

onLoadEventFired :: TargetClientAsync (MethodResult TimestampEvent)
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
