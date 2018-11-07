module Chrome.API.Page (
      module Chrome.API.Page.Types
    , enable
    , disable
    , reload
    , navigate
    , handleJavaScriptDialog
    , captureScreenshot
    , onDomContentEventFired
    , onLoadEventFired
    , onFrameAttached
    , onFrameNavigated
    , onFrameDetached
    , onJavascriptDialogOpening
    , onJavascriptDialogClosed
    , onInterstitialShown
    , onInterstitialHidden
    , onNavigationRequested
    ) where

import Data.Map (empty, insert)

import Chrome.Target.Message
import Chrome.Target.Client

import Chrome.API.Page.Types

enable :: TargetClientAsync (MethodResult AnyResult)
enable = callMethod $ Method "Page.enable" noParam

disable :: TargetClientAsync (MethodResult AnyResult)
disable = callMethod $ Method "Page.disable" noParam

reload :: PageReloadParams -> TargetClientAsync (MethodResult AnyResult)
reload = callMethod . Method "Page.reload"

navigate :: String -> TargetClientAsync (MethodResult FrameId)
navigate url' = callMethod $ Method "Page.navigate" (insert "url" url' empty)

handleJavaScriptDialog :: PageHandleDialogParams -> TargetClientAsync (MethodResult AnyResult)
handleJavaScriptDialog = callMethod . Method "Page.handleJavaScriptDialog"

captureScreenshot :: CaptureScreenshotParams -> TargetClientAsync (MethodResult CaptureScreenshotResult)
captureScreenshot = callMethod . Method "Page.captureScreenshot"

onDomContentEventFired :: TargetClientAsync (MethodResult TimestampEvent)
onDomContentEventFired = listenToEventMethod "Page.domContentEventFired"

onLoadEventFired :: TargetClientAsync (MethodResult TimestampEvent)
onLoadEventFired = listenToEventMethod "Page.loadEventFired"

onFrameAttached :: TargetClientAsync (MethodResult FrameAttachedEvent)
onFrameAttached = listenToEventMethod "Page.frameAttached"

onFrameNavigated :: TargetClientAsync (MethodResult Frame)
onFrameNavigated = listenToEventMethod "Page.frameNavigated"

onFrameDetached :: TargetClientAsync (MethodResult FrameId)
onFrameDetached = listenToEventMethod "Page.frameDetached"

onJavascriptDialogOpening :: TargetClientAsync (MethodResult DialogOpeningEvent)
onJavascriptDialogOpening = listenToEventMethod "Page.javascriptDialogOpening"

onJavascriptDialogClosed :: TargetClientAsync (MethodResult DialogClosingEvent)
onJavascriptDialogClosed = listenToEventMethod "Page.javascriptDialogClosed"

onInterstitialShown :: TargetClientAsync (MethodResult AnyResult)
onInterstitialShown = listenToEventMethod "Page.interstitialShown"

onInterstitialHidden :: TargetClientAsync (MethodResult AnyResult)
onInterstitialHidden = listenToEventMethod "Page.interstitialHidden"

onNavigationRequested :: TargetClientAsync (MethodResult NavigationRequestEvent)
onNavigationRequested = listenToEventMethod "Page.navigationRequested"
