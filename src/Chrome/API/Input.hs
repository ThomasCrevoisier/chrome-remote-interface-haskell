module Chrome.API.Input where

import           Chrome.Target.Client
import           Chrome.Target.Message

import           Chrome.API.Input.Types

dispatchKeyEvent :: KeyEvent -> TargetClientAsync (MethodResult AnyResult)
dispatchKeyEvent = callMethod . Method "Input.dispatchKeyEvent"

dispatchMouseEvent :: MouseEvent -> TargetClientAsync (MethodResult AnyResult)
dispatchMouseEvent = callMethod . Method "Input.dispatchMouseEvent"
