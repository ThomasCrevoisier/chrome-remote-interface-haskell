module Chrome.API.Emulation where

import           Chrome.Target.Client
import           Chrome.Target.Message

import           Chrome.API.Emulation.Types

setDeviceMetricsOverride :: SetMetricsOverrideParams -> TargetClientAsync (MethodResult AnyResult)
setDeviceMetricsOverride = callMethod . Method "Emulation.setDeviceMetricsOverride"

clearDeviceMetricsOverride :: TargetClientAsync (MethodResult AnyResult)
clearDeviceMetricsOverride = callMethod $ Method "Emulation.clearDeviceMetricsOverride" noParam

setTouchEmulationEnabled :: TouchEmulationEnabledParams -> TargetClientAsync (MethodResult AnyResult)
setTouchEmulationEnabled = callMethod . Method "Emulation.setTouchEmulationEnabled"

setEmulatedMedia :: EmulatedMediaParams -> TargetClientAsync (MethodResult AnyResult)
setEmulatedMedia = callMethod . Method "Emulation.setEmulatedMedia"
