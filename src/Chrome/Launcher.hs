{-# LANGUAGE RecordWildCards #-}

{-|
Module: Chrome.Launcher
Description: Helpers allowing to launch Chrome programmatically
|-}

module Chrome.Launcher
  ( ChromeConfig(..)
  , defaultConfig
  , withChrome
  ) where

import           Data.Default         (Default (..))
import           System.Process.Typed (proc, stopProcess, withProcess)

-- | Spawn Google Chrome using the provided config, execute an action and close the browser once the action is done
--
-- Example:
--
-- >>> withChrome defaultConfig (\_ -> putStrLn "Chrome started !")
withChrome :: ChromeConfig -> (ChromeConfig -> IO a) -> IO a
withChrome cfg action = withProcess (mkChromeProcess cfg) run
  where
    mkChromeProcess ChromeConfig{..} = proc chromeExecutablePath [ "--remote-debugging-port=" <> show chromeRemoteDebuggingPort ]

    run p = action cfg >>= (<$ stopProcess p)

data ChromeConfig
  = ChromeConfig { chromeExecutablePath      :: String
                  -- ^ Path to the Google Chrome executable
                 , chromeRemoteDebuggingPort :: Int
                  -- ^ Port for using the remote debugging protocol
                 }

instance Default ChromeConfig where
  def = defaultConfig

-- | Default configuration for launching "google-chrome" with the remote debugging port set to 9222
defaultConfig :: ChromeConfig
defaultConfig = ChromeConfig { chromeExecutablePath = "google-chrome"
                             , chromeRemoteDebuggingPort = 9222
                             }
