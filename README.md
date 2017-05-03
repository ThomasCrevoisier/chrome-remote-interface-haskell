# chrome-remote-interface-haskell

A client to interact with Chrome remotely.

## Motivation

The mainstream option to do that is [chrome-remote-interface](https://github.com/cyrus-and/chrome-remote-interface) which is a NodeJS lib.

I struggle a bit to find informations about what I would get from methods. Also, as a matter of personal preference, I find the Promise syntax a bit verbose.

So why not use Haskell.

## Installation

Publication on Hackage is coming soon.

## Usage

Before running it, launch Chrome with the remote debugging enabled : `chrome --remote-debugging-port=9222`

Notes :
- if this doesn't work, try with Chrome Canary
- this absolutely works with running Chrome in headless mode :)

Here is an example of how to use the library :

```
import Chrome.Target
import Chrome.Target.Client
import Chrome.Target.Async (waitFor, onEvent, stopEventListener)

import qualified Chrome.API.Page as Page
import qualified Chrome.API.DOM as DOM
import qualified Chrome.API.Network as Network

head' :: [a] -> Maybe a
head' (x:_) = Just x
head' _ = Nothing

sampleCommands :: TargetClient ()
sampleCommands = do
  traverse_ waitFor [Page.enable, Network.enable Network.defaultEnableParams]

  listener <- onEvent Network.onRequestWillBeSent (liftIO . printRequest)

  waitFor $ Page.navigate "http://gitlab.com"
  waitFor $ Page.onLoadEventFired

  stopEventListener listener

  traverse_ waitFor [Page.disable, Network.disable]

  where
    printRequest (Right event) = print event
    printRequest (Left err) = print err

main :: IO ()
main = do
  pages <- fetchTargets "http://localhost:9222"
  let firstPage = head' =<< pages
  case firstPage of
    Nothing -> putStrLn "No page found"
    Just p -> withTarget p sampleCommands
```

A bit of explainations :
- First we get all debuggable targets using `fetchTargets`
- Then, we can execute a bunch of commands using `withTarget`

Some notes :
- All method calls are executed asynchronously, thus to make a blocking call we use the `waitFor` helper
- We can execute a callback on certains event using `onEvent`, which returns a reference to the listener used when we want to stop listening using `stopEventListener`

## Basic Roadmap

The goal for the first version is to support the stable version of the protocol : https://chromedevtools.github.io/devtools-protocol/1-2/ 

It includes the domains :
- [ ] DOM
- [ ] DOMDebugger
- [ ] Debugger
- [ ] Emulation
- [ ] Input
- [x] Network
- [x] Page
- [ ] Profiler
- [ ] Runtime
- [ ] Schema
