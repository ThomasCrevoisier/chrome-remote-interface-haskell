module Chrome.API.Page where

import Data.Map (Map, empty, insert)

import Chrome.DebuggingMessage

enable :: Command (Map String String)
enable = Command "Page.enable" empty

disable :: Command (Map String String)
disable = Command "Page.disable" empty

navigate :: String -> Command (Map String String)
navigate url = Command "Page.navigate" (insert "url" url empty)
