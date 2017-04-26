module Chrome.API.Page where

import Data.Map (Map, empty, insert)

import Chrome.Target.Message

enable :: Method (Map String String)
enable = Method "Page.enable" empty

disable :: Method (Map String String)
disable = Method "Page.disable" empty

navigate :: String -> Method (Map String String)
navigate url = Method "Page.navigate" (insert "url" url empty)
