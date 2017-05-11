module Chrome.Target.Message.TH where

import Data.Aeson.TH

escapeKeywords :: String -> String
escapeKeywords "type" = "_type"
escapeKeywords "data" = "_data"
escapeKeywords str = str

deriveJSONMsg = deriveJSON (defaultOptions { omitNothingFields = True, fieldLabelModifier =  escapeKeywords })
