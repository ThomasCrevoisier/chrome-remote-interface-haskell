module Chrome.Target.Message.TH where

import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax (Dec, Name, Q)

escapeKeywords :: String -> String
escapeKeywords "type" = "_type"
escapeKeywords "data" = "_data"
escapeKeywords str    = str

deriveJSONMsg :: Name -> Q [Dec]
deriveJSONMsg = deriveJSON (defaultOptions { omitNothingFields = True, fieldLabelModifier =  escapeKeywords })
