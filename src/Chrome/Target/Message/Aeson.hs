{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Chrome.Target.Message.Aeson
  ( FromJSONMessage(..)
  , ToJSONMessage(..)
  , messageJSONOpts
  ) where

import           Data.Aeson
import           GHC.Generics (Generic, Rep)

newtype FromJSONMessage a
  = FromJSONMessage a
  deriving Generic

newtype ToJSONMessage a
  = ToJSONMessage a
  deriving Generic

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (FromJSONMessage a) where
  parseJSON = fmap FromJSONMessage . genericParseJSON messageJSONOpts

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (ToJSONMessage a) where
  toJSON (ToJSONMessage v) = genericToJSON messageJSONOpts v

messageJSONOpts :: Options
messageJSONOpts = defaultOptions { omitNothingFields = True
                                 , fieldLabelModifier = escapeKeywords
                                 }

escapeKeywords :: String -> String
escapeKeywords "_type" = "type"
escapeKeywords "_data" = "data"
escapeKeywords str     = str
