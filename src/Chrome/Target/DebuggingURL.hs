module Chrome.Target.DebuggingURL where

import           Data.Aeson
import           Data.Text   (unpack)
import           Network.URL

newtype DebuggingURL
  = DebuggingURL URL
  deriving Show

instance FromJSON DebuggingURL where
  -- TODO : use `maybe`
  parseJSON = withText "url" $ \t -> case importURL (unpack t) of
                                       Just url -> return $ DebuggingURL url
                                       Nothing  -> mempty

debuggingURLHost :: DebuggingURL -> Maybe Host
debuggingURLHost (DebuggingURL (URL urlType _ _)) = case urlType of
                                                      Absolute h -> Just h
                                                      _          -> Nothing

debuggingURLPath :: DebuggingURL -> String
debuggingURLPath (DebuggingURL url) = '/' : url_path url
