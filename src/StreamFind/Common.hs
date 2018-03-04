module StreamFind.Common where

import           StreamFind.Types       (EitherWWWResponse, Result, WWWResponse,
                                         provider, title, url)

import           Control.Exception      (catch)
import           Control.Exception.Base (SomeException)
import           Control.Lens           ((&), (^.))
import qualified Network.Wreq           as WWW

import           Data.Aeson             (FromJSON, Object, parseJSON)
import           Data.Aeson.Types       (Parser)
import qualified Data.HashMap.Lazy      as HM
import           Data.Monoid            ((<>))
import           Data.Text              as T

eitherGetWith :: WWW.Options -> String -> IO EitherWWWResponse
eitherGetWith = eitherWith WWW.getWith

eitherWith ::
     (WWW.Options -> String -> IO WWWResponse)
  -> WWW.Options
  -> String
  -> IO EitherWWWResponse
eitherWith fn opts url = catch (Right <$> fn opts url) handler
  where
    handler :: SomeException -> IO EitherWWWResponse
    handler e = return $ Left $ show e

responseBody' :: WWW.Response a -> a
responseBody' x = x ^. WWW.responseBody

prefixError :: String -> Either String a -> Either String a
prefixError tag (Left err)  = Left $ tag ++ err
prefixError tag (Right res) = Right res

fmt :: Result -> String
fmt r =
  provider r ++
  ": " ++
  title r ++
  case url r of
    Nothing -> ""
    Just u  -> " @ " ++ u
