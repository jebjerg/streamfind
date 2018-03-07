module StreamFind.Common where

import           StreamFind.Types           (EitherWWWResponse, Error, Result,
                                             WWWResponse, price, provider,
                                             title, url)

import           Control.Exception          (catch)
import           Control.Exception.Base     (SomeException)
import           Control.Lens               ((&), (^.))
import qualified Network.Wreq               as WWW

import           Data.Aeson                 (FromJSON, Object, parseJSON)
import           Data.Aeson.Types           (Parser)
import           Data.ByteString.Char8      (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.HashMap.Lazy          as HM
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Network.HTTP.Types.URI     (urlEncode)

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

unpackResponse :: EitherWWWResponse -> Either Error String
unpackResponse (Left err) = Left err
unpackResponse (Right response) = Right $ LBSC.unpack . responseBody' $ response

prefixError :: String -> Either String a -> Either String a
prefixError tag (Left err)  = Left $ tag ++ err
prefixError tag (Right res) = Right res

urlEncode' :: String -> String
urlEncode' s = unpack $ urlEncode False $ pack s

fmt :: Result -> String
fmt r =
  provider r ++
  ": " ++
  title r ++
  (case price r of
     Nothing -> ""
     Just p  -> " $$$ " ++ show p) ++
  (case url r of
     Nothing -> ""
     Just u  -> " @ " ++ u)
