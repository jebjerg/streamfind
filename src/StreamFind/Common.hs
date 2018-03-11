module StreamFind.Common where

import           StreamFind.Types           (EitherWWWResponse, Error, Result,
                                             WWWResponse, price, provider,
                                             title, url)

import           Control.Exception          (catch)
import           Control.Exception.Base     (SomeException)
import           Control.Lens               ((&), (^.))
import qualified Network.Wreq               as WWW
import           Network.Wreq.Types         (Postable)

import           Data.Aeson                 (FromJSON, Object, parseJSON)
import           Data.Aeson.Types           (Parser)
import           Data.ByteString.Char8      (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.HashMap.Lazy          as HM
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Network.HTTP.Types.URI     (urlEncode)

exceptionHandler :: SomeException -> IO EitherWWWResponse
exceptionHandler e = return $ Left $ show e

eitherGetWith :: WWW.Options -> String -> IO EitherWWWResponse
eitherGetWith opts url = catch (Right <$> WWW.getWith opts url) exceptionHandler

eitherPostWith ::
     Postable a => WWW.Options -> String -> a -> IO EitherWWWResponse
eitherPostWith opts url p =
  catch (Right <$> WWW.postWith opts url p) exceptionHandler

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
