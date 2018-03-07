{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.Viaplay where

import           StreamFind.Common          (eitherGetWith, unpackResponse,
                                             urlEncode')
import           StreamFind.Types           (Error, Query, Response,
                                             Result (..))

import           Control.Applicative        (optional, (<|>))
import           Control.Lens               ((&), (.~))
import           Control.Monad              ((>=>))
import           Data.Aeson                 (Value, eitherDecode, withArray,
                                             withObject, (.:), (.:?))
import           Data.Aeson.Types           (Parser, parseEither)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Text                  (replace, unpack)
import           Data.Vector                (toList)
import qualified Network.Wreq               as WWW
import           Text.Read                  (readMaybe)

providerName = "Viaplay"

searchViaplay :: Query -> IO Response
searchViaplay q = do
  vpData <- vpResp
  return $
    case vpData of
      Left e -> Left $ providerName ++ ":\n" ++ e
      Right resp -> eitherDecode (LBSC.pack resp) >>= hits
        where hits :: Value -> Either Error [Result]
              hits =
                parseEither $
                withObject "root" (.: "_embedded") >=>
                withObject "blocks" (.: "viaplay:blocks") >=>
                withArray "blocks" (searchType . head . toList)
              searchType :: Value -> Parser [Result]
              searchType =
                withObject "block" (.: "_embedded") >=>
                withObject "products" (.: "viaplay:products") >=>
                withArray "products" (mapM hit . toList)
              hit :: Value -> Parser Result
              hit =
                withObject "hit" $ \o -> do
                  content <- o .: "content"
                  title <-
                    content .: "title" <|>
                    (content .: "series" >>= (.: "title"))
                  synopsis <-
                    content .: "synopsis" <|>
                    (content .: "series" >>= (.: "synopsis"))
                  url <- o .: "_links" >>= (.: "viaplay:page") >>= (.: "href")
                  price <-
                    optional
                      (o .: "system" >>= (.: "availability") >>= (.: "planInfo") >>=
                       (.: "price"))
                  return $
                    Result
                      title
                      (Just synopsis)
                      (Just . unpack $
                       replace "content.viaplay.dk/pcdash-dk" "viaplay.dk" url)
                      (price >>= readMaybe)
                      True
                      providerName
  where
    vp = eitherGetWith opts url
    url = "https://content.viaplay.dk/pcdash-dk/search?query=" ++ urlEncode' q
    -- url = "http://localhost:1234/viaplay.json?" ++ urlEncode' q
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
    vpResp = unpackResponse <$> vp
