{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.TV2 where

import           StreamFind.Common    (eitherGetWith, prefixError,
                                       responseBody', urlEncode')
import           StreamFind.Types     (Query, Response, Result (..))

import           Control.Lens         ((&), (.~))
import           Data.Aeson           (Value, eitherDecode, withArray,
                                       withObject, (.:))
import           Data.Aeson.Types     (Parser, parseEither)
import           Data.ByteString.Lazy (ByteString)
import           Data.Vector          (toList)
import qualified Network.Wreq         as WWW

providerName = "TV2"

decodeTV2 :: ByteString -> Response
decodeTV2 resp = eitherDecode resp >>= hits
  where
    hits :: Value -> Response
    hits = parseEither $ withArray "suggestions" $ \s -> mapM hit (toList s)
    hit :: Value -> Parser Result
    hit =
      withObject "hit" $ \o -> do
        title <- o .: "label"
        slug <- o .: "url"
        return $
          Result
            title
            Nothing
            (Just $ "https://play.tv2.dk" ++ slug)
            Nothing
            True
            providerName

searchTV2 :: Query -> IO Response
searchTV2 q = do
  tv2Data <- response
  return . prefixError (providerName ++ ":\n") $
    responseBody' <$> tv2Data >>= decodeTV2
  where
    response = eitherGetWith opts url
    url =
      "https://r7.tv2.dk/api/3/search/autocomplete/ApplicationCode-Play_Web/query-" ++
      urlEncode' q ++ ".json"
    -- url = "http://localhost:1234/tv2.json?" ++ urlEncode' q
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
