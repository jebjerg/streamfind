{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.SFAnytime where

import           StreamFind.Common     (eitherPostWith, prefixError,
                                        responseBody', urlEncode')
import           StreamFind.Types      (Query, Response, Result (..))

import           Control.Lens          ((&), (.~))
import           Control.Monad         ((>=>))
import           Data.Aeson            (Value, eitherDecode, withArray,
                                        withObject, (.:))
import           Data.Aeson.Types      (Parser, parseEither)
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Lazy  (ByteString)
import           Data.Char             (toLower)
import           Data.Vector           (toList)
import qualified Network.Wreq          as WWW
import           Text.Regex            (mkRegex, subRegex)

providerName = "SF"

decodeSF :: ByteString -> Response
decodeSF resp = eitherDecode resp >>= parseEither results
  where
    results :: Value -> Parser [Result]
    results =
      withObject "results" (.: "results") >=>
      withArray "results[]" (fmap concat . mapM hits . toList)
    hits :: Value -> Parser [Result]
    hits =
      withObject "hits" (.: "hits") >=> withArray "hits[]" (mapM hit . toList)
    hit :: Value -> Parser Result
    hit =
      withObject "hit" $ \o -> do
        title <- o .: "title"
        kind <- o .: "producttype"
        mediaId <- o .: "mediaid"
        price <- o .: "price_da"
        summary <- o .: "short_summary_da"
        return $
          Result
            title
            (Just summary)
            (Just $ sfLink kind mediaId title)
            (Just price)
            True
            providerName

sfLink :: String -> Integer -> String -> String
sfLink kind mediaId title =
  "https://www.sfanytime.com/da/" ++
  kind ++ "/" ++ show mediaId ++ "/" ++ slug title
  where
    slug :: String -> String
    slug =
      flip (subRegex (mkRegex "[^a-z0-9_-]")) "" .
      flip (subRegex (mkRegex "[ ]")) "-" . map toLower

-- NOTE: lots of goodies. inspect with attributesToRetrieve=*. see https://www.algolia.com/doc/api-reference/
searchData q n =
  BSC.pack $
  "{\"requests\":[{\"indexName\":\"prod_sfanytime_movies\",\"params\":\"query=" ++
  urlEncode' q ++
  "&numericFilters=adult%3D0%2C%20available_in_dk%3D1&hitsPerPage=" ++
  show n ++
  "&maxValuesPerFacet=3&page=0&attributesToRetrieve=mediaid%2Cproducttype%2Cproducttypeid%2Ctitle%2Ctitle_sv%2Ctitle_no%2Ctitle_da%2Ctitle_fi%2Ccover_id%2Ccover_no%2Ccover_sv%2Ccover_da%2Ccover_fi%2Cprice_da%2Cshort_summary_da&distinct=true&facets=%5B%5D&tagFilters=\"}]}"

searchSF :: Query -> IO Response
searchSF q = do
  sfData <- response
  return . prefixError (providerName ++ ":\n") $
    responseBody' <$> sfData >>= decodeSF
  where
    response = eitherPostWith opts url (searchData q 3)
    url =
      "https://e38fd90mob-dsn.algolia.net/1/indexes/*/queries?x-algolia-agent=Algolia%20for%20vanilla%20JavaScript%20(lite)%203.21.1%3Binstantsearch.js%201.11.15%3BJS%20Helper%202.19.0&x-algolia-application-id=E38FD90MOB&x-algolia-api-key=3f56a452156f1a76c8939af1798a2335"
    -- url = "http://localhost:1234/sf.json"
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
