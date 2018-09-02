{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.CMore where

import           StreamFind.Common    (eitherGetWith, prefixError,
                                       responseBody', urlEncode')
import           StreamFind.Types     (Query, Response, Result (..))

import           Control.Applicative  ((<|>))
import           Control.Lens         ((&), (.~))
import           Control.Monad        ((>=>))
import           Data.Aeson           (Value, eitherDecode, withArray,
                                       withObject, (.:))
import           Data.Aeson.Types     (Parser, parseEither)
import           Data.ByteString.Lazy (ByteString)
import           Data.Vector          (toList)
import qualified Network.Wreq         as WWW

providerName = "CMore"

decodeCMore :: ByteString -> Response
decodeCMore resp = eitherDecode resp >>= assets
  where
    assets :: Value -> Response
    assets =
      parseEither $
      withObject "root" (.: "assets") >=>
      withArray "suggestions" (mapM asset . toList)
    asset :: Value -> Parser Result
    asset =
      withObject "suggestion" $ \o -> do
        title <- (o .: "original_title" >>= (.: "text")) <|> o .: "title_da"
        desc <- o .: "description_short_da"
        videoId <- o .: "video_id" <|> o .: "id"
        return $
          Result
            title
            (Just desc)
            (Just $ "https://cmore.dk/film/" ++ videoId)
            Nothing
            True
            providerName

searchCMore :: Query -> IO Response
searchCMore q = do
  cmData <- response
  return . prefixError (providerName ++ ":\n") $
    responseBody' <$> cmData >>= decodeCMore
  where
    response = eitherGetWith opts url
    url =
      "https://cmore-search.b17g.services/search?client=cmore-web&site=cmore.dk&lang=da&type=movie,series,sport&page_size=8&sort_by=relevance&q=" ++
      urlEncode' q
    -- url = "http://localhost:1234/cmore.json?" ++ urlEncode' q
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
