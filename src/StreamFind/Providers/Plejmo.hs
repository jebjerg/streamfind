{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.Plejmo where

import           StreamFind.Common    (eitherGetWith, prefixError,
                                       responseBody', urlEncode')
import           StreamFind.Types     (Query, Response, Result (..))

import           Control.Lens         ((&), (.~))
import           Control.Monad        ((>=>))
import           Data.Aeson           (Value, eitherDecode, withArray,
                                       withObject, (.:))
import           Data.Aeson.Types     (Parser, parseEither)
import           Data.ByteString.Lazy (ByteString)
import           Data.Char            (isDigit)
import           Data.Vector          (toList)
import qualified Network.Wreq         as WWW

providerName = "Plejmo"

decodePlejmo :: ByteString -> Response
decodePlejmo resp = eitherDecode resp >>= parseEither items
  where
    items :: Value -> Parser [Result]
    items =
      withObject "items" (.: "items") >=>
      withArray "items[]" (mapM item . toList)
    item :: Value -> Parser Result
    item =
      withObject "item" $ \o -> do
        title <- o .: "title"
        url <- o .: "url"
        price <- parsePrice <$> o .: "vodprice"
        vodAvailable <- o .: "isVodAvailable"
        return $
          Result
            title
            Nothing
            (Just $ "https://www.plejmo.com" ++ url)
            (Just price)
            vodAvailable
            providerName
    parsePrice :: String -> Int
    parsePrice = read . takeWhile isDigit

searchPlejmo :: Query -> IO Response
searchPlejmo q = do
  pData <- response
  return . prefixError (providerName ++ ":\n") $
    responseBody' <$> pData >>= decodePlejmo
  where
    response = eitherGetWith opts url
    url =
      "https://www.plejmo.com/api/search/get?srch=" ++
      urlEncode' q ++ "&sort=PublishDate&order=Desc&start=0&max=5&lang=da"
    -- url = "http://localhost:1234/plejmo.json?" ++ urlEncode' q
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
