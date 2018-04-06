{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.Blockbuster where

import           StreamFind.Common    (eitherGetWith, prefixError,
                                       responseBody', urlEncode')
import           StreamFind.Types     (Query, Response, Result (..))

import           Control.Lens         ((&), (.~))
import           Control.Monad        ((>=>))
import           Data.Aeson           (Value, eitherDecode, withArray,
                                       withObject, (.:))
import           Data.Aeson.Types     (Parser, parseEither)
import           Data.ByteString.Lazy (ByteString)
import           Data.Vector          (toList)
import qualified Network.Wreq         as WWW

providerName = "Blockbuster"

apiKey = "t2Wf4FZCrYGqany6GvceM85GVrEyqRTNyz4SvGhC"

decodeBlockbuster :: ByteString -> Response
decodeBlockbuster resp = eitherDecode resp >>= suggestions
  where
    suggestions :: Value -> Response
    suggestions =
      parseEither $
      withObject "root" (.: "suggestions") >=>
      withArray "suggestions" (mapM suggestion . toList)
    suggestion :: Value -> Parser Result
    suggestion =
      withObject "suggestion" $ \o -> do
        title <- o .: "title"
        slug <- o .: "url_id"
        return $
          Result
            title
            Nothing
            (Just $ "http://blockbuster.dk/" ++ slug)
            (Just 49)
            True
            providerName

searchBlockbuster :: Query -> IO Response
searchBlockbuster q = do
  bbData <- response
  return . prefixError (providerName ++ ":\n") $
    responseBody' <$> bbData >>= decodeBlockbuster
  where
    response = eitherGetWith opts url
    url =
      "http://bbapi.yousee.tv/rest/movie/typeahead_search?query=" ++
      urlEncode' q ++
      "&excludetvseries=0&exclude100movies=0&excludecmore=1&flavour=blockbuster"
    -- url = "http://localhost:1234/blockbuster.json?" ++ (urlEncode' q)
    opts =
      WWW.defaults & WWW.header "User-Agent" .~ [] & WWW.header "x-api-key" .~
      [apiKey]
