{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.Blockbuster where

import           StreamFind.Common          (eitherGetWith, unpackResponse,
                                             urlEncode')
import           StreamFind.Types           (Error, Query, Response,
                                             Result (..))

import           Control.Lens               ((&), (.~))
import           Control.Monad              ((>=>))
import           Data.Aeson                 (Value, eitherDecode, withArray,
                                             withObject, (.:))
import           Data.Aeson.Types           (Parser, parseEither)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Vector                (toList)
import qualified Network.Wreq               as WWW

providerName = "Blockbuster"

apiKey = "t2Wf4FZCrYGqany6GvceM85GVrEyqRTNyz4SvGhC"

searchBlockbuster :: Query -> IO Response
searchBlockbuster q = do
  bbData <- blockbusterResp
  return $
    case bbData of
      Left e -> Left $ providerName ++ ":\n" ++ e
      Right resp -> eitherDecode (LBSC.pack resp) >>= suggestions
        where suggestions :: Value -> Either Error [Result]
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
  where
    blockbuster = eitherGetWith opts url
    url =
      "http://bbapi.yousee.tv/rest/movie/typeahead_search?query=" ++
      urlEncode' q ++
      "&excludetvseries=0&exclude100movies=0&excludecmore=1&flavour=blockbuster"
    -- url = "http://localhost:1234/blockbuster.json?" ++ (urlEncode' q)
    opts =
      WWW.defaults & WWW.header "User-Agent" .~ [] & WWW.header "x-api-key" .~
      [apiKey]
    blockbusterResp = unpackResponse <$> blockbuster
