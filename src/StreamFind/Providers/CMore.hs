{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.CMore where

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

providerName = "CMore"

searchCMore :: Query -> IO Response
searchCMore q = do
  cmData <- cmoreResp
  return $
    case cmData of
      Left e -> Left $ providerName ++ ":\n" ++ e
      Right resp -> eitherDecode (LBSC.pack resp) >>= assets
        where assets :: Value -> Either Error [Result]
              assets =
                parseEither $
                withObject "root" (.: "assets") >=>
                withArray "suggestions" (mapM asset . toList)
              asset :: Value -> Parser Result
              asset =
                withObject "suggestion" $ \o -> do
                  title <- o .: "original_title" >>= (.: "text")
                  desc <- o .: "description_short_da"
                  videoId <- o .: "video_id"
                  return $
                    Result
                      title
                      (Just desc)
                      (Just $ "https://cmore.dk/film/" ++ videoId)
                      Nothing
                      True
                      providerName
  where
    cmore = eitherGetWith opts url
    url =
      "https://cmore-search.b17g.services/search?client=cmore-web&site=cmore.dk&lang=da&type=movie,series,sport&page_size=8&sort_by=relevance&q=" ++
      urlEncode' q
    -- url = "http://localhost:1234/cmore.json?" ++ urlEncode' q
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
    cmoreResp = unpackResponse <$> cmore
