{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.TV2 where

import           StreamFind.Common          (eitherGetWith, unpackResponse,
                                             urlEncode')
import           StreamFind.Types           (Error, Query, Response,
                                             Result (..))

import           Control.Lens               ((&), (.~))
import           Data.Aeson                 (Value, eitherDecode, withArray,
                                             withObject, (.:))
import           Data.Aeson.Types           (Parser, parseEither)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Vector                (toList)
import qualified Network.Wreq               as WWW

providerName = "TV2"

searchTV2 :: Query -> IO Response
searchTV2 q = do
  tv2Data <- tv2Resp
  return $
    case tv2Data of
      Left e -> Left $ providerName ++ ":\n" ++ e
      Right resp -> eitherDecode (LBSC.pack resp) >>= hits
        where hits :: Value -> Either Error [Result]
              hits =
                parseEither $
                withArray "suggestions" $ \s -> mapM hit (toList s)
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
  where
    tv2 = eitherGetWith opts url
    url =
      "https://r7.tv2.dk/api/3/search/autocomplete/ApplicationCode-Play_Web/query-" ++
      urlEncode' q ++ ".json"
    -- url = "http://localhost:1234/tv2.json?" ++ urlEncode' q
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
    tv2Resp = unpackResponse <$> tv2
