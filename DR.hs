{-# LANGUAGE OverloadedStrings #-}

module DR where

import           Common               (eitherGetWith, extractKey, prefixError,
                                       responseBody')
import           Control.Lens         ((&), (.~))
import           Data.Aeson           ((.:), FromJSON, eitherDecode, parseJSON,
                                       withObject, withArray, Value)
import           Data.Aeson.Types     (parseEither)
import           Data.Vector          (toList)
import           Data.ByteString.Lazy (ByteString)
import           Data.List            (map)
import qualified Network.Wreq         as WWW
import           Types                (EitherWWWResponse, Error, Query,
                                       Response, Result (..), ToResult,
                                       toResult)

data DREpisode = DREpisode
  { programcardTitle      :: String
  , programcardSlug       :: String
  , hasPublicPrimaryAsset :: Bool
  } deriving (Show)

instance ToResult DREpisode where
  toResult ep =
    Result (programcardTitle ep) Nothing Nothing Nothing (hasPublicPrimaryAsset ep) "DR"

instance FromJSON DREpisode where
  parseJSON =
    withObject "ep" $ \o -> do
      title <- o .: "ProgramcardTitle"
      slug <- o .: "ProgramcardSlug"
      public <- o .: "HasPublicPrimaryAsset"
      return $ DREpisode title slug public

decodeEpisodes :: ByteString -> Either String [DREpisode]
decodeEpisodes s = eitherDecode s >>= root >>= eps
  where
    root :: (FromJSON a) => Value -> Either String a
    root = parseEither $ withObject "root" (.: "Episodes")

    eps :: (FromJSON a) => Value -> Either String [a]
    eps = parseEither $ withArray "eps" $ mapM parseJSON . toList

-- "https://www.dr.dk/mu-online/api/1.3/list/view/quicksearch/" ++ query ++ "?limitprograms=" ++ show nPrograms ++ "%26limitepisodes=" ++ show nEpisodes ++ "%26orderBy=PrimaryBroadcastDay%26orderDescending=true"
drApiUrl :: Integer -> Integer -> Query -> String
drApiUrl nPrograms nEpisodes query = "http://localhost:1234/dr.json?" ++ query

decodeDR :: EitherWWWResponse -> Response
decodeDR (Left err) = Left err
decodeDR (Right response) =
  case drResult of
    Left x         -> Left $ "DR error:\n" ++ x
    Right episodes -> Right . map toResult $ episodes
  where
    drResult = decodeEpisodes . responseBody' $ response

searchDR :: Query -> IO Response
searchDR q = decodeDR . prefixError "DR:\n" <$> drData
  where
    drData = eitherGetWith opts url
    url = drApiUrl 5 5 q
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
