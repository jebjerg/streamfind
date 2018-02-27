{-# LANGUAGE OverloadedStrings #-}

module DR where

import           Common               (eitherGetWith, extractKey, prefixError,
                                       responseBody')
import           Control.Lens         ((&), (.~))
import           Data.Aeson           (FromJSON, eitherDecode, parseJSON,
                                       withObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.List            (map)
import qualified Network.Wreq         as WWW
import           Types                (EitherWWWResponse, Error, Query,
                                       Response, Result (..), ToResult,
                                       toResult)

newtype DRResult = DRResult
  { episodes :: [DREpisode]
  } deriving (Show)

instance FromJSON DRResult where
  parseJSON =
    withObject "dr" $ \o -> do
      eps <- extractKey "Episodes" o
      return $ DRResult eps

data DREpisode = DREpisode
  { programcardTitle      :: String
  , programcardSlug       :: String
  , hasPublicPrimaryAsset :: Bool
  } deriving (Show)

instance FromJSON DREpisode where
  parseJSON =
    withObject "ep" $ \o -> do
      title <- extractKey "ProgramcardTitle" o
      slug <- extractKey "ProgramcardSlug" o
      public <- extractKey "HasPublicPrimaryAsset" o
      return $ DREpisode title slug public

instance ToResult DREpisode where
  toResult ep =
    Result (programcardTitle ep) Nothing Nothing Nothing (hasPublicPrimaryAsset ep) "DR"

-- "https://www.dr.dk/mu-online/api/1.3/list/view/quicksearch/" ++ query ++ "?limitprograms=" ++ show nPrograms ++ "%26limitepisodes=" ++ show nEpisodes ++ "%26orderBy=PrimaryBroadcastDay%26orderDescending=true"
drApiUrl :: Integer -> Integer -> Query -> String
drApiUrl nPrograms nEpisodes query = "http://localhost:1234/dr.json?" ++ query

decodeDR :: EitherWWWResponse -> Response
decodeDR (Left err) = Left err
decodeDR (Right response) =
  case drResult of
    Left x         -> Left $ "DR error:\n" ++ x
    Right response -> Right . map toResult $ episodes response
  where
    drResult = eitherDecode . responseBody' $ response :: Either Error DRResult

searchDR :: Query -> IO Response
searchDR q = decodeDR . prefixError "DR:\n" <$> drData
  where
    drData = eitherGetWith opts url
    url = drApiUrl 5 5 q
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
