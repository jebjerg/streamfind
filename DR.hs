{-# LANGUAGE OverloadedStrings #-}

module DR where

import           Common               (eitherGetWith, prefixError,
                                       responseBody')
import           Types                (EitherWWWResponse, Error, Query,
                                       Response, Result (..), ToResult,
                                       toResult)

import           Control.Lens         ((&), (.~))
import           Data.Aeson           (FromJSON, Value, eitherDecode, parseJSON,
                                       withArray, withObject, (.:))
import           Data.Aeson.Types     (Parser, parseEither)
import           Data.ByteString.Lazy (ByteString)
import           Data.List            (map)
import           Data.Vector          (toList)
import qualified Network.Wreq         as WWW

data DREpisode = DREpisode
  { programcardTitle      :: String
  , programcardSlug       :: String
  , hasPublicPrimaryAsset :: Bool
  , directLink            :: Maybe String
  } deriving (Show)

instance ToResult DREpisode where
  toResult ep =
    Result
      (programcardTitle ep)
      Nothing
      (directLink ep)
      Nothing
      (hasPublicPrimaryAsset ep)
      "DR"

instance FromJSON DREpisode where
  parseJSON =
    withObject "ep" $ \o -> do
      title <- o .: "ProgramcardTitle"
      slug <- o .: "ProgramcardSlug"
      public <- o .: "HasPublicPrimaryAsset"
      return $ DREpisode title slug public Nothing

decodeEpisodes :: ByteString -> Either Error [DREpisode]
decodeEpisodes s = eitherDecode s >>= root >>= eps
  where
    root :: (FromJSON a) => Value -> Either String a
    root = parseEither $ withObject "root" (.: "Episodes")
    eps :: (FromJSON a) => Value -> Either String [a]
    eps = parseEither $ withArray "eps" $ mapM parseJSON . toList

drApiUrl :: Integer -> Integer -> Query -> String
drApiUrl nPrograms nEpisodes query =
  "https://www.dr.dk/mu-online/api/1.3/list/view/quicksearch/" ++
  query ++
  "?limitprograms=" ++
  show nPrograms ++
  "%26limitepisodes=" ++
  show nEpisodes ++ "%26orderBy=PrimaryBroadcastDay%26orderDescending=true"

-- drApiUrl nPrograms nEpisodes query = "http://localhost:1234/dr.json?" ++ query
drProgramCardUrl :: String -> String
drProgramCardUrl slug =
  "https://www.dr.dk/mu-online/api/1.4/programcard/" ++ slug ++ "?expanded=true"

-- drProgramCardUrl slug = "http://localhost:1234/programcard.json?" ++ slug
decodeDRResponse :: EitherWWWResponse -> Either Error [DREpisode]
decodeDRResponse (Left err) = Left err
decodeDRResponse (Right response) =
  case drResult of
    Left x         -> Left $ "DR error:\n" ++ x
    Right episodes -> Right episodes
  where
    drResult = decodeEpisodes . responseBody' $ response

decodeCard :: EitherWWWResponse -> Either Error String
decodeCard (Left err) = Left err
decodeCard (Right response) =
  case (eitherDecode . responseBody') response >>= primaryAsset >>= links >>=
       uris of
    Left e       -> Left $ "program card error:\n" ++ e
    Right direct -> Right . head . tail $ direct
  where
    primaryAsset :: (FromJSON a) => Value -> Either String a
    primaryAsset = parseEither $ withObject "pa" (.: "PrimaryAsset")
    links :: (FromJSON a) => Value -> Either String a
    links = parseEither $ withObject "pa" (.: "Links")
    uris :: (FromJSON a) => Value -> Either String [a]
    uris = parseEither (withArray "ls" $ \bs -> mapM uri (toList bs))
      where
        uri = withObject "uri" (.: "Uri")

programCard :: String -> IO (Maybe String)
programCard slug = do
  card <- eitherCard
  return $
    case card of
      Left j    -> Nothing
      Right uri -> Just uri
  where
    cardData = eitherGetWith opts url
    url = drProgramCardUrl slug
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
    eitherCard = decodeCard <$> cardData

searchDR :: Query -> IO Response
searchDR q = do
  episodes <- decodeDRResponse . prefixError "DR:\n" <$> drData
  case episodes of
    Left e -> return (Left e)
    Right episodes -> do
      cards <- mapM (programCard . programcardSlug) episodes
      let episodes' = zipWith attachUrl episodes cards
      return . Right $ map toResult episodes'
  where
    drData = eitherGetWith opts url
    url = drApiUrl 5 5 q
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
    attachUrl ep Nothing = ep
    attachUrl ep card    = ep {directLink = card}
