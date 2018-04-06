{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.DR where

import           StreamFind.Common          (eitherGetWith, prefixError,
                                             responseBody', urlEncode')
import           StreamFind.Types           (Error, Query, Response,
                                             Result (..), ToResult, toResult)

import           Control.Error.Util         (hush, hushT, note, noteT)
import           Control.Lens               ((&), (.~))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Maybe  (runMaybeT)
import           Data.Aeson                 (FromJSON, Value, eitherDecode,
                                             parseJSON, withArray, withObject,
                                             (.:))
import           Data.Aeson.Types           (Parser, parseEither)
import           Data.ByteString.Lazy       (ByteString)
import           Data.List                  (map)
import           Data.Vector                (toList)
import qualified Network.Wreq               as WWW

providerName = "DR"

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
      providerName

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
  urlEncode' query ++
  "?limitprograms=" ++
  show nPrograms ++
  "%26limitepisodes=" ++
  show nEpisodes ++ "%26orderBy=PrimaryBroadcastDay%26orderDescending=true"

-- drApiUrl nPrograms nEpisodes query = "http://localhost:1234/dr.json?" ++ query
drProgramCardUrl :: String -> String
drProgramCardUrl slug =
  "https://www.dr.dk/mu-online/api/1.4/programcard/" ++
  urlEncode' slug ++ "?expanded=true"

-- drProgramCardUrl slug = "http://localhost:1234/programcard.json?" ++ slug
decodeCard :: ByteString -> Either Error String
decodeCard response =
  head . tail <$> (eitherDecode response >>= primaryAsset >>= links >>= uris)
  where
    primaryAsset :: (FromJSON a) => Value -> Either String a
    primaryAsset = parseEither $ withObject "pa" (.: "PrimaryAsset")
    links :: (FromJSON a) => Value -> Either String a
    links = parseEither $ withObject "pa" (.: "Links")
    uris :: (FromJSON a) => Value -> Either String [a]
    uris = parseEither (withArray "ls" $ \bs -> mapM uri (toList bs))
    uri :: (FromJSON a) => Value -> Parser a
    uri = withObject "uri" (.: "Uri")

programCard :: String -> IO (Maybe String)
programCard slug = runMaybeT $ hushT (response >>= decoded)
  where
    url = drProgramCardUrl slug
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
    response = responseBody' <$> ExceptT (eitherGetWith opts url)
    decoded = ExceptT . return . decodeCard

searchDR :: Query -> IO Response
searchDR q =
  prefixError (providerName ++ ":\n") <$>
  runExceptT (map toResult <$> (response >>= decoded >>= mapM attachUrl))
  where
    url = drApiUrl 5 5 q
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
    response = responseBody' <$> ExceptT (eitherGetWith opts url)
    decoded = ExceptT . return . decodeEpisodes
    attachUrl :: DREpisode -> ExceptT Error IO DREpisode
    attachUrl ep = setUrl ep <$> (liftIO . programCard . programcardSlug) ep
    setUrl ep link = ep {directLink = link}
