{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.Netflix where

import           StreamFind.Common                    (eitherPostWith,
                                                       unpackResponse)
import           StreamFind.Providers.Netflix.Cookies (Cookie, chromeCookies,
                                                       cookieJar)
import           StreamFind.Providers.Netflix.Token   (apiToken)
import           StreamFind.Types                     (Error, Query, Response,
                                                       Result (..), ToResult,
                                                       toResult)

import           Control.Applicative                  ((<|>))
import           Control.Lens                         ((&), (.~))
import           Control.Monad                        ((>=>))
import           Data.Aeson                           (Value, eitherDecode,
                                                       withObject, (.:))
import           Data.Aeson.Types                     (Parser, parseEither)
import           Data.ByteString.Char8                as BSC
import qualified Data.ByteString.Lazy.Char8           as LBSC
import           Data.HashMap.Strict                  (toList)
import           Data.Maybe                           (catMaybes)
import qualified Data.Text                            as T
import           Data.Traversable                     (for)
import qualified Network.Wreq                         as WWW
import           System.Environment                   (lookupEnv)

providerName = "Netflix"

data Netflix = Netflix
  { nfTitle    :: String
  , nfTyp      :: String
  , nfOriginal :: Bool
  , nfLink     :: String
  } deriving (Show)

instance ToResult Netflix where
  toResult n =
    Result (nfTitle n) Nothing (Just (nfLink n)) Nothing True providerName

decodeNetflixSearch :: Either Error String -> Either Error [Netflix]
decodeNetflixSearch nfResp =
  case nfResp of
    Left e -> Left $ providerName ++ ":\n" ++ e
    Right resp ->
      eitherDecode (LBSC.pack resp) >>= parseEither netflix >>=
      Right . catMaybes
      where netflix :: Value -> Parser [Maybe Netflix]
            netflix =
              withObject "root" (.: "value") >=>
              withObject "value" (.: "videos") >=> videos
            videos :: Value -> Parser [Maybe Netflix]
            videos =
              withObject "videos" $ \o ->
                for (toList o) $ \(videoId, videoObj) -> do
                  let videoId' = T.unpack videoId
                  Just <$> video videoId' videoObj <|> return Nothing -- TODO: :(
            video :: String -> Value -> Parser Netflix
            video vId =
              withObject "hit" $ \v -> do
                nTitle <- v .: "title"
                nType <- v .: "summary" >>= (.: "type")
                nOriginal <- v .: "summary" >>= (.: "isOriginal")
                return
                  Netflix
                  { nfTitle = nTitle
                  , nfTyp = nType
                  , nfOriginal = nOriginal
                  , nfLink = "https://netflix.com/title/" ++ vId
                  }

-- TODO: type -> json
searchData q n =
  BSC.pack $
  "{\"paths\":[[\"search\",\"byTerm\",\"|" ++
  q ++
  "\",\"titles\"," ++
  show n ++
  ",{\"from\":0,\"to\":" ++
  show n ++
  "},\"reference\",[\"summary\",\"title\",\"titleMaturity\",\"userRating\",\"userRatingRequestId\"]]]}"

-- netflixUrl token = "http://localhost:1234/netflix.json?" ++ token
netflixUrl token =
  "https://www.netflix.com/api/shakti/" ++
  token ++
  "/pathEvaluator?drmSystem=widevine&isWatchlistEnabled=false&isShortformEnabled=false&fetchListAnnotations=false&canWatchBranchingPuss=false&withSize=true&materialize=true"

searchNetflix' :: IO [Cookie] -> Int -> Query -> IO Response
searchNetflix' cookies n q = do
  cookieJar' <- cookies'
  shaktiToken <- apiToken cookieJar'
  case shaktiToken of
    Left err -> return . Left $ err
    Right token ->
      (resultify . decodeNetflixSearch) . unpackResponse <$> response
      where response =
              eitherPostWith
                (opts cookieJar')
                (netflixUrl token)
                (searchData q n)
            opts cookieJar' =
              WWW.defaults & WWW.header "User-Agent" .~ [] &
              WWW.header "Content-Type" .~
              ["application/json"] &
              WWW.cookies .~
              Just cookieJar'
            resultify searchResults =
              case searchResults of
                Left err        -> Left err
                Right nfResults -> Right $ Prelude.map toResult nfResults
  where
    cookies' = cookieJar =<< cookies

searchNetflix :: Query -> IO Response
searchNetflix q = do
  home <- lookupEnv "HOME"
  case home of
    Nothing -> return . Left $ "Unable to find/use $HOME. No cookies specified"
    Just location -> searchNetflix' (defaultCookies location) 1 q
  where
    defaultCookies home = chromeCookies (cookiesLocation home) ".netflix.com"
    cookiesLocation = flip (++) "/.config/google-chrome/Default/Cookies"
