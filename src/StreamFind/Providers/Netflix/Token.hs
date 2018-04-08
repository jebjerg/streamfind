{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.Netflix.Token where

import           Control.Lens        ((&), (.~))
import           Network.HTTP.Client (CookieJar)
import qualified Network.Wreq        as WWW
import           StreamFind.Common   (eitherGetWith, prefixError,
                                      unpackResponse)
import           StreamFind.Types    (Error)
import           Text.Regex          (Regex, matchRegex, mkRegex)

tokenPattern :: Regex
tokenPattern = mkRegex "\"BUILD_IDENTIFIER\":\"([a-f0-9]+)\""

apiToken :: CookieJar -> IO (Either Error String)
apiToken cookieJar' = do
  response <- tokenResp
  return . prefixError "Unable to fetch /browse:\n" $
    response >>= tokenize . tokenMatch
  where
    url = "https://www.netflix.com/browse"
    -- url = "http://localhost:1234/browse.html"
    opts =
      WWW.defaults & WWW.header "User-Agent" .~ [] & WWW.cookies .~
      Just cookieJar'
    tokenResp :: IO (Either Error String)
    tokenResp = unpackResponse <$> eitherGetWith opts url
    tokenMatch :: String -> Maybe [String]
    tokenMatch = matchRegex tokenPattern
    tokenize :: Maybe [String] -> Either Error String
    tokenize (Just (token:_)) = Right token
    tokenize _                = Left "Unable to find BUILD_IDENTIFIER"
