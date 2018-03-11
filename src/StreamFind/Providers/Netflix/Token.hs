{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.Netflix.Token where

import           Control.Lens        ((&), (.~))
import           Network.HTTP.Client (CookieJar)
import qualified Network.Wreq        as WWW
import           StreamFind.Common   (eitherGetWith, unpackResponse)
import           StreamFind.Types    (Error)
import           Text.Regex.Posix    ((=~))

tokenPattern :: String
tokenPattern = "\"BUILD_IDENTIFIER\":\"([a-f0-9]+)\""

apiToken :: CookieJar -> IO (Either Error String)
apiToken cookieJar' = do
  response <- tokenResp -- TODO: eww :(
  return $
    case response of
      Left err  -> Left $ "Unable to fetch /browse:\n" ++ err
      Right res -> tokenize . Prelude.concat . tokenMatch $ res
  where
    url = "https://www.netflix.com/browse"
    -- url = "http://localhost:1234/browse.html"
    opts =
      WWW.defaults & WWW.header "User-Agent" .~ [] & WWW.cookies .~
      Just cookieJar'
    tokenResp :: IO (Either Error String)
    tokenResp = unpackResponse <$> eitherGetWith opts url
    tokenMatch :: String -> [[String]]
    tokenMatch = flip (=~) tokenPattern
    tokenize :: [String] -> Either Error String
    tokenize (_:token:_) = Right token
    tokenize _           = Left "Unable to find BUILD_IDENTIFIER"
