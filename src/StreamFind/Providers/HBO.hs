{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.HBO where

import           StreamFind.Common (eitherGetWith, unpackResponse)
import           StreamFind.Types  (Error, Query, Response, Result (..))

import           Control.Lens      ((&), (.~))
import           Data.Either       (rights)
import qualified Data.Text         as TXT
import qualified Network.Wreq      as WWW
import           Text.Feed.Import  (parseFeedString)
import           Text.Feed.Query   (feedItems)
import           Text.Feed.Types   (Item (RSSItem))
import           Text.RSS.Syntax   (RSSItem, rssItemDescription, rssItemLink,
                                    rssItemTitle)

providerName = "HBO"

result :: Item -> Either Error Result
result (RSSItem i) =
  case title of
    Nothing -> Left "failed to decode title"
    Just title ->
      Right $
      Result
        (TXT.unpack title)
        (TXT.unpack <$> rssItemDescription i)
        (TXT.unpack <$> rssItemLink i)
        Nothing
        True
        providerName
  where
    title = rssItemTitle i

searchHBO :: Query -> IO Response
searchHBO q = do
  hboData <- hboResp
  case hboData of
    Left e -> return . Left $ providerName ++ ":\n" ++ e
    Right resp ->
      return $
      case parseFeedString resp of
        Nothing -> Left "Unable to parse"
        Just f -> Right positive -- TODO: handle failed results mapping
          where positive = rights $ map result $ feedItems f
  where
    hbo = eitherGetWith opts url
    url =
      "https://api-hbon.hbo.clearleap.com/cloffice/client/web/search/?max=30&offset=0&sort=name&order=asc&query=" ++
      q ++ "&language=da_hbon"
    -- url = "http://localhost:1234/hbo.xml?" ++ q
    opts = WWW.defaults & WWW.header "User-Agent" .~ []
    hboResp = unpackResponse <$> hbo
