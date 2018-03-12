{-# LANGUAGE OverloadedStrings #-}

module StreamFind.Providers.Netflix.Cookies where

-- sqlite
import           Data.ByteString        (ByteString)
import           Database.SQLite.Simple (FromRow, Only (..), close, field,
                                         fromRow, open, query)

-- decryption
import           Crypto.Cipher.AES      (AES128)
import           Crypto.Cipher.Types    (BlockCipher (..), Cipher (..), makeIV,
                                         nullIV)
import           Crypto.Data.Padding    (Format (PKCS7, ZERO), pad, unpad)
import           Crypto.Error           (CryptoFailable (CryptoFailed, CryptoPassed))
import           Crypto.KDF.PBKDF2      (Parameters (..), fastPBKDF2_SHA1,
                                         iterCounts, outputLength)
import qualified Data.ByteString.Char8  as BSC
import           Data.Maybe             (fromMaybe)
import           Data.Time.Clock        (addUTCTime, getCurrentTime)
import qualified Network.HTTP.Client    as HTTP

data Cookie = Cookie
  { cookieName  :: String
  , cookieValue :: String
  } deriving (Show)

instance FromRow Cookie where
  fromRow = Cookie <$> field <*> field

cookieJar :: [Cookie] -> IO HTTP.CookieJar
cookieJar browserCookies =
  fmap HTTP.createCookieJar (sequence (convCookie <$> browserCookies))

convCookie :: Cookie -> IO HTTP.Cookie
convCookie c = do
  now <- getCurrentTime
  let tomorrow = addUTCTime (30 * 24 * 60 * 60) now
  return
    HTTP.Cookie
    { HTTP.cookie_name = BSC.pack . cookieName $ c
    , HTTP.cookie_value = BSC.pack . cookieValue $ c
    , HTTP.cookie_expiry_time = tomorrow
    , HTTP.cookie_domain = "netflix.com" :: ByteString
    , HTTP.cookie_path = "/" :: ByteString
    , HTTP.cookie_creation_time = now
    , HTTP.cookie_last_access_time = now
    , HTTP.cookie_persistent = True
    , HTTP.cookie_host_only = False
    , HTTP.cookie_secure_only = True
    , HTTP.cookie_http_only = False
    }

netflixCookies :: [Cookie] -> [Cookie]
netflixCookies = filter (\x -> cookieName x `elem` whitelist)
  where
    whitelist =
      [ "VisitorId"
      , "pas"
      , "memclid"
      , "nfvdid"
      , "SecureNetflixId"
      , "NetflixId"
      , "clSharedContext"
      , "profilesNewSession"
      ]

firefoxCookies :: String -> String -> IO [Cookie]
firefoxCookies path host = do
  conn <- open path
  cookies <-
    query conn "SELECT name, value FROM moz_cookies WHERE host = ?" (Only host)
  close conn
  return $ netflixCookies cookies

data ChromeCookie = ChromeCookie
  { name           :: String
  , encryptedValue :: ByteString
  } deriving (Show)

chromeKey =
  fastPBKDF2_SHA1
    Parameters {iterCounts = 1, outputLength = 16}
    (BSC.pack "peanuts")
    (BSC.pack "saltysalt")

chromeIV = "\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20"

instance FromRow ChromeCookie where
  fromRow = ChromeCookie <$> field <*> field

chromeCookies :: String -> String -> IO [Cookie]
chromeCookies path host = do
  conn <- open path
  rows <-
    query
      conn
      "SELECT name, IFNULL(CAST(substr(encrypted_value, 4) AS BLOB), CAST('' AS BLOB)) FROM cookies WHERE host_key = ?"
      (Only host)
  close conn
  let whitelist =
        [ "VisitorId"
        , "pas"
        , "memclid"
        , "nfvdid"
        , "SecureNetflixId"
        , "NetflixId"
        , "clSharedContext"
        , "profilesNewSession"
        ]
  return . netflixCookies $
    map (\r -> Cookie (name r) (decryptChrome (encryptedValue r))) rows
  where
    decryptChrome =
      BSC.unpack . unpad' . decrypt' chromeKey chromeIV . pad (ZERO blockSize)
    unpad' = fromMaybe "" . unpad (PKCS7 blockSize)
    blockSize = 16

newtype Key a =
  Key ByteString
  deriving (Show, Eq)

decrypt' :: ByteString -> ByteString -> ByteString -> ByteString
decrypt' key iv = cbcDecrypt ctx iv'
  where
    iv' = fromMaybe nullIV (makeIV iv)
    ctx = cipherInitNoErr blockCipher
    blockCipher = cipherMakeKey (undefined :: AES128) key
    cipherInitNoErr :: BlockCipher c => Key c -> c
    cipherInitNoErr (Key k) =
      case cipherInit k of
        CryptoPassed a -> a
        CryptoFailed e -> error (show e)
    cipherMakeKey :: Cipher cipher => cipher -> ByteString -> Key cipher
    cipherMakeKey _ = Key
