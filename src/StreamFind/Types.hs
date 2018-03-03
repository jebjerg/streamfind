module StreamFind.Types where

import           Data.ByteString.Lazy (ByteString)
import qualified Network.Wreq         as WWW

type Query = String

type Error = String

data Result = Result
  { title       :: String
  , description :: Maybe String
  , url         :: Maybe String
  , price       :: Maybe Int
  , available   :: Bool
  , provider    :: String
  } deriving (Show)

class (Show a) =>
      ToResult a where
  toResult :: a -> Result

type Response = Either Error [Result]

type WWWResponse = WWW.Response ByteString

type EitherWWWResponse = Either Error WWWResponse
