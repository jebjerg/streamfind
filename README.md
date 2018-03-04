# StreamFind

## Types / Adding new providers

```haskell
-- a search provider
searchXYZ :: Query -> IO Response

type Response = Either Error [Result]

data Result = Result
  { title       :: String
  , description :: Maybe String
  , url         :: Maybe String
  , price       :: Maybe Int
  , available   :: Bool
  , provider    :: String
  } deriving (Show)

-- helpers
eitherGetWith :: Options -> String -> IO EitherWWWResponse
-- reponseBody'
```

It's encouraged to use the `ToResult` typeclass, as you'll more often than not have a more detailed result from your provider. Keeping the (intermediary) type makes `Result`-fitting and the provider easier to maintain.

See [`DR.hs`](src/StreamFind/DR.hs) as an example.

## Usage

See [`XMonad.hs`](examples/XMonad.hs) as an example integration.

Snippit from [`Main.hs`](app/Main.hs):

```haskell
main = do
  q <- unwords <$> getArgs
  let backends = [searchDR]
  responses <- search q backends
  let errs = lefts responses
  let results = concat $ rights responses
  -- do something with reslts, and errors
  mapM_ (putStrLn . fmt) results
  mapM_ putStrLn errs
```
