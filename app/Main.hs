import           Cli                     (Arguments, argProviders, argQuery,
                                          parseArgs)
import           StreamFind              (Query, Response, SearchFunction, fmt)
import           StreamFind.Providers    (resolveProviders)

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Data.Either             (lefts, rights)
import           GHC.Conc.Sync           (ThreadId)
import           System.Environment      (getArgs)

forkSearch :: Chan (IO Response) -> Query -> SearchFunction -> IO ThreadId
forkSearch channel query backend = forkIO . writeChan channel $ backend query

search :: Query -> [SearchFunction] -> IO [Response]
search query backends = do
  wire <- newChan
  let searcher = mapM $ forkSearch wire query
  searcher backends >>= consume wire >>= sequence
  where
    consume channel = mapM $ \_ -> readChan channel

searchAction :: Arguments -> IO ()
searchAction args = do
  let query = argQuery args
  let backends = resolveProviders (argProviders args)
  responses <- search query backends
  let errs = lefts responses
  let results = concat $ rights responses
  -- do something with reslts, and errors
  mapM_ (putStrLn . fmt) results
  mapM_ putStrLn errs

main :: IO ()
main = parseArgs >>= searchAction
