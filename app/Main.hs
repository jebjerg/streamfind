import           StreamFind              (Query, Response, fmt)
import           StreamFind.Providers    (searchBlockbuster, searchCMore,
                                          searchDR, searchHBO, searchTV2,
                                          searchViaplay)

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Data.Either             (lefts, rights)
import           GHC.Conc.Sync           (ThreadId)
import           System.Environment      (getArgs)

forkSearch ::
     Chan (IO Response) -> Query -> (Query -> IO Response) -> IO ThreadId
forkSearch channel query backend = forkIO . writeChan channel $ backend query

search :: Query -> [Query -> IO Response] -> IO [Response]
search query backends = do
  wire <- newChan
  let searcher = mapM $ forkSearch wire query
  searcher backends >>= consume wire >>= sequence
  where
    consume channel = mapM $ \_ -> readChan channel

main = do
  q <- unwords <$> getArgs
  let backends =
        [ searchBlockbuster
        , searchCMore
        , searchDR
        , searchHBO
        , searchTV2
        , searchViaplay
        ]
  responses <- search q backends
  let errs = lefts responses
  let results = concat $ rights responses
  -- do something with reslts, and errors
  mapM_ (putStrLn . fmt) results
  mapM_ putStrLn errs
