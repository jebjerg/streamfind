module Cli where

import           Data.List.Split     (splitOn)
import           Data.Monoid         ((<>))
import           Options.Applicative (argument, execParser, help, helper, info,
                                      long, metavar, short, showDefault, some,
                                      str, strOption, switch, value, (<**>))

data Arguments = Arguments
  { argQuery     :: String
  , argProviders :: [String]
  }

debug :: Arguments -> IO ()
debug o = do
  putStrLn $ "providers: " ++ (show . argProviders) o
  putStrLn $ "query: " ++ argQuery o

parseArgs :: IO Arguments
parseArgs = execParser opts
  where
    parser = options <$> providers <*> queries <**> helper
    options ps qs = Arguments (unwords qs) (splitOn "," ps)
    -- options
    queries = some (argument str (metavar "QUERY"))
    providers =
      strOption
        (short 'p' <> long "providers" <>
         help "Comma separated list of providers" <>
         value "*" <>
         showDefault <>
         metavar "PROVIDERS")
    opts = info parser mempty
