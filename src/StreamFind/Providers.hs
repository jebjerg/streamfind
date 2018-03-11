module StreamFind.Providers
  ( resolveProviders
  , Blockbuster.searchBlockbuster
  , CMore.searchCMore
  , DR.searchDR
  , HBO.searchHBO
  , Netflix.searchNetflix
  , TV2.searchTV2
  , Viaplay.searchViaplay
  ) where

import           StreamFind.Providers.Blockbuster as Blockbuster
import           StreamFind.Providers.CMore       as CMore
import qualified StreamFind.Providers.DR          as DR
import           StreamFind.Providers.HBO         as HBO
import           StreamFind.Providers.Netflix     as Netflix
import           StreamFind.Providers.TV2         as TV2
import           StreamFind.Providers.Viaplay     as Viaplay

import           Data.Char                        (toLower)
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe                       (mapMaybe)
import           StreamFind.Types                 (SearchFunction)

providers :: [(String, SearchFunction)]
providers =
  [ (Blockbuster.providerName, Blockbuster.searchBlockbuster)
  , (CMore.providerName, CMore.searchCMore)
  , (DR.providerName, DR.searchDR)
  , (HBO.providerName, HBO.searchHBO)
  , (Netflix.providerName, Netflix.searchNetflix)
  , (TV2.providerName, TV2.searchTV2)
  , (Viaplay.providerName, Viaplay.searchViaplay)
  ]

resolveProviders :: [String] -> [SearchFunction]
resolveProviders ps = mapMaybe resolve ps
resolveProviders ["*"] = map snd providers
  where
    resolve :: String -> Maybe SearchFunction
    resolve askName = HM.lookup askName (HM.fromList (map lowerCase providers))
    lowerCase :: (String, SearchFunction) -> (String, SearchFunction)
    lowerCase (name, fn) = (map toLower name, fn)
