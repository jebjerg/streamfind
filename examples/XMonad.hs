import           StreamFind.Providers (searchDR)
import qualified StreamFind.Types     as SFT
import           XMonad.Prompt        (XPConfig (searchPredicate),
                                       XPrompt (showXPrompt), def, mkXPrompt)
import           XMonad.Prompt.Shell  (getShellCompl)

newtype SearchPrompt =
  SearchPrompt String

instance XPrompt SearchPrompt where
  showXPrompt (SearchPrompt n) = "Search: "

-- get search term and feed into searchMenu
searchPrompt :: XPConfig -> X ()
searchPrompt config =
  mkXPrompt
    (SearchPrompt "")
    config
    (getShellCompl [] $ searchPredicate config)
    searchMenu

defSearchPrompt = searchPrompt def

streamApp = "notify+send" -- clight stream, vlc, notify-send, ...

-- perform search, and display gridselect with [(title, url)], spawning streamApp url
searchMenu :: String -> X ()
searchMenu term = do
  response <- io $ searchDR term
  case response of
    Left e -> spawn $ "notify-send" ++ " " ++ "ERROR: " ++ e
    Right results ->
      gridselect defaultGSConfig (map menuItem results) >>=
      flip whenJust (\x -> spawn $ streamApp ++ " " ++ x)
  where
    menuItem :: SFT.Result -> (String, String)
    menuItem result = (SFT.title result, streamUrl $ SFT.url result)
      where
        streamUrl Nothing  = ""
        streamUrl (Just u) = u
{-
 ...
 add keybinding to defSearchPrompt, e.g.: `additionalKeysP` [("M-S-v", defSearchPrompt)]
-}
