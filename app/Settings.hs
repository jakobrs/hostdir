module Settings where

import System.Directory
import System.FilePath

data Settings = Settings
  { hhostPort :: Int
  , hhostHost :: String
  , hhostRoot :: String
  , hhostPath :: [String -> String]
  , hhost404  :: FilePath
  , hhostHelp :: Bool
  }

defaultSettings :: Settings
defaultSettings = Settings
  { hhostPort = 8080
  , hhostHost = "127.0.0.1"
  , hhostRoot = "/srv/www/html"
  , hhostPath = [id, (</> "index.html"), (</> "index.md"), (++ ".html"), (++ ".md")]
  , hhost404  = "404.html"
  , hhostHelp = False
  }

makeSettingsAbsolute :: Settings -> IO Settings
makeSettingsAbsolute set = do
  absRoot <- makeAbsolute (hhostRoot set)
  pure $ set
    { hhostPath = fmap ((absRoot </>) .) (hhostPath set)
    , hhost404 = absRoot </> hhost404 set
    , hhostRoot = absRoot
    }
