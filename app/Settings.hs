module Settings where

import Data.Streaming.Network
import System.Directory
import System.FilePath
import System.IO

data Settings = Settings
  { hhostPort :: Int
  , hhostHost :: HostPreference
  , hhostRoot :: FilePath
  , hhostPath :: [FilePath -> FilePath]
  , hhost404  :: FilePath
  , hhostHelp :: Bool
  , hhostVer  :: Bool
  , hhostBuf  :: BufferMode
  }

defaultSettings :: Settings
defaultSettings = Settings
  { hhostPort = 8080
  , hhostHost = "127.0.0.1"
  , hhostRoot = "."
  , hhostPath = [id, (</> "index.html"), (</> "index.md"), (++ ".html"), (++ ".md")]
  , hhost404  = "404.html"
  , hhostHelp = False
  , hhostVer  = False
  , hhostBuf  = LineBuffering
  }

makeSettingsAbsolute :: Settings -> IO Settings
makeSettingsAbsolute set = do
  absRoot <- makeAbsolute (hhostRoot set)
  pure $ set
    { hhostPath = fmap ((absRoot </>) .) (hhostPath set)
    , hhost404 = absRoot </> hhost404 set
    , hhostRoot = absRoot
    }
