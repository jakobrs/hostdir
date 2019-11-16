{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Network.Wai
import           Network.HTTP.Types.Status
import qualified Network.Wai.Handler.Warp  as Warp
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Pandoc

{-

  > hhost [args] <folder>

  Arguments:
    --port        -p <port>         What port to listen on         (default: 8080)
    --host        -h <host>         What host to listen on         (default: 0.0.0.0)
    --path           <path>         How to find the files
    --404            <page>         404                            (default: 404.html)
    --ipv4        -4                Use IPv4
    --ipv6        -6                Use IPv6

  Default path: "%:%/index.html:%.html"

-}

data Settings = Settings
  { hhostPort :: Int
  , hhostHost :: String
  , hhostFolder :: String
  , hhostPath :: [String -> String]
  , hhost404 :: FilePath
  }

defaultSettings :: Settings
defaultSettings = Settings
  { hhostPort = 8080
  , hhostHost = "0.0.0.0"
  , hhostFolder = "/srv/www/html"
  , hhostPath = [id, (</> "index.html"), (</> "index.md"), (++ ".html"), (++ ".md")]
  , hhost404 = "404.html"
  }

first :: Monad m => [a] -> (a -> m Bool) -> m (Maybe a)
first []      _ = pure $ Nothing
first (a : b) f = f a >>= \case
  True  -> pure $ pure a
  False -> first b f

doesFileExist' :: FilePath -> IO Bool
doesFileExist' a = do
  putStrLn $ "[doesFileExist'] Considering " ++ show a
  res <- doesFileExist a
  when res $ putStrLn "[doesFileExist'] Chosen"
  pure res

serveFile :: (Response -> IO w) -> [String] -> Status -> FilePath -> IO w
serveFile respond tags code res = case takeExtension res of
    ".md" -> do
      putStrLn $ "Response: " ++ formattedTags ++ "<Markdown as HTML> " ++ res

      mdcont <- readFile res
      cont <- runIOorExplode $ do
        v <- readMarkdown (def { readerStandalone = True }) $ Text.pack mdcont
        writeHtml5String def v

      respond $ responseLBS
        code
        [("Content-Type", "text/html")]
        (BSL.fromStrict (Text.encodeUtf8 cont))
    _ -> do
      putStrLn $ "Response: " ++ formattedTags ++ "<File> " ++ res
      respond $ responseFile
        code
        []
        res
        Nothing
  where
    formattedTags = tags >>= (++ " ")

app :: Settings -> Application
app settings req respond = do
  putStrLn $ replicate 70 '-'
  let requestString = joinPath (Text.unpack <$> pathInfo req)
  putStrLn $ "Handling request: " ++ requestString

  a <- first (fmap ($ requestString) (hhostPath settings)) doesFileExist'
  case a of
    Nothing  -> serveFile respond ["[404]"] status404 (hhost404 settings)
    Just res -> serveFile respond []        status200 res

main :: IO ()
main = do
  args <- getArgs

  [absFldr] <- makeAbsolute `mapM` args

  let settings = defaultSettings
        { hhostPath = fmap ((absFldr </>) .) (hhostPath defaultSettings)
        , hhost404 = absFldr </> hhost404 defaultSettings
        , hhostFolder = absFldr
        }

  putStrLn $ "Hosting folder: " ++ hhostFolder settings

  Warp.runSettings (Warp.setPort (hhostPort settings) Warp.defaultSettings) (app settings)
