{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy      as BSL
import           Data.String
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Network.Wai
import           Network.HTTP.Types.Status
import qualified Network.Wai.Handler.Warp  as Warp
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           Text.Pandoc

import Settings
import Opts

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

  settings <- case applyOpts args defaultSettings of
    Left  errs      -> putStrLn `mapM_` errs >> exitFailure
    Right (set, []) -> makeSettingsAbsolute set

  when (hhostHelp settings) $ do
    putStrLn $ "host v" ++ VERSION_host ++ ", compiled " ++ __TIME__
    putStr   $ usageInfo "" optDescrs
    putStrLn ""
    putStrLn "Valid arguments to --host:"
    putStrLn "- *   Any host, both IPv4 and IPv6"
    putStrLn "- *4  Any host, prefer IPv4"
    putStrLn "- *6  Any host, prefer IPv6"
    putStrLn "- !4  Any host, only IPv4"
    putStrLn "- !6  Any host, only IPv6"
    putStrLn "Any other value is treated as a normal hostname."
    exitSuccess

  putStrLn $ "Hosting folder " ++ hhostRoot settings ++ " on " ++ hhostHost settings ++ ":" ++ show (hhostPort settings)

  Warp.runSettings (Warp.setHost (fromString (hhostHost settings)) (Warp.setPort (hhostPort settings) Warp.defaultSettings)) (app settings)
