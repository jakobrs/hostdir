{-# LANGUAGE LambdaCase #-}

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

  > hostdir [options]

  Arguments:
    --port        -p <port>         What port to listen on         (default: 8080)
    --host        -h <host>         What host to listen on         (default: 127.0.0.1)
    --root        -r <root>         Root directory                 (default: .)
    --path           <path>         How to find the files
    --404            <page>         404                            (default: 404.html)

  Default path: "%:%/index.html:%.html"

-}

first :: Monad m => [a] -> (a -> m Bool) -> m (Maybe a)
first []      _ = pure $ Nothing
first (a : b) f = f a >>= \case
  True  -> pure $ pure a
  False -> first b f

serveFile :: (Response -> IO w) -> [String] -> Status -> FilePath -> IO w
serveFile respond tags code res = case takeExtension res of
    ".md" -> do
      putStrLn $ " -> " ++ formattedTags ++ "<Markdown as HTML> " ++ res

      mdcont <- readFile res
      cont <- runIOorExplode $ do
        v <- readMarkdown (def { readerStandalone = True }) $ Text.pack mdcont
        writeHtml5String def v

      respond $ responseLBS
        code
        [("Content-Type", "text/html")]
        (BSL.fromStrict (Text.encodeUtf8 cont))
    _ -> do
      putStrLn $ " -> " ++ formattedTags ++ "<File> " ++ res
      respond $ responseFile
        code
        []
        res
        Nothing
  where
    formattedTags = tags >>= (++ " ")

app :: Settings -> Application
app settings req respond = do
  let requestString = joinPath (Text.unpack <$> pathInfo req)
  putStr $ requestString ++ ""

  a <- first (fmap ($ requestString) (hhostPath settings)) doesFileExist
  case a of
    Nothing  -> serveFile respond ["[404]"] status404 (hhost404 settings)
    Just res -> serveFile respond []        status200 res

printVersion :: IO ()
#ifdef RELEASE
printVersion = putStrLn $ "hostdir v" ++ VERSION_hostdir
#else
printVersion = putStrLn $ "hostdir v" ++ VERSION_hostdir ++ ", compiled " ++ __DATE__ ++ " " ++ __TIME__
#endif

main :: IO ()
main = do
  args <- getArgs

  settings <- case applyOpts args defaultSettings of
    Left  errs      -> putStrLn `mapM_` errs >> exitFailure
    Right (set, []) -> makeSettingsAbsolute set

  when (hhostVer settings) $ do
    printVersion
    exitSuccess

  when (hhostHelp settings) $ do
    printVersion
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
