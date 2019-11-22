{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy      as BSL
import           Data.String
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Data.Time.Clock           as Time
import qualified Data.Time.Format          as Time
import           Network.Wai
import           Network.HTTP.Types.Status
import qualified Network.Wai.Handler.Warp  as Warp
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Pandoc

import Settings
import Opts

first :: Monad m => [a] -> (a -> m Bool) -> m (Maybe a)
first []      _ = pure Nothing
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
  let pathInfoReq = pathInfo req
  let requestString = joinPath (Text.unpack <$> pathInfoReq)

  do
    time <- Time.getCurrentTime
    putStr $ Time.formatTime Time.defaultTimeLocale "[%F %T%3Q %Z] " time
  putStr $ "<" ++ show (remoteHost req) ++ "> "
  putStr requestString

  -- Refuse to fulfill request if `..` found in the path
  if ".." `elem` pathInfoReq then do
    putStrLn " -> [403]"
    respond $ responseLBS
      status403
      [("Content-Type", "text/plain")]
      "Forbidden (suspicious request)"
  else do
    a <- first (fmap ($ requestString) (hhostPath settings)) doesFileExist
    case a of
      Nothing  -> serveFile respond ["[404]"] status404 (hhost404 settings)
      Just res -> serveFile respond []        status200 res

printVersion :: IO ()
#ifdef RELEASE
printVersion = putStrLn $ "hostdir v" ++ CURRENT_PACKAGE_VERSION
#else
printVersion = putStrLn $ "hostdir v" ++ CURRENT_PACKAGE_VERSION ++ ", compiled " ++ __DATE__ ++ " " ++ __TIME__
#endif

main :: IO ()
main = do
  args <- getArgs

  settings <- case applyOpts args defaultSettings of
    Left  errs      -> putStrLn `mapM_` errs >> exitFailure
    Right (set, []) -> makeSettingsAbsolute set
    Right (_,   _ ) -> putStrLn "hostdir takes no non-option arguments" >> exitFailure

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

  hSetBuffering stdout (hhostBuf settings)

  putStrLn $ "Hosting folder " ++ hhostRoot settings ++ " on " ++ hhostHost settings ++ ":" ++ show (hhostPort settings)

  Warp.runSettings (Warp.setHost (fromString (hhostHost settings)) (Warp.setPort (hhostPort settings) Warp.defaultSettings)) (app settings)
