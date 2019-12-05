{-# LANGUAGE LambdaCase, ViewPatterns #-}

module Main where

import           Control.Monad
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.String
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Time.Clock            as Time
import qualified Data.Time.Format           as Time
import           Network.Wai
import           Network.HTTP.Types.Status
import qualified Network.Wai.Handler.Warp   as Warp
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Pandoc
import qualified Data.CaseInsensitive as CI

import Lib
import Opts
import Settings

first :: Monad m => [a] -> (a -> m Bool) -> m (Maybe a)
first []      _ = pure Nothing
first (a : b) f = f a >>= \case
  True  -> pure $ pure a
  False -> first b f

serve404 :: (Response -> IO w) -> FilePath -> IO w
serve404 respond path = do
  content <- doesFileExist path >>= \case
    True -> do
      putStrLn $ " -> [404] <File> " ++ path
      BSL.readFile path
    False -> do
      putStrLn $ " -> [404] <Fallback>"
      pure "File not found"

  respond $ responseLBS
    status404
    []
    content

serveFile :: (Response -> IO w) -> Bool -> FilePath -> IO w
serveFile respond convert res = case (convert, takeExtension res) of
  (True, ".md") -> do
    putStrLn $ " -> <Markdown as HTML> " ++ res

    mdcont <- readFile res
    cont <- runIOorExplode $ do
      v <- readMarkdown (def { readerStandalone = True }) $ Text.pack mdcont
      writeHtml5String def v

    respond $ responseLBS
      status200
      [("Content-Type", "text/html")]
      (BSL.fromStrict (Text.encodeUtf8 cont))
  _ -> do
    putStrLn $ " -> <File> " ++ res
    respond $ responseFile
      status200
      []
      res
      Nothing

app :: Settings -> Application
app settings req respond = do
  let pathInfoReq = pathInfo req
  let requestString = joinPath (Text.unpack <$> pathInfoReq)

#ifdef VERBOSE
  putStrLn $ replicate 70 '-'
  do
    time <- Time.getCurrentTime
    putStr $ Time.formatTime Time.defaultTimeLocale 
             "Time:                 %F %T%3Q %Z \n" time
  putStrLn $ "Method:               " ++ BSC.unpack (requestMethod req)
  putStrLn $ "HTTP version:         " ++ show (httpVersion req)
  putStrLn $ "Raw path info:        " ++ show (rawPathInfo req)
  putStrLn $ "Raw query string:     " ++ show (rawQueryString req)
  putStrLn   "Request headers:"
  requestHeaders req `forM_` \(CI.original -> key, value) -> do
    putStr "- "
    let len = BS.length key
    BSC.putStr key
    putStr $ replicate (20 - len) ' '
    putStrLn $ show value
  putStrLn $ "Secure?               " ++ if (isSecure req) then "yes" else "no"
  putStrLn $ "Path info:            " ++ show pathInfoReq
  putStrLn $ "Query string:         " ++ show (queryString req)
  putStrLn $ "Request body length:  " ++ show (requestBodyLength req)
  putStrLn $ "Host:                 " ++ show (remoteHost req)
  putStrLn $ "Body:"
  do
    body <- strictRequestBody req
    BSLC.putStrLn body
  putStr $ "Response:"
#else
  do
    time <- Time.getCurrentTime
    putStr $ Time.formatTime Time.defaultTimeLocale "[%F %T%3Q %Z] " time
  putStr $ "<" ++ show (remoteHost req) ++ "> "
  putStr requestString
#endif

  -- Refuse to fulfill request if `..` is found in the path
  if ".." `elem` pathInfoReq then do
    putStrLn " -> [403]"
    respond $ responseLBS
      status403
      [("Content-Type", "text/plain")]
      "Forbidden (suspicious request)"
  else do
    let convert = hhostConv settings
    a <- first (fmap ($ requestString) (hhostPath settings)) doesFileExist
    case a of
      Nothing  -> serve404  respond (hhost404 settings)
      Just res -> serveFile respond convert res

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

  putStr $ "Hosting folder " ++ hhostRoot settings ++ " on " ++ ppHostPreference (hhostHost settings) ++ ":" ++ show (hhostPort settings)
  do
    time <- Time.getCurrentTime
    putStrLn $ Time.formatTime Time.defaultTimeLocale " [%F %T%3Q %Z] " time

  Warp.runSettings (Warp.setHost (hhostHost settings) (Warp.setPort (hhostPort settings) Warp.defaultSettings)) (app settings)
