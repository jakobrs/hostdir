module Opts
  ( optDescrs
  , parseOpts
  , applyOpts
  ) where

import Control.Monad
import Data.String
import System.Console.GetOpt
import System.IO
import Text.Read

import Settings

-- Endomorphism in the Kleisli category generated by `Either String`
newtype Opt = Opt { runOpt :: Settings -> Either String Settings }
instance Semigroup Opt where
  Opt a <> Opt b = Opt (b >=> a)
instance Monoid Opt where
  mempty = Opt pure

optDescrs :: [OptDescr Opt]
optDescrs =
    [ Option "p" ["port"] portArg "What port to listen on"
    , Option "h" ["host"] hostArg "What host to listen on"
    , Option ""  ["path"] pathArg "TODO: add description of --path"
    , Option "r" ["root"] rootArg "Root folder"
    , Option ""  ["help"] helpArg "Show command usage"
    , Option ""  ["404"]  _404Arg "404 page"
    , Option "V" ["version", "ver"] verArg "Show version"
    , Option "B" ["no-buffer"] nobArg "Don't buffer output"
    , Option "l" ["buffer-line", "buffer"] blArg "Use line buffering (default)"
    , Option "b" ["buffer-block"] bbArg "Use block buffering"
    ]
  where
    portArg, hostArg, pathArg, rootArg, _404Arg, verArg, nobArg, blArg, bbArg :: ArgDescr Opt
    portArg = ReqArg portHandler                                                         ""
    hostArg = ReqArg (\host -> Opt (\set -> pure (set { hhostHost = fromString host }))) ""
    rootArg = ReqArg (\root -> Opt (\set -> pure (set { hhostRoot = root })))            ""
    _404Arg = ReqArg (\_404 -> Opt (\set -> pure (set { hhost404  = _404 })))            ""
    pathArg = ReqArg (error "--path: NYI")                                               ""
    helpArg = NoArg (Opt (\set -> pure (set { hhostHelp = True })))
    verArg  = NoArg (Opt (\set -> pure (set { hhostVer  = True })))
    nobArg  = NoArg (Opt (\set -> pure (set { hhostBuf  = NoBuffering })))
    blArg   = NoArg (Opt (\set -> pure (set { hhostBuf  = LineBuffering })))
    bbArg   = OptArg bbHandler ""

    bbHandler :: Maybe String -> Opt
    bbHandler Nothing = Opt (\set -> pure (set { hhostBuf = BlockBuffering Nothing }))
    bbHandler (Just bs) = Opt $ \set -> case readMaybe bs of
      Nothing -> Left $ "Block size doesn't parse: " ++ show bs
      Just bs -> pure $ set { hhostBuf = BlockBuffering (Just bs) }

    portHandler :: String -> Opt
    portHandler str = Opt $ \set -> case readMaybe str of
      Nothing            -> Left $ "Port doesn't parse: " ++ show str
      Just n | n < 0     -> Left $ "Port less than 0: " ++ show n
             | n > 65535 -> Left $ "Port greater than 65535: " ++ show n
             | otherwise -> pure $ set { hhostPort = n }

parseOpts :: [String] -> ([Opt], [String], [String])
parseOpts = getOpt Permute optDescrs

applyOpts :: [String] -> Settings -> Either [String] (Settings, [String])
applyOpts args set =
  let (perm, nonopts, errors) = parseOpts args
  in case errors of
    [] -> case runOpt (mconcat perm) set of
      Left err  -> Left [err]
      Right res -> Right (res, nonopts)
    errs -> Left errs
