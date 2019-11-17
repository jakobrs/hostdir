{-# LANGUAGE ScopedTypeVariables #-}

module Opts
  ( optDescrs
  , parseOpts
  , applyOpts
  ) where

import Data.Monoid
import Data.Semigroup hiding (Option)
import System.Console.GetOpt
import Text.Read

import Settings

-- Endomorphism in the Kleisli category generated by `Either String`
newtype Opt = Opt { runOpt :: Settings -> Either String Settings }
instance Semigroup Opt where
  Opt a <> Opt b = Opt (\x -> b x >>= a)
instance Monoid Opt where
  mempty = Opt pure

optDescrs :: [OptDescr Opt]
optDescrs =
    [ Option "p" ["port"] portArg "What port to listen on"
    , Option "h" ["host"] hostArg "What host to listen on"
    , Option ""  ["path"] pathArg "Set path"
    , Option "r" ["root"] rootArg "Root folder"
    , Option ""  ["help"] helpArg "Show command usage"
    , Option ""  ["404"]  _404Arg "404 page"
    , Option "V" ["version", "ver"] verArg "Show version"
    ]
  where
    portArg, hostArg, pathArg, rootArg, _404Arg, verArg :: ArgDescr Opt
    portArg = ReqArg portHandler                                              ""
    pathArg = ReqArg pathHandler                                              ""
    hostArg = ReqArg (\host -> Opt (\set -> pure (set { hhostHost = host }))) ""
    rootArg = ReqArg (\root -> Opt (\set -> pure (set { hhostRoot = root }))) ""
    _404Arg = ReqArg (\_404 -> Opt (\set -> pure (set { hhost404  = _404 }))) ""
    helpArg = NoArg (Opt (\set -> pure (set { hhostHelp = True })))
    verArg  = NoArg (Opt (\set -> pure (set { hhostVer  = True })))

    portHandler, pathHandler :: String -> Opt
    portHandler str = Opt $ \set -> case readMaybe str of
      Nothing            -> Left $ "Port doesn't parse: " ++ show str
      Just n | n < 0     -> Left $ "Port less than 0: " ++ show n
             | n > 65535 -> Left $ "Port greater than 65535: " ++ show n
             | otherwise -> pure (set { hhostPort = n })

    pathHandler str = Opt $ \set -> pure $
      set { hhostPath = map (replace '%') (splitAt ':' str) }

    -- Split at character, considering backslashes
    splitAt :: Char -> String -> [String]
    splitAt r = go
      where
        go :: String -> [String]
        go "" = [""]
        go (       ch : chs) | ch == r = "" : go chs
        go ('\\' : ch : chs) =
          let rh : rt = go chs
          in  (ch : rh) : rt
        go (ch : chs) =
          let rh : rt = go chs
          in  (ch : rh) : rt

    replace :: forall a. Eq a
            => a   -- ^ Character to replace
            -> [a] -- ^ List to replace in
            -> [a] -- ^ Replacement
            -> [a]
    replace ch a r = go a
      where
        go :: [a] -> [a]
        go []                   = []
        go (x : xs) | x == ch   = r ++ go xs
                    | otherwise = x :  go xs

parseOpts :: [String] -> ([Opt], [String], [String])
parseOpts args = getOpt Permute optDescrs args

applyOpts :: [String] -> Settings -> Either [String] (Settings, [String])
applyOpts args set =
  let (perm, nonopts, errors) = parseOpts args
  in case errors of
    [] -> case runOpt (mconcat perm) set of
      Left err  -> Left [err]
      Right res -> Right (res, nonopts)
    errs -> Left errs
