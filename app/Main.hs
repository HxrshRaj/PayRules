{-# LANGUAGE OverloadedStrings #-}

-- | Thin CLI over the engine. The interesting code is the library; this just
-- wires stdin / a built-in sample set to 'PayRules.Engine.explain'.
module Main (main) where

import           Control.Monad     (forM_)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import           Data.Time         (getCurrentTime)
import           System.Environment (getArgs)
import           System.Exit       (exitFailure)
import           System.IO         (hPutStrLn, stderr)

import           PayRules.Engine   (explain)
import           PayRules.Sample   (demoResults, evaluateLine)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> runDemo
    ["demo"]   -> runDemo
    ["check"]  -> runCheck
    ["--help"] -> usage
    ["-h"]     -> usage
    _          -> usage >> exitFailure

-- | Evaluate every built-in scenario and print its reasoning trail.
runDemo :: IO ()
runDemo =
  forM_ demoResults $ \(name, result) -> do
    TIO.putStrLn ("### " <> name)
    TIO.putStr (explain result)
    TIO.putStrLn ""

-- | Read @|@-delimited transactions from stdin (blank lines and @#@ comments
-- ignored) and print a decision for each. Timestamp is "now", so the velocity
-- rule sees the sample history as far in the past.
runCheck :: IO ()
runCheck = do
  now      <- getCurrentTime
  contents <- TIO.getContents
  let rows = filter meaningful (map T.strip (T.lines contents))
  forM_ rows $ \row -> do
    TIO.putStrLn ("### " <> row)
    case evaluateLine now row of
      Left err  -> TIO.putStrLn ("  error: " <> err)
      Right res -> TIO.putStr (explain res)
    TIO.putStrLn ""
  where
    meaningful t = not (T.null t) && not ("#" `T.isPrefixOf` t)

usage :: IO ()
usage = mapM_ (hPutStrLn stderr)
  [ "payrules - evaluate a payment transaction against the authorization rules"
  , ""
  , "  payrules [demo]   run the built-in sample scenarios (default)"
  , "  payrules check    read '|'-delimited transactions from stdin, one per line"
  , "  payrules --help   this message"
  , ""
  , "stdin line format (for 'check'):"
  , "  account | CUR amount | merchantId | merchantName | category"
  , "  e.g.  acc-001 | USD 800.00 | mch-x | Some Shop | Other"
  , ""
  , "categories: Grocery Travel Electronics Entertainment Gambling CashAdvance Other"
  ]
