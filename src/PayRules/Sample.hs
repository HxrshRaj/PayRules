{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : PayRules.Sample
-- Description : A worked example context plus the scenarios the CLI shows.
--
-- This module is demo scaffolding, not part of the engine. It fixes one
-- plausible account ('demoContext') and a handful of transactions, each
-- crafted to exercise exactly one rule (plus one that trips two), so
-- @payrules demo@ prints a readable tour of the engine.
module PayRules.Sample
  ( demoContext
  , demoResults
  , evaluateLine
  ) where

import           Data.Bifunctor (first)
import qualified Data.Set       as Set
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Time      (UTCTime (..), addUTCTime, fromGregorian,
                                 secondsToDiffTime)
import           Numeric.Natural (Natural)

import           PayRules.Engine
import           PayRules.Money
import           PayRules.Rules
import           PayRules.Types

-- ---------------------------------------------------------------------------
-- The example account
-- ---------------------------------------------------------------------------

-- | One account, currency-polymorphic so the same policy can be viewed in
-- USD or GBP for the demo. Per-transaction limit 1,000; USD and EUR allowed;
-- at most 3 transactions per 60s; round amounts of 500+ to a new merchant are
-- suspicious; one merchant and one account are blocklisted.
demoContext :: KnownCurrency c => AuthContext c
demoContext = AuthContext
  { ctxAccount = Account
      { accountId                = AccountId "acc-001"
      , accountPerTxnLimit       = lit 1000 0
      , accountAllowedCurrencies = Set.fromList [USD, EUR]
      }
  , ctxHistory =
      [ historyTxn "h1" mCoffee   0
      , historyTxn "h2" mCoffee   30
      , historyTxn "h3" mGrocery  45
      , historyTxn "h4" mCoffee   50
      ]
  , ctxVelocity = VelocityPolicy
      { velocityWindow       = 60
      , velocityMaxPriorTxns = 3
      }
  , ctxFraud = FraudPolicy
      { fraudMinAmount         = lit 500 0
      , fraudRoundMajorModulus = 100
      }
  , ctxBlocklist = Blocklist
      { blockedMerchants = Set.fromList [MerchantId "mch-block"]
      , blockedAccounts  = Set.fromList [AccountId "acc-999"]
      }
  }

-- ---------------------------------------------------------------------------
-- Merchants and a time base
-- ---------------------------------------------------------------------------

mCoffee, mGrocery, mNew, mBlocked :: Merchant
mCoffee  = Merchant (MerchantId "mch-coffee")   "Blue Bottle"   Entertainment
mGrocery = Merchant (MerchantId "mch-grocery")  "Whole Foods"   Grocery
mNew     = Merchant (MerchantId "mch-new")      "Unknown Store" Other
mBlocked = Merchant (MerchantId "mch-block")    "QuickCash Ltd" CashAdvance

-- | 2026-06-01T00:00:00Z, the instant history is measured from.
timeBase :: UTCTime
timeBase = UTCTime (fromGregorian 2026 6 1) (secondsToDiffTime 0)

at :: Integer -> UTCTime
at secs = addUTCTime (fromInteger secs) timeBase

historyTxn :: KnownCurrency c => Text -> Merchant -> Integer -> Transaction c
historyTxn tid merch secs = Transaction
  { txnId        = tid
  , txnAccount   = AccountId "acc-001"
  , txnAmount    = lit 5 0
  , txnMerchant  = merch
  , txnTimestamp = at secs
  }

-- ---------------------------------------------------------------------------
-- Scenarios
-- ---------------------------------------------------------------------------

-- | Named scenarios and the decision the engine reaches for each. The names
-- say which rule is meant to fire; running it confirms the engine agrees.
demoResults :: [(Text, AuthResult)]
demoResults =
  [ ( "clean purchase -> approved"
    , evaluate defaultRules ctxUSD (usdTxn "t1" mCoffee 42 50 (at 100000)) )
  , ( "amount over the per-transaction limit -> declined"
    , evaluate defaultRules ctxUSD (usdTxn "t2" mGrocery 5000 0 (at 100001)) )
  , ( "currency not allowed for the account (GBP) -> declined"
    , evaluate defaultRules ctxGBP (gbpTxn "t3" mGrocery 50 0 (at 100002)) )
  , ( "too many transactions in the velocity window -> declined"
    , evaluate defaultRules ctxUSD (usdTxn "t4" mCoffee 12 00 (at 55)) )
  , ( "round amount to a first-seen merchant -> declined"
    , evaluate defaultRules ctxUSD (usdTxn "t5" mNew 800 0 (at 100003)) )
  , ( "merchant on the blocklist -> declined"
    , evaluate defaultRules ctxUSD (usdTxn "t6" mBlocked 20 0 (at 100004)) )
  , ( "blocklisted account AND over the limit -> declined, two reasons"
    , evaluate defaultRules ctxUSD (usdTxnFor "acc-999" "t7" mCoffee 5000 0 (at 100005)) )
  ]
  where
    ctxUSD = demoContext :: AuthContext 'USD
    ctxGBP = demoContext :: AuthContext 'GBP

-- ---------------------------------------------------------------------------
-- One line of stdin -> a decision
-- ---------------------------------------------------------------------------

-- | Parse a @|@-delimited line and authorize it against 'demoContext' at
-- whichever currency the amount declares. This is where a runtime-only
-- currency ('parseSomeMoney' -> 'SomeMoney') crosses back into the typed
-- engine via 'withSomeMoney'.
--
-- Format: @account | CUR amount | merchantId | merchantName | category@
-- e.g. @acc-001 | USD 800.00 | mch-x | Some Shop | Other@
evaluateLine :: UTCTime -> Text -> Either Text AuthResult
evaluateLine now raw =
  case map T.strip (T.splitOn "|" raw) of
    [acct, amountText, mid, mname, catText] -> do
      some <- first (T.pack . show) (parseSomeMoney amountText)
      cat  <- parseCategory catText
      let merch = Merchant (MerchantId mid) mname cat
      withSomeMoney some (\amount ->
        Right (evaluate defaultRules demoContext
                 (Transaction "stdin" (AccountId acct) amount merch now)))
    _ -> Left
      "expected 5 '|'-separated fields: account | CUR amount | merchantId | merchantName | category"

parseCategory :: Text -> Either Text MerchantCategory
parseCategory t =
  case T.toLower (T.strip t) of
    "grocery"       -> Right Grocery
    "travel"        -> Right Travel
    "electronics"   -> Right Electronics
    "entertainment" -> Right Entertainment
    "gambling"      -> Right Gambling
    "cashadvance"   -> Right CashAdvance
    "other"         -> Right Other
    _               -> Left ("unknown merchant category: " <> t)

-- ---------------------------------------------------------------------------
-- Local helpers
-- ---------------------------------------------------------------------------

-- | A known-good money literal for demo data. The 'money' smart constructor
-- can only reject out-of-range minor components, and every call here passes a
-- valid one, so the 'error' branch is unreachable — it exists so a typo in
-- this file fails loudly rather than silently.
lit :: KnownCurrency c => Natural -> Natural -> Money c
lit major minor =
  either (error . ("PayRules.Sample: invalid money literal: " <>) . show) id
         (money major minor)

usdTxn :: Text -> Merchant -> Natural -> Natural -> UTCTime -> Transaction 'USD
usdTxn = usdTxnFor "acc-001"

usdTxnFor :: Text -> Text -> Merchant -> Natural -> Natural -> UTCTime -> Transaction 'USD
usdTxnFor acct tid merch major minor ts = Transaction
  { txnId        = tid
  , txnAccount   = AccountId acct
  , txnAmount    = lit major minor
  , txnMerchant  = merch
  , txnTimestamp = ts
  }

gbpTxn :: Text -> Merchant -> Natural -> Natural -> UTCTime -> Transaction 'GBP
gbpTxn tid merch major minor ts = Transaction
  { txnId        = tid
  , txnAccount   = AccountId "acc-001"
  , txnAmount    = lit major minor
  , txnMerchant  = merch
  , txnTimestamp = ts
  }
