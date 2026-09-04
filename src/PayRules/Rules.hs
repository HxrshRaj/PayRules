{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : PayRules.Rules
-- Description : The six authorization rules.
--
-- Each rule is a pure function @'AuthContext' c -> 'Transaction' c ->
-- 'RuleOutcome'@ wrapped with its name in a 'NamedRule'. Rules never look at
-- each other and never short-circuit each other; the engine
-- ('PayRules.Engine.evaluate') runs them all and collects every 'Violation',
-- so a decline reports /all/ the reasons, not just the first.
--
-- Every 'Fail' carries a non-empty, human-readable reason built from the
-- actual amounts / counts involved — that is the "reasoning trail".
module PayRules.Rules
  ( -- * The rule set
    defaultRules

    -- * Individual rules
  , spendingLimitRule
  , amountCeilingRule
  , currencyAllowedRule
  , velocityRule
  , fraudPatternRule
  , blocklistRule

    -- * Helper (exported for testing)
  , isRoundAmount
  ) where

import           Data.List  (sort)
import qualified Data.Set   as Set
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Data.Time  (diffUTCTime)

import           PayRules.Money
import           PayRules.Types

-- | The six rules in reporting order. Order does not affect the decision
-- (the engine accumulates all violations); it only affects the order lines
-- appear in the trail.
defaultRules :: KnownCurrency c => [NamedRule c]
defaultRules =
  [ spendingLimitRule
  , amountCeilingRule
  , currencyAllowedRule
  , velocityRule
  , fraudPatternRule
  , blocklistRule
  ]

-- ---------------------------------------------------------------------------
-- 1. Spending limit
-- ---------------------------------------------------------------------------

-- | Decline when the amount exceeds the account's per-transaction limit.
-- Both sides are @'Money' c@ for the /same/ @c@, so the comparison needs no
-- currency handling.
spendingLimitRule :: KnownCurrency c => NamedRule c
spendingLimitRule = NamedRule SpendingLimit $ \ctx txn ->
  let limit  = accountPerTxnLimit (ctxAccount ctx)
      amount = txnAmount txn
  in if amount > limit
       then fed SpendingLimit $
              "amount " <> render amount
                <> " exceeds the per-transaction limit of " <> render limit
       else Pass

-- ---------------------------------------------------------------------------
-- 1b. Hard amount ceiling
-- ---------------------------------------------------------------------------

-- | Decline when the amount is at or above the absolute ceiling in
-- 'ctxAmountCeiling'. This is the ledger-edge maximum the 'Money' type does not
-- encode ('Integer' is unbounded); it is a backstop that catches an amount even
-- if the per-account 'spendingLimitRule' is misconfigured or absent. Note the
-- comparison is @>=@ (the ceiling itself is out of range), unlike the
-- per-transaction limit's @>@.
amountCeilingRule :: KnownCurrency c => NamedRule c
amountCeilingRule = NamedRule AmountCeiling $ \ctx txn ->
  let ceilingAmount = ctxAmountCeiling ctx
      amount        = txnAmount txn
  in if amount >= ceilingAmount
       then fed AmountCeiling $
              "amount " <> render amount
                <> " is at or above the hard ceiling of " <> render ceilingAmount
       else Pass

-- ---------------------------------------------------------------------------
-- 2. Currency validation
-- ---------------------------------------------------------------------------

-- | Decline when the transaction's currency is not in the account's allowed
-- set. The currency is recovered from the type via 'currencyOf'.
currencyAllowedRule :: KnownCurrency c => NamedRule c
currencyAllowedRule = NamedRule CurrencyAllowed $ \ctx txn ->
  let cur     = currencyOf (txnAmount txn)
      allowed = accountAllowedCurrencies (ctxAccount ctx)
  in if cur `Set.member` allowed
       then Pass
       else fed CurrencyAllowed $
              currencyCode cur <> " is not an allowed currency for this account"
                <> " (allowed: " <> renderSet (Set.map currencyCode allowed) <> ")"

-- ---------------------------------------------------------------------------
-- 3. Velocity
-- ---------------------------------------------------------------------------

-- | Decline when the number of prior transactions in the window immediately
-- before this one reaches the policy maximum. "Prior" means strictly earlier
-- than 'txnTimestamp'; the window is measured back from that instant.
velocityRule :: NamedRule c
velocityRule = NamedRule Velocity $ \ctx txn ->
  let policy  = ctxVelocity ctx
      now     = txnTimestamp txn
      window  = velocityWindow policy
      maxN    = velocityMaxPriorTxns policy
      -- seconds, via Double, to avoid depending on RealFrac NominalDiffTime
      windowSeconds = round (realToFrac window :: Double) :: Integer
      inWindow h =
        let t = txnTimestamp h
        in t < now && diffUTCTime now t <= window
      recent  = length (filter inWindow (ctxHistory ctx))
  in if recent >= maxN
       then fed Velocity $
              T.pack (show recent) <> " prior transactions within "
                <> T.pack (show windowSeconds) <> "s"
                <> " reaches the limit of " <> T.pack (show maxN)
       else Pass

-- ---------------------------------------------------------------------------
-- 4. Fraud pattern: suspicious round amount to a first-seen merchant
-- ---------------------------------------------------------------------------

-- | Decline when /all three/ hold: the amount is at least
-- 'fraudMinAmount', the amount is "round" (see 'isRoundAmount'), and the
-- merchant has not been seen in this account's history. Any one alone is
-- unremarkable; together they are the classic card-testing / bust-out shape.
fraudPatternRule :: KnownCurrency c => NamedRule c
fraudPatternRule = NamedRule FraudPattern $ \ctx txn ->
  let policy     = ctxFraud ctx
      amount     = txnAmount txn
      big        = amount >= fraudMinAmount policy
      roundish   = isRoundAmount (fraudRoundMajorModulus policy) amount
      seen       = Set.fromList
                     [ merchantId (txnMerchant h) | h <- ctxHistory ctx ]
      firstSeen  = merchantId (txnMerchant txn) `Set.notMember` seen
  in if big && roundish && firstSeen
       then fed FraudPattern $
              "round amount " <> render amount
                <> " to first-seen merchant " <> quote (merchantName (txnMerchant txn))
                <> " matches a suspicious pattern"
       else Pass

-- | Is @m@ a whole number of major units divisible by @modulusMajor@? E.g.
-- with a modulus of 100: 100.00 and 2000.00 are round, 150.00 and 100.01 are
-- not. A non-positive modulus makes nothing round.
isRoundAmount :: KnownCurrency c => Integer -> Money c -> Bool
isRoundAmount modulusMajor m
  | modulusMajor <= 0 = False
  | otherwise         = toMinorUnits m `mod` modulusMinor == 0
  where
    e            = currencyExponent (currencyOf m)
    modulusMinor = modulusMajor * (10 ^ e)

-- ---------------------------------------------------------------------------
-- 5. Blocklist
-- ---------------------------------------------------------------------------

-- | Decline when the merchant or the account is on the blocklist. The
-- merchant is checked first so its reason wins when both match.
blocklistRule :: NamedRule c
blocklistRule = NamedRule BlocklistRule $ \ctx txn ->
  let bl  = ctxBlocklist ctx
      mid = merchantId (txnMerchant txn)
      aid = txnAccount txn
  in if mid `Set.member` blockedMerchants bl
       then fed BlocklistRule $
              "merchant " <> quote (unMerchantId mid) <> " is blocklisted"
     else if aid `Set.member` blockedAccounts bl
       then fed BlocklistRule $
              "account " <> quote (unAccountId aid) <> " is blocklisted"
     else Pass

-- ---------------------------------------------------------------------------
-- Small helpers
-- ---------------------------------------------------------------------------

-- | Build a failing outcome. Named @fed@ ("fail-ed") to keep the rule bodies
-- lined up; the reason is always a non-empty string at every call site.
fed :: RuleName -> Text -> RuleOutcome
fed n msg = Fail (Violation n msg)

quote :: Text -> Text
quote t = "'" <> t <> "'"

renderSet :: Set.Set Text -> Text
renderSet = T.intercalate ", " . sort . Set.toList
