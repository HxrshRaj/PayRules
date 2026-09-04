{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Property suite for PayRules. Every property below runs on random inputs via
-- the 'Arbitrary' instances in "PayRules.Gen" -- none is an example test in
-- disguise. Grouped as: money algebra, money \<-\> text, and the engine.
module Main (main) where

import           Control.Monad   (unless)
import           Data.Aeson      (eitherDecode, encode)
import           Data.List       (sort)
import qualified Data.Set        as Set
import qualified Data.Text       as T
import           Numeric.Natural (Natural)
import           System.Exit     (exitFailure)
import           Test.QuickCheck hiding (scale)   -- 'scale' clashes with PayRules.Money.scale

import           PayRules.Engine
import           PayRules.Money
import           PayRules.Rules
import           PayRules.Sample (demoContext)
import           PayRules.Types
import           PayRules.Wire   (AuthRequest (..), AuthResponse (..), authorize)

import           PayRules.Gen    ()

-- ===========================================================================
-- Money: exact arithmetic
-- ===========================================================================

-- 'add' with 'zero' is a commutative monoid, and the 'Monoid' instance agrees.

prop_addAssoc :: Money 'USD -> Money 'USD -> Money 'USD -> Property
prop_addAssoc a b c = (a `add` b) `add` c === a `add` (b `add` c)

prop_addCommutes :: Money 'USD -> Money 'USD -> Property
prop_addCommutes a b = a `add` b === b `add` a

prop_zeroIsIdentity :: Money 'USD -> Property
prop_zeroIsIdentity a = (a `add` zero === a) .&&. (zero `add` a === a)

prop_mconcatIsSum :: [Money 'USD] -> Property
prop_mconcatIsSum ms = mconcat ms === foldr add zero ms

-- 'sub' is the inverse of 'add'; no value is lost on the round trip.
prop_subInvertsAdd :: Money 'USD -> Money 'USD -> Property
prop_subInvertsAdd a b = (a `add` b) `sub` b === a

-- Scaling by an integer is exact: the minor-unit count is multiplied exactly,
-- for arbitrarily large scalars (this is the "never silently loses precision /
-- never overflows" property -- it would fail immediately for Int64 or Double).
prop_scaleIsExact :: Integer -> Money 'USD -> Property
prop_scaleIsExact k m = toMinorUnits (scale k m) === k * toMinorUnits m

prop_scaleDistributes :: Integer -> Money 'USD -> Money 'USD -> Property
prop_scaleDistributes k a b =
  scale k (a `add` b) === (scale k a `add` scale k b)

-- ===========================================================================
-- Money: splitting is total and conserves the whole
-- ===========================================================================

-- The sum of the parts equals the original amount, always -- you cannot
-- create or destroy a minor unit by splitting.
-- Weights: keep the random shape (zeros inside the list are interesting -- a
-- part that gets nothing), but if they sum to zero substitute all-ones so the
-- call is always well-defined rather than discarded.
usableWeights :: [NonNegative Integer] -> [Natural]
usableWeights raw =
  let ws = map (fromInteger . getNonNegative) raw
  in if sum ws == 0 then map (const 1) ws else ws

prop_allocateConservesSum
  :: Money 'USD -> NonEmptyList (NonNegative Integer) -> Property
prop_allocateConservesSum m (NonEmpty raw) =
  (foldr add zero <$> allocate m (usableWeights raw)) === Right m

prop_allocateConservesCount
  :: Money 'USD -> NonEmptyList (NonNegative Integer) -> Property
prop_allocateConservesCount m (NonEmpty raw) =
  let ws = usableWeights raw
  in (length <$> allocate m ws) === Right (length ws)

-- With equal weights, no two parts differ by more than one minor unit: the
-- remainder is spread, not dumped on one part.
prop_allocateEqualWeightsAreFair :: Money 'USD -> Property
prop_allocateEqualWeightsAreFair m =
  forAll (choose (1, 64)) $ \n ->
    case allocate m (replicate n 1) of
      Left err    -> counterexample (show err) False
      Right parts ->
        let xs = map toMinorUnits parts
        in property (maximum xs - minimum xs <= 1)

-- Even with unequal weights, every part is within one minor unit of its exact
-- proportional share:  |part_i * totalWeight  -  total * weight_i|  <=  totalWeight.
prop_allocateIsProportional
  :: Money 'USD -> NonEmptyList (NonNegative Integer) -> Property
prop_allocateIsProportional m (NonEmpty raw) =
  case allocate m ws of
    Left err    -> counterexample (show err) False
    Right parts ->
      let total = toMinorUnits m
          w     = sum (map toInteger ws)
      in property $ and
           [ abs (toMinorUnits p * w - total * toInteger wi) <= w
           | (p, wi) <- zip parts ws ]
  where
    ws = usableWeights raw

-- ===========================================================================
-- Money <-> text round trips
-- ===========================================================================

prop_amountRoundTripsUSD :: Money 'USD -> Property
prop_amountRoundTripsUSD m = fromDecimal (renderAmount m) === Right m

prop_amountRoundTripsJPY :: Money 'JPY -> Property
prop_amountRoundTripsJPY m = fromDecimal (renderAmount m) === Right m

prop_parseSomeMoneyRoundTrips :: Money 'USD -> Property
prop_parseSomeMoneyRoundTrips m =
  parseSomeMoney (render m) === Right (SomeMoney m)

-- Over-precise input is rejected, never rounded: "<n>.123" has three
-- fractional digits and USD allows two.
prop_fromDecimalRefusesToRound :: Integer -> Bool
prop_fromDecimalRefusesToRound n =
  case fromDecimal (T.pack (show (abs n)) <> ".123") :: Either MoneyError (Money 'USD) of
    Left (TooManyFractionalDigits _ 2) -> True
    _                                  -> False

-- ===========================================================================
-- Engine
-- ===========================================================================

evalUSD :: AuthContext 'USD -> Transaction 'USD -> AuthResult
evalUSD = evaluate defaultRules

-- Declined exactly when there is at least one violation.
prop_declinedIffViolations :: AuthContext 'USD -> Transaction 'USD -> Property
prop_declinedIffViolations ctx txn =
  let r = evalUSD ctx txn
  in (decision r == Declined) === not (null (violations r))

-- Approved exactly when no individual rule objects (run each alone).
prop_approvedMeansEveryRulePasses :: AuthContext 'USD -> Transaction 'USD -> Property
prop_approvedMeansEveryRulePasses ctx txn =
  (decision (evalUSD ctx txn) == Approved)
    === all (\r -> isPass (runRule r ctx txn)) (defaultRules :: [NamedRule 'USD])

-- The reported violations are exactly the failures in the trail, in order.
prop_violationsMatchTrail :: AuthContext 'USD -> Transaction 'USD -> Property
prop_violationsMatchTrail ctx txn =
  let r = evalUSD ctx txn
  in violations r === [ v | (_, Fail v) <- trail r ]

-- Every decline reason is a non-empty string.
prop_everyReasonNonEmpty :: AuthContext 'USD -> Transaction 'USD -> Bool
prop_everyReasonNonEmpty ctx txn =
  all (not . T.null . violationReason) (violations (evalUSD ctx txn))

-- Same input, same output -- always.
prop_deterministic :: AuthContext 'USD -> Transaction 'USD -> Property
prop_deterministic ctx txn = evalUSD ctx txn === evalUSD ctx txn

-- The decision and the set of violated rules do not depend on rule order:
-- the accumulate-all engine treats rules as independent.
prop_ruleOrderIrrelevant :: AuthContext 'USD -> Transaction 'USD -> Property
prop_ruleOrderIrrelevant ctx txn =
  -- 'forAllBlind' because a list of rules (functions) has no 'Show' instance.
  forAllBlind (shuffle (defaultRules :: [NamedRule 'USD])) $ \shuffled ->
    let base = evalUSD ctx txn
        perm = evaluate shuffled ctx txn
    in decision base === decision perm
       .&&. sort (violatedRules base) === sort (violatedRules perm)
  where
    violatedRules = map violationRule . violations

-- Metamorphic: putting the transaction's account on the blocklist forces a
-- decline, whatever else is true.
prop_blocklistedAccountAlwaysDeclined
  :: AuthContext 'USD -> Transaction 'USD -> Property
prop_blocklistedAccountAlwaysDeclined ctx0 txn =
  decision (evalUSD ctx txn) === Declined
  where
    bl   = ctxBlocklist ctx0
    ctx  = ctx0 { ctxBlocklist =
                    bl { blockedAccounts =
                           Set.insert (txnAccount txn) (blockedAccounts bl) } }

-- Metamorphic: tightening the blocklist can never turn a decline into an
-- approval.
prop_tighteningNeverApproves
  :: AuthContext 'USD -> Transaction 'USD -> Bool
prop_tighteningNeverApproves ctx0 txn =
  not (decision base == Declined && decision tighter == Approved)
  where
    base = evalUSD ctx0 txn
    bl   = ctxBlocklist ctx0
    ctx' = ctx0 { ctxBlocklist =
                    bl { blockedMerchants =
                           Set.insert (merchantId (txnMerchant txn))
                                      (blockedMerchants bl) } }
    tighter = evalUSD ctx' txn

-- The hard ceiling: an amount at or above ctxAmountCeiling is always declined.
prop_atOrAboveCeilingIsDeclined
  :: AuthContext 'USD -> Transaction 'USD -> Bool
prop_atOrAboveCeilingIsDeclined ctx0 txn =
  decision (evalUSD (ctx0 { ctxAmountCeiling = txnAmount txn }) txn) == Declined

-- ...and an amount strictly below the ceiling never triggers the ceiling rule.
prop_belowCeilingHasNoCeilingViolation
  :: AuthContext 'USD -> Transaction 'USD -> Bool
prop_belowCeilingHasNoCeilingViolation ctx0 txn =
  let ctx = ctx0 { ctxAmountCeiling = txnAmount txn `add` fromMinorUnits 1 }
  in AmountCeiling `notElem` map violationRule (violations (evalUSD ctx txn))

-- Metamorphic: raising the ceiling can never *add* an AmountCeiling violation.
prop_raisingCeilingNeverAddsCeilingViolation
  :: AuthContext 'USD -> Transaction 'USD -> NonNegative Integer -> Bool
prop_raisingCeilingNeverAddsCeilingViolation ctx0 txn (NonNegative delta) =
  not (bumped && not base)
  where
    base   = AmountCeiling `elem` rulesOf (evalUSD ctx0 txn)
    bumped = AmountCeiling `elem` rulesOf (evalUSD ctx' txn)
    ctx'   = ctx0 { ctxAmountCeiling =
                      ctxAmountCeiling ctx0 `add` fromMinorUnits delta }
    rulesOf = map violationRule . violations

-- Metamorphic: raising the per-transaction limit can never *add* a
-- spending-limit violation.
prop_raisingLimitNeverAddsLimitViolation
  :: AuthContext 'USD -> Transaction 'USD -> NonNegative Integer -> Bool
prop_raisingLimitNeverAddsLimitViolation ctx0 txn (NonNegative delta) =
  not (bumpedHasLimitViolation && not baseHasLimitViolation)
  where
    baseHasLimitViolation   = SpendingLimit `elem` rulesOf (evalUSD ctx0 txn)
    bumpedHasLimitViolation = SpendingLimit `elem` rulesOf (evalUSD ctx' txn)
    acct = ctxAccount ctx0
    ctx' = ctx0 { ctxAccount =
                    acct { accountPerTxnLimit =
                             accountPerTxnLimit acct `add` fromMinorUnits delta } }
    rulesOf = map violationRule . violations

-- ===========================================================================
-- HTTP layer (PayRules.Wire) -- the /authorize endpoint's pure core
-- ===========================================================================

-- The JSON request equivalent of a typed transaction (currency fixed to USD,
-- amount as exact minor units, category via its Show name).
requestOf :: Transaction 'USD -> AuthRequest
requestOf txn = AuthRequest
  { reqAccount          = unAccountId (txnAccount txn)
  , reqCurrency         = "USD"
  , reqMinorUnits       = toMinorUnits (txnAmount txn)
  , reqMerchantId       = unMerchantId (merchantId (txnMerchant txn))
  , reqMerchantName     = merchantName (txnMerchant txn)
  , reqMerchantCategory = T.pack (show (merchantCategory (txnMerchant txn)))
  , reqTimestamp        = Just (txnTimestamp txn)
  }

-- Going through the JSON request type reaches the same decision as calling the
-- engine directly on the equivalent transaction (both against demoContext).
prop_apiMatchesEngine :: Transaction 'USD -> Property
prop_apiMatchesEngine txn =
  case authorize (txnTimestamp txn) (requestOf txn) of
    Left e     -> counterexample (T.unpack e) False
    Right resp -> respDecision resp === engineWord
  where
    engineWord =
      if decision (evaluate defaultRules (demoContext :: AuthContext 'USD) txn) == Approved
        then "approved" else "declined"

-- The response survives a JSON encode/decode round trip.
prop_authResponseJsonRoundTrips :: Transaction 'USD -> Property
prop_authResponseJsonRoundTrips txn =
  case authorize (txnTimestamp txn) (requestOf txn) of
    Left e     -> counterexample (T.unpack e) False
    Right resp -> eitherDecode (encode resp) === Right resp

-- ===========================================================================
-- Harness
-- ===========================================================================

main :: IO ()
main = do
  results <- sequence
    [ check "add is associative"                         prop_addAssoc
    , check "add commutes"                               prop_addCommutes
    , check "zero is the additive identity"              prop_zeroIsIdentity
    , check "mconcat == iterated add"                    prop_mconcatIsSum
    , check "sub inverts add"                            prop_subInvertsAdd
    , check "scale is exact for any scalar"              prop_scaleIsExact
    , check "scale distributes over add"                 prop_scaleDistributes
    , check "allocate conserves the total"              prop_allocateConservesSum
    , check "allocate conserves the part count"         prop_allocateConservesCount
    , check "allocate spreads the remainder fairly"     prop_allocateEqualWeightsAreFair
    , check "allocate is proportional to within 1 unit" prop_allocateIsProportional
    , check "renderAmount/fromDecimal round-trip (USD)" prop_amountRoundTripsUSD
    , check "renderAmount/fromDecimal round-trip (JPY)" prop_amountRoundTripsJPY
    , check "render/parseSomeMoney round-trip"          prop_parseSomeMoneyRoundTrips
    , check "fromDecimal refuses to round"              prop_fromDecimalRefusesToRound
    , check "declined iff there are violations"         prop_declinedIffViolations
    , check "approved => every rule passes alone"       prop_approvedMeansEveryRulePasses
    , check "violations are exactly the trail fails"    prop_violationsMatchTrail
    , check "every decline reason is non-empty"         prop_everyReasonNonEmpty
    , check "evaluation is deterministic"               prop_deterministic
    , check "rule order does not change the result"     prop_ruleOrderIrrelevant
    , check "blocklisted account is always declined"    prop_blocklistedAccountAlwaysDeclined
    , check "tightening never turns decline into approve" prop_tighteningNeverApproves
    , check "raising the limit never adds a limit violation"
                                                        prop_raisingLimitNeverAddsLimitViolation
    , check "amount at/above the hard ceiling is declined" prop_atOrAboveCeilingIsDeclined
    , check "amount below the ceiling never trips it"    prop_belowCeilingHasNoCeilingViolation
    , check "raising the ceiling never adds a ceiling violation"
                                                        prop_raisingCeilingNeverAddsCeilingViolation
    , check "the HTTP layer reaches the engine's decision" prop_apiMatchesEngine
    , check "the HTTP response JSON round-trips"         prop_authResponseJsonRoundTrips
    ]
  unless (and results) exitFailure

check :: Testable p => String -> p -> IO Bool
check name p = do
  putStrLn ("=== " <> name)
  isSuccess <$> quickCheckResult p
