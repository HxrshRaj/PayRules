{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : PayRules.Gen
-- Description : 'Arbitrary' instances for the domain types.
--
-- The generators deliberately draw identifiers from /small pools/ so that
-- interesting coincidences happen often under random testing: the same
-- merchant appearing twice (so "first-seen merchant?" is exercised both
-- ways), an account or merchant that is also on the blocklist, and so on. A
-- generator that used fresh unique strings everywhere would make most of the
-- rules trivially pass.
module PayRules.Gen () where

import qualified Data.Set        as Set
import           Data.Text       (Text)
import           Data.Time       (UTCTime (..), addUTCTime, fromGregorian)
import           Test.QuickCheck

import           PayRules.Money
import           PayRules.Types

-- ---------------------------------------------------------------------------
-- Identifier pools
-- ---------------------------------------------------------------------------

accountIdPool :: [AccountId]
accountIdPool = map AccountId ["acc-001", "acc-002", "acc-777", "acc-999"]

merchantIdPool :: [MerchantId]
merchantIdPool = map MerchantId ["mch-a", "mch-b", "mch-c", "mch-d", "mch-block"]

merchantNamePool :: [Text]
merchantNamePool = ["Shop A", "Shop B", "QuickCash Ltd", "Corner Cafe"]

-- ---------------------------------------------------------------------------
-- Scalars
-- ---------------------------------------------------------------------------

instance Arbitrary Currency where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary MerchantCategory where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AccountId where
  arbitrary = elements accountIdPool

instance Arbitrary MerchantId where
  arbitrary = elements merchantIdPool

-- | Money over any currency: an exact minor-unit count, positive or negative,
-- shrinking toward zero.
instance Arbitrary (Money c) where
  arbitrary = fromMinorUnits <$> arbitrary
  shrink m  = fromMinorUnits <$> shrink (toMinorUnits m)

-- | A time within a few days of a fixed base instant.
instance Arbitrary UTCTime where
  arbitrary = do
    secs <- choose (0, 3 * 24 * 3600) :: Gen Integer
    pure (addUTCTime (fromInteger secs) epoch)
    where epoch = UTCTime (fromGregorian 2026 1 1) 0

-- ---------------------------------------------------------------------------
-- Records
-- ---------------------------------------------------------------------------

instance Arbitrary Merchant where
  arbitrary =
    Merchant <$> arbitrary <*> elements merchantNamePool <*> arbitrary

instance KnownCurrency c => Arbitrary (Transaction c) where
  arbitrary =
    Transaction
      <$> elements ["txn-1", "txn-2", "txn-3"]
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance KnownCurrency c => Arbitrary (Account c) where
  arbitrary =
    Account
      <$> arbitrary
      <*> (fromMinorUnits . getNonNegative <$> arbitrary)
      <*> (Set.fromList <$> sublistOf [minBound .. maxBound])

instance Arbitrary VelocityPolicy where
  arbitrary =
    VelocityPolicy
      <$> (fromInteger <$> choose (1, 3600))
      <*> choose (0, 6)

instance KnownCurrency c => Arbitrary (FraudPolicy c) where
  arbitrary =
    FraudPolicy
      <$> (fromMinorUnits . getNonNegative <$> arbitrary)
      <*> choose (1, 1000)

instance Arbitrary Blocklist where
  arbitrary =
    Blocklist
      <$> (Set.fromList <$> sublistOf merchantIdPool)
      <*> (Set.fromList <$> sublistOf accountIdPool)

instance KnownCurrency c => Arbitrary (AuthContext c) where
  arbitrary =
    AuthContext
      <$> arbitrary
      <*> resize 8 (listOf arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary   -- ctxAmountCeiling
