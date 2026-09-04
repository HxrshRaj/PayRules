{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Module      : PayRules.Types
-- Description : The domain: transactions, accounts, the authorization context,
--               and the vocabulary rules speak in.
--
-- Everything that carries an amount is parameterised by a currency @c@ (the
-- same phantom used by 'Money'), so a 'Transaction' and the 'Account' it is
-- checked against are guaranteed by the type checker to be in the same
-- currency. A rule can therefore compare @txnAmount@ to @accountPerTxnLimit@
-- directly, with no "and are these the same currency?" branch to forget.
module PayRules.Types
  ( -- * Identifiers
    AccountId (..)
  , MerchantId (..)

    -- * Merchants
  , MerchantCategory (..)
  , Merchant (..)

    -- * Transactions
  , Transaction (..)

    -- * Account and policies
  , Account (..)
  , VelocityPolicy (..)
  , FraudPolicy (..)
  , Blocklist (..)
  , emptyBlocklist

    -- * The authorization context
  , AuthContext (..)

    -- * Rule vocabulary
  , RuleName (..)
  , Violation (..)
  , RuleOutcome (..)
  , isPass
  , NamedRule (..)
  ) where

import           Data.Set   (Set)
import qualified Data.Set   as Set
import           Data.Text  (Text)
import           Data.Time  (NominalDiffTime, UTCTime)

import           PayRules.Money (Currency, Money)

-- ---------------------------------------------------------------------------
-- Identifiers
-- ---------------------------------------------------------------------------

newtype AccountId = AccountId { unAccountId :: Text }
  deriving (Eq, Ord, Show)

newtype MerchantId = MerchantId { unMerchantId :: Text }
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Merchants
-- ---------------------------------------------------------------------------

data MerchantCategory
  = Grocery
  | Travel
  | Electronics
  | Entertainment
  | Gambling
  | CashAdvance
  | Other
  deriving (Eq, Ord, Show, Enum, Bounded)

data Merchant = Merchant
  { merchantId       :: MerchantId
  , merchantName     :: Text
  , merchantCategory :: MerchantCategory
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Transactions
-- ---------------------------------------------------------------------------

-- | A single authorization request. @c@ is the transaction's currency.
data Transaction (c :: Currency) = Transaction
  { txnId        :: Text
  , txnAccount   :: AccountId
  , txnAmount    :: Money c
  , txnMerchant  :: Merchant
  , txnTimestamp :: UTCTime
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Account and policies
-- ---------------------------------------------------------------------------

data Account (c :: Currency) = Account
  { accountId                :: AccountId
  , accountPerTxnLimit       :: Money c
    -- ^ Largest single transaction the account may make.
  , accountAllowedCurrencies :: Set Currency
    -- ^ Currencies this account may transact in. Checked as a value: the
    -- transaction's currency is already fixed by its type, this says whether
    -- that currency is permitted for this account.
  }
  deriving (Eq, Show)

-- | "No more than 'velocityMaxPriorTxns' transactions in the
-- 'velocityWindow' immediately before the one being authorized."
data VelocityPolicy = VelocityPolicy
  { velocityWindow        :: NominalDiffTime
  , velocityMaxPriorTxns  :: Int
  }
  deriving (Eq, Show)

-- | Parameters for the round-amount / new-merchant fraud heuristic. Carries a
-- 'Money' threshold so it is currency-correct like everything else.
data FraudPolicy (c :: Currency) = FraudPolicy
  { fraudMinAmount        :: Money c
    -- ^ Only amounts at least this large are considered suspicious.
  , fraudRoundMajorModulus :: Integer
    -- ^ An amount counts as "round" when it is a whole number of major units
    -- divisible by this (e.g. 100 flags 100.00, 500.00, 2000.00, ...).
  }
  deriving (Eq, Show)

data Blocklist = Blocklist
  { blockedMerchants :: Set MerchantId
  , blockedAccounts  :: Set AccountId
  }
  deriving (Eq, Show)

emptyBlocklist :: Blocklist
emptyBlocklist = Blocklist Set.empty Set.empty

-- ---------------------------------------------------------------------------
-- The authorization context
-- ---------------------------------------------------------------------------

-- | Everything a rule may look at /besides/ the transaction itself: the
-- account, recent history (for velocity and "have we seen this merchant?"),
-- and the policies in force. Bundling it keeps every rule a two-argument
-- pure function.
data AuthContext (c :: Currency) = AuthContext
  { ctxAccount   :: Account c
  , ctxHistory   :: [Transaction c]
    -- ^ Previously authorized transactions for this account. Order is not
    -- assumed; rules that care about time read 'txnTimestamp'.
  , ctxVelocity  :: VelocityPolicy
  , ctxFraud     :: FraudPolicy c
  , ctxBlocklist :: Blocklist
  , ctxAmountCeiling :: Money c
    -- ^ Absolute hard cap. Amounts at or above it are refused regardless of
    -- the per-account limit. This is the ledger-edge maximum that the 'Money'
    -- type deliberately does not encode ('Integer' is unbounded); it is a
    -- backstop, normally set far above any real 'accountPerTxnLimit'.
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Rule vocabulary
-- ---------------------------------------------------------------------------

-- | The identity of each rule, for the reasoning trail.
data RuleName
  = SpendingLimit
  | AmountCeiling
  | CurrencyAllowed
  | Velocity
  | FraudPattern
  | BlocklistRule
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Why a rule declined. 'violationReason' is human-readable and always
-- non-empty by construction of the rules that build it.
data Violation = Violation
  { violationRule   :: RuleName
  , violationReason :: Text
  }
  deriving (Eq, Show)

-- | A single rule's verdict on a single transaction.
data RuleOutcome
  = Pass
  | Fail Violation
  deriving (Eq, Show)

isPass :: RuleOutcome -> Bool
isPass Pass     = True
isPass (Fail _) = False

-- | A rule paired with its name. A rule is a /pure/ function
-- @'AuthContext' c -> 'Transaction' c -> 'RuleOutcome'@; pairing the name on
-- keeps the trail honest (the engine cannot mislabel which rule spoke).
data NamedRule (c :: Currency) = NamedRule
  { ruleName :: RuleName
  , runRule  :: AuthContext c -> Transaction c -> RuleOutcome
  }
