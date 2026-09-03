{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : PayRules.Engine
-- Description : Compose rules into a decision plus a reasoning trail.
--
-- The engine is deliberately tiny and total. It runs every rule (it does not
-- short-circuit), keeps the outcome of each for the trail, and declines iff
-- any rule failed. Because rules are pure and independent, the result is a
-- deterministic function of @(rules, context, transaction)@ and does not
-- depend on the order of the rule list.
module PayRules.Engine
  ( Decision (..)
  , AuthResult (..)
  , evaluate
  , explain
  ) where

import           Data.Text  (Text)
import qualified Data.Text  as T

import           PayRules.Types

data Decision = Approved | Declined
  deriving (Eq, Show)

-- | The outcome of authorizing one transaction.
--
-- Invariants (checked by the property suite):
--
--   * @'decision' = 'Declined'@  iff  @'violations'@ is non-empty;
--   * @'violations'@ is exactly the 'Fail's in @'trail'@, in trail order;
--   * every 'Violation' has a non-empty 'violationReason'.
data AuthResult = AuthResult
  { decision   :: Decision
  , violations :: [Violation]
  , trail      :: [(RuleName, RuleOutcome)]
    -- ^ Every rule that ran and what it said — passes included.
  }
  deriving (Eq, Show)

-- | Run all the rules and fold their outcomes into a decision.
evaluate
  :: [NamedRule c]
  -> AuthContext c
  -> Transaction c
  -> AuthResult
evaluate rules ctx txn =
  AuthResult
    { decision   = if null vs then Approved else Declined
    , violations = vs
    , trail      = steps
    }
  where
    steps = [ (ruleName r, runRule r ctx txn) | r <- rules ]
    vs    = [ v | (_, Fail v) <- steps ]

-- | A human-readable report: the decision, then one line per rule in the
-- trail (@ok@ / @DECLINE — reason@). This is the string the CLI prints.
explain :: AuthResult -> Text
explain result =
  T.unlines (header : map line (trail result))
  where
    header = "Decision: " <> T.pack (show (decision result))
    line (name, Pass) =
      "  ok      " <> T.pack (show name)
    line (_, Fail v) =
      "  DECLINE " <> T.pack (show (violationRule v)) <> " — " <> violationReason v
