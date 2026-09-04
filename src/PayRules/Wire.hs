{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : PayRules.Wire
-- Description : JSON request/response shapes for the HTTP API, and the pure
--               function behind the @POST /authorize@ endpoint.
--
-- The engine ('PayRules.Engine') and its types stay dependency-free; this
-- module is where @aeson@ enters, kept separate on purpose. The endpoint is
-- deliberately thin: it parses an 'AuthRequest', which knows its currency only
-- as a runtime string, crosses into the typed world through
-- 'withKnownCurrency', evaluates against the sample 'demoContext' (exactly like
-- the CLI's @check@ mode), and renders the 'AuthResult' back to JSON.
module PayRules.Wire
  ( AuthRequest (..)
  , AuthResponse (..)
  , authorize
  ) where

import           Data.Aeson
import           Data.Bifunctor (first)
import           Data.Maybe     (fromMaybe)
import           Data.Proxy     (Proxy (..))
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Time      (UTCTime)

import           PayRules.Engine
import           PayRules.Money
import           PayRules.Rules  (defaultRules)
import           PayRules.Sample (demoContext, parseMerchantCategory)
import           PayRules.Types

-- ---------------------------------------------------------------------------
-- Request
-- ---------------------------------------------------------------------------

-- | The body of @POST /authorize@. The amount is an exact integer count of
-- minor units plus its currency code — the same representation 'Money' uses,
-- so nothing is rounded on the wire. @timestamp@ is optional; when omitted the
-- server substitutes its own clock (which makes the sample history look old,
-- so the velocity rule sees a clean slate).
--
-- > { "account": "acc-001",
-- >   "amount":  { "currency": "USD", "minorUnits": 80000 },
-- >   "merchant": { "id": "m-9", "name": "Some Shop", "category": "Other" },
-- >   "timestamp": "2026-06-01T00:00:00Z" }
data AuthRequest = AuthRequest
  { reqAccount          :: Text
  , reqCurrency         :: Text
  , reqMinorUnits       :: Integer
  , reqMerchantId       :: Text
  , reqMerchantName     :: Text
  , reqMerchantCategory :: Text
  , reqTimestamp        :: Maybe UTCTime
  }
  deriving (Eq, Show)

instance FromJSON AuthRequest where
  parseJSON = withObject "AuthRequest" $ \o -> do
    acc          <- o .: "account"
    (cur, minor) <- o .: "amount" >>= withObject "amount"
                      (\a -> (,) <$> a .: "currency" <*> a .: "minorUnits")
    (mid, mn, mc) <- o .: "merchant" >>= withObject "merchant"
                      (\m -> (,,) <$> m .: "id" <*> m .: "name" <*> m .: "category")
    ts           <- o .:? "timestamp"
    pure (AuthRequest acc cur minor mid mn mc ts)

instance ToJSON AuthRequest where
  toJSON r = object
    [ "account"  .= reqAccount r
    , "amount"   .= object [ "currency" .= reqCurrency r, "minorUnits" .= reqMinorUnits r ]
    , "merchant" .= object [ "id"       .= reqMerchantId r
                           , "name"     .= reqMerchantName r
                           , "category" .= reqMerchantCategory r ]
    , "timestamp" .= reqTimestamp r
    ]

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | The engine's decision, flattened for JSON: @decision@ is
-- @"approved"@/@"declined"@; @violations@ is the reasons (empty iff approved);
-- @trail@ is every rule that ran with @"ok"@/@"decline"@.
data AuthResponse = AuthResponse
  { respDecision   :: Text
  , respViolations :: [(Text, Text)]
  , respTrail      :: [(Text, Text)]
  }
  deriving (Eq, Show)

instance ToJSON AuthResponse where
  toJSON r = object
    [ "decision"   .= respDecision r
    , "violations" .= [ object ["rule" .= a, "reason"  .= b] | (a, b) <- respViolations r ]
    , "trail"      .= [ object ["rule" .= a, "outcome" .= b] | (a, b) <- respTrail r ]
    ]

instance FromJSON AuthResponse where
  parseJSON = withObject "AuthResponse" $ \o -> do
    d  <- o .: "decision"
    vs <- o .: "violations" >>= traverse (withObject "violation"
            (\v -> (,) <$> v .: "rule" <*> v .: "reason"))
    ts <- o .: "trail" >>= traverse (withObject "step"
            (\s -> (,) <$> s .: "rule" <*> s .: "outcome"))
    pure (AuthResponse d vs ts)

-- ---------------------------------------------------------------------------
-- The endpoint's pure core
-- ---------------------------------------------------------------------------

-- | Evaluate one request against 'demoContext'. @now@ is used only when the
-- request omits its own @timestamp@. A 'Left' is a client error (bad currency
-- code or merchant category); the engine itself never fails.
authorize :: UTCTime -> AuthRequest -> Either Text AuthResponse
authorize now req = do
  currency <- first (T.pack . show) (parseCurrency (reqCurrency req))
  category <- parseMerchantCategory (reqMerchantCategory req)
  let merchant = Merchant (MerchantId (reqMerchantId req)) (reqMerchantName req) category
      stamp    = fromMaybe now (reqTimestamp req)
  pure $ withKnownCurrency currency $ \(Proxy :: Proxy c) ->
    let amount = fromMinorUnits (reqMinorUnits req) :: Money c
        txn    = Transaction "http" (AccountId (reqAccount req)) amount merchant stamp
    in render' (evaluate defaultRules (demoContext :: AuthContext c) txn)

render' :: AuthResult -> AuthResponse
render' result = AuthResponse
  { respDecision   = if decision result == Approved then "approved" else "declined"
  , respViolations = [ (name (violationRule v), violationReason v) | v <- violations result ]
  , respTrail      = [ (name r, if isPass o then "ok" else "decline") | (r, o) <- trail result ]
  }
  where
    name = T.pack . show
