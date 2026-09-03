{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : PayRules.Money
-- Description : Typed, exact money.
--
-- The centrepiece design decision of PayRules. In one paragraph:
--
-- Money is an /exact integer count of minor units/ (paise, cents) wrapped in a
-- 'newtype', and the currency is carried in a /phantom type parameter/ so the
-- compiler knows it. From that one choice three properties follow:
--
--   * __Currencies cannot be mixed.__ @gbp \`add\` usd@ is a compile error,
--     because 'add' has type @'Money' c -> 'Money' c -> 'Money' c@ and the two
--     @c@s are different types. There is no runtime currency check to forget.
--
--   * __Ordinary arithmetic cannot lose precision.__ There is no 'Double'
--     anywhere. 'add', 'sub' and 'scale' are 'Integer' operations, and
--     'Integer' does not overflow, so 0.1 + 0.2 is exactly 0.3 and a billion
--     transactions do not drift.
--
--   * __The one operation that /can/ round is total and conserves the sum.__
--     Splitting a value ('allocate') distributes the indivisible remainder one
--     minor unit at a time; @sum (allocate m ws) == m@ holds by construction,
--     so you can never create or destroy a paisa when splitting a bill or a fee.
--
-- Deliberately __no 'Num' instance__: @('*')@ on two 'Money' values has no
-- meaningful unit (money squared?), and 'fromInteger' would have to invent a
-- currency out of thin air. Only the operations that make sense are exposed.
module PayRules.Money
  ( -- * Currency
    Currency (..)
  , currencyCode
  , currencyExponent
  , parseCurrency

    -- * Typed money
  , Money
  , KnownCurrency (..)
  , currencyOf

    -- * Construction
  , fromMinorUnits
  , toMinorUnits
  , money
  , fromDecimal
  , MoneyError (..)

    -- * Exact arithmetic
    -- $arith
  , zero
  , add
  , sub
  , negate
  , scale
  , isZero
  , isPositive
  , isNegative

    -- * Lossless splitting
  , allocate

    -- * Rendering
  , render
  , renderAmount

    -- * Currency-erased money (a bridge for runtime-only currencies)
  , SomeMoney (..)
  , parseSomeMoney
  , withSomeMoney
  , withKnownCurrency
  ) where

import           Data.Char    (isDigit, isSpace)
import           Data.List    (sortBy)
import           Data.Ord     (Down (..), comparing)
import           Data.Proxy   (Proxy (..))
import           Data.Text    (Text)
import qualified Data.Text    as T
import           Numeric.Natural (Natural)
import           Prelude      hiding (negate)
import qualified Prelude

-- ---------------------------------------------------------------------------
-- Currency
-- ---------------------------------------------------------------------------

-- | The currencies the system knows about. With @DataKinds@ this type is also
-- promoted to a /kind/, and its constructors become types (@'USD@, @'INR@, ...)
-- that can index 'Money'.
data Currency = USD | EUR | GBP | INR | JPY
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | The ISO 4217 alphabetic code, e.g. @"INR"@.
currencyCode :: Currency -> Text
currencyCode = T.pack . show

-- | ISO 4217 minor-unit exponent: how many decimal places the currency has.
-- Most are 2 (100 minor units per major); JPY has 0.
currencyExponent :: Currency -> Int
currencyExponent JPY = 0
currencyExponent USD = 2
currencyExponent EUR = 2
currencyExponent GBP = 2
currencyExponent INR = 2

-- | Parse a currency code, case- and whitespace-insensitively.
parseCurrency :: Text -> Either MoneyError Currency
parseCurrency t =
  case T.toUpper (T.strip t) of
    "USD" -> Right USD
    "EUR" -> Right EUR
    "GBP" -> Right GBP
    "INR" -> Right INR
    "JPY" -> Right JPY
    other -> Left (UnknownCurrency other)

-- ---------------------------------------------------------------------------
-- The Money type
-- ---------------------------------------------------------------------------

-- | A monetary amount: an exact integer number of minor units (paise, cents,
-- ...) tagged with its currency @c@ /at the type level/.
--
-- The data constructor is intentionally not exported. Build values with
-- 'fromMinorUnits', 'money' or 'fromDecimal'; the invariant we get for free is
-- simply "the payload is an exact minor-unit count", but hiding the constructor
-- keeps 'render'/'fromDecimal' the only story for humans reading amounts.
newtype Money (c :: Currency) = Money Integer
  deriving (Eq, Ord)

-- | Show as a rendered amount, e.g. @INR 1234.50@. Handy in GHCi and test
-- counterexamples; not meant to be @read@-able.
instance KnownCurrency c => Show (Money c) where
  show = T.unpack . render

-- | Same-currency amounts form a monoid under addition. This is the /safe/
-- version of a @Num@ instance: @('<>')@ can only ever combine two amounts in
-- the same currency (the phantom @c@ is shared), and it is genuinely
-- associative with 'zero' as identity — the property suite checks the laws.
-- Lets you @mconcat@ a batch of line items without reaching for a fold.
instance Semigroup (Money c) where
  (<>) = add

instance Monoid (Money c) where
  mempty = zero

-- | Value-level recovery of the type-level 'Currency' tag. One instance per
-- promoted constructor; this is the (hand-rolled) singleton that lets a rule
-- ask "what currency is this?" without threading a 'Currency' value alongside
-- every 'Money'.
class KnownCurrency (c :: Currency) where
  currencySing :: proxy c -> Currency

instance KnownCurrency 'USD where currencySing _ = USD
instance KnownCurrency 'EUR where currencySing _ = EUR
instance KnownCurrency 'GBP where currencySing _ = GBP
instance KnownCurrency 'INR where currencySing _ = INR
instance KnownCurrency 'JPY where currencySing _ = JPY

-- | The currency of a typed 'Money' value, as a plain 'Currency'.
currencyOf :: forall c. KnownCurrency c => Money c -> Currency
currencyOf _ = currencySing (Proxy @c)

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data MoneyError
  = UnknownCurrency Text
    -- ^ 'parseCurrency' / 'parseSomeMoney' given a code we do not model.
  | MalformedDecimal Text
    -- ^ 'fromDecimal' given something that is not @[-+]?digits[.digits]@.
  | TooManyFractionalDigits Text Int
    -- ^ 'fromDecimal' given more fractional digits than the currency's
    -- exponent (@Int@). We /refuse/ rather than round: silently dropping a
    -- digit is exactly the precision loss this module exists to prevent.
  | MinorComponentTooLarge Natural Integer
    -- ^ 'money' given a minor component that does not fit the currency's
    -- precision (given value, and the scale factor it must be below).
  | AllocateEmptyOrZeroWeights
    -- ^ 'allocate' given no weights, or weights that are all zero.
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Wrap a raw minor-unit count. Total and exact; the currency comes from the
-- expected type. Use this when you already have paise/cents (e.g. from a
-- database column).
fromMinorUnits :: Integer -> Money c
fromMinorUnits = Money

-- | The underlying exact minor-unit count.
toMinorUnits :: Money c -> Integer
toMinorUnits (Money n) = n

-- | Build a non-negative amount from major and minor components, validated
-- against the currency's precision. Illustratively:
--
--   * @money \@'USD 12 34@  = @Right@ US$12.34
--   * @money \@'JPY 500 0@  = @Right@ ¥500
--   * @money \@'JPY 500 1@  = @Left (MinorComponentTooLarge 1 1)@   (JPY has no minor unit)
--   * @money \@'USD 12 345@ = @Left (MinorComponentTooLarge 345 100)@
--
-- For a negative amount, apply 'negate' to the result. Keeping the sign out of
-- this constructor removes the "is @money 0 34@ minus 34 cents or plus?"
-- ambiguity entirely.
money
  :: forall c. KnownCurrency c
  => Natural           -- ^ major units (e.g. rupees)
  -> Natural           -- ^ minor units (e.g. paise), must be @< 10^exponent@
  -> Either MoneyError (Money c)
money major minor
  | toInteger minor >= scaleFactor = Left (MinorComponentTooLarge minor scaleFactor)
  | otherwise = Right (Money (toInteger major * scaleFactor + toInteger minor))
  where
    scaleFactor = 10 ^ currencyExponent (currencySing (Proxy @c)) :: Integer

-- | Parse a decimal string (@"12.34"@, @"12"@, @".5"@, @"-0.05"@, @"+7"@) into
-- typed 'Money'. More fractional digits than the currency allows is a
-- 'TooManyFractionalDigits' error, never a silent round.
fromDecimal :: forall c. KnownCurrency c => Text -> Either MoneyError (Money c)
fromDecimal input = do
  let e  = currencyExponent (currencySing (Proxy @c))
      t0 = T.strip input
  (neg, t1) <- case T.uncons t0 of
    Nothing       -> Left (MalformedDecimal input)
    Just ('-', r) -> Right (True, r)
    Just ('+', r) -> Right (False, r)
    Just _        -> Right (False, t0)
  (intPart, fracPart) <- case T.splitOn "." t1 of
    [i]    -> Right (i, T.empty)
    [i, f] -> Right (i, f)
    _      -> Left (MalformedDecimal input)
  -- must have at least one digit somewhere, and every character a digit
  if T.null intPart && T.null fracPart
    then Left (MalformedDecimal input) else Right ()
  if not (T.all isDigit intPart) || not (T.all isDigit fracPart)
    then Left (MalformedDecimal input) else Right ()
  if T.length fracPart > e
    then Left (TooManyFractionalDigits input e) else Right ()
  let intDigits  = if T.null intPart then "0" else intPart
      fracPadded = fracPart <> T.replicate (e - T.length fracPart) (T.singleton '0')
      n          = T.foldl' (\acc ch -> acc * 10 + toInteger (fromEnum ch - fromEnum '0'))
                            0 (intDigits <> fracPadded)
  Right (Money (if neg then Prelude.negate n else n))

-- ---------------------------------------------------------------------------
-- Exact arithmetic
-- ---------------------------------------------------------------------------

-- $arith
-- Every operation here is 'Integer' arithmetic on minor units, so none of them
-- can round, and because 'Integer' is unbounded none can overflow: a billion
-- additions do not drift. The phantom @c@ on 'add' and 'sub' forces both
-- operands to the same currency, so @gbp \`add\` usd@ is rejected by the type
-- checker rather than by a runtime guard you could forget to write.
--
-- The trade-off of 'Integer' over a fixed 'Int64' is that there is no natural
-- ceiling; a production ledger would still want an explicit maximum-amount
-- check at its edges. That belongs in a domain rule (see @PayRules.Rules@),
-- not in the number type.

zero :: Money c
zero = Money 0

add :: Money c -> Money c -> Money c
add (Money a) (Money b) = Money (a + b)

sub :: Money c -> Money c -> Money c
sub (Money a) (Money b) = Money (a - b)

negate :: Money c -> Money c
negate (Money a) = Money (Prelude.negate a)

-- | Multiply an amount by an integer scalar (a quantity, a count). Exact.
-- There is deliberately no @Money c -> Money c -> Money c@ multiplication.
scale :: Integer -> Money c -> Money c
scale k (Money a) = Money (k * a)

isZero :: Money c -> Bool
isZero (Money a) = a == 0

isPositive :: Money c -> Bool
isPositive (Money a) = a > 0

isNegative :: Money c -> Bool
isNegative (Money a) = a < 0

-- ---------------------------------------------------------------------------
-- Lossless splitting
-- ---------------------------------------------------------------------------

-- | Split an amount into parts proportional to the given weights, distributing
-- the indivisible remainder one minor unit at a time (largest-remainder /
-- Hamilton method). The sum of the parts always equals the original amount --
-- @foldr add zero <$> allocate m ws == Right m@ -- which the property suite
-- checks over random amounts and weights.
--
-- Fails only if there are no weights, or every weight is zero.
--
-- Example: @allocate (money \@'USD 10 0) [1,1,1]@ → @[3.34, 3.33, 3.33]@
-- (the extra cent goes to the first part, and 3.34 + 3.33 + 3.33 = 10.00).
allocate :: Money c -> [Natural] -> Either MoneyError [Money c]
allocate (Money total) weights
  | null ws || w == 0 = Left AllocateEmptyOrZeroWeights
  | otherwise         = Right (map Money distributed)
  where
    ws = map toInteger weights
    w  = sum ws

    -- floor share and truncated-fraction numerator for each part
    shares = [ (total * wi) `div` w | wi <- ws ]
    fracs  = [ (total * wi) `mod` w | wi <- ws ]   -- each in [0, w) since w > 0

    -- how many minor units are still unallocated after flooring
    remainder = fromInteger (total - sum shares) :: Int   -- in [0, length ws)

    -- indices that should receive an extra unit: those with the largest
    -- truncated fraction, ties broken by original position
    order = map snd
          . sortBy (comparing (\(f, i) -> (Down f, i)))
          $ zip fracs [0 :: Int ..]
    bumped = take remainder order

    distributed =
      [ if i `elem` bumped then s + 1 else s
      | (s, i) <- zip shares [0 :: Int ..]
      ]

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

-- | @"INR 1234.50"@ — currency code, space, signed decimal at the currency's
-- precision.
render :: forall c. KnownCurrency c => Money c -> Text
render m = currencyCode (currencyOf m) <> " " <> renderAmount m

-- | Just the signed decimal, @"1234.50"@ / @"-0.05"@ / @"500"@ (JPY). The
-- inverse of 'fromDecimal': @fromDecimal (renderAmount m) == Right m@.
renderAmount :: forall c. KnownCurrency c => Money c -> Text
renderAmount m =
  sign <> T.pack (show intPart) <> fracText
  where
    e           = currencyExponent (currencyOf m)
    scaleFactor = 10 ^ e :: Integer
    n           = toMinorUnits m
    sign        = if n < 0 then "-" else ""
    (intPart, fracPart) = abs n `divMod` scaleFactor
    fracText
      | e == 0    = ""
      | otherwise = "." <> T.justifyRight e '0' (T.pack (show fracPart))

-- ---------------------------------------------------------------------------
-- Currency-erased money
--
-- The type-level currency is great inside the engine, but input from stdin /
-- JSON only knows its currency at runtime. 'SomeMoney' packs a 'Money c'
-- together with evidence of 'KnownCurrency c'; 'withSomeMoney' unpacks it and
-- brings that instance back into scope so typed code can run.
-- ---------------------------------------------------------------------------

data SomeMoney where
  SomeMoney :: KnownCurrency c => Money c -> SomeMoney

instance Show SomeMoney where
  show (SomeMoney m) = show m

instance Eq SomeMoney where
  SomeMoney a == SomeMoney b =
    currencyOf a == currencyOf b && toMinorUnits a == toMinorUnits b

-- | Run a currency-polymorphic action at whichever 'Currency' is given,
-- recovering the 'KnownCurrency' instance. This is the only place the
-- runtime→type-level crossing happens, and it is exhaustive over 'Currency'.
withKnownCurrency
  :: Currency
  -> (forall (c :: Currency). KnownCurrency c => Proxy c -> r)
  -> r
withKnownCurrency USD k = k (Proxy @'USD)
withKnownCurrency EUR k = k (Proxy @'EUR)
withKnownCurrency GBP k = k (Proxy @'GBP)
withKnownCurrency INR k = k (Proxy @'INR)
withKnownCurrency JPY k = k (Proxy @'JPY)

-- | Parse @"INR 1234.50"@ (code, whitespace, decimal) into 'SomeMoney'.
parseSomeMoney :: Text -> Either MoneyError SomeMoney
parseSomeMoney txt = do
  let (codeT, rest) = T.break isSpace (T.strip txt)
  c <- parseCurrency codeT
  withKnownCurrency c (\(Proxy :: Proxy k) ->
    SomeMoney <$> fromDecimal @k (T.strip rest))

-- | Unpack 'SomeMoney' for a currency-polymorphic consumer.
withSomeMoney :: SomeMoney -> (forall c. KnownCurrency c => Money c -> r) -> r
withSomeMoney (SomeMoney m) k = k m
