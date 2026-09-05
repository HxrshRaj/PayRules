# PayRules

[![CI](https://github.com/HxrshRaj/PayRules/actions/workflows/ci.yml/badge.svg)](https://github.com/HxrshRaj/PayRules/actions/workflows/ci.yml)

A pure, typed authorization rules engine for payment transactions, written in
Haskell.

Given a transaction and an account context, PayRules runs a pipeline of
composable authorization rules and returns an **approve / decline** decision
together with a **reasoning trail** — which rule fired, and why.

**Live demo:** [payrules.onrender.com](https://payrules.onrender.com) — click
one of the six example transactions to see a real decision from the real
engine (free tier; the first request after a while sleeps and takes ~50s to
wake up).

```
$ stack exec payrules -- demo
### round amount to a first-seen merchant -> declined
Decision: Declined
  ok      SpendingLimit
  ok      CurrencyAllowed
  ok      Velocity
  DECLINE FraudPattern — round amount USD 800.00 to first-seen merchant 'Unknown Store' matches a suspicious pattern
  ok      BlocklistRule
```

---

## Build and run

The toolchain is pinned: **Stack** with snapshot `lts-22.28` (**GHC 9.6.6**).
Stack provisions that exact GHC itself, so a clean machine needs nothing else.

```
stack build      # first run also fetches GHC 9.6.6
stack test       # runs the QuickCheck property suite (29 properties)
stack exec payrules -- demo      # built-in scenarios, one per rule
stack exec payrules -- check     # read '|'-delimited transactions from stdin
```

`cabal build` / `cabal test` also work — the `.cabal` file is hand-written and
is the single source of truth; `stack.yaml` just points at it.

**Prebuilt binaries** for Linux / macOS / Windows are attached to each
[GitHub Release](https://github.com/HxrshRaj/PayRules/releases) (pushing a
`v*` tag builds them). **API docs** (Haddock) are published to
[hxrshraj.github.io/PayRules](https://hxrshraj.github.io/PayRules/).

Example of `check` mode:

```
$ printf 'acc-001 | GBP 10.00 | m-x | Corner Shop | Grocery\n' | stack exec payrules -- check
### acc-001 | GBP 10.00 | m-x | Corner Shop | Grocery
Decision: Declined
  ok      SpendingLimit
  DECLINE CurrencyAllowed — GBP is not an allowed currency for this account (allowed: EUR, USD)
  ...
```

---

## HTTP API and demo UI

The same engine behind a `servant` + `warp` server (`payrules-server`):

```
PORT=8080 stack exec payrules-server
```

| Endpoint | |
|---|---|
| `GET /` | the demo page below |
| `GET /healthz` | `{"status":"ok"}` |
| `POST /authorize` | body below → decision + reasoning trail |

**`GET /`** is a single self-contained HTML page (`server/static/index.html`,
embedded into the binary at compile time — no separate assets to ship) that
calls this same `/authorize` endpoint from the browser: a form, six one-click
example transactions (one per rule, plus a clean approve), and the result
rendered as a decision badge and a pass/fail row per rule — nothing on the
page is mocked. It is genuinely additive: it does not change `/healthz` or
`/authorize`, and it does not touch `PayRules.Engine`/`Rules`/`Money` at all.

```
$ curl -s localhost:8080/authorize -H 'content-type: application/json' -d '
  { "account": "acc-001",
    "amount": { "currency": "GBP", "minorUnits": 1000 },
    "merchant": { "id": "m-x", "name": "Corner Shop", "category": "Grocery" } }'

{ "decision": "declined",
  "violations": [ { "rule": "CurrencyAllowed",
                    "reason": "GBP is not an allowed currency for this account (allowed: EUR, USD)" } ],
  "trail": [ { "rule": "SpendingLimit", "outcome": "ok" }, … ] }
```

Two things this demonstrates:

* **The runtime → type-level currency bridge.** The request's currency is just
  a string; `PayRules.Wire` runs it through `withKnownCurrency` so the body is
  evaluated at the correct `Money c` type — the same mechanism the CLI's
  `check` mode uses. A code it doesn't model is a `400`, not a crash.
* **The engine stays dependency-free.** `aeson` is confined to `PayRules.Wire`
  and the server executable; `servant`/`warp` to the executable alone. The
  library that does the actual authorization still depends only on `base`,
  `text`, `time`, `containers`.

The request is evaluated against the sample account (`demoContext`), matching
the CLI — sending a full account/policy context per request is out of scope for
the demo.

### Deploy

CI builds the multi-stage [`Dockerfile`](Dockerfile) on every push and
publishes the image to the GitHub Container Registry:

```
docker run -p 8080:8080 ghcr.io/hxrshraj/payrules:latest
```

[`render.yaml`](render.yaml) is a Render Blueprint — connect the repo at
render.com and it builds the Dockerfile, injects `$PORT`, and health-checks
`/healthz`; no secrets needed for a public repo. Any other Docker host works
the same way.

---

## Why Stack (not Cabal)

Both are fine; the deciding factor for a project a reviewer will *clone and
build once* is reproducibility with the least ceremony:

* an LTS snapshot pins every transitive dependency by construction — no
  `cabal.project.freeze` to remember to commit and keep in sync;
* `stack build` installs the matching GHC, so the reviewer doesn't need a
  system GHC that happens to line up.

To keep Cabal users first-class, there is **no `package.yaml`** — the
`PayRules.cabal` file is written by hand and both tools consume it directly.

---

## Design decisions

### 1. Typed money is the foundation — `src/PayRules/Money.hs`

Money is a `newtype` over an **exact `Integer` count of minor units** (paise,
cents), with the **currency carried in a phantom type parameter**:

```haskell
newtype Money (c :: Currency) = Money Integer   -- constructor not exported
```

Three properties fall out of that one choice:

**Currencies cannot be mixed — it is a *compile* error.**
`add :: Money c -> Money c -> Money c` shares one `c` across both operands, so:

```
    • Couldn't match type ‘GBP’ with ‘USD’
      Expected: Money USD
        Actual: Money GBP
    • In the second argument of ‘add’, namely ‘gbp’
```

There is no runtime "are these the same currency?" branch anywhere in the
codebase, because there is nothing to check — the mistake doesn't typecheck.
`Currency` is an ordinary enum promoted with `DataKinds`; a small hand-rolled
singleton class `KnownCurrency` recovers the value when a rule needs it
(`currencyOf :: KnownCurrency c => Money c -> Currency`).

**Arithmetic cannot silently lose precision or overflow.**
There is no `Double` in the module. `add`, `sub` and `scale` are `Integer`
operations, and `Integer` is unbounded, so `0.10 + 0.20` is exactly `0.30` and
a billion additions do not drift. `scale :: Integer -> Money c -> Money c`
multiplies by an integer *count*; there is deliberately **no
`Money -> Money -> Money` multiplication** and **no `Num` instance** — `money²`
has no unit and `fromInteger` would have to invent a currency. What money *does*
have is a lawful, currency-safe **`Monoid`** (`(<>) = add`, `mempty = zero`), so
`mconcat` over a list of line items is available without the footgun.

**The one operation that can round is total and conserves the whole.**
Splitting a value — a bill, a percentage fee — is the only place indivisibility
bites. `allocate :: Money c -> [Natural] -> Either MoneyError [Money c]` uses the
largest-remainder method: it hands out the floor share to everyone, then
distributes the leftover one minor unit at a time to the parts with the largest
truncated fractions. `sum (allocate m ws) == m` holds by construction.

**Parsing refuses to round.** `fromDecimal "10.005" :: Money 'USD` is
`Left (TooManyFractionalDigits "10.005" 2)`, not `10.00` or `10.01`. Dropping a
digit silently is exactly the bug this module exists to prevent, so it is a
first-class error instead.

The type-level currency is ideal *inside* the engine but useless for stdin/JSON
input that only knows its currency at runtime. `SomeMoney` (an existential
carrying the `KnownCurrency` evidence) plus `withKnownCurrency` /
`parseSomeMoney` are the single, exhaustive bridge back into the typed world —
the CLI's `check` mode parses `USD 800.00` at runtime and evaluates it at type
`Money 'USD`.

The one honest trade-off: `Integer` has no ceiling. A production ledger still
wants a maximum-amount check — but that is a *domain rule*, not a property of the
number type, so it lives in `PayRules.Rules` as `amountCeilingRule` (backed by
`ctxAmountCeiling`), not in `Money`.

### 2. Rules are pure functions; the engine accumulates

Each rule is a pure `AuthContext c -> Transaction c -> RuleOutcome`, paired with
its name in a `NamedRule` so the engine cannot mislabel the trail. Rules never
call each other and never short-circuit each other.

`evaluate` runs **every** rule and collects **every** violation — it does not
stop at the first failure. A decline therefore reports *all* its reasons (see
the two-reason scenario in `demo`), which is what makes the trail useful for a
human. Because the rules are independent and pure, `evaluate` is a
deterministic function of `(rules, context, transaction)` and does not depend on
the order of the rule list — both facts are asserted as properties.

### 3. The six rules — `src/PayRules/Rules.hs`

| Rule | Declines when | Notes |
|---|---|---|
| `SpendingLimit` | `amount > accountPerTxnLimit` | both sides are `Money c` for the same `c` — no currency handling in the comparison |
| `AmountCeiling` | `amount >= ctxAmountCeiling` | absolute backstop for the unbounded `Integer`; `>=` (ceiling itself is out of range), unlike the per-txn limit's `>` |
| `CurrencyAllowed` | txn currency ∉ account's allowed set | currency recovered from the type via `currencyOf` |
| `Velocity` | ≥ N prior transactions within the time window before this one | needs transaction history; window measured back from `txnTimestamp` |
| `FraudPattern` | amount is large **and** round **and** the merchant is first-seen in history | any one alone is unremarkable; together they are the card-testing / bust-out shape |
| `BlocklistRule` | merchant id or account id on the blocklist | merchant checked first so its reason wins |

### 4. Module layout

```
src/PayRules/
  Money.hs     typed money: currency, exact arithmetic, allocate, SomeMoney
  Types.hs     Transaction / Account / AuthContext, rule vocabulary
  Rules.hs     the six rules + defaultRules
  Engine.hs    evaluate (accumulate-all) + explain (the trail printer)
  Sample.hs    a worked account + the demo scenarios (scaffolding, not engine)
  Wire.hs      JSON shapes + the pure core of POST /authorize (only this needs aeson)
app/Main.hs    thin CLI: demo mode / stdin check mode
server/Main.hs       thin HTTP server: servant + warp over PayRules.Wire
server/static/index.html  the demo page (embedded into the binary at compile time)
test/          Spec.hs (29 properties) + PayRules/Gen.hs (Arbitrary instances)
```

---

## Property-based testing — `test/Spec.hs`

29 properties, every one driven by random inputs through `Arbitrary` instances
in `PayRules.Gen` (generators draw ids from small pools on purpose, so
collisions — repeat merchants, blocklisted accounts — actually occur). A
representative slice:

* **money is a commutative monoid under `add`**, and `mconcat == foldr add zero`;
* **`scale` is exact for any scalar** — `toMinorUnits (scale k m) == k * toMinorUnits m`
  for arbitrarily large `k` (this would fail immediately for `Int64` or `Double`);
* **`allocate` conserves the total and the part count**, and every part lands
  within one minor unit of its exact proportional share, even for unequal
  weights;
* **`renderAmount` / `fromDecimal` round-trip** (USD and JPY — different
  exponents), and **`fromDecimal` refuses over-precise input** rather than
  rounding it;
* **declined iff there is ≥ 1 violation**; **approved iff every rule passes when
  run alone**; **the reported violations are exactly the `Fail`s in the trail**;
* **evaluation is deterministic** and **independent of rule order** (same
  decision, same set of violated rules under a random shuffle of the rule list);
* **metamorphic:** blocklisting the transaction's account forces a decline;
  tightening the blocklist never turns a decline into an approval; raising the
  per-transaction limit (or the hard ceiling) never *adds* the corresponding
  violation; an amount at or above the ceiling is always declined.

### What the type checker caught, and what the properties caught

Honesty first: no property uncovered a *logic* bug in the final engine — once it
compiled, the suite was green. That is partly the point. The bugs this style of
code is prone to — mixing currencies, losing a fraction — are **unrepresentable
here**, so the type checker rejects them before a test ever runs. The mistakes
that *did* surface during development were all compile-time: a missing
`OverloadedStrings`, and a name clash between `Money.scale` and
QuickCheck's own `scale`.

The properties still earn their place as regression guards, and one is
load-bearing rather than decorative. `allocate`'s largest-remainder method is
easy to get subtly wrong. The obvious shortcut — give everyone the floor share
and dump the entire remainder on the first part — passes *sum conservation*, so
a weaker test suite would wave it through. It fails
`prop_allocateEqualWeightsAreFair` in three tests:

```
*** Failed! Falsified (after 3 tests):
USD 0.02, split 32 ways
parts (minor units) = [2,0,0,0,0,0,0,0,0,0,0,0, ... ]   -- one part takes everything
```

The real implementation returns `[1,1,0,0, ... ]`. The fairness and
proportionality properties are what pin the method down.

---

## License

MIT — see [LICENSE](LICENSE).
