# Phase 3 blast-radius enumeration — written BEFORE any check is implemented

Required by the executor prompt. Four rounds of operator review found the same defect four times:
*a correct amendment whose blast radius was not traced to every place it applies.* The trace does
not happen reliably in prose, so it is done here as an enumeration, on paper, first.

## A. Entry points — every check inherits to all four

`with_retracted/2` and `with_asserted/2` are one-line delegations (`probe_harness.pl:70-74`), and
`with_overlay/4` will delegate the same way. **There is exactly one snapshot/install path**, so a
check written into the preflight applies to all four entry points. Nothing else needs editing.

| entry point | Templates | Facts |
|---|---|---|
| `with_retracted(T, G)` | caller's | **`[]`** |
| `with_asserted(F, G)` | **`[]`** | caller's |
| `with_overlay(T, F, G)` | caller's | caller's |
| `with_overlay(T, F, R, G)` *(new)* | caller's | caller's |

Consequence already used: `with_asserted/2` passes an **empty template list**, which is exactly why
clause 4′ bites it — not a special case in the code, a consequence of the delegation.

## B. Per-check argument positions — the column that has been wrong four times

| # | check | Templates | Facts | why |
|---|---|---|---|---|
| 2 | resolvable | **YES** | **NO** *(amended)* | an undefined *template* silently retracts nothing; an undefined *assert target* is the normal fixture-planting idiom and `assertz` creates it |
| 3 | rule-bearing | **YES** | NO | "matches a rule clause" is a property of the retract target only |
| 1 | snapshot non-empty | **YES** (per template) | NO | an empty `Templates` list stays legal by construction |
| 4 | shadowed at template shape | reads them | **YES** (per fact) | per-FACT trigger, tested at the shape of the template covering it |
| 4′ | reach undecidable | reads them | **YES** (per fact) | fires per fact that **no** template covers |
| 5 | dynamic/mutable | **YES** | **YES** | a static assert target throws `permission_error` inside `apply_overlay/2` — verified live at `unanimity_adjudication_probe.pl:66` |

### B1. Trap the enumeration caught — check 5 on the assert side needs a DEFINED guard

`predicate_property(M:F, dynamic)` **fails for an undefined predicate**. A naive check 5 on the
assert side would therefore read *undefined* as *static* and throw `probe_overlay_immutable` on the
7 legitimate fixture-planting sites — re-creating, through check 5, exactly the defect that
removing check 2 from the assert side was meant to fix.

**Check 5 on Facts must be: DEFINED and NOT dynamic ⇒ throw. UNDEFINED ⇒ legal** (`assertz` creates
it dynamic — verified). On Templates no guard is needed: check 2 runs first and rejects an
undefined template, so check 5 never sees one.

## C. The wrapper terms — the widest radius, 7 call sites

Escapes wrap their subject **in place**, inside the same list: `expect_empty(Reason, M:T)`. So the
lists are no longer homogeneous `M:T`, and **every predicate that walks `Templates` or `Facts` must
strip wrappers first**:

1. `check_qualified/1` — else a wrapped term throws the qualification type error
2. `warn_if_rule_clauses/1` → `rule_clauses/2`
3. `snapshot/2` (both the collect and the `verify_restore/2` re-collect)
4. `apply_overlay/2` — **the dangerous one**: unstripped, it calls `retract(expect_empty(...))`
5. `restore_overlay/3` — same, both the retract and the re-assert arms
6. `verify_restore/2` — re-collects via `snapshot/2`; a mismatch here reads as `probe_restore_failed`
7. every preflight check in §B

Missing any one of these is silent or misattributed, not loud. One helper — `unwrap/3` yielding
`Subject` + `Escape` — is used everywhere, and no walker touches a raw list element.

## D. Escape scoping — each suppresses its OWN clause only

| wrapper | suppresses | scope | legality |
|---|---|---|---|
| `expect_empty(R, M:T)` | clause 1 | that template | — |
| `allow_partial(R, M:T)` | clause 3 | that template | — |
| `allow_shadowed(R, M:F)` | clause 4 | that fact | — |
| `reach_undeclared(R, M:F)` | clause 4′ | that fact | **type error if the fact IS template-covered** |
| *(none)* | clauses **2** and **5** | — | unescapable |

Composition consequences, tested not assumed: `allow_partial` + `expect_empty` on a **static**
target still throws `probe_overlay_immutable`; suppressing 3 lets 1 fire on the same template by
design (two declarations for two distinct facts).

`Reason` must be `retrofit(Date, Text)` or `authored(Text)`; anything else is a type error, so the
provenance bit cannot be dropped.

## E. Order of operations — one mutation point, nothing throws past it

`setup_call_cleanup/3` registers `Cleanup` only once `Setup` **succeeds**, so a throw from Setup
means Cleanup never runs. Verified live at `unanimity_adjudication_probe.pl:66`, which leaks two
facts today. Therefore **the entire preflight runs before `setup_call_cleanup/3` is entered**, in
the ruled order 2 → 3 → 1 → 4/4′ → 5, over the unmutated database. `apply_overlay/2` is the single
mutation point and nothing after it can raise.

## F. Clause 4's guard property — pinned by a test, not by prose

Clause 4 is currently unreachable **given clauses 1 and 3 in the ruled order plus snapshot
completeness** (WRITEUP §8). That property depends on decisions elsewhere in this file, so it is
pinned: **artificially narrow the snapshot and clause 4 MUST fire.** That test failing is the
named signal that snapshot completeness or check ordering has regressed.
