# Restatement-gate no-op — FIXED (2026-06-14)

**The defect.** The external-vs-restatement gate was a no-op: it missed both seeded restatements
(id20, id27). Root cause — the gate **ignored the entry's `declared_fields`**, so it could not tell
a *declared* reading from an *open* term, defaulted every fired signature to "external," and so
could never reach "all signatures internal → restatement." The judge typed id20 (`generate the
homoiousios sibling and compare base properties / ε`) Ω_C, calling the declared sibling "the open
term"; the deterministic baseline let one external `define` override a restating `measure`.

**The fix** (`deterministic_baseline.py`, `spec.md`). The gate now consults `declared_fields`:
re-deriving the constraint's **own authored fields** (ε-invariance; comparing authored ε / base
properties / beneficiary–victim sets across readings **named in `cs_reading_relation`/`cs_kernel_id`**)
is **restatement**, on whichever signature fired — including an *incidental* `decide` that has no
real external decider (the id27 spurious "stakeholder" mention). A reading/kernel **not** declared
stays external; an open conceptual criterion over declared readings (foreclose-vs-coexist) stays Ω_C.

## Witness 1 — deterministic, runnable, GREEN

`python3 deterministic_baseline.py` (exit 0): `seed_control()` —
```
KNOWN_EXTERNAL    id 1 -> hybrid(define+measure) PASS
KNOWN_EXTERNAL    id14 -> empirical               PASS
KNOWN_RESTATEMENT id20 -> restatement             PASS   (was MISSED)
KNOWN_EXTERNAL    id24 -> hybrid(decide+define)   PASS
UNDER_DECLARATION id25 -> conceptual              PASS
KNOWN_RESTATEMENT id27 -> restatement             PASS   (was MISSED)
KNOWN_EXTERNAL    id30 -> conceptual              PASS
SEED CONTROL: GREEN | commit-control both_pass=True | unknown_rate=0.100
```
Two-sided: catches both restatements AND passes all externals + under-declaration; the two-sided
commit control still holds; no collateral (the foreclose/coexist family stays Ω_C, not restatement).

## Witness 2 — blind LLM, fixed protocol (refutes regex-overfit)

A fresh instance, no hand key, no repo access, given the fixed protocol + `declared_fields`, typed:
id20 **restatement**, id27 **restatement**, id1 **Ω_E**, id30 **Ω_C** — matching the deterministic
gate and the hand key. So the fix is the right *concept* (declared-field re-derivation), not a regex
tuned to the two seeds: an independent reasoner reaches the same calls from the protocol alone.

## Scope

This fixes the restatement *limb*. The historical `judge_results.json` records the pre-fix judge and
is kept as the POC's as-run artifact (a full re-run under the fixed rule would refresh the headline
metrics). The determinism boundary is unchanged: typing the *contested family* still needs judgment
(deterministic `unknown_rate` 0.10; the family split rides the operation-locus seat) — the gate fix
removes a structural no-op, it does not make typing deterministic.
