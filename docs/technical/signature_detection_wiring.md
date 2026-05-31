# signature_detection.pl — Wiring Gotchas

Generated 2026-05-30 during authored-field liveness audit. Documents three failure
modes discovered while testing whether authored data fields reach the final static type.
Scope: things that look right but produce wrong results in practice.

---

## 1. `findall` with Sig bound gives wrong constraint lists

**The mistake:** querying which constraints get `natural_law` signature like this:

```prolog
findall(C, signature_detection:constraint_signature(C, natural_law), NLCs)
```

**Why it's wrong:** `constraint_signature/2` has five clauses before the `natural_law`
clause (false_natural_law, false_ci_rope, false_summit_mountain, and two others). When
`natural_law` is bound as the second argument, Prolog skips any clause whose head
doesn't unify — so clauses 1–4 are bypassed entirely. The result is every constraint
that satisfies clause 5's body conditions, regardless of whether those earlier clauses
would fire first with Sig unbound.

**Concrete failure:** `constraint_signature(disparity_as_depth_signal, natural_law)`
succeeds (all natural_law conditions are met), but `constraint_signature(
disparity_as_depth_signal, Sig)` returns `false_summit_mountain`. The findall
result included disparity_as_depth_signal; the engine never actually gives it
natural_law.

**Correct query** (matches what the engine does — Sig unbound, first-match wins):

```prolog
findall(C, (
    signature_detection:constraint_signature(C, Sig),
    Sig == natural_law
), NLCs)
```

In the current corpus this returns exactly two constraints:
`explanatory_closure_mechanism` and `state_role_time_collapse`.

---

## 2. Signature priority order locks most naturally-emerging constraints

**The clause order in `constraint_signature/2`:**

1. `false_natural_law` — fires when `false_natural_law(C, _)` (Boltzmann test fails)
2. `false_ci_rope` — fires when `false_ci_rope(C, _)` (Boltzmann CI-rope fails)
3. `false_summit_mountain` — fires when `constraint_beneficiary(C, _)` exists AND metric
   profile matches mountain conditions
4. `natural_law` — fires when `emerges_naturally AND natural_law_signature(Profile)`
5. `coupling_invariant_rope` — Boltzmann-derived
6. Profile-based catch-all → `classify_by_signature` → constructed_* / ambiguous

**Consequence:** a constraint that passes every `natural_law_signature` condition will
still get `false_natural_law`, `false_ci_rope`, or `false_summit_mountain` if any of
those fire first. In the current 223-constraint corpus, only 2 get `natural_law` with
Sig unbound.

**Impact on liveness testing:** accessibility_collapse and resistance are live via the
`natural_law_signature` gate (which reads them). But that gate is only reached when none
of the first three signatures fire. Testing those fields requires a constraint from the
narrow set that actually gets `natural_law`. Using any other constraint produces
zero-flip results that look like "reachable-not-live" but are actually "signature-locked
before the gate that uses this field."

**Confirmed live testsets for accessibility_collapse and resistance:**
- `explanatory_closure_mechanism` (AC=0.92, resistance=0.08, natural_law baseline →
  mountain; AC=0.50 → ambiguous → powerless rope; resistance=0.40 →
  constructed_low_extraction → powerless+analytical rope)
- `state_role_time_collapse` (not tested this session; same natural_law signature)

---

## 3. `false_summit_mountain` uses `constraint_beneficiary/2` facts; `natural_law_signature` uses `count_power_beneficiaries/2` — they are not equivalent

**`false_summit_mountain` gate** (`signature_detection.pl:1208`):

```prolog
findall(B, narrative_ontology:constraint_beneficiary(C, B), Beneficiaries),
Beneficiaries \= [],
```

Fires when at least one `constraint_beneficiary/2` fact exists for C.

**`natural_law_signature` gate** (`signature_detection.pl:295`):

```prolog
BeneficiaryCount == 0,
```

Where `BeneficiaryCount` comes from `count_power_beneficiaries/2`, which counts classes
with `intent_power_change(Interval, Class, Delta)` where Delta > 0.1, found via
`affects_constraint(Interval, C)`.

**The gap:** a constraint with `constraint_beneficiary` facts but NO `intent_power_change`
facts (or no interval with Delta > 0.1) has `BeneficiaryCount = 0` (passes the
natural_law BC gate) but `Beneficiaries \= []` (triggers false_summit_mountain). The
natural_law conditions are all satisfied, but false_summit_mountain fires first.

**Concrete case:** `disparity_as_depth_signal` has 3 `constraint_beneficiary` facts and
no `intent_power_change` facts → BeneficiaryCount=0, Beneficiaries=[3 items] →
passes natural_law gate but gets false_summit_mountain.

**Implication for testset authoring:** to get `natural_law` signature (not
false_summit_mountain), a constraint must have zero `constraint_beneficiary` facts, OR
the false_summit_mountain mountain-metric preconditions must fail (BaseEps >
mountain_extractiveness_max or Supp > mountain_suppression_ceiling).
