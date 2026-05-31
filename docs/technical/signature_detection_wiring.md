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

## 3. `false_summit_mountain` and `natural_law_signature` beneficiary sources — UNIFIED 2026-05-31 (Commit B1): both now read `constraint_beneficiary/2`

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

**RESOLVED 2026-05-31 (Commit B1 / OQ-43):** `count_power_beneficiaries/2` was repointed from the
empty `intent_power_change` join to the authored `constraint_beneficiary` table, so the two gates now
read the **same** source. The divergence described below no longer occurs; the section is retained as
history.

*Now (post-B1):* `count_power_beneficiaries(C, N)` = `findall(B, constraint_beneficiary(C, B), Bs),
sort, length`. So `disparity_as_depth_signal` (3 `constraint_beneficiary` facts) yields
`BeneficiaryCount = 3`, failing `BeneficiaryCount == 0` directly — it can no longer pass the NL gate
vacuously. The NL gate now certifies only constraints with **zero authored beneficiaries**, which is
exactly what `false_summit_mountain` filters on; the two are consistent. (Live NL certifications
dropped 5→2 when this landed: the 3 declined were constraints with authored asymmetric beneficiaries.)

*Historical (pre-B1) — the gap this section documented:* `BeneficiaryCount` came from
`count_power_beneficiaries/2`, which counted classes with `intent_power_change(Interval, Class, Delta)`
(Delta > 0.1) via `affects_constraint(Interval, C)`. `intent_power_change` is empty corpus-wide
(0/0 both corpora), so `BeneficiaryCount` was 0 for **every** constraint by absence — a vacuous pass.
A constraint with `constraint_beneficiary` facts but no `intent_power_change` (e.g.
`disparity_as_depth_signal`) passed the natural_law BC gate while `Beneficiaries \= []` triggered
false_summit_mountain, so FSM (checked first in the cascade) fired and the NL pass was masked. B1
removed the vacuity by reading the authored table directly.
