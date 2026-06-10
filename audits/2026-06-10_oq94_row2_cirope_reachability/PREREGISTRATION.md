# Pre-registration — Q2 row-2 reachability control: can a captured constraint reach CI_Rope certification?

**Written before the run (2026-06-10).** Commissioned by operator ruling (step-3 preregistration
Q2 row 2): reachability of `signature_detection.pl:1019` for captured constraints was asserted
from CLAUSE ORDER only — an intent-label — and this thread's precedent is that intent-labels
flip on inspection. The control converts row 2 into row 1's evidentiary shape (or shows the
profile doesn't reach, diagnosed).

## The claim under test

From the read-site pass: "FSM intercepts *mountain-metric* beneficiary-bearers (dispatch
priority: FNL :70 > FCR :77 > FSM :99 > NL :109 > CI_Rope :117), but a captured low-ε
NON-mountain profile can reach the CI_Rope gate." FSM requires `emerges_naturally` (clause
:1332), so a non-emergent captured constraint should pass FSM; whether it then SATISFIES
CI_Rope's own gates (Boltzmann-compliant + scope-invariant + coordination function) on an
asserted-facts vector is unknown — that is what the run reveals.

## Cases (asserted facts, prototype pattern; FULL profile authored so no unknown-profile guard fires)

Both: ε=0.10, suppression_requirement=0.05, theater_ratio=0.10, accessibility_collapse=0.20,
resistance=0.10; NO `emerges_naturally`; NO naturality/rope claims (avoids FNL/FCR claim
clauses).

| case | beneficiary | role |
|---|---|---|
| `cir_cap` | `constraint_beneficiary(cir_cap, capturer_ci)` | the question: captured low-ε non-emergent |
| `cir_ctl` | none | probe-validity twin: CI_Rope REQUIRES coordination function, so this case must NOT certify CI_Rope — if it does, the probe is INVALID |

The probe prints each CI_Rope gate component separately (`boltzmann_compliant/2` result,
`scope_invariance_test/2` result, `has_coordination_function/1`, `excess_extraction/2`) so a
non-reach is diagnosable as failed-at-gate-X, never silent (every diagnostic needs its positive
control; `cache_registry:clear_all_caches` before reading — Boltzmann memo caches read stale).

## Pre-registered outcomes

1. **`cir_cap` → `constraint_signature = coupling_invariant_rope`** (and `cir_ctl` ≠ CI_Rope) ⇒
   **REACHABLE, witnessed**: a captured constraint certifies "structurally sound true
   coordination" today. Row 2 becomes row-1-shaped (misfire witness in hand); the gate ruling
   returns to the operator with evidence.
2. **`cir_cap` blocked at a named gate** (Boltzmann non-compliant / scope-variant / intercepted
   by an earlier signature — the component printout names which) ⇒ **NOT REACHED on this
   vector**; under-claim: reachability stays unwitnessed, NOT disproven — the block is diagnosed
   and row 2 stays deferred with the diagnosis recorded. A block at Boltzmann/scope on an
   asserted-facts vector is environment-shaped, not an interception proof.
3. **`cir_ctl` certifies CI_Rope** (without a coordination function) ⇒ **INVALID probe** —
   something other than the read clause dispatched; diagnose before any use.

Run from `prolog/`:
`swipl -g run_cirope_reachability -t halt ../audits/2026-06-10_oq94_row2_cirope_reachability/cirope_reachability.pl`
→ `cirope_reachability.out`.
