# Findings — OQ-92 step-2 gain-flow prototype (run 2026-06-10)

**Verdict: Outcome 1 — PASS, 8/8 as pre-registered.** Raw run: `gain_flow_prototype.out`
(preregistration committed before the run: `eb24a927`).

## Per-case results vs preregistration

| # | case | predicted | observed | match |
|---|------|-----------|----------|-------|
| 1 | cap_a | captures=[capturer_a], rest false | captures=[capturer_a], rest false | YES |
| 2 | mild_b | uncaptured, piton_candidate | uncaptured=true, piton_candidate=true | YES |
| 3 | dmv_c | uncaptured, piton_candidate | uncaptured=true, piton_candidate=true | YES |
| 4 | dmv_designed | uncaptured, piton_candidate | uncaptured=true, piton_candidate=true | YES |
| 5 | cheap_fix_e | uncaptured, transient_neglect, NOT piton | piton_candidate=false, transient_neglect=true | YES |
| 6 | captured_cheap_f | captures=[capturer_f], no demotion | captures=[capturer_f], rest false | YES |
| 7 | absent_g | NOTHING fires | fires_any=no | YES |
| 8 | malformed_h | NOTHING fires | fires_any=no | YES |

## The pairs (the silences and their firing twins)

- **2 ↔ 7 (diffuse vs absent).** Same seat structure (payer + institutional beneficiary).
  Authored-`diffuse` fired `uncaptured`/`piton_candidate` on mild_b; the surface-absent twin
  absent_g fired **nothing**. Case 2 firing is what makes case 7's silence a witness of the
  fail-closed register rather than a dead probe. The third provenance value
  (absent-fails-closed) is witnessed, not assumed.
- **1 ↔ 8 (existing-seat join vs malformed absorption).** Same seat structure. gain_flow →
  capturer_a completed the `role_of/3` join on cap_a (captures=[capturer_a]); gain_flow →
  ghost_seat_h (no such seat) fired **nothing** on malformed_h. Case 1 proves the join fires;
  case 8's silence witnesses the DECIDED absorption default (malformed-gain → fail-closed).
  The step-3 validation item stands: schema/compiler must reject gain_flow naming a seat not
  in `stakeholders[]` at authoring time, so this runtime absorption never hides a data error
  silently on corpus data.

## The load-bearing test (case 5)

cheap_fix_e is seat-identical to dmv_designed (case 4); the only authored difference is
`fixing_cost_class` (cheap vs prohibitive). Case 4 → piton_candidate; case 5 →
transient_neglect, NOT piton. This is OQ-90's decisive pre-wiring control discharged: with
both fields authored, piton and transient-neglect separate; without the fixing_cost field they
are the same gate output (both uncaptured). fixing_cost is load-bearing, as OQ-90 suspected —
now witnessed rather than argued.

## What this run does and does not witness (under-claim, per preregistration)

Cases 1–6 are near-tautological as logic tests — asserted facts feeding two-clause predicates.
What the 8/8 actually witnesses: (i) the four cells (captured/uncaptured × cheap/prohibitive)
and the two non-authored conditions (absent, malformed) separate **on these constructed cases**;
(ii) the `role_of/3` join behaves as designed in both directions; (iii) the eight fact-sets are
coherently authorable side-by-side in one session with no engine interference (per-seat
`dr_type_for_stakeholder` computed normally on every seat throughout). It does NOT witness:
representability across the corpus range, that generated stories will author the fields honestly
(that is exactly the step-3 diffuse-audit gate), or any calibration of fixing_cost beyond the
two-class atom.

Incidental observation (consistent with the 06-09 control's "bonus" finding, not new):
`constraint_beneficiary/2` pushed both capturer seats (cap_a, captured_cheap_f) to final_type
**scaffold** via `has_coordination_function/1` — the wrong direction for a captured constraint.
The authored gain-flow surface is what corrects this at the capture layer; the coordination-read
behavior itself is unchanged by this prototype. **Promoted post-run to OQ-94 (operator
2026-06-10):** this is the only production-engine behavior the prototype touched, and once step 3
wires `seat_captures` into classification, the two will be live in the same engine making
opposite-direction calls on the same constraints — named as a known-interference item the step-3
preregistration must carry.

## Consequence

Step 3 (schema field + compiler emission + prompt change, the OQ-83 Phase-A playbook) is
UNBLOCKED, carrying two preconditions recorded in OQ-92's Rulings block before any
classification wiring: the generated-diffuse audit gate (pre-stated sample size + tolerance,
pinned in the step-3 preregistration) and the malformed-gain schema rejection.
