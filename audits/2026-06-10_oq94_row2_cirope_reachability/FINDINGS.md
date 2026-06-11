# Findings — Q2 row-2 CI_Rope reachability control (run 2026-06-10)

## Constructed-vector probe: Outcome 2, diagnosed (raw: `cirope_reachability.out`)

`cir_cap` (captured low-ε non-emergent, full profile) did NOT reach CI_Rope — blocked at a
named gate: **`boltzmann_compliant = inconclusive(insufficient_classifications)`** (the gate
requires `compliant(_)`); both cases landed `ambiguous`. Component printout shows every other
gate green for `cir_cap` (scope `invariant`, coordination_fn `true`, excess_extraction 0.05),
and the validity twin behaved (`cir_ctl` coordination_fn `false`). Per the pre-registered
Outcome 2: the block is **environment-shaped** — asserted-facts vectors cannot feed the
Boltzmann factorization test the cross-index classification mass it needs — so reachability is
NOT disproven by this probe; it is simply not testable on synthetic vectors. Diagnosis recorded:
any future constructed-vector probe of a Boltzmann-gated signature must author classification
mass, not just metrics.

## Follow-up existence check on the live corpus (expectations stated before running)

Stated before the run: nonzero CI_Rope∧beneficiary → gate-reachability witnessed on real data;
zero → still unwitnessed (small n). Positive controls: the sweep must find CI_Rope
certifications at all and beneficiary-bearers at all — both fired.

**Result (raw: `corpus_existence_check.out`, corpus n=39 by `corpus_constraint/1` census):**

- CI_Rope certified: **7** constraints
- beneficiary-bearing (unique): 36 of 39
- **CI_Rope ∧ beneficiary: 7 of 7** — every constraint the live corpus certifies as
  "structurally sound true coordination" carries `constraint_beneficiary` facts.

## What this does and does not witness

**Witnessed:** the CI_Rope gate (`signature_detection.pl:1019`) is reached — and passed — by
exactly the population the capture question lives in (beneficiary-bearing constraints), at 7/7
of the live certified set. The reachability deferral reason for Q2 row 2 is discharged: this is
no corner case; CI_Rope certification currently runs entirely on constraints with beneficiaries,
with nothing upstream intercepting them (FSM requires `emerges_naturally` + mountain metrics).

**Not witnessed, and cannot be yet:** that any of the 7 is *captured*. Beneficiary-bearing ≠
captured — that distinction is the whole of OQ-92, and `gain_flow` does not exist until Stage C
generates stories that author it. A row-1-style misfire witness (a captured constraint
certifying CI_Rope) is structurally unobtainable pre-build. Row 2 therefore returns to the
operator with the strongest evidence available pre-Stage-C: gate live on the relevant
population, interception hypothesis dead, capture-status of the certified set unknowable until
the surface exists.

## Consequence for the step-3 preregistration

Row 2's ruling remains the operator's, now evidence-shaped rather than intent-labeled. The
options as evidence stands: (a) GATE now, riding with rows 1/3 (same signal, gate witnessed
live on beneficiary-bearers); (b) DEFER to first post-Stage-C data (rule after the first
generated batch shows whether any CI_Rope candidate authors a captured gain_flow). Either way it
gates only its own Stage-D clause; stages A–C are unblocked regardless.
