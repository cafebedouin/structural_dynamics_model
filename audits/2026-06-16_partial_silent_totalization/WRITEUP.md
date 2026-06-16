# Partial-silent totalization — `consensus_provenance/2` + `seat_perceived_vs_real/4`

**Date:** 2026-06-16  **Tracking:** OQ-121 follow-up (the two partial-silent family members)

OQ-121 totalized `extraction_reading`; the closer-look table flagged two more commentary predicates
as PARTIAL (explicit verdicts but a silent-failure path). Brought both up to the never-fail
convention (`constraint_signature/2` / `q6_cell/2` discipline). Neither has any consumer outside the
module — zero blast radius (verified by grep: no callers, no tests, no negation-as-failure).

## Changes

- **`consensus_provenance/2`** — was silent when `Ns=[]` (no non-excluded agent seat). Now TOTAL:
  added `no_agent_seats` (out-of-domain — no seats to compare) and `seats_untyped` (seats present,
  none typed — absence) as explicit verdicts. Always returns exactly one verdict.
- **`seat_perceived_vs_real/4`** — was silent when `dr_type_for_stakeholder/3` failed on an existing
  seat. Now returns `Computed = untyped` (explicit absence) instead. Still keyed on an EXISTING
  (C,Name) seat — a non-existent seat correctly has no reading (that is the domain, not silence).

## Witnesses (`totality_witness.txt`, `plunit_run.txt`)

- `consensus_provenance` TOTAL over the corpus (n=72): plural 37, **no_agent_seats 21**,
  manufactured 8, unanimous_no_excluded_seats 6 (Σ=72). The 21 `no_agent_seats` are the constraints
  that silently FAILED before — now explicit. Boundary fixtures: a no-seat and an excluded-only
  constraint both report `no_agent_seats`.
- `seat_perceived_vs_real` TOTAL over 370 existing seats; **0 `untyped` fallbacks** on the live
  corpus (the `untyped` branch is a defensive guard with no current trigger — declared, not
  witnessed-firing).
- plunit `test_seat_totality.pl`: **8/8**. Existing units unaffected: `commentary_census` 40/40,
  `oq86_extraction_commentary` 14/14.
- Commentary-grade: neither predicate is on the `dr_type` path; classification untouched.

## Note

`mandatrophy_gap` is the last unconverted R3 commentary member — convert if/when it becomes a census
source (extension point in `commentary_census.pl`). The substantive findings the totalized buckets
surface (e.g. 21 no_agent_seats, 5 extraction_unnameable) are the subject of **OQ-136**.
