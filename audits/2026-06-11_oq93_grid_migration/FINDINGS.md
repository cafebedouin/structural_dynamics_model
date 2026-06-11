# Findings — OQ-93 grid migration (executed 2026-06-11)

**Verdict: migration LANDED end-to-end, every stage same-commit witnessed.** Stages A–D +
the coverage-carrying half-step + shim retirement, with riders OQ-102(a)/(b) as separate
commits inside their stages and the OQ-101 ledger as terminal consumer. OQ-93, OQ-96,
OQ-101, OQ-102 resolved; OQ-106 filed for the deferred intent sub-fork. Preregistration
(`PREREGISTRATION.md`, committed before any write pass) carried the operator's typed κ-gate
ruling: split tolerance by indicator kind, N=10, pass = necessary-not-sufficient for the flip.

## Witness index (all in this directory; per-case detail inside each file)

| Stage | Witness file | Result |
|---|---|---|
| A schema | stage_a_schema_witness.txt | 13/13 battery; 143-file additivity sweep, 0 deltas |
| A rider basis | stage_a_rider_basis_witness.txt | 16/16; sweep 0 deltas |
| B compiler | stage_b_compiler_witness.txt | 8/8; 143/143 byte-identical; duplicate REJECT both CLI paths |
| B rider basis | stage_b_rider_basis_witness.txt | 6/6; fixture → meas_prov(39,0,0,2,39) |
| Coverage read | phase3_coverage_read_witness.txt | 9/9 two-sided (8/32→OPEN; 5 probe pins exact; suite green, 48/48 INTENT OPEN) |
| C batch + κ | kappa_audit_run.txt (+ _prefix.txt = bug witness) | PASS 0/10 excluded, 0 echo, 0 flat/dir |
| D consumers | stage_d_consumer_witness.txt | 11/11 (signal two-sided; FCR/FSM end-to-end; CONDITIONAL 16/32 tag FIRED; cap why-not: 0 correction-grade carriers) |
| (b) rider | rider_b_drift_join_witness.txt | 3/3 ([warning \| confidence: low] live; projected caveat new-only) |
| Shim retirement | phase6_suite_before/after.txt | 0 unclassified diff lines; class counts identical; residue grep 0 + positive control |
| Ledger | phase7_ledger_witness.txt | 8/8 (48/48 blocks; fidelity clean; fixture 'projected 2/39' on drift line) |
| Final pins | final_pin_check.txt | all five probe pins exact at final HEAD; 8/32 OPEN |

## Two bugs the migration itself found (the probe pattern repeating)

1. **Scalar-series times poisoned the gradient next-point** (`time_point_in_interval/2`):
   a story authoring BOTH a grid and mid-interval scalar series read open(no_gradient_data)
   on a full 32-point grid — witnessed on all 10 batch stories (`kappa_audit_run_prefix.txt`,
   the pre-fix run kept as the bug witness; the audit's G-value output is the positive
   control that the engine read dispatches). Fix: compound(Metric) guard — grid times are
   grid-measurement times. Probe stories had masked it (no scalar series).
2. **Clause-interleaving load warning** caught by the warning gate (the OQ-96 instrument
   catching a defect in the OQ-93 work): `confidence_rung_up/2` initially landed between
   `signature_confidence/3` clauses; relocated.

## Deviations from preregistration (surfaced, not absorbed)

- **Phase-6 "0-diff" read as behavior-identity, not byte-identity:** the before/after suite
  diff is 232 lines, ALL classified — the two retirement-message rewordings (the old text
  named the now-retired flag, which would have been a lying message) and [ELAPSED] timing
  noise; per-class counts identical (FAIL 0/0, OPEN 513/513, SHIM 48/48, INTENT-OPEN 48/48).
- **C-echo operationalization extension:** with no worked value table in the prompt (by
  design), C-echo's "duplicates the prompt example" clause is vacuous-by-construction;
  cross-story identical value tuples were added as the live form of "the prompt taught a
  convention" — pinned in the audit script BEFORE the batch was read, recorded here.

## Pending operator (recorded in ISSUES.md OQ-93 + KNOWN_STATE)

The live-prompt flip to opt-in-by-story-focus and the promotion of the 10 grid-batch stories
(`grid_batch/json|pl/`) into the corpus — one ruling; the N=10 PASS is necessary, not
sufficient, by the operator's own provision (supplemental batch optional).
