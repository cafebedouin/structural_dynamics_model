# OQ-91 — repair-transition detector + report surface — BUILT + WITNESSED

**Date:** 2026-06-26. Close-state 1 (operator-approved). Evidence in this dir.

## B0 — rulings recorded (substrate)
`docs/repair_dynamics.md` §8 + §6 pointer; `ISSUES.md` OQ-91 → resolved. Three
operator rulings: (1) dedicated repair-named `repair_transition/4`; (2)
commentary-grade; (3) `maintain/splice/replace`/`scaffold_struck` named ops +
report surface in `enhanced_report.py`.

## B2 — `repair_transition/4` (transition_paths.pl)
Upward dual of the 8 decay heads. Reuses `degradation_chain/3` (snapshot_type
series) as source — NOT re-derived. "Upward" = transitive closure of the 8
`transition_path/4` decay edges, read backwards (`unknown` excluded). Op = function
of from/to + chain prefix: round-trip→`maintain`, scaffold→`scaffold_struck`,
snare/piton/false_mountain→`replace`, tangled_rope→`splice`.

- Standalone (testsets/): lycurgan snare→tangled_rope [replace], shinbutsu
  snare→rope [replace]; decay-only `apoe4` [tangled_rope,snare] yields NONE
  (`b2_standalone_test.log`).
- kernel_v1 op tally: maintain-11, splice-1, replace-18, scaffold_struck-0 (3/4 ops
  fire; scaffold_struck is a real-but-unexercised head) (`b2_kernel_v1_op_tally.log`).
- **Bug found + fixed:** `repair_op` first version guarded clauses on a bound 4th
  arg, so `repair_transition(_,snare,tangled_rope,splice)` wrongly succeeded via the
  default clause (cut never reached). Fixed to drive clause selection on from/to/pre
  and unify Op in the body → a true function of inputs, correct under bound queries.
  Regression: op-bound queries correct, unbound tally unchanged, deterministic
  (one op per step).

## B3 — report surface (enhanced_report.py)
`build_repair_section` consumes the `repair_transitions` field (serialized in
`json_report.pl` via `write_repair_array`, hermetic `preserve_classify_globals/1`
wrapper so snapshot_type's nb-globals cannot leak). Single data direction
Prolog→field→Python; no Python recompute. Q6-framed. Silent on decay-only/flat
(honest absence). lycurgan report renders the section; apoe4 renders none.

## B4 — two-sided + invariant witness
- Positive: lycurgan/shinbutsu render the upward transition + correct op.
- Negative: apoe4 (decay-only) renders nothing (no false-positive repair).
- **Invariant (`b4_invariant_diff.log`): PASS.** `pipeline_output.json` per_constraint
  (104), diagnostic, validation, config, type_hierarchy all byte-identical with vs
  without repair; the ONLY change is the added `repair_transitions` field. Both runs
  exit 0. Additive-only confirmed.
- Suite: 0 errors (1 pre-existing corpus warning); snapshot-migration 10/10; warning
  gate 3/3 allowlisted, 0 unexpected.
