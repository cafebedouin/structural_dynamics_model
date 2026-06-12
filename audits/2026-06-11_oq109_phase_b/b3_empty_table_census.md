# Census: empty-authored-table behavior of every perspectives-table consumer

**Trigger:** two incidentally-found instances of fail-open-on-empty (operator, 2026-06-12).
**Probe:** every call-shaped read of `constraint_indexing:constraint_classification/3` outside
archives/testsets/demo-data (18 files, ~57 hits), each read in context and classified by its
behavior when the table is empty for a constraint — the state every post-Phase-C story is in.
**Positive controls:** the two known instances, re-found by the probe with their expected
classifications (#A1, #B1).

**Verdict: the class is NOT {2}.** Two subclasses; six silent members (2 known → 6), and the
known pair actually splits across subclasses — the FCR guard is silent, the invariance_check
emission is LOUD at suite time (reclassified below).

## Class A — silent fail-open (success-shaped absence; the seam class proper)

| # | site | empty-table behavior |
|---|---|---|
| A1 | `signature_detection.pl:1345-1347` `only_mountain_classifications/1` under `\+` in `appears_as_rope` | KNOWN. Guard false → negation succeeds → FCR loses mountain protection |
| A2 | `test_harness.pl:111-124` `validate_per_index/1` | vacuous `forall` — section header prints, zero rows, reads as clean |
| A3 | `narrative_ontology.pl:439` `check_indexical_relativity/1` | detector requires a mountain cell; silently inert on empty |
| A4 | `narrative_ontology.pl:469,483` `detect_omega(_,mandatrophy)`, `detect_mandatrophy_omega/1` | mandatrophy omegas silently not generated |
| A5 | `report_generator.pl:207-232` `detect_gap_pattern/2` | fails on empty → `"gaps": []` in pipeline JSON (measured-empty/didn't-look collapse at the key) AND omegas-from-gaps silently absent |
| A6 | `report_generator.pl:448` PERSPECTIVAL_GAPS section | inner goal fails → `; true` → empty section reads as "none detected" |

## Class B — loud false-alarm on legitimate perspectives-free stories (blocks adoption, not silent)

| # | site | empty-table behavior |
|---|---|---|
| B1 | `generate_constraint_pl.py` `_generate_tests` mountain `invariance_check` | KNOWN, RECLASSIFIED: emitted test FAILS at suite time (loud false alarm), not silent |
| B2 | `data_validation.pl:132,172` | warning + fail on legitimate new-format stories (scoped B3 item) |
| B3 | `linter.py:82+` OUTDATED_HOOK, MISSING_PERSPECTIVE, INSUFFICIENT_VARIANCE | fire on every new-format story (witnessed on the B2 example; scoped B3 item) |

## Class C — absence-tolerant by design (no action)

`data_repair.pl` claim-derivation priority chain (computed fallback);
`logical_fingerprint.pl:480` or-fallback over `constraint_claim`;
`report_generator.pl:288,301` `none`-token sites; `json_report.pl:344` raw authored dump
(A3-dispositioned: migrates/retires); `constraint_indexing.pl` query utilities
(fail-on-empty = correct query semantics, no gate); `reading_diff.pl` (R4 four-tuple-arm
instrument, authored-cells by design through Phase B); Python regex utilities
(`duplicate_checker`, `domain_priors` carry fallbacks; `fix_missing_claims` is legacy repair,
silent-skip on absence is its correct behavior).

## Disposition

- A1, B1–B3 already carried by the OQ-109 seam gate / B3 items.
- A3–A5 are ALREADY the A3-table migration items (`narrative_ontology` utilities;
  `report_generator` gap reports) — the census adds the explicit FAIL-CLOSED requirement:
  each migration must distinguish measured-no-gap from no-data (e.g. `gaps: null` vs `[]`,
  or a coverage bit), not just re-point the read.
- A2 (`validate_per_index`) and A6 (report :448) are NEW — folded into the seam gate.
- Every Class-A migration owes the seam positive control (perspectives-free story, shown
  fail-closed or computing over seats) per the gate as written.

## Closure (2026-06-12)

- A1 — closed (unanimity dispatch; seam control passes via C arm).
- A2 — closed (`validate_per_index` counts first, logs `[INDEX VACUOUS]` on zero; two-sided
  witness `b3_a2_witness.out`).
- A3/A4 — closed by RETIREMENT (zero consumers, dead exemption legs; tombstones point at
  R5/FSM/T17 successors; pipeline identity `b3_no_utilities_pattern3.out`).
- A5 — closed (`gaps` carries the coverage bit: null = didn't-look, [] = measured-empty;
  validator extended; output diff confined to the 2 no-cell stories,
  `b3_a5_a6_witness.out`).
- A6 — closed (PERSPECTIVAL_GAPS section carries its ran-witness count / explicit vacuity
  line; rendered witness in `b3_a5_a6_witness.out`; the vacuity branch shares A2's
  witnessed construction — the empty-corpus side was not separately exercised).
- B1 — closed (compiler emission gated, two-sided, `b3_emission_seam_control.out`).
- B2 — closed (`agent_surface_present/1`, 5 two-sided controls).
- B3 — closed (linter dispatch; example 5→0; sweep 92→80 fully decomposed,
  `b3_linter_sweep_diff.out`).

**The census class is fully discharged.**
