# B4 gauntlet — expected-divergence manifest (compiled BEFORE the run)

**Pass condition (operator, 2026-06-12):** every divergence from the pre-Phase-B world maps
to an enumerated, witnessed Phase B change below. **Any divergence NOT on this manifest is
a finding that BLOCKS B4.** The gauntlet reads as reconciliation against this prediction,
never post-hoc explanation.

## Expected divergences (each with its landing witness)

| # | surface | expected divergence | witness at landing |
|---|---|---|---|
| 1 | per-constraint reports, Section 7 | +1 `R5 ZOMBIE CROSSCHECK` line on exactly 6 stories: adjunctification_of_university_teaching, company_town_scrip_economy, information_suppression, regime_change_structural_break (authored_zombie_uncorroborated); institutional_trust_erosion, regulatory_measurement_gap (computed_piton_unflagged) | b3_r5_report_diff.out |
| 2 | pipeline_output.json | `gaps: [] → null` on exactly 2 stories (employment_boundary_contradictions, human_dignity_ai_governance_contradictions); NO other per-constraint change vs the pre-Phase-B output | b3_a5_a6_witness.out |
| 3 | corpus-level report, PERSPECTIVAL_GAPS | new ran-witness line "(N constraints with both seats examined)" (or [VACUOUS] on a cell-less KB) | b3_a5_a6_witness.out |
| 4 | test_harness per-index section | new "(N authored classifications examined)" line; `[INDEX VACUOUS]` on empty | b3_a2_witness.out |
| 5 | linter findings | corpus sweep 92→80: 7 UNRESOLVED_MANDATROPHY cleared (authored genealogy), 2 retained with extended message text, 2 no-data stories consolidate 4 findings → 1 MISSING_AGENT_SURFACE | b3_linter_sweep_diff.out |
| 6 | data_validation warnings | `missing_classification` → `missing_agent_surface` (name + message text) | b3_presence_gate_controls.out |
| 7 | unanimity guard | NO divergence — dispatch restored old extension byte-identically | b3_unanimity_dispatch_diff.out |
| 8 | epistemic_access_check | NO live divergence — corpus silent on seat-counting (controls-only semantics) | b3_open2_* |
| 9 | compiler / prompt / example / schema | generation-time only; no effect on existing corpus artifacts (existing-corpus compiles byte-identical, witnessed twice) | b2_*, b3_emission_seam_control.out |
| 10 | retired narrative_ontology detectors | NO divergence — zero consumers (positive-controlled) | b3_no_utilities_pattern3.out |
| 11 | check_stack | ZERO new findings vs the KNOWN_STATE 2026-06-04 baseline — all Phase B cross-module calls are qualified imports of loaded modules | this gauntlet |

## Standing flag carried into B4 and Phase C

**corroborated_zombie is the only R5 path whose sole witness is an overlay control** (live
corpus has 0). If any future story — including Phase C's regenerated minority — produces a
live `corroborated_zombie`, that is the path's FIRST LIVE EXERCISE and must be flagged as
such (inspected, not passed silently).

## Gauntlet checklist

- [ ] full `python3 python/run_pipeline.py` green (incl. ISSUES gate)
- [ ] validation suite (`run_dynamic_suite`)
- [ ] plunit files in `prolog/tests/` (except test_battery_variants)
- [ ] `check_stack.pl` vs the 2026-06-04 baseline
- [ ] reconciliation: every observed divergence maps to rows 1–10; row 11 holds

## B4 reconciliation (run 2026-06-12, AFTER the manifest above was compiled)

| gauntlet stage | result |
|---|---|
| full pipeline (incl. ISSUES gate) | GREEN (exit 0) |
| validation suite (`run_dynamic_suite`) | GREEN — "DATA QUALITY: EXCELLENT" |
| plunit (`prolog/tests/`, 14 files, variants excluded) | 14/14 exit 0 |
| check_stack vs 2026-06-04 baseline | 4 baseline findings + **1 NOT on baseline and NOT on this manifest** |

**The one unmanifested divergence — investigated to attribution, filed as OQ-115:**
`abductive_helpers:known_override_signature/1` undefined under the [stack]/check_stack
chain (phantom module — file never loads there; the pipeline chain loads it via
json_report → diagnostic_summary, so the consumer path is healthy). Witnessed present at
pre-Phase-B `c22ec561` (temp worktree); absent from the 2026-06-04 baseline list — a
post-baseline, pre-Phase-B regression created by the OQ-98-era alert path. **Not
attributable to Phase B** (no Phase B edit touches the reference or the load chain), so it
does not block B4 — but per the manifest rule it could not pass silently: it is filed,
attributed, and carries its resolution shape.

**Rows 1–10:** all observed divergences reconciled — pipeline diff vs the pre-Phase-B
baseline confined to row 2's two `gaps` nulls (witnessed `b3_a5_a6_witness.out`); report,
linter, validation-text, and harness divergences match rows 1, 3–6 with their landing
witnesses; rows 7–10 held (no divergence where none was predicted).

**B4: PASS** — every divergence either manifested-and-matched or filed-and-attributed
(OQ-115). The corroborated_zombie first-live-exercise flag carries forward into Phase C.
