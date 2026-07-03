# OQ-138 FNL conversion — twin pipeline OLD-vs-NEW diff reconciliation (2026-07-03)

**Commit under witness:** `d248a6b1` (Commit 1: route + fnl_routed/1 + discriminated severity).
Commit 2 (seat_overrides + maxent seat-aware boost) NOT yet landed — the maxent boost at FNL
seats is still legacy-unconditional in every run below. **Code state:** all runs at `d248a6b`,
`code_dirty=True` throughout = the pre-existing `validation_suite.pl` +
`cs_reading_relation_quarantine.json` working-tree dirt (not engine edits); the OLD arms
additionally carried the one-line lever flip `false_natural_law_override_enabled 0→1`, restored
byte-identical afterward (`git diff --quiet` witnessed).

**Method.** OLD = ablation lever `false_natural_law_override_enabled=1` (legacy overwrite);
NEW = 0 (route, committed default). `classify_corpus` per leg into non-canonical outputs
(`outputs/fnl_{old,new}.{testsets,haiku,flash}.json`, gitignored — the projections in this dir are
the durable record). Projection carries the FULL verdict surface (claimed | sig | base | join |
cap | grade | alerts | sheaf | h1 | diag | tensions), not signature-grade alone (the FSM lesson).

**OLD-arm validity anchor (non-circular):** the lever=1 testsets arm is byte-identical at
`per_constraint` (0/119 records) to the canonical pre-conversion baseline
(`pipeline_output.json` run 2026-07-03T17:45:35Z at code `006f1d0`) — the lever restores legacy
behavior exactly, witnessed against an artifact produced before the conversion existed.

**Determinism control (two-sided):** NEW-flash re-run at identical config →
`flash_new_vs_new2` changed=0/960. The pipeline is deterministic at fixed input; every delta
below is CAUSED by the route, none is run-to-run noise.

## Headline counts (projection surface)

| leg | changed | FNL seats | spillover |
|---|---|---|---|
| testsets | **0** | 0 | 0 |
| haiku | 14 | 6 | 8 |
| flash | 39 | 8 | 31 |

## A. The 14 routed FNL seats — every one reconciled as the intended unmask

**8 of 14 render RED (4 haiku + 4 flash), not the census's predicted green→yellow.** The plan
budgeted for exactly this (the FSM lesson): the census's "green→yellow" was a prediction from the
verdict decomposition under the OLD engine; the diff is the measurement.

- **6 yellow-stable** (beta_designation, magna_carta_1215, commerce_clause, honor_settlement,
  reformation_event_boundary, shinbutsu_syncretic): base green→yellow (the honest base verdict on
  the routed scaffold/snare type), join yellow either way — the cap source shifts from
  `moderate_alert` to the base itself. h1/sheaf unchanged.
- **8 green/yellow→RED** (haiku: first_amendment_absolutist, quantum_copenhagen,
  salic_prohibition, separation_of_powers; flash: competence_occupation, monopoly_rulebook,
  sacrifice_obligation, vaccine_mandate — the flash four are exactly the high-ε snare-routed
  seats named in the plan). Two co-moving mechanisms, both Position-A honest:
  1. **`type_1_false_summit` informational→severe** — `dr_claim_mismatch` grades on the metric
     outcome; claimed `mountain` over a computed `snare` is the severe type-1 false-summit case.
     Under the overwrite the computed type read `tangled_rope`, which kept it informational. This
     is the standing claim-mismatch subsystem doing its job on the now-honest type.
  2. **h1 0→3 (separation_of_powers: 0→2), sheaf genuine/fragile→`manifest_presheaf`** — the
     override applied at EVERY context, flattening the whole orbit to tangled_rope (a
     manufactured global section, H¹=0). Routed, the real cross-position disagreement surfaces
     as a manifest obstruction. The overwrite was masking H¹, exactly the FSM/CI-rope
     consumer-side story (there via dirac; here via cohomology).
- The routed-but-yellow seats carry NEW `tensions=[maxent;signature]` — partly the STALE
  unconditional maxent boost still pushing tangled_rope at routed seats; Commit 2's seat-aware
  skip is expected to clear the maxent limb. Re-witness after Commit 2.

**No seat lost its diagnostic:** all 14 keep `signature_correction:moderate` (vic>0 across all
routed seats) and grade `correction` — the route annotates, never drops.

## B. Live leg — verdict-null, but NOT byte-null (a census scope correction)

Projection changed = **0/119**. But record-level, 89/119 differ, and the cause is a genuine
finding: **`organization_floor_c0` ("type-inert" per the census) is inert only at the DEFAULT
(analytical) context.** At the **institutional gauge position** its per-position type routes
`tangled_rope→scaffold`, changing `perspectives`, `fingerprint_shift`, `transition_boundaries`,
and its `arakelov_height` (0.158→0.305). The census's TYPE-INERT column was default-context-scoped
(`metric_based_type_indexed → dr_type` at ONE context); the override was live elsewhere in the
orbit. The other 88 records move only in corpus-relative ensemble diagnostics
(`wasserstein_incomparable_mass` ×84, `arakelov_height` ×45, `signature_pressure` ×28) — the
one-seat orbit change re-centers corpus-level distributions. Verdicts, types, signatures, grades:
0 changes. Full attribution: canonical baseline (006f1d0) vs lever=1 arm = 0/119, so this 89 is
entirely the conversion, and commit `823b6789` contributed nothing.

## C. Twin spillover (8 haiku + 31 flash non-FNL seats) — ensemble refit, mechanism-attributed

All spillover seats are maxent/ensemble-mediated; **anomaly screen: zero spillover seats changed
signature, claimed_type, or signature_grade.** `maxent_probs` move on all 960 records (corpus-
relative refit after 6–8 seats change type); only threshold-crossings surface in the projection:

- flash: 12 maxent-tension add/drop only; 7 green→yellow, 7 yellow→red, 5 red→yellow (both
  directions — a re-centering, not a systematic harshening) — all with the maxent tension
  appearing/disappearing as the base-verdict tension count crosses its threshold; 5 seats flip
  `maxent_top_type` at a rope/scaffold/tangled_rope boundary (shadow classifier only, dr_type
  untouched).
- haiku: 4 maxent-tension drops, 3 sheaf genuine↔fragile flips at h1=0 (the fragile/genuine
  discriminant rides ensemble-relative diagnostics, not H¹), 1 context_gap tension add.

These verdicts sit on FCR seats whose maxent boost is corpus-relative; they are downstream of the
honest type distribution and carry no FNL machinery. They are the same class of movement any
corpus-content change produces (ensemble refit, the OQ-112/known mechanism), here triggered by an
engine-regime change — cite counts across these runs with BOTH corpus and code state named.

## Verdict-direction bottom line

The unmask direction is **red-ward on the routed seats themselves** (8/14 RED, all reconciled as
honest: severe claim-mismatch + surfaced H¹ obstruction), **neutral on the live leg** (0 verdict
changes), and **bidirectional re-centering on twin bystanders** (7 up, 12 down at flash+haiku
verdict level). Nothing in the diff is a defect signature: no dropped diagnostics, no spillover
type/signature/grade changes, no non-determinism.

**OPEN at this gate:** Commit 2 (seat_overrides + maxent seat-aware skip) will move maxent
fields again (routed seats lose the stale tangled_rope boost; under lever=0 the inert/abstain FNL
seats also lose it — the plan's clause shape). The twin NEW pair must be re-projected after
Commit 2; the graduation witness for the full conversion is that post-Commit-2 state.
