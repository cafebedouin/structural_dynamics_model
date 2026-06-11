# OQ-46 Backed reconciliation — bucketed semantics, explicit sanction marker, OQ-105 spin-off

**Date:** 2026-06-11 (same-day follow-on to the OQ-46 close, `audits/2026-06-11_oq46_close/`,
commit `9a13b0b4`). **Substrate:** live corpus, 48 testsets / 46 stories, worktree branch
`oq46-ruling` from `9a13b0b4`. **Probes:** `evidence/probe1..8*.pl` with raw outputs alongside;
every census ran with in-process positive controls (corpus_constraint=48, constraint_metric=206,
measurement=576 — all non-zero, validating the zeros).

## What this audit settled

The OQ-46 close ruled the scalar suppression read SANCTIONED but left `Backed=false` on **all**
scalar-supplied rows. This pass split the 47 scalar rows and measured what each Backed semantics
does at the read site; the operator then ruled **bucketed with an explicit sanction marker**, and
the build landed (commit `b0a0e380`).

### Row decomposition (probe2; reproduces the close's numbers exactly)

209 rows = 162 temporal + **26 seriesless-scalar** (7 constraints, all type-uniform across their
timelines — probe5: 5× scaffold, 2× mountain) + **21 misalignment** (10 series-authoring
constraints; off-grid times come from `base_extractiveness`/`theater_ratio`) + 0 unknown.

### Bucketed vs blanket (probes 3, 4, 8 — the decision witness)

- **Bucketed** (seriesless-sanctioned rows back; misalignment stays excluded): flips **59→59**,
  fab_adjacent **20→20** — zero motion delta. Only `backed_times` rises (7 constraints × 4
  contexts; 0→104 row-context pairs).
- **Blanket** (any authored scalar backs): flips **59→79**, fab_adjacent **20→0** — every
  excluded transition graduates into the real-flip count that decides the OQ-83 D-fork,
  including the substitution-dated flips below. Rejected.

### The misalignment substitution is anti-causal and sets flip timing (probes 5, 6)

The scalar ≈ series **endpoint** corpus-wide (probe6: 39 dual-representation constraints,
**37 exact scalar==endpoint, 2 within 0.05, 0 violations**). So off-grid substitution injects the
end-state suppression value at earlier times. **Witnessed (2 of 3 checked row-by-row):**
`substantive_employment_reading` flips tangled_rope→snare at T=9 on substituted 0.67 (own series
0.58 at T=6, 0.67 at T=12); `post_1998_convergence` flips at T=13 on substituted 0.72 (own series
0.48 at T=9, 0.65 at T=18). **Checked-negative (1 of 3):** `truth_democracy_disinformation` flips
at T=4 on a series row. **The mechanism predicts more of the 21 rows are affected — that is a
prediction, not a witness; the remaining rows have not been swept.** Filed as **OQ-105**.

Correction to the close-session evidence: phantom transitions under scalar-clause deletion would
surface via `temporal_residual` (`json_report.pl:483`), not `drift_trajectory`
(`write_drift_trajectory`, `json_report.pl:728`, emits raw measurement series only — no types).

### Endpoint lint disposition (probe6, pre-registered)

Criterion pinned before the run: violations > 0 → standing-lint question stays open; = 0 →
closed-no-demonstrated-content. Result: **0 violations** → closed-no-demonstrated-content. The
earlier 8/39 scalar-above-series-MEAN divergence is the expected signature of the endpoint
convention under rising trajectories, not a data-quality signal. Re-run probe6 before re-opening.

## What landed (commits on `oq46-ruling`)

1. **`00040bb9`** — OQ-37 piton honest line: `data_validation`'s piton check joined over
   `resistance_to_change` (0 facts in every corpus ever — probe1 + grep with positive control on
   sibling name `resistance`, 34 live facts) and printed "✓ No pitons detected" unconditionally
   inside `validate_all`, run by the auto-generated `validation_suite.pl` every pipeline. The pass
   now carries its witness; empty table prints a VACUOUS notice
   (`evidence/w1_piton_vacuity_line.txt`). Heuristic removal stays gated on OQ-90.
2. **`b0a0e380`** — bucketed Backed (output-changing): compiler-stamped
   `narrative_ontology:suppression_profile(C, static)` emitted only on positive-control absence
   (other series authored, suppression deliberately omitted — never bare emptiness);
   `classify_at_time` three-way `SuppBacked` (sanctioned-static backs / misalignment excluded /
   **unmarked seriesless fails closed**); the 7 seriesless testsets recompiled from their JSON
   sidecars — per-file diff was exactly the marker fact + multifile declaration, zero generator
   drift. Witnesses: `evidence/probe7_w2_witnesses_output.txt` (synthetic unmarked-seriesless →
   `Backed=false`; marked → `true`; misalignment rows T3/T9 stay `false`),
   `evidence/probe8_live_flip_census_output.txt` (59/20 unchanged, backed_times 0→104),
   `evidence/pipeline_ab_diff_output.txt` (30 diffs = 28 backed_times + 2 manifest; baseline =
   stashed clean tree at `00040bb9`, hence `code_dirty False→True`; full JSONs alongside).
3. **`609dbb47`** — ISSUES.md: OQ-46 ruling block amended (bucketed follow-on, lint disposition),
   OQ-105 filed with witnessed-vs-predicted scope kept separate.

## Evidence index

| File | What it witnesses |
|---|---|
| `probe1_oq44_gate_census*` | OQ-44 gate-class census on live corpus (intent_* all 0; agent_beneficiary 161; resistance_to_change 0) |
| `probe2_row_split*` | 209 = 162/26/21/0 row decomposition + constraint lists |
| `probe3_bucketed_counterfactual*` | bucketed semantics: zero flip/fab_adjacent delta (post-change run: live ≡ bucketed) |
| `probe4_blanket_counterfactual*` | blanket semantics: 79/0 — the laundering witness |
| `probe5_flip_site_rows*` | per-row supp source/value/type at the 3 checked misalignment constraints + type-uniformity of the 7 |
| `probe6_endpoint_lint*` | scalar==endpoint 37/39, 0 violations (pre-registered) |
| `probe7_w2_witnesses*` | fail-closed control, marker backing, misalignment exclusion |
| `probe8_live_flip_census*` | live 59/20 + backed_times 104 post-change |
| `pipeline_ab_diff*`, `pipeline_output_{baseline,postchange}.json` | W2 witness 4: output diff = exactly the derived expectation |
| `w1_piton_vacuity_line.txt` | the OQ-37 honest line in the running suite |
