# OQ-98 close — the verdict banner becomes a join over the report's own evidence

**Date:** 2026-06-11. **Branch:** `oq98-verdict-join`. **Plan:** operator-approved
(`~/.claude/plans/lexical-wishing-badger.md`); operator rulings 1–3 recorded 2026-06-11.
**Commits:** `e8ab707b` (plumbing) → `170db693` (histogram gate) → `ce9a26ec`
(output-changing) → ledger/docs commit (this file's commit).

## The defect

`build_verdict_banner` consumed only `diagnostic_verdict` from enriched_pipeline.json, so
`VERDICT: GREEN / 12/12 subsystems — no tensions` printed over a 0%-authored grid and beside
`! ALERT [severe]: type_1_false_summit` (witness: OQ-98 entry,
`audits/2026-06-10_external_review_vote_market/`). Build Discipline spine: a success-shaped
token filled the hole where the provenance bit should be at the read site.

## Probes (Step 0, read-only)

- **P1 — grid dependency** (`p1_grid_dependency.pl`, `p1_witness.txt`): coverage precheck
  witnessed the constraint↔interval mapping (46 bijective pairs; 2 constraints
  unprobed-by-construction with no interval; 0 non-corpus intervals), then asserted a full
  synthetic 32-slot leveled grid per interval (probe ID `oq98_probe`, value 0.95), cleared
  memo caches, recomputed. **Result: BRANCH A** — 0/48 diagnostic summaries changed; positive
  control PASS (46/46 `report_generator:classify_interval/3` confidence low→high — the
  instrument sees a live grid consumer); restore verified (0 probe facts left; S2≡S0).
  ⇒ Operator ruling 1 lands on the per-question branch: CONDITIONAL tags on grid-fed
  findings, headline NOT gated by grid provenance.
- **P2 — load path** (`p2_load_path.pl`, `p2_witness.txt`): on the exact run_pipeline loader
  chain, `data_repair:grid_provenance(scale_ceiling, prov(0,0,0,32,32))` and the UNEXPORTED
  `data_repair:source_class/2` both callable module-qualified; `dr_mismatch(scale_ceiling)` =
  `type_1_false_summit-severe`. Gate for Commit 2: PASS.
- **P3 — recon** (`p3_recon_witness.txt`): `verdict_join` name free (positive control:
  `diagnostic_verdict` grep fires); `schema_version` single-writer/zero-reader; full
  `dr_mismatch/3` enumeration 0.034 s over the 48-corpus ⇒ perspectival clause kept, no
  `alerts_omitted` marker needed (plan risk 2 dissolved); m_gen/repair facts 0 live
  (injection confirmed dead, OQ-96 shim-off regime).

## What landed

- `signature_detection:signature_grade/2` — correction iff an override signature
  (`abductive_helpers:known_override_signature/1`) actually rewired the type at default
  context (`metric_based_type_indexed` ≠ `dr_type`); else commentary.
  `signature_severity/2`: correction → moderate; commentary never alerts.
- `diagnostic_summary:verdict_join/3` — Joined = max-badness(Base, floors: severe→red,
  moderate→yellow, informational→none); `cap_applied` token; alerts from
  `drl_core:dr_mismatch/3` + the signature alert; `grid_provenance` (null ≠ authored-0/32 —
  no-interval serializes null); `measurement_provenance` by `source_class/2`.
- `json_report.pl` serializes the join as a SIBLING of `diagnostic_verdict`
  (`diagnostic_verdict` byte-unchanged); `run_pipeline.py` schema_version 1→2;
  `shared/schemas.py` contract + structural validation.
- `report_generator.pl`: `[CONDITIONAL: grid authored A/T]` on the grid-diet line and the
  kappa diet tail when authored < total. Alert loop unchanged (raw evidence stays).
- `enhanced_report.py`: banner headlines `verdict_join.verdict`; capped banners print
  `BASE: … — CAPPED TO …` + one line per alert; grid line ALWAYS printed; stale artifacts
  render `[UNJOINED verdict — regenerate pipeline (OQ-98)]`; drift section gains
  `[CONDITIONAL: N/M measurement points non-authored — OQ-93/OQ-102]`; sidecar verdict =
  joined + `verdict_join` passthrough. Banner box now dynamic-width (min 51) — risk-3
  layout decision.

## Histogram gate (pre-output; `histogram_witness.txt`)

48/48 rows (`once/1` matches json_report's first-solution read — first run without it
inflated to 50/48, noted in the script): green→red 6, yellow→red 2, unchanged 40.
Alerts: type_1_false_summit[severe] 7, type_3_snare_as_rope[severe] 1,
signature_correction[moderate] 13, perspectival_incoherence[informational] 32.
**Severity=moderate confirmed conservative:** zero moderate caps (all 13 correction
carriers already base ≥ yellow); the yellow floor guards the future green+correction case.

## Witnesses (close)

- **W1** (`w1_w2_w3_banner_witness.txt`, `scale_ceiling_report.md` copy): scale_ceiling
  banner `VERDICT: RED / BASE: GREEN (12/12 …) — CAPPED TO RED / ! [severe]
  type_1_false_summit (claim_mismatch) / Grid: authored 0/32 …`.
  **W1b substitution (witnessed, not predicted):** the plan predicted agenda_conditioning
  for the moderate-alert witness; it grades COMMENTARY (signature `constructed_high_extraction`
  did not rewire snare→snare — alert correctly absent under grade-determines-wiring).
  `hybrid_security_reading` carries the visible `! [moderate] signature_correction` line.
- **W2:** `[CONDITIONAL: grid authored 0/32]` fires on the live 0-authored regime
  (scale_ceiling report line 230). Known edge: the kappa-tail tag is unreachable live
  (no grid ⇒ kappa DATA_INSUFFICIENT); it fires when a partially-authored grid first exists.
- **W3** (`w3_w4_falsifier2_witness.txt`): thermal_dissipation_constraint stays GREEN;
  A/B against the pre-Commit-2 banner code (`git show e8ab707b`) shows only the additive
  grid line (+ box widening).
- **W4:** 4/4 sidecar `verdict` == banner `VERDICT:` line (`w_checks.py`).
- **Falsifier 1** (`falsifier1_witness.txt`): in-session
  `assertz(constraint_claim(agenda_conditioning, mountain))` caps yellow→red
  (severe type_1 fires); retract + cache clear restores summary AND join byte-equal to PRE.
  (The plan's "clean green" target was unusable: the only live green,
  thermal_dissipation_constraint, has dr_type=mountain, so a mountain claim there is
  GENUINE and correctly raises no alert — the substitution is itself a correctness witness.)
- **Falsifier 2:** corpus scan — 13 correction-grade signatures, 0 without a join alert
  (non-vacuous).
- Chain re-certified: `verdict_join` present 48/48 in `pipeline_output.json` AND
  `enriched_pipeline.json` (enrich is an in-place field-adder; passthrough verified at run).

## Substrate pin

Live corpus, 48 testsets (`manifest.n_constraints` 48, run 2026-06-11, code commit at
`ce9a26ec` lineage); all counts above are this-corpus, this-commit. The 8/48 blast radius
is a corpus-relative figure — re-derive it on any other corpus rather than citing it.
