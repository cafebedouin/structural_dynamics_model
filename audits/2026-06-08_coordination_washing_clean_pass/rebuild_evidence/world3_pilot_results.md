# world3 regeneration pilot — results (2026-06-09)

Pilot of the require-metrics-at-source fix. Article: `agent/analysis/originals/world_model3.md`.
Run: `DR_TEMPERATURE=0 c-orchestrator.py --input-file ... --skip-search --skip-essay`
(web search skipped — it hung on the API ~3.5 min with no response in this environment; research
grounding does not affect metric authoring, the pilot's target).

## Engine + schema + prompt gate (pre-regen, all witnessed)
- V0: baseline diff pinned; apparatus real (`with_overlay/3`, `clear_all_caches/0`,
  `cached_coupling/2`, `cached_classification/3`).
- V1: schema rejects each field independently — omit-`accessibility_collapse` → rejected;
  omit-`resistance` → rejected; complete → accepted. `_basic_validate` fallback made consistent.
- V2: every metric-comparison site grep-enumerated; 4 signature predicates carry in-clause
  `number/1` guards; `compute_signature_confidence` gated by `profile_numeric` at its sole entry
  `signature_confidence` (incl. the external caller `report_generator.pl:348`); `classify_by_signature`
  Extraction + `get_constraint_profile` `=`-unification are category-(i)/non-arithmetic. Empirical
  witness: **0 throws across the 18 corpus constraints + probe_bare + probe_empty**.
- V3: two-sided. probe_empty → `unknown` with `profile_metrics_authored=false` (path exercised);
  the 4 fully-vectored constraints classifiable pre-guard are byte-identical post-guard
  (anti-over-abstain control); under-authored constructed_high → `unknown` (correct fail-closed).
  Baseline A (0.5-era) captured + hashed: `baseline_A_0p5_era.tsv`
  sha256 71fa78f3...46e09b. Post-guard sweep: `postguard_classification.tsv` sha256 ab686ad8...c24130.
- V6 pre-flight: full `run_pipeline.py` completes (no downstream throw on the 9 unknowns).

## Pilot regeneration (V4/V5/V6)
- Fresh decompose chose 4 axes: proxy_measurement_validity, regime_change_structural_break,
  recalibration_interpretive_validity, collapse_timing_uncertainty. (Only `proxy` overlaps the
  prior world3 set — re-decompose is lossy; optimization_artifact_risk + collapse_mechanism_ambiguity
  orphaned, left in place per operator ruling.)
- **V4:** all 4 regenerated testsets author BOTH `accessibility_collapse` and `resistance`
  (e.g. proxy: extractiveness 0.08, suppression 0.12, accessibility_collapse 0.88, resistance 0.05).
  None abstain to `unknown`.
- **V4 verdicts (C):** proxy→coupling_invariant_rope (ε0.08); regime_change→false_ci_rope (ε0.28);
  recalibration→constructed_high_extraction (ε0.48); collapse_timing→constructed_high_extraction (ε0.68).
- **V5 (deterministic substitution, `with_overlay/3`, caches auto-cleared):** for each, B = swap
  accessibility_collapse/resistance → 0.5 (reconstructs the 0.5-era fill), hold ext/supp/theater.
  Result: **B == C for all 4** → the formerly-defaulted metrics do NOT move these verdicts. The
  verdicts are extraction/suppression-driven; the 0.5 default was not silently corrupting them
  (it fabricated only when extraction itself was unauthored, and threw once removed). Fix value is
  structural: no throw, no fabrication path, metrics present for NL/mountain certification.
- **V6:** post-regen pipeline 41/41 steps OK in 3.0s.

## Conclusion
Fix validated end-to-end on the pilot. Schema now requires the two metrics; prompt instructs
honest authoring for all types; engine fail-closes (`unknown`) instead of throwing/fabricating on
absence. china + magnifica full re-runs follow (operator ruling: continue full re-runs).
