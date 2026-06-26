# OQ-18 — temporal first/last reduction: witness set

**Date:** 2026-06-25. **Closes:** OQ-18 (behavior-preserving). **Spawns:** OQ-183, OQ-184, GAP-21.

The four collapsing temporal predicates in `prolog/metric_drift_events.pl` reduce a full
`measurement/5` series to endpoints (or first-3-points). OQ-18's premise — "safe because the
output is only a boolean gate" — was **partly false**: two of them reach a SERIALIZED verdict
in `pipeline_output.json`. This directory holds the read-only probes that settled whether the
reductions actually corrupt those verdicts (they do not, yet), plus their re-run output.

## Probes (read-only; corpus via argv, all run on all three live legs)

- `oq18_flipped_probe.pl` — replicates `network_dynamics:network_drift_velocity/4` EXACTLY,
  swaps only the per-neighbor rate source from endpoint `drift_velocity/3` to the faithful
  least-squares slope (`drl_composition:linear_slope/2`), recounts serialized `cs_drift_mismatch`
  verdicts whose gate outcome flips. Live control: an uneven-spacing series where endpoint≠lsq.
  **This is the OQ-184 kill-condition prototype** (`faithful_ndv` = `sum_list` over `Rate>0`
  contributors; the gate compares the SUM, so headroom is sum-level).
- `oq18_divergence_probe.pl` — per-neighbor endpoint-rate vs lsq-rate divergence + point-count
  histogram (guards the 3-evenly-spaced-points degeneracy where lsq≡endpoint).
- `oq18_metric_trend_flip.pl` — does `metric_trend/3`'s net-change bucket flip a serialized
  `cs_verdict(scaffold_suppression_escalating)` vs an LSQ-fitted total change? Binds `C` from
  `corpus_loader:corpus_constraint/1` first (the mandatory positive-control lesson: an unbound
  `cs_verdict/2` query returns a false 0 because `dr_type/3` cannot generate `C`).
- `oq18_realized_probe3.pl` — census of serialized `cs_drift_mismatch` verdicts whose velocity
  sum includes a non-monotone contributor (the endpoint reduction that feeds gate conjunct 2).

Run: `cd prolog && swipl -g "halt" ../audits/2026-06-25_oq18_temporal_reduction/<probe>.pl <leg>`

## Settled findings (re-witnessed 2026-06-25; raw output in `*.out`)

| Probe | testsets | testsets_haiku | testsets_flash |
|---|---|---|---|
| `drift_velocity`→`cs_drift_mismatch` flips | **0** (of 14) | **0** (of 169) | **0** (of 18) |
| max faithful SUM (gate quantity) | 0.006745 | 0.007851 | 0.004333 |
| gate-live / non-mono-contributor verdicts | 3 / 0 | 56 / 29 | 10 / 6 |
| `metric_trend`→`scaffold_suppression_escalating` diverge | 0 (of 14) | 1 (of 52) | 17 (of 43) |
| per-neighbor series endpoint≠lsq | 86/97 | 890/954 | 639/949 |
| max per-neighbor \|Δrate\| | 0.0011 | 0.0057 | 0.0067 |

**Verdict:** the endpoint reduction is real and the gate is *exposed* (consults non-monotone
contributors), but **NO serialized `cs_drift_mismatch` verdict is currently wrong on any live
leg** — 0 flips; closest headroom `Thresh − max faithful sum = 0.01 − 0.007851 = 0.00215` on
`testsets_haiku`, a SUM-level margin (the gate's exact quantity). `metric_trend`'s 0/1/17
divergences are at the ±0.05 net-change vs sustained-trend boundary — a semantic seat (OQ-183),
not a correctness bug. The haiku flip is `nicene_creed` Δ=0.08 vs fit 0.0207.

**Note (corpus drift):** the original plan recorded the haiku headroom as 0.0018; this re-run
gives 0.00215 — the corpus changed since the plan was authored, which is exactly why the
docstrings cite re-witnessed numbers rather than the plan's. The 0-flip qualitative verdict is
unchanged.

## Ruling

OQ-18 closes **behavior-preserving**: the collapsing predicates are annotated with
reduction-kind + faithful-source + a falsifier (NOT "safe-as-gate"); the dead zero-caller
`drift_acceleration/3` is deleted (GAP-21 logs the faithful capability); and the actual fixes
are routed — OQ-184 (faithful least-squares `drift_velocity`, output-changing, carries the
sum-level kill-condition tripwire prototyped in `oq18_flipped_probe.pl`) and OQ-183 (the
`metric_trend` net-change-vs-sustained-trend semantic seat + cross-module name collision).
