# ε census (OQ-205 spec recon) — read-only, 2026-07-03

**Purpose:** inform the stability-radius recommendation (R3) in
`docs/design/epsilon_declaration_discipline.md` §5 with the live distance-to-threshold
distribution, and re-baseline the OQ-78 grid statistics in passing. No engine change; no
corpus write.

**Code state:** commit `6c59615e` (clean engine; pre-existing uncommitted working-tree noise
unrelated to ε paths). Corpora: `testsets` (live), `testsets_haiku`, `testsets_flash`
(`corpus_path` overlay, retractall+asserta), `archives/datasets/kernel_v1`.

**Method:** `eps_census_dump.pl` — per leg, one swipl process loads the corpus and emits
`id<TAB>ε` per `corpus_loader:corpus_constraint/1` via the live read path
(`constraint_data:base_extractiveness/2`); `no_eps` on read failure.
`eps_census_analyze.py` — min-distance to the five ε-gating thresholds
{0.10, 0.25, 0.30, 0.45, 0.46} per candidate radius {0.001…0.05}, plus OQ-78 stats.

**Positive control (ran FIRST; the analysis halts if it fails):** a constraint planted
**in-memory only** at `snare_epsilon_floor + 0.0005 = 0.4605` on the live leg, asserted after
corpus load so it flows through the same enumeration + read path as every real value. δ was
chosen inside the smallest candidate radius (planting at threshold + r/2 would presuppose the
r the census exists to inform). Result: emitted in the dump
(`eps_live.tsv: census_planted_control 0.4605`) and flagged at all 6 candidate radii ≥ δ —
**CONTROL PASS**; excluded from all statistics. Nothing was written under `prolog/testsets*`.

**Results:** `eps_census_result.txt` (raw). Headlines:
- **flash authors ε exactly ON thresholds**: 218/960 (22.7%) at distance 0.000
  (0.45×100, 0.30×69, 0.25×26, 0.10×23 — its .x5/.x0 grid lands on threshold values).
  Live/haiku/kernel_v1 (.x8/.x2 rail): 1/9/4 exactly-at respectively.
- **(0.45, 0.46) open interval EMPTY on all four legs** — the tight-radius binding constraint
  from the plan is moot on current corpora (re-check at any regeneration; kill condition on R3).
- within-0.02 band: ~10% on the .x8-rail legs (the 0.48-vs-0.46 class); within-0.05: 18–45%.
- OQ-78 re-baseline: live mode 0.68 share 41.8% (46/110, n grew 91→110); flash mode 0.65
  (19.2%) — the rail is model-specific (flash last-digit: 5×599, 0×326; haiku: 8×598).
- live-leg 9 `no_eps` = the `*_contradictions` axiom meta-files (OQ-136/OQ-202 strata) —
  measured-empty, not didn't-look.

**Consumer:** spec §5 census table + R3 (r = 0.02, committed with kill conditions); §8 OQ-78
readout prototype.
