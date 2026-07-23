# 0b consumption reclassification table (2026-07-23)

Per the plan: site | classification | how classified (the consumer grep, pasted) | justification.
Rule: a descriptive stat feeding a threshold is dispositive-by-consumption.

| site | classification | consumer evidence (grep, pasted in session) | justification / action |
|---|---|---|---|
| `json_report` `purity_summary` (`/diagnostic/`) | DESCRIPTIVE | `python/reports/queries/meta_reporter.py:75,137` — `diag.get('purity_summary', {})`, re-emitted verbatim into the meta report; no threshold consumes it | Band tally over scorable rows; gains `purity_n_scored`/`purity_n_total` siblings (153/199 on testsets, witnessed additive-only: per_constraint 199/199 identical, exactly 2 new diagnostic keys) |
| `giant_component` purity distributions (`:901-903` feeders → `distribution_stats`) | DESCRIPTIVE | `.md` report only — no python/prolog consumer greps the "Purity Within Giant Component" lines (grep empty, control: the emitting format string found at source) | Coverage line added: `intrinsic n/N scorable, effective n/N scorable` (unconditional, R4) |
| `maxent_report` Entropy-vs-Purity (`:372-393`) | DESCRIPTIVE | `.md` report only (grep for "Entropy vs Purity" outside the module: empty) | Unconditional coverage line added above the table |
| `grothendieck` H1-vs-purity per-band means (`:791-798`) | DESCRIPTIVE | `.md`/stdout report only | Per-band `n_scored=n/N_band` (band total via `cached_obstruction/3` count) |
| `maxent_diagnostic` `count_low_purity`/`avg_purity_for` (`:603-630`) | DESCRIPTIVE — **already labeled** | print site `:566-575` emits `PURITY_AVAILABLE_HIGH/LOW: n/N` adjacent to the averages | No edit: coverage denominator already carried at the read site |
| `drl_purity_network` `cluster_purity/3` weighted mean | DESCRIPTIVE, **DORMANT** | exported but zero consumers (`grep -rn cluster_purity` hits only the defining module — pasted) | No read site to label; noted as dormant API. Its `Pairs=[] → 0.0` empty-default is pre-existing Pattern-6 residue on a dead surface — routed to close notes, not edited (no witnessable consumer) |
| `network_stability_assessment` `stable` | **DISPOSITIVE** (negative existential over the whole network) | consumers: `metric_drift_report.pl:116` (report), `json_report.pl:1713` → `/diagnostic/network_stability` (resolves via import; value witnessed in fresh JSON) | Coverage-1.0 gate added: zero-drifting + any member with non-numeric effective purity ⇒ `undetermined` (distinct abstention), never `stable`. `cascading`/`degrading` stay existential (fire through unknowns). Two-sided witness pasted: synthetic corpus `stable` (full coverage) → `undetermined` (bare member claims); live testsets: 11 unknown-EP members, 35 drifting ⇒ `cascading` (unchanged) |
| `ep_base_severity` | DISPOSITIVE | (0a.2, landed) → `undetermined` on non-numeric EP | Already done in 0a.2 |
| signature `purity_test_*` | DISPOSITIVE | (producer commits) → `unknown(...)` verdicts; `structural_purity` abstains `inconclusive(no_data)` | Done in C-LATENT |

Witness status: JSON additive delta + stability two-sided + live-corpus value: PASTED this
session. The three `.md` report lines (gc / maxent_report / grothendieck): syntax-loaded and
code-reviewed only — **OPEN, graduation = Phase-2 full `run_pipeline`** (those stages don't run
under `classify_corpus`); Phase-2 asserts the lines appear in the regenerated reports.
