# OQ-60 implementation handoff (2026-07-17; updated 2026-07-23)

State of the rev-4 plan (`~/.claude/plans/review-and-present-a-virtual-breeze.md`), executed
2026-07-23 under `~/.claude/plans/can-you-review-oq-60-reflective-clarke.md` (operator
plan-review corrections folded in).

## Landed 2026-07-23 (read this first)

- **`bc9bffde` — Preflight.** (a) testsets DRIFTED 145→189 since pin → census probe RE-RUN
  (`census_testsets_2026-07-23.tsv`): **m1–m4 gate-pass victims = 0 re-established; m5
  gate-pass victims 9→11** (new: deflationary_reading 0.988, sufficiency_reading 0.972).
  Expected C-FLOOR flips now **11/2/80/2**; testsets join uses the 2026-07-23 TSV, other legs
  the pinned 2026-07-17 TSVs. (b) tokens pinned both producers (census `-1.0` literal /
  emitter `unknown`→`null`). (c) 0a/0a.2 retro-witnessed via injected unknown end-to-end
  (`PREFLIGHT_2026-07-23.md`; durable: `prolog/tests/test_purity_absence.pl`).
- **`3f0f96c2` — item-0 ordering audit.** No live path lets `unknown` reach a sort; fpn/gc
  ingest collapse unknown→-1.0 ahead of `>= 0.0` filters (`ORDERING_AUDIT_2026-07-23.md`;
  +2 ingest tests).
- **`0bc180c0` — C-LATENT (m1–m4, ONE commit).** 7 tests RED→GREEN; per_constraint
  BYTE-IDENTICAL testsets+flash (stash-baseline method, planted-difference differ control);
  sweeps pasted+controlled; synthetic all-bare + mixed controls green
  (`WITNESS_CLATENT_2026-07-23.md`). New tokens: `scope_invariance_test` → `no_data`;
  `cross_index_coupling`/`detect_nonsensical_coupling` FAIL on no-grid; subscores → `unknown`;
  `structural_purity` → `inconclusive(no_data)` via `aggregate_purity_tests/2`. Declared
  residue: `excess_extraction_factor` 0.5 neutral; `boltzmann_compliance:589` T3.
  **haiku/kernel_v1 legs OPEN — discharge at C-FLOOR.**
- **`0be41518` — C-FLOOR (m5, THE live commit).** `boltzmann_floor_for/2` default clause
  removed; 4-leg census join CLEAN (flips exactly 11/2/80/2, screens within 1e-5, money pair
  0.972→null / 0.948→null); C-LATENT OPEN legs (haiku/kernel_v1) discharged by the join.
  Declared ensemble ripple (attributed): maxent/wasserstein/arakelov/FPN corpus-relative
  layers move; `classifications` changed on ZERO rows; 9 near-boundary rows verdict_join
  red→yellow via the maxent-divergence alert; 1 gate-fail raw-m5 flash row loses its
  fabricated FCR failure. `WITNESS_CFLOOR_2026-07-23.md`; post-C-FLOOR dumps
  `oq60_cfloor_*.json.gz` = the Phase-2 baseline. Remaining: 0b (additive R3/R4), Python
  commit, Phase-2 consolidation, Phase-3 close.
- **Census probe v2 (`census_oq60_v2.pl`):** REQUIRED for any census re-run on the
  post-C-LATENT engine — v1's m1 tag reads the retired `variant([])` token (vacuous zero).
- **C-FLOOR join notes:** (1) corpus is LIVE (operator topic runs land mid-day; testsets was
  189→199 during execution) — re-census testsets with v2 immediately before the C-FLOOR
  baseline, same session, and md5-fingerprint the corpus around every witness pair;
  (2) JSON serializes purity at 6 decimals (`0.354167`) vs census TSV full precision — join
  numerically at JSON precision, never string-equal; (3) pre-C-FLOOR baseline dumps =
  `oq60_clatent_edit_{testsets,flash}.json.gz` (this commit's edited-engine runs).

## Landed 2026-07-17 (committed, witnessed)

- **`62493223` — Phase-0 census.** All four legs instrumented (0 cross-check mismatches;
  positive control fires all 5 branches per-process). Artifacts: `CENSUS.md`,
  `consumer_polarity_0c.md`, `census_*.tsv`, `pin.txt`.
  **Headline: every live (gate-pass) victim on every leg is mechanism 5 (fabricated
  `boltzmann_floor_default`). Mechanisms 1–4 have zero gate-pass victims anywhere** →
  C-SI/C-COUPLING/C-CC/C-EX are latent (byte-identical expected); only C-FLOOR moves
  constraints (scored→unknown: testsets 9, haiku 2, flash 80, kernel_v1 2).
- **`8ada1ff2` — Commit 0a (inert `unknown` plumbing / throw-guards).** ~30 consumer sites
  made `number(P)`-safe against the atom `unknown`; `purity_score/2` short-circuits a subscore
  `unknown` to `unknown`; `purity_zone/2` total on non-numbers; json `unknown → null`.
  **Witnessed byte-identical** on testsets/ (baseline-vs-edited same-session pipeline diff,
  `per_constraint` IDENTICAL, exit 0, mtime advanced, no new top-level JSON keys). Inert by
  construction on the other 3 legs.

## Split deviation (flagged)

The plan's "Commit 0" is split into **0a** (throw-guards, byte-identical — DONE) and **0b**
(additive coverage siblings + R4 report coverage line + R3 abstention token — NOT DONE),
mirroring the plan's own three-part identity claim (byte-identical `per_constraint` / additive
JSON delta / new report line). 0b is additive polish and is NOT a prerequisite for the producer
commits — it can land before or after them.

## Remaining work (ordered)

### Producer commits (the actual fix) — partitioned by terminus, EX before FLOOR
Each = producer edit + its own subscore line + its own fail-open callers (0c section D). All but
C-FLOOR are latent (expect byte-identical). **Per edited .pl file, run `swipl -g halt -l <file>`
before the pipeline** — moves a syntax-break catch from the slowest gate to seconds (the swallowed
comma in 0a survived to the pipeline stage error; five producer commits of Prolog editing remain).
**EX-before-FLOOR ordering demoted from insurance to confirmation:** with C-EX at zero victims, all
floor-movers are already known to carry extraction data, so the confound the ordering guarded
against does not exist — ship it anyway (costs nothing, argument stays clean). **Witness method: reuse the Phase-0 census TSVs as the
clean-HEAD baseline** — run `classify_corpus(Leg, out, model)` on the edited engine per leg and
assert per-constraint `purity_score == (census P if number & ≠ -1.0 else null)`; for C-FLOOR the
census `disposition==unknown` rows (m5) must become JSON `null` and NOT throw.

1. **C-EX (mech 4):** `excess_extraction_subscore` (`purity_scoring.pl:85-89`) no-data → `unknown`
   (currently `EX = 1.0`). Latent (m4 gate-pass victims = 0). Fail-open callers of a FAILING
   `excess_extraction` to handle (0c-D): `signature_detection.pl:1291` (`purity_test_excess` →
   `pass(no_extraction_data)` must become an `unknown`/no-data verdict, not a clean pass),
   `drl_boltzmann_analysis.pl:148` (`excess_extraction_factor` fallback). The
   `metric_drift_events` drift-event sites (312/332/353/362/422) are **existential** (R3: a
   positive finding legitimately doesn't fire on absent data — leave as-is, they do not read
   "clean," they read "no positive finding").
   **Money witness — paste per-constraint in the C-FLOOR commit body** (pre-values from census,
   clean HEAD; post = `null`/`unknown`): `conceptual_framework_reading` 0.972 → unknown,
   `vocabulary_collision_reading` 0.948 → unknown (both testsets, near-pristine off a fabricated
   floor — OQ-60's thesis on real data). Full flip set = the `disposition==unknown` rows of each
   `census_*.tsv`.
   **Prediction to record in the C-FLOOR body (whole-story check, operator):** flash's
   *scorable-mean* purity moves **DOWN** post-fix (fabricated near-pristine scores leave the
   numerator). **If it moves UP, the 80 skewed low and the mechanism-5 account is wrong** — cheap
   falsifier of the entire account; run it.

2. **C-FLOOR (mech 5, THE live commit):** `boltzmann_floor_for/2` clause 3
   (`boltzmann_compliance.pl:528-529`) absent `coordination_type` → fail/sentinel per spec
   (`logic_extensions.md:746`), NOT `boltzmann_floor_default`. This makes `excess_extraction` fail
   for the m5 victims → EX no-data → `unknown` (needs C-EX already landed). **EX-before-FLOOR is
   load-bearing**: C-EX wires the EX subscore terminus; C-FLOOR then routes m5 victims through it.
   Sweep every `boltzmann_floor_for` caller. Live-firing → witness the 9/80/2/2 flips alone with
   justified diff; the census `census_*.tsv` disposition==unknown rows are the exact expected set.
3. **C-COUPLING (mech 2):** `compute_cross_index_coupling/2` `GridSize < 2` → **fail** (no caching
   of failure — do NOT `assertz(cached_coupling)` on the failure path);
   `factorization_subscore` (`purity_scoring.pl:55-59`, the currently-DEAD `F = 0.5` branch) →
   `unknown`. Latent. Fail-open callers of failing `cross_index_coupling` (0c-D): the many bare/
   `->0.0`/`->0.5` sites (signature_detection:1113 FNL, drl_boltzmann_analysis:218,
   context_profile_mining:186, abductive_triggers:422/640, metric_drift_events coupling-drift
   sites, logical_fingerprint:424/428) — most are existential or already explicit-`unknown`; the
   ones that read a failure as `0.0`/clean (drl_boltzmann_analysis:218, context_profile_mining:186)
   need a no-data branch.
4. **C-CC (mech 3):** `detect_nonsensical_coupling` empty-grid no-data → `unknown` at
   `coupling_cleanliness_subscore` (`purity_scoring.pl:76-81`). Latent. `signature_detection:1283`
   (`purity_test_coupling` → `pass`) is the fail-open to fix.
5. **C-SI (mech 1):** `scope_invariance_test/2` empty type list (`boltzmann_compliance.pl:610-624`)
   → distinct `no_data` (not `variant([])`); `scope_invariance_subscore` (`purity_scoring.pl:63-72`)
   no-data → `unknown`. **No clamp change** (N=0 overshoot unconstructible post-fix; 0b census
   confirmed no SI>1 at N≥1). Callers `signature_detection.pl:1187,1276,1396,1548` swept for
   match-completeness.

### Tests — `prolog/tests/test_purity_absence.pl` (net-new)
(a) bare constraint → `purity_score = unknown` never 1.0; (b) two-sided golden vs
`alignment_constraint_narrowing`=`0.3541666666666667` (census, clean HEAD) byte-identical pre/post;
(c) one per mechanism; (d) **precedence** gate-fail→−1.0 xor gate-pass+subscore-unknown→unknown,
never both; (e) R3 polarity (existential fires through unknown members; clean aggregate over a set
with one unknown → distinct abstention token); (f) `purity_zone`/json handle `unknown` without
throwing. Positive-control template: `oq60_control_bare` in `census_oq60.pl`.

### Commit 0b (additive) — R3 (revised ruling) / R4
**R3 axis = claim-type, NOT a coverage threshold (operator, 2026-07-17; CENSUS 0d(ruling)):**
- **Descriptive** (mean/distribution/counts — all ~8 shape-aggregates, confirmed terminal by the
  consumption trace): compute over scorable, **label carries `n_scored/n_total`, NO gate**. Restate
  the referent. Sites: `tally_purity_bands`/`purity_summary`, `giant_component:893/894` distributions,
  `maxent_report:375`, `grothendieck:791`, `maxent_diagnostic:606/616/625`,
  `drl_purity_network` weighted-purity means.
- **Dispositive** (band/pristine/clean/severity verdicts): gate at coverage 1.0 → distinct
  abstention token. Sites: `ep_base_severity` (DONE in 0a.2 → `undetermined`),
  `network_stability_assessment=stable` (negative existential — must abstain if any member severity
  is `undetermined`), `purity_zone` verdicts, signature `purity_test_*` (→ producer commits).
- **Before encoding, re-confirm the classification by CONSUMPTION** for any new aggregate: a
  descriptive stat that feeds a threshold is dispositive-by-consumption.
- json: `purity_summary` gains `n_scored`/`n_total` coverage siblings (descriptive label, not a gate).
- enhanced_report.py: **unconditional** per-section coverage line (`N/N scorable, M unknown`,
  including `40/40` on a fully-covered corpus — R4); per-constraint unknown rows already render
  `N/A` (line 891-894, safe). Python null-safety: grep `purity_score`/`purity_band` for truthiness
  (`if purity_score:` conflates None/0.0) and pandas `.mean()/.sum()` NaN-skip.
- 0g confirmed **no strict report scraper** and schemas.py is permissive → additive is safe.

### Phase 2 witness
Per producer commit: `run_pipeline.py` (testsets) + `classify_corpus(...)` for haiku/flash/
kernel_v1 (serialized). Mechanical diff↔census join against `census_*.tsv` (snapshot equality from
`pin.txt`); deliverable = unjoined residue (0e granularity residue = none, caches per-constraint).
All-bare synthetic corpus control: JSON carries `n_unknown` + abstention token; report contains
`0/N scorable`, N>0, zero purity value rows (empty report must NOT pass). Gates: OQ-137 totality,
`validation_suite` `run_dynamic_suite`, new plunit, FPN canaries.

### Phase 3 close
Audit writeup; ISSUES.md OQ-60 → resolved (R1/R2/R3/R4); file the −1.0-in-aggregate finding to
OQ-62 (propagation via `effective_purity`/`fpn_intrinsic`/`gc_node_purity` — see CENSUS 0c/0d);
mint follow-ups (SI N≥5 undershoot; "every filtering section prints what it dropped"; any
live-firing split-out; census-motivated R3 relaxation; **cross-leg comparability** — exclusion is
non-random w.r.t. provenance (10.7% flash vs 0.4% haiku), scorable-means not comparable across
legs, biased before the fix and differently after; tripwire "scorable-means are not comparable
across legs"; **build_discipline lesson** — don't write falsifiers over counts of heterogeneous
populations, write over the load-bearing member). Author Priority/Deps; regenerate
`omega_resolver.py index`. KNOWN_STATE dated entry + the two Architecture-Invariants tripwires
(reading rule: two absence tokens null/unknown vs −1.0, never coerce/average; writing rule:
clean-aggregate coverage-1.0 gate, existential fires >0 w/ coverage, abstain≠pass, unconditional
coverage line). Auto-memory: shared-terminus→inert-commit-0; partition-by-terminus;
instrument-don't-reconstruct; assert-the-positive; direction-of-concealment decides scope;
coverage-denominator-unconditional; census-the-opposite-sign-token before promoting.

## Environment note
`.git/sequencer/` is a **stale orphaned cherry-pick from Jul 11** (no CHERRY_PICK_HEAD, no active
op) — `git status` prints "cherry-pick in progress" but commits work fine. Left untouched (not
mine). `git cherry-pick --quit` would clear it if the operator confirms.
