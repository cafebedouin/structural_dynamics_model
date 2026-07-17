# OQ-60 implementation handoff (2026-07-17)

State of the rev-4 plan (`~/.claude/plans/review-and-present-a-virtual-breeze.md`).

## Landed (committed, witnessed)

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

### Commit 0b (additive) — R3/R4
- json: `purity_summary` (`json_report.pl:tally_purity_bands` ~2009) gains `n_scorable`/`n_unknown`
  coverage siblings; R3 clean aggregates in `drl_purity_network`/`network_dynamics`/
  `giant_component_analysis`/`fpn_report` gate at coverage 1.0 → distinct surfaced abstention token.
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
live-firing split-out; census-motivated R3 relaxation). Author Priority/Deps; regenerate
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
