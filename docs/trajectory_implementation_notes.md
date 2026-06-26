# Trajectory Pattern-Mining System — Implementation Notes

*Implementation of the system designed in `outputs/trajectory_mining_plan.md`.*
*Subsystem version: v6.4*

---

## 0. Current state — corpus reset, C-null validation, wiring gotchas (2026-06-25, OQ-182)

**The §4 Validation numbers below are kernel_v1-era and STALE for citation.** The live corpus was
reset 2026-06-05 (de-leak rebuild); §4's "1021 trajectories / 26 families / 75,129 twins / Family
A/B (`decentralized_infrastructure_rope`, …)" were measured on a kernel_v1 ancestor and survive
below only as history. Current **testsets/** leg: 104 `corpus_constraint` → **97 trajectories → 11
families** (sizes `[1,2,3,4,4,8,8,9,13,21,24]`, 1 singleton). Cite the manifest/run, never these
frozen figures.

**C-null PASS — families are validated MEANING-bearing (not merely stable).** OQ-182 added the
scope-setter the original build never ran: a per-component-independent permutation null over the
silhouette statistic. C1 (non-degenerate sizes) + C3 (determinism + reorder-invariance) establish
only *stability* — a fixed-seed clustering of pure noise clears them; **C-null is the gate that
separates "stable function of the data" from "real structure."** Witnessed (testsets/): real
silhouette **0.161** > null **P95 −0.026** over 200 destroying-shuffle draws, **0/200** reach real,
teeth gap **+5.0σ**; reproducible under seed `20260625` (SWI 9.2.9). Harness + protocol + erratum:
`audits/2026-06-25_oq182_trajectory_revive/` (`c_null_harness.pl`, `c_null_results.log`,
`c_null_protocol_FROZEN.md`). Methodology promoted to `docs/technical/build_discipline.md` →
*Shuffle-test / permutation-null discipline*.

**The gate is STILL `trajectory_enabled=0` — the subsystem is validated-but-not-live.** C0
(commentary-only pipeline diff) and C-gen (cross-generation family recovery) and the Step-4 gate
flip remain. Do not describe trajectory as live.

**FAMILIES and TWINS are distinct products; the TWIN product is OPEN.** `cross_domain_twins/3` gates
on `constraint_domain/2`, which is a **name-prefix heuristic** (the constraint id before the first
`_`), NOT the authored `topic_domain` — 86/104 distinct on testsets, so the `D1 \= D2` gate is
near-vacuous and the 448 "twins" are mostly same-family pairs with different first name-tokens. The
twin product's meaning is OPEN (value deferred to the rebuild; ruling in
`audits/2026-06-25_oq182_trajectory_revive/c2_domain_finding.md`). **Never cite a twin count as a
finding.** (This is the §7-#1 limitation, now scoped: not "tunable," but unwitnessable on this
substrate.)

**Wiring gotchas (each a silent-bug source — read before modifying the subsystem):**
- **`group_by_shift/2` recomputes the shift pre-grouping via `logical_fingerprint:fingerprint_shift(C)`
  from the constraint IDENTITY, ignoring `trajectory_cached` entirely.** So you CANNOT override or
  shuffle the shift pre-grouping by building a chimera `trajectory_cached` and re-running
  `run_hierarchical_clustering/1` — the grouping stays pinned to the real shift boundaries. (This
  invalidated the C-null protocol's original "chimera surgery map"; the harness builds the groups
  itself via `make_groups`, keyed on `fingerprint_shift(C[σ(i)])`.)
- **The 4 component distances (`shift/metric/stability/pathology_distance`) each take WHOLE
  trajectories and read only their own fields** (shift=Point types; metric=Point chi/entropy/conf;
  stability=Summary preservation/coupling/purity/boltzmann; pathology=Summary
  signature/drift/voids/severity). So a per-component recombination is just calling each predicate on
  *different source trajectories* — no term surgery needed.
- **`get_pair_dist/3` silently returns `1.0` for a missing pair** — any overlay/recompute of
  `pair_dist` must cover every intra-group pair or it corrupts distances silently.
- **`trajectory_selftest/0` is STALE** — it hard-codes kernel_v1-era family names
  (`decentralized_infrastructure_rope`, `moltbook_agent_theater`, …) and expects `>1000`
  trajectories, so on testsets/ it WARNs/SKIPs rather than gates. Treat it as a smoke test, not a
  validation gate, until the Step-4 extension.

---

## 1. Files Modified / Created

| File | Action | Purpose |
|------|--------|---------|
| `prolog/config.pl` | Modified | Added Section 13: 8 trajectory mining parameters |
| `prolog/config_validation.pl` | Modified | Added `trajectory_enabled` binary flag + 7 range-bounded params |
| `prolog/maxent_classifier.pl` | Modified | Added `maxent_multi_run/2` for multi-context MaxEnt |
| `prolog/context_profile_mining.pl` | Created (~1000 lines) | Full 4-phase trajectory mining module |
| `prolog/context_profile_report.pl` | Created (~320 lines) | Markdown report generator (5 sections) |
| `scripts/run_full_pipeline.sh` | Modified | Added step 8f + dashboard section |

## 2. Architecture Decisions

### 2.1 Two-Stage Clustering

The design document suggested flat HAC over all 1021 constraints, which would produce ~520K pairwise distances and O(n³) merge complexity. Instead, we use a two-stage approach:

1. **Stage 1**: Group by shift pattern (36 groups, free from `fingerprint_shift/2`)
2. **Stage 2**: HAC with average linkage within each shift group

This is semantically sound because constraints with different shift patterns have high shift-distance by definition (weight 0.35), so they would rarely merge before the cut level anyway. The approach reduces the largest single HAC from 1021 to ~315 constraints.

**Trade-off**: Family A members span 2 families because `noethers_theorem_symmetry` and `reciprocity_laws_math` have `shift(scaffold,scaffold,scaffold,scaffold)` while `decentralized_infrastructure_rope` and `fair_use_doctrine` have `shift(rope,rope,rope,rope)`. These are genuinely different shift patterns despite sharing the preserved-orbit rope/scaffold ambiguity pattern. The cross-domain isomorphism detection (Phase 4) handles this by detecting structural similarities across families.

### 2.2 Cut Level

The configured `trajectory_family_cut_level = 0.30` produces 26 structural families from 24 orbit families. 8 orbit families are split (33%), adding resolution where continuous metrics differentiate constraints with the same orbit but different coupling/drift/purity profiles.

### 2.3 MaxEnt Integration

The trajectory computation supports three modes:
1. **Full multi-context MaxEnt** — runs `maxent_multi_run/2` across 4 standard contexts
2. **Single-context fallback** — uses base context MaxEnt if multi-context fails
3. **Degraded mode** — returns `unavailable` atoms when MaxEnt is entirely unavailable

This ensures trajectory mining works even when MaxEnt is disabled (`maxent_enabled=0`).

### 2.4 Diagnostic-Only Guarantee

The trajectory mining module:
- Does NOT modify any classification, purity score, or existing output
- Uses only its own dynamic facts (`trajectory_cached/3`, `pair_dist/3`, `cluster_member/2`, `family_assignment/2`)
- Is gated behind `trajectory_enabled` config flag (default: 0 = disabled)
- Runs as a separate pipeline step (8f) after abductive analysis

## 3. Distance Metric Weights

| Component | Weight | Design Doc | Notes |
|-----------|--------|------------|-------|
| Shift distance | 0.35 | 0.30 | Increased to strengthen type-ordering signal |
| Metric distance | 0.25 | 0.25 | As designed |
| Stability distance | 0.25 | 0.25 | As designed |
| Pathology distance | 0.15 | 0.20 | Reduced — drift count noise in larger corpus |

Shift weight was increased from 0.30 to 0.35 (and pathology reduced from 0.20 to 0.15) because the two-stage clustering already pre-groups by shift, so within-group shift distances are 0. The extra shift weight ensures that the `type_distance_lookup` sub-distances within a shift group still properly discriminate constraints where individual context types differ slightly.

## 4. Validation Results

### 4.1 Corpus Statistics

| Metric | Value |
|--------|-------|
| Total trajectories | 1021 |
| Structural families | 26 |
| Orbit families split | 8 / 24 (33%) |
| Cross-domain twins | 75,129 (at threshold 0.15) |
| Singletons | 4 |
| Largest family | 315 (shift: snare,snare,rope,snare) |
| Smallest family | 1 (4 singletons) |

### 4.2 Phase A Validation (6 Genuine Findings)

**Family A** (preserved-orbit rope/scaffold ambiguity):
- `decentralized_infrastructure_rope` → Family 330067 (rope orbit)
- `fair_use_doctrine` → Family 330067 (rope orbit)
- `noethers_theorem_symmetry` → Family 130027 (scaffold orbit)
- `reciprocity_laws_math` → Family 130027 (scaffold orbit)
- Intra-pair distances: 0.025–0.185 (mean 0.124)
- Spans 2 families due to different shift patterns (rope vs scaffold)

**Family B** (violated-orbit liminal cases):
- `moltbook_agent_theater` → Family 170035
- `ulysses_calypso_1904` → Family 1300261
- Intra-pair distance: 0.091
- Spans 2 families due to different 4th-context types

**Cross-family separation**: d(Family A, Family B) = 0.409 (> 0.30 threshold)

### 4.3 Deviations from Design Targets

| Target | Design | Actual | Status |
|--------|--------|--------|--------|
| Family A intra-distance | < 0.15 | max 0.185 | Close — rope/scaffold type distance contributes |
| Family B intra-distance | < 0.15 | 0.091 | PASS |
| Cross-family distance | > 0.40 | 0.409 | PASS |
| Total families | 8–15 | 26 | Higher — shift pre-grouping creates more families |
| Family A same cluster | Yes | Spans 2 | Expected with shift-based pre-grouping |
| Family B same cluster | Yes | Spans 2 | Expected with shift-based pre-grouping |

The family count exceeds the design estimate because the two-stage approach creates a minimum of one family per shift group (36 groups). With the 0.30 cut level, some groups are further split while others remain as single families. The isomorphism detection (Phase 4) compensates by identifying structural twins across families.

## 5. Test Suite Impact

- **1025/1025 existing tests pass** — no regressions
- Trajectory mining adds no new test cases to the validation suite (diagnostic-only)
- Self-test available via `trajectory_selftest/0`

## 6. Pipeline Integration

Step 8f runs after abductive analysis (8e), gated on `trajectory_enabled=1`:
```
swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
      -l maxent_classifier.pl -l context_profile_mining.pl \
      -l context_profile_report.pl -g "run_trajectory_report, halt."
```

Dashboard section shows: trajectories, structural families, cross-domain twins, singletons.

## 7. Known Limitations

1. **Cross-domain twin count (75K)**: The heuristic domain inference from constraint naming (`prefix_before_first_underscore`) is coarse. Many constraint pairs from "different domains" are actually from related sub-domains. The threshold (0.15) could be tightened or the domain inference improved.

2. **Family A/B spanning**: The two-stage clustering cannot group constraints with different shift patterns. The Phase 4 isomorphism detection handles this at a different semantic level. A future single-stage HAC with pre-computed distances could unify them but at O(n³) cost.

3. **MaxEnt multi-context**: The `maxent_multi_run/2` runs `maxent_precompute` across contexts without cleaning between them. This means context-specific `maxent_dist/3` facts accumulate. Since the facts are keyed by `(Constraint, Context)`, this is correct but consumes more memory.
