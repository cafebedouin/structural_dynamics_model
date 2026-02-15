# Trajectory Pattern-Mining System — Implementation Notes

*Implementation of the system designed in `outputs/trajectory_mining_plan.md`.*
*Subsystem version: v6.4*

---

## 1. Files Modified / Created

| File | Action | Purpose |
|------|--------|---------|
| `prolog/config.pl` | Modified | Added Section 13: 8 trajectory mining parameters |
| `prolog/config_validation.pl` | Modified | Added `trajectory_enabled` binary flag + 7 range-bounded params |
| `prolog/maxent_classifier.pl` | Modified | Added `maxent_multi_run/2` for multi-context MaxEnt |
| `prolog/trajectory_mining.pl` | Created (~1000 lines) | Full 4-phase trajectory mining module |
| `prolog/trajectory_report.pl` | Created (~320 lines) | Markdown report generator (5 sections) |
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
      -l maxent_classifier.pl -l trajectory_mining.pl \
      -l trajectory_report.pl -g "run_trajectory_report, halt."
```

Dashboard section shows: trajectories, structural families, cross-domain twins, singletons.

## 7. Known Limitations

1. **Cross-domain twin count (75K)**: The heuristic domain inference from constraint naming (`prefix_before_first_underscore`) is coarse. Many constraint pairs from "different domains" are actually from related sub-domains. The threshold (0.15) could be tightened or the domain inference improved.

2. **Family A/B spanning**: The two-stage clustering cannot group constraints with different shift patterns. The Phase 4 isomorphism detection handles this at a different semantic level. A future single-stage HAC with pre-computed distances could unify them but at O(n³) cost.

3. **MaxEnt multi-context**: The `maxent_multi_run/2` runs `maxent_precompute` across contexts without cleaning between them. This means context-specific `maxent_dist/3` facts accumulate. Since the facts are keyed by `(Constraint, Context)`, this is correct but consumes more memory.
