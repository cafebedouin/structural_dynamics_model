# Trajectory Pattern-Mining System — Design Document

*Design only. No implementation in this document. Grounded in actual corpus data (1,023-1,024 constraints) and the 6 genuine abductive findings from Phase A.*

---

## 1. Existing Infrastructure Inventory

### 1.1 Gauge Orbit System (`dirac_classification.pl`)

**API surface:**
- `gauge_orbit(C, OrbitPoints)` — collects `[orbit_point(Type, Context), ...]` across 4 standard contexts
- `preserved_under_context_shift(C, Result)` — returns `preserved(Type)` or `violated(Transitions)`

**Corpus statistics:**
- 1,023 constraints analyzed
- 24 distinct orbit families (sets of unique types observed across 4 contexts)
- 4 single-type (gauge-invariant): `[rope]` (34), `[tangled_rope]` (143), `[mountain]` (21), `[scaffold]` (14)
- 20 multi-type (gauge-variant): 811 constraints (79.3%)
- Largest family: `[rope,snare]` with 315 constraints

**What orbits capture:** The *set* of types a constraint manifests across the 4 standard contexts. Orbits lose context ordering — `[rope,snare]` tells you both types appear but not *which context* produces which type.

**What orbits miss:** Context ordering (the *shift* captures this), metric magnitudes, temporal drift, and coupling structure. The 34-member `[rope]` family includes all 4 Family A findings from Phase A, but cannot distinguish them from the other 30 members.

### 1.2 Logical Fingerprint System (`logical_fingerprint.pl`)

**API surface:**
- `logical_fingerprint(C, Fingerprint)` — 7-dimensional structured term: `fingerprint(Shift, Properties, Voids, Actors, Drift, Zone, Coupling)`
- `fingerprint_shift(C, Shift)` — `shift(T_powerless, T_moderate, T_institutional, T_analytical)` — ordered type at each context
- `fingerprint_voids(C, Voids)` — diagnostic absences (8 void types)
- `fingerprint_zone(C, Zone)` — `zone(ExtractionZone, SuppressionZone)` — categorical metric regime
- `fingerprint_coupling(C, Coupling)` — coupling topology from Boltzmann compliance
- `shift_family(Pattern, Constraints)` — group by shift pattern
- `fingerprint_match(C1, C2, Dimensions, Result)` — match on specific dimensions

**Corpus statistics:**
- 1,024 constraints analyzed
- 36 distinct shift patterns
- Largest: `shift(snare, snare, rope, snare)` with 315 constraints

**Orbit-fingerprint cross-reference:** Every shift pattern maps to exactly one orbit family (the sorted set of unique types in the shift). Multiple shift patterns can map to the same orbit family. Example: `shift(tangled_rope, tangled_rope, rope, snare)` and `shift(tangled_rope, snare, rope, snare)` both map to orbit `[rope,snare,tangled_rope]` (138 + 26 = 164 constraints). The shift preserves context ordering; the orbit discards it.

### 1.3 What Exists vs. What's Missing

| Dimension | Existing source | Captured? |
|-----------|----------------|-----------|
| Type at each context | `fingerprint_shift/2` | Yes |
| Orbit family (unordered types) | `gauge_orbit/2` | Yes |
| Metric zone (categorical) | `fingerprint_zone/2` | Yes (categorical only) |
| Coupling score | `cross_index_coupling/2` | Yes (single number) |
| Purity score | `purity_score/2` | Yes (single number) |
| MaxEnt entropy | `maxent_entropy/3` | Context-specific only |
| MaxEnt distribution | `maxent_distribution/3` | Context-specific only |
| Drift event count/severity | `scan_constraint_drift/2` | Yes (list of events) |
| Fingerprint voids | `fingerprint_voids/2` | Yes (list of void atoms) |
| **Continuous metric values per context** | **Not assembled** | **No** |
| **MaxEnt distribution per context** | **Not computed** | **No (single context)** |
| **Integrated trajectory object** | **Does not exist** | **No** |
| **Distance metric between trajectories** | **Does not exist** | **No** |
| **Clustering of trajectories** | **Does not exist** | **No** |

---

## 2. Trajectory Representation

### 2.1 Design Rationale

A *trajectory* is the full characterization of how a constraint behaves as the observer moves through the 4 standard contexts. It extends the shift (type sequence) with continuous metric values, confidence scores, and temporal signals.

The 6 genuine findings from Phase A demonstrate why a type-only trajectory (the existing shift pattern) is insufficient:
- **Family A** (decentralized_infrastructure_rope, fair_use_doctrine, noethers_theorem_symmetry, reciprocity_laws_math) all share `shift(rope,rope,rope,rope)` or `shift(scaffold,scaffold,scaffold,scaffold)` — the shift alone cannot separate them from the other 28-48 members of their shift families. Their distinguishing feature is **MaxEnt entropy** (0.40-0.50, above the uncertainty threshold) combined with **preserved orbits** — a combination the shift doesn't capture.
- **Family B** (moltbook_agent_theater, ulysses_calypso_1904) share different shift patterns but converge on **maximal coupling (1.0) + 4-5 active drift events + 3 extractive voids**. These signals exist in individual subsystems but are never assembled into a unified per-constraint object.

### 2.2 Trajectory Data Structure

```prolog
trajectory(Constraint, Points, Summary)
```

where `Points` is a list of 4 trajectory_point terms, one per standard context:

```prolog
trajectory_point(
    context(Context),          % The standard context (powerless/moderate/institutional/analytical)
    type(DRType),              % dr_type/3 result for this context
    chi(ChiValue),             % extractiveness_for_agent/3 (continuous, 0.0-1.0+)
    maxent(Distribution),      % maxent_distribution/3 for THIS context (or unavailable)
    entropy(H_norm),           % maxent_entropy/3 for THIS context (or unavailable)
    confidence(MaxentConf)     % 1 - H_norm (or unavailable)
)
```

and `Summary` aggregates context-invariant signals:

```prolog
trajectory_summary(
    orbit_family(OrbitFamily),          % sorted list of unique types
    shift(ShiftPattern),                % shift(T1,T2,T3,T4)
    preservation(PreservedOrViolated),  % preserved(Type) | violated(Transitions)
    coupling(CouplingScore),            % cross_index_coupling/2 (0.0-1.0)
    purity(PurityScore),               % purity_score/2 (-1.0 to 1.0)
    signature(Sig),                     % constraint_signature/2
    drift(DriftEvents),                 % scan_constraint_drift/2 list
    drift_count(N),                     % length of drift events
    drift_max_severity(Severity),       % max severity among events
    voids(VoidList),                    % fingerprint_voids/2
    zone(Zone),                         % fingerprint_zone/2
    boltzmann(BoltzResult)              % boltzmann_compliant/2
)
```

### 2.3 Why This Shape

**4 trajectory points (not 1):** The core insight from the Dirac classification layer is that a constraint's identity is its *orbit*, not any single observation. A trajectory must capture the continuous metric profile at each observation point, not just the discrete type.

**MaxEnt per context:** Currently `maxent_run/2` computes distributions at a single context. Multi-context MaxEnt (Section 8) would produce distributions at each of the 4 standard contexts, enabling entropy trajectories — how uncertainty changes as the observer gains power. This is the most expensive new computation but the highest-value for trajectory mining.

**Summary as aggregation, not replacement:** The summary provides pre-computed aggregates (coupling, purity, drift) that don't vary per context. These are computed once and shared across all trajectory points. This avoids redundant storage while keeping the trajectory self-contained.

### 2.4 Trajectory for the 6 Genuine Findings (Concrete Examples)

**decentralized_infrastructure_rope** trajectory:
```
Points: [
  point(powerless, rope, chi=0.0101, entropy=?, conf=?),
  point(moderate,  rope, chi=0.1096, entropy=0.44, conf=0.56),  % default context
  point(institutional, rope, chi=?, entropy=?, conf=?),
  point(analytical, rope, chi=?, entropy=?, conf=?)
]
Summary: orbit=[rope], shift=shift(rope,rope,rope,rope),
         preserved=preserved(rope), coupling=0.375, purity=0.8755,
         sig=constructed_low_extraction, drift=[purity_drift(warning)],
         drift_count=1, drift_max=warning, voids=[], zone=zone(negligible,low)
```

**ulysses_calypso_1904** trajectory:
```
Points: [
  point(powerless, tangled_rope, chi=high, entropy=?, conf=?),
  point(moderate,  tangled_rope, chi=0.6439, entropy=0.41, conf=0.59),
  point(institutional, rope, chi=low, entropy=?, conf=?),
  point(analytical, tangled_rope, chi=?, entropy=?, conf=?)
]
Summary: orbit=[rope,tangled_rope], shift=shift(tangled_rope,tangled_rope,rope,tangled_rope),
         preserved=violated(...), coupling=1.0, purity=0.4903,
         sig=false_ci_rope, drift=[metric_sub(critical), extract_accum(critical),
         coupling_drift(critical), purity_drift(warning), network_drift(critical)],
         drift_count=5, drift_max=critical, voids=[drifting_without_limit,
         no_exit_for_victims, unaccountable_extraction],
         zone=zone(extreme,extreme)
```

These two trajectories are maximally distant — every dimension diverges. A well-designed distance metric would confirm this while also detecting that the 4 Family A members form a tight cluster.

---

## 3. Structural Distance Metric

### 3.1 Design Requirements

The distance metric must satisfy:
1. **Type-aware:** Two trajectories with identical type sequences but different entropies are closer than two with different type sequences.
2. **Multi-scale:** Capture both context-specific differences (per-point) and aggregate differences (summary-level).
3. **Asymmetry-tolerant:** Some dimensions are categorical (type, signature, voids), others continuous (chi, entropy, coupling). The metric must handle mixed types.
4. **Phase A-validated:** Family A's 4 members must cluster tightly; Family B's 2 must cluster tightly; the two families must be distant.

### 3.2 Distance Decomposition

The total distance decomposes into 4 weighted components:

```
d(T1, T2) = w_shift * d_shift(T1, T2)
           + w_metric * d_metric(T1, T2)
           + w_stability * d_stability(T1, T2)
           + w_pathology * d_pathology(T1, T2)
```

**Component 1: Shift Distance (`d_shift`)**

Compares the ordered type sequences at each context. Uses a type distance matrix rather than simple equality:

| | mountain | rope | scaffold | tangled_rope | snare | piton | unknown | io |
|--|--|--|--|--|--|--|--|--|
| **mountain** | 0.0 | 0.8 | 0.7 | 0.9 | 1.0 | 0.7 | 0.5 | 0.5 |
| **rope** | 0.8 | 0.0 | 0.2 | 0.3 | 0.6 | 0.4 | 0.4 | 0.5 |
| **scaffold** | 0.7 | 0.2 | 0.0 | 0.4 | 0.5 | 0.3 | 0.4 | 0.5 |
| **tangled_rope** | 0.9 | 0.3 | 0.4 | 0.0 | 0.3 | 0.5 | 0.4 | 0.5 |
| **snare** | 1.0 | 0.6 | 0.5 | 0.3 | 0.0 | 0.6 | 0.4 | 0.5 |
| **piton** | 0.7 | 0.4 | 0.3 | 0.5 | 0.6 | 0.0 | 0.4 | 0.5 |
| **unknown** | 0.5 | 0.4 | 0.4 | 0.4 | 0.4 | 0.4 | 0.0 | 0.3 |

Design rationale: Distances reflect the Dirac first-class/second-class decomposition. rope-scaffold = 0.2 (both first-class coordination). rope-tangled_rope = 0.3 (first-class vs mixed). tangled_rope-snare = 0.3 (mixed vs second-class extraction — the liminal boundary from Phase A). mountain-snare = 1.0 (maximum — immutable natural law vs pure extraction). `unknown` and `indexically_opaque` are 0.3-0.5 from everything (moderate uncertainty penalty).

```
d_shift(T1, T2) = (1/4) * sum_over_contexts(type_distance(T1_ctx, T2_ctx))
```

Normalized to [0, 1].

**Component 2: Metric Distance (`d_metric`)**

Compares continuous metric values across contexts.

```
d_metric(T1, T2) = (1/4) * sum_over_contexts(
    0.5 * |chi1_ctx - chi2_ctx|
  + 0.3 * |entropy1_ctx - entropy2_ctx|
  + 0.2 * |conf1_ctx - conf2_ctx|
)
```

When per-context MaxEnt is unavailable (single-context run), use the default-context entropy for all 4 contexts (degraded mode). Normalized to [0, 1] since chi is bounded [0, ~1.5], entropy [0, 1], confidence [0, 1].

**Component 3: Stability Distance (`d_stability`)**

Compares structural stability signals.

```
d_stability(T1, T2) = 0.30 * |coupling1 - coupling2|
                     + 0.25 * |purity1 - purity2|  (handle -1.0 inconclusive as 0.5)
                     + 0.25 * preservation_distance(T1, T2)
                     + 0.20 * boltzmann_distance(T1, T2)
```

where:
- `preservation_distance` = 0.0 if both preserved or both violated; 1.0 if one preserved and one violated
- `boltzmann_distance` = 0.0 if same compliance status; 0.5 if one inconclusive; 1.0 if compliant vs non-compliant

**Component 4: Pathology Distance (`d_pathology`)**

Compares drift and void profiles — the signals that distinguish Family B from everything else.

```
d_pathology(T1, T2) = 0.35 * drift_distance(T1, T2)
                     + 0.30 * void_distance(T1, T2)
                     + 0.20 * severity_distance(T1, T2)
                     + 0.15 * signature_distance(T1, T2)
```

where:
- `drift_distance` = |drift_count1 - drift_count2| / max_drift (capped at 1.0)
- `void_distance` = 1 - Jaccard(voids1, voids2) (set similarity)
- `severity_distance` = 0 if same max severity; 0.5 if one step apart; 1.0 if two+ steps
- `signature_distance` = 0 if same signature; appropriate distances between signature types (NL-FNL = 0.8, FCR-CI_Rope = 0.6, constructed-NL = 0.5, etc.)

### 3.3 Default Weights

```prolog
trajectory_weight(shift, 0.30).
trajectory_weight(metric, 0.25).
trajectory_weight(stability, 0.25).
trajectory_weight(pathology, 0.20).
```

These weights are config-driven and tunable. The shift component is weighted highest because it's the most structurally informative dimension (it directly captures gauge structure). Pathology is weighted lowest because most constraints have no pathology signals — it's a high-discrimination signal for a small subset.

### 3.4 Validation Against Phase A

With these weights, the expected distances:

- **d(decentralized_infrastructure_rope, fair_use_doctrine)**: ~0.04 (nearly identical — same shift, very similar metrics, both low-pathology)
- **d(noethers_theorem_symmetry, reciprocity_laws_math)**: 0.00 (identical profiles by construction)
- **d(Family A member, Family B member)**: ~0.75+ (different shift patterns, extreme metric divergence, massive pathology gap)
- **d(moltbook_agent_theater, ulysses_calypso_1904)**: ~0.15 (different shifts but convergent coupling, drift, and void profiles)

This correctly reproduces the Phase A finding: two tight clusters with large inter-cluster distance.

---

## 4. Clustering Approach

### 4.1 Corpus Scale

~1,025 constraints. This is small enough for O(n^2) pairwise distance computation (~525,000 pairs). No approximate nearest-neighbor methods needed.

### 4.2 Algorithm Selection

**Primary: Hierarchical agglomerative clustering with average linkage.**

Rationale:
1. Produces a dendrogram — users can cut at any desired granularity
2. No need to pre-specify number of clusters (unlike k-means)
3. Average linkage handles non-spherical clusters well
4. Deterministic (no random initialization)
5. O(n^2 log n) with the trajectory distance already precomputed as a pairwise matrix

**Secondary: Density-based validation via DBSCAN-like detection.**

After hierarchical clustering, identify "core" structural families (dense regions) versus "boundary" constraints (sparse — the liminal constraints from Phase A). This is purely diagnostic, not a replacement for the hierarchical tree.

### 4.3 Prolog-Native Implementation Strategy

Prolog isn't ideal for matrix operations. The implementation strategy:

1. **Compute pairwise distances in Prolog** using `trajectory_distance/4` — Prolog handles the predicate logic cleanly.
2. **Store distances as asserted facts**: `trajectory_pair_dist(C1, C2, Distance)` — ~525K facts, manageable.
3. **Build dendrogram in Prolog** using a merge-based algorithm:
   - Initialize each constraint as a singleton cluster
   - Repeat: find the two closest clusters, merge them, update distances (average linkage = average of all pairwise distances between merged clusters)
   - Store merge history as `cluster_merge(Step, ClusterA, ClusterB, MergeDist)`

This is tractable: 1,024 merge steps, each scanning the remaining cluster pairs. The O(n^2) storage for the distance matrix is the bottleneck (~4MB for 525K float pairs), not the merge computation.

### 4.4 Cluster Naming

Each cluster at the dendrogram cut level is named by its **dominant shift pattern + dominant zone**:

```prolog
cluster_name(ClusterID, Name) :-
    cluster_members(ClusterID, Members),
    findall(Shift, (member(M, Members), fingerprint_shift(M, Shift)), Shifts),
    msort(Shifts, Sorted),
    mode(Sorted, DominantShift),
    ...
```

Example: a cluster containing 300+ constraints might be named `snare_rope_extreme_high` for the `shift(snare,snare,rope,snare)` pattern with `zone(extreme, high)`.

### 4.5 Expected Cluster Structure (Predictions)

Based on existing orbit and fingerprint data:

| Predicted cluster | Size | Defining features |
|-------------------|------|-------------------|
| Rope-snare extractive | ~315 | shift(snare,snare,rope,snare), high extraction, high suppression |
| Tangled-rope coordination | ~143 | Gauge-invariant tangled_rope, medium extraction |
| Mixed tangled-rope-snare | ~138 | shift(tangled_rope,tangled_rope,rope,snare), boundary constraints |
| Pure rope coordination | ~30-34 | Gauge-invariant rope, low extraction, low theater |
| Rope-scaffold ambiguous | ~4-8 | Gauge-invariant rope/scaffold, HIGH MaxEnt entropy (>0.40) — **this is where Family A lives** |
| Mountain invariant | ~21 | Gauge-invariant mountain, zero coupling |
| Mountain-unknown gap | ~50 | shift(mountain,unknown,unknown,mountain), missing intermediate contexts |
| Liminal degrading | ~2-5 | High coupling (>0.8), 3+ drift events, extractive voids — **this is where Family B lives** |
| Scaffold transitional | ~14 | Gauge-invariant scaffold, low extraction |
| Indexically opaque | ~17 | Contains IO type, undetermined Dirac class |

The key prediction: **Family A emerges as a distinct sub-cluster within the pure-rope family**, separated by their elevated MaxEnt entropy. **Family B emerges as an outlier cluster** with extreme pathology scores. This validates Phase A's structural families and demonstrates the trajectory mining system adds analytical value beyond the existing orbit/fingerprint infrastructure.

---

## 5. Isomorphism Detection

### 5.1 Definition of Structural Isomorphism

Two constraints are **structurally isomorphic** if they share:
1. The same shift pattern (type at each context)
2. The same fingerprint zone (categorical metric regime)
3. The same void profile (diagnostic absences)
4. Coupling scores within a tolerance band

They need NOT share:
- Domain content
- Actor topology (beneficiaries/victims may differ)
- Exact metric values (zone equivalence suffices)

This definition extends the existing `fingerprint_match/4` from `logical_fingerprint.pl`, which already supports dimension-selective matching.

### 5.2 Isomorphism Levels

**Strict isomorphism:** All 4 criteria match exactly. This is the existing `logical_fingerprint/2` unification — two constraints with unifiable fingerprints are strictly isomorphic.

**Trajectory isomorphism:** Shift + zone + coupling_band + void_profile match, AND trajectory distance < threshold (default 0.10). This adds the continuous metric layer — two constraints might have identical fingerprints but very different MaxEnt entropy, indicating different structural positions within the same fingerprint family.

**Family isomorphism:** Same orbit family + same coupling_band + same pathology_band. The loosest level — allows different context orderings (different shifts within the same orbit family) but requires the stability and pathology signatures to match.

### 5.3 Cross-Domain Detection

The compelling use case: finding structural twins across unrelated domains.

From Phase A, the 6 genuine findings demonstrate this:
- `noethers_theorem_symmetry` (mathematics) and `reciprocity_laws_math` (mathematics) are strictly isomorphic — trivially, since they're from the same domain with identical metrics
- `moltbook_agent_theater` (AI agent systems) and `ulysses_calypso_1904` (literary analysis) are **trajectory isomorphic** despite having zero domain overlap — same coupling, same voids, same drift severity class, comparable entropy

The cross-domain discovery pipeline:

```
For each pair (C1, C2) where domain(C1) != domain(C2):
  1. Check shift isomorphism (fast — sort & compare atoms)
  2. If shift matches, check zone + void (fast — term comparison)
  3. If those match, compute trajectory distance (expensive — full metric comparison)
  4. If distance < threshold, flag as cross-domain structural twin
```

Step 1 eliminates 95%+ of pairs immediately. The expensive step 3 only runs on the small set that passes the structural gates.

### 5.4 Isomorphism Classes vs. Orbit Families

An isomorphism class is a *refinement* of an orbit family. Every isomorphism class is contained within exactly one orbit family, but an orbit family may contain multiple isomorphism classes.

Example: The `[rope]` orbit family (34 constraints) would split into approximately:
- Isomorphism class A: low entropy (<0.40), zero coupling, no voids — the "clean rope" class (~28 constraints)
- Isomorphism class B: high entropy (>=0.40), low coupling, no voids — the "ambiguous rope" class (~4 constraints, = Family A from Phase A)
- Possibly others based on zone and coupling variation

This refinement is exactly what the existing orbit/fingerprint systems cannot do — they lack the continuous metric and drift dimensions.

---

## 6. Concrete Prolog Predicates

### 6.1 Module Structure: `trajectory_mining.pl`

```prolog
:- module(trajectory_mining, [
    % Core
    trajectory_run/2,              % trajectory_run(Context, Summary)
    constraint_trajectory/3,       % constraint_trajectory(C, Context, Trajectory)

    % Distance
    trajectory_distance/4,         % trajectory_distance(C1, C2, Context, Distance)
    trajectory_pairwise/2,         % trajectory_pairwise(Context, PairList)

    % Clustering
    trajectory_cluster/3,          % trajectory_cluster(Context, CutLevel, Clusters)
    cluster_members/2,             % cluster_members(ClusterID, Members)
    cluster_dendrogram/2,          % cluster_dendrogram(Context, MergeHistory)

    % Isomorphism
    structural_family/2,           % structural_family(FamilyID, Members)
    structural_isomorphism/4,      % structural_isomorphism(C1, C2, Level, Evidence)
    cross_domain_twins/3,          % cross_domain_twins(Context, Threshold, Pairs)

    % Report
    trajectory_report/2            % trajectory_report(Context, ReportPath)
]).
```

### 6.2 Predicate Specifications

#### `trajectory_run/2`

```prolog
%% trajectory_run(+Context, -Summary)
%  Main entry point. Computes trajectories for all loaded constraints,
%  then runs clustering and isomorphism detection.
%
%  Prerequisites: covering_analysis:load_all_testsets must have been called.
%  MaxEnt must have been run (maxent_run/2) for at least the default context.
%  If multi-context MaxEnt is available, uses it; otherwise degrades gracefully.
%
%  Summary = trajectory_summary(
%      total_constraints(N),
%      families(NFamilies),
%      isomorphism_classes(NClasses),
%      cross_domain_twins(NTwins),
%      anomalies(AnomalyList)   % constraints with no family (singletons)
%  )
%
%  Side effects: asserts trajectory_cached/3, pair_dist/3, cluster_member/2
trajectory_run(Context, Summary) :-
    trajectory_cleanup,
    compute_all_trajectories(Context),
    compute_pairwise_distances(Context),
    run_hierarchical_clustering(Context),
    detect_isomorphism_classes(Context),
    find_cross_domain_twins(Context, 0.10),
    assemble_summary(Context, Summary).
```

#### `constraint_trajectory/3`

```prolog
%% constraint_trajectory(+Constraint, +BaseContext, -Trajectory)
%  Builds the full trajectory for a single constraint.
%  BaseContext is used for MaxEnt if multi-context MaxEnt is unavailable.
%
%  Trajectory = trajectory(Constraint, Points, Summary)
%  where Points = [trajectory_point(...), ...] (4 elements, one per standard context)
%  and Summary = trajectory_summary(...) as defined in Section 2.2
constraint_trajectory(C, BaseContext, trajectory(C, Points, Summary)) :-
    findall(Point,
        (dirac_classification:standard_context(Ctx),
         build_trajectory_point(C, Ctx, BaseContext, Point)),
        Points),
    build_trajectory_summary(C, Summary).
```

#### `trajectory_distance/4`

```prolog
%% trajectory_distance(+C1, +C2, +Context, -Distance)
%  Computes the weighted 4-component distance between two constraint trajectories.
%  Returns Distance in [0.0, 1.0].
%
%  If trajectories are not yet cached, computes them on the fly.
%  Context determines which MaxEnt run to use for the metric component.
trajectory_distance(C1, C2, Context, Distance) :-
    ensure_trajectory(C1, Context, T1),
    ensure_trajectory(C2, Context, T2),
    shift_distance(T1, T2, DShift),
    metric_distance(T1, T2, DMetric),
    stability_distance(T1, T2, DStability),
    pathology_distance(T1, T2, DPathology),
    config:param(trajectory_weight_shift, WShift),
    config:param(trajectory_weight_metric, WMetric),
    config:param(trajectory_weight_stability, WStability),
    config:param(trajectory_weight_pathology, WPathology),
    Distance is WShift * DShift + WMetric * DMetric
             + WStability * DStability + WPathology * DPathology.
```

#### `structural_family/2`

```prolog
%% structural_family(+FamilyID, -Members)
%  Returns the members of a structural family. Families are identified
%  after clustering — each cluster at the default cut level becomes a family.
%  FamilyID is an integer (merge step at which the cluster formed).
structural_family(FamilyID, Members) :-
    cluster_members(FamilyID, Members).
```

#### `structural_isomorphism/4`

```prolog
%% structural_isomorphism(+C1, +C2, -Level, -Evidence)
%  Tests whether two constraints are structurally isomorphic and at what level.
%
%  Level ∈ { strict, trajectory, family, none }
%  Evidence = isomorphism_evidence(
%      shift_match(Bool),
%      zone_match(Bool),
%      void_match(Bool),
%      coupling_band_match(Bool),
%      trajectory_distance(D),
%      orbit_family_match(Bool)
%  )
structural_isomorphism(C1, C2, Level, Evidence) :-
    compute_evidence(C1, C2, Evidence),
    classify_level(Evidence, Level).
```

### 6.3 Configuration Parameters (additions to `config.pl`)

```prolog
% Trajectory mining
param(trajectory_weight_shift, 0.30).
param(trajectory_weight_metric, 0.25).
param(trajectory_weight_stability, 0.25).
param(trajectory_weight_pathology, 0.20).
param(trajectory_isomorphism_threshold, 0.10).
param(trajectory_cluster_cut_level, 0.25).     % default dendrogram cut height
param(trajectory_coupling_band_width, 0.15).   % coupling match tolerance
param(trajectory_enabled, 0).                  % disabled by default (like MaxEnt, FPN)
```

### 6.4 Dependencies

```
trajectory_mining.pl
  ├── dirac_classification.pl     (gauge_orbit/2, standard_context/1)
  ├── logical_fingerprint.pl      (fingerprint_shift/2, fingerprint_voids/2, etc.)
  ├── maxent_classifier.pl        (maxent_entropy/3, maxent_distribution/3)
  ├── structural_signatures.pl    (constraint_signature/2, purity_score/2, etc.)
  ├── drl_lifecycle.pl            (scan_constraint_drift/2)
  ├── drl_core.pl                 (dr_type/3, dr_mismatch/4)
  ├── constraint_indexing.pl      (extractiveness_for_agent/3, default_context/1)
  ├── narrative_ontology.pl       (constraint_metric/3 for raw metrics)
  ├── config.pl                   (all threshold and weight parameters)
  └── covering_analysis.pl        (load_all_testsets, constraint discovery)
```

Load order: after all dependency modules, before report generation. Add to `stack.pl` loading sequence.

---

## 7. Phased Implementation Plan

### Phase 1: Trajectory Computation (foundation)

**Goal:** Build and cache trajectories for all constraints.

**Tasks:**
1. Add configuration parameters to `config.pl`
2. Create `trajectory_mining.pl` module skeleton with exports
3. Implement `build_trajectory_point/4` — assembles one point from existing predicates
4. Implement `build_trajectory_summary/2` — aggregates context-invariant signals
5. Implement `constraint_trajectory/3` — assembles full trajectory
6. Implement `compute_all_trajectories/1` — iterates over corpus, asserts `trajectory_cached/3`
7. Implement `trajectory_cleanup/0` — retracts prior run's dynamic facts
8. Add `trajectory_run/2` with only the trajectory computation step (no clustering yet)

**Testing:** Run on full corpus. Verify that all 1,024 constraints produce valid trajectories. Spot-check the 6 genuine findings against Phase A profiles. Verify `trajectory_cached/3` fact count matches corpus size.

**Risk:** Multi-context MaxEnt not yet available. Mitigate: implement degraded mode that uses default-context MaxEnt for all 4 points. Flag degraded trajectories with `maxent_mode(single_context)` in the trajectory term.

**Estimated complexity:** ~200 lines of Prolog. Mostly glue code calling existing predicates.

### Phase 2: Distance Metric + Pairwise Computation

**Goal:** Compute all pairwise distances and store as indexed facts.

**Tasks:**
1. Implement the type distance matrix as `type_pair_distance/3` facts
2. Implement `shift_distance/3`, `metric_distance/3`, `stability_distance/3`, `pathology_distance/3`
3. Implement `trajectory_distance/4` with weight-based combination
4. Implement `compute_pairwise_distances/1` — O(n^2) computation, asserts `pair_dist/3`
5. Add progress reporting (the computation will take minutes on 1,024 constraints)

**Testing:** Verify distance properties: d(x,x) = 0, d(x,y) = d(y,x), d >= 0. Verify the Phase A validation targets: Family A intra-distance < 0.05, Family B intra-distance < 0.20, inter-family distance > 0.60. Run sensitivity analysis on weight parameters.

**Risk:** Performance. 525K distance computations, each involving 4 trajectory point comparisons. If each takes 1ms, that's ~9 minutes. Mitigate: cache trajectory lookups, avoid redundant MaxEnt queries, use first/0 cuts in helper predicates.

**Estimated complexity:** ~300 lines of Prolog. The distance subcomponents are the bulk of the logic.

### Phase 3: Clustering + Family Detection

**Goal:** Build the dendrogram and identify structural families.

**Tasks:**
1. Implement `init_clusters/1` — each constraint starts as a singleton cluster
2. Implement `find_closest_pair/3` — scans cluster pairs for minimum average-linkage distance
3. Implement `merge_clusters/4` — merges two clusters, updates distance matrix
4. Implement `run_hierarchical_clustering/1` — the full merge loop, stores `cluster_merge/4`
5. Implement `trajectory_cluster/3` — cuts dendrogram at specified level
6. Implement `cluster_members/2` — retrieves members of a cluster
7. Implement `cluster_dendrogram/2` — retrieves full merge history

**Testing:** Verify merge history is monotonically non-decreasing in distance. Verify that at cut level 0.0 there are N clusters (singletons) and at cut level 1.0 there is 1 cluster (all). Verify the predicted cluster structure from Section 4.5.

**Risk:** The hierarchical clustering loop is O(n^3) in naive implementation (n merge steps, each scanning O(n^2) pairs). For n=1024, this is ~1 billion operations. Mitigate: maintain a priority queue of cluster pairs sorted by distance. Update only affected pairs after each merge. This reduces to O(n^2 log n).

**Estimated complexity:** ~250 lines of Prolog. The merge loop and priority queue maintenance are the most complex pieces.

### Phase 4: Isomorphism Detection + Report Generation

**Goal:** Detect structural twins (especially cross-domain) and generate the trajectory mining report.

**Tasks:**
1. Implement `compute_evidence/3` for isomorphism evidence computation
2. Implement `classify_level/2` — maps evidence to strict/trajectory/family/none
3. Implement `structural_isomorphism/4`
4. Implement `cross_domain_twins/3` — the filtered cross-domain pair search
5. Implement `structural_family/2` as alias for cluster_members
6. Implement `trajectory_report/2` — markdown report generation
7. Add trajectory mining step to `run_full_pipeline.sh` (conditional on `trajectory_enabled`)
8. Add `trajectory_selftest/0` — validates against the 6 genuine findings

**Testing:** Verify that noethers_theorem_symmetry and reciprocity_laws_math are flagged as strict isomorphs. Verify that moltbook_agent_theater and ulysses_calypso_1904 are flagged as trajectory isomorphs. Verify cross-domain twin detection across the full corpus.

**Estimated complexity:** ~250 lines of Prolog for the predicates, ~150 lines for report generation.

### Total Estimated Complexity

~1,150 lines of Prolog across all 4 phases. This is comparable to `maxent_classifier.pl` (~400 lines) + `abductive_engine.pl` (~350 lines) combined, which is proportionate to the added functionality.

---

## 8. Cost/Benefit Assessment: Multi-Context MaxEnt

### 8.1 Current State

`maxent_run/2` runs at a single context (typically the default moderate/biographical/mobile/national context). It computes Gaussian log-likelihood + boolean features + corpus priors for each of the 6 types, for each constraint. The output is one 6-type probability distribution per constraint.

### 8.2 What Multi-Context MaxEnt Would Provide

Running MaxEnt at each of the 4 standard contexts produces:
- 4 distributions per constraint (instead of 1)
- An **entropy trajectory**: how uncertainty changes as the observer gains power
- Per-context disagreement detection: a constraint might agree at one context and disagree at another
- Richer trajectory representation: the `maxent(Distribution)` and `entropy(H_norm)` fields in trajectory_point would be populated with actual per-context values instead of the degraded single-context fill

### 8.3 Computation Cost

| Component | Single-context | 4-context | Multiplier |
|-----------|---------------|-----------|------------|
| MaxEnt precompute | 1x | 4x | 4.0x |
| MaxEnt classify all | 1x | 4x | 4.0x |
| Dynamic fact storage | ~1K facts | ~4K facts | 4.0x |
| Total MaxEnt runtime | ~2-5 seconds | ~8-20 seconds | 4.0x |

The 4x multiplier is a linear constant factor. No algorithmic complexity increase.

**Downstream cost in trajectory mining:**
- Trajectory computation: negligible increase (4 lookups instead of 1 repeated)
- Pairwise distance: no increase (the metric_distance component changes how it reads entropy, not how many comparisons it makes)
- Total pipeline impact: MaxEnt goes from ~3% of pipeline runtime to ~12%. Still dominated by the O(n^2) pairwise distance and O(n^2 log n) clustering.

### 8.4 Analytical Value

**High value for Family A detection:** The 4 metric_structural_divergence findings are distinguished by elevated entropy at the default context. Multi-context MaxEnt would reveal whether this entropy is:
- **Context-invariant** (high entropy from every perspective) — indicating genuine metric-level ambiguity that's intrinsic to the constraint
- **Context-dependent** (high entropy from moderate, low from institutional) — indicating that the ambiguity resolves for certain observers

This distinction directly informs whether the constraint needs reclassification (intrinsic ambiguity) or perspectival documentation (context-dependent ambiguity).

**High value for Family B detection:** The 2 confirmed_liminal findings already show orbit violations (different types at different contexts). Multi-context MaxEnt would quantify *how confident* the classification is at each context. Currently we know moltbook_agent_theater is rope from the institutional perspective and tangled_rope from others, but we don't know whether the institutional classification is 95% confident or 51% confident. This matters for the "institutional escape hatch" analysis in Phase A Finding 5.

**Moderate value for unknown-containing orbits:** 157 constraints contain `unknown` in their orbit (7 orbit families). Multi-context MaxEnt might resolve some unknowns by providing a probabilistic classification even where the deterministic cascade falls through.

### 8.5 Recommendation

**Implement multi-context MaxEnt in Phase 1 of trajectory mining.** The cost is low (4x a cheap operation), the implementation is straightforward (iterate `maxent_run/2` over 4 contexts instead of 1, store with context key), and the analytical value for trajectory mining is high — it's the single biggest improvement to the trajectory representation.

Concretely:

```prolog
%% maxent_multi_run(+Contexts, -Summaries)
%  Runs MaxEnt at each context in Contexts. Stores distributions
%  indexed by (Constraint, Context). If Contexts = [default_only],
%  runs once and replicates.
maxent_multi_run(Contexts, Summaries) :-
    maplist(maxent_run, Contexts, Summaries).
```

The existing `maxent_distribution/3`, `maxent_entropy/3`, `maxent_confidence/3` already take a Context parameter. The internal storage (`maxent_dist/3`) already includes context. The only change needed is calling `maxent_run/2` four times with different contexts instead of once.

**Fallback:** If multi-context MaxEnt is deferred, the trajectory mining system runs in degraded mode using single-context entropy for all 4 trajectory points. This still works — the 4 Family A findings are still detectable by their elevated single-context entropy. The degraded mode just can't answer the context-invariance question from Section 8.4.

---

## Summary of Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Trajectory representation | 4 points + summary | Mirrors gauge orbit structure; captures per-context variation |
| Distance metric | 4 weighted components | Separates structural (shift, stability) from behavioral (metric, pathology) |
| Clustering algorithm | Hierarchical agglomerative, average linkage | Dendrograms allow user-chosen granularity; no k required |
| Isomorphism levels | strict / trajectory / family | Progressive relaxation from exact match to orbit-level |
| Multi-context MaxEnt | Recommended for Phase 1 | Low cost (4x), high analytical value |
| Implementation language | Pure Prolog | Consistent with codebase; leverages existing predicate infrastructure |
| Phasing | 4 phases, ~1,150 lines total | Incremental value delivery; each phase independently testable |

---

*End of trajectory mining design document*
