
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================
# Giant Component Analysis: Erdos-Renyi Phase Transition

*Investigates whether the constraint network exhibits a phase transition*  
*in connected component structure as coupling threshold varies.*

---

## Phase 1: Network Topology at Default Threshold

**Context**: analytical/global (default)  
**Coupling threshold**: 0.500

### Network Summary

| Metric | Value |
|--------|-------|
| Total nodes (constraints) | 258 |
| Connected nodes (degree > 0) | 165 |
| Isolated nodes (degree 0) | 93 |
| Edges | 179 |
| Graph density | 0.005399 |
| Average degree | 1.39 |
| Connected components | 140 |
| E-R critical edge count (n/2) | 129.0 |

### Degree Distribution

| Stat | Value |
|------|-------|
| N | 258 |
| Min | 0 |
| Q1 | 0 |
| Median | 1 |
| Q3 | 2 |
| Max | 6 |
| Mean | 1.39 |

#### Degree Histogram

| Degree Range | Count |
|-------------|-------|
| 0 (isolated) | 93 |
| 1 | 61 |
| 2-3 | 83 |
| 4-6 | 21 |
| 7-10 | 0 |
| 11-20 | 0 |
| 21+ | 0 |

### Connected Components

**140 components** found.

**Largest component**: 12 nodes (4.7% of network)

**No giant component.** The network is fragmented at this threshold.

#### Top Components by Size

| Rank | Size | Fraction |
|------|------|----------|
| 1 | 12 | 0.047 |
| 2 | 8 | 0.031 |
| 3 | 8 | 0.031 |
| 4 | 7 | 0.027 |
| 5 | 6 | 0.023 |
| 6 | 6 | 0.023 |
| 7 | 5 | 0.019 |
| 8 | 5 | 0.019 |
| 9 | 5 | 0.019 |
| 10 | 5 | 0.019 |
| 11 | 4 | 0.016 |
| 12 | 4 | 0.016 |
| 13 | 4 | 0.016 |
| 14 | 4 | 0.016 |
| 15 | 4 | 0.016 |
### Type Distribution

| Type | Count | Fraction |
|------|-------|----------|
| mountain | 4 | 0.016 |
| rope | 18 | 0.070 |
| scaffold | 24 | 0.093 |
| tangled_rope | 39 | 0.151 |
| piton | 14 | 0.054 |
| snare | 105 | 0.407 |
| unknown | 54 | 0.209 |

### Purity Landscape

#### Intrinsic Purity (228/258 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.312 |
| Q1 | 0.354 |
| Median | 0.466 |
| Q3 | 0.647 |
| Max | 1.000 |
| Mean | 0.541 |

#### Effective Purity (228/258 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.312 |
| Q1 | 0.354 |
| Median | 0.466 |
| Q3 | 0.647 |
| Max | 0.996 |
| Mean | 0.536 |

#### Purity Zone Distribution

| Zone | Intrinsic | Effective | Shift |
|------|-----------|-----------|-------|
| Sound (>= 0.70) | 43 | 43 | 0 |
| Borderline (0.50 - 0.70) | 58 | 57 | 1 |
| Warning (0.30 - 0.50) | 127 | 128 | -1 |
| Degraded (< 0.30) | 0 | 0 | 0 |

**1 constraints shifted purity zone** due to network contamination effects.

### Super-spreaders (Highest Contamination Potential)

| Constraint | Type | Degree | Contam Str | Eff Purity | Potential |
|------------|------|--------|------------|------------|-----------|
| epistemic_collapse | snare | 5 | 1.00 | -1.000 | 5.00 |
| authoritative_specification_reading | snare | 4 | 1.00 | 0.354 | 4.00 |
| dispositional_reading | snare | 3 | 1.00 | 0.354 | 3.00 |
| evaluator_incentive_asymmetry | piton | 3 | 0.80 | 0.546 | 2.40 |
| ability_ceiling_reading | snare | 2 | 1.00 | -1.000 | 2.00 |
| adjacency_reading | piton | 2 | 0.80 | 0.490 | 1.60 |
| adverse_effect_guarantee_kernel_flat_control | tangled_rope | 3 | 0.50 | 0.485 | 1.50 |
| arbitrary_selection_under_competence_signaling | snare | 1 | 1.00 | 0.530 | 1.00 |
| authority_vacuum_incommensurability | piton | 1 | 0.80 | 0.633 | 0.80 |
| fictional_construct_reading | scaffold | 3 | 0.20 | 0.754 | 0.60 |
| adverse_effect_measurability_flat_control | tangled_rope | 1 | 0.50 | 0.458 | 0.50 |
| bureaucratic_drift_reading | scaffold | 2 | 0.20 | 0.794 | 0.40 |
| stance_reading | rope | 3 | 0.10 | 0.984 | 0.30 |
| autonomy_reading | scaffold | 1 | 0.20 | 0.766 | 0.20 |
| deflationary_reading | rope | 1 | 0.10 | -1.000 | 0.10 |


---

## Phase 2: Threshold Sweep (Erdos-Renyi Phase Transition)

**No inferred coupling edges** in the corpus (0 constraints with gradient data).
Threshold sweep is degenerate: all thresholds produce the same edge set (only `explicit` and `shared_agent` edges survive regardless of threshold).

| Threshold | Edges | Components | Largest | Fraction |
|-----------|-------|------------|---------|----------|
| 0.500 (all) | 179 | 140 | 12 | 0.047 |


---

## Phase 3: Contamination Through the Giant Component

**Threshold**: 0.500 (default)

**No significant component found** at threshold 0.500. The largest component contains fewer than 10% of nodes.

This means the network is naturally fragmented at the current coupling threshold. Contamination cannot cascade across the full network because constraints are organized into small, isolated clusters.

### Contamination at Lower Threshold

No giant component (>25% of nodes) found at any threshold from 0.10 to 0.50.
The network is inherently fragmented.

---

## Phase 4: Context Comparison

The edge set is context-independent (edges come from `affects_constraint`, `infer_structural_coupling`, and `shared_agent_link` — none of which depend on observer context). What changes across contexts is the **type classification** and hence the **contamination dynamics**.

**Fixed topology**: 179 edges, 140 components, largest = 12 nodes (threshold = 0.500)

### Type Distribution by Context

| Type | Institutional/Local | Moderate/National | Analytical/Global (default) |
|------|------|------|------|
| mountain | 0 | 0 | 4 |
| rope | 183 | 26 | 18 |
| scaffold | 50 | 37 | 24 |
| tangled_rope | 1 | 50 | 39 |
| piton | 14 | 15 | 14 |
| snare | 2 | 100 | 105 |
| naturalized | 7 | 0 | 0 |
| unknown | 1 | 30 | 54 |

### Contamination Source Comparison

Number of constraints that are active contamination sources (type strength > 0, acts as contamination emitter) by context:

| Context | Snare | Piton | Tangled Rope | Scaffold | Total Sources |
|---------|-------|-------|-------------|----------|---------------|
| Institutional/Local | 2 | 14 | 1 | 50 | 67 |
| Moderate/National | 100 | 15 | 50 | 37 | 202 |
| Analytical/Global (default) | 105 | 14 | 39 | 24 | 182 |

### Key Finding

Since edges are context-independent, the network topology (connected components, component sizes, degree distribution) is identical across all contexts. What changes is WHICH nodes are contamination sources. A constraint classified as a snare from one context (high contamination strength = 1.0) may be classified as a rope from another (low strength = 0.1). This means the effective contamination pressure varies by context even though the network structure does not.

---

## Embedded Prolog Facts

```prolog
%% Sweep results: gc_sweep_result(Threshold, NEdges, NComponents, LargestSize, LargestFraction)
gc_sweep_result(0.500, 179, 140, 12, 0.047).
```

---

## Provenance split (OQ-193)

*Pooled topology counts within-kernel reading-plurality (sibling `affects_constraint` edges) as coupling. The stratum strips explicit same-kernel sibling edges (retract-recompute) to expose cross-kernel structure. Operator ruling (c), 2026-07-02: siblings STAY in the engine topology — this is a presentation split only, no engine-behavior change.*

**Sibling edges stripped**: 240  
**same_kernel_edges_surviving**: 0 (dedup-resurfaced 0, never-stripped 0)  
**Positive control**: ok — raw `affects_constraint` dropped by exactly 240.

| Stratum | Edges | Components | Giant size | Giant fraction |
|---------|-------|------------|------------|----------------|
| Pooled | 179 | 140 | 12 | 0.047 |
| Cross-kernel | 56 | 206 | 9 | 0.035 |

---

*End of giant component analysis*
