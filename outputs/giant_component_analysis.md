
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
| Total nodes (constraints) | 1033 |
| Connected nodes (degree > 0) | 543 |
| Isolated nodes (degree 0) | 490 |
| Edges | 775 |
| Graph density | 0.001454 |
| Average degree | 1.50 |
| Connected components | 792 |
| E-R critical edge count (n/2) | 516.5 |

### Degree Distribution

| Stat | Value |
|------|-------|
| N | 1033 |
| Min | 0 |
| Q1 | 0 |
| Median | 1 |
| Q3 | 1 |
| Max | 10 |
| Mean | 1.06 |

#### Degree Histogram

| Degree Range | Count |
|-------------|-------|
| 0 (isolated) | 490 |
| 1 | 289 |
| 2-3 | 183 |
| 4-6 | 55 |
| 7-10 | 16 |
| 11-20 | 0 |
| 21+ | 0 |

### Connected Components

**792 components** found.

**Largest component**: 41 nodes (4.0% of network)

**No giant component.** The network is fragmented at this threshold.

#### Top Components by Size

| Rank | Size | Fraction |
|------|------|----------|
| 1 | 41 | 0.040 |
| 2 | 33 | 0.032 |
| 3 | 23 | 0.022 |
| 4 | 19 | 0.018 |
| 5 | 18 | 0.017 |
| 6 | 13 | 0.013 |
| 7 | 11 | 0.011 |
| 8 | 11 | 0.011 |
| 9 | 10 | 0.010 |
| 10 | 10 | 0.010 |
| 11 | 9 | 0.009 |
| 12 | 8 | 0.008 |
| 13 | 8 | 0.008 |
| 14 | 8 | 0.008 |
| 15 | 7 | 0.007 |
### Type Distribution

| Type | Count | Fraction |
|------|-------|----------|
| mountain | 128 | 0.124 |
| rope | 51 | 0.049 |
| tangled_rope | 282 | 0.273 |
| snare | 519 | 0.502 |
| unknown | 53 | 0.051 |

### Purity Landscape

#### Intrinsic Purity (1008 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.271 |
| Q1 | 0.354 |
| Median | 0.575 |
| Q3 | 0.587 |
| Max | 1.000 |
| Mean | 0.555 |

#### Effective Purity (1008 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.271 |
| Q1 | 0.354 |
| Median | 0.561 |
| Q3 | 0.575 |
| Max | 1.000 |
| Mean | 0.552 |

#### Purity Zone Distribution

| Zone | Intrinsic | Effective | Shift |
|------|-----------|-----------|-------|
| Sound (>= 0.70) | 200 | 198 | 2 |
| Borderline (0.50 - 0.70) | 362 | 359 | 3 |
| Warning (0.30 - 0.50) | 434 | 439 | -5 |
| Degraded (< 0.30) | 12 | 12 | 0 |

**7 constraints shifted purity zone** due to network contamination effects.

### Super-spreaders (Highest Contamination Potential)

| Constraint | Type | Degree | Contam Str | Eff Purity | Potential |
|------------|------|--------|------------|------------|-----------|
| us_sanctions_icc_israel_case | snare | 10 | 1.00 | 0.312 | 10.00 |
| us_usmca_china_leverage | snare | 9 | 1.00 | 0.331 | 9.00 |
| deferred_risk_realization | snare | 8 | 1.00 | 0.561 | 8.00 |
| hypernormie_equilibrium | snare | 7 | 1.00 | 0.520 | 7.00 |
| climate_event_attribution | snare | 6 | 1.00 | 0.312 | 6.00 |
| carbon_credit_markets_2026 | snare | 5 | 1.00 | 0.352 | 5.00 |
| tiktok_us_divestiture_mandate | tangled_rope | 9 | 0.50 | 0.420 | 4.50 |
| adversarial_truth_decay | snare | 4 | 1.00 | 0.575 | 4.00 |
| civilizational_maintenance_debt | tangled_rope | 7 | 0.50 | 0.461 | 3.50 |
| ai_nonconsensual_content_facilitation | snare | 3 | 1.00 | 0.575 | 3.00 |
| rare_earth_hydrogen_extraction | tangled_rope | 5 | 0.50 | 0.529 | 2.50 |
| ai_adoption_stigma | snare | 2 | 1.00 | 0.312 | 2.00 |
| eu_mercosur_trade_agreement | tangled_rope | 3 | 0.50 | 0.606 | 1.50 |
| academic_fashion_modernism_2026 | snare | 1 | 1.00 | 0.354 | 1.00 |
| boom_bust_path_dependency | rope | 7 | 0.10 | 0.585 | 0.70 |
| academic_peer_review_gatekeeping | tangled_rope | 1 | 0.50 | 0.310 | 0.50 |
| open_source_commons | rope | 4 | 0.10 | 0.991 | 0.40 |
| ice_memory_archive | rope | 3 | 0.10 | 0.833 | 0.30 |
| cuny_light_2026 | rope | 2 | 0.10 | -1.000 | 0.20 |
| asean_ceasefire_2011 | rope | 1 | 0.10 | 0.980 | 0.10 |


---

## Phase 2: Threshold Sweep (Erdos-Renyi Phase Transition)

Sweeping `network_coupling_threshold` from 0.10 to 0.90 in steps of 0.05.
For each threshold, only `inferred_coupling` edges are filtered; `explicit` and `shared_agent` edges always survive.

### Sweep Results

| Threshold | Edges | Components | Largest | Fraction |
|-----------|-------|------------|---------|----------|
| 0.100 | 775 | 792 | 41 | 0.040 |
| 0.150 | 775 | 792 | 41 | 0.040 |
| 0.200 | 775 | 792 | 41 | 0.040 |
| 0.250 | 775 | 792 | 41 | 0.040 |
| 0.300 | 775 | 792 | 41 | 0.040 |
| 0.350 | 775 | 792 | 41 | 0.040 |
| 0.400 | 775 | 792 | 41 | 0.040 |
| 0.450 | 775 | 792 | 41 | 0.040 |
| 0.500 | 775 | 792 | 41 | 0.040 |
| 0.550 | 775 | 792 | 41 | 0.040 |
| 0.600 | 775 | 792 | 41 | 0.040 |
| 0.650 | 775 | 792 | 41 | 0.040 |
| 0.700 | 775 | 792 | 41 | 0.040 |
| 0.750 | 775 | 792 | 41 | 0.040 |
| 0.800 | 775 | 792 | 41 | 0.040 |
| 0.850 | 775 | 792 | 41 | 0.040 |
| 0.900 | 775 | 792 | 41 | 0.040 |

**Erdos-Renyi prediction**: For a random graph with n=1033 nodes, the giant component emerges when the number of edges exceeds n/2 = 516.5.

### Phase Transition Analysis

**Steepest jump**: threshold 0.000 (fraction=0.000) -> 0.000 (fraction=0.000), delta = 0.000

**Critical threshold (midpoint of steepest jump)**: 0.000

**Transition width**: Could not identify clean 10%->50% crossing range.

### Comparison to Erdos-Renyi Prediction

- **ER critical edge count**: 516.5 (for n=1033 nodes)

**Verdict**: **No clear phase transition** (delta < 0.05). The network may be naturally resilient to cascading connectivity, or the edge types may be too heterogeneous for a clean ER transition.


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

**Fixed topology**: 775 edges, 792 components, largest = 41 nodes (threshold = 0.500)

### Type Distribution by Context

| Type | Institutional/Local | Moderate/National | Analytical/Global (default) |
|------|------|------|------|
| mountain | 32 | 32 | 128 |
| rope | 758 | 61 | 51 |
| scaffold | 52 | 14 | 0 |
| tangled_rope | 105 | 426 | 282 |
| piton | 0 | 0 | 0 |
| snare | 0 | 359 | 519 |
| indexically_opaque | 4 | 0 | 0 |
| unknown | 82 | 141 | 53 |

### Contamination Source Comparison

Number of constraints that are active contamination sources (type strength > 0, acts as contamination emitter) by context:

| Context | Snare | Piton | Tangled Rope | Scaffold | Total Sources |
|---------|-------|-------|-------------|----------|---------------|
| Institutional/Local | 0 | 0 | 105 | 52 | 157 |
| Moderate/National | 359 | 0 | 426 | 14 | 799 |
| Analytical/Global (default) | 519 | 0 | 282 | 0 | 801 |

### Key Finding

Since edges are context-independent, the network topology (connected components, component sizes, degree distribution) is identical across all contexts. What changes is WHICH nodes are contamination sources. A constraint classified as a snare from one context (high contamination strength = 1.0) may be classified as a rope from another (low strength = 0.1). This means the effective contamination pressure varies by context even though the network structure does not.

---

## Embedded Prolog Facts

```prolog
%% Sweep results: gc_sweep_result(Threshold, NEdges, NComponents, LargestSize, LargestFraction)
gc_sweep_result(0.100, 775, 792, 41, 0.040).
gc_sweep_result(0.150, 775, 792, 41, 0.040).
gc_sweep_result(0.200, 775, 792, 41, 0.040).
gc_sweep_result(0.250, 775, 792, 41, 0.040).
gc_sweep_result(0.300, 775, 792, 41, 0.040).
gc_sweep_result(0.350, 775, 792, 41, 0.040).
gc_sweep_result(0.400, 775, 792, 41, 0.040).
gc_sweep_result(0.450, 775, 792, 41, 0.040).
gc_sweep_result(0.500, 775, 792, 41, 0.040).
gc_sweep_result(0.550, 775, 792, 41, 0.040).
gc_sweep_result(0.600, 775, 792, 41, 0.040).
gc_sweep_result(0.650, 775, 792, 41, 0.040).
gc_sweep_result(0.700, 775, 792, 41, 0.040).
gc_sweep_result(0.750, 775, 792, 41, 0.040).
gc_sweep_result(0.800, 775, 792, 41, 0.040).
gc_sweep_result(0.850, 775, 792, 41, 0.040).
gc_sweep_result(0.900, 775, 792, 41, 0.040).
```

---

*End of giant component analysis*
