
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
| Total nodes (constraints) | 37 |
| Connected nodes (degree > 0) | 24 |
| Isolated nodes (degree 0) | 13 |
| Edges | 75 |
| Graph density | 0.112613 |
| Average degree | 4.05 |
| Connected components | 15 |
| E-R critical edge count (n/2) | 18.5 |

### Degree Distribution

| Stat | Value |
|------|-------|
| N | 37 |
| Min | 0 |
| Q1 | 0 |
| Median | 3 |
| Q3 | 5 |
| Max | 10 |
| Mean | 3.35 |

#### Degree Histogram

| Degree Range | Count |
|-------------|-------|
| 0 (isolated) | 13 |
| 1 | 1 |
| 2-3 | 10 |
| 4-6 | 5 |
| 7-10 | 8 |
| 11-20 | 0 |
| 21+ | 0 |

### Connected Components

**15 components** found.

**Largest component**: 44 nodes (118.9% of network)

**Giant component detected.** The largest component contains >50% of all nodes.

#### Top Components by Size

| Rank | Size | Fraction |
|------|------|----------|
| 1 | 44 | 1.189 |
| 2 | 5 | 0.135 |
| 3 | 1 | 0.027 |
| 4 | 1 | 0.027 |
| 5 | 1 | 0.027 |
| 6 | 1 | 0.027 |
| 7 | 1 | 0.027 |
| 8 | 1 | 0.027 |
| 9 | 1 | 0.027 |
| 10 | 1 | 0.027 |
| 11 | 1 | 0.027 |
| 12 | 1 | 0.027 |
| 13 | 1 | 0.027 |
| 14 | 1 | 0.027 |
| 15 | 1 | 0.027 |
### Type Distribution

| Type | Count | Fraction |
|------|-------|----------|
| mountain | 1 | 0.027 |
| rope | 4 | 0.108 |
| tangled_rope | 11 | 0.297 |
| snare | 19 | 0.514 |
| unknown | 2 | 0.054 |

### Purity Landscape

#### Intrinsic Purity (37 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.312 |
| Q1 | 0.354 |
| Median | 0.482 |
| Q3 | 0.751 |
| Max | 1.000 |
| Mean | 0.569 |

#### Effective Purity (37 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.094 |
| Q1 | 0.341 |
| Median | 0.474 |
| Q3 | 0.605 |
| Max | 1.000 |
| Mean | 0.529 |

#### Purity Zone Distribution

| Zone | Intrinsic | Effective | Shift |
|------|-----------|-----------|-------|
| Sound (>= 0.70) | 11 | 8 | 3 |
| Borderline (0.50 - 0.70) | 6 | 7 | -1 |
| Warning (0.30 - 0.50) | 20 | 21 | -1 |
| Degraded (< 0.30) | 0 | 1 | -1 |

**4 constraints shifted purity zone** due to network contamination effects.

### Super-spreaders (Highest Contamination Potential)

| Constraint | Type | Degree | Contam Str | Eff Purity | Potential |
|------------|------|--------|------------|------------|-----------|
| employment_boundary_flat_control | snare | 10 | 1.00 | 0.312 | 10.00 |
| truth_democracy_disinformation | snare | 9 | 1.00 | 0.312 | 9.00 |
| digital_power_concentration | snare | 5 | 1.00 | 0.489 | 5.00 |
| war_normalization_ai_weapons | snare | 4 | 1.00 | 0.396 | 4.00 |
| wage_convergence_sustainability | tangled_rope | 7 | 0.50 | 0.526 | 3.50 |
| surveillance_control_freedom | snare | 3 | 1.00 | 0.348 | 3.00 |
| ai_governance_accountability | snare | 2 | 1.00 | 0.312 | 2.00 |
| magisterial_integralist_reading | tangled_rope | 3 | 0.50 | 0.639 | 1.50 |
| war_normalization_autonomous_weapons | snare | 1 | 1.00 | 0.454 | 1.00 |
| hybrid_security_reading | rope | 8 | 0.10 | 0.094 | 0.80 |


---

## Phase 2: Threshold Sweep (Erdos-Renyi Phase Transition)

**No inferred coupling edges** in the corpus (0 constraints with gradient data).
Threshold sweep is degenerate: all thresholds produce the same edge set (only `explicit` and `shared_agent` edges survive regardless of threshold).

| Threshold | Edges | Components | Largest | Fraction |
|-----------|-------|------------|---------|----------|
| 0.500 (all) | 75 | 15 | 44 | 1.189 |


---

## Phase 3: Contamination Through the Giant Component

**Threshold**: 0.500 (default)

**Giant component size**: 44 nodes (118.9% of network)

### Giant Component Composition

| Type | Count | Fraction |
|------|-------|----------|
| rope | 1 | 0.023 |
| tangled_rope | 4 | 0.091 |
| snare | 15 | 0.341 |
| unknown | 2 | 0.045 |

#### Purity Within Giant Component

- **Intrinsic**: min=0.312, median=0.354, max=0.794, mean=0.471
- **Effective**: min=0.094, median=0.351, max=0.639, mean=0.405

- **Active contamination sources** (intrinsic purity < 0.50): 14
- **Sound constraints** (effective purity >= 0.70): 0

### Contamination Sources (Super-spreaders in Giant Component)

**8 contamination-capable nodes** in the giant component.

| Constraint | Type | Intra-GC Degree | Contam Str | Eff Purity | Potential |
|------------|------|-----------------|------------|------------|-----------|
| employment_boundary_flat_control | snare | 10 | 1.00 | 0.312 | 10.00 |
| truth_democracy_disinformation | snare | 9 | 1.00 | 0.312 | 9.00 |
| digital_power_concentration | snare | 5 | 1.00 | 0.489 | 5.00 |
| wage_convergence_sustainability | tangled_rope | 7 | 0.50 | 0.526 | 3.50 |
| surveillance_control_freedom | snare | 3 | 1.00 | 0.348 | 3.00 |
| ai_governance_accountability | snare | 2 | 1.00 | 0.312 | 2.00 |
| magisterial_integralist_reading | tangled_rope | 3 | 0.50 | 0.639 | 1.50 |
| hybrid_security_reading | rope | 8 | 0.10 | 0.094 | 0.80 |

### Multi-hop Contamination Simulation

Simulating contamination propagation beyond the current one-hop model.
Attenuation: 0.50 per hop. Stop when attenuation * strength < 0.01.

**13 active contamination sources** (type strength >= 0.5, purity < 0.50)

| Source | Type | Purity | 1-hop | 2-hop | 3-hop | Total Reach |
|--------|------|--------|-------|-------|-------|-------------|
| employment_boundary_flat_control | snare | 0.312 | 10 | 14 | 2 | 26 |
| truth_democracy_disinformation | snare | 0.312 | 9 | 16 | 5 | 30 |
| technocratic_paradigm_resistance | snare | 0.354 | 5 | 1 | 3 | 9 |
| surveillance_control_freedom | snare | 0.354 | 3 | 11 | 16 | 30 |
| ai_governance_accountability | snare | 0.312 | 2 | 4 | 6 | 12 |

**Total unique nodes reached** within 3 hops of any source: 31 (70.5% of giant component)

### Sound Constraint Exposure to Contamination

**0 sound constraints** (effective purity >= 0.70) in the giant component.

No contamination sources or no sound constraints in the giant component.

### Contamination Collapse Analysis

At what contamination settings would sound constraints in the giant component collapse into the degraded zone?

Current settings: cap=0.30, attenuation=0.50
Sound constraints in giant component: 0

Sweeping contamination_cap from 0.10 to 1.00 (attenuation fixed at 0.50):

| Cap | Sound (>=0.70) | Borderline | Warning | Degraded (<0.30) |
|-----|--------|------------|---------|---------|
| 0.10 | 0 | 6 | 16 | 0 |
| 0.20 | 0 | 6 | 15 | 1 |
| 0.30 | 0 | 6 | 15 | 1 |
| 0.40 | 0 | 6 | 15 | 1 |
| 0.50 | 0 | 6 | 15 | 1 |
| 0.60 | 0 | 6 | 15 | 1 |
| 0.70 | 0 | 6 | 15 | 1 |
| 0.80 | 0 | 6 | 15 | 1 |
| 0.90 | 0 | 6 | 15 | 1 |
| 1.00 | 0 | 6 | 15 | 1 |

---

## Phase 4: Context Comparison

The edge set is context-independent (edges come from `affects_constraint`, `infer_structural_coupling`, and `shared_agent_link` — none of which depend on observer context). What changes across contexts is the **type classification** and hence the **contamination dynamics**.

**Fixed topology**: 75 edges, 15 components, largest = 44 nodes (threshold = 0.500)

### Type Distribution by Context

| Type | Institutional/Local | Moderate/National | Analytical/Global (default) |
|------|------|------|------|
| mountain | 1 | 1 | 1 |
| rope | 23 | 6 | 4 |
| scaffold | 2 | 1 | 0 |
| tangled_rope | 7 | 15 | 11 |
| piton | 0 | 0 | 0 |
| snare | 0 | 12 | 19 |
| naturalized | 4 | 1 | 0 |
| unknown | 0 | 1 | 2 |

### Contamination Source Comparison

Number of constraints that are active contamination sources (type strength > 0, acts as contamination emitter) by context:

| Context | Snare | Piton | Tangled Rope | Scaffold | Total Sources |
|---------|-------|-------|-------------|----------|---------------|
| Institutional/Local | 0 | 0 | 7 | 2 | 9 |
| Moderate/National | 12 | 0 | 15 | 1 | 28 |
| Analytical/Global (default) | 19 | 0 | 11 | 0 | 30 |

### Key Finding

Since edges are context-independent, the network topology (connected components, component sizes, degree distribution) is identical across all contexts. What changes is WHICH nodes are contamination sources. A constraint classified as a snare from one context (high contamination strength = 1.0) may be classified as a rope from another (low strength = 0.1). This means the effective contamination pressure varies by context even though the network structure does not.

---

## Embedded Prolog Facts

```prolog
%% Sweep results: gc_sweep_result(Threshold, NEdges, NComponents, LargestSize, LargestFraction)
gc_sweep_result(0.500, 75, 15, 44, 1.189).
```

---

*End of giant component analysis*
