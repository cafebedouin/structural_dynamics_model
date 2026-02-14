
# Invertibility Analysis of Context-Tuple Transformations

## Executive Summary

- **[mountain]** (4 constraints): Strategy B success rate = **100.0%** (48/48 roundtrips)
- **[mountain,unknown]** (14 constraints): Strategy B success rate = **50.0%** (84/168 roundtrips)
- **[piton,rope]** (3 constraints): Strategy B success rate = **0.0%** (0/36 roundtrips)
- **[rope]** (18 constraints): Strategy B success rate = **100.0%** (216/216 roundtrips)
- **[rope,scaffold]** (1 constraints): Strategy B success rate = **0.0%** (0/12 roundtrips)
- **[rope,scaffold,unknown]** (2 constraints): Strategy B success rate = **25.0%** (6/24 roundtrips)
- **[rope,snare]** (156 constraints): Strategy B success rate = **100.0%** (1872/1872 roundtrips)
- **[rope,snare,tangled_rope]** (52 constraints): Strategy B success rate = **42.8%** (267/624 roundtrips)
- **[rope,tangled_rope]** (58 constraints): Strategy B success rate = **97.4%** (678/696 roundtrips)
- **[rope,tangled_rope,unknown]** (22 constraints): Strategy B success rate = **30.7%** (81/264 roundtrips)
- **[rope,unknown]** (20 constraints): Strategy B success rate = **56.2%** (135/240 roundtrips)
- **[scaffold]** (10 constraints): Strategy B success rate = **100.0%** (120/120 roundtrips)
- **[scaffold,snare]** (2 constraints): Strategy B success rate = **0.0%** (0/24 roundtrips)
- **[scaffold,snare,tangled_rope]** (3 constraints): Strategy B success rate = **0.0%** (0/36 roundtrips)
- **[scaffold,snare,unknown]** (5 constraints): Strategy B success rate = **45.0%** (27/60 roundtrips)
- **[scaffold,tangled_rope]** (2 constraints): Strategy B success rate = **0.0%** (0/24 roundtrips)
- **[scaffold,tangled_rope,unknown]** (2 constraints): Strategy B success rate = **50.0%** (12/24 roundtrips)
- **[scaffold,unknown]** (9 constraints): Strategy B success rate = **75.0%** (81/108 roundtrips)
- **[tangled_rope]** (92 constraints): Strategy B success rate = **100.0%** (1104/1104 roundtrips)

**Overall**: 83.0% success (4731/5700)

## 1. Strategy B Results: Failure Rates by Orbit Family

### Family: [rope,snare]

Population: 156 constraints

| Source → Target | Success | Fail | Rate |
|---|---|---|---|
| powerless → moderate | 156 | 0 | 100.0% |
| powerless → institutional | 156 | 0 | 100.0% |
| powerless → analytical | 156 | 0 | 100.0% |
| moderate → powerless | 156 | 0 | 100.0% |
| moderate → institutional | 156 | 0 | 100.0% |
| moderate → analytical | 156 | 0 | 100.0% |
| institutional → powerless | 156 | 0 | 100.0% |
| institutional → moderate | 156 | 0 | 100.0% |
| institutional → analytical | 156 | 0 | 100.0% |
| analytical → powerless | 156 | 0 | 100.0% |
| analytical → moderate | 156 | 0 | 100.0% |
| analytical → institutional | 156 | 0 | 100.0% |

No failures in this family.

### Family: [tangled_rope]

Population: 92 constraints

| Source → Target | Success | Fail | Rate |
|---|---|---|---|
| powerless → moderate | 92 | 0 | 100.0% |
| powerless → institutional | 92 | 0 | 100.0% |
| powerless → analytical | 92 | 0 | 100.0% |
| moderate → powerless | 92 | 0 | 100.0% |
| moderate → institutional | 92 | 0 | 100.0% |
| moderate → analytical | 92 | 0 | 100.0% |
| institutional → powerless | 92 | 0 | 100.0% |
| institutional → moderate | 92 | 0 | 100.0% |
| institutional → analytical | 92 | 0 | 100.0% |
| analytical → powerless | 92 | 0 | 100.0% |
| analytical → moderate | 92 | 0 | 100.0% |
| analytical → institutional | 92 | 0 | 100.0% |

No failures in this family.

### Family: [rope,snare,tangled_rope]

Population: 52 constraints

| Source → Target | Success | Fail | Rate |
|---|---|---|---|
| powerless → moderate | 37 | 15 | 71.2% |
| powerless → institutional | 52 | 0 | 100.0% |
| powerless → analytical | 0 | 52 | 0.0% |
| moderate → powerless | 0 | 52 | 0.0% |
| moderate → institutional | 52 | 0 | 100.0% |
| moderate → analytical | 0 | 52 | 0.0% |
| institutional → powerless | 0 | 52 | 0.0% |
| institutional → moderate | 37 | 15 | 71.2% |
| institutional → analytical | 0 | 52 | 0.0% |
| analytical → powerless | 0 | 52 | 0.0% |
| analytical → moderate | 37 | 15 | 71.2% |
| analytical → institutional | 52 | 0 | 100.0% |

**Notable failures** (357 total):

- `airbnb_str_regulation`: tangled_rope(powerless) → predicted tangled_rope, actual snare(moderate)
- `airbnb_str_regulation`: tangled_rope(powerless) → predicted tangled_rope, actual snare(analytical)
- `airbnb_str_regulation`: snare(moderate) → predicted snare, actual tangled_rope(powerless)
- `airbnb_str_regulation`: snare(moderate) → predicted tangled_rope, actual snare(analytical)
- `airbnb_str_regulation`: rope(institutional) → predicted snare, actual tangled_rope(powerless)

### Family: [rope,tangled_rope]

Population: 58 constraints

| Source → Target | Success | Fail | Rate |
|---|---|---|---|
| powerless → moderate | 55 | 3 | 94.8% |
| powerless → institutional | 58 | 0 | 100.0% |
| powerless → analytical | 58 | 0 | 100.0% |
| moderate → powerless | 55 | 3 | 94.8% |
| moderate → institutional | 58 | 0 | 100.0% |
| moderate → analytical | 58 | 0 | 100.0% |
| institutional → powerless | 55 | 3 | 94.8% |
| institutional → moderate | 55 | 3 | 94.8% |
| institutional → analytical | 58 | 0 | 100.0% |
| analytical → powerless | 55 | 3 | 94.8% |
| analytical → moderate | 55 | 3 | 94.8% |
| analytical → institutional | 58 | 0 | 100.0% |

**Notable failures** (18 total):

- `local_vs_global_optima`: rope(powerless) → predicted tangled_rope, actual rope(moderate)
- `local_vs_global_optima`: rope(moderate) → predicted tangled_rope, actual rope(powerless)
- `local_vs_global_optima`: rope(institutional) → predicted tangled_rope, actual rope(powerless)
- `local_vs_global_optima`: rope(institutional) → predicted tangled_rope, actual rope(moderate)
- `local_vs_global_optima`: tangled_rope(analytical) → predicted tangled_rope, actual rope(powerless)

### Other Orbit Families

- **[mountain]** (4 constraints): 100.0% success
- **[mountain,unknown]** (14 constraints): 50.0% success
- **[piton,rope]** (3 constraints): 0.0% success
- **[rope]** (18 constraints): 100.0% success
- **[rope,scaffold]** (1 constraints): 0.0% success
- **[rope,scaffold,unknown]** (2 constraints): 25.0% success
- **[rope,tangled_rope,unknown]** (22 constraints): 30.7% success
- **[rope,unknown]** (20 constraints): 56.2% success
- **[scaffold]** (10 constraints): 100.0% success
- **[scaffold,snare]** (2 constraints): 0.0% success
- **[scaffold,snare,tangled_rope]** (3 constraints): 0.0% success
- **[scaffold,snare,unknown]** (5 constraints): 45.0% success
- **[scaffold,tangled_rope]** (2 constraints): 0.0% success
- **[scaffold,tangled_rope,unknown]** (2 constraints): 50.0% success
- **[scaffold,unknown]** (9 constraints): 75.0% success

## 2. Loss Attribution: Which Pipeline Steps Cause Failures

### Aggregate Loss Sources

| Loss Step | Count | % of Failures |
|---|---|---|
| threshold_crossing | 613 | 63.3% |
| gate_priority_shadow | 668 | 68.9% |
| immutability_flip | 515 | 53.1% |
| coalition_upgrade | 5 | 0.5% |
| signature_override_divergence | 0 | 0.0% |

*Note: A single failure may be attributed to multiple loss steps.*

### Loss Sources by Orbit Family

**[rope,snare]**:
  (no failures)

**[tangled_rope]**:
  (no failures)

**[rope,snare,tangled_rope]**:
  - threshold_crossing: 290 (81.2%)
  - gate_priority_shadow: 290 (81.2%)
  - immutability_flip: 223 (62.5%)
  - coalition_upgrade: 5 (1.4%)
  - signature_override_divergence: 0 (0.0%)

**[rope,tangled_rope]**:
  - threshold_crossing: 6 (33.3%)
  - gate_priority_shadow: 6 (33.3%)
  - immutability_flip: 12 (66.7%)
  - coalition_upgrade: 0 (0.0%)
  - signature_override_divergence: 0 (0.0%)


## 3. Tangled Middle Band: Irreducible Information

### Chi Distribution at Moderate Context

| Sub-band | Count |
|---|---|
| low_tangled | 3 |
| mid_tangled | 48 |
| above_range | 1 |

Chi at moderate context: min=0.542, median=0.609, max=0.941

### Observed Context→Type Patterns

The assumed "clean" pattern (powerless=snare, moderate=tangled_rope, institutional=rope)
may not hold. Here are the actually observed patterns:

| Powerless | Moderate | Institutional | Analytical | Count |
|---|---|---|---|---|
| tangled_rope | tangled_rope | rope | snare | 37 |
| tangled_rope | snare | rope | snare | 15 |

### Where Tangled_Rope Appears

- Powerless sees tangled_rope: 52/52
- Moderate sees tangled_rope: 37/52
- Institutional sees tangled_rope: 0/52
- Analytical sees tangled_rope: 0/52

### Irreducibility Assessment

**2 distinct context→type patterns** observed across 52 constraints.

Multiple patterns coexist within the same orbit family.
This means knowing the orbit signature [rope, snare, tangled_rope]
is NOT sufficient to predict which context sees which type.
The tangled_rope position within the orbit carries **irreducible
structural information** about the specific constraint.

Concretely: two constraints can both belong to the [rope, snare, tangled_rope]
orbit but differ in WHERE the tangled_rope appears. This difference
reflects genuine structural variation (e.g., coalition dynamics,
epsilon magnitude) that the orbit signature flattens.

### Concrete Examples

**Most common pattern** (tangled_rope/tangled_rope/rope/snare):
- Example: `future_dsm_integration`

**Second pattern** (tangled_rope/snare/rope/snare):
- Example: `airbnb_str_regulation`


## 4. The Scholze Assessment

### Where the Analogy Illuminates

The tilting equivalence maps between different "completions" of the same
underlying object. In this system:

- The **orbit family** is the underlying object (the perfectoid space)
- Each **context-indexed classification** is a completion
- The **structural invariants** (BaseEps, Supp, flags, signature) are the perfectoid core
- **Singleton orbits** are perfectoid primes — trivially invertible

The orbit taxonomy successfully organizes constraints by their gauge structure,
and the Strategy B test quantifies exactly how much of that structure
is recoverable from type information alone.

### Where It Breaks Down

1. **Tilting is exact; this is not.** Scholze\'s tilting equivalence is an
   *equivalence of categories* — no information is lost. The DR pipeline has
   threshold gates that intentionally discard distance-from-threshold.
   Empirically, 17.0% of roundtrips lose information.

2. **No simpler characteristic.** Tilting works because characteristic p
   is "simpler" than characteristic 0. There is no analog here — the
   analytical context is the most comprehensive, not the simplest.

3. **Lossiness is load-bearing.** Classification MEANS choosing a discrete
   type from continuous data. The thresholds are not accidental — they
   encode the system\'s theory of what distinctions matter. Removing
   the lossiness would remove the classification.

### Verdict

The Scholze analogy is a **productive metaphor** for organizing the analysis
but does not hold as a formal equivalence. The orbit taxonomy recovers
most of the gauge structure (Strategy B succeeds for the majority of
roundtrips), but the threshold boundaries introduce genuine, irreversible
information loss that is *by design*.

## 5. Practical Implications

### For `check_all_contexts`

The finding that Strategy B has non-trivial failure rates means that
`check_all_contexts` cannot be replaced by a single-context query plus
an orbit-based reconstruction. The orbit signature tells you *which types*
are possible but not *which context produces which type* — that requires
the full pipeline (Strategy A), which needs the constraint identity.

### For the Reform Pipeline

The [rope, snare, tangled_rope] orbit (52 constraints) shows 2 distinct
context→type patterns. The non-uniqueness means that knowing a constraint
is in this orbit family does NOT tell you which context sees the tangled
middle — the coalition upgrade mechanism and epsilon magnitude create
structural variation within the family. Reform analysis must still
run the full pipeline per constraint.

### For Constraint Authors

The orbit taxonomy provides a **structural fingerprint** that is more
informative than any single classification. A constraint\'s orbit signature
encodes its gauge structure — how it transforms across perspectives.
This is a first-class property that should be surfaced in constraint reports.
