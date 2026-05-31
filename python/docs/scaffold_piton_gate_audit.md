# Scaffold & Piton Gate Trace Audit

*Generated 2026-05-29 21:23:12 by `python/scaffold_piton_gate_audit.py`*

## Executive Summary

This audit traces **1 scaffolds** and **1 pitons** through the complete Prolog gate priority chain (`classify_from_metrics/6` in `drl_core.pl`) to diagnose why MaxEnt confidence is universally borderline for these two types.

Key findings:

- **Piton theater ratio**: 1/1 (100.0%) pass the Prolog TR ≥ 0.70 gate; 0 fail
- **Scaffold temporality**: 0 with RAE=F (auto-pass), 1 with RAE=T (depends on has_sunset_clause)
- **RAE=F enrichment in pitons**: 100.0% vs false_ci_rope base rate 25.7% (3.89x)
- **H1**: SUPPORTED
- **H2**: INCONCLUSIVE
- **H3**: NOT SUPPORTED


## 1. Population Extraction


### 1.1 Scaffold Population (N=1)

**Signature distribution:**

| Signature | Count |
| :--- | ---: |
| false_natural_law | 1 |

**MaxEnt top type (post-override):**

| Type | Count | % |
| :--- | ---: | ---: |
| tangled_rope | 1 | 100.0% |

**Raw MaxEnt top type (pre-override):**

| Type | Count | % |
| :--- | ---: | ---: |
| tangled_rope | 1 | 100.0% |

**Confidence band:**

| Band | Count | % |
| :--- | ---: | ---: |
| borderline | 1 | 100.0% |

**Rival type distribution:**

| Rival Type | Count | % |
| :--- | ---: | ---: |
| tangled_rope | 1 | 100.0% |

**Boundary classification:**

| Boundary | Count | % |
| :--- | ---: | ---: |
| scaffold->tangled_rope | 1 | 100.0% |

**Continuous metrics:**

| Metric | N | Mean | Median | Std | Min | Max |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| Epsilon (base_extractiveness) | 1 | 0.5200 | 0.5200 | 0.0000 | 0.5200 | 0.5200 |
| Sigma (suppression) | 1 | 0.6800 | 0.6800 | 0.0000 | 0.6800 | 0.6800 |
| Tau (theater_ratio) | 1 | 0.5500 | 0.5500 | 0.0000 | 0.5500 | 0.5500 |
| Purity Score | 1 | 0.3245 | 0.3245 | 0.0000 | 0.3245 | 0.3245 |
| Confidence (P(claimed)) | 1 | 0.0100 | 0.0100 | 0.0000 | 0.0100 | 0.0100 |
| Confidence Margin | 1 | -0.9400 | -0.9400 | 0.0000 | -0.9400 | -0.9400 |


### 1.2 Piton Population (N=1)

**Signature distribution:**

| Signature | Count |
| :--- | ---: |
| false_natural_law | 1 |

**MaxEnt top type (post-override):**

| Type | Count | % |
| :--- | ---: | ---: |
| tangled_rope | 1 | 100.0% |

**Raw MaxEnt top type (pre-override):**

| Type | Count | % |
| :--- | ---: | ---: |
| tangled_rope | 1 | 100.0% |

**Confidence band:**

| Band | Count | % |
| :--- | ---: | ---: |
| borderline | 1 | 100.0% |

**Rival type distribution:**

| Rival Type | Count | % |
| :--- | ---: | ---: |
| tangled_rope | 1 | 100.0% |

**Boundary classification:**

| Boundary | Count | % |
| :--- | ---: | ---: |
| piton->tangled_rope | 1 | 100.0% |

**Continuous metrics:**

| Metric | N | Mean | Median | Std | Min | Max |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| Epsilon (base_extractiveness) | 1 | 0.3800 | 0.3800 | 0.0000 | 0.3800 | 0.3800 |
| Sigma (suppression) | 1 | 0.6200 | 0.6200 | 0.0000 | 0.6200 | 0.6200 |
| Tau (theater_ratio) | 1 | 0.7600 | 0.7600 | 0.0000 | 0.7600 | 0.7600 |
| Purity Score | 1 | 0.2883 | 0.2883 | 0.0000 | 0.2883 | 0.2883 |
| Confidence (P(claimed)) | 1 | 0.0100 | 0.0100 | 0.0000 | 0.0100 | 0.0100 |
| Confidence Margin | 1 | -0.9400 | -0.9400 | 0.0000 | -0.9400 | -0.9400 |


## 2. Gate Profile Census

Chi (power-scaled extractiveness) is not stored in enriched_pipeline.json. This trace uses base_extractiveness (epsilon) as a proxy. For the analytical context (default), d≈0 so f(d)≈1.0, making Chi ≈ epsilon. Minor deviations are possible from scope scaling σ(S).


### 2.1 Scaffold Gate Profiles

**Gate marginal rates:**

| Gate | True | False | P(True) |
| :--- | ---: | ---: | ---: |
| emerges_naturally | 0 | 1 | 0.0000 |
| requires_active_enforcement | 1 | 0 | 1.0000 |
| has_coordination_function | 1 | 0 | 1.0000 |
| has_asymmetric_extraction | 1 | 0 | 1.0000 |

**Profile distribution:**

| Profile | N | % | Mean Conf | % Border | Mean Eps | Mean Sig | Mean Tau |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| EN=F, RAE=T, HCF=T, HAE=T | 1 | 100.0% | 0.0100 | 100.0% | 0.5200 | 0.6800 | 0.5500 |

**Cross-reference with false_ci_rope profiles:**

- Dominant (F,T,T,T): 1
- Atypical (F,F,T,T): 0
- Other profiles: 0

**RAE=F rate**: 0/1 (0.0%) vs false_ci_rope base rate 25.7% (**0.0x** enrichment)


### 2.2 Piton Gate Profiles

**Gate marginal rates:**

| Gate | True | False | P(True) |
| :--- | ---: | ---: | ---: |
| emerges_naturally | 0 | 1 | 0.0000 |
| requires_active_enforcement | 0 | 1 | 0.0000 |
| has_coordination_function | 1 | 0 | 1.0000 |
| has_asymmetric_extraction | 1 | 0 | 1.0000 |

**Profile distribution:**

| Profile | N | % | Mean Conf | % Border | Mean Eps | Mean Sig | Mean Tau |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| EN=F, RAE=F, HCF=T, HAE=T | 1 | 100.0% | 0.0100 | 100.0% | 0.3800 | 0.6200 | 0.7600 |

**Cross-reference with false_ci_rope profiles:**

- Dominant (F,T,T,T): 0
- Atypical (F,F,T,T): 1
- Other profiles: 0

**RAE=F rate**: 1/1 (100.0%) vs false_ci_rope base rate 25.7% (**3.89x** enrichment)


## 3. Gate Path Trace

Each constraint is traced through the `classify_from_metrics/6` priority chain (drl_core.pl:288-358). The trace uses `base_extractiveness` as a proxy for Chi (power-scaled extractiveness) — see note above.


### 3.1 Scaffold Gate Trace (N=1)

**Prolog gate trace result:**

| Gate Result | Count | % |
| :--- | ---: | ---: |
| tangled_rope | 1 | 100.0% |

**Claimed vs Prolog**: 0 match, 1 mismatch (100.0% intercepted by higher-priority gate)

**Interception by gate:**

| Intercepting Gate | Count | % |
| :--- | ---: | ---: |
| tangled_rope | 1 | 100.0% |


#### 3.1.1 Scaffold Temporality Path

The scaffold gate requires `scaffold_temporality_check` (drl_core.pl:273-276), which passes if `has_sunset_clause=T` OR `requires_active_enforcement=F`.

- **RAE=F** (temporality auto-passes): 0
- **RAE=T** (depends on has_sunset_clause): 1

RAE=T scaffold IDs (sunset-clause-dependent):

- `revolutionary_legitimacy_scaffold`


### 3.2 Piton Gate Trace (N=1)

**Prolog gate trace result:**

| Gate Result | Count | % |
| :--- | ---: | ---: |
| piton | 1 | 100.0% |

**Claimed vs Prolog**: 1 match, 0 mismatch (0.0% intercepted by higher-priority gate)


#### 3.2.1 Piton Theater Ratio Gate

The piton gate requires TR ≥ 0.70 (drl_core.pl:346). This is the threshold that distinguishes theatrical/dead code (piton) from legitimate low-extraction coordination (rope/scaffold).

- **TR ≥ 0.70** (passes piton gate): 1 (100.0%)
- **TR < 0.70** (fails piton gate): 0 (0.0%)


## 4. Rival Type Analysis


### 4.1 Scaffold Rival Analysis

**Scaffold→tangled_rope** (N=1, 100.0%):

Gate profiles: EN=F, RAE=T, HCF=T, HAE=T: 1

Metrics — eps: 0.5200, sigma: 0.6800, tau: 0.5500, conf: 0.0100


### 4.2 Piton Rival Analysis

**Piton→tangled_rope** (N=1, 100.0%):

Gate profiles: EN=F, RAE=F, HCF=T, HAE=T: 1

Metrics — eps: 0.3800, sigma: 0.6200, tau: 0.7600, conf: 0.0100


## 5. Sunset Clause Inference

`has_sunset_clause` is evaluated only within the Prolog inference engine (narrative_ontology.pl:326, default fail). It is not exported to enriched_pipeline.json. However, we can make structural inferences:


### 5.1 Scaffold Sunset Clause Inference

**RAE=T scaffolds** (N=1): scaffold_temporality_check requires has_sunset_clause=T when RAE=T (drl_core.pl:273-276). If the Prolog classified these as scaffold, has_sunset_clause must be T.

IDs:

- `revolutionary_legitimacy_scaffold`

**RAE=F scaffolds** (N=0): scaffold_temporality_check auto-passes when RAE=F (drl_core.pl:276). Sunset clause status is unknown.


### 5.2 Piton RAE Analysis

**RAE=T pitons** (N=0): Pitons with RAE=T: requires active enforcement but claimed as dead/theatrical code. These are enforced pitons — the constraint persists through enforcement even though its function is theatrical.

**RAE=F pitons** (N=1): Pitons with RAE=F: no enforcement required. This is the 'institutional inertia' pattern — the constraint persists through neglect, not through active maintenance.


## 6. Hypothesis Assessment


### 6.1 H1: Piton borderline confidence traces to RAE=F ambiguity

**Verdict: SUPPORTED**

Piton RAE=F rate (100.0%) is 3.89x the false_ci_rope base rate (25.7%). RAE=F is significantly overrepresented in pitons.

- Piton RAE=F count: 1/1
- Piton RAE=F rate: 100.0%
- FCR base rate: 25.7%
- Enrichment ratio: 3.89x
- RAE=F piton rival types: tangled_rope: 1


### 6.2 H2: Scaffold borderline confidence traces to sunset clause unobservability

**Verdict: INCONCLUSIVE**

Insufficient data to assess.

- Scaffolds with RAE=T: 1
- Scaffolds with RAE=F: 0
- RAE=F scaffolds borderline: 0/0 (0.0%)
- Total scaffolds borderline: 1/1 (100.0%)


### 6.3 H3: The RAE=F subgroup is the piton-scaffold population

**Verdict: NOT SUPPORTED**

RAE=F subgroup piton/scaffold rate (0.0%) is not substantially higher than the population rate (0.0%).

- FCR RAE=F total: 9
- FCR RAE=F piton/scaffold: 0 (0.0%)
- FCR piton/scaffold rate (full population): 0.0%

FCR RAE=F claimed type distribution:

| Claimed Type | Count |
| :--- | ---: |
| rope | 7 |
| snare | 2 |


## 7. Recommendations


### 7.1. Expose has_sunset_clause in enriched_pipeline.json

**Impact**: moderate

1 scaffolds have RAE=T, meaning their scaffold classification depends entirely on has_sunset_clause — a gate that is invisible to the MaxEnt classifier. Exposing this gate would give the classifier a direct feature for distinguishing scaffold from tangled_rope in the RAE=T subpopulation.


### 7.2. Address piton gate interception by higher-priority gates

**Impact**: high

All 1/1 pitons pass the TR ≥ 0.70 gate, so theater ratio is not the bottleneck. The real issue is gate priority: 0/1 pitons (0.0%) are intercepted by higher-priority gates before the piton gate is reached. Interceptions: {}. Most pitons have epsilon > 0.25 (the piton Chi ceiling), so the snare and tangled_rope gates fire first. The MaxEnt classifier should encode the piton gate's Chi ≤ 0.25 ceiling and TR ≥ 0.70 floor as joint boolean features. Without these, the classifier has no way to distinguish piton from snare/tangled_rope.


### 7.3. Consider splitting false_ci_rope subcategories by RAE gate

**Impact**: low

H3 verdict: NOT SUPPORTED. RAE=F subgroup piton/scaffold rate (0.0%) is not substantially higher than the population rate (0.0%).


### 7.4. Review gate-trace mismatches as reclassification candidates

**Impact**: moderate

1 constraints are claimed as scaffold/piton but would be classified differently by the Prolog gate priority chain. Piton interceptions: {}. Scaffold interceptions: {'tangled_rope': 1}. These are candidates for reclassification or for documenting why the LLM's claimed type diverges from the gate logic.
