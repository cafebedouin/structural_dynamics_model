# Scaffold & Piton Gate Trace Audit

*Generated 2026-02-23 19:56:17 by `python/scaffold_piton_gate_audit.py`*

## Executive Summary

This audit traces **21 scaffolds** and **95 pitons** through the complete Prolog gate priority chain (`classify_from_metrics/6` in `drl_core.pl`) to diagnose why MaxEnt confidence is universally borderline for these two types.

Key findings:

- **Piton theater ratio**: 95/95 (100.0%) pass the Prolog TR ≥ 0.70 gate; 0 fail
- **Scaffold temporality**: 12 with RAE=F (auto-pass), 9 with RAE=T (depends on has_sunset_clause)
- **RAE=F enrichment in pitons**: 25.3% vs false_ci_rope base rate 6.8% (3.74x)
- **H1**: SUPPORTED
- **H2**: PARTIALLY SUPPORTED
- **H3**: SUPPORTED


## 1. Population Extraction


### 1.1 Scaffold Population (N=21)

**Signature distribution:**

| Signature | Count |
| :--- | ---: |
| false_ci_rope | 15 |
| coupling_invariant_rope | 5 |
| false_natural_law | 1 |

**MaxEnt top type (post-override):**

| Type | Count | % |
| :--- | ---: | ---: |
| rope | 10 | 47.6% |
| tangled_rope | 9 | 42.9% |
| snare | 2 | 9.5% |

**Raw MaxEnt top type (pre-override):**

| Type | Count | % |
| :--- | ---: | ---: |
| rope | 11 | 52.4% |
| tangled_rope | 7 | 33.3% |
| snare | 3 | 14.3% |

**Confidence band:**

| Band | Count | % |
| :--- | ---: | ---: |
| borderline | 21 | 100.0% |

**Rival type distribution:**

| Rival Type | Count | % |
| :--- | ---: | ---: |
| rope | 10 | 47.6% |
| tangled_rope | 9 | 42.9% |
| snare | 2 | 9.5% |

**Boundary classification:**

| Boundary | Count | % |
| :--- | ---: | ---: |
| scaffold->rope | 10 | 47.6% |
| scaffold->tangled_rope | 9 | 42.9% |
| scaffold->snare | 2 | 9.5% |

**Continuous metrics:**

| Metric | N | Mean | Median | Std | Min | Max |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| Epsilon (base_extractiveness) | 21 | 0.2500 | 0.2000 | 0.1620 | 0.0000 | 0.6400 |
| Sigma (suppression) | 21 | 0.4376 | 0.4000 | 0.2465 | 0.1000 | 0.9500 |
| Tau (theater_ratio) | 21 | 0.1971 | 0.1500 | 0.1674 | 0.0500 | 0.6500 |
| Purity Score | 21 | 0.7129 | 0.8258 | 0.2642 | 0.3083 | 1.0000 |
| Confidence (P(claimed)) | 21 | 0.0192 | 0.0100 | 0.0269 | 0.0000 | 0.1213 |
| Confidence Margin | 21 | -0.8869 | -0.9400 | 0.1151 | -0.9985 | -0.5474 |


### 1.2 Piton Population (N=95)

**Signature distribution:**

| Signature | Count |
| :--- | ---: |
| false_ci_rope | 81 |
| false_natural_law | 11 |
| constructed_high_extraction | 2 |
| constructed_low_extraction | 1 |

**MaxEnt top type (post-override):**

| Type | Count | % |
| :--- | ---: | ---: |
| snare | 69 | 72.6% |
| tangled_rope | 18 | 18.9% |
| rope | 7 | 7.4% |
| piton | 1 | 1.1% |

**Raw MaxEnt top type (pre-override):**

| Type | Count | % |
| :--- | ---: | ---: |
| snare | 78 | 82.1% |
| rope | 11 | 11.6% |
| tangled_rope | 5 | 5.3% |
| piton | 1 | 1.1% |

**Confidence band:**

| Band | Count | % |
| :--- | ---: | ---: |
| borderline | 94 | 98.9% |
| moderate | 1 | 1.1% |

**Rival type distribution:**

| Rival Type | Count | % |
| :--- | ---: | ---: |
| snare | 70 | 73.7% |
| tangled_rope | 18 | 18.9% |
| rope | 7 | 7.4% |

**Boundary classification:**

| Boundary | Count | % |
| :--- | ---: | ---: |
| piton->snare | 70 | 73.7% |
| piton->tangled_rope | 18 | 18.9% |
| piton->rope | 7 | 7.4% |

**Continuous metrics:**

| Metric | N | Mean | Median | Std | Min | Max |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| Epsilon (base_extractiveness) | 95 | 0.6295 | 0.6800 | 0.2196 | 0.0300 | 0.9200 |
| Sigma (suppression) | 95 | 0.6761 | 0.7300 | 0.2184 | 0.0400 | 0.9600 |
| Tau (theater_ratio) | 95 | 0.8479 | 0.8500 | 0.0716 | 0.7200 | 0.9900 |
| Purity Score | 95 | 0.5012 | 0.5750 | 0.1438 | 0.2763 | 0.8875 |
| Confidence (P(claimed)) | 95 | 0.0230 | 0.0100 | 0.0768 | 0.0000 | 0.7300 |
| Confidence Margin | 95 | -0.8679 | -0.9400 | 0.1993 | -0.9992 | 0.5878 |


## 2. Gate Profile Census

Chi (power-scaled extractiveness) is not stored in enriched_pipeline.json. This trace uses base_extractiveness (epsilon) as a proxy. For the analytical context (default), d≈0 so f(d)≈1.0, making Chi ≈ epsilon. Minor deviations are possible from scope scaling σ(S).


### 2.1 Scaffold Gate Profiles

**Gate marginal rates:**

| Gate | True | False | P(True) |
| :--- | ---: | ---: | ---: |
| emerges_naturally | 0 | 21 | 0.0000 |
| requires_active_enforcement | 9 | 12 | 0.4286 |
| has_coordination_function | 21 | 0 | 1.0000 |
| has_asymmetric_extraction | 19 | 2 | 0.9048 |

**Profile distribution:**

| Profile | N | % | Mean Conf | % Border | Mean Eps | Mean Sig | Mean Tau |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| EN=F, RAE=F, HCF=T, HAE=T | 10 | 47.6% | 0.0302 | 100.0% | 0.2660 | 0.5690 | 0.1890 |
| EN=F, RAE=T, HCF=T, HAE=T | 9 | 42.9% | 0.0089 | 100.0% | 0.2600 | 0.3611 | 0.1778 |
| EN=F, RAE=F, HCF=T, HAE=F | 2 | 9.5% | 0.0100 | 100.0% | 0.1250 | 0.1250 | 0.3250 |

**Cross-reference with false_ci_rope profiles:**

- Dominant (F,T,T,T): 9
- Atypical (F,F,T,T): 10
- Other profiles: 2

**RAE=F rate**: 12/21 (57.1%) vs false_ci_rope base rate 6.8% (**8.47x** enrichment)


### 2.2 Piton Gate Profiles

**Gate marginal rates:**

| Gate | True | False | P(True) |
| :--- | ---: | ---: | ---: |
| emerges_naturally | 0 | 95 | 0.0000 |
| requires_active_enforcement | 71 | 24 | 0.7474 |
| has_coordination_function | 95 | 0 | 1.0000 |
| has_asymmetric_extraction | 95 | 0 | 1.0000 |

**Profile distribution:**

| Profile | N | % | Mean Conf | % Border | Mean Eps | Mean Sig | Mean Tau |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| EN=F, RAE=T, HCF=T, HAE=T | 71 | 74.7% | 0.0144 | 100.0% | 0.6892 | 0.7492 | 0.8556 |
| EN=F, RAE=F, HCF=T, HAE=T | 24 | 25.3% | 0.0485 | 95.8% | 0.4529 | 0.4600 | 0.8250 |

**Cross-reference with false_ci_rope profiles:**

- Dominant (F,T,T,T): 71
- Atypical (F,F,T,T): 24
- Other profiles: 0

**RAE=F rate**: 24/95 (25.3%) vs false_ci_rope base rate 6.8% (**3.74x** enrichment)


## 3. Gate Path Trace

Each constraint is traced through the `classify_from_metrics/6` priority chain (drl_core.pl:288-358). The trace uses `base_extractiveness` as a proxy for Chi (power-scaled extractiveness) — see note above.


### 3.1 Scaffold Gate Trace (N=21)

**Prolog gate trace result:**

| Gate Result | Count | % |
| :--- | ---: | ---: |
| unknown | 10 | 47.6% |
| scaffold | 9 | 42.9% |
| tangled_rope | 2 | 9.5% |

**Claimed vs Prolog**: 9 match, 12 mismatch (57.1% intercepted by higher-priority gate)

**Interception by gate:**

| Intercepting Gate | Count | % |
| :--- | ---: | ---: |
| unknown | 10 | 47.6% |
| tangled_rope | 2 | 9.5% |


#### 3.1.1 Scaffold Temporality Path

The scaffold gate requires `scaffold_temporality_check` (drl_core.pl:273-276), which passes if `has_sunset_clause=T` OR `requires_active_enforcement=F`.

- **RAE=F** (temporality auto-passes): 12
- **RAE=T** (depends on has_sunset_clause): 9

RAE=T scaffold IDs (sunset-clause-dependent):

- `alternative_sovereignty_scaffold`
- `artificial_scarcity_scaffold`
- `asean_ceasefire_2011`
- `erasmus_rejoining_scaffold`
- `finnish_ubi_experiment`
- `isa_education_scaffold`
- `portugal_ad_stability_2026`
- `rn_proteus_adoption`
- `swift_piton_snap`


### 3.2 Piton Gate Trace (N=95)

**Prolog gate trace result:**

| Gate Result | Count | % |
| :--- | ---: | ---: |
| snare | 47 | 49.5% |
| tangled_rope | 30 | 31.6% |
| unknown | 10 | 10.5% |
| piton | 8 | 8.4% |

**Claimed vs Prolog**: 8 match, 87 mismatch (91.6% intercepted by higher-priority gate)

**Interception by gate:**

| Intercepting Gate | Count | % |
| :--- | ---: | ---: |
| snare | 47 | 49.5% |
| tangled_rope | 30 | 31.6% |
| unknown | 10 | 10.5% |


#### 3.2.1 Piton Theater Ratio Gate

The piton gate requires TR ≥ 0.70 (drl_core.pl:346). This is the threshold that distinguishes theatrical/dead code (piton) from legitimate low-extraction coordination (rope/scaffold).

- **TR ≥ 0.70** (passes piton gate): 95 (100.0%)
- **TR < 0.70** (fails piton gate): 0 (0.0%)


## 4. Rival Type Analysis


### 4.1 Scaffold Rival Analysis

**Scaffold→rope** (N=10, 47.6%):

Gate profiles: EN=F, RAE=F, HCF=T, HAE=T: 7, EN=F, RAE=T, HCF=T, HAE=T: 2, EN=F, RAE=F, HCF=T, HAE=F: 1

Metrics — eps: 0.1270, sigma: 0.4250, tau: 0.0970, conf: 0.0332

**Scaffold→tangled_rope** (N=9, 42.9%):

Gate profiles: EN=F, RAE=T, HCF=T, HAE=T: 7, EN=F, RAE=F, HCF=T, HAE=F: 1, EN=F, RAE=F, HCF=T, HAE=T: 1

Metrics — eps: 0.3133, sigma: 0.3778, tau: 0.2889, conf: 0.0078

**Scaffold→snare** (N=2, 9.5%):

Gate profiles: EN=F, RAE=F, HCF=T, HAE=T: 2

Metrics — eps: 0.5800, sigma: 0.7700, tau: 0.2850, conf: 0.0000


### 4.2 Piton Rival Analysis

**Piton→snare** (N=70, 73.7%):

Gate profiles: EN=F, RAE=T, HCF=T, HAE=T: 60, EN=F, RAE=F, HCF=T, HAE=T: 10

Metrics — eps: 0.7176, sigma: 0.7431, tau: 0.8507, conf: 0.0237

**Piton→tangled_rope** (N=18, 18.9%):

Gate profiles: EN=F, RAE=T, HCF=T, HAE=T: 11, EN=F, RAE=F, HCF=T, HAE=T: 7

Metrics — eps: 0.4650, sigma: 0.5683, tau: 0.8433, conf: 0.0281

**Piton→rope** (N=7, 7.4%):

Gate profiles: EN=F, RAE=F, HCF=T, HAE=T: 7

Metrics — eps: 0.1714, sigma: 0.2829, tau: 0.8314, conf: 0.0031


## 5. Sunset Clause Inference

`has_sunset_clause` is evaluated only within the Prolog inference engine (narrative_ontology.pl:326, default fail). It is not exported to enriched_pipeline.json. However, we can make structural inferences:


### 5.1 Scaffold Sunset Clause Inference

**RAE=T scaffolds** (N=9): scaffold_temporality_check requires has_sunset_clause=T when RAE=T (drl_core.pl:273-276). If the Prolog classified these as scaffold, has_sunset_clause must be T.

IDs:

- `alternative_sovereignty_scaffold`
- `artificial_scarcity_scaffold`
- `asean_ceasefire_2011`
- `erasmus_rejoining_scaffold`
- `finnish_ubi_experiment`
- `isa_education_scaffold`
- `portugal_ad_stability_2026`
- `rn_proteus_adoption`
- `swift_piton_snap`

**RAE=F scaffolds** (N=12): scaffold_temporality_check auto-passes when RAE=F (drl_core.pl:276). Sunset clause status is unknown.


### 5.2 Piton RAE Analysis

**RAE=T pitons** (N=71): Pitons with RAE=T: requires active enforcement but claimed as dead/theatrical code. These are enforced pitons — the constraint persists through enforcement even though its function is theatrical.

**RAE=F pitons** (N=24): Pitons with RAE=F: no enforcement required. This is the 'institutional inertia' pattern — the constraint persists through neglect, not through active maintenance.


## 6. Hypothesis Assessment


### 6.1 H1: Piton borderline confidence traces to RAE=F ambiguity

**Verdict: SUPPORTED**

Piton RAE=F rate (25.3%) is 3.74x the false_ci_rope base rate (6.8%). RAE=F is significantly overrepresented in pitons.

- Piton RAE=F count: 24/95
- Piton RAE=F rate: 25.3%
- FCR base rate: 6.8%
- Enrichment ratio: 3.74x
- RAE=F piton rival types: snare: 10, rope: 7, tangled_rope: 7


### 6.2 H2: Scaffold borderline confidence traces to sunset clause unobservability

**Verdict: PARTIALLY SUPPORTED**

RAE=F scaffolds are also 100% borderline, meaning sunset clause unobservability is not the SOLE source of ambiguity. Continuous metric proximity to the tangled_rope/rope boundary also contributes.

- Scaffolds with RAE=T: 9
- Scaffolds with RAE=F: 12
- RAE=F scaffolds borderline: 12/12 (100.0%)
- Total scaffolds borderline: 21/21 (100.0%)


### 6.3 H3: The RAE=F subgroup is the piton-scaffold population

**Verdict: SUPPORTED**

26/61 (42.6%) of RAE=F false_ci_rope constraints are piton/scaffold, vs 10.6% in the full false_ci_rope population. Pitons and scaffolds are significantly concentrated in the RAE=F subgroup.

- FCR RAE=F total: 61
- FCR RAE=F piton/scaffold: 26 (42.6%)
- FCR piton/scaffold rate (full population): 10.6%

FCR RAE=F claimed type distribution:

| Claimed Type | Count |
| :--- | ---: |
| piton | 18 |
| snare | 11 |
| rope | 10 |
| mountain | 9 |
| scaffold | 8 |
| tangled_rope | 5 |


## 7. Recommendations


### 7.1. Expose has_sunset_clause in enriched_pipeline.json

**Impact**: high

9 scaffolds have RAE=T, meaning their scaffold classification depends entirely on has_sunset_clause — a gate that is invisible to the MaxEnt classifier. Exposing this gate would give the classifier a direct feature for distinguishing scaffold from tangled_rope in the RAE=T subpopulation.


### 7.2. Address piton gate interception by higher-priority gates

**Impact**: high

All 95/95 pitons pass the TR ≥ 0.70 gate, so theater ratio is not the bottleneck. The real issue is gate priority: 87/95 pitons (91.6%) are intercepted by higher-priority gates before the piton gate is reached. Interceptions: {'snare': 47, 'tangled_rope': 30, 'unknown': 10}. Most pitons have epsilon > 0.25 (the piton Chi ceiling), so the snare and tangled_rope gates fire first. The MaxEnt classifier should encode the piton gate's Chi ≤ 0.25 ceiling and TR ≥ 0.70 floor as joint boolean features. Without these, the classifier has no way to distinguish piton from snare/tangled_rope.


### 7.3. Consider splitting false_ci_rope subcategories by RAE gate

**Impact**: moderate

H3 verdict: SUPPORTED. 26/61 (42.6%) of RAE=F false_ci_rope constraints are piton/scaffold, vs 10.6% in the full false_ci_rope population. Pitons and scaffolds are significantly concentrated in the RAE=F subgroup.


### 7.4. Review gate-trace mismatches as reclassification candidates

**Impact**: high

99 constraints are claimed as scaffold/piton but would be classified differently by the Prolog gate priority chain. Piton interceptions: {'snare': 47, 'tangled_rope': 30, 'unknown': 10}. Scaffold interceptions: {'unknown': 10, 'tangled_rope': 2}. These are candidates for reclassification or for documenting why the LLM's claimed type diverges from the gate logic.
