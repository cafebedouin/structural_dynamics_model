# false_ci_rope Internal Structure Audit

*Generated 2026-02-23 19:12:22 by `python/false_ci_rope_audit.py`*

## Executive Summary

The `false_ci_rope` population contains **904** constraints (77.2% of 1171 total). This signature applies a conditional 3x boost to `tangled_rope` probability in the MaxEnt classifier, then renormalizes.

Key findings:

- **Dominant gate profile**: EN=F, RAE=T, HCF=T, HAE=T — 837/904 (92.6%)
- **Override-flipped**: 164 constraints (18.1%) had their argmax changed by the override
- **tangled_rope->snare boundary**: 643 constraints (71.1%)
- **6 candidate subcategories** identified


## 1. Population Census


### 1.1 Claimed Type Distribution

| Claimed Type | Count | % |
| :--- | ---: | ---: |
| tangled_rope | 704 | 77.9% |
| piton | 81 | 9.0% |
| snare | 61 | 6.7% |
| rope | 32 | 3.5% |
| scaffold | 15 | 1.7% |
| mountain | 9 | 1.0% |
| [social_governance] | 1 | 0.1% |
| None | 1 | 0.1% |


### 1.2 Boundary Distribution

| Boundary | Count | % |
| :--- | ---: | ---: |
| tangled_rope->snare | 643 | 71.1% |
| piton->snare | 68 | 7.5% |
| snare->tangled_rope | 60 | 6.6% |
| tangled_rope->scaffold | 42 | 4.6% |
| rope->tangled_rope | 22 | 2.4% |
| tangled_rope->rope | 14 | 1.5% |
| mountain->rope | 9 | 1.0% |
| scaffold->tangled_rope | 8 | 0.9% |
| piton->tangled_rope | 7 | 0.8% |
| piton->rope | 6 | 0.7% |
| rope->mountain | 5 | 0.6% |
| scaffold->rope | 5 | 0.6% |
| tangled_rope->piton | 4 | 0.4% |
| rope->scaffold | 4 | 0.4% |
| scaffold->snare | 2 | 0.2% |
| rope->snare | 1 | 0.1% |
| [social_governance]->tangled_rope | 1 | 0.1% |
| None | 1 | 0.1% |
| tangled_rope->mountain | 1 | 0.1% |
| snare->piton | 1 | 0.1% |


### 1.3 Confidence Band Distribution

| Band | Count | % |
| :--- | ---: | ---: |
| borderline | 458 | 50.7% |
| deep | 260 | 28.8% |
| moderate | 185 | 20.5% |
| None | 1 | 0.1% |


### 1.4 Tangled Band Distribution

| Tangled Band | Count | % |
| :--- | ---: | ---: |
| snare_leaning | 608 | 67.3% |
| None | 200 | 22.1% |
| rope_leaning | 70 | 7.7% |
| genuinely_tangled | 26 | 2.9% |


### 1.5 Continuous Metrics Summary

| Metric | N | Mean | Median | Std | Min | Max |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| Epsilon (base_extractiveness) | 904 | 0.6065 | 0.5800 | 0.1875 | 0.1000 | 1.0000 |
| Sigma (suppression) | 904 | 0.6646 | 0.7000 | 0.1859 | 0 | 0.9500 |
| Tau (theater_ratio) | 904 | 0.3470 | 0.2000 | 0.2839 | 0.0000 | 0.9800 |
| Purity Score | 884 | 0.4807 | 0.4743 | 0.1459 | 0.2708 | 0.9720 |
| Confidence (P(claimed)) | 903 | 0.4783 | 0.4983 | 0.3672 | 0.0000 | 1.0000 |
| Confidence Margin | 903 | -0.0309 | -0.0035 | 0.7221 | -0.9998 | 1.0000 |
| Confidence Entropy | 903 | 0.2163 | 0.2330 | 0.1383 | 0.0000 | 0.5057 |
| Tangled PSI | 704 | 0.8829 | 0.9979 | 0.2831 | 0.0000 | 0.9990 |


## 2. Binary Gate Profile Analysis

has_sunset_clause is evaluated only within the Prolog inference engine (narrative_ontology.pl:326) and is not exported to enriched_pipeline.json. This audit uses 4 of the 5 Tier 1 gates.


### 2.1 Gate Marginal Rates

| Gate | True | False | P(True) |
| :--- | ---: | ---: | ---: |
| emerges_naturally | 17 | 887 | 0.0188 |
| requires_active_enforcement | 843 | 61 | 0.9325 |
| has_coordination_function | 902 | 2 | 0.9978 |
| has_asymmetric_extraction | 899 | 5 | 0.9945 |
Derived: `is_constructed` (NOT emerges_naturally) = 887/904. `natural_law_without_beneficiary` = 0/904.


### 2.2 Profile Distribution

| Profile | Count | % | Top Rival |
| :--- | ---: | ---: | :--- |
| EN=F, RAE=T, HCF=T, HAE=T | 837 | 92.6% | snare |
| EN=F, RAE=F, HCF=T, HAE=T | 46 | 5.1% | snare |
| EN=T, RAE=F, HCF=T, HAE=T | 14 | 1.5% | rope |
| EN=F, RAE=T, HCF=F, HAE=F | 2 | 0.2% | snare |
| EN=F, RAE=T, HCF=T, HAE=F | 2 | 0.2% | scaffold |
| EN=T, RAE=T, HCF=T, HAE=T | 2 | 0.2% | rope |
| EN=T, RAE=F, HCF=T, HAE=F | 1 | 0.1% | rope |


### 2.3 Per-Profile Metric Summary

| Profile | N | Top Claimed | Mean Conf | % Border | % Deep | Mean Eps | Mean Sig | Mean Tau |
| :--- | ---: | :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| EN=F, RAE=T, HCF=T, HAE=T | 837 | tangled_rope | 0.4884 | 49.6% | 28.7% | 0.6221 | 0.6849 | 0.3472 |
| EN=F, RAE=F, HCF=T, HAE=T | 46 | piton | 0.2311 | 78.3% | 17.4% | 0.5098 | 0.5387 | 0.4657 |
| EN=T, RAE=F, HCF=T, HAE=T | 14 | mountain | 0.6123 | 35.7% | 57.1% | 0.1643 | 0.0536 | 0.0779 |
| EN=F, RAE=T, HCF=F, HAE=F | 2 | rope | 0.0002 | 50.0% | 0.0% | 0.4750 | 0.4000 | 0.1300 |
| EN=F, RAE=T, HCF=T, HAE=F | 2 | rope | 0.9607 | 0.0% | 100.0% | 0.1250 | 0.2250 | 0.0900 |
| EN=T, RAE=T, HCF=T, HAE=T | 2 | tangled_rope | 0.5343 | 50.0% | 50.0% | 0.2500 | 0.3250 | 0.0350 |
| EN=T, RAE=F, HCF=T, HAE=F | 1 | mountain | 0.9789 | 0.0% | 100.0% | 0.1000 | 0.0500 | 0.1000 |


### 2.4 Claimed Type Breakdown by Profile

**EN=F, RAE=T, HCF=T, HAE=T** (N=837):

| Claimed Type | Count |
| :--- | ---: |
| tangled_rope | 697 |
| piton | 63 |
| snare | 50 |
| rope | 19 |
| scaffold | 7 |
| [social_governance] | 1 |

**EN=F, RAE=F, HCF=T, HAE=T** (N=46):

| Claimed Type | Count |
| :--- | ---: |
| piton | 18 |
| snare | 11 |
| scaffold | 8 |
| tangled_rope | 5 |
| rope | 4 |

**EN=T, RAE=F, HCF=T, HAE=T** (N=14):

| Claimed Type | Count |
| :--- | ---: |
| mountain | 8 |
| rope | 6 |

**EN=F, RAE=T, HCF=F, HAE=F** (N=2):

| Claimed Type | Count |
| :--- | ---: |
| rope | 1 |
| None | 1 |

**EN=F, RAE=T, HCF=T, HAE=F** (N=2):

| Claimed Type | Count |
| :--- | ---: |
| rope | 2 |


## 3. Override-Flipped vs Non-Flipped

**Flipped**: 164 constraints where the 3x tangled_rope boost changed the argmax type.
**Non-flipped**: 740 constraints where the override did not change the top type.
**No data**: 0 constraints lacking raw_maxent_probs.


### 3.1 Flip Transitions

| Transition | Count |
| :--- | ---: |
| snare->tangled_rope | 156 |
| rope->tangled_rope | 7 |
| scaffold->tangled_rope | 1 |


### 3.2 Metric Comparison

| Metric | Flip Mean | Flip Med | Flip Std | NoFlip Mean | NoFlip Med | NoFlip Std |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| base_extractiveness | 0.5368 | 0.5500 | 0.1019 | 0.6219 | 0.6500 | 0.1983 |
| suppression | 0.7059 | 0.7000 | 0.1202 | 0.6554 | 0.7000 | 0.1963 |
| theater_ratio | 0.2299 | 0.2000 | 0.1371 | 0.3729 | 0.2000 | 0.3009 |
| purity_score | 0.3989 | 0.3525 | 0.1221 | 0.4992 | 0.5263 | 0.1445 |
| confidence | 0.5822 | 0.5802 | 0.1290 | 0.4553 | 0.3130 | 0.3977 |
| confidence_margin | 0.1757 | 0.1604 | 0.2157 | -0.0767 | -0.3740 | 0.7844 |
| tangled_psi | 0.9777 | 0.9975 | 0.1387 | 0.8568 | 0.9983 | 0.3062 |


### 3.3 Confidence Band Comparison

| Band | Flip N | Flip % | NoFlip N | NoFlip % |
| :--- | ---: | ---: | ---: | ---: |
| None | 0 | 0.0% | 1 | 0.1% |
| borderline | 12 | 7.3% | 446 | 60.3% |
| deep | 0 | 0.0% | 260 | 35.1% |
| moderate | 152 | 92.7% | 33 | 4.5% |


### 3.4 Gate Profile Distribution

| Profile | Flip N | Flip % | NoFlip N | NoFlip % |
| :--- | ---: | ---: | ---: | ---: |
| EN=F, RAE=T, HCF=T, HAE=T | 161 | 98.2% | 676 | 91.4% |
| EN=F, RAE=F, HCF=T, HAE=T | 3 | 1.8% | 43 | 5.8% |
| EN=T, RAE=F, HCF=T, HAE=T | 0 | 0.0% | 14 | 1.9% |
| EN=F, RAE=T, HCF=T, HAE=F | 0 | 0.0% | 2 | 0.3% |
| EN=T, RAE=T, HCF=T, HAE=T | 0 | 0.0% | 2 | 0.3% |
| EN=F, RAE=T, HCF=F, HAE=F | 0 | 0.0% | 2 | 0.3% |
| EN=T, RAE=F, HCF=T, HAE=F | 0 | 0.0% | 1 | 0.1% |


### 3.5 Rival Type Distribution

| Rival Type | Flip N | Flip % | NoFlip N | NoFlip % |
| :--- | ---: | ---: | ---: | ---: |
| snare | 149 | 90.9% | 565 | 76.4% |
| tangled_rope | 12 | 7.3% | 86 | 11.6% |
| scaffold | 0 | 0.0% | 46 | 6.2% |
| rope | 3 | 1.8% | 31 | 4.2% |
| mountain | 0 | 0.0% | 6 | 0.8% |
| piton | 0 | 0.0% | 5 | 0.7% |
| None | 0 | 0.0% | 1 | 0.1% |


### 3.6 Interpretation

Flipped constraints had a raw MaxEnt top type that was *not* tangled_rope, but the 3x conditional boost on tangled_rope was sufficient to change the argmax. Non-flipped constraints already had tangled_rope as raw top type, so the override only increased its margin.

The key diagnostic question: do the flipped constraints share a distinctive binary gate profile or continuous metric signature? If yes, that profile is a candidate for a new signature subcategory. If the gate profile distributions are similar between flipped and non-flipped, then the override is compensating for something the gates cannot see — likely fine-grained continuous metric positioning near the tangled_rope/snare decision boundary.


## 4. Clustering Within tangled_rope->snare Boundary

This section focuses on the 643 false_ci_rope constraints on the tangled_rope->snare boundary specifically.


### 4.1 By Binary Gate Profile

| Profile | N | % | Conf Mean | Conf Std | % Border | PSI Mean | PSI Std | Flipped |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| EN=F, RAE=T, HCF=T, HAE=T | 638 | 99.2% | 0.4905 | 0.3313 | 49.7% | 0.9571 | 0.1441 | 149 |
| EN=F, RAE=F, HCF=T, HAE=T | 5 | 0.8% | 0.0265 | 0.0041 | 100.0% | 0.9990 | 0.0000 | 0 |


### 4.2 By Tangled Band

| Tangled Band | N | % | Mean Conf | Mean Eps | Mean PSI | Flipped | Flip % |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| snare_leaning | 606 | 94.2% | 0.4557 | 0.6454 | 0.9909 | 149 | 24.6% |
| genuinely_tangled | 25 | 3.9% | 0.9974 | 0.5032 | 0.5065 | 0 | 0.0% |
| rope_leaning | 12 | 1.9% | 0.9997 | 0.5725 | 0.2062 | 0 | 0.0% |


### 4.3 PSI Distribution (Histogram)

PSI available for 643 of 643 boundary constraints (0 missing). Mean=0.9574, Median=0.9980, Std=0.1436.

| PSI Range | Count | Distribution |
| :--- | ---: | :--- |
| [0.00, 0.10) | 1 | # |
| [0.10, 0.20) | 4 | # |
| [0.20, 0.30) | 7 | # |
| [0.30, 0.50) | 9 | # |
| [0.50, 0.70) | 16 | ## |
| [0.70, 0.80) | 5 | # |
| [0.80, 0.90) | 6 | # |
| [0.90, 0.95) | 25 | ### |
| [0.95, 1.00) | 570 | ######################################################################################## |

PSI near 0.5 means the classifier genuinely cannot resolve rope vs snare. PSI near 1.0 means the classifier thinks it's really a snare. PSI near 0.0 means it looks like a rope.


## 5. Candidate Subcategories

Based on the above analysis, the following candidate groupings were identified. For each, we assess whether the group is more internally homogeneous (lower confidence std) than the full false_ci_rope population.


### 5.1 near_snare_borderline

Snare-leaning (PSI > 0.7) with borderline confidence (P(claimed) < 0.5). The 3x override holds them as tangled_rope but the evidence strongly favors snare. These are the constraints most likely to be misclassified.

- **Count**: 322 (35.6% of FCR)
- **More homogeneous than population**: Yes (std=0.1457)
- **Override flipped**: 0 (0.0%)
- **Confidence bands**: borderline=322

**Exemplars:**

| ID | Claimed | MaxEnt Top | Conf | Band | PSI | Gates |
| :--- | :--- | :--- | ---: | :--- | ---: | :--- |
| 26usc469_real_estate_exemption | tangled_rope | snare | 0.1847 | borderline | 0.9988 | EN=F, RAE=T, HCF=T, HAE=T |
| abstraction_boundary_overrun | tangled_rope | snare | 0.0257 | borderline | 0.9990 | EN=F, RAE=T, HCF=T, HAE=T |
| academic_peer_review_gatekeeping_u2_sed_r1 | tangled_rope | snare | 0.0633 | borderline | 0.9989 | EN=F, RAE=T, HCF=T, HAE=T |
| academic_peer_review_gatekeeping_u2_sed_r2 | tangled_rope | snare | 0.0633 | borderline | 0.9989 | EN=F, RAE=T, HCF=T, HAE=T |
| academic_peer_review_gatekeeping_u2_sed_r3 | tangled_rope | snare | 0.0570 | borderline | 0.9989 | EN=F, RAE=T, HCF=T, HAE=T |


### 5.2 override_flipped

Constraints where the 3x tangled_rope boost changed the argmax. Without the override, a different type would be the classifier's top pick. The override is doing real classificatory work here.

- **Count**: 164 (18.1% of FCR)
- **More homogeneous than population**: Yes (std=0.1290)
- **Override flipped**: 164 (100.0%)
- **Confidence bands**: moderate=152, borderline=12

**Exemplars:**

| ID | Claimed | MaxEnt Top | Conf | Band | PSI | Gates |
| :--- | :--- | :--- | ---: | :--- | ---: | :--- |
| abstraction_leakage | tangled_rope | tangled_rope | 0.6137 | moderate | 0.9974 | EN=F, RAE=T, HCF=T, HAE=T |
| ai_adoption_stigma | tangled_rope | tangled_rope | 0.6056 | moderate | 0.9975 | EN=F, RAE=T, HCF=T, HAE=T |
| arctic_maritime_control | tangled_rope | tangled_rope | 0.6156 | moderate | 0.9974 | EN=F, RAE=T, HCF=T, HAE=T |
| arg_ev_tariff | tangled_rope | tangled_rope | 0.5118 | moderate | 0.9980 | EN=F, RAE=T, HCF=T, HAE=T |
| artificial_scarcity_scaffold | scaffold | tangled_rope | 0.0000 | borderline | --- | EN=F, RAE=T, HCF=T, HAE=T |


### 5.3 genuinely_tangled

PSI in [0.3, 0.7] — the classifier genuinely cannot resolve whether this is more rope or more snare. These occupy the true middle ground of the tangled_rope concept.

- **Count**: 26 (2.9% of FCR)
- **More homogeneous than population**: Yes (std=0.0057)
- **Override flipped**: 0 (0.0%)
- **Confidence bands**: deep=26

**Exemplars:**

| ID | Claimed | MaxEnt Top | Conf | Band | PSI | Gates |
| :--- | :--- | :--- | ---: | :--- | ---: | :--- |
| aging_longevity_tests | tangled_rope | tangled_rope | 0.9987 | deep | 0.5662 | EN=F, RAE=T, HCF=T, HAE=T |
| attribution_ambiguity_triplet_sc | tangled_rope | tangled_rope | 0.9987 | deep | 0.5528 | EN=F, RAE=T, HCF=T, HAE=T |
| brazil_2026_general_elections | tangled_rope | tangled_rope | 0.9947 | deep | 0.6420 | EN=F, RAE=T, HCF=T, HAE=T |
| buffons_needle_pi_estimation | tangled_rope | tangled_rope | 0.9992 | deep | 0.3573 | EN=F, RAE=T, HCF=T, HAE=T |
| cost_of_observation | tangled_rope | tangled_rope | 0.9991 | deep | 0.4832 | EN=F, RAE=T, HCF=T, HAE=T |


### 5.4 atypical_gate_profile

Constraints not matching the dominant gate profile (EN=F, RAE=T, HCF=T, HAE=T). These have different structural properties and may represent edge cases or distinct phenomena.

- **Count**: 67 (7.4% of FCR)
- **More homogeneous than population**: No (std=0.4325)
- **Override flipped**: 3 (4.5%)
- **Confidence bands**: deep=20, borderline=43, moderate=3, None=1
- **Gate profiles**: EN=F, RAE=F, HCF=T, HAE=T: 46, EN=T, RAE=F, HCF=T, HAE=T: 14, EN=F, RAE=T, HCF=F, HAE=F: 2, EN=F, RAE=T, HCF=T, HAE=F: 2, EN=T, RAE=T, HCF=T, HAE=T: 2, EN=T, RAE=F, HCF=T, HAE=F: 1

**Exemplars:**

| ID | Claimed | MaxEnt Top | Conf | Band | PSI | Gates |
| :--- | :--- | :--- | ---: | :--- | ---: | :--- |
| absorbing_markov_chain_trap | snare | snare | 0.9390 | deep | --- | EN=F, RAE=F, HCF=T, HAE=T |
| academic_fashion_modernism_2026 | piton | snare | 0.0094 | borderline | --- | EN=F, RAE=F, HCF=T, HAE=T |
| agent_opt_2026 | piton | snare | 0.0051 | borderline | --- | EN=F, RAE=F, HCF=T, HAE=T |
| ai_scholar_citation_trap | tangled_rope | snare | 0.0285 | borderline | 0.9990 | EN=F, RAE=F, HCF=T, HAE=T |
| ai_superpowers_2026 | scaffold | snare | 0.0000 | borderline | --- | EN=F, RAE=F, HCF=T, HAE=T |


### 5.5 rope_leaning_outliers

PSI < 0.3 (rope-leaning) despite carrying the false_ci_rope signature. The classifier sees more rope than snare in these constraints, making the tangled_rope classification unusual.

- **Count**: 70 (7.7% of FCR)
- **More homogeneous than population**: Yes (std=0.1589)
- **Override flipped**: 3 (4.3%)
- **Confidence bands**: deep=65, moderate=3, borderline=2

**Exemplars:**

| ID | Claimed | MaxEnt Top | Conf | Band | PSI | Gates |
| :--- | :--- | :--- | ---: | :--- | ---: | :--- |
| advice_as_dangerous_gift | tangled_rope | tangled_rope | 0.9963 | deep | 0.0063 | EN=F, RAE=T, HCF=T, HAE=T |
| alzheimers_levetiracetam | tangled_rope | tangled_rope | 0.9967 | deep | 0.0473 | EN=F, RAE=T, HCF=T, HAE=T |
| arrows_impossibility_theorem | tangled_rope | tangled_rope | 0.9997 | deep | 0.2236 | EN=F, RAE=T, HCF=T, HAE=T |
| axiom_reasoner_2026 | tangled_rope | tangled_rope | 0.9996 | deep | 0.0070 | EN=F, RAE=T, HCF=T, HAE=T |
| bgs_eigenvector_thermalization | tangled_rope | tangled_rope | 0.9995 | deep | 0.2681 | EN=F, RAE=T, HCF=T, HAE=T |


### 5.6 non_tangled_rope_claimed

Constraints with false_ci_rope signature but claimed_type is not tangled_rope. The signature override targets tangled_rope probability specifically, so the interaction with other claimed types is indirect.

- **Count**: 200 (22.1% of FCR)
- **More homogeneous than population**: No (std=0.3871)
- **Override flipped**: 12 (6.0%)
- **Confidence bands**: deep=45, borderline=134, moderate=20, None=1
- **Claimed types**: piton: 81, snare: 61, rope: 32, scaffold: 15, mountain: 9, [social_governance]: 1, None: 1

**Exemplars:**

| ID | Claimed | MaxEnt Top | Conf | Band | PSI | Gates |
| :--- | :--- | :--- | ---: | :--- | ---: | :--- |
| absorbing_markov_chain_trap | snare | snare | 0.9390 | deep | --- | EN=F, RAE=F, HCF=T, HAE=T |
| academic_fashion_modernism_2026 | piton | snare | 0.0094 | borderline | --- | EN=F, RAE=F, HCF=T, HAE=T |
| adversarial_truth_decay | piton | snare | 0.0072 | borderline | --- | EN=F, RAE=T, HCF=T, HAE=T |
| adverse_possession | snare | tangled_rope | 0.0120 | borderline | --- | EN=F, RAE=T, HCF=T, HAE=T |
| agent_opt_2026 | piton | snare | 0.0051 | borderline | --- | EN=F, RAE=F, HCF=T, HAE=T |


## 6. Conclusions

This section summarizes what the data shows about whether `false_ci_rope` is one thing or several things wearing the same label.

**Gate profile homogeneity**: The dominant profile (EN=F, RAE=T, HCF=T, HAE=T) accounts for 92.6% of the population. The binary gates do not subdivide false_ci_rope into many structurally distinct groups — the vast majority share the same gate fingerprint.

**Override impact**: 18.1% of constraints had their top type flipped by the override. This is a substantial minority where the override is doing real classificatory work rather than merely reinforcing an existing lean.

**Boundary population structure**: Of the 643 constraints on the tangled_rope->snare boundary, 94.2% are snare-leaning by PSI. The tangled_rope->snare boundary is overwhelmingly populated by constraints that the classifier considers more snare than rope.

**Assessment**: The binary gate profiles provide limited subdivision of the false_ci_rope population because the dominant profile is so prevalent. The more meaningful axis of variation is the *continuous* one: where a constraint falls on the PSI spectrum (rope-leaning vs genuinely tangled vs snare-leaning) and whether the override flips the argmax. These continuous dimensions, not the binary gates, are what differentiate the population's internal structure.
