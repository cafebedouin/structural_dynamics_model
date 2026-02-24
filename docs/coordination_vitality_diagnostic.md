# Coordination Vitality Diagnostic

*Generated 2026-02-23 20:46:28 by `python/coordination_vitality_diagnostic.py`*

## Executive Summary

This diagnostic examines whether the coordination function in **95 pitons** and **21 scaffolds** is genuinely active or merely residual. The binary gate `has_coordination_function` fires T for all 116 because it only checks `constraint_beneficiary(C, _)` existence. This diagnostic distinguishes active coordination (beneficiaries benefit from the constraint's function) from residual coordination (beneficiaries benefit from the constraint's persistence).

Key findings:

- 90.5% of pitons show dead/degrading coordination (terminal+degrading). The ontology may need a wider piton definition or an 'extractive piton' subtype.
- 81.0% of scaffolds appear genuinely temporary. The scaffold extraction ceiling may be too low for legitimate transition costs.
- Proxy 2 (TR×beneficiary-type): 60 constraints show 'theatrical persistence' (high-TR + institutional beneficiary) vs 35 showing 'theatrical function'. The theatrical persistence pattern dominates.

Recommendations:

- Consider an 'extractive piton' subtype for high-epsilon pitons with dead coordination — these differ from classic low-extraction pitons.
- Consider adding 'coordination vitality' as a formal axis in the taxonomy. The binary has_coordination_function gate misses the functional/persistence distinction that this diagnostic reveals.


## Methodological Note

Beneficiary/victim atoms are structural role labels (e.g., `executive_state_body`, `medical_applicants`), not descriptive text. Classification uses keyword matching on underscore-tokenized atoms against four actor-type categories: `institutional_actor`, `individual_actor`, `collective_actor`, `abstract_entity`. Unmatched atoms are labeled `ambiguous`. This is a heuristic — it classifies WHO benefits, not HOW they benefit. The cross-reference with metric profiles (Step 3) provides the complementary signal.


## 1. Beneficiary Actor-Type Analysis


### 1.1 Population Distribution

Total beneficiary atoms classified: **125** across 116 constraints.

| Actor Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 70 | 56.0% |
| individual_actor | 9 | 7.2% |
| collective_actor | 14 | 11.2% |
| abstract_entity | 10 | 8.0% |
| ambiguous | 22 | 17.6% |

Prolog .pl file cross-reference coverage: 116/116 (100.0%).


### 1.2 By Claimed Type (atom-level)

**piton** (N=97 atoms):

| Actor Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 60 | 61.9% |
| individual_actor | 2 | 2.1% |
| collective_actor | 9 | 9.3% |
| abstract_entity | 8 | 8.2% |
| ambiguous | 18 | 18.6% |

**scaffold** (N=28 atoms):

| Actor Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 10 | 35.7% |
| individual_actor | 7 | 25.0% |
| collective_actor | 5 | 17.9% |
| abstract_entity | 2 | 7.1% |
| ambiguous | 4 | 14.3% |


### 1.3 Dominant Beneficiary Type by Claimed Type (constraint-level)

**piton** (N=95 constraints):

| Dominant Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 60 | 63.2% |
| individual_actor | 2 | 2.1% |
| collective_actor | 8 | 8.4% |
| abstract_entity | 8 | 8.4% |
| ambiguous | 17 | 17.9% |

**scaffold** (N=21 constraints):

| Dominant Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 7 | 33.3% |
| individual_actor | 5 | 23.8% |
| collective_actor | 5 | 23.8% |
| abstract_entity | 1 | 4.8% |
| ambiguous | 3 | 14.3% |


### 1.4 Intercepted vs Passing

**Intercepted** (N=101):

| Dominant Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 64 | 63.4% |
| individual_actor | 2 | 2.0% |
| collective_actor | 9 | 8.9% |
| abstract_entity | 8 | 7.9% |
| ambiguous | 18 | 17.8% |

**Passing own gate** (N=15):

| Dominant Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 3 | 20.0% |
| individual_actor | 5 | 33.3% |
| collective_actor | 4 | 26.7% |
| abstract_entity | 1 | 6.7% |
| ambiguous | 2 | 13.3% |


## 2. Victim Actor-Type Analysis


### 2.1 Population Distribution

Total victim atoms classified: **116** across 116 constraints.

| Actor Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 9 | 7.8% |
| individual_actor | 64 | 55.2% |
| collective_actor | 13 | 11.2% |
| abstract_entity | 13 | 11.2% |
| ambiguous | 17 | 14.7% |


### 2.2 By Claimed Type (atom-level)

**piton** (N=97 atoms):

| Actor Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 5 | 5.2% |
| individual_actor | 57 | 58.8% |
| collective_actor | 11 | 11.3% |
| abstract_entity | 10 | 10.3% |
| ambiguous | 14 | 14.4% |

**scaffold** (N=19 atoms):

| Actor Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 4 | 21.1% |
| individual_actor | 7 | 36.8% |
| collective_actor | 2 | 10.5% |
| abstract_entity | 3 | 15.8% |
| ambiguous | 3 | 15.8% |


### 2.3 Dominant Victim Type by Claimed Type

**piton** (N=95 constraints):

| Dominant Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 5 | 5.3% |
| individual_actor | 56 | 58.9% |
| collective_actor | 11 | 11.6% |
| abstract_entity | 9 | 9.5% |
| ambiguous | 14 | 14.7% |

**scaffold** (N=21 constraints):

| Dominant Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 4 | 19.0% |
| individual_actor | 7 | 33.3% |
| collective_actor | 2 | 9.5% |
| abstract_entity | 3 | 14.3% |
| ambiguous | 5 | 23.8% |


## 3. Theater-Extraction-Coordination Cross-Reference


### 3.1 Proxy 1: Beneficiary-Victim Asymmetry Score

Asymmetry = (institutional beneficiary fraction) − (institutional victim fraction). Positive = persistence pattern (institutional beneficiaries, individual victims). Negative = functional pattern (individual beneficiaries, institutional victims).

Overall: **64** persistence pattern (>0), **4** functional pattern (<0), **48** neutral (=0) out of 116.

| Type | N | Mean | Median | Min | Max |
| :--- | ---: | ---: | ---: | ---: | ---: |
| piton | 95 | 0.5737 | 1.0000 | -1.0000 | 1.0000 |
| scaffold | 21 | 0.1746 | 0.0000 | -1.0000 | 1.0000 |


### 3.2 Proxy 2: Theater Ratio × Beneficiary Type

Partitions constraints by theater ratio threshold (TR ≥ 0.70) and dominant beneficiary actor type (institutional vs non-institutional).

| Cell | N | % | Mean ε | Median ε |
| :--- | ---: | ---: | ---: | ---: |
| Theatrical Persistence | 60 | 51.7% | 0.6628 | 0.7350 |
| Theatrical Function | 35 | 30.2% | 0.5723 | 0.5500 |
| Active Persistence | 7 | 6.0% | 0.3686 | 0.3500 |
| Active Function | 14 | 12.1% | 0.1907 | 0.1650 |

- **Theatrical Persistence**: High-TR + institutional → dead coordination, theater persists
- **Theatrical Function**: High-TR + non-institutional → active coordination masked by theater
- **Active Persistence**: Low-TR + institutional → active institutional extraction
- **Active Function**: Low-TR + non-institutional → genuine functional coordination


### 3.3 Proxy 3: RAE × Beneficiary Type

Partitions by requires_active_enforcement and dominant beneficiary type.

| Cell | N | % |
| :--- | ---: | ---: |
| Enforced Persistence | 50 | 43.1% |
| Enforced Function | 30 | 25.9% |
| Emergent Persistence | 17 | 14.7% |
| Emergent Function | 19 | 16.4% |

- **Enforced Persistence**: RAE=T + institutional → enforced theater (extractive piton)
- **Enforced Function**: RAE=T + non-institutional → enforced coordination (tangled_rope)
- **Emergent Persistence**: RAE=F + institutional → emergent institutional inertia
- **Emergent Function**: RAE=F + non-institutional → emergent functional coordination


## 4. Scaffold Vitality Assessment

N=21 scaffolds. Sunset clause detected in .pl files: 21. Coordination type coverage: 13/21. Intercepted by higher-priority gate: 6.

| Constraint | Ben. Type | Sunset | Coord Type | ε | Risk | Signals |
| :--- | :--- | :---: | :--- | ---: | :---: | :--- |
| `ai_superpowers_2026` | institutiona | Y | --- | 0.6400 | medium | institutional_beneficiary, epsilon_exceeds_ceiling |
| `alternative_sovereignty_scaffold` | individual_a | Y | global_infrastructure | 0.2200 | low | --- |
| `artificial_scarcity_scaffold` | ambiguous | Y | resource_allocation | 0.4800 | low | epsilon_exceeds_ceiling |
| `asean_ceasefire_2011` | collective_a | Y | enforcement_mechanism | 0.1500 | low | enforcement_mechanism_coord |
| `boundary_protocol` | ambiguous | Y | information_standard | 0.0000 | low | --- |
| `canada_goose_realignment_2026` | collective_a | Y | resource_allocation | 0.2500 | low | --- |
| `cinderella_midnight_deadline` | individual_a | Y | resource_allocation | 0.1000 | low | --- |
| `coffee_cardiovascular_2026` | individual_a | Y | --- | 0.1200 | low | --- |
| `erasmus_rejoining_scaffold` | individual_a | Y | resource_allocation | 0.2800 | low | --- |
| `finnish_ubi_experiment` | institutiona | Y | resource_allocation | 0.2000 | low | institutional_beneficiary |
| `fmt_oncology_2026` | ambiguous | Y | --- | 0.1800 | low | --- |
| `ice_memory_archive` | collective_a | Y | global_infrastructure | 0.1000 | low | --- |
| `isa_education_scaffold` | institutiona | Y | resource_allocation | 0.4200 | medium | institutional_beneficiary, epsilon_exceeds_ceiling |
| `maha_recovery_2026` | collective_a | Y | --- | 0.4200 | low | epsilon_exceeds_ceiling |
| `manganese_catalysis_2026` | abstract_ent | Y | --- | 0.1800 | low | --- |
| `mit_tfus_2026` | institutiona | Y | --- | 0.5200 | medium | institutional_beneficiary, epsilon_exceeds_ceiling |
| `narrative_engineering_2026` | collective_a | Y | --- | 0.1500 | low | --- |
| `portugal_ad_stability_2026` | institutiona | Y | enforcement_mechanism | 0.3500 | high | institutional_beneficiary, enforcement_mechanism_coord, epsilon_exceeds_ceiling |
| `rn_proteus_adoption` | institutiona | Y | resource_allocation | 0.2000 | low | institutional_beneficiary |
| `silklink_2026` | institutiona | Y | --- | 0.2500 | low | institutional_beneficiary |
| `swift_piton_snap` | individual_a | Y | global_infrastructure | 0.0400 | low | --- |

Risk distribution: **1** high, **3** medium, **17** low.


## 5. Piton Degradation Path Assessment

N=95 pitons classified by degradation state.


### 5.1 Classification Distribution

| State | Count | % |
| :--- | ---: | ---: |
| terminal | 70 | 73.7% |
| degrading | 16 | 16.8% |
| transitional | 9 | 9.5% |


### 5.2 Signal Frequency

| Signal | Count | % of pitons |
| :--- | ---: | ---: |
| high_theater_ratio | 95 | 100.0% |
| requires_active_enforcement | 71 | 74.7% |
| institutional_beneficiary | 60 | 63.2% |
| individual_victim | 56 | 58.9% |


### 5.3 Metric Profiles by Classification

**Terminal** (N=70, intercepted: 70):

| Metric | Mean | Median | Std | Min | Max |
| :--- | ---: | ---: | ---: | ---: | ---: |
| epsilon | 0.6537 | 0.7000 | 0.2002 | 0.1500 | 0.9200 |
| suppression | 0.7196 | 0.7500 | 0.1585 | 0.1000 | 0.9600 |
| theater_ratio | 0.8470 | 0.8500 | 0.0690 | 0.7200 | 0.9900 |

**Degrading** (N=16, intercepted: 16):

| Metric | Mean | Median | Std | Min | Max |
| :--- | ---: | ---: | ---: | ---: | ---: |
| epsilon | 0.6606 | 0.6900 | 0.1835 | 0.3000 | 0.8500 |
| suppression | 0.6775 | 0.7250 | 0.2413 | 0.1000 | 0.9500 |
| theater_ratio | 0.8525 | 0.8650 | 0.0827 | 0.7400 | 0.9800 |

**Transitional** (N=9, intercepted: 9):

| Metric | Mean | Median | Std | Min | Max |
| :--- | ---: | ---: | ---: | ---: | ---: |
| epsilon | 0.3856 | 0.3500 | 0.2679 | 0.0300 | 0.7200 |
| suppression | 0.3356 | 0.3000 | 0.2717 | 0.0400 | 0.8500 |
| theater_ratio | 0.8467 | 0.8500 | 0.0699 | 0.7800 | 0.9800 |


### 5.4 Gate Interception by Classification

Intercepted by classification: {'terminal': 70, 'degrading': 16, 'transitional': 9}. Passing own gate by classification: {}.


## 6. Scaffold Lifecycle Position

N=21 scaffolds classified by lifecycle position.

| Lifecycle | Count | % |
| :--- | ---: | ---: |
| genuinely_temporary | 12 | 57.1% |
| likely_temporary | 5 | 23.8% |
| ambiguous_lifecycle | 1 | 4.8% |
| calcifying | 2 | 9.5% |
| calcified | 1 | 4.8% |

| Constraint | Lifecycle | Temp Score | Calc Score | ε | Intercepted |
| :--- | :--- | ---: | ---: | ---: | :---: |
| `ai_superpowers_2026` | calcifying | 1 | 2 | 0.6400 | Y |
| `alternative_sovereignty_scaffold` | genuinely_temporary | 4 | 0 | 0.2200 | N |
| `artificial_scarcity_scaffold` | likely_temporary | 2 | 1 | 0.4800 | Y |
| `asean_ceasefire_2011` | genuinely_temporary | 3 | 1 | 0.1500 | N |
| `boundary_protocol` | likely_temporary | 2 | 0 | 0.0000 | N |
| `canada_goose_realignment_2026` | genuinely_temporary | 4 | 0 | 0.2500 | N |
| `cinderella_midnight_deadline` | genuinely_temporary | 4 | 0 | 0.1000 | N |
| `coffee_cardiovascular_2026` | genuinely_temporary | 3 | 0 | 0.1200 | N |
| `erasmus_rejoining_scaffold` | genuinely_temporary | 4 | 0 | 0.2800 | N |
| `finnish_ubi_experiment` | genuinely_temporary | 3 | 1 | 0.2000 | N |
| `fmt_oncology_2026` | likely_temporary | 2 | 0 | 0.1800 | N |
| `ice_memory_archive` | genuinely_temporary | 4 | 0 | 0.1000 | N |
| `isa_education_scaffold` | ambiguous_lifecycle | 2 | 2 | 0.4200 | Y |
| `maha_recovery_2026` | likely_temporary | 2 | 1 | 0.4200 | Y |
| `manganese_catalysis_2026` | genuinely_temporary | 3 | 0 | 0.1800 | N |
| `mit_tfus_2026` | calcifying | 1 | 2 | 0.5200 | Y |
| `narrative_engineering_2026` | genuinely_temporary | 3 | 0 | 0.1500 | N |
| `portugal_ad_stability_2026` | calcified | 1 | 3 | 0.3500 | Y |
| `rn_proteus_adoption` | genuinely_temporary | 3 | 1 | 0.2000 | N |
| `silklink_2026` | likely_temporary | 2 | 1 | 0.2500 | N |
| `swift_piton_snap` | genuinely_temporary | 4 | 0 | 0.0400 | N |


## 7. Verdict and Implications


### 7.1 Piton Coordination Vitality

Of 95 pitons: **70** terminal (73.7%), **16** degrading (16.8%), **9** transitional (9.5%).

Dead/degrading coordination fraction: **90.5%**. Active coordination fraction: **9.5%**.

**Verdict**: Majority of pitons have dead or degrading coordination. The ontology's piton definition may be too narrow (epsilon ceiling 0.25 excludes high-extraction dead constraints), or an 'extractive piton' subtype is needed.


### 7.2 Scaffold Coordination Vitality

Of 21 scaffolds: **17** genuinely/likely temporary (81.0%), **3** calcifying/calcified (14.3%).

**Verdict**: Most scaffolds appear genuinely temporary. The scaffold extraction ceiling (0.30) may be too low for legitimate high-cost transition support.


### 7.3 Cross-Type: Does Beneficiary Type Predict Gate Outcome?

Beneficiary dominant type distribution by gate interception status:

**Intercepted** (N=101):

| Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 64 | 63.4% |
| individual_actor | 2 | 2.0% |
| collective_actor | 9 | 8.9% |
| abstract_entity | 8 | 7.9% |
| ambiguous | 18 | 17.8% |

**Passing** (N=15):

| Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 3 | 20.0% |
| individual_actor | 5 | 33.3% |
| collective_actor | 4 | 26.7% |
| abstract_entity | 1 | 6.7% |
| ambiguous | 2 | 13.3% |

Mean asymmetry score — intercepted: 0.5693, passing: 0.0444. The difference suggests beneficiary type carries predictive signal.


### 7.4 Recommendations

1. Consider an 'extractive piton' subtype for high-epsilon pitons with dead coordination — these differ from classic low-extraction pitons.
2. Consider adding 'coordination vitality' as a formal axis in the taxonomy. The binary has_coordination_function gate misses the functional/persistence distinction that this diagnostic reveals.
