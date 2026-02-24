# Rope-Dominant Spot Check

*Generated 2026-02-24 13:53 by `python/rope_dominant_spot_check.py`*

---

## 1. Executive Summary

Analyzed **40** rope-dominant tangled_rope constraints (max g_chi < 0.3 across all 4 perspectives).

### Diagnostic Counts

| Diagnostic | Count | % |
| :--- | ---: | ---: |
| Low-epsilon trivial (eps < 0.15) | 7 | 17.5% |
| Sigmoid-compressed (all f(d) < 0.5) | 0 | 0.0% |
| Low f(d) spread (< 0.1) | 0 | 0.0% |
| Chi override overlap | 0 | 0.0% |
| Perspective divergent (>1 type label) | 15 | 37.5% |
| Has tangled_rope/snare label | 38 | 95.0% |

### Recommendation Tiers

| Tier | Count | % |
| :--- | ---: | ---: |
| Keep | 27 | 67.5% |
| Investigate | 9 | 22.5% |
| Reclassify | 4 | 10.0% |

## 2. Structural Observation

All 28 constraints share **identical d, f(d), and scope_mod values** per perspective:

| Perspective | d | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 |
| moderate | 0.7000 | 1.106492 | 1.0 |
| institutional | 0.1200 | -0.042252 | 1.0 |
| analytical | 0.7200 | 1.141609 | 1.2 |

Since chi = epsilon × f(d) × scope_mod, and f(d) and scope_mod are per-perspective constants, **rope-dominance is driven entirely by epsilon**.  A constraint with low epsilon produces low chi across all perspectives, guaranteeing max(g_chi) < 0.30.

Consequently:

- **f(d) spread** = 1.400858 for all 40 (structural invariant)
- **Sigmoid-compressed count** = 0 (powerless f(d) = 1.358606 > 0.5)

## 3. Population Statistics

### 3.1 Epsilon Distribution

| Stat | Value |
| :--- | ---: |
| n | 40 |
| mean | 0.2145 |
| median | 0.2000 |
| std | 0.0889 |
| min | 0.0000 |
| max | 0.3200 |
| q25 | 0.1500 |
| q75 | 0.3000 |

| Range | Count |  |
| :--- | ---: | :--- |
| 0.00–0.09 | 4 | ████ |
| 0.10–0.14 | 3 | ███ |
| 0.15–0.19 | 6 | ██████ |
| 0.20–0.24 | 8 | ████████ |
| 0.25–0.29 | 5 | █████ |
| 0.30–0.35 | 14 | ██████████████ |

### 3.2 Domain Distribution

| Domain | Count | % |
| :--- | ---: | ---: |
| technological/economic | 6 | 15.0% |
| economic | 2 | 5.0% |
| economic/political | 2 | 5.0% |
| economic/social | 2 | 5.0% |
| political | 2 | 5.0% |
| technological | 2 | 5.0% |
| economic/technological | 2 | 5.0% |
| political/environmental | 1 | 2.5% |
| investigation/testing | 1 | 2.5% |
| technological/mathematical | 1 | 2.5% |
| mathematical/logical | 1 | 2.5% |
| mathematical/philosophical | 1 | 2.5% |
| social/ethical | 1 | 2.5% |
| legal/geopolitical/environmental | 1 | 2.5% |
| technological/cognitive | 1 | 2.5% |
| religious/philosophical/social | 1 | 2.5% |
| political/religious | 1 | 2.5% |
| technological/legal/economic | 1 | 2.5% |
| technological/scientific | 1 | 2.5% |
| health/medical | 1 | 2.5% |
| technological/social | 1 | 2.5% |
| medical/health | 1 | 2.5% |
| social/technological/biological | 1 | 2.5% |
| political/legal | 1 | 2.5% |
| technological/political/social | 1 | 2.5% |
| economic/social/technological | 1 | 2.5% |
| legal/economic/social | 1 | 2.5% |

### 3.3 Perspective Type Labels

- **Divergent** (>1 unique type across 4 perspectives): **15** / 40

- **Uniform** (all 4 perspectives agree): **25** / 40

- **Has tangled_rope or snare label**: **38** / 40

### 3.4 Chi Override Overlap

**0** of the 40 rope-dominant constraints overlap with the 19 chi override set.


## 4. Recommendations by Tier

### 4.1 Reclassify (4 constraints)

These constraints have epsilon < 0.10.  Extraction potential is trivially low; rope-dominant is obviously correct.  Recommend reclassifying from tangled_rope to rope.

| Constraint | ε | Domain | Perspective Types | Reason |
| :--- | ---: | :--- | :--- | :--- |
| `e2ee_digital_privacy_2026` | 0.05 | technological/political/social | tangled_rope | Trivially low epsilon (0.05); rope-dominant is obviously correct |
| `platform_cooperativism_governance` | 0.05 | economic/social/technological | tangled_rope | Trivially low epsilon (0.05); rope-dominant is obviously correct |
| `legacy_system_technical_debt` | 0.03 | technological/economic | rope | Trivially low epsilon (0.03); rope-dominant is obviously correct |
| `public_domain_commons` | 0.00 | legal/economic/social | rope | Trivially low epsilon (0.00); rope-dominant is obviously correct |

### 4.2 Investigate (9 constraints)

These constraints have moderate epsilon (0.10–0.19) or ambiguous perspective labels.  The tangled_rope label may or may not add signal.  Human review of the Prolog spec is recommended.

| Constraint | ε | Domain | Perspective Types | Reason |
| :--- | ---: | :--- | :--- | :--- |
| `fmt_oncology_2026` | 0.18 | health/medical | tangled_rope | Moderate epsilon (0.18); tangled_rope label may or may not add signal — needs spec review |
| `manganese_catalysis_2026` | 0.18 | technological/economic | tangled_rope | Moderate epsilon (0.18); tangled_rope label may or may not add signal — needs spec review |
| `ergo_lets_protocol` | 0.15 | economic/technological | tangled_rope | Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review |
| `ergo_storage_rent_mechanism` | 0.15 | economic/technological | tangled_rope | Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review |
| `narrative_engineering_2026` | 0.15 | technological/social | tangled_rope | Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review |
| `rogue_wave_control_2026` | 0.15 | --- | tangled_rope | Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review |
| `coffee_cardiovascular_2026` | 0.12 | medical/health | tangled_rope | Moderate epsilon (0.12); tangled_rope label may or may not add signal — needs spec review |
| `kidney_exchange_market` | 0.10 | social/technological/biological | tangled_rope | Moderate epsilon (0.10); tangled_rope label may or may not add signal — needs spec review |
| `udhr_1948` | 0.10 | political/legal | rope, tangled_rope | Moderate epsilon (0.10); tangled_rope label may or may not add signal — needs spec review |

### 4.3 Keep (27 constraints)

These constraints have epsilon >= 0.20 and at least one perspective labels them tangled_rope or snare.  The classification appears to be doing structural work, though qualitative confirmation from spec review is still valuable.

| Constraint | ε | Domain | Perspective Types | Reason |
| :--- | ---: | :--- | :--- | :--- |
| `china_africa_zero_tariff_2026` | 0.32 | economic | rope, tangled_rope | Meaningful epsilon (0.32) with tangled_rope/snare perspective labels; classification is doing structural work |
| `fiscal_equalization_friction` | 0.32 | economic/political | tangled_rope | Meaningful epsilon (0.32) with tangled_rope/snare perspective labels; classification is doing structural work |
| `oc_donation_model` | 0.32 | --- | rope, tangled_rope | Meaningful epsilon (0.32) with tangled_rope/snare perspective labels; classification is doing structural work |
| `blackstone_carried_interest_taxation` | 0.30 | economic/political | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `climate_target_one_point_five` | 0.30 | political/environmental | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `fnl_shadow_probe` | 0.30 | investigation/testing | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `gradient_descent_optimization` | 0.30 | technological/mathematical | tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `gs1_gln_identification` | 0.30 | technological/economic | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `inner_model_theory_constraints` | 0.30 | mathematical/logical | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `large_cardinal_foundations` | 0.30 | mathematical/philosophical | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `migration_decision_threshold` | 0.30 | economic/social | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `moores_law` | 0.30 | technological/economic | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `sadhu_integrity_protocol` | 0.30 | social/ethical | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `unclos_2026` | 0.30 | legal/geopolitical/environmental | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `emergency_oversight_bureau` | 0.28 | political | tangled_rope | Meaningful epsilon (0.28) with tangled_rope/snare perspective labels; classification is doing structural work |
| `erasmus_rejoining_scaffold` | 0.28 | political | tangled_rope | Meaningful epsilon (0.28) with tangled_rope/snare perspective labels; classification is doing structural work |
| `canada_goose_realignment_2026` | 0.25 | economic | tangled_rope | Meaningful epsilon (0.25) with tangled_rope/snare perspective labels; classification is doing structural work |
| `perseverance_ai_drive` | 0.25 | technological | tangled_rope | Meaningful epsilon (0.25) with tangled_rope/snare perspective labels; classification is doing structural work |
| `silklink_2026` | 0.25 | technological/economic | tangled_rope | Meaningful epsilon (0.25) with tangled_rope/snare perspective labels; classification is doing structural work |
| `cognitive_bicycle_scaffold` | 0.20 | technological/cognitive | tangled_rope | Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work |
| `ergo_autolykos_asic_resistance` | 0.20 | technological/economic | tangled_rope | Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work |
| `finnish_ubi_experiment` | 0.20 | economic/social | tangled_rope | Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work |
| `gita_kurukshetra` | 0.20 | religious/philosophical/social | rope, tangled_rope | Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work |
| `kjv_puritan_new_world_exit` | 0.20 | political/religious | rope, tangled_rope | Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work |
| `permissive_software_licensing` | 0.20 | technological/legal/economic | tangled_rope | Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work |
| `rn_proteus_adoption` | 0.20 | technological | tangled_rope | Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work |
| `solar_system_weirdness` | 0.20 | technological/scientific | tangled_rope | Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work |

## 5. Calibration Implications

- **4** constraints recommended for reclassification to plain rope (epsilon < 0.1)

- **9** constraints need human review (epsilon 0.1–0.2)

- **27** constraints appear correctly labeled (epsilon >= 0.2 with extraction signals)


**If reclassifying**: consider tightening the tangled_rope binary gate to require epsilon >= 0.10 before the tangled_rope label can apply.  This would prevent very-low-extraction coordination mechanisms from receiving the tangled_rope label.

**Structural note**: Since rope-dominance is entirely epsilon-driven (f(d) and scope_mod are perspective constants), the epsilon threshold is the only lever.  The question for the Investigate tier is whether the Prolog specs describe genuine rope-snare entanglement at moderate epsilon levels, or merely incidental extraction.

## 6. Per-Constraint Detail

Sorted by epsilon (descending).

### 6.1. `china_africa_zero_tariff_2026` — **KEEP**

**China-Africa Zero-Tariff Trade Framework** | Domain: economic

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3200 |
| suppression | 0.4500 |
| theater_ratio | 0.2000 |
| max gradient (g_chi) | 0.2851 |
| max Chi | 0.4384 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.347803 | -0.007087 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.354077 | 0.013152 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.013521 | -1.172648 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.438378 | 0.285090 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0787, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: China implements zero-tariff treatment for 53 African nations to expand market access. While nominally a coordination mechanism (Rope) for trade, the inclusion of "green channels" and "joint economic partnership pacts" introduces structural extraction through standard-setting and diplomatic alignment. KEY AGENTS (by structural relationship): - smallholder_african_farmers: Primary target (powerless/trapped) — bears highest compliance costs, lacks capital to meet standards. - organized_african_exporters: Secondary target (moderate/constrained) — can meet standards but at a significant cost. - china_state_actors: Primary beneficiary (institutional/arbitrage) — gains geopolitical alignment and resource security. - focac_administrators: Analytical observer — monitors the Forum on China-Africa Cooperation (FOCAC) outcomes.

**Key agents**: (by structural relationship): - smallholder_african_farmers: Primary target (powerless/trapped) — bears highest compliance costs, lacks capital to meet standards. - organized_african_exporters: Secondary target (moderate/constrained) — can meet standards but at a significant cost. - china_state_actors: Primary beneficiary (institutional/arbitrage) — gains geopolitical alignment and resource security. - focac_administrators: Analytical observer — monitors the Forum on China-Africa Cooperation (FOCAC) outcomes.

**Beneficiaries**: china_state_actors, _

**Victims**: african_producers_and_exporters, _

**Omega questions**:

- Non-tariff barrier impact of green channel standards

**Recommendation**: KEEP — Meaningful epsilon (0.32) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.2. `fiscal_equalization_friction` — **KEEP**

**The Equalization Conflict (Net Transfer Friction)** | Domain: economic/political

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3200 |
| suppression | 0.1500 |
| theater_ratio | 0.4500 |
| max gradient (g_chi) | 0.2851 |
| max Chi | 0.4384 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.347803 | -0.007087 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.354077 | 0.013152 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.013521 | -1.172648 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.438378 | 0.285090 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: Models the coordination mechanism of fiscal federalism in Canada, specifically the equalization program. This system transfers funds from wealthier ("have") provinces to less wealthy ("have-not") provinces to ensure comparable levels of public services. From the perspective of net contributor provinces like Alberta, this constitutional coordination tool is often perceived as a "siphoning" of resource wealth. KEY AGENTS (by structural relationship): - individual_taxpayer_in_alberta: Primary target (powerless/trapped) — Experiences tax outflow with no direct control or perceived return. - provincial_nationalist_bloc: Secondary target (moderate/constrained) — Politically frames the net fiscal outflow as extraction to argue for sovereignty. - federal_government: Primary beneficiary (institutional/arbitrage) — Maintains national standards and political stability through redistribution. - recipient_provinces: Secondary beneficiary (institutional/constrained) — Relies on transfers for service parity. - fiscal_analyst: Analytical observer — Evaluates net contribution formulas and their economic impact.

**Key agents**: (by structural relationship): - individual_taxpayer_in_alberta: Primary target (powerless/trapped) — Experiences tax outflow with no direct control or perceived return. - provincial_nationalist_bloc: Secondary target (moderate/constrained) — Politically frames the net fiscal outflow as extraction to argue for sovereignty. - federal_government: Primary beneficiary (institutional/arbitrage) — Maintains national standards and political stability through redistribution. - recipient_provinces: Secondary beneficiary (institutional/constrained) — Relies on transfers for service parity. - fiscal_analyst: Analytical observer — Evaluates net contribution formulas and their economic impact.

**Beneficiaries**: federal_government_and_recipient_provinces

**Victims**: net_contributor_provinces

**Omega questions**:

- Definitional threshold of fiscal fairness in a federal system

**Recommendation**: KEEP — Meaningful epsilon (0.32) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.3. `oc_donation_model` — **KEEP**

**None** | Domain: ---

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3200 |
| suppression | 0.4500 |
| theater_ratio | 0.1000 |
| max gradient (g_chi) | 0.2851 |
| max Chi | 0.4384 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.347803 | -0.007087 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.354077 | 0.013152 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.013521 | -1.172648 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.438378 | 0.285090 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0691, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: Open Culture is a web-based cultural and educational aggregator that provides free access to a vast repository of high-quality content. The organization is sustained by a voluntary donation model, where a small fraction of users provide the financial support for the entire operation. This constraint is the funding mechanism itself: a system for solving a public goods problem that relies on asymmetric, non-coercive extraction. KEY AGENTS (by structural relationship): - Donors: Primary target (moderate/mobile) — The small subset of users who bear the financial cost of the service for everyone. - Open Culture Editors: Primary beneficiary (institutional/arbitrage) — The organization that uses the funds to operate and fulfill its mission. - Global Learners: Secondary beneficiary (powerless/mobile) — The vast majority of users who access the content for free. - Analytical Observer: Sees the full structure of coordination and asymmetric cost-bearing.

**Key agents**: (by structural relationship): - Donors: Primary target (moderate/mobile) — The small subset of users who bear the financial cost of the service for everyone. - Open Culture Editors: Primary beneficiary (institutional/arbitrage) — The organization that uses the funds to operate and fulfill its mission. - Global Learners: Secondary beneficiary (powerless/mobile) — The vast majority of users who access the content for free. - Analytical Observer: Sees the full structure of coordination and asymmetric cost-bearing.

**Beneficiaries**: open_culture_editors, global_learners, _

**Victims**: donors, _

**Recommendation**: KEEP — Meaningful epsilon (0.32) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.4. `blackstone_carried_interest_taxation` — **KEEP**

**Carried Interest Partnership Taxation** | Domain: economic/political

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3000 |
| suppression | 0.7000 |
| theater_ratio | 0.1000 |
| max gradient (g_chi) | 0.1967 |
| max Chi | 0.4110 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.326066 | -0.077206 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.331948 | -0.058232 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.012676 | -1.169923 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.410979 | 0.196706 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.7755, band=snare_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: The regulatory and tax framework in the United States that treats "carried interest" (performance fees) for partners in private equity and hedge funds as long-term capital gains rather than ordinary income. This results in a significantly lower tax rate. The constraint's existence relies on active lobbying and complex legal structuring to defend its status against legislative challenges that seek to reclassify it as income. KEY AGENTS (by structural relationship): - us_taxpayers: Primary target (powerless/trapped) — bear the cost via reduced public tax revenue, with no direct recourse. - private_equity_partners: Primary beneficiary (institutional/arbitrage) — benefit from lower tax rates on their primary form of compensation. - reformist_legislators: Secondary institutional actor (institutional/constrained) — attempt to change the rule but face high political and structural barriers. - analytical_observer: Analytical observer — sees the dual function of coordination (for partners) and extraction (from the tax base).

**Key agents**: (by structural relationship): - us_taxpayers: Primary target (powerless/trapped) — bear the cost via reduced public tax revenue, with no direct recourse. - private_equity_partners: Primary beneficiary (institutional/arbitrage) — benefit from lower tax rates on their primary form of compensation. - reformist_legislators: Secondary institutional actor (institutional/constrained) — attempt to change the rule but face high political and structural barriers. - analytical_observer: Analytical observer — sees the dual function of coordination (for partners) and extraction (from the tax base).

**Beneficiaries**: private_equity_partners

**Victims**: us_taxpayers

**Omega questions**:

- Whether the 

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.5. `climate_target_one_point_five` — **KEEP**

**The 1.5°C Global Warming Target** | Domain: political/environmental

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3000 |
| suppression | 0.6000 |
| theater_ratio | 0.1000 |
| max gradient (g_chi) | 0.1967 |
| max Chi | 0.4110 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.326066 | -0.077206 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.331948 | -0.058232 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.012676 | -1.169923 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.410979 | 0.196706 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.4757, band=genuinely_tangled, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: The 1.5°C target is a global policy constraint that lowered the "safe" warming threshold from 2°C. Championed by the Alliance of Small Island States (AOSIS), it redefines acceptable climate risk based on the survival of the most vulnerable nations rather than the economic convenience of larger powers. It functions as both a coordination mechanism for global climate action and an extractive limit on high-emission development paths. KEY AGENTS (by structural relationship): - Small Island States (AOSIS): Primary beneficiary (organized/trapped) — uses the target as a survival mechanism. - Fossil Fuel Reliant Economies: Primary target (institutional/constrained) — bears the cost of constrained development. - Citizen in a Coastal Community: Secondary target (powerless/trapped) — experiences the physical reality of climate change, for whom the policy target is largely abstract. - Analytical Observer: Sees the dual coordination/extraction function.

**Key agents**: (by structural relationship): - Small Island States (AOSIS): Primary beneficiary (organized/trapped) — uses the target as a survival mechanism. - Fossil Fuel Reliant Economies: Primary target (institutional/constrained) — bears the cost of constrained development. - Citizen in a Coastal Community: Secondary target (powerless/trapped) — experiences the physical reality of climate change, for whom the policy target is largely abstract. - Analytical Observer: Sees the dual coordination/extraction function.

**Beneficiaries**: small_island_states, _

**Victims**: fossil_fuel_reliant_economies, _

**Omega questions**:

- Physical attainability of the 1.5C target given current global inertia.

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.6. `fnl_shadow_probe` — **KEEP**

**FNL Shadow Mode Probe (Physics-Washed Construction)** | Domain: investigation/testing

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3000 |
| suppression | 0.5000 |
| theater_ratio | 0.1000 |
| max gradient (g_chi) | 0.1967 |
| max Chi | 0.4110 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.326066 | -0.077206 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.331948 | -0.058232 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.012676 | -1.169923 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.410979 | 0.196706 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0973, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: A synthetic constraint designed to model a constructed system that could be mistaken for a natural law (a False Natural Law, or FNL). It combines a genuine coordination function with asymmetric extraction, enforced by an active apparatus. Its high suppression score and requirement for active enforcement are structural giveaways that it is constructed, not natural. From the beneficiary's perspective, it appears as a low-cost coordination mechanism (Rope), while for its targets, it is an extractive Tangled Rope. KEY AGENTS (by structural relationship): - constrained_subjects: Primary target (powerless/trapped) — bears the costs of the system. - institutional_apparatus: Primary beneficiary (institutional/arbitrage) — benefits from the coordination and extraction. - Analytical observer: Sees the full structure, including both the coordination function and the asymmetric extraction.

**Key agents**: (by structural relationship): - constrained_subjects: Primary target (powerless/trapped) — bears the costs of the system. - institutional_apparatus: Primary beneficiary (institutional/arbitrage) — benefits from the coordination and extraction. - Analytical observer: Sees the full structure, including both the coordination function and the asymmetric extraction.

**Beneficiaries**: institutional_apparatus, _

**Victims**: constrained_subjects, _

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.7. `gradient_descent_optimization` — **KEEP**

**Gradient Descent Iterative Optimization** | Domain: technological/mathematical

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3000 |
| suppression | 0.2000 |
| theater_ratio | 0.1000 |
| max gradient (g_chi) | 0.1967 |
| max Chi | 0.4110 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.326066 | -0.077206 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.331948 | -0.058232 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.012676 | -1.169923 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.410979 | 0.196706 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: Gradient Descent is a first-order iterative optimization algorithm for finding a local minimum of a differentiable function. It functions as a fundamental tool in machine learning, but its behavior is highly dependent on the topology of the "loss landscape" it traverses. The constraint models the rules of this traversal. KEY AGENTS (by structural relationship): - Parameter Vector (theta): Primary target (powerless/trapped) — its state is dictated by the update rule. - ML Practitioner: Primary beneficiary (institutional/mobile) — uses the algorithm as a tool to achieve a goal. - Hardware & Energy Budgets: Secondary victims (powerless/trapped) — bear the computational cost. - Analytical Observer: Analytical observer — sees the full mathematical structure.

**Key agents**: (by structural relationship): - Parameter Vector (theta): Primary target (powerless/trapped) — its state is dictated by the update rule. - ML Practitioner: Primary beneficiary (institutional/mobile) — uses the algorithm as a tool to achieve a goal. - Hardware & Energy Budgets: Secondary victims (powerless/trapped) — bear the computational cost. - Analytical Observer: Analytical observer — sees the full mathematical structure.

**Beneficiaries**: ml_practitioners, automation_systems

**Victims**: hardware_longevity, energy_budgets

**Omega questions**:

- Whether the specific loss landscape is convex, determining if local minima are a trap.

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.8. `gs1_gln_identification` — **KEEP**

**Global Location Number (GLN) Standard** | Domain: technological/economic

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3000 |
| suppression | 0.7000 |
| theater_ratio | 0.1300 |
| max gradient (g_chi) | 0.1967 |
| max Chi | 0.4110 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.326066 | -0.077206 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.331948 | -0.058232 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.012676 | -1.169923 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.410979 | 0.196706 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.7826, band=snare_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: The GLN is a 13-digit GS1 identification key used to identify physical locations (warehouses, hospital rooms) or legal entities (corporations). It constrains the digital "map" of global commerce by enforcing a single, globally unique identifier for every point of business interaction.

**Key agents**: - Warehouse Receiving Clerk: Individual powerless; scans GLNs as an immutable part of their environment. - Supply Chain Director: Institutional; uses GLNs to coordinate complex inter-organizational shipping and billing. - Regulatory Auditor: Analytical; evaluates the traceability and data integrity provided by the GLN hierarchy.

**Beneficiaries**: global_traceability_efficiency, enterprise_resource_planning_vendors

**Victims**: small_scale_suppliers_overhead

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.9. `inner_model_theory_constraints` — **KEEP**

**The Axiom of Constructibility (V=L)** | Domain: mathematical/logical

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3000 |
| suppression | 0.5000 |
| theater_ratio | 0.0000 |
| max gradient (g_chi) | 0.1967 |
| max Chi | 0.4110 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.326066 | -0.077206 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.331948 | -0.058232 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.012676 | -1.169923 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.410979 | 0.196706 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0877, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: Inner Model Theory studies sub-universes of the set-theoretic universe (V) that satisfy the axioms of ZFC. The archetypal inner model is Gödel's Constructible Universe (L), defined by the axiom V=L. This axiom imposes a rigid, determinate structure on the transfinite, resolving questions like the Continuum Hypothesis. However, this determinacy comes at the cost of excluding more complex structures like large cardinals, creating a significant perspectival gap between different schools of set theorists. KEY AGENTS (by structural relationship): - Large Cardinal Pluralists (Victim): Set theorists who view V=L as a snare that extracts the potential richness of the set-theoretic universe (V) to enforce a narrow, predictable structure. (moderate/constrained) - Consistency Proof Developers (Beneficiary): Logicians and model theorists who use inner models like L as a coordination tool (a rope) to establish the relative consistency of various mathematical axioms. (institutional/mobile) - The Constructible Set (Subject): A mathematical object whose existence is rigidly defined by the L-hierarchy, for which the constraint is an unchangeable law of its nature. (powerless/trapped) - Analytical Observer: Sees the full structure as a Tangled Rope, acknowledging both its genuine coordination function for consistency proofs and its extractive nature in suppressing alternative set-theoretic ontologies.

**Key agents**: (by structural relationship): - Large Cardinal Pluralists (Victim): Set theorists who view V=L as a snare that extracts the potential richness of the set-theoretic universe (V) to enforce a narrow, predictable structure. (moderate/constrained) - Consistency Proof Developers (Beneficiary): Logicians and model theorists who use inner models like L as a coordination tool (a rope) to establish the relative consistency of various mathematical axioms. (institutional/mobile) - The Constructible Set (Subject): A mathematical object whose existence is rigidly defined by the L-hierarchy, for which the constraint is an unchangeable law of its nature. (powerless/trapped) - Analytical Observer: Sees the full structure as a Tangled Rope, acknowledging both its genuine coordination function for consistency proofs and its extractive nature in suppressing alternative set-theoretic ontologies.

**Beneficiaries**: consistency_proof_developers, fine_structure_theorists

**Victims**: large_cardinal_pluralists

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.10. `large_cardinal_foundations` — **KEEP**

**Large Cardinal Axioms as a Foundational System** | Domain: mathematical/philosophical

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3000 |
| suppression | 0.4000 |
| theater_ratio | 0.0100 |
| max gradient (g_chi) | 0.1967 |
| max Chi | 0.4110 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.326066 | -0.077206 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.331948 | -0.058232 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.012676 | -1.169923 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.410979 | 0.196706 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0050, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

*Prolog spec file not found.*

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.11. `migration_decision_threshold` — **KEEP**

**The Migration Decision Threshold (Cost-Benefit Equilibrium)** | Domain: economic/social

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3000 |
| suppression | 0.4000 |
| theater_ratio | 0.0800 |
| max gradient (g_chi) | 0.1967 |
| max Chi | 0.4110 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.326066 | -0.077206 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.331948 | -0.058232 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.012676 | -1.169923 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.410979 | 0.196706 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0055, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

*Prolog spec file not found.*

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.12. `moores_law` — **KEEP**

**Moore's Law as an Industrial Convention** | Domain: technological/economic

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3000 |
| suppression | 0.5000 |
| theater_ratio | 0.0200 |
| max gradient (g_chi) | 0.1967 |
| max Chi | 0.4110 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.326066 | -0.077206 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.331948 | -0.058232 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.012676 | -1.169923 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.410979 | 0.196706 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0893, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: Moore's Law is the observation that the number of transistors on a microchip doubles approximately every two years. This story models the law not as a physical inevitability, but as a self-fulfilling prophecy or a "socially enforced" pace of innovation that coordinates the global semiconductor industry while simultaneously creating a coercive R&D treadmill and planned obsolescence. KEY AGENTS (by structural relationship): - Chip Fabricators (e.g., Intel, TSMC): Primary target (institutional/constrained) — bears the immense R&D cost to maintain the pace. - Platform Capitalists & Software Developers: Primary beneficiary (institutional/arbitrage) — benefits from predictable hardware gains to build more complex services. - Consumers / Legacy Infrastructure Owners: Secondary target (powerless/mobile) — benefits from cheaper compute but is subject to planned obsolescence. - Analytical Observer: Sees the full structure as a Tangled Rope of coordination and extraction.

**Key agents**: (by structural relationship): - Chip Fabricators (e.g., Intel, TSMC): Primary target (institutional/constrained) — bears the immense R&D cost to maintain the pace. - Platform Capitalists & Software Developers: Primary beneficiary (institutional/arbitrage) — benefits from predictable hardware gains to build more complex services. - Consumers / Legacy Infrastructure Owners: Secondary target (powerless/mobile) — benefits from cheaper compute but is subject to planned obsolescence. - Analytical Observer: Sees the full structure as a Tangled Rope of coordination and extraction.

**Beneficiaries**: platform_capitalists, software_developers, _

**Victims**: semiconductor_fabricators, legacy_infrastructure_owners, _

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.13. `sadhu_integrity_protocol` — **KEEP**

**The Integrity Requirement (Sadhu's Sugar)** | Domain: social/ethical

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3000 |
| suppression | 0.4000 |
| theater_ratio | 0.1400 |
| max gradient (g_chi) | 0.1967 |
| max Chi | 0.4110 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.326066 | -0.077206 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.331948 | -0.058232 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.012676 | -1.169923 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.410979 | 0.196706 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0061, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: This constraint mandates that an advisor must embody their own counsel before delivering it to others. In the source narrative, a Sadhu (holy man) delays giving advice to a boy to stop eating sugar for two weeks because the Sadhu was still consuming sugar himself. The constraint posits that effective social coordination (advice) requires internal synchronization (integrity), extracting time from those seeking immediate solutions to ensure efficacy. KEY AGENTS (by structural relationship): - The Mother & Son (seekers_of_immediate_remedies): Primary targets (powerless/trapped) — bear the cost of the two-week delay. - The Sadhu (upholders_of_tradition): Primary beneficiary (institutional/mobile) — benefits from enhanced authority and social efficacy. - The Community (implicit): Secondary beneficiary — benefits from a system of trustworthy counsel. - Analytical Observer: Sees the full structure of coordination and extraction.

**Key agents**: (by structural relationship): - The Mother & Son (seekers_of_immediate_remedies): Primary targets (powerless/trapped) — bear the cost of the two-week delay. - The Sadhu (upholders_of_tradition): Primary beneficiary (institutional/mobile) — benefits from enhanced authority and social efficacy. - The Community (implicit): Secondary beneficiary — benefits from a system of trustworthy counsel. - Analytical Observer: Sees the full structure of coordination and extraction.

**Beneficiaries**: upholders_of_tradition

**Victims**: seekers_of_immediate_remedies

**Omega questions**:

- Causality of advisor

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.14. `unclos_2026` — **KEEP**

**UN Convention on the Law of the Sea (2026 Context)** | Domain: legal/geopolitical/environmental

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3000 |
| suppression | 0.2000 |
| theater_ratio | 0.1400 |
| max gradient (g_chi) | 0.1967 |
| max Chi | 0.4110 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.326066 | -0.077206 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.331948 | -0.058232 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.012676 | -1.169923 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.410979 | 0.196706 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: UNCLOS, often called the "Constitution of the Oceans," establishes a comprehensive legal framework for all maritime activities. In 2026, this constraint is revitalized by the entry into force of the BBNJ Treaty (High Seas Treaty) on January 17, 2026, which creates the first legally binding rules for biodiversity in international waters.

**Key agents**: - Coastal States (e.g., Philippines): Rely on UNCLOS to defend sovereign rights within 200nm Exclusive Economic Zones (EEZ). - Hegemonic Dissents (e.g., China, USA): Challenge specific parts of the regime (e.g., seabed mining or compulsory arbitration) as coercive. - International Seabed Authority (ISA): Institutional enforcer of mining rules.

**Beneficiaries**: land_locked_states, coastal_middle_powers

**Victims**: maritime_unilateralists

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.15. `emergency_oversight_bureau` — **KEEP**

**The Crisis Scaffold** | Domain: political

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2800 |
| suppression | 0.7500 |
| theater_ratio | 0.1500 |
| max gradient (g_chi) | 0.1083 |
| max Chi | 0.3836 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.304328 | -0.147329 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.309818 | -0.129619 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.011831 | -1.167197 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.383581 | 0.108326 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.6554, band=genuinely_tangled, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: A temporary administrative body created to manage a specific recovery period. While it exerts high control (suppression), it includes a mandatory expiration date to prevent it from hardening into a permanent Snare. Its extraction is moderate, representing compliance costs rather than direct rent-seeking.

**Key agents**: - The Citizen: Subject (Powerless) - Undergoing temporary mandatory vetting. - The Coordinator: Architect (Organized) - Managing the sunset transition. - The Historian: Auditor (Analytical) - Monitoring for "Scaffold-to-Piton" drift.

**Beneficiaries**: crisis_affected_populations

**Victims**: citizens_under_vetting

**Recommendation**: KEEP — Meaningful epsilon (0.28) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.16. `erasmus_rejoining_scaffold` — **KEEP**

**UK's potential re-entry into the EU Erasmus+ student exchange program** | Domain: political

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2800 |
| suppression | 0.2000 |
| theater_ratio | 0.2500 |
| max gradient (g_chi) | 0.1083 |
| max Chi | 0.3836 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.304328 | -0.147329 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.309818 | -0.129619 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.011831 | -1.167197 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.383581 | 0.108326 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: Following its exit from the EU and the Erasmus+ program, the UK is considering rejoining. This constraint models the structure of the re-entry agreement itself. It is framed as a temporary support (Scaffold) to rebuild a coordination mechanism (student and academic mobility) that was dismantled, with clear beneficiaries and a quantifiable cost borne by the state. KEY AGENTS (by structural relationship): - UK Students & Universities: Primary beneficiary (organized/constrained) — regain access to EU mobility. - UK Taxpayers & Fiscal Conservatives: Primary cost-bearer (powerless/trapped) — bear the financial contribution to the EU budget. - UK Government: Architect & Institutional Beneficiary (institutional/constrained) — gains soft power and satisfies a key sector, but constrained by political pressure. - EU Commission: Institutional Beneficiary (institutional/arbitrage) — regains a major partner, strengthening the program, with many other partners as alternatives. - Analytical Observer: Sees the full structure of costs, benefits, and temporary nature.

**Key agents**: (by structural relationship): - UK Students & Universities: Primary beneficiary (organized/constrained) — regain access to EU mobility. - UK Taxpayers & Fiscal Conservatives: Primary cost-bearer (powerless/trapped) — bear the financial contribution to the EU budget. - UK Government: Architect & Institutional Beneficiary (institutional/constrained) — gains soft power and satisfies a key sector, but constrained by political pressure. - EU Commission: Institutional Beneficiary (institutional/arbitrage) — regains a major partner, strengthening the program, with many other partners as alternatives. - Analytical Observer: Sees the full structure of costs, benefits, and temporary nature.

**Beneficiaries**: uk_students_and_universities, eu_students_and_universities, uk_government

**Victims**: uk_taxpayers_and_fiscal_conservatives

**Recommendation**: KEEP — Meaningful epsilon (0.28) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.17. `canada_goose_realignment_2026` — **KEEP**

**Canada Goose Strategic Realignment Under New Leadership (2026)** | Domain: economic

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2500 |
| suppression | 0.1500 |
| theater_ratio | 0.6000 |
| max gradient (g_chi) | -0.0242 |
| max Chi | 0.3425 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9500 | 1.392945 | 0.8 | 0.278589 | -0.230358 |
| moderate | 0.6500 | 1.008614 | 1.0 | 0.252154 | -0.315632 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.010563 | -1.163106 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.342483 | -0.024248 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4352)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: In early 2026, luxury brand Canada Goose appointed Patrick Bourke as President of North America. This move signals a strategic shift focusing on expanding the brand's direct-to-consumer retail footprint while implementing significant cost management and operational efficiency measures. The constraint represents this temporary, top-down strategic framework intended to "reignite brand heat" and improve profitability over a defined fiscal period.

**Key agents**: - Retail Employee: Subject (Powerless) - Investors & Executives: Beneficiary (Institutional) - Market Analyst: Auditor (Analytical)

**Beneficiaries**: investors_and_executives, _

**Recommendation**: KEEP — Meaningful epsilon (0.25) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.18. `perseverance_ai_drive` — **KEEP**

**AI-Driven Martian Rover Autonomy** | Domain: technological

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2500 |
| suppression | 0.1000 |
| theater_ratio | 0.0500 |
| max gradient (g_chi) | -0.0242 |
| max Chi | 0.3425 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9500 | 1.392945 | 0.8 | 0.278589 | -0.230358 |
| moderate | 0.6500 | 1.008614 | 1.0 | 0.252154 | -0.315632 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.010563 | -1.163106 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.342483 | -0.024248 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4352)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: NASA's Perseverance rover successfully completed its first AI-planned drive on Mars, autonomously navigating a 200-foot (61-meter) path. The constraint lies in the rover's limited resources (processing power, energy) for navigation and decision-making, requiring a balance between autonomy and reliance on Earth-based commands.

**Key agents**: - Rover Resources: Subject (Powerless) - NASA Engineers: Beneficiary (Institutional) - Scientific Community: Auditor (Analytical)

**Beneficiaries**: nasa_engineers

**Recommendation**: KEEP — Meaningful epsilon (0.25) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.19. `silklink_2026` — **KEEP**

**SilkLink Syria-Saudi Telecom Project** | Domain: technological/economic

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2500 |
| suppression | 0.4000 |
| theater_ratio | 0.1500 |
| max gradient (g_chi) | -0.0242 |
| max Chi | 0.3425 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.271721 | -0.252513 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.276623 | -0.236700 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.010563 | -1.163106 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.342483 | -0.024248 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0004, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: On Feb 7, 2026, Syria and Saudi Arabia signed the "SilkLink" agreement, a nearly $1B infrastructure project led by Saudi Telecom Company (STC). It involves laying 4,500km of fiber-optic cables and establishing data centers to turn Syria into a regional hub connecting Asia and Europe.

**Key agents**: - Syrian Citizens/Tech Sector: Subject (Organized - Potential growth) - STC Group: Beneficiary (Institutional - Monopoly access) - Regional Data Carriers: Auditor (Analytical)

**Beneficiaries**: stc_group_dominance

**Victims**: legacy_intermediary_carriers

**Recommendation**: KEEP — Meaningful epsilon (0.25) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.20. `cognitive_bicycle_scaffold` — **KEEP**

**The Bicycle of the Mind** | Domain: technological/cognitive

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2000 |
| suppression | 0.3000 |
| theater_ratio | 0.1500 |
| max gradient (g_chi) | -0.2452 |
| max Chi | 0.2740 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.217377 | -0.427816 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.221298 | -0.415168 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.008450 | -1.156290 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.273986 | -0.245206 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: A tool-based constraint where an agent utilizes an AI assistant to amplify reasoning capacity. The tool's classification depends heavily on the user's pre-existing skill and power. For a skilled user, it's a force multiplier. For an unskilled user, it can become a dependency that extracts the user's own cognitive faculties.

**Key agents**: - The Dependent User: Subject (Powerless) - Uses the AI as a crutch, leading to skill atrophy. - The System Architect: Beneficiary (Institutional) - Provides the tool as a temporary support structure. - The Skilled User: Auditor (Analytical) - Uses the AI as a bicycle to augment existing skills.

**Beneficiaries**: system_architects, _

**Victims**: dependent_users

**Recommendation**: KEEP — Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.21. `ergo_autolykos_asic_resistance` — **KEEP**

**Autolykos PoW Algorithm (ASIC Resistance)** | Domain: technological/economic

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2000 |
| suppression | 0.8500 |
| theater_ratio | 0.1400 |
| max gradient (g_chi) | -0.2452 |
| max Chi | 0.2740 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.217377 | -0.427816 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.221298 | -0.415168 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.008450 | -1.156290 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.273986 | -0.245206 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.4762, band=genuinely_tangled, signature=false_natural_law, coalition=uniform_tangled

**Spec summary**: Autolykos is Ergo's Proof-of-Work (PoW) algorithm, specifically designed to be memory-hard and ASIC-resistant. By requiring large datasets (the dataset size increases over time) to be held in GPU memory, it prevents specialized mining hardware (ASICs) from dominating the hash rate, thus favoring commodity GPU hardware.

**Key agents**: - GPU Miners: Individual and small-scale participants using off-the-shelf hardware. - ASIC Manufacturers: Institutional entities that build high-efficiency specialized hardware. - Network Security: The collective robustness against 51% attacks and hash rate centralization.

**Beneficiaries**: retail_gpu_miners

**Victims**: asic_manufacturers

**Recommendation**: KEEP — Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.22. `finnish_ubi_experiment` — **KEEP**

**Finnish Basic Income Experiment (2017-2018)** | Domain: economic/social

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2000 |
| suppression | 0.3500 |
| theater_ratio | 0.1500 |
| max gradient (g_chi) | -0.2452 |
| max Chi | 0.2740 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.217377 | -0.427816 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.221298 | -0.415168 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.008450 | -1.156290 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.273986 | -0.245206 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: Between 2017 and 2018, Finland conducted a nationwide basic income experiment. 2,000 unemployed individuals were given a guaranteed, unconditional monthly payment of €560. The experiment aimed to test whether a simplified social security model could reduce bureaucracy and incentivize employment by removing welfare traps. This constraint represents the structure, rules, and temporary nature of this policy intervention. KEY AGENTS (by structural relationship): - Unemployed Participants: Primary target (powerless/trapped) — subject to the experiment's rules, with no ability to opt-out. - Kela & Finnish Policymakers: Primary beneficiary (institutional/arbitrage) — gained valuable data, tested a policy hypothesis, and controlled the experiment's lifecycle. - Social Scientists: Analytical observer — sees the full structure, including its temporary nature and coordination goals.

**Key agents**: (by structural relationship): - Unemployed Participants: Primary target (powerless/trapped) — subject to the experiment's rules, with no ability to opt-out. - Kela & Finnish Policymakers: Primary beneficiary (institutional/arbitrage) — gained valuable data, tested a policy hypothesis, and controlled the experiment's lifecycle. - Social Scientists: Analytical observer — sees the full structure, including its temporary nature and coordination goals.

**Beneficiaries**: kela_policymakers

**Victims**: unemployed_participants

**Recommendation**: KEEP — Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.23. `gita_kurukshetra` — **KEEP**

**The Duty of the Kshatriya (Warrior Caste)** | Domain: religious/philosophical/social

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2000 |
| suppression | 0.9000 |
| theater_ratio | 0.3400 |
| max gradient (g_chi) | -0.2452 |
| max Chi | 0.2740 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.217377 | -0.427816 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.221298 | -0.415168 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.008450 | -1.156290 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.273986 | -0.245206 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0406, band=rope_leaning, signature=false_ci_rope, coalition=other

**Spec summary**: The "dharma" or sacred duty of the warrior caste (Kshatriya) to fight in a lawful war, even against kin. It presents a powerful moral obligation rooted in a specific metaphysical worldview and social order, as articulated by Krishna to Arjuna on the battlefield of Kurukshetra. The constraint is the social and ideological pressure to conform to one's caste duty. KEY AGENTS (by structural relationship): - Arjuna: Primary target (powerless/trapped) — bears the moral and psychological cost of the duty. - The Cosmic Order (as personified by Krishna): Primary beneficiary (institutional/arbitrage) — the social and metaphysical system is upheld by adherence to dharma. - The Enlightened Sage: Analytical observer — perceives the system's logic from a detached viewpoint.

**Key agents**: (by structural relationship): - Arjuna: Primary target (powerless/trapped) — bears the moral and psychological cost of the duty. - The Cosmic Order (as personified by Krishna): Primary beneficiary (institutional/arbitrage) — the social and metaphysical system is upheld by adherence to dharma. - The Enlightened Sage: Analytical observer — perceives the system's logic from a detached viewpoint.

**Beneficiaries**: cosmic_order

**Victims**: individual_ego

**Omega questions**:

- Is selfless action universally applicable or dependent on a specific metaphysical frame?

**Recommendation**: KEEP — Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.24. `kjv_puritan_new_world_exit` — **KEEP**

**The Puritan Textual Re-Indexing (KJV in the New World)** | Domain: political/religious

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2000 |
| suppression | 0.5000 |
| theater_ratio | 0.1700 |
| max gradient (g_chi) | -0.2452 |
| max Chi | 0.2740 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.217377 | -0.427816 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.221298 | -0.415168 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.008450 | -1.156290 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.273986 | -0.245206 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0008, band=rope_leaning, signature=false_ci_rope, coalition=other

**Spec summary**: This story tracks the movement of the King James Version (KJV) of the Bible across a geographic "Exit Option." In England, the KJV was a tool of the Crown used to suppress Puritan identity (perceived as a Snare). Upon arrival in the New World, the Puritans—now holding institutional power—adopted the KJV as a functional coordination tool (Rope) to maintain social order and literacy in a "wilderness" environment. For subsequent generations born in the colonies, it became an immutable feature of reality (perceived as a Mountain). KEY AGENTS (by structural relationship): - English Puritan Dissenters: Primary target (in the original English context) (moderate/trapped) — bears suppression. - New England Magistrates: Primary beneficiary (in the New World context) (institutional/mobile) — benefits from the text as a coordination tool. - Second-Generation Colonists: Inheritor of the system (powerless/trapped) — experiences the text as a fixed reality. - Analytical Observer: Sees the full re-indexing dynamic.

**Key agents**: (by structural relationship): - English Puritan Dissenters: Primary target (in the original English context) (moderate/trapped) — bears suppression. - New England Magistrates: Primary beneficiary (in the New World context) (institutional/mobile) — benefits from the text as a coordination tool. - Second-Generation Colonists: Inheritor of the system (powerless/trapped) — experiences the text as a fixed reality. - Analytical Observer: Sees the full re-indexing dynamic.

**Beneficiaries**: new_england_magistrates

**Victims**: english_puritan_dissenters

**Omega questions**:

- Whether the coercive potential of a re-indexed text remains latent or is permanently neutralized.

**Recommendation**: KEEP — Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.25. `permissive_software_licensing` — **KEEP**

**Permissive Software Licenses (e.g., MIT, Apache)** | Domain: technological/legal/economic

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2000 |
| suppression | 0.1000 |
| theater_ratio | 0.1000 |
| max gradient (g_chi) | -0.2452 |
| max Chi | 0.2740 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.217377 | -0.427816 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.221298 | -0.415168 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.008450 | -1.156290 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.273986 | -0.245206 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.4762, band=genuinely_tangled, signature=false_natural_law, coalition=uniform_tangled

**Spec summary**: Permissive licenses (like MIT or Apache) allow users to do almost anything with source code—copy, modify, and redistribute it—even as part of proprietary, closed-source software. The only common requirement is to include the original copyright and license notice.

**Key agents**: - The Corporate Developer (Institutional): Seeks to incorporate open-source code into proprietary products. - The Copyleft Ideologue (Individual Powerful): Believes software freedom requires sharing modifications. - The Hobbyist Developer (Individual Powerless): Uses free code for personal projects without legal burden.

**Beneficiaries**: commercial_software_vendors

**Victims**: reciprocal_commons

**Recommendation**: KEEP — Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.26. `rn_proteus_adoption` — **KEEP**

**Royal Navy's adoption of the Leonardo Proteus uncrewed helicopter** | Domain: technological

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2000 |
| suppression | 0.3500 |
| theater_ratio | 0.1500 |
| max gradient (g_chi) | -0.2452 |
| max Chi | 0.2740 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.217377 | -0.427816 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.221298 | -0.415168 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.008450 | -1.156290 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.273986 | -0.245206 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: The UK Royal Navy has initiated a two-year experimental contract to integrate the Leonardo Proteus, an uncrewed helicopter, into its fleet starting in 2026. This technology will take on roles like resupply and surveillance, currently performed by manned aircraft. The constraint is the institutional and technological path dependency created by this pilot program, which shifts risk, cost, and roles within the naval aviation structure. KEY AGENTS (by structural relationship): - Manned Helicopter Crews: Primary target (organized/constrained) — face role displacement and potential career path obsolescence. - Leonardo (Manufacturer): Primary beneficiary (institutional/arbitrage) — secures a key contract and market position. - Royal Navy Command: Secondary beneficiary (institutional/constrained) — gains new capabilities and potential long-term cost savings. - Analytical Observer: Sees the full structure, including the temporary/experimental nature of the constraint.

**Key agents**: (by structural relationship): - Manned Helicopter Crews: Primary target (organized/constrained) — face role displacement and potential career path obsolescence. - Leonardo (Manufacturer): Primary beneficiary (institutional/arbitrage) — secures a key contract and market position. - Royal Navy Command: Secondary beneficiary (institutional/constrained) — gains new capabilities and potential long-term cost savings. - Analytical Observer: Sees the full structure, including the temporary/experimental nature of the constraint.

**Beneficiaries**: defense_contractors_leonardo, royal_navy_command, _

**Victims**: manned_helicopter_crews

**Recommendation**: KEEP — Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.27. `solar_system_weirdness` — **KEEP**

**The Solar System Configuration Anomaly** | Domain: technological/scientific

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2000 |
| suppression | 0.6000 |
| theater_ratio | 0.0800 |
| max gradient (g_chi) | -0.2452 |
| max Chi | 0.2740 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.217377 | -0.427816 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.221298 | -0.415168 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.008450 | -1.156290 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.273986 | -0.245206 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0022, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: For decades, astronomers assumed our solar system was a typical model for the universe. Large-scale surveys since the early 2000s have revealed that our neat arrangement of four rocky planets and four gas giants is actually an outlier compared to the more common "super-Earth" or "sub-Neptune" systems found elsewhere. This constraint represents both the physical reality of the solar system's configuration and the conceptual paradigm shift it forced in planetary science. KEY AGENTS (by structural relationship): - Legacy Copernican Models: Primary target (institutional/trapped) — The prior scientific paradigm that was suppressed and invalidated by new data. - Modern Planetary Theorists: Primary beneficiary (analytical/arbitrage) — Researchers who use the "weirdness" as a tool to develop more accurate models of planetary formation. - The Sun/Planets: Physical subjects (powerless/trapped) — The celestial bodies whose configuration is an immutable fact of physics. - Analytical Observer: The modern astronomer seeing the full structure.

**Key agents**: (by structural relationship): - Legacy Copernican Models: Primary target (institutional/trapped) — The prior scientific paradigm that was suppressed and invalidated by new data. - Modern Planetary Theorists: Primary beneficiary (analytical/arbitrage) — Researchers who use the "weirdness" as a tool to develop more accurate models of planetary formation. - The Sun/Planets: Physical subjects (powerless/trapped) — The celestial bodies whose configuration is an immutable fact of physics. - Analytical Observer: The modern astronomer seeing the full structure.

**Beneficiaries**: modern_planetary_theorists

**Victims**: legacy_copernican_models

**Omega questions**:

- Calibrating the statistical degree of the solar system anomaly.
- Distinguishing between survey bias and actual rarity of Earth-twins.

**Recommendation**: KEEP — Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.28. `fmt_oncology_2026` — **INVESTIGATE**

**FMT Immunotherapy Realignment** | Domain: health/medical

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1800 |
| suppression | 0.4500 |
| theater_ratio | 0.1200 |
| max gradient (g_chi) | -0.3336 |
| max Chi | 0.2466 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.195639 | -0.497939 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.199169 | -0.486552 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.007605 | -1.153565 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.246588 | -0.333587 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0001, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

*Prolog spec file not found.*

**Recommendation**: INVESTIGATE — Moderate epsilon (0.18); tangled_rope label may or may not add signal — needs spec review

---

### 6.29. `manganese_catalysis_2026` — **INVESTIGATE**

**Manganese-Formate Fuel Pathway** | Domain: technological/economic

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1800 |
| suppression | 0.2500 |
| theater_ratio | 0.0800 |
| max gradient (g_chi) | -0.3336 |
| max Chi | 0.2466 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.195639 | -0.497939 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.199169 | -0.486552 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.007605 | -1.153565 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.246588 | -0.333587 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: Researchers have identified manganese as a high-efficiency catalyst for converting CO2 into formate, a potential hydrogen carrier for fuel cells. This removes the "Mountain" of precious-metal scarcity (Platinum/Iridium) and establishes a more accessible "Rope" for the global hydrogen economy.

**Key agents**: - Industrial Energy Consumers: Subject (Powerless) - CCU Tech Developers: Beneficiary (Institutional) - Environmental Auditors: Auditor (Analytical)

**Beneficiaries**: clean_energy_infrastructure

**Victims**: precious_metal_cartels

**Recommendation**: INVESTIGATE — Moderate epsilon (0.18); tangled_rope label may or may not add signal — needs spec review

---

### 6.30. `ergo_lets_protocol` — **INVESTIGATE**

**Ergo Local Exchange Trading System (LETS)** | Domain: economic/technological

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1500 |
| suppression | 0.1000 |
| theater_ratio | 0.1500 |
| max gradient (g_chi) | -0.4662 |
| max Chi | 0.2055 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.163033 | -0.603119 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.165974 | -0.593632 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.006338 | -1.149477 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.205490 | -0.466161 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: LETS on the Ergo blockchain is a trustless mutual credit system where the sum of all participant balances is always zero. It allows communities to trade goods and services using a local currency created through "IOUs" backed by collateral or reputation, enforced via smart contracts. The protocol transforms currency from a commodity to be extracted into a public utility for coordination. KEY AGENTS (by structural relationship): - New_Entrant: Primary target (powerless/trapped) — faces a collateral requirement that acts as an unchangeable barrier. - Community_Member: Primary beneficiary (moderate/mobile) — uses the system to facilitate local trade without needing external fiat. - Protocol_Auditor: Analytical observer — sees the mathematical integrity of the zero-sum invariant as a logical necessity. - Over-leveraged_Defaulter: Secondary target (powerless/constrained) — faces coercive collateral seizure upon default.

**Key agents**: (by structural relationship): - New_Entrant: Primary target (powerless/trapped) — faces a collateral requirement that acts as an unchangeable barrier. - Community_Member: Primary beneficiary (moderate/mobile) — uses the system to facilitate local trade without needing external fiat. - Protocol_Auditor: Analytical observer — sees the mathematical integrity of the zero-sum invariant as a logical necessity. - Over-leveraged_Defaulter: Secondary target (powerless/constrained) — faces coercive collateral seizure upon default.

**Beneficiaries**: local_communities, unbanked_users

**Victims**: defaulters

**Omega questions**:

- Whether a technical standard for cross-community reputation can be developed and adopted.

**Recommendation**: INVESTIGATE — Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review

---

### 6.31. `ergo_storage_rent_mechanism` — **INVESTIGATE**

**Ergo Storage Rent (Demurrage)** | Domain: economic/technological

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1500 |
| suppression | 0.8000 |
| theater_ratio | 0.3100 |
| max gradient (g_chi) | -0.4662 |
| max Chi | 0.2055 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.163033 | -0.603119 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.165974 | -0.593632 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.006338 | -1.149477 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.205490 | -0.466161 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.4762, band=genuinely_tangled, signature=false_natural_law, coalition=uniform_tangled

**Spec summary**: Ergo introduces a "Storage Rent" or demurrage fee for inactive data stored on the blockchain. Any UTXO (box) that remains unspent for 4 years is subject to a recurring fee. This prevents "blockchain bloat" and ensures miners have a long-term revenue stream after the emission of new coins ends.

**Key agents**: - Long-term Holders (Individual Powerless): Subject to the fee if inactive. - Miners (Institutional): The beneficiaries who collect the rent. - Protocol Developers (Analytical): Architects of the eUTXO model.

**Beneficiaries**: ergo_miners

**Victims**: inactive_wallet_addresses

**Recommendation**: INVESTIGATE — Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review

---

### 6.32. `narrative_engineering_2026` — **INVESTIGATE**

**The Narrative Engineering Stabilization Signal** | Domain: technological/social

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1500 |
| suppression | 0.4500 |
| theater_ratio | 0.0500 |
| max gradient (g_chi) | -0.4662 |
| max Chi | 0.2055 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.163033 | -0.603119 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.165974 | -0.593632 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.006338 | -1.149477 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.205490 | -0.466161 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: In a post-truth environment characterized by AI-driven manipulation, Narrative Engineering (Constraint-Based Storytelling) serves as a "stabilizing signal." By adhering to rigorous structural constraints, it counters the sensationalist "noise" favored by previous algorithmic eras, aligning with the February 2026 Google Discover Core Update.

**Key agents**: - General Information Consumers: Subject (Powerless) - Narrative Engineers/Architects: Beneficiary (Institutional) - Algorithmic Auditors (Google/Platforms): Auditor (Analytical)

**Beneficiaries**: epistemic_communities

**Victims**: sensationalist_media_outlets

**Recommendation**: INVESTIGATE — Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review

---

### 6.33. `rogue_wave_control_2026` — **INVESTIGATE**

**Rogue Wave Control in VCSEL Lasers (2026)** | Domain: ---

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1500 |
| suppression | 0.1000 |
| theater_ratio | 0.0500 |
| max gradient (g_chi) | -0.4662 |
| max Chi | 0.2055 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.163033 | -0.603119 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.165974 | -0.593632 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.006338 | -1.149477 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.205490 | -0.466161 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: constraint_id: rogue_wave_control_2026 human_readable: Rogue Wave Control in VCSEL Lasers (2026) Researchers have achieved deterministic control over chaotic "rogue waves" in VCSEL lasers using a $\lambda/2$-waveplate. This turns a physical "Mountain" of chaos into a "Rope" of engineered signaling.

**Beneficiaries**: photonics_researchers

**Victims**: none

**Recommendation**: INVESTIGATE — Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review

---

### 6.34. `coffee_cardiovascular_2026` — **INVESTIGATE**

**The Caffeine Paradox Realignment** | Domain: medical/health

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1200 |
| suppression | 0.4000 |
| theater_ratio | 0.1000 |
| max gradient (g_chi) | -0.5987 |
| max Chi | 0.1644 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.130426 | -0.708303 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.132779 | -0.700713 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.005070 | -1.145387 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.164392 | -0.598735 |

**Diagnostics**: low-epsilon=YES, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: For years, cardiologists cautioned against coffee for patients with atrial fibrillation (AF). New randomized trial data (Feb 2026) reveals that daily coffee reduces AF recurrence by 17% and improves metabolic activity. This transforms coffee from a "Snare" of jitter-inducing risk into a "Rope" of protective coordination for heart health and gut microbiome diversity.

**Key agents**: - AF Patients/Coffee Drinkers: Subject (Powerless against previous medical dogma) - Cardiologists (Adelaide Study): Beneficiary (Institutional - New Knowledge) - British Heart Foundation: Auditor (Analytical)

**Beneficiaries**: public_health_outcomes

**Victims**: medical_precautionary_dogma

**Recommendation**: INVESTIGATE — Moderate epsilon (0.12); tangled_rope label may or may not add signal — needs spec review

---

### 6.35. `kidney_exchange_market` — **INVESTIGATE**

**Kidney Exchange Cycles/Chains** | Domain: social/technological/biological

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1000 |
| suppression | 0.9000 |
| theater_ratio | 0.1500 |
| max gradient (g_chi) | -0.6871 |
| max Chi | 0.1370 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.108689 | -0.778423 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.110649 | -0.772100 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.004225 | -1.142661 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.136993 | -0.687119 |

**Diagnostics**: low-epsilon=YES, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.4762, band=genuinely_tangled, signature=false_natural_law, coalition=uniform_tangled

**Spec summary**: Kidney exchange solves the "incompatible donor" problem. When a willing donor cannot give to their intended recipient due to blood/tissue type, they enter a swap. This creates a matching market where "Price" is illegal, necessitating algorithmic coordination to save lives.

**Key agents**: - The Sensitized Patient (Individual Powerless): Needs a kidney but is hard to match. - The Algorithm / UNOS (Institutional): Computes optimal cycles and chains. - The Altruistic Donor (Individual Moderate): Starts a chain by donating a kidney.

**Beneficiaries**: organ_recipients

**Victims**: sensitized_patients

**Recommendation**: INVESTIGATE — Moderate epsilon (0.10); tangled_rope label may or may not add signal — needs spec review

---

### 6.36. `udhr_1948` — **INVESTIGATE**

**Universal Declaration of Human Rights (1948)** | Domain: political/legal

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1000 |
| suppression | 0.2000 |
| theater_ratio | 0.1600 |
| max gradient (g_chi) | -0.6871 |
| max Chi | 0.1370 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.108689 | -0.778423 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.110649 | -0.772100 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.004225 | -1.142661 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.136993 | -0.687119 |

**Diagnostics**: low-epsilon=YES, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=other

*Prolog spec file not found.*

**Recommendation**: INVESTIGATE — Moderate epsilon (0.10); tangled_rope label may or may not add signal — needs spec review

---

### 6.37. `e2ee_digital_privacy_2026` — **RECLASSIFY**

**End-to-End Encryption (E2EE)** | Domain: technological/political/social

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.0500 |
| suppression | 0.5000 |
| theater_ratio | 0.0900 |
| max gradient (g_chi) | -0.9081 |
| max Chi | 0.0685 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.054344 | -0.953729 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.055325 | -0.950565 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.002113 | -1.135848 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.068497 | -0.908074 |

**Diagnostics**: low-epsilon=YES, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.4762, band=genuinely_tangled, signature=false_natural_law, coalition=uniform_tangled

**Spec summary**: End-to-end encryption (E2EE) is a digital "wall" that ensures messages are only readable by the sender and recipient, rendering them "meaningless gibberish" during transmission. It provides a technological safeguard for privacy by relying on "immutable mathematics" rather than the promises of service providers.

**Key agents**: - The Vulnerable Individual (Individual Powerless): Relies on E2EE for physical safety. - The Surveillance State (Institutional): A spy agency or police force unable to access communications. - The Mathematician (Analytical): Observer of the underlying mathematical laws.

**Beneficiaries**: individual_users

**Victims**: institutional_surveillance

**Recommendation**: RECLASSIFY — Trivially low epsilon (0.05); rope-dominant is obviously correct

---

### 6.38. `platform_cooperativism_governance` — **RECLASSIFY**

**Democratic Worker Governance in Platform Cooperativism** | Domain: economic/social/technological

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.0500 |
| suppression | 0.2000 |
| theater_ratio | 0.1200 |
| max gradient (g_chi) | -0.9081 |
| max Chi | 0.0685 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=tangled_rope, institutional=tangled_rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.054344 | -0.953729 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.055325 | -0.950565 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.002113 | -1.135848 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.068497 | -0.908074 |

**Diagnostics**: low-epsilon=YES, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.4762, band=genuinely_tangled, signature=false_natural_law, coalition=uniform_tangled

**Spec summary**: Platform Cooperativism is a model where a digital platform is owned and governed by its workers. The core constraint is a "democratic mandate" that requires profits to be distributed to members and algorithms to be accountable to the collective.

**Key agents**: - The Worker-Owner (Individual Moderate): Possesses voting rights and a share of the surplus. - The VC-Backed Competitor (Institutional): Views the coop's lack of "blitzscaling" as a weakness. - The Non-Member Gig Worker (Individual Powerless): Works for a competitor like Uber, sees the coop as an inaccessible alternative.

**Beneficiaries**: worker_members

**Victims**: external_capital_investors

**Recommendation**: RECLASSIFY — Trivially low epsilon (0.05); rope-dominant is obviously correct

---

### 6.39. `legacy_system_technical_debt` — **RECLASSIFY**

**Cumulative Technical Debt in Legacy Monoliths** | Domain: technological/economic

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.0300 |
| suppression | 0.0400 |
| theater_ratio | 0.8500 |
| max gradient (g_chi) | -0.9965 |
| max Chi | 0.0411 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | no |

**Perspective type labels**: powerless=rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.032607 | -1.023848 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.033195 | -1.021952 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.001268 | -1.133123 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.041098 | -0.996458 |

**Diagnostics**: low-epsilon=YES, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=constructed_low_extraction, coalition=other

**Spec summary**: Technical debt is the implied cost of future refactoring caused by choosing an easy, limited solution now instead of a better approach. In legacy systems, this debt ossifies into a constraint that limits innovation. What begins as a strategic shortcut (Rope) becomes an unmovable reality (Mountain) for new hires, and eventually an inertial burden (Piton) that consumes resources via performative maintenance.

**Key agents**: - Junior Developer: Subject (Powerless), navigating a brittle codebase. - CTO/VPE: Beneficiary (Institutional), balancing velocity vs. stability. - System Architect/Auditor: Auditor (Analytical), identifying structural decay.

**Beneficiaries**: short_term_profit_margins

**Victims**: engineering_morale, long_term_viability

**Recommendation**: RECLASSIFY — Trivially low epsilon (0.03); rope-dominant is obviously correct

---

### 6.40. `public_domain_commons` — **RECLASSIFY**

**The Public Domain as a Cultural Commons** | Domain: legal/economic/social

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.0000 |
| suppression | 0.3000 |
| theater_ratio | 0.0800 |
| max gradient (g_chi) | -1.1290 |
| max Chi | 0.0000 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | no |

**Perspective type labels**: powerless=rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9500 | 1.392945 | 0.8 | 0.000000 | -1.129032 |
| moderate | 0.6500 | 1.008614 | 1.0 | 0.000000 | -1.129032 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.000000 | -1.129032 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.000000 | -1.129032 |

**Diagnostics**: low-epsilon=YES, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4352)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=constructed_low_extraction, coalition=other

**Spec summary**: The Public Domain consists of all creative work to which no exclusive intellectual property rights apply. These works are free for anyone to use, remix, and build upon. It represents the default state of information before or after the artificial constraint of Copyright is applied. This constraint is the existence and defense of that commons. KEY AGENTS (by structural relationship): - Commons Participants (students, artists, developers): Primary beneficiary (powerless/moderate/mobile) — uses the commons for education, creation, and innovation. - Legacy Rights-Holders (large media corporations): Institutional actor (institutional/constrained) — views the commons as a boundary condition that limits their ability to extract rent from intellectual property. - Analytical Observer: Sees the full structure as a pure coordination mechanism.

**Key agents**: (by structural relationship): - Commons Participants (students, artists, developers): Primary beneficiary (powerless/moderate/mobile) — uses the commons for education, creation, and innovation. - Legacy Rights-Holders (large media corporations): Institutional actor (institutional/constrained) — views the commons as a boundary condition that limits their ability to extract rent from intellectual property. - Analytical Observer: Sees the full structure as a pure coordination mechanism.

**Beneficiaries**: commons_participants

**Recommendation**: RECLASSIFY — Trivially low epsilon (0.00); rope-dominant is obviously correct

---

