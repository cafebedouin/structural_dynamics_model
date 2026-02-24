# Rope-Dominant Spot Check

*Generated 2026-02-24 02:30 by `python/rope_dominant_spot_check.py`*

---

## 1. Executive Summary

Analyzed **28** rope-dominant tangled_rope constraints (max g_chi < 0.3 across all 4 perspectives).

### Diagnostic Counts

| Diagnostic | Count | % |
| :--- | ---: | ---: |
| Low-epsilon trivial (eps < 0.15) | 5 | 17.9% |
| Sigmoid-compressed (all f(d) < 0.5) | 0 | 0.0% |
| Low f(d) spread (< 0.1) | 0 | 0.0% |
| Chi override overlap | 0 | 0.0% |
| Perspective divergent (>1 type label) | 16 | 57.1% |
| Has tangled_rope/snare label | 21 | 75.0% |

### Recommendation Tiers

| Tier | Count | % |
| :--- | ---: | ---: |
| Keep | 17 | 60.7% |
| Investigate | 7 | 25.0% |
| Reclassify | 4 | 14.3% |

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

- **f(d) spread** = 1.400858 for all 28 (structural invariant)
- **Sigmoid-compressed count** = 0 (powerless f(d) = 1.358606 > 0.5)

## 3. Population Statistics

### 3.1 Epsilon Distribution

| Stat | Value |
| :--- | ---: |
| n | 28 |
| mean | 0.2111 |
| median | 0.2000 |
| std | 0.0929 |
| min | 0.0200 |
| max | 0.3200 |
| q25 | 0.1500 |
| q75 | 0.3000 |

| Range | Count |  |
| :--- | ---: | :--- |
| 0.00–0.09 | 4 | ████ |
| 0.10–0.14 | 1 | █ |
| 0.15–0.19 | 5 | █████ |
| 0.20–0.24 | 5 | █████ |
| 0.25–0.29 | 2 | ██ |
| 0.30–0.35 | 11 | ███████████ |

### 3.2 Domain Distribution

| Domain | Count | % |
| :--- | ---: | ---: |
| economic/technological | 2 | 7.1% |
| technological/institutional | 2 | 7.1% |
| economic | 1 | 3.6% |
| economic/political | 1 | 3.6% |
| political/environmental | 1 | 3.6% |
| investigation/testing | 1 | 3.6% |
| mathematical/logical | 1 | 3.6% |
| mathematical/philosophical | 1 | 3.6% |
| economic/social | 1 | 3.6% |
| technological/economic | 1 | 3.6% |
| social/ethical | 1 | 3.6% |
| social/psychological | 1 | 3.6% |
| political | 1 | 3.6% |
| social | 1 | 3.6% |
| religious/philosophical/social | 1 | 3.6% |
| social/intellectual | 1 | 3.6% |
| technological/mathematical | 1 | 3.6% |
| physics/economics/mathematics | 1 | 3.6% |
| technological/economic/environmental | 1 | 3.6% |
| social/political/health | 1 | 3.6% |
| ecological/economic/social | 1 | 3.6% |
| technological/social | 1 | 3.6% |
| political/social/legal | 1 | 3.6% |
| political/legal | 1 | 3.6% |

### 3.3 Perspective Type Labels

- **Divergent** (>1 unique type across 4 perspectives): **16** / 28

- **Uniform** (all 4 perspectives agree): **12** / 28

- **Has tangled_rope or snare label**: **21** / 28

### 3.4 Chi Override Overlap

**0** of the 28 rope-dominant constraints overlap with the 19 chi override set.


## 4. Recommendations by Tier

### 4.1 Reclassify (4 constraints)

These constraints have epsilon < 0.10.  Extraction potential is trivially low; rope-dominant is obviously correct.  Recommend reclassifying from tangled_rope to rope.

| Constraint | ε | Domain | Perspective Types | Reason |
| :--- | ---: | :--- | :--- | :--- |
| `decentralized_infrastructure_rope` | 0.08 | technological/social | rope | Trivially low epsilon (0.08); rope-dominant is obviously correct |
| `sts86_ascent_checklist` | 0.05 | technological/institutional | rope | Trivially low epsilon (0.05); rope-dominant is obviously correct |
| `thai_article_112_mountain` | 0.04 | political/social/legal | rope | Trivially low epsilon (0.04); rope-dominant is obviously correct |
| `portuguese_presidential_term_limits` | 0.02 | political/legal | rope | Trivially low epsilon (0.02); rope-dominant is obviously correct |

### 4.2 Investigate (7 constraints)

These constraints have moderate epsilon (0.10–0.19) or ambiguous perspective labels.  The tangled_rope label may or may not add signal.  Human review of the Prolog spec is recommended.

| Constraint | ε | Domain | Perspective Types | Reason |
| :--- | ---: | :--- | :--- | :--- |
| `quine_self_replication` | 0.20 | technological/mathematical | mountain, rope | Epsilon (0.20) is meaningful but no perspective sees tangled_rope/snare — possible mislabel |
| `boltzmann_universality_2026` | 0.15 | physics/economics/mathematics | rope, tangled_rope | Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review |
| `electrification_scale_2026` | 0.15 | technological/economic/environmental | rope, tangled_rope | Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review |
| `ergo_storage_rent_mechanism` | 0.15 | economic/technological | tangled_rope | Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review |
| `rogue_wave_control_2026` | 0.15 | --- | tangled_rope | Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review |
| `viral_emergence_covid19_exemplar` | 0.15 | social/political/health | piton, rope | Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review |
| `planetary_diet_constraint_2026` | 0.10 | ecological/economic/social | rope | Moderate epsilon (0.10); tangled_rope label may or may not add signal — needs spec review |

### 4.3 Keep (17 constraints)

These constraints have epsilon >= 0.20 and at least one perspective labels them tangled_rope or snare.  The classification appears to be doing structural work, though qualitative confirmation from spec review is still valuable.

| Constraint | ε | Domain | Perspective Types | Reason |
| :--- | ---: | :--- | :--- | :--- |
| `china_africa_zero_tariff_2026` | 0.32 | economic | rope, tangled_rope | Meaningful epsilon (0.32) with tangled_rope/snare perspective labels; classification is doing structural work |
| `oc_donation_model` | 0.32 | --- | rope, tangled_rope | Meaningful epsilon (0.32) with tangled_rope/snare perspective labels; classification is doing structural work |
| `blackstone_carried_interest_taxation` | 0.30 | economic/political | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `climate_target_one_point_five` | 0.30 | political/environmental | tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `fnl_shadow_probe` | 0.30 | investigation/testing | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `inner_model_theory_constraints` | 0.30 | mathematical/logical | tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `large_cardinal_foundations` | 0.30 | mathematical/philosophical | tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `migration_decision_threshold` | 0.30 | economic/social | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `moores_law` | 0.30 | technological/economic | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `sadhu_integrity_protocol` | 0.30 | social/ethical | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `social_narrative_casting` | 0.30 | social/psychological | rope, tangled_rope | Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work |
| `emergency_oversight_bureau` | 0.28 | political | tangled_rope | Meaningful epsilon (0.28) with tangled_rope/snare perspective labels; classification is doing structural work |
| `guinea_worm_eradication` | 0.25 | social | rope, tangled_rope | Meaningful epsilon (0.25) with tangled_rope/snare perspective labels; classification is doing structural work |
| `dexy_gold_protocol` | 0.20 | economic/technological | rope, tangled_rope | Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work |
| `gita_kurukshetra` | 0.20 | religious/philosophical/social | tangled_rope | Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work |
| `lindy_effect` | 0.20 | social/intellectual | rope, tangled_rope | Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work |
| `rfc9293_state_machine` | 0.20 | technological/institutional | rope, tangled_rope | Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work |

## 5. Calibration Implications

- **4** constraints recommended for reclassification to plain rope (epsilon < 0.1)

- **7** constraints need human review (epsilon 0.1–0.2)

- **17** constraints appear correctly labeled (epsilon >= 0.2 with extraction signals)


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

**Cross-refs**: tangled_psi=0.0179, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: China implements zero-tariff treatment for 53 African nations to expand market access. While nominally a coordination mechanism (Rope) for trade, the inclusion of "green channels" and "joint economic partnership pacts" introduces structural extraction through standard-setting and diplomatic alignment. KEY AGENTS (by structural relationship): - smallholder_african_farmers: Primary target (powerless/trapped) — bears highest compliance costs, lacks capital to meet standards. - organized_african_exporters: Secondary target (moderate/constrained) — can meet standards but at a significant cost. - china_state_actors: Primary beneficiary (institutional/arbitrage) — gains geopolitical alignment and resource security. - focac_administrators: Analytical observer — monitors the Forum on China-Africa Cooperation (FOCAC) outcomes.

**Key agents**: (by structural relationship): - smallholder_african_farmers: Primary target (powerless/trapped) — bears highest compliance costs, lacks capital to meet standards. - organized_african_exporters: Secondary target (moderate/constrained) — can meet standards but at a significant cost. - china_state_actors: Primary beneficiary (institutional/arbitrage) — gains geopolitical alignment and resource security. - focac_administrators: Analytical observer — monitors the Forum on China-Africa Cooperation (FOCAC) outcomes.

**Beneficiaries**: china_state_actors, _

**Victims**: african_producers_and_exporters, _

**Omega questions**:

- Non-tariff barrier impact of green channel standards

**Recommendation**: KEEP — Meaningful epsilon (0.32) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.2. `oc_donation_model` — **KEEP**

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

**Cross-refs**: tangled_psi=0.0155, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: Open Culture is a web-based cultural and educational aggregator that provides free access to a vast repository of high-quality content. The organization is sustained by a voluntary donation model, where a small fraction of users provide the financial support for the entire operation. This constraint is the funding mechanism itself: a system for solving a public goods problem that relies on asymmetric, non-coercive extraction. KEY AGENTS (by structural relationship): - Donors: Primary target (moderate/mobile) — The small subset of users who bear the financial cost of the service for everyone. - Open Culture Editors: Primary beneficiary (institutional/arbitrage) — The organization that uses the funds to operate and fulfill its mission. - Global Learners: Secondary beneficiary (powerless/mobile) — The vast majority of users who access the content for free. - Analytical Observer: Sees the full structure of coordination and asymmetric cost-bearing.

**Key agents**: (by structural relationship): - Donors: Primary target (moderate/mobile) — The small subset of users who bear the financial cost of the service for everyone. - Open Culture Editors: Primary beneficiary (institutional/arbitrage) — The organization that uses the funds to operate and fulfill its mission. - Global Learners: Secondary beneficiary (powerless/mobile) — The vast majority of users who access the content for free. - Analytical Observer: Sees the full structure of coordination and asymmetric cost-bearing.

**Beneficiaries**: open_culture_editors, global_learners, _

**Victims**: donors, _

**Recommendation**: KEEP — Meaningful epsilon (0.32) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.3. `blackstone_carried_interest_taxation` — **KEEP**

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

**Cross-refs**: tangled_psi=0.7776, band=snare_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: The regulatory and tax framework in the United States that treats "carried interest" (performance fees) for partners in private equity and hedge funds as long-term capital gains rather than ordinary income. This results in a significantly lower tax rate. The constraint's existence relies on active lobbying and complex legal structuring to defend its status against legislative challenges that seek to reclassify it as income. KEY AGENTS (by structural relationship): - us_taxpayers: Primary target (powerless/trapped) — bear the cost via reduced public tax revenue, with no direct recourse. - private_equity_partners: Primary beneficiary (institutional/arbitrage) — benefit from lower tax rates on their primary form of compensation. - reformist_legislators: Secondary institutional actor (institutional/constrained) — attempt to change the rule but face high political and structural barriers. - analytical_observer: Analytical observer — sees the dual function of coordination (for partners) and extraction (from the tax base).

**Key agents**: (by structural relationship): - us_taxpayers: Primary target (powerless/trapped) — bear the cost via reduced public tax revenue, with no direct recourse. - private_equity_partners: Primary beneficiary (institutional/arbitrage) — benefit from lower tax rates on their primary form of compensation. - reformist_legislators: Secondary institutional actor (institutional/constrained) — attempt to change the rule but face high political and structural barriers. - analytical_observer: Analytical observer — sees the dual function of coordination (for partners) and extraction (from the tax base).

**Beneficiaries**: private_equity_partners

**Victims**: us_taxpayers

**Omega questions**:

- Whether the 

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.4. `climate_target_one_point_five` — **KEEP**

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

**Cross-refs**: tangled_psi=0.4762, band=genuinely_tangled, signature=false_natural_law, coalition=uniform_tangled

**Spec summary**: The 1.5°C target is a global policy constraint that lowered the "safe" warming threshold from 2°C. Championed by the Alliance of Small Island States (AOSIS), it redefines acceptable climate risk based on the survival of the most vulnerable nations rather than the economic convenience of larger powers. It functions as both a coordination mechanism for global climate action and an extractive limit on high-emission development paths. KEY AGENTS (by structural relationship): - Small Island States (AOSIS): Primary beneficiary (organized/trapped) — uses the target as a survival mechanism. - Fossil Fuel Reliant Economies: Primary target (institutional/constrained) — bears the cost of constrained development. - Citizen in a Coastal Community: Secondary target (powerless/trapped) — experiences the physical reality of climate change, for whom the policy target is largely abstract. - Analytical Observer: Sees the dual coordination/extraction function.

**Key agents**: (by structural relationship): - Small Island States (AOSIS): Primary beneficiary (organized/trapped) — uses the target as a survival mechanism. - Fossil Fuel Reliant Economies: Primary target (institutional/constrained) — bears the cost of constrained development. - Citizen in a Coastal Community: Secondary target (powerless/trapped) — experiences the physical reality of climate change, for whom the policy target is largely abstract. - Analytical Observer: Sees the dual coordination/extraction function.

**Beneficiaries**: small_island_states, _

**Victims**: fossil_fuel_reliant_economies, _

**Omega questions**:

- Physical attainability of the 1.5C target given current global inertia.

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.5. `fnl_shadow_probe` — **KEEP**

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

**Cross-refs**: tangled_psi=0.0347, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: A synthetic constraint designed to model a constructed system that could be mistaken for a natural law (a False Natural Law, or FNL). It combines a genuine coordination function with asymmetric extraction, enforced by an active apparatus. Its high suppression score and requirement for active enforcement are structural giveaways that it is constructed, not natural. From the beneficiary's perspective, it appears as a low-cost coordination mechanism (Rope), while for its targets, it is an extractive Tangled Rope. KEY AGENTS (by structural relationship): - constrained_subjects: Primary target (powerless/trapped) — bears the costs of the system. - institutional_apparatus: Primary beneficiary (institutional/arbitrage) — benefits from the coordination and extraction. - Analytical observer: Sees the full structure, including both the coordination function and the asymmetric extraction.

**Key agents**: (by structural relationship): - constrained_subjects: Primary target (powerless/trapped) — bears the costs of the system. - institutional_apparatus: Primary beneficiary (institutional/arbitrage) — benefits from the coordination and extraction. - Analytical observer: Sees the full structure, including both the coordination function and the asymmetric extraction.

**Beneficiaries**: institutional_apparatus, _

**Victims**: constrained_subjects, _

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.6. `inner_model_theory_constraints` — **KEEP**

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

**Cross-refs**: tangled_psi=0.4762, band=genuinely_tangled, signature=false_natural_law, coalition=uniform_tangled

**Spec summary**: Inner Model Theory studies sub-universes of the set-theoretic universe (V) that satisfy the axioms of ZFC. The archetypal inner model is Gödel's Constructible Universe (L), defined by the axiom V=L. This axiom imposes a rigid, determinate structure on the transfinite, resolving questions like the Continuum Hypothesis. However, this determinacy comes at the cost of excluding more complex structures like large cardinals, creating a significant perspectival gap between different schools of set theorists. KEY AGENTS (by structural relationship): - Large Cardinal Pluralists (Victim): Set theorists who view V=L as a snare that extracts the potential richness of the set-theoretic universe (V) to enforce a narrow, predictable structure. (moderate/constrained) - Consistency Proof Developers (Beneficiary): Logicians and model theorists who use inner models like L as a coordination tool (a rope) to establish the relative consistency of various mathematical axioms. (institutional/mobile) - The Constructible Set (Subject): A mathematical object whose existence is rigidly defined by the L-hierarchy, for which the constraint is an unchangeable law of its nature. (powerless/trapped) - Analytical Observer: Sees the full structure as a Tangled Rope, acknowledging both its genuine coordination function for consistency proofs and its extractive nature in suppressing alternative set-theoretic ontologies.

**Key agents**: (by structural relationship): - Large Cardinal Pluralists (Victim): Set theorists who view V=L as a snare that extracts the potential richness of the set-theoretic universe (V) to enforce a narrow, predictable structure. (moderate/constrained) - Consistency Proof Developers (Beneficiary): Logicians and model theorists who use inner models like L as a coordination tool (a rope) to establish the relative consistency of various mathematical axioms. (institutional/mobile) - The Constructible Set (Subject): A mathematical object whose existence is rigidly defined by the L-hierarchy, for which the constraint is an unchangeable law of its nature. (powerless/trapped) - Analytical Observer: Sees the full structure as a Tangled Rope, acknowledging both its genuine coordination function for consistency proofs and its extractive nature in suppressing alternative set-theoretic ontologies.

**Beneficiaries**: consistency_proof_developers, fine_structure_theorists

**Victims**: large_cardinal_pluralists

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.7. `large_cardinal_foundations` — **KEEP**

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

**Cross-refs**: tangled_psi=0.4762, band=genuinely_tangled, signature=false_natural_law, coalition=uniform_tangled

*Prolog spec file not found.*

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.8. `migration_decision_threshold` — **KEEP**

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

**Cross-refs**: tangled_psi=0.0007, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

*Prolog spec file not found.*

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.9. `moores_law` — **KEEP**

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

**Cross-refs**: tangled_psi=0.0313, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: Moore's Law is the observation that the number of transistors on a microchip doubles approximately every two years. This story models the law not as a physical inevitability, but as a self-fulfilling prophecy or a "socially enforced" pace of innovation that coordinates the global semiconductor industry while simultaneously creating a coercive R&D treadmill and planned obsolescence. KEY AGENTS (by structural relationship): - Chip Fabricators (e.g., Intel, TSMC): Primary target (institutional/constrained) — bears the immense R&D cost to maintain the pace. - Platform Capitalists & Software Developers: Primary beneficiary (institutional/arbitrage) — benefits from predictable hardware gains to build more complex services. - Consumers / Legacy Infrastructure Owners: Secondary target (powerless/mobile) — benefits from cheaper compute but is subject to planned obsolescence. - Analytical Observer: Sees the full structure as a Tangled Rope of coordination and extraction.

**Key agents**: (by structural relationship): - Chip Fabricators (e.g., Intel, TSMC): Primary target (institutional/constrained) — bears the immense R&D cost to maintain the pace. - Platform Capitalists & Software Developers: Primary beneficiary (institutional/arbitrage) — benefits from predictable hardware gains to build more complex services. - Consumers / Legacy Infrastructure Owners: Secondary target (powerless/mobile) — benefits from cheaper compute but is subject to planned obsolescence. - Analytical Observer: Sees the full structure as a Tangled Rope of coordination and extraction.

**Beneficiaries**: platform_capitalists, software_developers, _

**Victims**: semiconductor_fabricators, legacy_infrastructure_owners, _

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.10. `sadhu_integrity_protocol` — **KEEP**

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

**Cross-refs**: tangled_psi=0.0007, band=rope_leaning, signature=false_ci_rope, coalition=institutional_dissent

**Spec summary**: This constraint mandates that an advisor must embody their own counsel before delivering it to others. In the source narrative, a Sadhu (holy man) delays giving advice to a boy to stop eating sugar for two weeks because the Sadhu was still consuming sugar himself. The constraint posits that effective social coordination (advice) requires internal synchronization (integrity), extracting time from those seeking immediate solutions to ensure efficacy. KEY AGENTS (by structural relationship): - The Mother & Son (seekers_of_immediate_remedies): Primary targets (powerless/trapped) — bear the cost of the two-week delay. - The Sadhu (upholders_of_tradition): Primary beneficiary (institutional/mobile) — benefits from enhanced authority and social efficacy. - The Community (implicit): Secondary beneficiary — benefits from a system of trustworthy counsel. - Analytical Observer: Sees the full structure of coordination and extraction.

**Key agents**: (by structural relationship): - The Mother & Son (seekers_of_immediate_remedies): Primary targets (powerless/trapped) — bear the cost of the two-week delay. - The Sadhu (upholders_of_tradition): Primary beneficiary (institutional/mobile) — benefits from enhanced authority and social efficacy. - The Community (implicit): Secondary beneficiary — benefits from a system of trustworthy counsel. - Analytical Observer: Sees the full structure of coordination and extraction.

**Beneficiaries**: upholders_of_tradition

**Victims**: seekers_of_immediate_remedies

**Omega questions**:

- Causality of advisor

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.11. `social_narrative_casting` — **KEEP**

**Social Narrative Casting (Criticism-as-Projection)** | Domain: social/psychological

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.3000 |
| suppression | 0.6000 |
| theater_ratio | 0.0600 |
| max gradient (g_chi) | 0.1967 |
| max Chi | 0.4110 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=rope, moderate=rope, institutional=rope, analytical=tangled_rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.326066 | -0.077206 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.331948 | -0.058232 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.012676 | -1.169923 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.410979 | 0.196706 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.2014, band=rope_leaning, signature=false_ci_rope, coalition=analytical_dissent

**Spec summary**: This constraint models the act of criticism as an attempt by a critic (the "Director") to "hire" the subject into a specific role (villain, victim, obstacle) within the critic's internal narrative. The subject must choose whether to internalize this casting (a Snare) or consciously play the role to learn from it (a Rope). The underlying mechanism is the ego's tendency to reduce complex reality into simple, self-serving stories. KEY AGENTS (by structural relationship): - The Subject (Actor): Primary target (powerless/trapped) — bears the extraction of having their identity constrained by another's narrative. - The Critic (Director): Primary beneficiary (organized/mobile) — benefits by reinforcing their ego and worldview through the narrative casting of others. - The Therapist/Coach: Secondary beneficiary (institutional/arbitrage) — uses the understanding of this dynamic as a tool to help others. - The Analytical Observer: Sees the full structure of ego-driven narrative projection as a system.

**Key agents**: (by structural relationship): - The Subject (Actor): Primary target (powerless/trapped) — bears the extraction of having their identity constrained by another's narrative. - The Critic (Director): Primary beneficiary (organized/mobile) — benefits by reinforcing their ego and worldview through the narrative casting of others. - The Therapist/Coach: Secondary beneficiary (institutional/arbitrage) — uses the understanding of this dynamic as a tool to help others. - The Analytical Observer: Sees the full structure of ego-driven narrative projection as a system.

**Beneficiaries**: critics_ego

**Victims**: criticized_subject

**Omega questions**:

- Is criticism always projection, or can it be objective feedback?

**Recommendation**: KEEP — Meaningful epsilon (0.30) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.12. `emergency_oversight_bureau` — **KEEP**

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

**Cross-refs**: tangled_psi=0.6723, band=genuinely_tangled, signature=false_ci_rope, coalition=uniform_tangled

**Spec summary**: A temporary administrative body created to manage a specific recovery period. While it exerts high control (suppression), it includes a mandatory expiration date to prevent it from hardening into a permanent Snare. Its extraction is moderate, representing compliance costs rather than direct rent-seeking.

**Key agents**: - The Citizen: Subject (Powerless) - Undergoing temporary mandatory vetting. - The Coordinator: Architect (Organized) - Managing the sunset transition. - The Historian: Auditor (Analytical) - Monitoring for "Scaffold-to-Piton" drift.

**Beneficiaries**: crisis_affected_populations

**Victims**: citizens_under_vetting

**Recommendation**: KEEP — Meaningful epsilon (0.28) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.13. `guinea_worm_eradication` — **KEEP**

**Global Guinea Worm Eradication Program** | Domain: social

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2500 |
| suppression | 0.3000 |
| theater_ratio | 0.1000 |
| max gradient (g_chi) | -0.0242 |
| max Chi | 0.3425 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.271721 | -0.252513 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.276623 | -0.236700 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.010563 | -1.163106 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.342483 | -0.024248 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=other

**Spec summary**: The Global Guinea Worm Eradication Program, led by The Carter Center, aims to eradicate Guinea worm disease through providing safe drinking water sources and health education, reducing human suffering in endemic regions. The program serves as a coordination mechanism, but also requires some level of enforcement by restricting access to unsafe water sources.  The program is nearing completion but remains an active effort.

**Key agents**: - Infected Individuals: Subject (Powerless) - The Carter Center: Beneficiary (Institutional) - Analytical Observer: Auditor (Analytical)

**Beneficiaries**: endemic_communities

**Victims**: endemic_communities

**Recommendation**: KEEP — Meaningful epsilon (0.25) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.14. `dexy_gold_protocol` — **KEEP**

**DexyGold Seigniorage Mechanism** | Domain: economic/technological

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2000 |
| suppression | 0.2000 |
| theater_ratio | 0.1100 |
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

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=other

*Prolog spec file not found.*

**Recommendation**: KEEP — Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.15. `gita_kurukshetra` — **KEEP**

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

**Spec summary**: The "dharma" or sacred duty of the warrior caste (Kshatriya) to fight in a lawful war, even against kin. It presents a powerful moral obligation rooted in a specific metaphysical worldview and social order, as articulated by Krishna to Arjuna on the battlefield of Kurukshetra. The constraint is the social and ideological pressure to conform to one's caste duty. KEY AGENTS (by structural relationship): - Arjuna: Primary target (powerless/trapped) — bears the moral and psychological cost of the duty. - The Cosmic Order (as personified by Krishna): Primary beneficiary (institutional/arbitrage) — the social and metaphysical system is upheld by adherence to dharma. - The Enlightened Sage: Analytical observer — perceives the system's logic from a detached viewpoint.

**Key agents**: (by structural relationship): - Arjuna: Primary target (powerless/trapped) — bears the moral and psychological cost of the duty. - The Cosmic Order (as personified by Krishna): Primary beneficiary (institutional/arbitrage) — the social and metaphysical system is upheld by adherence to dharma. - The Enlightened Sage: Analytical observer — perceives the system's logic from a detached viewpoint.

**Beneficiaries**: cosmic_order

**Victims**: individual_ego

**Omega questions**:

- Is selfless action universally applicable or dependent on a specific metaphysical frame?

**Recommendation**: KEEP — Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.16. `lindy_effect` — **KEEP**

**The Lindy Effect** | Domain: social/intellectual

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2000 |
| suppression | 0.5000 |
| theater_ratio | 0.1100 |
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

**Cross-refs**: tangled_psi=0.0002, band=rope_leaning, signature=false_ci_rope, coalition=other

**Spec summary**: The Lindy Effect is a theorized phenomenon where the future life expectancy of non-perishable things (ideas, books, technologies) is proportional to their current age. It functions as a powerful heuristic for filtering information, but also creates a structural bias against novelty. This dual nature—a genuine coordination function (filtering) combined with asymmetric suppression of new entrants—makes it a canonical Tangled Rope. KEY AGENTS (by structural relationship): - Disruptive Innovators: Primary target (powerless/constrained) — bears the cost of being new. - Established Institutions: Primary beneficiary (institutional/arbitrage) — leverages its age as a signal of quality. - Pragmatic Investors: Secondary beneficiary (moderate/mobile) — uses the effect as a coordination heuristic. - Analytical Observer: Sees the full structure as a Tangled Rope.

**Key agents**: (by structural relationship): - Disruptive Innovators: Primary target (powerless/constrained) — bears the cost of being new. - Established Institutions: Primary beneficiary (institutional/arbitrage) — leverages its age as a signal of quality. - Pragmatic Investors: Secondary beneficiary (moderate/mobile) — uses the effect as a coordination heuristic. - Analytical Observer: Sees the full structure as a Tangled Rope.

**Beneficiaries**: established_institutions, _

**Victims**: disruptive_innovators, _

**Omega questions**:

- Whether the Lindy Effect holds for rapidly iterating digital phenomena.

**Recommendation**: KEEP — Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.17. `quine_self_replication` — **INVESTIGATE**

**Quines (Computational Self-Replication)** | Domain: technological/mathematical

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2000 |
| suppression | 0.0500 |
| theater_ratio | 0.0100 |
| max gradient (g_chi) | -0.2452 |
| max Chi | 0.2740 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | no |

**Perspective type labels**: powerless=mountain, moderate=rope, institutional=rope, analytical=mountain

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.217377 | -0.427816 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.221298 | -0.415168 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.008450 | -1.156290 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.273986 | -0.245206 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=other

**Spec summary**: A Quine is a non-empty computer program which takes no input and produces a copy of its own source code as its only output. This demonstrates a fundamental property of computability derived from Kleene's Second Recursion Theorem: any Turing-complete system possesses the latent capability for self-description and replication without external templates. The constraint is the logical necessity of this fixed-point behavior. KEY AGENTS (by structural relationship): - The Source Code (Subject): The powerless agent whose structure must simultaneously encode logic and its own literal representation. (powerless/trapped) - The Compiler/Interpreter: The institutional rule-enforcing environment that translates the quine's instructions into output, benefiting from the demonstration of logical completeness. (institutional/arbitrage) - Cybersecurity Defenders: Agents who must contend with the consequences of self-replication in malware, treating the capability as a structural vulnerability. (moderate/constrained) - The Computer Scientist (Analytical): The observer mapping the "Mountain" of Kleene's Recursion Theorem through the "Rope" of specific code.

**Key agents**: (by structural relationship): - The Source Code (Subject): The powerless agent whose structure must simultaneously encode logic and its own literal representation. (powerless/trapped) - The Compiler/Interpreter: The institutional rule-enforcing environment that translates the quine's instructions into output, benefiting from the demonstration of logical completeness. (institutional/arbitrage) - Cybersecurity Defenders: Agents who must contend with the consequences of self-replication in malware, treating the capability as a structural vulnerability. (moderate/constrained) - The Computer Scientist (Analytical): The observer mapping the "Mountain" of Kleene's Recursion Theorem through the "Rope" of specific code.

**Beneficiaries**: autonomous_agent_developers, computer_science_educators

**Victims**: static_code_analysis_tools, cybersecurity_defenders

**Omega questions**:

- Whether quines can be a basis for open-ended digital evolution.

**Recommendation**: INVESTIGATE — Epsilon (0.20) is meaningful but no perspective sees tangled_rope/snare — possible mislabel

---

### 6.18. `rfc9293_state_machine` — **KEEP**

**TCP State Machine Constraints** | Domain: technological/institutional

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.2000 |
| suppression | 0.6000 |
| theater_ratio | 0.0600 |
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

**Cross-refs**: tangled_psi=0.0035, band=rope_leaning, signature=false_ci_rope, coalition=other

**Spec summary**: The TCP state machine governs the lifecycle of a connection, from initial handshake (SYN) to termination (FIN/TIME-WAIT). It enforces strict transition rules that ensure both peers remain synchronized despite network delays or reboots.

**Key agents**: - Stack Developer (Individual Moderate): Must implement transitions for 11 distinct states. - IETF (Institutional): The governing body of the standard. - Automated Scanner/Script (Individual Powerless): No ability to modify the host's logic.

**Beneficiaries**: protocol_stability

**Victims**: developer_effort

**Recommendation**: KEEP — Meaningful epsilon (0.20) with tangled_rope/snare perspective labels; classification is doing structural work

---

### 6.19. `boltzmann_universality_2026` — **INVESTIGATE**

**The Boltzmann Distribution Uniqueness Proof** | Domain: physics/economics/mathematics

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1500 |
| suppression | 0.6500 |
| theater_ratio | 0.0500 |
| max gradient (g_chi) | -0.4662 |
| max Chi | 0.2055 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.163033 | -0.603119 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.165974 | -0.593632 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.006338 | -1.149477 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.205490 | -0.466161 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0012, band=rope_leaning, signature=false_ci_rope, coalition=other

**Spec summary**: Caltech economists and mathematicians have proven that the Boltzmann distribution is the only mathematical law that accurately describes unrelated or "uncoupled" systems. This resolves the puzzle of why the same law appears in gas molecules (physics), AI models, and consumer choice theory (economics' "multinomial logit").

**Key agents**: - Uncoupled Systems (Molecules/Cereal Buyers): Subject (Powerless) - Interdisciplinary Researchers (Tamuz/Sandomirskiy): Beneficiary (Institutional) - Mathematical Auditors: Auditor (Analytical)

**Beneficiaries**: interdisciplinary_modeling_fidelity

**Victims**: non_boltzmann_theoretical_alternatives

**Recommendation**: INVESTIGATE — Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review

---

### 6.20. `electrification_scale_2026` — **INVESTIGATE**

**Industrial Scale Electrification** | Domain: technological/economic/environmental

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1500 |
| suppression | 0.5000 |
| theater_ratio | 0.2400 |
| max gradient (g_chi) | -0.4662 |
| max Chi | 0.2055 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | YES |

**Perspective type labels**: powerless=tangled_rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.163033 | -0.603119 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.165974 | -0.593632 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.006338 | -1.149477 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.205490 | -0.466161 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=false_ci_rope, coalition=other

**Spec summary**: The transition from fossil fuels to renewables catalyzed by "gigafactories" that leverage economies of scale and supply-chain integration. This constraint represents the shift from fossil fuel-powered infrastructure to a dispatchable solar and electric vehicle ecosystem.

**Key agents**: - Coal Miner / Oil Rig Worker (Individual Powerless): Faces job displacement and livelihood threats. - Energy Planner (Institutional): Views electrification as a tool for dispatchable power. - Legacy Auto/Oil Industry (Individual Powerful): Views the shift as a disruptive Snare.

**Beneficiaries**: renewable_energy_sector

**Victims**: fossil_fuel_workers

**Recommendation**: INVESTIGATE — Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review

---

### 6.21. `ergo_storage_rent_mechanism` — **INVESTIGATE**

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

### 6.22. `rogue_wave_control_2026` — **INVESTIGATE**

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

### 6.23. `viral_emergence_covid19_exemplar` — **INVESTIGATE**

**Societal Response to SARS-CoV-2 Emergence** | Domain: social/political/health

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1500 |
| suppression | 0.3000 |
| theater_ratio | 0.8500 |
| max gradient (g_chi) | -0.4662 |
| max Chi | 0.2055 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | YES |
| has tangled/snare label | no |

**Perspective type labels**: powerless=piton, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.163033 | -0.603119 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.165974 | -0.593632 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.006338 | -1.149477 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.205490 | -0.466161 |

**Diagnostics**: low-epsilon=no, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=constructed_low_extraction, coalition=other

**Spec summary**: Models the evolving "social response" constraint following the emergence of a novel virus, using COVID-19 as the exemplar. The constraint begins as the virus itself (a Snare), transforms into a collective action problem (a Scaffold), degrades into political theater (a Tangled Rope), and finally settles into institutional inertia (a Piton) as the virus becomes endemic. KEY AGENTS (by structural relationship): - The Immunocompromised/Elderly: Primary targets of the viral constraint (powerless/trapped). - Public Health Officials: Beneficiaries of the coordination constraint (institutional/mobile). - The General Public: Bears the cost of the response constraint (moderate/constrained). - Epidemiologist: The analytical observer.

**Key agents**: (by structural relationship): - The Immunocompromised/Elderly: Primary targets of the viral constraint (powerless/trapped). - Public Health Officials: Beneficiaries of the coordination constraint (institutional/mobile). - The General Public: Bears the cost of the response constraint (moderate/constrained). - Epidemiologist: The analytical observer.

**Beneficiaries**: public_health_bureaucracy

**Victims**: long_covid_sufferers

**Recommendation**: INVESTIGATE — Moderate epsilon (0.15); tangled_rope label may or may not add signal — needs spec review

---

### 6.24. `planetary_diet_constraint_2026` — **INVESTIGATE**

**Planetary Boundary Dietary Alignment** | Domain: ecological/economic/social

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.1000 |
| suppression | 0.6000 |
| theater_ratio | 0.2900 |
| max gradient (g_chi) | -0.6871 |
| max Chi | 0.1370 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | no |

**Perspective type labels**: powerless=rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.108689 | -0.778423 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.110649 | -0.772100 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.004225 | -1.142661 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.136993 | -0.687119 |

**Diagnostics**: low-epsilon=YES, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=constructed_low_extraction, coalition=other

**Spec summary**: The article identifies the Mediterranean diet as being "good for the planet." This creates a "Planetary Health" constraint where individual consumption is indexed against global ecological survival. The constraint shifts from a personal health choice to a collective survival mandate, where high-meat and high-dairy consumption are increasingly classified as ecological "theft" or systemically unviable.

**Key agents**: - Citizen in a High-Meat Culture (Individual Powerless): Faces pressure to change ingrained dietary habits. - Institutional Planner (Institutional): Global governance/Climate Scientist who views the diet as a necessary Rope. - Industrial Meat Producer (Individual Powerful): Views the constraint as a Snare strangling their business model.

**Beneficiaries**: future_humanity

**Victims**: industrial_animal_agriculture

**Recommendation**: INVESTIGATE — Moderate epsilon (0.10); tangled_rope label may or may not add signal — needs spec review

---

### 6.25. `decentralized_infrastructure_rope` — **RECLASSIFY**

**The Auditable Bridge** | Domain: technological/social

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.0800 |
| suppression | 0.2500 |
| theater_ratio | 0.0200 |
| max gradient (g_chi) | -0.7755 |
| max Chi | 0.1096 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | no |

**Perspective type labels**: powerless=rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.086951 | -0.848545 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.088519 | -0.843487 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.003380 | -1.139935 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.109594 | -0.775503 |

**Diagnostics**: low-epsilon=YES, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=constructed_low_extraction, coalition=other

*Prolog spec file not found.*

**Recommendation**: RECLASSIFY — Trivially low epsilon (0.08); rope-dominant is obviously correct

---

### 6.26. `sts86_ascent_checklist` — **RECLASSIFY**

**Space Shuttle Ascent/Abort Procedural Matrix** | Domain: technological/institutional

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.0500 |
| suppression | 0.9500 |
| theater_ratio | 0.0300 |
| max gradient (g_chi) | -0.9081 |
| max Chi | 0.0685 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | no |

**Perspective type labels**: powerless=rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.054344 | -0.953729 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.055325 | -0.950565 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.002113 | -1.135848 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.068497 | -0.908074 |

**Diagnostics**: low-epsilon=YES, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=constructed_low_extraction, coalition=other

**Spec summary**: The checklist (JSC-48005) represents the ultimate procedural constraint. It manages the transition from a ground-controlled environment to a physics-dominated ascent where human agency is bound by rigid logic gates.

**Key agents**: - NASA/Mission Operations (Institutional): The architect of the procedural matrix. - Flight Crew (CDR/PLT) (Individual Moderate): The agents executing the script. - The 'Powerless' Pilot (Individual Powerless): The crew-member in a 3-Engine-Out scenario.

**Beneficiaries**: mission_success

**Victims**: pilot_discretion

**Recommendation**: RECLASSIFY — Trivially low epsilon (0.05); rope-dominant is obviously correct

---

### 6.27. `thai_article_112_mountain` — **RECLASSIFY**

**Article 112 (Lèse-majesté Laws) as a Legal Mountain** | Domain: political/social/legal

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.0400 |
| suppression | 0.9500 |
| theater_ratio | 0.1000 |
| max gradient (g_chi) | -0.9523 |
| max Chi | 0.0548 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | no |

**Perspective type labels**: powerless=rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.043475 | -0.988790 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.044260 | -0.986258 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.001690 | -1.134484 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.054797 | -0.952268 |

**Diagnostics**: low-epsilon=YES, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=constructed_low_extraction, coalition=other

**Spec summary**: Article 112 of the Thai Criminal Code, which criminalizes insults against the monarchy, functions within the system as a "Mountain"—a fixed, unchangeable limit[cite: 80]. While logically a human-made law, the 2024 dissolution of the Move Forward Party [cite: 80, 81] and the subsequent exclusion of any party seeking its amendment from coalition talks in 2025/2026 [cite: 148] have effectively rendered it a physical-like boundary of the political possible.

**Key agents**: - People's Party: Subject (Organized) - Operates under the constraint while barred from voting for PMs who support amendment[cite: 148]. - Anutin Charnvirakul: Beneficiary (Institutional) - Rules out any coalition with those seeking amendment[cite: 148]. - The Constitutional Court: Auditor (Analytical) - Enforces the "Mountain" status by dissolving non-compliant parties[cite: 80].

**Beneficiaries**: royalist_establishment

**Victims**: progressive_political_movements

**Recommendation**: RECLASSIFY — Trivially low epsilon (0.04); rope-dominant is obviously correct

---

### 6.28. `portuguese_presidential_term_limits` — **RECLASSIFY**

**Portuguese Constitutional Term Limits (Article 123)** | Domain: political/legal

| Metric | Value |
| :--- | ---: |
| epsilon (base_extractiveness) | 0.0200 |
| suppression | 0.9800 |
| theater_ratio | 0.0500 |
| max gradient (g_chi) | -1.0406 |
| max Chi | 0.0274 |
| claimed_type | tangled_rope |
| chi override | no |
| perspective divergence | no |
| has tangled/snare label | no |

**Perspective type labels**: powerless=rope, moderate=rope, institutional=rope, analytical=rope

| Perspective | d | f(d) | scope | Chi | g_chi |
| :--- | ---: | ---: | ---: | ---: | ---: |
| powerless | 0.9000 | 1.358606 | 0.8 | 0.021738 | -1.058910 |
| moderate | 0.7000 | 1.106492 | 1.0 | 0.022130 | -1.057645 |
| institutional | 0.1200 | -0.042252 | 1.0 | -0.000845 | -1.131758 |
| analytical | 0.7200 | 1.141609 | 1.2 | 0.027399 | -1.040648 |

**Diagnostics**: low-epsilon=YES, sigmoid-compressed=no, low-f(d)-spread=no (spread=1.4009)

**Cross-refs**: tangled_psi=0.0000, band=rope_leaning, signature=constructed_low_extraction, coalition=other

**Spec summary**: Under the Portuguese Constitution, a President cannot serve a third consecutive term. This creates an absolute "Mountain" constraint for the 2026 election, as the highly popular incumbent, Marcelo Rebelo de Sousa, is ineligible to run. This structural limit forces a total reconfiguration of the political field, transitioning from a stable incumbency to an open, multi-polar race.

**Key agents**: - Portuguese Electorate: Subject (Powerless) - Forbidden from voting for their most preferred candidate due to legal limits. - Marcelo Rebelo de Sousa: Subject (Institutional) - The popular incumbent barred from re-election. - Potential Presidential Candidates: Beneficiary (Organized) - Benefit from the open field created by the incumbent's ineligibility. - Legal System: Auditor (Analytical) - Upholds the constitution as a fixed rule.

**Beneficiaries**: potential_presidential_candidates

**Victims**: electorate_preferring_incumbent

**Recommendation**: RECLASSIFY — Trivially low epsilon (0.02); rope-dominant is obviously correct

---

