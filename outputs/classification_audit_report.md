# Classification Audit Report
Generated: 2026-02-08 02:01:33  |  Corpus: 342 constraints  |  .pl files: 666

## Executive Summary

| Category | Description | Count | Severity |
|----------|-------------|------:|----------|
| A+ | Severe Naturalization (auto-regen) | 15 | critical |
| A | Naturalization Errors (auto-regen) | 51 | critical |
| B | Theater-Mountain Conflicts | 5 | warning |
| C | Legitimate Gaps (exonerated) | 31 | info |
| D | WHO Suspects (human review) | 13 | warning |
| E1 | Invalid Claim Values | 2 | warning |
| E2 | Missing Theater Ratio | 195 | info |
| E3 | Missing Core Metrics | 1 | warning |
| E4 | Classification-Metric Inconsistency | 170 | warning |
| F1 | Naturalization Rate by Domain | 1 | research |
| F2 | Theater Coverage by Domain | 1 | research |
| F3 | Claim Distribution (high-ε) | 1 | research |

## The 252-to-Triage Story

The classification engine found 252 False Mountains.
This audit partitions them: **37** are parser errors (A/A+), **31** legitimate (C), **184** uncategorized (need manual review).

## Category A+: Severe Naturalization (15 findings)

Mountain classification with high extraction, active enforcement, AND high theater ratio.
These are the strongest candidates for automated regeneration.

### `adverse_possession`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.65, enforcement=True, theater=0.65
- **Domain:** economic
- **Claimed Type:** snare
- **Perspectival Types:** institutional→rope
- **Beneficiaries:** squatter_or_possessor
- **Victims:** negligent_property_owner
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `ai_cognitive_diversity_arbitrage`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.72, enforcement=True, theater=0.68
- **Domain:** technological
- **Claimed Type:** snare
- **Perspectival Types:** analytical→rope, institutional→mountain, powerless→snare
- **Beneficiaries:** strategic_ai_deployers
- **Victims:** trust_dependent_end_users

### `ai_professional_displacement`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.71, enforcement=True, theater=0.55
- **Domain:** economic
- **Claimed Type:** mountain
- **Perspectival Types:** powerless→snare
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `algorithmic_bias`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.75, enforcement=True, theater=0.62
- **Domain:** technological
- **Claimed Type:** snare
- **Perspectival Types:** powerless→snare
- **Beneficiaries:** institutional_deployer
- **Victims:** marginalized_data_subjects
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `antifragility`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.75, enforcement=True, theater=0.55
- **Domain:** technological
- **Claimed Type:** mountain
- **Perspectival Types:** analytical→mountain, individual_moderate→rope, institutional→tangled_rope, powerless→snare
- **Beneficiaries:** antifragile_practitioner
- **Victims:** fragile_institutions, optimized_serfs

### `asshole_filter_2015`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.75, enforcement=True, theater=0.55
- **Domain:** psychological
- **Claimed Type:** snare
- **Perspectival Types:** powerless→snare
- **Beneficiaries:** transgressive_agents
- **Victims:** rule_following_agents

### `authoritarian_power_paradox`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.80, enforcement=True, theater=0.65
- **Domain:** political
- **Claimed Type:** snare
- **Perspectival Types:** powerless→mountain
- **Beneficiaries:** central_bureaucracy
- **Victims:** individual_outliers
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `bay_of_pigs_operational_silo`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.90, enforcement=True, theater=0.72
- **Domain:** political
- **Claimed Type:** snare
- **Perspectival Types:** powerless→mountain
- **Beneficiaries:** cia_operational_autonomy
- **Victims:** brigade_2506_exiles
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `juvenile_underclass_2026`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.75, enforcement=True, theater=0.80
- **Domain:** social
- **Claimed Type:** piton
- **Perspectival Types:** analytical→piton, institutional→rope, powerless→mountain
- **Beneficiaries:** adult_institutional_hegemony
- **Victims:** juvenile_autonomy
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `ulysses_chp03`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.52, enforcement=True, theater=0.72
- **Domain:** philosophical
- **Claimed Type:** piton
- **Perspectival Types:** analytical→piton, institutional→mountain, powerless→snare
- **Beneficiaries:** physical_reality
- **Victims:** stephen_dedalus

### `ulysses_chp06`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.49, enforcement=True, theater=0.88
- **Domain:** social
- **Claimed Type:** piton
- **Perspectival Types:** analytical→piton, institutional→rope, powerless→mountain
- **Beneficiaries:** burial_societies
- **Victims:** paddy_dignam
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `ulysses_chp14`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.55, enforcement=True, theater=0.98
- **Domain:** biological
- **Claimed Type:** piton
- **Perspectival Types:** analytical→piton, institutional→mountain, powerless→snare
- **Beneficiaries:** biological_continuance
- **Victims:** mina_purefoy

### `ulysses_chp15`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.58, enforcement=True, theater=0.99
- **Domain:** social
- **Claimed Type:** piton
- **Perspectival Types:** analytical→piton, institutional→mountain, powerless→snare
- **Beneficiaries:** nighttown_economy
- **Victims:** leopold_bloom

### `ulysses_chp18`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.50, enforcement=True, theater=0.97
- **Domain:** social
- **Claimed Type:** piton
- **Perspectival Types:** analytical→piton, institutional→mountain, powerless→snare
- **Beneficiaries:** biological_affirmation
- **Victims:** molly_bloom

### `xi_mao_ideological_centralization`
- **Category:** A+ | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.75, enforcement=True, theater=0.82
- **Domain:** political
- **Claimed Type:** mountain
- **Perspectival Types:** institutional→rope
- **Beneficiaries:** ccp_leadership_core
- **Victims:** individual_political_autonomy
- **False Mountain:** Yes (gap: snare_masked_as_rope)


## Category A: Naturalization Errors (51 findings)

Mountain classification with high extraction and active enforcement.
Queue for classification regeneration.

### `academic_peer_review_gatekeeping`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.75, enforcement=True
- **Domain:** economic
- **Claimed Type:** tangled_rope
- **Perspectival Types:** analytical→tangled_rope, institutional→rope, powerless→mountain
- **Beneficiaries:** journal_publishers
- **Victims:** junior_professors
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `ad_synaptic_deficit`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.85, enforcement=True
- **Domain:** biological
- **Claimed Type:** mountain
- **Perspectival Types:** analytical→mountain, institutional→rope, powerless→snare
- **Beneficiaries:** metabolic_efficiency
- **Victims:** neural_integrity
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `ai_driven_surveillance_sensor_layer`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.75, enforcement=True
- **Domain:** technological
- **Claimed Type:** tangled_rope
- **Perspectival Types:** powerless→snare
- **Beneficiaries:** security_agencies, ai_vendors
- **Victims:** privacy_advocates, political_activists
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `ai_edu_decentralization`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.50, enforcement=True
- **Domain:** technological
- **Claimed Type:** rope
- **Perspectival Types:** individual_moderate→rope
- **Beneficiaries:** tech_platforms, microschool_families
- **Victims:** legacy_mass_institutions

### `amish_technological_renunciation`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.80, enforcement=True
- **Domain:** social
- **Claimed Type:** snare
- **Perspectival Types:** powerless→snare
- **Beneficiaries:** community_integrity
- **Victims:** individual_autonomy
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `ancestral_pueblo_hydrology`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.82, enforcement=True
- **Domain:** environmental
- **Claimed Type:** tangled_rope
- **Perspectival Types:** analytical→mountain, institutional→rope, organized→scaffold, powerless→snare
- **Beneficiaries:** priesthood_leadership
- **Victims:** pueblo_farmers
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `apartheid_nuclear_program`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.85, enforcement=True
- **Domain:** political
- **Claimed Type:** tangled_rope
- **Perspectival Types:** analytical→mountain, institutional→rope, powerless→snare
- **Beneficiaries:** apartheid_regime
- **Victims:** forced_laborers, political_dissidents
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `arctic_geopolitical_flashpoint`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.75, enforcement=True
- **Domain:** geopolitical
- **Claimed Type:** snare
- **Perspectival Types:** analytical→mountain, collective_organized→tangled_rope, institutional→rope, powerless→snare
- **Beneficiaries:** us_defense_sector, arctic_shipping_conglomerates
- **Victims:** greenlandic_sovereignty, dmr_alliances
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `asce_7_22_seismic_design`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.20, enforcement=True
- **Domain:** technological
- **Claimed Type:** mountain
- **Perspectival Types:** powerless→mountain
- **Beneficiaries:** public_safety, insurance_industry
- **Victims:** property_developers
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `blackstone_carried_interest_taxation`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.30, enforcement=True
- **Domain:** economic
- **Claimed Type:** rope
- **Perspectival Types:** powerful→rope
- **Beneficiaries:** senior_managing_directors, blackstone_management, private_equity_industry
- **Victims:** public_tax_revenue, none, retail_investor, public_equity_principles

### `broke_vs_poor_grocery_math`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.80, enforcement=True
- **Domain:** economic
- **Claimed Type:** enforcement
- **Perspectival Types:** institutional→mountain, moderate→rope, powerless→snare
- **Beneficiaries:** economic_system_beneficiaries
- **Victims:** individuals_in_poverty

### `burden_of_proof_scientific_empirical`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.30, enforcement=True
- **Domain:** technological
- **Claimed Type:** mountain
- **Perspectival Types:** powerless→mountain
- **Beneficiaries:** established_journals, scientific_consensus_stability
- **Victims:** novel_high_risk_hypotheses, null_result_transparency
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `climate_target_one_point_five`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.30, enforcement=True
- **Domain:** political
- **Claimed Type:** tangled_rope
- **Perspectival Types:** collective_organized→rope, institutional→snare, powerless→mountain
- **Beneficiaries:** small_island_states
- **Victims:** fossil_fuel_reliant_economies

### `columbia_2026_elections`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.45, enforcement=True
- **Domain:** political
- **Claimed Type:** rope
- **Perspectival Types:** powerful→rope

### `comitatus_bond`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** social
- **Claimed Type:** rope
- **Beneficiaries:** tribal_stability
- **Victims:** cowardly_thanes

### `constitutional_supremacy`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.30, enforcement=True
- **Domain:** legal
- **Claimed Type:** mountain
- **Perspectival Types:** institutional→rope, powerful→snare, powerless→mountain
- **Beneficiaries:** judicial_authority
- **Victims:** legislative_supremacy
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `copyright_protection`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** social
- **Claimed Type:** tangled_rope
- **Perspectival Types:** individual_moderate→rope, institutional→snare, powerless→mountain
- **Beneficiaries:** creators
- **Victims:** the_public

### `crispr_genomic_rewrite_2026`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** technological
- **Claimed Type:** rope
- **Perspectival Types:** analytical→mountain, institutional→rope, powerless→rope
- **Beneficiaries:** genetic_disease_patients
- **Victims:** congenital_disease_legacy

### `cuban_missile_crisis_excomm_delibration`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.20, enforcement=True
- **Domain:** political
- **Claimed Type:** rope
- **Perspectival Types:** institutional→mountain
- **Beneficiaries:** global_population, diplomatic_stability
- **Victims:** castro_regime_autonomy

### `deferential_realism_core`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.20, enforcement=True
- **Domain:** philosophical
- **Claimed Type:** rope
- **Perspectival Types:** institutional→rope
- **Beneficiaries:** analytical_observers, policy_reformers
- **Victims:** universalist_ideologues
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `dldr_information_policy`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.35, enforcement=True
- **Domain:** technological
- **Claimed Type:** tangled_rope
- **Beneficiaries:** user_autonomy, archive_plurality
- **Victims:** mainstream_sanitization

### `educational_unbundling_implementation`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** technological
- **Claimed Type:** rope
- **Perspectival Types:** analytical→rope
- **Beneficiaries:** independent_learners, skills_based_employers
- **Victims:** administrative_bureaucracies

### `ergo_autolykos_asic_resistance`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.20, enforcement=True
- **Domain:** technological
- **Claimed Type:** snare
- **Perspectival Types:** analytical→mountain, individual_moderate→rope, institutional→snare, powerless→mountain
- **Beneficiaries:** retail_gpu_miners
- **Victims:** asic_manufacturers

### `fmeca_procedures_1980`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** technological
- **Claimed Type:** rope
- **Perspectival Types:** powerless→snare
- **Beneficiaries:** department_of_defense, system_safety
- **Victims:** contractors, innovative_design_flexibility
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `glp1_payload_efficiency_pivot`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.35, enforcement=True
- **Domain:** economic
- **Claimed Type:** rope
- **Perspectival Types:** institutional→rope
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `golden_handcuffs`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** economic
- **Claimed Type:** tangled_rope
- **Perspectival Types:** institutional→rope, powerful→tangled_rope, powerless→mountain
- **Beneficiaries:** institutional
- **Victims:** powerless
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `gs1_gln_identification`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.30, enforcement=True
- **Domain:** technological
- **Claimed Type:** rope
- **Perspectival Types:** powerless→mountain
- **Beneficiaries:** global_traceability_efficiency, enterprise_resource_planning_vendors
- **Victims:** small_scale_suppliers_overhead
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `gs1_standardized_identification`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** technological
- **Claimed Type:** rope
- **Perspectival Types:** powerless→mountain
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `keltner_relationship_evaluation`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.35, enforcement=True
- **Domain:** social
- **Claimed Type:** unknown
- **Perspectival Types:** analytical→mountain, individual_moderate→rope, institutional→rope, powerless→snare
- **Beneficiaries:** relationship_clarity
- **Victims:** non_standard_dyads
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `kjv_textual_authority`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** religious
- **Claimed Type:** mountain
- **Perspectival Types:** individual_moderate→snare, institutional→rope, powerless→mountain
- **Beneficiaries:** english_monarchy
- **Victims:** puritan_dissenters
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `magna_carta_liberties`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** political
- **Claimed Type:** rope
- **Beneficiaries:** free_men, the_barons
- **Victims:** arbitrary_monarchy

### `mars_rovers_navigational_autonomy`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** technological
- **Claimed Type:** rope
- **Perspectival Types:** powerless→mountain
- **Beneficiaries:** mission_longevity, navigational_safety
- **Victims:** traverse_speed, cpu_availability_for_science
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `matching_markets_general`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** economic
- **Claimed Type:** tangled_rope
- **Perspectival Types:** analytical→mountain, institutional→rope, powerless→snare
- **Beneficiaries:** elite_participants
- **Victims:** low_signal_agents
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `moores_law`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.30, enforcement=True
- **Domain:** technological
- **Claimed Type:** mountain
- **Perspectival Types:** powerless→rope
- **Beneficiaries:** platform_capitalists
- **Victims:** legacy_infrastructure_owners

### `omelet_perfection_complexity`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.25, enforcement=True
- **Domain:** social
- **Claimed Type:** tangled_rope
- **Beneficiaries:** complex_wisdom
- **Victims:** simple_answer

### `openbsd_netiquette_protocol`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.30, enforcement=True
- **Domain:** technological
- **Claimed Type:** tangled_rope
- **Perspectival Types:** individual_moderate→rope
- **Beneficiaries:** technical_clarity
- **Victims:** casual_expression

### `overton_window`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** political
- **Claimed Type:** mountain
- **Perspectival Types:** institutional→mountain
- **Beneficiaries:** political_incumbents
- **Victims:** radical_reformers

### `permissive_software_licensing`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.20, enforcement=True
- **Domain:** technological
- **Claimed Type:** rope
- **Perspectival Types:** institutional→rope, powerful→snare, powerless→mountain
- **Beneficiaries:** commercial_software_vendors
- **Victims:** reciprocal_commons
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `perseverance_ai_drive`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.25, enforcement=True
- **Domain:** technological
- **Claimed Type:** rope
- **Perspectival Types:** analytical→rope, institutional→rope, powerless→mountain
- **Beneficiaries:** nasa_engineers
- **Victims:** rover_resources
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `postman_survival_protocol`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** social
- **Claimed Type:** tangled_rope
- **Beneficiaries:** individual_subject
- **Victims:** institutional_bureaucracy

### `private_identity_integration`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** social
- **Claimed Type:** rope
- **Perspectival Types:** powerless→snare
- **Beneficiaries:** social_hygiene
- **Victims:** radical_transparency
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `quantum_entanglement_protocol`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.20, enforcement=True
- **Domain:** technological
- **Claimed Type:** rope
- **Perspectival Types:** powerless→mountain
- **Beneficiaries:** interstellar_governance, high_frequency_galactic_trade
- **Victims:** local_planetary_sovereignty
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `rfc9293_state_machine`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.20, enforcement=True
- **Domain:** technological
- **Claimed Type:** mountain
- **Perspectival Types:** individual_moderate→snare, institutional→rope, powerless→mountain
- **Beneficiaries:** protocol_stability
- **Victims:** developer_effort
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `s1_visa`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.20, enforcement=True
- **Domain:** economic
- **Claimed Type:** mountain
- **Perspectival Types:** powerful→snare
- **Beneficiaries:** institutional_investors, sec, the_public, visa_inc
- **Victims:** registrant_legal_budget, joseph_saunders, none

### `s1_visa_judgment_sharing_agreement`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** legal
- **Claimed Type:** rope
- **Perspectival Types:** individual_moderate→snare
- **Beneficiaries:** visa_inc_shareholders, visa_inc, global_payment_system, none
- **Victims:** signatory_member_banks, small_bank, none

### `south_china_sea_arbitration_2016_2026`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.45, enforcement=True
- **Domain:** legal
- **Claimed Type:** mountain
- **Perspectival Types:** powerless→snare
- **Beneficiaries:** philippines, international_rules_based_order
- **Victims:** chinese_maritime_expansion

### `transient_event_detection`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.45, enforcement=True
- **Domain:** technological
- **Claimed Type:** tangled_rope
- **Perspectival Types:** powerless→mountain
- **Beneficiaries:** modern_astronomers
- **Victims:** traditional_methodologists

### `unclos_2026`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.30, enforcement=True
- **Domain:** legal
- **Claimed Type:** rope
- **Perspectival Types:** powerless→mountain
- **Beneficiaries:** land_locked_states, coastal_middle_powers
- **Victims:** maritime_unilateralists
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `vienna_quantum_superposition_2026`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.60, enforcement=True
- **Domain:** technological
- **Claimed Type:** tangled_rope
- **Perspectival Types:** analytical→mountain, institutional→rope, powerless→snare
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `viral_transmission_rates`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.70, enforcement=True
- **Domain:** technological
- **Claimed Type:** mountain
- **Perspectival Types:** analytical→mountain
- **Beneficiaries:** institutional_governance
- **Victims:** precarious_laborers
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `wikipedia_notability_requirement_2026`
- **Category:** A | **Severity:** critical
- **Summary:** Mountain naturalization: ε=0.40, enforcement=True
- **Domain:** social
- **Claimed Type:** mountain
- **Perspectival Types:** institutional→rope
- **Beneficiaries:** institutional_editors
- **Victims:** marginalized_knowledge_producers
- **False Mountain:** Yes (gap: snare_masked_as_rope)


## Category B: Theater-Mountain Conflicts (5 findings)

Mountain classification with high theater ratio but not meeting full Category A criteria.
Flag for manual review.

### `asymmetric_computational_difficulty`
- **Category:** B | **Severity:** warning
- **Summary:** Theater-Mountain conflict: theater=0.75 but classified as mountain
- **Domain:** technological
- **Claimed Type:** snare
- **Perspectival Types:** analytical→piton, institutional→snare, organized→scaffold, powerless→mountain
- **Beneficiaries:** secret_adversary
- **Victims:** network_participant

### `belief_argument_conclusion`
- **Category:** B | **Severity:** warning
- **Summary:** Theater-Mountain conflict: theater=0.80 but classified as mountain
- **Domain:** social
- **Claimed Type:** snare
- **Perspectival Types:** analytical→mountain
- **Beneficiaries:** instinctive_certainty
- **Victims:** the_arguer
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `big_data_astrophysics_arbitrage`
- **Category:** B | **Severity:** warning
- **Summary:** Theater-Mountain conflict: theater=0.60 but classified as mountain
- **Domain:** technological
- **Claimed Type:** mountain
- **Perspectival Types:** individual_moderate→rope
- **Beneficiaries:** institutional_data_centers
- **Victims:** resource_limited_astronomers

### `winners_curse`
- **Category:** B | **Severity:** warning
- **Summary:** Theater-Mountain conflict: theater=0.65 but classified as mountain
- **Domain:** economic
- **Claimed Type:** mountain
- **Perspectival Types:** analytical→mountain, institutional→rope, powerless→snare
- **Beneficiaries:** sellers, auctioneers
- **Victims:** aggressive_bidders, overconfident_investors
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `zombie_reasoning_2026`
- **Category:** B | **Severity:** warning
- **Summary:** Theater-Mountain conflict: theater=0.88 but classified as mountain
- **Domain:** philosophical
- **Claimed Type:** tangled_rope
- **Perspectival Types:** analytical→mountain, institutional→rope, powerless→snare
- **Beneficiaries:** generative_ai_marketing
- **Victims:** human_epistemic_clarity
- **False Mountain:** Yes (gap: snare_masked_as_rope)


## Category C: Legitimate Gaps — Exoneration (31 findings)

False mountains where Mountain classification is defensible:
low extraction, no enforcement. These are correctly-identified perspectival gaps.

- `automatic_enrollment_defaults` — Legitimate gap: ε=0.05, no enforcement — mountain defensible
- `biological_curiosity` — Legitimate gap: ε=0.15, no enforcement — mountain defensible
- `brain_network_paradigm_2026` — Legitimate gap: ε=0.00, no enforcement — mountain defensible
- `church_turing_thesis` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `click_chemistry_paradigm_2026` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `coffee_cardiovascular_2026` — Legitimate gap: ε=0.12, no enforcement — mountain defensible
- `collatz_conjecture_determinism` — Legitimate gap: ε=0.15, no enforcement — mountain defensible
- `continuum_hypothesis_undecidability` — Legitimate gap: ε=0.15, no enforcement — mountain defensible
- `elencher_identity_transformation` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `epigenetics_complexity_2026` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `euler_characteristic_topology` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `fundamental_theorem_of_algebra` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `gauss_bonnet_topology` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `genetic_algorithms_evolution` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `goldbach_conjecture` — Legitimate gap: ε=0.15, no enforcement — mountain defensible
- `hamiltonian_path_complexity` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `hilberts_hotel_infinity` — Legitimate gap: ε=0.15, no enforcement — mountain defensible
- `indexical_relativity_core` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `marriage_problem` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `martian_signal_latency` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `mom_z14_galaxy_2026` — Legitimate gap: ε=0.05, no enforcement — mountain defensible
- `narrative_engineering_2026` — Legitimate gap: ε=0.15, no enforcement — mountain defensible
- `newtons_method_convergence` — Legitimate gap: ε=0.15, no enforcement — mountain defensible
- `noethers_theorem_symmetry` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `prime_number_theorem` — Legitimate gap: ε=0.15, no enforcement — mountain defensible
- `reciprocity_laws_math` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `rices_theorem_undecidability` — Legitimate gap: ε=0.15, no enforcement — mountain defensible
- `rogue_wave_control_2026` — Legitimate gap: ε=0.15, no enforcement — mountain defensible
- `shannon_entropy_limit` — Legitimate gap: ε=0.10, no enforcement — mountain defensible
- `sylow_theorems_group_theory` — Legitimate gap: ε=0.15, no enforcement — mountain defensible
- `vertebrate_turning_point_2026` — Legitimate gap: ε=0.05, no enforcement — mountain defensible

## Category D: WHO Suspects (13 findings)

High extraction with declared beneficiary/victim asymmetry but missing
key perspectival coverage. Needs human review to determine if extraction is hidden.

### `adverse_possession`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.65, has beneficiary/victim but missing powerless perspective
- **Domain:** economic
- **Claimed Type:** snare
- **Perspectival Types:** institutional→rope
- **Beneficiaries:** squatter_or_possessor
- **Victims:** negligent_property_owner
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `ai_driven_surveillance_sensor_layer`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.75, has beneficiary/victim but missing institutional perspective
- **Domain:** technological
- **Claimed Type:** tangled_rope
- **Perspectival Types:** powerless→snare
- **Beneficiaries:** security_agencies, ai_vendors
- **Victims:** privacy_advocates, political_activists
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `ai_edu_decentralization`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.50, has beneficiary/victim but missing powerless, institutional perspective
- **Domain:** technological
- **Claimed Type:** rope
- **Perspectival Types:** individual_moderate→rope
- **Beneficiaries:** tech_platforms, microschool_families
- **Victims:** legacy_mass_institutions

### `algorithmic_bias`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.75, has beneficiary/victim but missing institutional perspective
- **Domain:** technological
- **Claimed Type:** snare
- **Perspectival Types:** powerless→snare
- **Beneficiaries:** institutional_deployer
- **Victims:** marginalized_data_subjects
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `amish_technological_renunciation`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.80, has beneficiary/victim but missing institutional perspective
- **Domain:** social
- **Claimed Type:** snare
- **Perspectival Types:** powerless→snare
- **Beneficiaries:** community_integrity
- **Victims:** individual_autonomy
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `arrows_impossibility_theorem`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.60, has beneficiary/victim but missing institutional perspective
- **Domain:** political
- **Claimed Type:** mountain
- **Perspectival Types:** powerless→mountain
- **Beneficiaries:** stable_incumbents
- **Victims:** democratic_idealists
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `asshole_filter_2015`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.75, has beneficiary/victim but missing institutional perspective
- **Domain:** psychological
- **Claimed Type:** snare
- **Perspectival Types:** powerless→snare
- **Beneficiaries:** transgressive_agents
- **Victims:** rule_following_agents

### `authoritarian_power_paradox`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.80, has beneficiary/victim but missing institutional perspective
- **Domain:** political
- **Claimed Type:** snare
- **Perspectival Types:** powerless→mountain
- **Beneficiaries:** central_bureaucracy
- **Victims:** individual_outliers
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `bay_of_pigs_operational_silo`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.90, has beneficiary/victim but missing institutional perspective
- **Domain:** political
- **Claimed Type:** snare
- **Perspectival Types:** powerless→mountain
- **Beneficiaries:** cia_operational_autonomy
- **Victims:** brigade_2506_exiles
- **False Mountain:** Yes (gap: rope_appears_as_mountain)

### `belief_argument_conclusion`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.70, has beneficiary/victim but missing powerless, institutional perspective
- **Domain:** social
- **Claimed Type:** snare
- **Perspectival Types:** analytical→mountain
- **Beneficiaries:** instinctive_certainty
- **Victims:** the_arguer
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `big_data_astrophysics_arbitrage`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.75, has beneficiary/victim but missing powerless, institutional perspective
- **Domain:** technological
- **Claimed Type:** mountain
- **Perspectival Types:** individual_moderate→rope
- **Beneficiaries:** institutional_data_centers
- **Victims:** resource_limited_astronomers

### `viral_transmission_rates`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.70, has beneficiary/victim but missing powerless, institutional perspective
- **Domain:** technological
- **Claimed Type:** mountain
- **Perspectival Types:** analytical→mountain
- **Beneficiaries:** institutional_governance
- **Victims:** precarious_laborers
- **False Mountain:** Yes (gap: snare_masked_as_rope)

### `xi_mao_ideological_centralization`
- **Category:** D | **Severity:** warning
- **Summary:** WHO suspect: ε=0.75, has beneficiary/victim but missing powerless perspective
- **Domain:** political
- **Claimed Type:** mountain
- **Perspectival Types:** institutional→rope
- **Beneficiaries:** ccp_leadership_core
- **Victims:** individual_political_autonomy
- **False Mountain:** Yes (gap: snare_masked_as_rope)


## Category E: Structural Defects (368 findings)

### E1: Invalid Claim Values (2 findings)

- `keltner_relationship_evaluation` — Invalid claim value: 'social_governance'
- `social_narrative_casting` — Invalid claim value: 'psychological_projection'

### E2: Missing Theater Ratio (195 findings)

- `26usc469_real_estate_exemption` — Missing theater_ratio in .pl file
- `advice_as_dangerous_gift` — Missing theater_ratio in .pl file
- `asce_7_22_seismic_design` — Missing theater_ratio in .pl file
- `astm_d638_tensile_testing` — Missing theater_ratio in .pl file
- `automatic_enrollment_defaults` — Missing theater_ratio in .pl file
- `availability_heuristic` — Missing theater_ratio in .pl file
- `axiom_of_choice_determinacy` — Missing theater_ratio in .pl file
- `banach_tarski_paradox` — Missing theater_ratio in .pl file
- `base_pair_complementarity` — Missing theater_ratio in .pl file
- `basel_problem_convergence` — Missing theater_ratio in .pl file
- `berkshire_compounding_culture` — Missing theater_ratio in .pl file
- `biological_curiosity` — Missing theater_ratio in .pl file
- `birthday_paradox_collison` — Missing theater_ratio in .pl file
- `blackstone_carried_interest_taxation` — Missing theater_ratio in .pl file
- `brain_network_paradigm_2026` — Missing theater_ratio in .pl file
- `brouwer_fixed_point` — Missing theater_ratio in .pl file
- `buffons_needle_pi_estimation` — Missing theater_ratio in .pl file
- `burali_forte_paradox` — Missing theater_ratio in .pl file
- `burden_of_proof_legal_criminal` — Missing theater_ratio in .pl file
- `burden_of_proof_scientific_empirical` — Missing theater_ratio in .pl file
- `busy_beaver_noncomputability` — Missing theater_ratio in .pl file
- `cantor_set_topology` — Missing theater_ratio in .pl file
- `cap_theorem` — Missing theater_ratio in .pl file
- `central_limit_theorem_convergence` — Missing theater_ratio in .pl file
- `chaitins_omega_undecidability` — Missing theater_ratio in .pl file
- `choice_architecture_design` — Missing theater_ratio in .pl file
- `church_turing_thesis` — Missing theater_ratio in .pl file
- `cinderella_midnight_deadline` — Missing theater_ratio in .pl file
- `click_chemistry_paradigm_2026` — Missing theater_ratio in .pl file
- `climate_attribution_2026` — Missing theater_ratio in .pl file
- `climate_target_one_point_five` — Missing theater_ratio in .pl file
- `coinbase_crypto_volatility` — Missing theater_ratio in .pl file
- `collatz_conjecture_determinism` — Missing theater_ratio in .pl file
- `columbia_2026_elections` — Missing theater_ratio in .pl file
- `comitatus_bond` — Missing theater_ratio in .pl file
- `confirmation_bias` — Missing theater_ratio in .pl file
- `constitutional_supremacy` — Missing theater_ratio in .pl file
- `continuum_hypothesis_undecidability` — Missing theater_ratio in .pl file
- `conways_game_of_life_dynamics` — Missing theater_ratio in .pl file
- `copyleft_viral_licensing` — Missing theater_ratio in .pl file
- `copyright_protection` — Missing theater_ratio in .pl file
- `countable_infinity_cardinality` — Missing theater_ratio in .pl file
- `cow_field_poop` — Missing theater_ratio in .pl file
- `creative_commons_licensing` — Missing theater_ratio in .pl file
- `crispr_genomic_rewrite_2026` — Missing theater_ratio in .pl file
- `cuban_missile_crisis_excomm_delibration` — Missing theater_ratio in .pl file
- `currys_paradox` — Missing theater_ratio in .pl file
- `damped_harmonics` — Missing theater_ratio in .pl file
- `deferential_realism_core` — Missing theater_ratio in .pl file
- `dldr_information_policy` — Missing theater_ratio in .pl file
- `dunbars_number` — Missing theater_ratio in .pl file
- `e2ee_digital_privacy_2026` — Missing theater_ratio in .pl file
- `educational_unbundling_implementation` — Missing theater_ratio in .pl file
- `electrification_scale_2026` — Missing theater_ratio in .pl file
- `elencher_identity_transformation` — Missing theater_ratio in .pl file
- `empty_tomb_transformation` — Missing theater_ratio in .pl file
- `endowment_effect` — Missing theater_ratio in .pl file
- `epigenetics_complexity_2026` — Missing theater_ratio in .pl file
- `ergo_autolykos_asic_resistance` — Missing theater_ratio in .pl file
- `ergo_dexy_gold_protocol` — Missing theater_ratio in .pl file
- `ergo_lets_protocol` — Missing theater_ratio in .pl file
- `ergo_mixer_protocol` — Missing theater_ratio in .pl file
- `ergo_nipopows` — Missing theater_ratio in .pl file
- `ergo_sig_usd_protocol` — Missing theater_ratio in .pl file
- `ergo_storage_rent_mechanism` — Missing theater_ratio in .pl file
- `ergodic_theorems` — Missing theater_ratio in .pl file
- `euler_characteristic_topology` — Missing theater_ratio in .pl file
- `exoplanet_habitability_arbitrage` — Missing theater_ratio in .pl file
- `extraordinary_narrative_shift` — Missing theater_ratio in .pl file
- `fair_use_doctrine` — Missing theater_ratio in .pl file
- `feigenbaum_universality` — Missing theater_ratio in .pl file
- `finite_simple_groups_classification` — Missing theater_ratio in .pl file
- `fittss_law` — Missing theater_ratio in .pl file
- `fmeca_procedures_1980` — Missing theater_ratio in .pl file
- `four_color_theorem_topological_bound` — Missing theater_ratio in .pl file
- `fundamental_theorem_of_algebra` — Missing theater_ratio in .pl file
- `galois_theory_symmetry` — Missing theater_ratio in .pl file
- `gauss_bonnet_topology` — Missing theater_ratio in .pl file
- `genetic_algorithms_evolution` — Missing theater_ratio in .pl file
- `gita_kurukshetra` — Missing theater_ratio in .pl file
- `glp1_payload_efficiency_pivot` — Missing theater_ratio in .pl file
- `godels_incompleteness_theorems` — Missing theater_ratio in .pl file
- `goldbach_conjecture` — Missing theater_ratio in .pl file
- `golden_handcuffs` — Missing theater_ratio in .pl file
- `gradient_descent_optimization` — Missing theater_ratio in .pl file
- `graph_coloring_complexity` — Missing theater_ratio in .pl file
- `gs1_gln_identification` — Missing theater_ratio in .pl file
- `gs1_standardized_identification` — Missing theater_ratio in .pl file
- `halting_problem_undecidability` — Missing theater_ratio in .pl file
- `hamiltonian_path_complexity` — Missing theater_ratio in .pl file
- `hawthorne_effect` — Missing theater_ratio in .pl file
- `heisenberg_uncertainty` — Missing theater_ratio in .pl file
- `helsinki_bus_theory` — Missing theater_ratio in .pl file
- `heuristic_optimization` — Missing theater_ratio in .pl file
- `hilberts_hotel_infinity` — Missing theater_ratio in .pl file
- `hiv_prep_prevention_2026` — Missing theater_ratio in .pl file
- `indexical_relativity_core` — Missing theater_ratio in .pl file
- `information_foraging_theory` — Missing theater_ratio in .pl file
- `inner_model_theory_constraints` — Missing theater_ratio in .pl file
- `keltner_relationship_evaluation` — Missing theater_ratio in .pl file
- `kidney_exchange_market` — Missing theater_ratio in .pl file
- `kirby_paris_theorem` — Missing theater_ratio in .pl file
- `kjv_great_awakening` — Missing theater_ratio in .pl file
- `kjv_linguistic_residue` — Missing theater_ratio in .pl file
- `kjv_puritan_new_world_exit` — Missing theater_ratio in .pl file
- `kjv_textual_authority` — Missing theater_ratio in .pl file
- `kleene_recursion_theorem` — Missing theater_ratio in .pl file
- `landscape_of_fear_2026` — Missing theater_ratio in .pl file
- `large_cardinals_foundations` — Missing theater_ratio in .pl file
- `law_of_diminishing_returns` — Missing theater_ratio in .pl file
- `layered_brain_processing` — Missing theater_ratio in .pl file
- `liar_paradox` — Missing theater_ratio in .pl file
- `lindy_effect` — Missing theater_ratio in .pl file
- `litany_of_the_real` — Missing theater_ratio in .pl file
- `lln_convergence` — Missing theater_ratio in .pl file
- `lobs_theorem` — Missing theater_ratio in .pl file
- `local_vs_global_optima` — Missing theater_ratio in .pl file
- `logistic_map_dynamics` — Missing theater_ratio in .pl file
- `lorenz_attractor_dynamics` — Missing theater_ratio in .pl file
- `lowenheim_skolem_theorem` — Missing theater_ratio in .pl file
- `lsd_microdosing_professional_openness` — Missing theater_ratio in .pl file
- `magna_carta_liberties` — Missing theater_ratio in .pl file
- `marriage_problem` — Missing theater_ratio in .pl file
- `mars_rovers_navigational_autonomy` — Missing theater_ratio in .pl file
- `martian_signal_latency` — Missing theater_ratio in .pl file
- `matching_markets_general` — Missing theater_ratio in .pl file
- `max_flow` — Missing theater_ratio in .pl file
- `med_diet_consensus_2026` — Missing theater_ratio in .pl file
- `micro_robot_electronics_integration` — Missing theater_ratio in .pl file
- `microbiome_symbiosis` — Missing theater_ratio in .pl file
- `minimax_decision_rule` — Missing theater_ratio in .pl file
- `monty_hall_conditional_probability` — Missing theater_ratio in .pl file
- `moores_law` — Missing theater_ratio in .pl file
- `newtons_method_convergence` — Missing theater_ratio in .pl file
- `no_cloning_theorem` — Missing theater_ratio in .pl file
- `noethers_theorem_symmetry` — Missing theater_ratio in .pl file
- `nonstandard_arithmetic_models` — Missing theater_ratio in .pl file
- `omelet_perfection_complexity` — Missing theater_ratio in .pl file
- `openbsd_netiquette_protocol` — Missing theater_ratio in .pl file
- `overton_window` — Missing theater_ratio in .pl file
- `p_vs_np` — Missing theater_ratio in .pl file
- `pareto_principle` — Missing theater_ratio in .pl file
- `peano_curve_mapping` — Missing theater_ratio in .pl file
- `permissive_software_licensing` — Missing theater_ratio in .pl file
- `planetary_diet_constraint_2026` — Missing theater_ratio in .pl file
- `platform_cooperativism_governance` — Missing theater_ratio in .pl file
- `poincare_conjucture` — Missing theater_ratio in .pl file
- `postman_survival_protocol` — Missing theater_ratio in .pl file
- `prime_number_theorem` — Missing theater_ratio in .pl file
- `private_identity_integration` — Missing theater_ratio in .pl file
- `proof_of_work_consensus` — Missing theater_ratio in .pl file
- `public_domain_commons` — Missing theater_ratio in .pl file
- `pythagorean_theorem_geometric_constancy` — Missing theater_ratio in .pl file
- `quantum_entanglement_protocol` — Missing theater_ratio in .pl file
- `quantum_nonlocality_2026` — Missing theater_ratio in .pl file
- `quine_self_replication` — Missing theater_ratio in .pl file
- `qwerty_vs_dvorak` — Missing theater_ratio in .pl file
- `reciprocity_laws_math` — Missing theater_ratio in .pl file
- `relativity_of_simultaneity` — Missing theater_ratio in .pl file
- `relativity_physical_invariance` — Missing theater_ratio in .pl file
- `rfc9293_interoperability` — Missing theater_ratio in .pl file
- `rfc9293_state_machine` — Missing theater_ratio in .pl file
- `rices_theorem_undecidability` — Missing theater_ratio in .pl file
- `rotmigration_decision_threshold` — Missing theater_ratio in .pl file
- `s1_visa` — Missing theater_ratio in .pl file
- `s1_visa_judgment_sharing_agreement` — Missing theater_ratio in .pl file
- `sadhu_integrity_protocol` — Missing theater_ratio in .pl file
- `sat_csp_complexity` — Missing theater_ratio in .pl file
- `shannon_entropy_limit` — Missing theater_ratio in .pl file
- `shitty_feedback_handling` — Missing theater_ratio in .pl file
- `skills_based_hiring` — Missing theater_ratio in .pl file
- `skolems_paradox` — Missing theater_ratio in .pl file
- `social_narrative_casting` — Missing theater_ratio in .pl file
- `solar_system_weirdness` — Missing theater_ratio in .pl file
- `somatic_focusing_awareness` — Missing theater_ratio in .pl file
- `sorites_paradox` — Missing theater_ratio in .pl file
- `south_china_sea_arbitration_2016_2026` — Missing theater_ratio in .pl file
- `stable_marriage_coordination` — Missing theater_ratio in .pl file
- `strange_attractor_dynamics` — Missing theater_ratio in .pl file
- `sts86_ascent_checklist` — Missing theater_ratio in .pl file
- `sturgeons_law` — Missing theater_ratio in .pl file
- `suslin_hypothesis_undecidability` — Missing theater_ratio in .pl file
- `sylow_theorems_group_theory` — Missing theater_ratio in .pl file
- `tarski_undefinability` — Missing theater_ratio in .pl file
- `three_body_unpredicability` — Missing theater_ratio in .pl file
- `trade_secret_law` — Missing theater_ratio in .pl file
- `transient_event_detection` — Missing theater_ratio in .pl file
- `traveling_salesperson_problem` — Missing theater_ratio in .pl file
- `udhr_1946` — Missing theater_ratio in .pl file
- `unclos_2026` — Missing theater_ratio in .pl file
- `universal_mathematics_communication` — Missing theater_ratio in .pl file
- `van_der_waerden_theorem` — Missing theater_ratio in .pl file
- `whitehead_problem_undecidability` — Missing theater_ratio in .pl file
- `wikipedia_crowdsourcing_2026` — Missing theater_ratio in .pl file
- `wikipedia_notability_requirement_2026` — Missing theater_ratio in .pl file

### E3: Missing Core Metrics (1 findings)

- `unknown` — Missing core metrics: extractiveness, suppression

### E4: Classification-Metric Inconsistency (170 findings)

- `academic_peer_review_gatekeeping` — Mountain classification but ε=0.75 > 0.15
- `ad_synaptic_deficit` — Mountain classification but ε=0.85 > 0.15
- `advice_as_dangerous_gift` — Snare classification but ε=0.20 < 0.46
- `ai_cognitive_diversity_arbitrage` — Mountain classification but ε=0.72 > 0.15
- `ancestral_pueblo_hydrology` — Mountain classification but ε=0.82 > 0.15
- `antifragility` — Mountain classification but ε=0.75 > 0.15
- `apartheid_nuclear_program` — Mountain classification but ε=0.85 > 0.15
- `arctic_geopolitical_flashpoint` — Mountain classification but ε=0.75 > 0.15
- `arrows_impossibility_theorem` — Mountain classification but ε=0.60 > 0.15
- `asce_7_22_seismic_design` — Mountain classification but ε=0.20 > 0.15
- `asymmetric_computational_difficulty` — Mountain classification but ε=0.85 > 0.15
- `authoritarian_power_paradox` — Mountain classification but ε=0.80 > 0.15
- `automatic_enrollment_defaults` — Snare classification but ε=0.05 < 0.46
- `availability_heuristic` — Snare classification but ε=0.40 < 0.46
- `axiom_of_choice_determinacy` — Mountain classification but ε=0.25 > 0.15
- `axiom_reasoner_2026` — Mountain classification but ε=0.48 > 0.15
- `banach_tarski_paradox` — Mountain classification but ε=0.20 > 0.15
- `basel_problem_convergence` — Mountain classification but ε=0.20 > 0.15
- `bay_of_pigs_operational_silo` — Mountain classification but ε=0.90 > 0.15
- `belief_argument_conclusion` — Mountain classification but ε=0.70 > 0.15
- `biological_curiosity` — Snare classification but ε=0.15 < 0.46
- `birthday_paradox_collison` — Mountain classification but ε=0.40 > 0.15
- `brain_network_paradigm_2026` — Snare classification but ε=0.00 < 0.46
- `broke_vs_poor_grocery_math` — Mountain classification but ε=0.80 > 0.15
- `brouwer_fixed_point` — Mountain classification but ε=0.20 > 0.15
- `buffons_needle_pi_estimation` — Mountain classification but ε=0.20 > 0.15
- `burali_forte_paradox` — Mountain classification but ε=0.20 > 0.15
- `burden_of_proof_scientific_empirical` — Mountain classification but ε=0.30 > 0.15
- `busy_beaver_noncomputability` — Mountain classification but ε=0.40 > 0.15
- `cantor_set_topology` — Mountain classification but ε=0.30 > 0.15
- `cap_theorem` — Mountain classification but ε=0.40 > 0.15
- `central_limit_theorem_convergence` — Mountain classification but ε=0.25 > 0.15
- `chaitins_omega_undecidability` — Mountain classification but ε=0.20 > 0.15
- `click_chemistry_paradigm_2026` — Snare classification but ε=0.10 < 0.46
- `climate_attribution_2026` — Mountain classification but ε=0.20 > 0.15
- `climate_target_one_point_five` — Mountain classification but ε=0.30 > 0.15
- `coffee_cardiovascular_2026` — Snare classification but ε=0.12 < 0.46
- `coinbase_crypto_volatility` — Snare classification but ε=0.40 < 0.46
- `confirmation_bias` — Mountain classification but ε=0.30 > 0.15
- `constitutional_supremacy` — Mountain classification but ε=0.30 > 0.15
- `conways_game_of_life_dynamics` — Mountain classification but ε=0.30 > 0.15
- `copyleft_viral_licensing` — Snare classification but ε=0.05 < 0.46
- `copyright_protection` — Mountain classification but ε=0.40 > 0.15
- `countable_infinity_cardinality` — Mountain classification but ε=0.20 > 0.15
- `cow_field_poop` — Mountain classification but ε=0.40 > 0.15
- `creative_commons_licensing` — Snare classification but ε=0.10 < 0.46
- `crispr_genomic_rewrite_2026` — Mountain classification but ε=0.40 > 0.15
- `cuban_missile_crisis_excomm_delibration` — Mountain classification but ε=0.20 > 0.15
- `currys_paradox` — Mountain classification but ε=0.20 > 0.15
- `damped_harmonics` — Mountain classification but ε=0.25 > 0.15
- `e2ee_digital_privacy_2026` — Snare classification but ε=0.05 < 0.46
- `electrification_scale_2026` — Snare classification but ε=0.15 < 0.46
- `empty_tomb_transformation` — Snare classification but ε=0.40 < 0.46
- `endowment_effect` — Mountain classification but ε=0.30 > 0.15
- `epigenetics_complexity_2026` — Snare classification but ε=0.10 < 0.46
- `ergo_autolykos_asic_resistance` — Snare classification but ε=0.20 < 0.46
- `ergo_lets_protocol` — Snare classification but ε=0.15 < 0.46
- `ergo_mixer_protocol` — Snare classification but ε=0.10 < 0.46
- `ergo_sig_usd_protocol` — Mountain classification but ε=0.40 > 0.15
- `ergo_storage_rent_mechanism` — Snare classification but ε=0.15 < 0.46
- `ergodic_theorems` — Mountain classification but ε=0.30 > 0.15
- `extraordinary_narrative_shift` — Snare classification but ε=0.40 < 0.46
- `feigenbaum_universality` — Mountain classification but ε=0.30 > 0.15
- `finite_simple_groups_classification` — Mountain classification but ε=0.35 > 0.15
- `fmeca_procedures_1980` — Snare classification but ε=0.40 < 0.46
- `fmt_oncology_realignment_2026` — Snare classification but ε=0.18 < 0.46
- `four_color_theorem_topological_bound` — Mountain classification but ε=0.25 > 0.15
- `france_cordon_sanitaire_2026` — Snare classification but ε=0.42 < 0.46
- `galois_theory_symmetry` — Mountain classification but ε=0.20 > 0.15
- `genetic_algorithms_evolution` — Snare classification but ε=0.10 < 0.46
- `gita_kurukshetra` — Snare classification but ε=0.20 < 0.46
- `godels_incompleteness_theorems` — Mountain classification but ε=0.20 > 0.15
- `gold_piton_2026` — Snare classification but ε=0.20 < 0.46
- `golden_handcuffs` — Mountain classification but ε=0.40 > 0.15
- `gradient_descent_optimization` — Mountain classification but ε=0.30 > 0.15
- `graph_coloring_complexity` — Mountain classification but ε=0.40 > 0.15
- `gs1_gln_identification` — Mountain classification but ε=0.30 > 0.15
- `gs1_standardized_identification` — Mountain classification but ε=0.40 > 0.15
- `guinea_worm_eradication` — Snare classification but ε=0.25 < 0.46
- `halting_problem_undecidability` — Mountain classification but ε=0.20 > 0.15
- `hamiltonian_path_complexity` — Snare classification but ε=0.10 < 0.46
- `hawthorne_effect` — Mountain classification but ε=0.40 > 0.15
- `heisenberg_uncertainty` — Mountain classification but ε=0.20 > 0.15
- `helsinki_bus_theory` — Snare classification but ε=0.40 < 0.46
- `heuristic_optimization` — Mountain classification but ε=0.20 > 0.15
- `informational_time_2026` — Mountain classification but ε=0.78 > 0.15
- `inner_model_theory_constraints` — Mountain classification but ε=0.30 > 0.15
- `juvenile_underclass_2026` — Mountain classification but ε=0.75 > 0.15
- `keltner_relationship_evaluation` — Snare classification but ε=0.35 < 0.46
- `kidney_exchange_market` — Snare classification but ε=0.10 < 0.46
- `kirby_paris_theorem` — Mountain classification but ε=0.20 > 0.15
- `kjv_great_awakening` — Snare classification but ε=0.10 < 0.46
- `kjv_puritan_new_world_exit` — Snare classification but ε=0.20 < 0.46
- `kjv_textual_authority` — Mountain classification but ε=0.40 > 0.15
- `kleene_recursion_theorem` — Mountain classification but ε=0.20 > 0.15
- `landscape_of_fear_2026` — Snare classification but ε=0.40 < 0.46
- `large_cardinals_foundations` — Mountain classification but ε=0.30 > 0.15
- `layered_brain_processing` — Mountain classification but ε=0.35 > 0.15
- `lindy_effect` — Mountain classification but ε=0.20 > 0.15
- `lln_convergence` — Mountain classification but ε=0.25 > 0.15
- `lobs_theorem` — Mountain classification but ε=0.30 > 0.15
- `local_vs_global_optima` — Snare classification but ε=0.30 < 0.46
- `logistic_map_dynamics` — Mountain classification but ε=0.35 > 0.15
- `lorenz_attractor_dynamics` — Mountain classification but ε=0.40 > 0.15
- `lowenheim_skolem_theorem` — Mountain classification but ε=0.20 > 0.15
- `lsd_microdosing_professional_openness` — Snare classification but ε=0.40 < 0.46
- `maha_recovery_2026` — Snare classification but ε=0.42 < 0.46
- `manganese_catalysis_2026` — Mountain classification but ε=0.18 > 0.15
- `marriage_problem` — Snare classification but ε=0.10 < 0.46
- `mars_rovers_navigational_autonomy` — Mountain classification but ε=0.40 > 0.15
- `matching_markets_general` — Snare classification but ε=0.40 < 0.46
- `max_flow` — Mountain classification but ε=0.40 > 0.15
- `med_diet_consensus_2026` — Snare classification but ε=0.20 < 0.46
- `micro_robot_electronics_integration` — Mountain classification but ε=0.20 > 0.15
- `microbiome_symbiosis` — Mountain classification but ε=0.45 > 0.15
- `minimax_decision_rule` — Mountain classification but ε=0.30 > 0.15
- `monty_hall_conditional_probability` — Mountain classification but ε=0.33 > 0.15
- `new_civilizational_rope` — Snare classification but ε=0.08 < 0.46
- `no_cloning_theorem` — Mountain classification but ε=0.20 > 0.15
- `nonstandard_arithmetic_models` — Mountain classification but ε=0.25 > 0.15
- `overton_window` — Mountain classification but ε=0.40 > 0.15
- `p_vs_np` — Mountain classification but ε=0.40 > 0.15
- `pareto_principle` — Snare classification but ε=0.40 < 0.46
- `peano_curve_mapping` — Mountain classification but ε=0.25 > 0.15
- `permissive_software_licensing` — Mountain classification but ε=0.20 > 0.15
- `perseverance_ai_drive` — Mountain classification but ε=0.25 > 0.15
- `planetary_diet_constraint_2026` — Snare classification but ε=0.10 < 0.46
- `platform_cooperativism_governance` — Snare classification but ε=0.05 < 0.46
- `poincare_conjucture` — Mountain classification but ε=0.20 > 0.15
- `private_identity_integration` — Snare classification but ε=0.40 < 0.46
- `proof_of_work_consensus` — Mountain classification but ε=0.20 > 0.15
- `public_domain_commons` — Snare classification but ε=0.00 < 0.46
- `quantum_entanglement_protocol` — Mountain classification but ε=0.20 > 0.15
- `quantum_nonlocality_2026` — Snare classification but ε=0.00 < 0.46
- `quine_self_replication` — Mountain classification but ε=0.20 > 0.15
- `qwerty_vs_dvorak` — Mountain classification but ε=0.40 > 0.15
- `relativity_physical_invariance` — Snare classification but ε=0.00 < 0.46
- `rfc9293_interoperability` — Snare classification but ε=0.20 < 0.46
- `rfc9293_state_machine` — Mountain classification but ε=0.20 > 0.15
- `rotmigration_decision_threshold` — Mountain classification but ε=0.30 > 0.15
- `s1_visa` — Snare classification but ε=0.20 < 0.46
- `s1_visa_judgment_sharing_agreement` — Snare classification but ε=0.40 < 0.46
- `sat_csp_complexity` — Mountain classification but ε=0.40 > 0.15
- `shannon_entropy_limit` — Snare classification but ε=0.10 < 0.46
- `shitty_feedback_handling` — Snare classification but ε=0.40 < 0.46
- `skolems_paradox` — Mountain classification but ε=0.20 > 0.15
- `social_narrative_casting` — Snare classification but ε=0.30 < 0.46
- `somatic_focusing_awareness` — Snare classification but ε=0.20 < 0.46
- `south_china_sea_arbitration_2016_2026` — Snare classification but ε=0.45 < 0.46
- `stable_marriage_coordination` — Mountain classification but ε=0.40 > 0.15
- `strange_attractor_dynamics` — Mountain classification but ε=0.35 > 0.15
- `suslin_hypothesis_undecidability` — Mountain classification but ε=0.20 > 0.15
- `tarski_undefinability` — Mountain classification but ε=0.20 > 0.15
- `thai_article_112_mountain` — Snare classification but ε=0.04 < 0.46
- `three_body_unpredicability` — Mountain classification but ε=0.40 > 0.15
- `trade_secret_law` — Snare classification but ε=0.20 < 0.46
- `transient_event_detection` — Mountain classification but ε=0.45 > 0.15
- `traveling_salesperson_problem` — Snare classification but ε=0.40 < 0.46
- `ulysses_chp03` — Mountain classification but ε=0.52 > 0.15
- `ulysses_chp06` — Mountain classification but ε=0.49 > 0.15
- `ulysses_chp14` — Mountain classification but ε=0.55 > 0.15
- `ulysses_chp15` — Mountain classification but ε=0.58 > 0.15
- `ulysses_chp18` — Mountain classification but ε=0.50 > 0.15
- `unclos_2026` — Mountain classification but ε=0.30 > 0.15
- `van_der_waerden_theorem` — Mountain classification but ε=0.20 > 0.15
- `vienna_quantum_superposition_2026` — Mountain classification but ε=0.60 > 0.15
- `viral_transmission_rates` — Mountain classification but ε=0.70 > 0.15
- `whitehead_problem_undecidability` — Mountain classification but ε=0.20 > 0.15
- `winners_curse` — Mountain classification but ε=0.70 > 0.15
- `zombie_reasoning_2026` — Mountain classification but ε=0.74 > 0.15

## Category F: Corpus Bias Analysis

### F1: Naturalization Rate by Domain

| Domain | Total | Mountain Count | Naturalization Rate |
|--------|------:|--------------:|-------------------:|
| analytical | 2 | 2 | 100.0% |
| artistic | 2 | 2 | 100.0% |
| astrophysical | 1 | 1 | 100.0% |
| biological | 8 | 8 | 100.0% |
| corporate_governance | 1 | 0 | 0.0% |
| ecological | 1 | 0 | 0.0% |
| economic | 46 | 26 | 56.5% |
| environmental | 1 | 1 | 100.0% |
| geopolitical | 1 | 1 | 100.0% |
| health | 3 | 1 | 33.3% |
| legal | 7 | 5 | 71.4% |
| linguistic | 1 | 1 | 100.0% |
| logical | 1 | 1 | 100.0% |
| magical | 1 | 1 | 100.0% |
| mathematical | 39 | 39 | 100.0% |
| mathematics | 2 | 2 | 100.0% |
| medical | 2 | 0 | 0.0% |
| organizational | 1 | 0 | 0.0% |
| philosophical | 6 | 5 | 83.3% |
| physical | 1 | 1 | 100.0% |
| physics | 1 | 1 | 100.0% |
| political | 36 | 14 | 38.9% |
| psychological | 4 | 2 | 50.0% |
| religious | 4 | 4 | 100.0% |
| scientific | 6 | 6 | 100.0% |
| social | 53 | 26 | 49.1% |
| technological | 108 | 81 | 75.0% |
| technology | 1 | 0 | 0.0% |
| unknown | 2 | 0 | 0.0% |

### F2: Theater Coverage Rate by Domain

| Domain | Total | Theater Covered | Coverage Rate |
|--------|------:|---------------:|-------------:|
| analytical | 2 | 0 | 0.0% |
| artistic | 2 | 0 | 0.0% |
| astrophysical | 1 | 1 | 100.0% |
| biological | 8 | 3 | 37.5% |
| corporate_governance | 1 | 1 | 100.0% |
| ecological | 1 | 0 | 0.0% |
| economic | 46 | 26 | 56.5% |
| environmental | 1 | 1 | 100.0% |
| geopolitical | 1 | 1 | 100.0% |
| health | 3 | 2 | 66.7% |
| legal | 7 | 0 | 0.0% |
| linguistic | 1 | 0 | 0.0% |
| logical | 1 | 1 | 100.0% |
| magical | 1 | 0 | 0.0% |
| mathematical | 39 | 2 | 5.1% |
| mathematics | 2 | 0 | 0.0% |
| medical | 2 | 2 | 100.0% |
| organizational | 1 | 1 | 100.0% |
| philosophical | 6 | 3 | 50.0% |
| physical | 1 | 1 | 100.0% |
| physics | 1 | 0 | 0.0% |
| political | 36 | 28 | 77.8% |
| psychological | 4 | 2 | 50.0% |
| religious | 4 | 0 | 0.0% |
| scientific | 6 | 1 | 16.7% |
| social | 53 | 29 | 54.7% |
| technological | 108 | 39 | 36.1% |
| technology | 1 | 1 | 100.0% |
| unknown | 2 | 1 | 50.0% |

### F3: Claim Distribution for High-Extraction Constraints (ε > 0.46)

| Claimed Type | Count |
|-------------|------:|
| tangled_rope | 56 |
| piton | 23 |
| enforcement | 11 |
| snare | 11 |
| mountain | 8 |
| coordination | 8 |
| constructed | 4 |
| rope | 1 |

---
*Report generated by classification_audit.py on 2026-02-08 02:01:33*