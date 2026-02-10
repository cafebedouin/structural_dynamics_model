# Stress Test: Pre-Rebuild vs Current Classification Comparison

**Generated:** 2026-02-10 11:27:06
**Baseline commit:** `8150d65` (pre-rebuild)
**Current:** HEAD

## 1. Summary

### Corpus Comparison

| Metric | Count |
|--------|-------|
| Old corpus constraints | 467 |
| Current corpus constraints | 731 |
| Shared (intersection) | 403 |
| Added in current | 328 |
| Removed from old | 64 |
| Type changed (shared) | 279 |
| Type unchanged (shared) | 124 |

### Type Distribution Side-by-Side

| Type | Old Count | Old % | New Count | New % |
|------|-----------|-------|-----------|-------|
| NONE | 108 | 23.1% | 1 | 0.1% |
| mountain | 141 | 30.2% | 165 | 22.6% |
| noose | 135 | 28.9% | 0 | 0.0% |
| piton | 0 | 0.0% | 87 | 11.9% |
| rope | 83 | 17.8% | 64 | 8.8% |
| scaffold | 0 | 0.0% | 15 | 2.1% |
| snare | 0 | 0.0% | 52 | 7.1% |
| tangled_rope | 0 | 0.0% | 347 | 47.5% |

## 2. Claimed Type Changes

**279 constraints** changed `claimed_type` across **15 migration paths**.

### NONE -> tangled_rope (53 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| 26usc469_real_estate_exemption | legal | 0.50 | 0.75 | 0.65 | 0.80 |
| academic_peer_review_gatekeeping | economic | 0.70 | 0.75 | 0.50 | 0.55 |
| advice_as_dangerous_gift | social | 0.20 | 0.20 | 0.40 | 0.40 |
| bip_narrative_illusion | philosophical | 0.85 | 0.85 | 0.75 | 0.75 |
| castration_longevity_choice | technological | 0.80 | 0.80 | 0.50 | 0.50 |
| china_critical_mineral_chokepoint | economic | 0.85 | 0.85 | 0.70 | 0.70 |
| climate_target_one_point_five | political | 0.30 | 0.30 | 0.60 | 0.60 |
| cloudflare_dual_class_asymmetry | economic | 0.80 | 0.80 | 0.90 | 0.90 |
| cognitive_diversity_arbitrage | economic | 0.72 | 0.72 | 0.45 | 0.45 |
| cognitive_induction_gap | psychological | 0.70 | 0.70 | 0.60 | 0.60 |
| cognitive_mimicry_arbitrage | technological | 0.82 | 0.82 | 0.55 | 0.55 |
| conversational_dogmas_interuption | social | 0.30 | 0.55 | 0.60 | 0.60 |
| electrification_scale_2026 | technological | 0.15 | 0.15 | 0.50 | 0.50 |
| ergot_grain_poisoning | social | 0.80 | 0.80 | 0.70 | 0.70 |
| extraordinary_narrative_shift | social | 0.40 | 0.40 | 0.40 | 0.40 |
| family_estrangement_ratio | social | 0.80 | 0.80 | 0.70 | 0.70 |
| gale_shapley | economic | 0.80 | 0.80 | 0.80 | 0.80 |
| germline_regulation_threshold_2026 | political | 0.50 | 0.50 | 0.80 | 0.80 |
| gita_kurukshetra | religious | 0.20 | 0.20 | 0.90 | 0.90 |
| happiness_of_others | social | 0.75 | 0.75 | 0.50 | 0.50 |
| individual_revolution_autonomy | political | 0.75 | 0.75 | 0.80 | 0.80 |
| insult_wisdom_training | religious | 0.75 | 0.75 | 0.50 | 0.50 |
| job_hunt_volume_system_2026 | economic | 0.75 | 0.75 | 0.60 | 0.60 |
| landscape_of_fear_2026 | biological | 0.40 | 0.40 | 0.70 | 0.70 |
| lehman_repo_105 | economic | 0.90 | 0.90 | 0.85 | 0.85 |
| lsd_microdosing_professional_openness | psychological | 0.40 | 0.40 | 0.50 | 0.50 |
| microbiome_symbiosis | biological | 0.45 | 0.45 | 0.65 | 0.65 |
| misunderstanding_as_mismatch | social | 0.75 | 0.75 | 0.70 | 0.70 |
| negative_emissions_arbitrage | economic | 0.82 | 0.82 | 0.55 | 0.55 |
| net_zero_stabilization | scientific | 0.85 | 0.85 | 0.45 | 0.45 |
| neural_interoperability | technological | 0.85 | 0.85 | 0.50 | 0.50 |
| neurodiversity_spectrum | social | 0.72 | 0.72 | 0.55 | 0.55 |
| parable_fish_turtle | philosophical | 0.70 | 0.70 | 0.85 | 0.85 |
| personalized_nutritional_arbitrage | economic | 0.65 | 0.65 | 0.40 | 0.40 |
| planetary_boundaries | environmental | 0.80 | 0.80 | 0.80 | 0.80 |
| planetary_diet_constraint_2026 | ecological | 0.10 | 0.10 | 0.60 | 0.60 |
| poetic_verse_and_past | social | 0.75 | 0.75 | 0.60 | 0.60 |
| politeness_face_negotiation | social | 0.65 | 0.65 | 0.50 | 0.50 |
| quellcrist_falconer_justice | political | 0.90 | 0.90 | 0.80 | 0.80 |
| rotation_seven_kubo_ranking | economic | 0.75 | 0.75 | 0.90 | 0.90 |
| rules_based_international_order | political | 0.75 | 0.75 | 0.60 | 0.60 |
| scurvy_maritime_extraction | biological | 0.90 | 0.90 | 0.70 | 0.70 |
| shobies_existential_commitment | social | 0.65 | 0.65 | 0.50 | 0.50 |
| silicon_lexicon_overload | linguistic | 0.80 | 0.80 | 0.70 | 0.70 |
| smartphone_ubiquity | technological | 0.75 | 0.75 | 0.50 | 0.50 |
| steinmetz_valuation_asymmetry | economic | 0.90 | 0.90 | 0.50 | 0.50 |
| suanne_coup_of_peace | social | 0.80 | 0.80 | 0.70 | 0.70 |
| suanne_face_restoration | social | 0.75 | 0.75 | 0.65 | 0.65 |
| teaching_horses_to_sing | political | 0.75 | 0.75 | 0.40 | 0.40 |
| temporal_scale_arbitrage | technological | 0.76 | 0.76 | 0.40 | 0.40 |
| theory_of_visitors | social | 0.65 | 0.65 | 0.50 | 0.50 |
| toxic_social_infection | social | 0.90 | 0.90 | 0.70 | 0.70 |
| transformer_self_attention | technological | 0.75 | 0.75 | 0.40 | 0.40 |

### noose -> tangled_rope (35 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| ancient_grudge_verona | social | 0.80 | 0.80 | 0.70 | 0.70 |
| apartheid_nuclear_program | political | 0.85 | 0.85 | 0.90 | 0.90 |
| blackstone_conflicts_of_interest | corporate_governance | 0.75 | 0.75 | 0.85 | 0.85 |
| blackstone_smd_control | economic | 0.80 | 0.80 | 0.90 | 0.90 |
| blackstone_tax_receiveable_agreement | economic | 0.85 | 0.85 | 0.95 | 0.95 |
| carrying_capacity | economic | 0.70 | 0.70 | 0.60 | 0.60 |
| cobra_effect | economic | 0.60 | 0.60 | 0.40 | 0.40 |
| cognitive_surrender_to_system_3 | technological | 0.65 | 0.65 | 0.75 | 0.75 |
| dunning_kruger_effect | social | 0.30 | 0.55 | 0.70 | 0.75 |
| factional_instability | political | 0.70 | 0.70 | 0.50 | 0.50 |
| framing_effect | social | 0.50 | 0.55 | 0.50 | 0.50 |
| frankenstein_creation_hubris | technological | 0.90 | 0.90 | 0.85 | 0.85 |
| gig_economy_algorithmic_managment | economic | 0.80 | 0.80 | 0.70 | 0.70 |
| hanlons_razor | social | 0.20 | 0.55 | 0.40 | 0.40 |
| institutional_mutation_domestication | political | 0.70 | 0.70 | 0.80 | 0.80 |
| jevons_paradox | economic | 0.60 | 0.60 | 0.50 | 0.50 |
| mandatrophic_margin_collapse | institutional | 0.85 | 0.85 | 0.70 | 0.70 |
| mandatrophic_margin_collapse_diagnostic | institutional | 0.90 | 0.90 | 0.75 | 0.75 |
| necessary_day_job | economic | 0.65 | 0.65 | 0.70 | 0.70 |
| planning_fallacy | economic | 0.50 | 0.55 | 0.60 | 0.60 |
| qualified_immunity | political | 0.80 | 0.80 | 0.70 | 0.70 |
| regulatory_capture | economic | 0.80 | 0.80 | 0.70 | 0.20 |
| russian_war_cannibalization | political | 0.80 | 0.80 | 0.75 | 0.75 |
| sapir_whorf_hypothesis | social | 0.50 | 0.55 | 0.70 | 0.70 |
| sludge_bureaucratic_friction | political | 0.70 | 0.70 | 0.60 | 0.60 |
| social_loafing | social | 0.50 | 0.55 | 0.40 | 0.45 |
| social_media_participation_threshold | social | 0.60 | 0.60 | 0.50 | 0.50 |
| statecraft_virtu | political | 0.70 | 0.70 | 0.90 | 0.90 |
| taiwan_strait_hegemony_shift | political | 0.90 | 0.90 | 0.80 | 0.80 |
| texas_hispanic_political_pivot | political | 0.70 | 0.70 | 0.60 | 0.60 |
| the_calm_protocol_suppression | social | 0.75 | 0.75 | 0.80 | 0.80 |
| tragedy_of_the_commons | economic | 0.70 | 0.70 | 0.40 | 0.40 |
| trump_making_china_great_2026 | political | 0.65 | 0.65 | 0.40 | 0.40 |
| us_suburban_zoning_2025 | political | 0.70 | 0.70 | 0.65 | 0.65 |
| us_two_party_duopoly | political | 0.75 | 0.75 | 0.80 | 0.80 |

### rope -> tangled_rope (30 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| academic_tenure_system | economic | 0.75 | 0.75 | 0.60 | 0.60 |
| ai_driven_surveillance_sensor_layer | technological | 0.75 | 0.75 | 0.80 | 0.80 |
| ai_evaluators_matching | None | 0.75 | 0.75 | 0.80 | 0.80 |
| carbon_credit_markets_2026 | economic | 0.55 | 0.55 | 0.60 | 0.60 |
| cbdc_implementation | economic | 0.80 | 0.80 | 0.70 | 0.70 |
| college_admissions_market | social | 0.70 | 0.70 | 0.50 | 0.50 |
| compounding_logic | economic | 0.50 | 0.50 | 0.40 | 0.40 |
| constitutional_consecration | political | 0.60 | 0.60 | 0.90 | 0.90 |
| copyright_protection | social | 0.40 | 0.40 | 0.50 | 0.50 |
| digital_credentialing_verification | technological | 0.65 | 0.65 | 0.75 | 0.75 |
| emrgency_medicine_clinical_guidelines | medical | 0.60 | 0.60 | 0.50 | 0.50 |
| ergo_mixer_protocol | social | 0.10 | 0.10 | 0.10 | 0.10 |
| ergo_rosen_bridge_protocol | technological | 0.50 | 0.50 | 0.20 | 0.20 |
| ergo_storage_rent | technological | N/A | 0.55 | N/A | 0.20 |
| ergo_storage_rent_mechanism | economic | 0.15 | 0.15 | 0.80 | 0.80 |
| golden_handcuffs | economic | 0.40 | 0.40 | 0.20 | 0.20 |
| grete_samsa_transition | social | 0.60 | 0.75 | 0.50 | 0.50 |
| harm_principle_liberty | political | 0.50 | 0.50 | 0.80 | 0.80 |
| integrated_digital_governance_stack | technological | 0.90 | 0.90 | 0.95 | 0.95 |
| japanese_energy_scaffold_2025 | economic | 0.50 | 0.55 | 0.60 | 0.60 |
| matching_markets_general | economic | 0.40 | 0.40 | 0.70 | 0.70 |
| medical_residency_match | economic | 0.60 | 0.60 | 0.95 | 0.95 |
| mil_std_461g_emi_control | technological | 0.50 | 0.50 | 0.40 | 0.40 |
| mil_std_810f_tailoring | technological | 0.50 | 0.51 | 0.40 | 0.45 |
| monetary_regime_transition | economic | 0.50 | 0.55 | 0.40 | 0.40 |
| non_compete_agreements | economic | 0.80 | 0.80 | 0.70 | 0.70 |
| self_surpassing_superman | philosophical | 0.80 | 0.80 | 0.70 | 0.70 |
| taiwan_existential_sovereignty | political | 0.70 | 0.70 | 0.80 | 0.80 |
| trade_secret_law | legal | 0.20 | 0.20 | 0.30 | 0.30 |
| union_protection_underperformance | economic | 0.60 | 0.60 | 0.50 | 0.50 |

### noose -> mountain (27 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| ai_professional_displacement | economic | 0.70 | 0.71 | 0.80 | 0.80 |
| asce_7_22_seismic_design | technological | 0.20 | 0.20 | 0.70 | 0.70 |
| availability_heuristic | social | 0.40 | 0.40 | 0.70 | 0.70 |
| basel_problem_convergence | mathematical | 0.20 | 0.20 | 0.10 | 0.10 |
| burden_of_proof_scientific_empirical | technological | 0.30 | 0.30 | 0.60 | 0.60 |
| continuum_hypothesis_undecidability | mathematical | 0.15 | 0.15 | 0.25 | 0.25 |
| dark_patterns_manipulation | technological | 0.85 | 0.85 | 0.90 | 0.90 |
| ergodic_theorems | scientific | 0.30 | 0.30 | 0.60 | 0.60 |
| euler_characteristic_topology | mathematical | 0.10 | 0.10 | 0.10 | 0.10 |
| feigenbaum_universality | mathematical | 0.30 | 0.30 | 0.20 | 0.20 |
| gauss_bonnet_topology | mathematical | 0.10 | 0.10 | 0.05 | 0.05 |
| goodharts_law | economic | 0.50 | 0.50 | 0.70 | 0.70 |
| greenland_seizure_trade_war | geopolitical | 0.85 | 0.85 | 0.75 | 0.75 |
| hilberts_hotel_infinity | mathematical | 0.15 | 0.15 | 0.10 | 0.10 |
| iran_mandatrophic_collapse | political | 0.90 | 0.90 | 0.85 | 0.85 |
| kleene_recursion_theorem | technological | 0.20 | 0.20 | 0.15 | 0.15 |
| lowenheim_skolem_theorem | technological | 0.20 | 0.20 | 0.40 | 0.40 |
| minimax_decision_rule | technological | 0.30 | 0.30 | 0.20 | 0.20 |
| p_vs_np | technological | 0.40 | 0.40 | 0.20 | 0.20 |
| royal_navy_middle_east_withdrawal | political | 0.80 | 0.80 | 0.50 | 0.50 |
| sat_csp_complexity | technological | 0.40 | 0.40 | 0.50 | 0.50 |
| sturgeons_law | artistic | 0.10 | 0.10 | 0.40 | 0.40 |
| tarski_undefinability | technological | 0.20 | 0.20 | 0.30 | 0.30 |
| the_bacchae_madness_protocol | religious | 0.95 | 0.95 | 0.85 | 0.85 |
| trumps_second_term_authoritarianism_2026 | political | 0.85 | 0.85 | 0.90 | 0.90 |
| whitehead_problem_undecidability | mathematical | 0.20 | 0.20 | 0.30 | 0.30 |
| winners_curse | economic | 0.60 | 0.70 | 0.50 | 0.60 |

### NONE -> mountain (27 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| big_data_astrophysics_arbitrage | technological | 0.75 | 0.75 | 0.45 | 0.45 |
| brain_network_paradigm_2026 | technological | 0.00 | 0.00 | 0.70 | 0.70 |
| broke_vs_poor_grocery_math | economic | 0.80 | 0.80 | 0.60 | 0.60 |
| bushman_money_magic | economic | 0.90 | 0.90 | 0.80 | 0.80 |
| challenger_o_ring_integrity | technological | 0.80 | 0.80 | 0.70 | 0.70 |
| dldr_information_policy | technological | 0.35 | 0.35 | 0.20 | 0.20 |
| elencher_identity_transformation | technological | 0.10 | 0.10 | 0.20 | 0.20 |
| empty_tomb_transformation | religious | 0.40 | 0.40 | 0.40 | 0.40 |
| finite_pool_of_worry | psychological | 0.75 | 0.75 | 0.65 | 0.65 |
| hiv_prep_prevention_2026 | technological | 0.15 | 0.15 | 0.50 | 0.50 |
| keltner_relationship_evaluation | social | 0.35 | 0.35 | 0.45 | 0.45 |
| khantivadin_radical_patience | religious | 1.00 | 1.00 | 0.90 | 0.90 |
| kjv_linguistic_residue | linguistic | 0.10 | 0.10 | 0.20 | 0.20 |
| kjv_textual_authority | religious | 0.40 | 0.40 | 0.70 | 0.70 |
| omelet_perfection_complexity | social | 0.25 | 0.25 | 0.45 | 0.45 |
| openbsd_netiquette_protocol | technological | 0.30 | 0.30 | 0.60 | 0.60 |
| postman_survival_protocol | social | 0.40 | 0.40 | 0.50 | 0.50 |
| quantam_decryption_risk_2026 | technological | 0.80 | 0.80 | 0.30 | 0.30 |
| quantum_nonlocality_2026 | scientific | 0.00 | 0.00 | 0.80 | 0.80 |
| rfc9293_state_machine | technological | 0.20 | 0.20 | 0.60 | 0.60 |
| rotation_seven_black_soil | biological | 0.95 | 0.95 | 0.10 | 0.10 |
| shannon_entropy_limit | mathematical | 0.10 | 0.10 | 0.20 | 0.20 |
| social_narrative_casting | social | 0.30 | 0.30 | 0.60 | 0.60 |
| sts86_ascent_checklist | technological | 0.05 | 0.05 | 0.95 | 0.95 |
| utopia_apocalypse_fragility | social | 0.70 | 0.70 | 0.60 | 0.60 |
| wikipedia_crowdsourcing_2026 | technological | 0.05 | 0.05 | 0.40 | 0.40 |
| wikipedia_notability_requirement_2026 | social | 0.40 | 0.40 | 0.70 | 0.70 |

### noose -> snare (24 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| adverse_possession | economic | 0.70 | 0.65 | 0.40 | 0.45 |
| algorithmic_bias | technological | 0.60 | 0.75 | 0.70 | 0.70 |
| arctic_geopolitical_flashpoint | geopolitical | 0.75 | 0.75 | 0.80 | 0.80 |
| authoritarian_power_paradox | political | 0.80 | 0.80 | 0.90 | 0.90 |
| bay_of_pigs_operational_silo | political | 0.90 | 0.90 | N/A | 0.80 |
| biological_curiosity | biological | 0.15 | 0.15 | 0.20 | 0.20 |
| burden_of_proof_engineering_safety | technological | 0.90 | 0.90 | 0.80 | 0.80 |
| confirmation_bias | social | 0.30 | 0.30 | 0.80 | 0.80 |
| cuba_mandatrophic_collapse | political | 0.95 | 0.95 | 0.85 | 0.85 |
| currys_paradox | analytical | 0.20 | 0.20 | 0.50 | 0.50 |
| dead_sea_effect | social | 0.50 | 0.50 | 0.40 | 0.40 |
| endowment_effect | economic | 0.30 | 0.30 | 0.40 | 0.40 |
| faint_blue_neural_bifurcation | technological | 0.90 | 0.90 | 0.80 | 0.80 |
| hawthorne_effect | social | 0.40 | 0.40 | 0.50 | 0.50 |
| lula_hemisphere_2026 | political | 0.75 | 0.75 | 0.85 | 0.85 |
| micro_robot_electronics_integration | technological | 0.20 | 0.20 | 0.60 | 0.60 |
| nasa_faster_better_cheaper | political | 0.80 | 0.80 | 0.70 | 0.70 |
| north_korea_songun_mandatrophy | political | 0.95 | 0.95 | 0.90 | 0.95 |
| qwerty_vs_dvorak | technological | 0.40 | 0.40 | 0.70 | 0.70 |
| rogers_commission_institutional_analysis | political | 0.85 | 0.85 | 0.75 | 0.75 |
| skolems_paradox | technological | 0.20 | 0.20 | 0.40 | 0.40 |
| starwars_evolutionary_mutation | social | 0.75 | 0.75 | 0.80 | 0.80 |
| sunk_cost_fallacy | economic | 0.50 | 0.50 | 0.60 | 0.60 |
| trojan_war_spoils | military | 1.00 | 1.00 | 1.00 | 1.00 |

### mountain -> tangled_rope (24 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| ai_task_horizon_reliability | technological | 0.45 | 0.48 | 0.60 | 0.60 |
| china_taiwan_reunification_mandate | political | 0.80 | 0.80 | 0.90 | 0.90 |
| cia_fbi_legal_wall | political | 0.70 | 0.70 | 0.85 | 0.85 |
| coinbase_regulatory_uncertainty | political | 0.70 | 0.70 | 0.90 | 0.90 |
| colorado_sbe_decentralization_friction | political | 0.70 | 0.70 | 0.60 | 0.60 |
| couples_residency_match | technological | 0.50 | 0.50 | 0.95 | 0.95 |
| dionysaic_frenzy | religious | 0.80 | 0.80 | 0.90 | 0.90 |
| exploration_vs_exploitation | technological | 0.30 | 0.55 | 0.40 | 0.65 |
| genetic_predisposition | technological | 0.60 | 0.60 | 0.50 | 0.50 |
| gilgamesh_mortality_limit | philosophical | 1.00 | 1.00 | 0.90 | 0.90 |
| greshams_law | economic | 0.60 | 0.60 | 0.70 | 0.70 |
| hammurabi | political | 0.50 | 0.50 | 0.80 | 0.80 |
| innovators_dilemma | economic | 0.50 | 0.55 | 0.70 | 0.70 |
| iron_law_of_oligarchy | political | 0.60 | 0.60 | 0.60 | 0.60 |
| medieval_church_hegemony | religious | 0.70 | 0.70 | 0.90 | 0.90 |
| nash_equilibrium_coordination | economic | 0.50 | 0.52 | 0.40 | 0.45 |
| network_effects | economic | 0.50 | 0.55 | 0.60 | 0.60 |
| parkinsons_law | organizational | 0.50 | 0.50 | 0.40 | 0.40 |
| peter_principle | organizational | 0.40 | 0.55 | 0.50 | 0.60 |
| radiologic_diagnostic_threshold | medical | 0.70 | 0.70 | 0.60 | 0.60 |
| s1_airbnb | economic | 0.60 | 0.60 | 0.80 | 0.80 |
| st_petersburg_paradox | mathematical | 0.65 | 0.65 | 0.40 | 0.40 |
| stoic_logos_governance | philosophical | 0.70 | 0.75 | 0.80 | 0.80 |
| zipfs_law | technological | 0.50 | 0.75 | 0.70 | 0.70 |

### noose -> piton (17 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| legacy_system_technical_debt | technological | 0.80 | 0.03 | 0.60 | 0.04 |
| mco_unit_system_discontinuity | technological | 0.90 | 0.30 | 0.70 | 0.10 |
| railway_gauge_standard | technological | 0.30 | 0.30 | 0.80 | 0.20 |
| ulysses_chp01 | social | 0.80 | 0.48 | 0.80 | 0.65 |
| ulysses_chp02 | economic | 0.70 | 0.49 | 0.70 | 0.72 |
| ulysses_chp03 | philosophical | 0.50 | 0.52 | 0.70 | 0.85 |
| ulysses_chp04 | social | 0.40 | 0.47 | 0.50 | 0.60 |
| ulysses_chp05 | social | 0.40 | 0.48 | 0.80 | 0.70 |
| ulysses_chp06 | social | 0.50 | 0.49 | 0.70 | 0.95 |
| ulysses_chp07 | technological | 0.40 | 0.51 | 0.50 | 0.75 |
| ulysses_chp08 | social | 0.60 | 0.50 | 0.40 | 0.80 |
| ulysses_chp10 | social | 0.40 | 0.48 | 0.60 | 0.75 |
| ulysses_chp11 | social | 0.60 | 0.53 | 0.70 | 0.82 |
| ulysses_chp12 | social | 0.60 | 0.54 | 0.70 | 0.88 |
| ulysses_chp13 | social | 0.50 | 0.49 | 0.50 | 0.75 |
| ulysses_chp15 | social | 0.60 | 0.58 | 0.80 | 0.90 |
| ulysses_chp16 | social | 0.50 | 0.47 | 0.60 | 0.70 |

### NONE -> rope (13 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| click_chemistry_paradigm_2026 | scientific | 0.10 | 0.10 | 0.30 | 0.30 |
| climate_attribution_2026 | scientific | 0.20 | 0.20 | 0.30 | 0.30 |
| crispr_genomic_rewrite_2026 | technological | 0.40 | 0.40 | 0.20 | 0.20 |
| e2ee_digital_privacy_2026 | technological | 0.05 | 0.05 | 0.50 | 0.50 |
| epigenetics_complexity_2026 | biological | 0.10 | 0.10 | 0.20 | 0.20 |
| exoplanet_habitability_arbitrage | technological | 0.45 | 0.45 | 0.55 | 0.55 |
| hominin_evolutionary_bottleneck | scientific | 0.75 | 0.75 | 0.60 | 0.60 |
| kjv_puritan_new_world_exit | political | 0.20 | 0.20 | 0.50 | 0.50 |
| rfc9293_interoperability | technological | 0.20 | 0.20 | 0.70 | 0.70 |
| sadhu_integrity_protocol | social | 0.30 | 0.30 | 0.40 | 0.40 |
| solar_system_weirdness | technological | 0.20 | 0.20 | 0.60 | 0.60 |
| somatic_focusing_awareness | social | 0.20 | 0.20 | 0.30 | 0.30 |
| transient_event_detection | technological | 0.45 | 0.45 | 0.50 | 0.50 |

### NONE -> snare (11 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| ai_cognitive_diversity_arbitrage | technological | 0.72 | 0.72 | 0.45 | 0.45 |
| amish_technological_renunciation | social | 0.80 | 0.80 | 0.75 | 0.75 |
| asshole_filter_2015 | psychological | 0.75 | 0.75 | 0.60 | 0.60 |
| bedouin_sedentary_transition | social | 0.75 | 0.75 | 0.60 | 0.60 |
| generational_replacement_inertia | social | 0.75 | 0.75 | 0.80 | 0.80 |
| kjv_great_awakening | religious | 0.10 | 0.10 | 0.40 | 0.40 |
| layered_brain_processing | technological | 0.35 | 0.35 | 0.75 | 0.75 |
| med_diet_consensus_2026 | health | 0.20 | 0.20 | 0.40 | 0.40 |
| rotation_seven_isolation | political | 0.85 | 0.85 | 0.70 | 0.70 |
| the_churn_systemic_upheaval | political | 0.90 | 0.90 | 0.80 | 0.80 |
| the_wall_procedural_barrier | legal | 0.80 | 0.80 | 0.90 | 0.90 |

### mountain -> snare (6 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| ergo_autolykos_asic_resistance | technological | 0.20 | 0.20 | 0.85 | 0.85 |
| gamblers_ruin_stochastic_extinction | mathematical | 0.90 | 0.90 | 0.50 | 0.50 |
| metamorphosis_samsa | economic | 0.80 | 0.80 | 0.70 | 0.70 |
| russells_paradox_self_reference | mathematical | 0.70 | 0.70 | 0.40 | 0.40 |
| thermodynamics_entropy | technological | 0.80 | 0.80 | 0.70 | 0.95 |
| tractarian_logic_limit | philosophical | 0.70 | 0.70 | 0.95 | 0.95 |

### noose -> rope (5 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| berkshire_compounding_culture | economic | 0.10 | 0.10 | 0.20 | 0.20 |
| fair_use_doctrine | social | 0.10 | 0.10 | 0.40 | 0.40 |
| platform_cooperativism_governance | economic | 0.05 | 0.05 | 0.20 | 0.20 |
| quantum_entanglement_protocol | technological | 0.20 | 0.20 | 0.95 | 0.95 |
| shitty_feedback_handling | social | 0.40 | 0.40 | 0.30 | 0.30 |

### rope -> piton (3 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| hoa_covenants | economic | 0.45 | 0.55 | 0.50 | 0.30 |
| ulysses_chp17 | technological | 0.40 | 0.46 | 0.60 | 0.95 |
| ulysses_chp18 | social | 0.40 | 0.50 | 0.50 | 0.96 |

### mountain -> rope (2 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| ergo_nipopows | technological | 0.10 | 0.10 | 0.05 | 0.10 |
| kidney_exchange_market | social | 0.10 | 0.10 | 0.90 | 0.90 |

### mountain -> piton (2 constraints)

| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |
|------------|--------|---------|---------|---------|---------|
| ulysses_chp09 | social | 0.40 | 0.50 | 0.60 | 0.70 |
| ulysses_chp14 | biological | 0.30 | 0.55 | 0.40 | 0.95 |

## 3. Perspective Changes

**305 constraints** had perspective-level classification changes
(after filtering Prolog artifacts and null-context entries).

- Overt (claimed_type also changed): 249
- Silent (claimed_type unchanged, perspectives differ): 56

### Silent Reclassifications

These constraints kept the same `claimed_type` but their perspective arrays changed:

| Constraint | Claimed Type | Old Perspectives | New Perspectives |
|------------|-------------|------------------|------------------|
| antifragility | mountain | mountain:1 | mountain:1, rope:1, snare:1, tangled_rope:1 |
| automatic_enrollment_defaults | rope | rope:1 | mountain:1, rope:1, snare:1 |
| banach_fixed_point | mountain | mountain:1 | mountain:3 |
| birthday_paradox_collison | mountain |  | mountain:1 |
| burali_forte_paradox | mountain |  | mountain:1 |
| choice_architecture_design | rope |  | rope:1 |
| coinbase_crypto_volatility | mountain | noose:1 | snare:1 |
| columbia_2026_elections | rope |  | rope:1 |
| constitutional_supremacy | mountain |  | mountain:1, rope:1, snare:1 |
| copyleft_viral_licensing | rope | rope:1 | mountain:2, rope:1, snare:1 |
| cost_of_observation | mountain | mountain:1 | mountain:1, rope:1, snare:1 |
| cow_field_poop | mountain | rope:1 | mountain:1, rope:1, snare:1 |
| creative_commons_licensing | rope | rope:1 | rope:2, snare:1 |
| cuban_missile_crisis_excomm_delibration | rope |  | mountain:1 |
| deferential_realism_core | rope |  | rope:1 |
| educational_unbundling_implementation | rope |  | rope:1 |
| ergo_dexy_gold_protocol | rope |  | rope:1 |
| ergo_lets_protocol | rope | rope:1 | mountain:2, rope:1, scaffold:1, snare:1 |
| ergo_sig_usd_protocol | rope |  | mountain:1 |
| family_succession_and_decadence | mountain |  | mountain:1, rope:1, tangled_rope:1 |
| fast_growing_hierarchy | mountain | rope:1 | mountain:3 |
| finite_simple_groups_classification | mountain |  | mountain:1 |
| fmeca_procedures_1980 | rope | noose:1 | snare:1 |
| genetic_algorithms_evolution | rope | rope:1 | mountain:1, rope:1, snare:1 |
| godels_incompleteness_theorems | mountain |  | mountain:1 |
| hamiltonian_path_complexity | mountain | noose:1 | mountain:1, rope:1, snare:1 |
| helsinki_bus_theory | rope | noose:1 | mountain:1, rope:1, snare:1, tangled_rope:1 |
| heuristic_optimization | rope | rope:1 | mountain:1, rope:1, scaffold:1, snare:1 |
| hydra_game | mountain | rope:1 | mountain:1, rope:1, snare:1 |
| large_cardinals_foundations | mountain |  | mountain:1 |
| lindy_effect | mountain | mountain:1 | mountain:1, rope:2, snare:1 |
| local_vs_global_optima | mountain | noose:1 | mountain:2, rope:1, snare:1 |
| marriage_problem | mountain |  | snare:1 |
| mars_rovers_navigational_autonomy | rope |  | mountain:1 |
| material_tensile_strength | mountain | rope:1 | mountain:3 |
| max_flow | mountain |  | mountain:1 |
| pareto_principle | mountain | mountain:1 | mountain:1, rope:2, snare:1 |
| permissive_software_licensing | rope | rope:1 | mountain:1, rope:1, scaffold:1, snare:1 |
| poincare_conjucture | mountain |  | mountain:1 |
| prisoners_dilemma_equilibrium | mountain | mountain:1 | mountain:3 |
| private_identity_integration | rope | noose:1 | snare:1 |
| proof_of_work_consensus | rope |  | mountain:1, rope:1, scaffold:1, snare:1 |
| public_domain_commons | mountain | rope:1 | mountain:1, rope:1, scaffold:1, snare:1 |
| pythagorean_theorem_geometric_constancy | mountain |  | rope:1 |
| relativity_of_simultaneity | mountain | mountain:1 | mountain:2, rope:1 |
| relativity_physical_invariance | mountain | noose:1 | mountain:1, rope:1, scaffold:1, snare:1 |
| rotmigration_decision_threshold | mountain |  | mountain:1 |
| s1_visa | mountain |  | snare:1 |
| s1_visa_judgment_sharing_agreement | rope |  | snare:1 |
| south_china_sea_arbitration_2016_2026 | mountain | noose:1 | snare:1 |
| ... | ... | ... | ... |

*(6 additional silent reclassifications omitted)*

## 4. Metric Drift

**35 constraints** had extractiveness or suppression shift >0.1.

| Constraint | Domain | Old Type | New Type | Ext Delta | Sup Delta |
|------------|--------|----------|----------|-----------|-----------|
| banach_fixed_point | logical | mountain | mountain | -0.19 | +0.79 |
| legacy_system_technical_debt | technological | noose | piton | -0.77 | -0.56 |
| square_cube_law | technological | mountain | mountain | -0.49 | -0.69 |
| mco_unit_system_discontinuity | technological | noose | piton | -0.60 | -0.60 |
| railway_gauge_standard | technological | noose | piton | +0.00 | -0.60 |
| prisoners_dilemma_equilibrium | logical | mountain | mountain | -0.60 | -0.35 |
| fast_growing_hierarchy | technological | mountain | mountain | -0.55 | -0.39 |
| ulysses_chp14 | biological | mountain | piton | +0.25 | +0.55 |
| regulatory_capture | economic | noose | tangled_rope | +0.00 | -0.50 |
| material_tensile_strength | technological | mountain | mountain | -0.48 | -0.39 |
| ulysses_chp18 | social | rope | piton | +0.10 | +0.46 |
| ulysses_chp08 | social | noose | piton | -0.10 | +0.40 |
| hanlons_razor | social | noose | tangled_rope | +0.35 | +0.00 |
| antifragility | technological | mountain | mountain | +0.35 | +0.05 |
| ulysses_chp17 | technological | rope | piton | +0.06 | +0.35 |
| ulysses_chp01 | social | noose | piton | -0.32 | -0.15 |
| conversational_dogmas_interuption | social | NONE | tangled_rope | +0.25 | +0.00 |
| dunning_kruger_effect | social | noose | tangled_rope | +0.25 | +0.05 |
| exploration_vs_exploitation | technological | mountain | tangled_rope | +0.25 | +0.25 |
| 26usc469_real_estate_exemption | legal | NONE | tangled_rope | +0.25 | +0.15 |
| thermodynamics_entropy | technological | mountain | snare | +0.00 | +0.25 |
| ulysses_chp06 | social | noose | piton | -0.01 | +0.25 |
| ulysses_chp07 | technological | noose | piton | +0.11 | +0.25 |
| ulysses_chp13 | social | noose | piton | -0.01 | +0.25 |
| zipfs_law | technological | mountain | tangled_rope | +0.25 | +0.00 |
| ulysses_chp02 | economic | noose | piton | -0.21 | +0.02 |
| hoa_covenants | economic | rope | piton | +0.10 | -0.20 |
| ulysses_chp12 | social | noose | piton | -0.06 | +0.18 |
| algorithmic_bias | technological | noose | snare | +0.15 | +0.00 |
| grete_samsa_transition | social | rope | tangled_rope | +0.15 | +0.00 |
| peter_principle | organizational | mountain | tangled_rope | +0.15 | +0.10 |
| ulysses_chp03 | philosophical | noose | piton | +0.02 | +0.15 |
| ulysses_chp10 | social | noose | piton | +0.08 | +0.15 |
| ulysses_chp11 | social | noose | piton | -0.07 | +0.12 |
| ulysses_chp05 | social | noose | piton | +0.08 | -0.10 |

## 5. Migration Matrix

Rows = old type, Columns = new type. Cells = number of constraints taking that path.

| Old \ New | mountain | piton | rope | snare | tangled_rope | **Total** |
|-----------|----------|-------|-------|-------|--------------|-----------|
| **NONE** | 27 | . | 13 | 11 | 53 | **104** |
| **mountain** | 88 | 2 | 2 | 6 | 24 | **122** |
| **noose** | 27 | 17 | 5 | 24 | 35 | **108** |
| **rope** | . | 3 | 36 | . | 30 | **69** |
| **Total** | **142** | **22** | **56** | **41** | **142** | **403** |
