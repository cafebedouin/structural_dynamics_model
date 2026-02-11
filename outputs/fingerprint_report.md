
============================================================
  FNL SHADOW MODE DIAGNOSTIC TRACE
============================================================

[STEP 1] Loading fnl_shadow_probe through pipeline...

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/fnl_shadow_probe.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: fnl_shadow_probe...
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.5 for suppression(organizational) at T=0
  [FIXED] Imputed 0.5 for resistance(organizational) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.5 for suppression(organizational) at T=10
  [FIXED] Imputed 0.5 for resistance(organizational) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.5 for suppression(class) at T=0
  [FIXED] Imputed 0.5 for resistance(class) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.5 for suppression(class) at T=10
  [FIXED] Imputed 0.5 for resistance(class) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.5 for suppression(individual) at T=0
  [FIXED] Imputed 0.5 for resistance(individual) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.5 for suppression(individual) at T=10
  [FIXED] Imputed 0.5 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: fnl_shadow_probe...

>>> INITIATING DR-AUDIT SUITE: fnl_shadow_probe

[REPAIR] Auditing vectors for: fnl_shadow_probe...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: fnl_shadow_probe (0-10)
Checking Interval: fnl_shadow_probe (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: fnl_shadow_probe

From YOUR perspective (context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))): tangled_rope

From ANALYTICAL perspective: tangled_rope

→ Perspectives AGREE

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] fnl_shadow_probe from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] fnl_shadow_probe from context(agent_power(institutional),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=mountain, computed=tangled_rope
  [INDEX OK] fnl_shadow_probe from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: fnl_shadow_probe ===

[CONSTRAINT INVENTORY]
  No active constraints found.

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 2 | Watch: 0

  fnl_shadow_probe:
    [warning] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.25,0.3)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.4525,decline_signals,[coupling_above_threshold(1.0),excess_above_floor(0.15)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  fnl_shadow_probe -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  fnl_shadow_probe (ε=0.30):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.30 × 1.36 × 0.80 = 0.326
    moderate@national: d=0.700 f(d)=1.11 χ = 0.30 × 1.11 × 1.00 = 0.332
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.30 × -0.04 × 1.00 = -0.013
    analytical@global: d=0.720 f(d)=1.14 χ = 0.30 × 1.14 × 1.20 = 0.411

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: fnl_shadow_probe ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [extractive_immutable,no_exit_for_victims]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=stable  suppression=unknown  theater=stable
  Zone:       extraction=low  suppression=high
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,global-local,1.0),coupled(power_scope,analytical,global-national,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.453 (contaminated)


--- SYSTEM INSIGHTS ---
  Omegas Identified: 0


====================================================
   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[CONSTRAINT INVENTORY: INDEXICAL AUDIT]


Constraint: fnl_shadow_probe
  Claimed Type: mountain
  Perspectives:
    - [context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local))]: snare (Mismatch)
    - [context(agent_power(institutional),time_horizon(generational),exit_options(mobile),spatial_scope(national))]: mountain (Matches Claim)
    - [context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))]: tangled_rope (Mismatch)

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: fnl_shadow_probe]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [severe]: type_1_false_summit detected for fnl_shadow_probe

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  fnl_shadow_probe: false_natural_law (confidence: high)
    → FALSE NATURAL LAW signature for fnl_shadow_probe: Claims naturality (explicit_mountain_claim) but fails Boltzmann independence test. Coupling score=1.000 with 6 coupled dimension pairs. Excess extraction=0.15. This constraint is "physics-washed" — it appears natural but its coupling topology reveals structural construction.

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.50

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: fnl_shadow_probe
    - Individual (Powerless): snare [d=0.900 f(d)=1.36 χ=0.33]
    - Institutional (Manager): mountain [d=0.120 f(d)=-0.04 χ=-0.01 → net benefit]
    ! MANDATROPHY GAP: delta_chi = 0.34 (moderate)

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: fnl_shadow_probe]
  Generated 1 Omega variables from perspectival gaps:

  Ω: omega_learned_helplessness_fnl_shadow_probe (conceptual)
     Question: Constraint fnl_shadow_probe appears extractive (Snare) to individuals but unchangeable (Mountain) to institutions...
     Source: gap(snare_mountain_confusion,snare,mountain)


[OMEGA TRIAGE & PRIORITIZATION]

  [critical] 1 omega(s):
    - omega_learned_helplessness_fnl_shadow_probe (conceptual)
      Constraint fnl_shadow_probe appears extractive (Snare) to individuals but unchangeable (Mountain) to institutions...

  [high] 1 omega(s):
    - omega_learned_helplessness_fnl_shadow_probe (conceptual)
      Constraint fnl_shadow_probe appears extractive (Snare) to individuals but unchangeable (Mountain) to institutions...

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_learned_helplessness_fnl_shadow_probe] CONCEPTUAL CLARIFICATION
  │  Constraint: fnl_shadow_probe
  │  Gap: Constraint fnl_shadow_probe appears extractive (Snare) to individuals but unchangeable (Mountain) to institutions...
  │
  │  CRITICAL: Learned Helplessness Pattern
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: MOUNTAIN (unchangeable law)
  │
  │  RESOLUTION STRATEGY:
  │  1. Test changeability:
  │     - Can institutions modify fnl_shadow_probe?
  │     - What legal/political mechanisms exist?
  │     - Historical precedents of change?
  │  2. Test extraction:
  │     - Is benefit flow symmetric or asymmetric?
  │     - Who has veto power over changes?
  │  3. Decision tree:
  │     IF truly unchangeable + extractive → MANDATROPHY
  │     IF changeable + extractive → Correct to SNARE
  │     IF unchangeable + fair → Correct to MOUNTAIN
  │     IF institutions falsely claim necessity → SNARE + fraud flag
  └─

====================================================

============================================================
  DIAGNOSTIC RESULTS
============================================================

[STEP 2] Metric-based classification (pre-override):
  metric_based_type_indexed = tangled_rope

[STEP 3] Structural signature:
  constraint_signature = false_natural_law

[STEP 4] Boltzmann compliance:
  boltzmann_compliant = non_compliant(1.0,0.3)

[STEP 5] Cross-index coupling:
  cross_index_coupling = 1.0

[STEP 6] Final classification (post-override via dr_type/3):
  dr_type = tangled_rope

============================================================
  OVERRIDE ANALYSIS
============================================================

  MetricType  = tangled_rope
  Signature   = false_natural_law
  FinalType   = tangled_rope

  >>> NO OVERRIDE DETECTED <<<
  MetricType == FinalType. Either the override did not
  fire, or both paths agree on the same type.

============================================================
  OVERRIDE RULE STATUS
============================================================

  Rule NL  (natural_law -> mountain):      ACTIVE (maps to mountain)
  Rule FNL (false_natural_law -> tangled):  ACTIVE (maps to tangled_rope)
  Rule CIR (coupling_invariant -> rope):    ACTIVE (maps to rope)
  Rule FCR (false_ci_rope -> tangled):      ACTIVE (maps to tangled_rope)

============================================================
  END OF DIAGNOSTIC
============================================================

# Logical Fingerprint Report

*Generated: corpus-wide structural fingerprint analysis*

## Summary

- **Constraints analyzed**: 477
- **Distinct shift patterns**: 27

## Shift Pattern Families

Each family groups constraints with identical perspectival shift — same classification at each power level.

### `shift(mountain, rope, rope, mountain)` — 1 constraints

- `milano_cortina_2026`

### `shift(piton, rope, rope, rope)` — 1 constraints

- `gold_piton_2026`

### `shift(rope, rope, rope, rope)` — 22 constraints

- `boundary_protocol`
- `genetic_algorithms_evolution`
- `hamiltonian_path_complexity`
- `kjv_great_awakening`
- `kjv_linguistic_residue`
- `mom_z14_2026`
- `open_source_commons`
- `perseverance_rover_autonomy`
- `planetary_diet_constraint_2026`
- `public_domain_commons`
- `quantum_nonlocality_2026`
- `rafah_crossing_lifeline`
- `rare_earth_coop_2026`
- `relativity_of_simultaneity`
- `relativity_physical_invariance`
- `shannon_entropy_limit`
- `silver_scarcity_2026`
- `spv_variations`
- `swift_piton_snap`
- `udhr_1948`
- `vertebrate_genetics_2026`
- `wikipedia_crowdsourcing_2026`

### `shift(rope, rope, rope, tangled_rope)` — 2 constraints

- `inner_model_theory_constraints`
- `sadhu_integrity_protocol`

### `shift(rope, rope, scaffold, unknown)` — 6 constraints

- `gradient_descent_optimization`
- `large_cardinal_foundations`
- `lobs_theorem`
- `migration_decision_threshold`
- `minimax_decision_rule`
- `skills_based_hiring`

### `shift(rope, scaffold, scaffold, rope)` — 1 constraints

- `gauss_bonnet_topology`

### `shift(scaffold, scaffold, scaffold, rope)` — 5 constraints

- `four_color_theorem_topological_bound`
- `lln_convergence`
- `nonstandard_arithmetic_models`
- `peano_curve_mapping`
- `silklink_2026`

### `shift(scaffold, scaffold, scaffold, scaffold)` — 9 constraints

- `fundamental_theorem_of_algebra`
- `indexical_relativity_core`
- `liar_paradox`
- `martian_signal_latency`
- `noethers_theorem_symmetry`
- `optimal_stopping_marriage`
- `pythagorean_geometric_constancy`
- `reciprocity_laws_math`
- `sturgeons_law`

### `shift(snare, snare, rope, snare)` — 141 constraints

- `erised_expectation`
- `fragile_middle_layer_collapse`
- `gale_shapley`
- `generational_replacement_inertia`
- `gig_economy_algorithmic_management`
- `global_economic_anxiety_2026`
- `global_water_bankruptcy`
- `goal_boundary_poisoning`
- `governance_latency_gap`
- `governance_overfitting`
- `grievance_stack_overflow`
- `guthrie_kidnapping_2026`
- `hidden_interdependency_risk`
- `hollow_state_syndrome`
- `huang_expectation_resilience_2026`
- `hypercompression_of_time_horizons`
- `hypernormie_equilibrium`
- `ibm_shield_2026`
- `ibm_shield_contract_2026`
- `identity_stack_incompatibility`
- `individual_revolution_autonomy`
- `inference_cost_scaling_law`
- `infrastructure_interoperability_decay`
- `institutional_memory_loss`
- `institutional_mutation_domestication`
- `institutional_mutation_without_selection`
- `integrated_digital_governance_stack`
- `interface_contract_breakdown`
- `interpretive_frame_fragmentation`
- `intertemporal_responsibility_gap`
- `invisible_infrastructure_dependency`
- `irreversible_policy_commitment`
- `israel_override_clause`
- `job_hunt_volume_system_2026`
- `latent_goal_activation`
- `latent_regulatory_bomb`
- `legal_formalism_overhang`
- `legibility_trap`
- `legitimacy_without_capacity`
- `legitimacy_without_effectiveness`
- `lehman_repo_105`
- `liquidity_illusion`
- `litchfield_sensitive_locations_2026`
- `maintenance_capacity_shortfall`
- `maladaptive_selection_process`
- `mandatrophic_margin_collapse`
- `marriage_market_asymmetry_2026`
- `mass_market_extinction_2026`
- `medieval_church_hegemony`
- `memetic_fitness_vs_truth`
- `meta_governance_overload`
- `meta_model_lock_in`
- `metabolic_constraint_cognition`
- `minnesota_sovereignty_2026`
- `misunderstanding_as_mismatch`
- `model_autonomy_creep`
- `model_collapse_feedback_loop`
- `model_of_models_regression`
- `moltbook_breach_2026`
- `moral_outsourcing`
- `multi_agent_reward_hacking`
- `multi_planetary_latency_lock`
- `naming_as_control`
- `narcissistic_ego_maintenance`
- `narrative_capacity_exhaustion`
- `narrative_overfitting`
- `necessary_day_job`
- `non_compete_agreements`
- `norm_erosion_threshold`
- `north_korea_songun_mandatrophy`
- `opioid_political_realignment_2026`
- `optimization_fragility`
- `other_peoples_troubles_2026`
- `overfitting_to_frameworks`
- `panama_canal_ports`
- `parable_fish_turtle`
- `planetary_boundaries`
- `poetic_verse_and_past`
- `policy_lag_catastrophe`
- `power_without_responsibility`
- `prestige_signal_inflation`
- `price_signal_corruption`
- `prime_age_male_unwork`
- `procedural_legitimacy_decay`
- `project_vault_extraction_2026`
- `protocol_drift_accumulation`
- `quellcrist_falconer_justice`
- `radiologic_diagnostic_threshold`
- `rational_inertia_trap`
- `rent_seeking_equilibrium`
- `responsibility_dilution`
- `responsibility_without_power`
- `riot_incentive_loop_2026`
- `risk_socialization_threshold`
- `ritual_without_belief`
- `robustness_vs_efficiency_tradeoff`
- `rotation_seven_isolation`
- `rotation_seven_kubo_ranking`
- `rules_based_international_order`
- `russian_war_cannibalization`
- `scam_compound_2026`
- `scurvy_maritime_extraction`
- `second_order_unintended_consequences`
- `semantic_attack_surface`
- `shadow_pricing_failure`
- `signal_without_control`
- `silent_dependency_activation`
- `silicon_lexicon_overload`
- `slow_crisis_invisibility`
- `sludge_bureaucratic_friction`
- `soft_authoritarian_drift`
- `spain_digital_offensive_2026`
- `statecraft_virtu`
- `status_flattening_effect`
- `structural_extraction_without_actor`
- `suanne_coup_of_peace`
- `suanne_face_restoration`
- `synthetic_data_feedback_loop`
- `systemic_blindspot`
- `tail_risk_compression`
- `taiwan_existential_sovereignty`
- `taiwan_grand_bargain`
- `taiwan_storm_2026`
- `taxonomy_drift`
- `tear_gas_repression_2026`
- `technocratic_overreach`
- `thai_senate_veto_2026`
- `the_calm_protocol_suppression`
- `toxic_social_infection`
- `tractarian_logic_limit`
- `tx_hispanic_pivot`
- `ukr_mobilization`
- `us_suburban_zoning_2025`
- `us_two_party_duopoly`
- `value_alignment_drift`
- `value_extraction_plateau`
- `viral_transmission_rates`
- `world_factbook_sunset_2026`
- `xi_mao_ideological_centralization`
- `zipfs_law`
- `zombie_reasoning_2026`

### `shift(tangled_rope, snare, rope, snare)` — 11 constraints

- `airbnb_str_regulation`
- `geopolitical_insularity_2026`
- `greshams_law`
- `iron_law_of_oligarchy`
- `israel_gaza_ceasefire_violation`
- `medical_residency_match`
- `meta_nda`
- `microwave_weapon_1`
- `new_start_expiration`
- `rare_earth_dependency`
- `us_iran_drone_conflict`

### `shift(tangled_rope, tangled_rope, rope, snare)` — 33 constraints

- `future_dsm_integration`
- `hammurabi_lex_talionis`
- `hu_2026_election_rules`
- `innovators_dilemma`
- `israel_norwegian_law`
- `japanese_energy_scaffold_2025`
- `lung_transplant_protocol`
- `neural_substrate_2026`
- `nine_day_buffer`
- `nuclear_order_2026`
- `nuclear_vacuum_2026`
- `openclaw_regulation`
- `peter_principle`
- `portugal_polarization_threshold_2026`
- `procedural_compliance_theater`
- `protocol_capture_eee`
- `sapir_whorf_hypothesis`
- `semantic_overload_friction`
- `start_treaty`
- `strait_coercion_2025`
- `sunk_cost_fallacy`
- `swift_legacy_piton`
- `trump_indian_tariffs_2026`
- `ulysses_aeolus_1904`
- `ulysses_cyclops_1904`
- `ulysses_lestrygonians_1904`
- `ulysses_nausicaa_1904`
- `ulysses_school_1904`
- `ulysses_scylla_1904`
- `ulysses_sirens_1904`
- `us_employer_health_insurance`
- `usc_26_469_passive_loss`
- `visibility_bias_governance`

### `shift(tangled_rope, tangled_rope, rope, tangled_rope)` — 45 constraints

- `framing_effect`
- `france_cordon_sanitaire_2026`
- `genetic_predisposition`
- `gpt5_codex_dev_cycle`
- `hanlons_razor`
- `hp_liberalism`
- `india_semi_mission`
- `institutional_inertia_lock`
- `israel_electoral_threshold`
- `israel_surplus_vote_agreements`
- `jevons_paradox`
- `landscape_of_fear_2026`
- `lorenz_attractor_dynamics`
- `lsd_microdosing_professional_openness`
- `magna_carta_liberties`
- `mars_rover_navigational_autonomy`
- `mil_std_461g_emi_control`
- `mil_std_810f_tailoring`
- `monetary_regime_transition`
- `overton_window`
- `parkinsons_law`
- `pna`
- `politeness_face_negotiation`
- `postman_survival_protocol`
- `private_identity_integration`
- `rare_earth_seabed_mining`
- `semiconductor_mission_2026`
- `shobies_existential_commitment`
- `social_loafing`
- `social_media_participation_threshold`
- `st_petersburg_paradox`
- `theory_of_visitors`
- `transient_event_detection`
- `trump_critical_minerals`
- `trump_making_china_great_2026`
- `ulysses_calypso_1904`
- `ulysses_eumaeus_1904`
- `ulysses_ithaca_1904`
- `ulysses_lotus_1904`
- `ulysses_rocks_1904`
- `ulysses_tower_1904`
- `union_protection_underperformance`
- `us_labor_mobility`
- `visa_judgment_sharing_agreement`
- `wikipedia_notability_requirement_2026`

### `shift(tangled_rope, tangled_rope, rope, unknown)` — 11 constraints

- `grete_samsa_transition`
- `institutional_trust_decay`
- `insult_wisdom_training`
- `moltbook_agent_theater`
- `neurodiversity_spectrum`
- `rule_update_failure`
- `smartphone_ubiquity`
- `teaching_horses_to_sing`
- `temporal_scale_arbitrage`
- `tragedy_of_the_commons`
- `transformer_self_attention`

### `shift(tangled_rope, tangled_rope, scaffold, snare)` — 3 constraints

- `ia_digital_preservation`
- `perovskite_self_etching`
- `project_vault_2026`

### `shift(tangled_rope, tangled_rope, scaffold, tangled_rope)` — 2 constraints

- `global_stimulus_spree`
- `isa_education_scaffold`

### `shift(tangled_rope, tangled_rope, tangled_rope, tangled_rope)` — 114 constraints

- `decentralized_infrastructure_rope`
- `fnl_shadow_probe`
- `frankenstein_creation_hubris`
- `galois_theory_symmetry`
- `germline_regulation_threshold_2026`
- `gilgamesh_mortality_limit`
- `gita_kurukshetra`
- `global_hoarding_scaling_laws`
- `global_protocol_entrenchment`
- `goedels_incompleteness_theorems`
- `goldbach_conjecture`
- `golden_handcuffs`
- `goodharts_law`
- `greenland_seizure_trade_war`
- `halting_problem_undecidability`
- `happiness_of_others`
- `harm_principle_liberty`
- `hedonic_adaptation_baseline`
- `heisenberg_uncertainty`
- `helsinki_bus_theory`
- `heuristic_optimization`
- `hilberts_hotel_infinity`
- `hominin_evolutionary_bottleneck`
- `hydra_game`
- `information_foraging_theory`
- `informational_time_2026`
- `iran_mandatrophic_collapse`
- `iran_war_room_2026`
- `juvenile_underclass_2026`
- `keltner_relationship_evaluation`
- `khantivadin_radical_patience`
- `kidney_exchange_market`
- `kirby_paris_theorem`
- `kjv_puritan_new_world_exit`
- `kjv_textual_authority`
- `kleene_recursion_theorem`
- `knowledge_action_gap`
- `layered_brain_processing`
- `legacy_system_technical_debt`
- `lindy_effect`
- `litany_of_the_real`
- `local_vs_global_optima`
- `lowenheim_skolem_theorem`
- `lula_hemisphere_2026`
- `mandatrophy_systemic_collapse`
- `manganese_catalysis_2026`
- `matching_markets_general`
- `material_tensile_strength`
- `mco_unit_system_discontinuity`
- `med_diet_consensus_2026`
- `metamorphosis_samsa`
- `micro_robot_electronics_integration`
- `microbiome_symbiosis`
- `mutual_defection_equilibrium`
- `narrative_engineering_2026`
- `nasa_faster_better_cheaper`
- `nash_equilibrium_coordination`
- `network_effects`
- `neuroplasticity_plateau`
- `newtons_method_convergence`
- `no_cloning_theorem`
- `pareto_principle`
- `path_dependence_lock_in`
- `permissive_software_licensing`
- `perseverance_ai_drive`
- `planning_fallacy`
- `platform_cooperativism_governance`
- `poincare_conjecture`
- `portuguese_presidential_term_limits`
- `prime_number_theorem`
- `prisoners_dilemma_equilibrium`
- `proof_of_work_consensus`
- `qualified_immunity`
- `quantum_decryption_risk_2026`
- `quine_self_replication`
- `railway_gauge_standard`
- `rfc9293_state_machine`
- `rices_theorem_undecidability`
- `rogers_commission_institutional_analysis`
- `rogue_wave_control_2026`
- `rotation_seven_black_soil`
- `royal_navy_middle_east_withdrawal`
- `self_surpassing`
- `skolems_paradox`
- `sleep_debt_externality`
- `social_narrative_casting`
- `solar_system_weirdness`
- `square_cube_law`
- `starwars_evolutionary_mutation`
- `steinmetz_valuation_asymmetry`
- `stoic_logos_governance`
- `sts86_ascent_checklist`
- `suslin_hypothesis_undecidability`
- `sylow_theorems_group_theory`
- `taiwan_strait_hegemony_shift`
- `tarski_undefinability`
- `technological_point_of_no_return`
- `thai_article_112_mountain`
- `the_bacchae_madness_protocol`
- `the_churn_systemic_upheaval`
- `the_wall_procedural_barrier`
- `trade_secret_law`
- `trojan_war_spoils`
- `trump_second_term_authoritarianism_2026`
- `ulysses_circe_1904`
- `ulysses_hades_1904`
- `ulysses_oxen_1904`
- `ulysses_penelope_1904`
- `ulysses_proteus_1904`
- `utopia_apocalypse_fragility`
- `van_der_waerden_theorem`
- `vienna_quantum_superposition_2026`
- `whitehead_problem_undecidability`
- `winners_curse`

### `shift(tangled_rope, unknown, rope, unknown)` — 1 constraints

- `negative_emissions_arbitrage`

### `shift(unknown, rope, rope, rope)` — 7 constraints

- `guinea_worm_eradication`
- `hiv_prep_prevention_2026`
- `omelet_perfection_complexity`
- `quantum_entanglement_protocol`
- `somatic_focusing_awareness`
- `tcp_rfc9293_interoperability`
- `visa_ipo_regulatory_compliance`

### `shift(unknown, rope, rope, tangled_rope)` — 3 constraints

- `gs1_gln_identification`
- `moores_law`
- `openbsd_netiquette_protocol`

### `shift(unknown, rope, rope, unknown)` — 1 constraints

- `unclos_2026`

### `shift(unknown, snare, scaffold, snare)` — 1 constraints

- `genie_ip_constraint`

### `shift(unknown, unknown, rope, tangled_rope)` — 5 constraints

- `germany_tennet_takeover`
- `logistic_map_dynamics`
- `monty_hall_conditional_probability`
- `notary_ink_dependency`
- `ritual_transition_scaffold`

### `shift(unknown, unknown, rope, unknown)` — 11 constraints

- `france_2027_presidential_election`
- `gs1_standardized_identification`
- `hegemonic_entropy_2026`
- `hoa_covenants`
- `net_zero_stabilization`
- `neural_interoperability`
- `paris_municipal_reform_2026`
- `regulatory_capture`
- `repair_probe_incomplete`
- `shitty_feedback_handling`
- `south_china_sea_arbitration_2016_2026`

### `shift(unknown, unknown, scaffold, snare)` — 4 constraints

- `mit_tfus_2026`
- `openai_api_access`
- `openai_codex_app_constraint`
- `openscholar_peer_review`

### `shift(unknown, unknown, scaffold, tangled_rope)` — 2 constraints

- `france_local_elections_march_2026`
- `portugal_ad_stability_2026`

### `shift(unknown, unknown, scaffold, unknown)` — 19 constraints

- `gamblers_ruin_stochastic_extinction`
- `glp1_payload_efficiency_pivot`
- `graph_coloring_complexity`
- `hawthorne_effect`
- `law_of_diminishing_returns`
- `maha_recovery_2026`
- `max_flow_min_cut`
- `p_vs_np`
- `platonic_coparenting_decoupling`
- `qwerty_vs_dvorak`
- `recipe_scaling_ai`
- `russells_paradox_self_reference`
- `sat_csp_complexity`
- `sorites_paradox`
- `stable_marriage_coordination`
- `strange_attractor_dynamics`
- `three_body_unpredictability`
- `traveling_salesperson_problem`
- `trillion_bond_rush_2026`

### `shift(unknown, unknown, unknown, unknown)` — 16 constraints

- `26usc469_real_estate_exemption`
- `abstraction_boundary_overrun`
- `abstraction_leakage`
- `academic_fashion_modernism_2026`
- `academic_peer_review_gatekeeping`
- `academic_tenure_system`
- `ad_fus_coordination`
- `ad_synaptic_deficit`
- `adaptive_lag_trap`
- `adversarial_truth_decay`
- `adverse_possession`
- `catholic_church_1200`
- `cg_israelgaza_20231012`
- `moltbot_religion`
- `thermodynamics_entropy`
- `universal_mathematics_communication`

## Scope Differentiation

Constraints where changing scope (local vs national vs global) changes the classification type, holding power/time/exit constant.

- **Scope-sensitive constraints**: 136 / 477

- `airbnb_str_regulation`: local=tangled_rope, national=snare, global=snare
- `erised_expectation`: local=tangled_rope, national=snare, global=snare
- `four_color_theorem_topological_bound`: local=scaffold, national=scaffold, global=rope
- `france_cordon_sanitaire_2026`: local=unknown, national=tangled_rope, global=tangled_rope
- `france_local_elections_march_2026`: local=rope, national=unknown, global=tangled_rope
- `future_dsm_integration`: local=tangled_rope, national=tangled_rope, global=snare
- `genie_ip_constraint`: local=unknown, national=snare, global=snare
- `geopolitical_insularity_2026`: local=tangled_rope, national=snare, global=snare
- `germany_tennet_takeover`: local=rope, national=unknown, global=tangled_rope
- `global_economic_anxiety_2026`: local=unknown, national=snare, global=snare
- `glp1_payload_efficiency_pivot`: local=rope, national=unknown, global=unknown
- `governance_overfitting`: local=tangled_rope, national=snare, global=snare
- `gradient_descent_optimization`: local=scaffold, national=rope, global=unknown
- `greshams_law`: local=tangled_rope, national=snare, global=snare
- `grete_samsa_transition`: local=tangled_rope, national=tangled_rope, global=unknown
- `gs1_gln_identification`: local=rope, national=rope, global=unknown
- `gs1_standardized_identification`: local=rope, national=unknown, global=unknown
- `hammurabi_lex_talionis`: local=tangled_rope, national=tangled_rope, global=snare
- `hu_2026_election_rules`: local=tangled_rope, national=tangled_rope, global=snare
- `ia_digital_preservation`: local=tangled_rope, national=tangled_rope, global=snare
- `ibm_shield_2026`: local=tangled_rope, national=snare, global=snare
- `ibm_shield_contract_2026`: local=tangled_rope, national=snare, global=snare
- `inner_model_theory_constraints`: local=rope, national=rope, global=unknown
- `innovators_dilemma`: local=tangled_rope, national=tangled_rope, global=snare
- `institutional_mutation_domestication`: local=tangled_rope, national=snare, global=snare
- `institutional_trust_decay`: local=tangled_rope, national=tangled_rope, global=unknown
- `insult_wisdom_training`: local=tangled_rope, national=tangled_rope, global=unknown
- `iron_law_of_oligarchy`: local=tangled_rope, national=snare, global=snare
- `irreversible_policy_commitment`: local=tangled_rope, national=snare, global=snare
- `isa_education_scaffold`: local=unknown, national=tangled_rope, global=tangled_rope
- `israel_gaza_ceasefire_violation`: local=tangled_rope, national=snare, global=snare
- `israel_norwegian_law`: local=tangled_rope, national=tangled_rope, global=snare
- `japanese_energy_scaffold_2025`: local=tangled_rope, national=tangled_rope, global=snare
- `landscape_of_fear_2026`: local=unknown, national=tangled_rope, global=tangled_rope
- `large_cardinal_foundations`: local=scaffold, national=rope, global=unknown
- `lln_convergence`: local=scaffold, national=scaffold, global=rope
- `lobs_theorem`: local=scaffold, national=rope, global=unknown
- `logistic_map_dynamics`: local=rope, national=unknown, global=tangled_rope
- `lorenz_attractor_dynamics`: local=unknown, national=tangled_rope, global=tangled_rope
- `lsd_microdosing_professional_openness`: local=unknown, national=tangled_rope, global=tangled_rope
- `lung_transplant_protocol`: local=tangled_rope, national=tangled_rope, global=snare
- `magna_carta_liberties`: local=unknown, national=tangled_rope, global=tangled_rope
- `marriage_market_asymmetry_2026`: local=tangled_rope, national=snare, global=snare
- `mars_rover_navigational_autonomy`: local=unknown, national=tangled_rope, global=tangled_rope
- `mass_market_extinction_2026`: local=tangled_rope, national=snare, global=snare
- `medical_residency_match`: local=tangled_rope, national=snare, global=snare
- `medieval_church_hegemony`: local=tangled_rope, national=snare, global=snare
- `meta_nda`: local=tangled_rope, national=snare, global=snare
- `microwave_weapon_1`: local=tangled_rope, national=snare, global=snare
- `migration_decision_threshold`: local=scaffold, national=rope, global=unknown
- `minimax_decision_rule`: local=scaffold, national=rope, global=unknown
- `minnesota_sovereignty_2026`: local=tangled_rope, national=snare, global=snare
- `mit_tfus_2026`: local=unknown, national=unknown, global=snare
- `moltbook_agent_theater`: local=tangled_rope, national=tangled_rope, global=unknown
- `moltbook_breach_2026`: local=unknown, national=snare, global=snare
- `monty_hall_conditional_probability`: local=rope, national=unknown, global=tangled_rope
- `moores_law`: local=rope, national=rope, global=unknown
- `necessary_day_job`: local=tangled_rope, national=snare, global=snare
- `negative_emissions_arbitrage`: local=tangled_rope, national=unknown, global=unknown
- `net_zero_stabilization`: local=tangled_rope, national=unknown, global=unknown
- `neural_interoperability`: local=tangled_rope, national=unknown, global=unknown
- `neural_substrate_2026`: local=tangled_rope, national=tangled_rope, global=snare
- `neurodiversity_spectrum`: local=tangled_rope, national=tangled_rope, global=unknown
- `new_start_expiration`: local=tangled_rope, national=snare, global=snare
- `nine_day_buffer`: local=tangled_rope, national=tangled_rope, global=snare
- `nonstandard_arithmetic_models`: local=scaffold, national=scaffold, global=rope
- `notary_ink_dependency`: local=rope, national=unknown, global=tangled_rope
- `nuclear_order_2026`: local=tangled_rope, national=tangled_rope, global=snare
- `nuclear_vacuum_2026`: local=tangled_rope, national=tangled_rope, global=snare
- `openai_api_access`: local=unknown, national=unknown, global=snare
- `openai_codex_app_constraint`: local=unknown, national=unknown, global=snare
- `openbsd_netiquette_protocol`: local=rope, national=rope, global=unknown
- `openclaw_regulation`: local=tangled_rope, national=tangled_rope, global=snare
- `openscholar_peer_review`: local=unknown, national=unknown, global=snare
- `opioid_political_realignment_2026`: local=tangled_rope, national=snare, global=snare
- `overton_window`: local=unknown, national=tangled_rope, global=tangled_rope
- `panama_canal_ports`: local=tangled_rope, national=snare, global=snare
- `parable_fish_turtle`: local=tangled_rope, national=snare, global=snare
- `peano_curve_mapping`: local=scaffold, national=scaffold, global=rope
- `perovskite_self_etching`: local=tangled_rope, national=tangled_rope, global=snare
- `peter_principle`: local=tangled_rope, national=tangled_rope, global=snare
- `portugal_ad_stability_2026`: local=rope, national=unknown, global=tangled_rope
- `portugal_polarization_threshold_2026`: local=tangled_rope, national=tangled_rope, global=snare
- `postman_survival_protocol`: local=unknown, national=tangled_rope, global=tangled_rope
- `prime_age_male_unwork`: local=tangled_rope, national=snare, global=snare
- `private_identity_integration`: local=unknown, national=tangled_rope, global=tangled_rope
- `procedural_compliance_theater`: local=tangled_rope, national=tangled_rope, global=snare
- `project_vault_2026`: local=tangled_rope, national=tangled_rope, global=snare
- `protocol_capture_eee`: local=tangled_rope, national=tangled_rope, global=snare
- `radiologic_diagnostic_threshold`: local=tangled_rope, national=snare, global=snare
- `rare_earth_dependency`: local=tangled_rope, national=snare, global=snare
- `ritual_transition_scaffold`: local=rope, national=unknown, global=tangled_rope
- `rule_update_failure`: local=tangled_rope, national=tangled_rope, global=unknown
- `sadhu_integrity_protocol`: local=rope, national=rope, global=unknown
- `sapir_whorf_hypothesis`: local=tangled_rope, national=tangled_rope, global=snare
- `semantic_overload_friction`: local=tangled_rope, national=tangled_rope, global=snare
- `silklink_2026`: local=scaffold, national=scaffold, global=rope
- `skills_based_hiring`: local=scaffold, national=rope, global=unknown
- `sludge_bureaucratic_friction`: local=tangled_rope, national=snare, global=snare
- `smartphone_ubiquity`: local=tangled_rope, national=tangled_rope, global=unknown
- `spain_digital_offensive_2026`: local=tangled_rope, national=snare, global=snare
- `start_treaty`: local=tangled_rope, national=tangled_rope, global=snare
- `statecraft_virtu`: local=tangled_rope, national=snare, global=snare
- `strait_coercion_2025`: local=tangled_rope, national=tangled_rope, global=snare
- `strange_attractor_dynamics`: local=rope, national=unknown, global=unknown
- `sunk_cost_fallacy`: local=tangled_rope, national=tangled_rope, global=snare
- `taiwan_existential_sovereignty`: local=tangled_rope, national=snare, global=snare
- `taiwan_grand_bargain`: local=tangled_rope, national=snare, global=snare
- `teaching_horses_to_sing`: local=tangled_rope, national=tangled_rope, global=unknown
- `technocratic_overreach`: local=tangled_rope, national=snare, global=snare
- `temporal_scale_arbitrage`: local=tangled_rope, national=tangled_rope, global=unknown
- `thai_senate_veto_2026`: local=tangled_rope, national=snare, global=snare
- `tractarian_logic_limit`: local=tangled_rope, national=snare, global=snare
- `tragedy_of_the_commons`: local=tangled_rope, national=tangled_rope, global=unknown
- `transformer_self_attention`: local=tangled_rope, national=tangled_rope, global=unknown
- `transient_event_detection`: local=unknown, national=tangled_rope, global=tangled_rope
- `trump_indian_tariffs_2026`: local=tangled_rope, national=tangled_rope, global=snare
- `tx_hispanic_pivot`: local=tangled_rope, national=snare, global=snare
- `ukr_mobilization`: local=tangled_rope, national=snare, global=snare
- `ulysses_aeolus_1904`: local=tangled_rope, national=tangled_rope, global=snare
- `ulysses_cyclops_1904`: local=tangled_rope, national=tangled_rope, global=snare
- `ulysses_lestrygonians_1904`: local=tangled_rope, national=tangled_rope, global=snare
- `ulysses_scylla_1904`: local=tangled_rope, national=tangled_rope, global=snare
- `ulysses_sirens_1904`: local=tangled_rope, national=tangled_rope, global=snare
- `unclos_2026`: local=rope, national=rope, global=unknown
- `us_employer_health_insurance`: local=tangled_rope, national=tangled_rope, global=snare
- `us_iran_drone_conflict`: local=tangled_rope, national=snare, global=snare
- `us_suburban_zoning_2025`: local=tangled_rope, national=snare, global=snare
- `usc_26_469_passive_loss`: local=tangled_rope, national=tangled_rope, global=snare
- `value_alignment_drift`: local=tangled_rope, national=snare, global=snare
- `viral_transmission_rates`: local=tangled_rope, national=snare, global=snare
- `visa_judgment_sharing_agreement`: local=unknown, national=tangled_rope, global=tangled_rope
- `visibility_bias_governance`: local=tangled_rope, national=tangled_rope, global=snare
- `wikipedia_notability_requirement_2026`: local=unknown, national=tangled_rope, global=tangled_rope
- `world_factbook_sunset_2026`: local=tangled_rope, national=snare, global=snare
- `zombie_reasoning_2026`: local=unknown, national=snare, global=snare
## Metric Zone Distribution

### Extraction Zones

| extreme | 306 |
| low | 62 |
| negligible | 95 |

### Suppression Zones

| extreme | 282 |
| high | 93 |
| low | 75 |
| moderate | 6 |
| negligible | 21 |

---
*End of fingerprint report*
