
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================
=== MAXENT DIAGNOSTIC OUTPUT ===
SUMMARY: NTotal=1032 MeanEntropy=0.197728 NHighUncertainty=15 NHard=182 NSoft=0

=== TASK 1: MISSING CONSTRAINTS ===
VISIBLE_CLAIMS: 1032
ALL_CLAIMS_RAW: 1034
LIST_FORM_CLAIMS: 1
  LIST_CLAIM: [wikipedia_crowdsourcing_2026]
NON_ATOM_CLAIMS: 0
TESTSET_FILES: 1035
EXPECTED_IDS: 1035
MISSING_FROM_CLAIMS: 135
MISSING_IDS_SAMPLE:
  26usc469_real_estate_exemption
  8k_tv_limit_2026
  CG_IsraelGaza_20231012
  MOLTBOT_RELIGION
  absorbing_markov_chains
  agentive_optimism_2026
  aging_well_assessment
  ai_superpowers_race_2026
  axiom_of_choice_determinacy
  bayes_theorem
  birthday_paradox_collison
  blackstone_tax_receiveable_agreement
  borsuk_ulam_theorem
  burali_forte_paradox
  burden_of_proof_scientific_empirical
  central_limit_theorem_convergence
  cognitive_mimicry_arbitrage
  columbia_2026_elections
  conversational_dogmas_interuption
  cuban_missile_crisis_excomm_delibration
  deferential_realism_core
  dionysaic_frenzy
  educational_unbundling_implementation
  emergency_deployment_scaffold
  emotional_cycles_of_change
  emrgency_medicine_clinical_guidelines
  epstein_espionage_crisis_2026
  epstein_kgb_honeytrap
  ergo_dexy_gold_protocol
  ergo_rosen_bridge_protocol
  ergo_sig_usd_protocol
  exoplanet_habitability_arbitrage
  family_succession_and_decadence
  fermats_last_theorem
  financialization_drag
  finite_simple_groups_classification
  fittss_law
  fmt_oncology_realignment_2026
  french_local_elections_march_2026
  fundamental_theorem_of_calculus
  future_dsm_integration_2026
  gale_shapley_variants
  galois_theory
  gig_economy_algorithmic_managment
  godels_incompleteness_theorems
  goodsteins_theorem
  hammurabi
  harry_potter_liberalism
  heine_borel_theorem
  hilberts_hotel
CONSTRAINTS_WITH_DIST: 1032
VISIBLE_BUT_NO_DIST: 0
DET_TYPE_DISTRIBUTION:
  mountain: 129
  rope: 51
  snare: 519
  tangled_rope: 333
RESIDUAL_TYPE_COUNT: 0
=== END TASK 1 ===

=== TASK 2: PER-TYPE ENTROPY BREAKDOWN ===
TYPE_ENTROPY_TABLE:
Type|Count|Mean|Median|Min|Max|StdDev
mountain|129|0.153738|0.155706|0.031285|0.514653|0.044203
rope|51|0.199920|0.155706|0.007231|0.468247|0.117343
snare|519|0.251335|0.290304|0.009372|0.449794|0.124054
tangled_rope|333|0.130885|0.155706|0.000000|0.494871|0.114796
=== END TASK 2 ===

=== TASK 3: HARD DISAGREEMENTS ===
TOTAL_HARD: 182
DISAGREEMENT_PAIRS:
DetType->ShadowType|Count
mountain->rope|1
rope->tangled_rope|15
snare->tangled_rope|141
tangled_rope->piton|1
tangled_rope->rope|7
tangled_rope->snare|17
HARD_DISAGREEMENT_DETAILS:
Constraint|DetType|ShadowType|ShadowTopP|ShadowConf|Distribution
ai_adoption_stigma|snare|tangled_rope|0.612573|0.627300|tangled_rope:0.613 snare:0.387
ai_performance_watermark|snare|tangled_rope|0.760154|0.692446|tangled_rope:0.760 snare:0.240
arctic_maritime_control|snare|tangled_rope|0.618349|0.628867|tangled_rope:0.618 snare:0.382
arg_ev_tariff|snare|tangled_rope|0.517907|0.613459|tangled_rope:0.518 snare:0.482
astm_d638_tensile_testing|rope|tangled_rope|0.571313|0.596895|rope:0.421 tangled_rope:0.571
atrophied_optimization_piton|tangled_rope|snare|0.902250|0.799359|tangled_rope:0.084 snare:0.902 piton:0.014
board_of_peace_2026|tangled_rope|snare|0.777040|0.679886|tangled_rope:0.212 snare:0.777 piton:0.011
boltzmann_universality_2026|rope|tangled_rope|0.708839|0.645726|rope:0.285 tangled_rope:0.709
brain_network_paradigm_2026|snare|tangled_rope|0.758937|0.691507|tangled_rope:0.759 snare:0.241
broke_vs_poor_grocery_math|snare|tangled_rope|0.640825|0.635586|tangled_rope:0.641 snare:0.359
cancer_prevention|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
carbon_credit_markets_2026|snare|tangled_rope|0.895914|0.813473|tangled_rope:0.896 snare:0.104
carrier_deployment_deterrence|snare|tangled_rope|0.517907|0.613459|tangled_rope:0.518 snare:0.482
carrying_capacity|snare|tangled_rope|0.732755|0.676014|tangled_rope:0.733 snare:0.267
china_ev_export_oversupply|snare|tangled_rope|0.724298|0.671315|tangled_rope:0.724 snare:0.276
china_vactrain_standard|snare|tangled_rope|0.760154|0.692446|tangled_rope:0.760 snare:0.240
clawderberg_recursive_slop|tangled_rope|piton|0.647378|0.505129|tangled_rope:0.142 snare:0.211 piton:0.647
climate_event_attribution|snare|tangled_rope|0.534897|0.614452|tangled_rope:0.535 snare:0.465
cn_tech_decoupling_security_software|snare|tangled_rope|0.537282|0.614660|tangled_rope:0.537 snare:0.463
codex_access|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
coffee_cardiovascular_2026|tangled_rope|rope|0.952971|0.876735|rope:0.953 tangled_rope:0.018 scaffold:0.029
cognac_geopolitical_risk|snare|tangled_rope|0.932705|0.862080|tangled_rope:0.933 snare:0.067
cognitive_bicycle_scaffold|tangled_rope|rope|0.897452|0.776380|rope:0.897 tangled_rope:0.044 scaffold:0.059
cognitive_induction_gap|snare|tangled_rope|0.732755|0.676014|tangled_rope:0.733 snare:0.267
colorado_sbe_decentralization_friction|snare|tangled_rope|0.722643|0.670466|tangled_rope:0.723 snare:0.277
constitutional_consecration|snare|tangled_rope|0.511750|0.613291|tangled_rope:0.512 snare:0.488
constitutional_supremacy|snare|tangled_rope|0.795383|0.717129|tangled_rope:0.795 snare:0.205
consumer_debt_slavery|snare|tangled_rope|0.545475|0.615458|tangled_rope:0.545 snare:0.455
conversational_dogmas_interruption|snare|tangled_rope|0.889390|0.805781|tangled_rope:0.889 snare:0.111
couples_residency_match|snare|tangled_rope|0.844696|0.758905|tangled_rope:0.845 snare:0.155
credentialism_national_security|snare|tangled_rope|0.586266|0.621432|tangled_rope:0.586 snare:0.414
cuban_missile_crisis_excomm_deliberation|rope|tangled_rope|0.513086|0.562923|rope:0.465 tangled_rope:0.513 scaffold:0.022
cultural_homogenization_social_media|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
data_privacy_regulation|snare|tangled_rope|0.618349|0.628867|tangled_rope:0.618 snare:0.382
debt_trap_microfinance|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
deferential_realism_framework|rope|tangled_rope|0.752535|0.654773|rope:0.232 tangled_rope:0.753 scaffold:0.016
dexy_gold_protocol|rope|tangled_rope|0.579834|0.571519|rope:0.398 tangled_rope:0.580 scaffold:0.022
digital_identity_tether|snare|tangled_rope|0.505287|0.613175|tangled_rope:0.505 snare:0.495
dldr_information_policy|rope|tangled_rope|0.497369|0.560266|rope:0.479 tangled_rope:0.497 scaffold:0.023
dn_paywall|snare|tangled_rope|0.624613|0.630542|tangled_rope:0.625 snare:0.375
dunning_kruger_effect|snare|tangled_rope|0.555957|0.616593|tangled_rope:0.556 snare:0.444
ec_meta_manus_block|snare|tangled_rope|0.537282|0.614660|tangled_rope:0.537 snare:0.463
edelman_2026_insularity|snare|tangled_rope|0.657076|0.641150|tangled_rope:0.657 snare:0.343
electrification_scale_2026|rope|tangled_rope|0.694902|0.637672|rope:0.298 tangled_rope:0.695
elliq_ai_companion|snare|tangled_rope|0.670344|0.646181|tangled_rope:0.670 snare:0.330
emergency_bridge_scaffold|snare|tangled_rope|0.863579|0.777524|tangled_rope:0.864 snare:0.136
ergo_lets_protocol|tangled_rope|rope|0.983063|0.942974|rope:0.983
eu_digital_services_act|snare|tangled_rope|0.534897|0.614452|tangled_rope:0.535 snare:0.465
eu_ev_tariff_wall|snare|tangled_rope|0.547568|0.615617|tangled_rope:0.548 snare:0.452
eu_unanimity_rule_foreign_policy|snare|tangled_rope|0.534897|0.614452|tangled_rope:0.535 snare:0.465
eurozone_fragmentation_2026|tangled_rope|snare|0.577423|0.618489|tangled_rope:0.422 snare:0.577
exploration_vs_exploitation|snare|tangled_rope|0.781204|0.706667|tangled_rope:0.781 snare:0.219
fcc_dji_covered_list|snare|tangled_rope|0.596281|0.623465|tangled_rope:0.596 snare:0.404
fda_component_efficacy_standard|snare|tangled_rope|0.547568|0.615617|tangled_rope:0.548 snare:0.452
fiber_optic_chip_tech|snare|tangled_rope|0.781204|0.706667|tangled_rope:0.781 snare:0.219
fine_particle_policy|snare|tangled_rope|0.618349|0.628867|tangled_rope:0.618 snare:0.382
fitts_law_industrial_application|snare|tangled_rope|0.540867|0.614991|tangled_rope:0.541 snare:0.459
fmt_oncology_2026|tangled_rope|rope|0.902613|0.784129|rope:0.903 tangled_rope:0.051 scaffold:0.046
fraser_river_salmon_regulation|snare|tangled_rope|0.509680|0.613243|tangled_rope:0.510 snare:0.490
future_dsm_integration|snare|tangled_rope|0.509680|0.613243|tangled_rope:0.510 snare:0.490
g7_debt_trap|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
gamblers_ruin_stochastic_extinction|tangled_rope|snare|0.706928|0.662410|tangled_rope:0.293 snare:0.707
gaza_border_control_rafah|snare|tangled_rope|0.598604|0.624042|tangled_rope:0.599 snare:0.401
glen_canyon_water_allocation|snare|tangled_rope|0.670344|0.646181|tangled_rope:0.670 snare:0.330
global_digital_divide|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
goodharts_law|snare|tangled_rope|0.758937|0.691507|tangled_rope:0.759 snare:0.241
google_ai_search_overview|snare|tangled_rope|0.537282|0.614660|tangled_rope:0.537 snare:0.463
google_universal_commerce_protocol|snare|tangled_rope|0.612573|0.627300|tangled_rope:0.613 snare:0.387
great_awakening_rekindling|snare|tangled_rope|0.936842|0.868176|tangled_rope:0.937 snare:0.063
great_mongolian_road_economic_dependency|snare|tangled_rope|0.785442|0.709777|tangled_rope:0.785 snare:0.215
greenland_defence_pact_2026|snare|tangled_rope|0.823449|0.739650|tangled_rope:0.823 snare:0.177
greshams_law|snare|tangled_rope|0.532432|0.614298|tangled_rope:0.532 snare:0.468
gs1_standardized_identification|tangled_rope|snare|0.997468|0.989541|snare:0.997
guano_wealth_extraction|snare|tangled_rope|0.618349|0.628867|tangled_rope:0.618 snare:0.382
guinea_worm_eradication|rope|tangled_rope|0.897753|0.792660|rope:0.088 tangled_rope:0.898 scaffold:0.014
hammurabi_lex_talionis|snare|tangled_rope|0.642285|0.635908|tangled_rope:0.642 snare:0.358
hawthorne_effect|snare|tangled_rope|0.667461|0.645034|tangled_rope:0.667 snare:0.333
hegemonic_entropy_2026|tangled_rope|snare|0.941403|0.856281|tangled_rope:0.016 snare:0.941 piton:0.043
horizon_liability_contract|tangled_rope|snare|0.865485|0.779312|tangled_rope:0.134 snare:0.865
hu_2026_election_rules|snare|tangled_rope|0.688158|0.653652|tangled_rope:0.688 snare:0.312
hub_short_form_tv_market_fragmentation|snare|tangled_rope|0.724298|0.671315|tangled_rope:0.724 snare:0.276
india_nuclear_liability_act_2010|snare|tangled_rope|0.517907|0.613459|tangled_rope:0.518 snare:0.482
indian_import_tariffs_eu|snare|tangled_rope|0.547568|0.615617|tangled_rope:0.548 snare:0.452
inner_model_confirmation_bias|snare|tangled_rope|0.618349|0.628867|tangled_rope:0.618 snare:0.382
innovators_dilemma|snare|tangled_rope|0.646053|0.637226|tangled_rope:0.646 snare:0.354
institutional_trust_decay|tangled_rope|snare|0.568609|0.521745|tangled_rope:0.372 snare:0.569 piton:0.060
international_seabed_mining_regime|snare|tangled_rope|0.509680|0.613243|tangled_rope:0.510 snare:0.490
iron_law_of_oligarchy|snare|tangled_rope|0.843034|0.757397|tangled_rope:0.843 snare:0.157
israel_gaza_ceasefire_violation|snare|tangled_rope|0.502679|0.613137|tangled_rope:0.503 snare:0.497
israel_norwegian_law|snare|tangled_rope|0.671638|0.646615|tangled_rope:0.672 snare:0.328
japanese_energy_scaffold_2025|snare|tangled_rope|0.895914|0.813473|tangled_rope:0.896 snare:0.104
job_hunt_volume_system_2026|snare|tangled_rope|0.680720|0.650440|tangled_rope:0.681 snare:0.319
jp_eez_enforcement|snare|tangled_rope|0.510017|0.613212|tangled_rope:0.510 snare:0.490
lindy_effect|rope|tangled_rope|0.838021|0.732248|rope:0.152 tangled_rope:0.838
lung_transplant_protocol|snare|tangled_rope|0.618349|0.628867|tangled_rope:0.618 snare:0.382
manganese_catalysis_2026|tangled_rope|rope|0.936930|0.846273|rope:0.937 tangled_rope:0.021 scaffold:0.042
max_flow_min_cut|snare|tangled_rope|0.941681|0.875579|tangled_rope:0.942 snare:0.058
medical_residency_match|snare|tangled_rope|0.662972|0.643332|tangled_rope:0.663 snare:0.337
metabolic_constraint_cognition|snare|tangled_rope|0.574887|0.619431|tangled_rope:0.575 snare:0.425
mexican_airline_merger|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
microwave_weapon_1|snare|tangled_rope|0.502679|0.613137|tangled_rope:0.503 snare:0.497
moltbot_religion|snare|tangled_rope|0.502679|0.613137|tangled_rope:0.503 snare:0.497
mrna_melanoma_therapy|snare|tangled_rope|0.547568|0.615617|tangled_rope:0.548 snare:0.452
narrative_engineering_2026|tangled_rope|rope|0.940959|0.851952|rope:0.941 tangled_rope:0.029 scaffold:0.030
ncaa_eligibility_rules|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
neural_substrate_2026|snare|tangled_rope|0.602694|0.624926|tangled_rope:0.603 snare:0.397
new_start_expiration|snare|tangled_rope|0.502679|0.613137|tangled_rope:0.503 snare:0.497
nfl_superbowl_marketing_regulation|snare|tangled_rope|0.618349|0.628867|tangled_rope:0.618 snare:0.382
notary_ink_dependency|tangled_rope|snare|0.494085|0.548456|tangled_rope:0.476 snare:0.494 piton:0.030
omelet_perfection_complexity|rope|tangled_rope|0.941074|0.861732|rope:0.051 tangled_rope:0.941
openai_health_review|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
openai_implicit_translator|snare|tangled_rope|0.830743|0.746011|tangled_rope:0.831 snare:0.169
openai_prism_development|snare|tangled_rope|0.517907|0.613459|tangled_rope:0.518 snare:0.482
openbsd_netiquette_protocol|snare|tangled_rope|0.853952|0.767896|tangled_rope:0.854 snare:0.146
openclaw_data_lock_in|snare|tangled_rope|0.550411|0.615944|tangled_rope:0.550 snare:0.450
openclaw_regulation|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
oral_glp1_market_access|snare|tangled_rope|0.781204|0.706667|tangled_rope:0.781 snare:0.219
p_g_golden_pear_surveillance|snare|tangled_rope|0.618349|0.628867|tangled_rope:0.618 snare:0.382
perovskite_self_etching|snare|tangled_rope|0.823449|0.739650|tangled_rope:0.823 snare:0.177
peter_principle|snare|tangled_rope|0.889390|0.805781|tangled_rope:0.889 snare:0.111
poetic_verse_and_past|snare|tangled_rope|0.669526|0.645870|tangled_rope:0.670 snare:0.330
portugal_polarization_threshold_2026|snare|tangled_rope|0.837652|0.752436|tangled_rope:0.838 snare:0.162
private_credit_market_opacity|snare|tangled_rope|0.517907|0.613459|tangled_rope:0.518 snare:0.482
quantum_entanglement_protocol|rope|tangled_rope|0.829551|0.736474|rope:0.168 tangled_rope:0.830
quine_self_replication|mountain|rope|0.598271|0.485347|mountain:0.300 rope:0.598 tangled_rope:0.096
radiologic_diagnostic_threshold|snare|tangled_rope|0.722643|0.670466|tangled_rope:0.723 snare:0.277
rare_earth_dependency|snare|tangled_rope|0.502679|0.613137|tangled_rope:0.503 snare:0.497
rare_earth_export_restrictions|snare|tangled_rope|0.618349|0.628867|tangled_rope:0.618 snare:0.382
rfc9293_state_machine|rope|tangled_rope|0.857682|0.755719|rope:0.136 tangled_rope:0.858
rogue_wave_control_2026|tangled_rope|rope|0.782423|0.675801|rope:0.782 tangled_rope:0.202 scaffold:0.016
rule_update_failure|tangled_rope|snare|0.651379|0.613677|tangled_rope:0.339 snare:0.651 piton:0.010
rules_based_international_order|snare|tangled_rope|0.669526|0.645870|tangled_rope:0.670 snare:0.330
sapir_whorf_hypothesis|snare|tangled_rope|0.646053|0.637226|tangled_rope:0.646 snare:0.354
scientific_paradigm_lifecycle|tangled_rope|snare|0.732349|0.662707|tangled_rope:0.263 snare:0.732
seedance_export_restriction|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
semiconductor_fabrication_chokepoint|snare|tangled_rope|0.537282|0.614660|tangled_rope:0.537 snare:0.463
shadow_fleet_sanctions_evasion|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
sk_ai_act_2026|snare|tangled_rope|0.517907|0.613459|tangled_rope:0.518 snare:0.482
skills_based_hiring|rope|tangled_rope|0.837376|0.718374|rope:0.143 tangled_rope:0.837 scaffold:0.020
slow_crisis_invisibility|snare|tangled_rope|0.654275|0.640152|tangled_rope:0.654 snare:0.346
sludge_bureaucratic_friction|snare|tangled_rope|0.732755|0.676014|tangled_rope:0.733 snare:0.267
sm_addictive_design|tangled_rope|snare|0.815735|0.733335|tangled_rope:0.184 snare:0.816
somatic_focusing_awareness|rope|tangled_rope|0.715019|0.629806|rope:0.268 tangled_rope:0.715 scaffold:0.017
south_china_sea_arbitration_2016_2026|snare|tangled_rope|0.787739|0.711388|tangled_rope:0.788 snare:0.212
start_treaty|snare|tangled_rope|0.618349|0.628867|tangled_rope:0.618 snare:0.382
sti_clinical_testing_bottleneck|snare|tangled_rope|0.664858|0.643880|tangled_rope:0.665 snare:0.335
strait_coercion_2025|snare|tangled_rope|0.523098|0.613735|tangled_rope:0.523 snare:0.477
strange_attractor_dynamics|snare|tangled_rope|0.942498|0.876894|tangled_rope:0.942 snare:0.057
strange_attractor_systemic_risk|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
strategic_deep_sea_rare_earth_mining|snare|tangled_rope|0.618349|0.628867|tangled_rope:0.618 snare:0.382
streaming_bundling_mandate|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
student_loan_default_cliff|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
student_loan_interest_accrual|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
sunk_cost_fallacy|snare|tangled_rope|0.939787|0.872634|tangled_rope:0.940 snare:0.060
taiwan_ids_program|snare|tangled_rope|0.534897|0.614452|tangled_rope:0.535 snare:0.465
taiwan_university_application_system|snare|tangled_rope|0.762188|0.693802|tangled_rope:0.762 snare:0.238
tcp_rfc9293_interoperability|rope|tangled_rope|0.874938|0.772744|rope:0.118 tangled_rope:0.875
texas_insurance_market_instability|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
traveling_salesperson_problem|snare|tangled_rope|0.697377|0.657806|tangled_rope:0.697 snare:0.303
trump_epa_greenhouse_gas_reversal|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
trump_indian_tariffs_2026|snare|tangled_rope|0.618349|0.628867|tangled_rope:0.618 snare:0.382
tx_hispanic_pivot|snare|tangled_rope|0.722643|0.670466|tangled_rope:0.723 snare:0.277
uk_help_to_buy_scheme|snare|tangled_rope|0.724298|0.671315|tangled_rope:0.724 snare:0.276
uk_hicbc_trap|snare|tangled_rope|0.634298|0.633451|tangled_rope:0.634 snare:0.366
ulysses_eumaeus_1904|tangled_rope|snare|0.726111|0.631325|tangled_rope:0.253 snare:0.726 piton:0.021
ulysses_ithaca_1904|tangled_rope|snare|0.707514|0.602206|tangled_rope:0.257 snare:0.708 piton:0.036
ulysses_lotus_1904|tangled_rope|snare|0.748984|0.646186|tangled_rope:0.231 snare:0.749 piton:0.020
ulysses_rocks_1904|tangled_rope|snare|0.793251|0.687802|tangled_rope:0.193 snare:0.793 piton:0.013
ulysses_tower_1904|tangled_rope|snare|0.569958|0.572994|tangled_rope:0.410 snare:0.570 piton:0.020
us_arms_transfer_policy|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
us_china_chip_tariffs_v2|snare|tangled_rope|0.596281|0.623465|tangled_rope:0.596 snare:0.404
us_employer_health_insurance|snare|tangled_rope|0.699806|0.658954|tangled_rope:0.700 snare:0.300
us_iran_drone_conflict|snare|tangled_rope|0.502679|0.613137|tangled_rope:0.503 snare:0.497
us_military_recruitment_advertising|snare|tangled_rope|0.509680|0.613243|tangled_rope:0.510 snare:0.490
us_suburban_zoning_2025|snare|tangled_rope|0.506619|0.613194|tangled_rope:0.507 snare:0.493
us_taiwan_arms_sales|snare|tangled_rope|0.575597|0.619482|tangled_rope:0.576 snare:0.424
us_usmca_china_leverage|snare|tangled_rope|0.534897|0.614452|tangled_rope:0.535 snare:0.465
us_visa_lottery|snare|tangled_rope|0.724298|0.671315|tangled_rope:0.724 snare:0.276
utopia_apocalypse_fragility|snare|tangled_rope|0.732755|0.676014|tangled_rope:0.733 snare:0.267
vns_implant_for_trd|snare|tangled_rope|0.537282|0.614660|tangled_rope:0.537 snare:0.463
wpl_scotland|snare|tangled_rope|0.823449|0.739650|tangled_rope:0.823 snare:0.177
yangtze_fishing_ban|snare|tangled_rope|0.573191|0.619118|tangled_rope:0.573 snare:0.427
ROPE_CLUSTER_ONLY: 180
INVOLVES_MTN_SCAFFOLD_PITON: 2
MOUNTAIN_PITON_DISAGREEMENTS:
  MTN_PITON: clawderberg_recursive_slop det=tangled_rope shadow=piton eps=0.85 supp=0.4 theater=0.95 sig=false_ci_rope dist=tangled_rope:0.142 snare:0.211 piton:0.647
  MTN_PITON: quine_self_replication det=mountain shadow=rope eps=0.2 supp=0.05 theater=0.01 sig=false_ci_rope dist=mountain:0.300 rope:0.598 tangled_rope:0.096
MEAN_SHADOW_TOP_P: 0.671085
=== END TASK 3 ===

=== TASK 4: NON-OVERLAPPING POPULATION ===
HARD_TOTAL: 182
MULTI_TYPE_ORBIT: 173
SINGLE_TYPE_ORBIT: 9
OVERLAP_PCT: 95.05
SINGLE_TYPE_DETAILS:
Constraint|DetType|ShadowType|OrbitTypes|Eps|Supp|Theater|NearestBoundary|BoundaryDist
coffee_cardiovascular_2026|tangled_rope|rope|tangled_rope|0.12|0.4|0.1|tangled_rope_supp_floor|0.0
cognitive_bicycle_scaffold|tangled_rope|rope|tangled_rope|0.2|0.3|0.15|rope_chi_ceiling|0.07601386373892077
ergo_lets_protocol|tangled_rope|rope|tangled_rope|0.15|0.1|0.15|mountain_supp_ceiling|0.05
fmt_oncology_2026|tangled_rope|rope|tangled_rope|0.18|0.45|0.12|tangled_rope_supp_floor|0.04999999999999999
horizon_liability_contract|tangled_rope|snare|tangled_rope|0.85|0.95|0.6|snare_suppression_floor|0.35
manganese_catalysis_2026|tangled_rope|rope|tangled_rope|0.18|0.25|0.08|rope_chi_ceiling|0.10341247736502876
narrative_engineering_2026|tangled_rope|rope|tangled_rope|0.15|0.45|0.05|tangled_rope_supp_floor|0.04999999999999999
rogue_wave_control_2026|tangled_rope|rope|tangled_rope|0.15|0.1|0.05|mountain_supp_ceiling|0.05
sm_addictive_design|tangled_rope|snare|tangled_rope|0.68|0.85|0.4|snare_epsilon_floor|0.22000000000000003
INVERSE_CHECK:
Constraint|DetType|H_norm|OrbitTypes
INVERSE_COUNT: 494
absorbing_markov_chain_trap|snare|0.129271|scaffold/snare/tangled_rope
abstraction_boundary_overrun|snare|0.097900|rope/snare
academic_fashion_modernism_2026|snare|0.030855|rope/snare
access_arbitrage|tangled_rope|0.014223|rope/tangled_rope
ad_fus_coordination|snare|0.270397|scaffold/snare
adaptive_lag_trap|snare|0.079716|rope/snare
adversarial_surface_inflation|snare|0.074072|rope/snare
adversarial_truth_decay|snare|0.045913|rope/snare
adverse_possession|tangled_rope|0.045378|rope/tangled_rope
advice_as_dangerous_gift|tangled_rope|0.015512|rope/tangled_rope
agency_atrophy|snare|0.050807|rope/snare
agent_opt_2026|snare|0.027527|rope/snare
aging_longevity_tests|tangled_rope|0.006711|rope/tangled_rope
ai_banal_capture|snare|0.194713|rope/snare
ai_driven_surveillance_sensor_layer|snare|0.200482|rope/snare
ai_edu_decentralization|tangled_rope|0.001210|rope/tangled_rope
ai_evaluators_matching|snare|0.189664|rope/snare
ai_nonconsensual_content_facilitation|snare|0.207450|rope/snare
ai_professional_displacement|snare|0.171695|rope/snare
ai_scholar_citation_trap|snare|0.073060|scaffold/snare/tangled_rope
ai_superpowers_2026|snare|0.024125|scaffold/snare
ai_training_data_dependency|tangled_rope|0.211712|rope/tangled_rope
airport_slot_use_it_or_lose_it|snare|0.290210|rope/snare
algeria_france_colonial_legacy|snare|0.261890|rope/snare
algorithmic_bias|snare|0.180136|rope/snare
algorithmic_epistemic_capture|snare|0.076550|rope/snare
alignment_tax_tradeoff|snare|0.230755|rope/snare
altruistic_misery_paradox_2026|snare|0.243106|rope/snare
alzheimers_levetiracetam|tangled_rope|0.014263|rope/tangled_rope
alzheimers_nlrp3_inflammasome|snare|0.019362|scaffold/snare
amish_technological_renunciation|snare|0.271060|rope/snare
ancestral_pueblo_hydrology|snare|0.260655|scaffold/snare
anticipatory_capacity_failure|snare|0.076881|rope/snare
appropriations_brinkmanship|snare|0.191268|rope/snare
armra_colostrum_regulation|tangled_rope|0.055920|rope/tangled_rope
arrows_impossibility_theorem|tangled_rope|0.001890|rope/tangled_rope
art_market_decoupling|snare|0.056578|rope/snare
artificial_snow_2026|tangled_rope|0.283086|rope/tangled_rope
asce_7_22_seismic_design|tangled_rope|0.097104|rope/tangled_rope
asymmetric_burden_distribution|snare|0.062269|rope/snare
atrophied_optimization_piton|tangled_rope|0.200641|rope/tangled_rope
attention_as_bottleneck_resource|snare|0.068211|rope/snare
attention_market_cannibalization|snare|0.063472|rope/snare
australia_social_ban_2026|snare|0.199200|rope/snare
authoritarian_power_paradox|snare|0.140719|rope/snare
autonomous_toolchain_sprawl|snare|0.120793|rope/snare
availability_heuristic|tangled_rope|0.161077|rope/tangled_rope
average_is_over_2026|snare|0.264419|rope/snare
axiom_reasoner_2026|tangled_rope|0.001955|scaffold/tangled_rope
bay_of_pigs_operational_silo|snare|0.075167|rope/snare
bayes_theorem_cognitive_bias|tangled_rope|0.014333|rope/tangled_rope
beehiiv_platform_model|tangled_rope|0.046316|rope/tangled_rope
belief_argument_conclusion|snare|0.096540|rope/snare
bgs_eigenvector_thermalization|tangled_rope|0.003189|rope/tangled_rope
big_data_astrophysics_arbitrage|tangled_rope|0.057790|rope/tangled_rope
biological_curiosity|mountain|0.050204|mountain/scaffold
bip_narrative_illusion|snare|0.260283|rope/snare
blackstone_carried_interest_taxation|tangled_rope|0.073633|rope/tangled_rope
blackstone_conflicts_of_interest|snare|0.290210|rope/snare
bnpl_payment_systems|tangled_rope|0.203751|rope/tangled_rope
boom_bust_path_dependency|rope|0.052149|piton/rope
bor_tax_exemption_nl|snare|0.207450|rope/snare
boundary_dissolution_risk|snare|0.060787|rope/snare
brazil_2026_general_elections|tangled_rope|0.022448|rope/tangled_rope
brazil_mexico_financial_requirement|snare|0.207450|rope/snare
buffons_needle_pi_estimation|tangled_rope|0.004345|rope/tangled_rope
burali_forti_paradox|mountain|0.086039|mountain/scaffold
burden_of_proof_engineering_safety|snare|0.237614|rope/snare
burden_of_proof_scientific|tangled_rope|0.203751|rope/tangled_rope
bureaucratic_legibility_collapse|snare|0.058101|rope/snare
bureaucratic_self_preservation|snare|0.083029|rope/snare
bushman_money_magic|snare|0.237614|rope/snare
capability_eval_overhang|snare|0.162559|rope/snare
capital_misallocation_spiral|snare|0.065461|rope/snare
capital_rotation_ai_narrative|tangled_rope|0.203751|rope/tangled_rope
carbon_credit_markets_2026|snare|0.186527|rope/snare/tangled_rope
cartel_drone_surveillance_el_paso|snare|0.276745|rope/snare
cascading_constraint_failure|snare|0.212772|rope/snare
cascading_uncertainty_2026|snare|0.103672|rope/snare
champions_bass_fishing_exclusion|tangled_rope|0.117409|rope/tangled_rope
child_marriage|snare|0.275519|rope/snare
china_africa_zero_tariff_2026|tangled_rope|0.024039|rope/tangled_rope
choice_architecture_design|tangled_rope|0.046316|rope/tangled_rope
citation_collapse_dynamics|snare|0.055769|rope/snare
civilizational_lifecycle_solara|snare|0.099279|rope/snare
civilizational_maintenance_debt|tangled_rope|0.002472|rope/tangled_rope
click_chemistry_paradigm_2026|mountain|0.031285|mountain/scaffold
climate_attribution_2026|mountain|0.127602|mountain/scaffold
cma|tangled_rope|0.237784|rope/tangled_rope
cmr_001|tangled_rope|0.001500|rope/tangled_rope
coalition_disinfo_framework_2026|tangled_rope|0.211712|rope/tangled_rope
cobra_effect|tangled_rope|0.001739|rope/tangled_rope
cognac_geopolitical_risk|snare|0.137920|rope/snare/tangled_rope
coinbase_crypto_volatility|tangled_rope|0.244789|rope/tangled_rope
cold_dark_matter_paradigm|tangled_rope|0.234251|rope/tangled_rope
collective_action_deadlock|snare|0.211001|rope/snare
collective_stupidity_2026|snare|0.024797|rope/snare
college_admissions_market|tangled_rope|0.068479|rope/tangled_rope
colombia_2026_presidential_election|tangled_rope|0.002583|rope/tangled_rope
comitatus_bond|tangled_rope|0.217601|rope/tangled_rope
communal_narcissism_social_trap|snare|0.149546|rope/snare
complexity_debt|snare|0.171199|rope/snare
compounding_logic|tangled_rope|0.001662|rope/tangled_rope
consensus_without_truth|snare|0.092555|rope/snare
constitutional_supremacy|snare|0.282871|rope/snare/tangled_rope
constraint_interaction_explosion|snare|0.208302|rope/snare
constraint_lagrange_multipliers|tangled_rope|0.014263|rope/tangled_rope
constraint_riemann_mapping|tangled_rope|0.014341|rope/tangled_rope
constraint_yoneda|tangled_rope|0.006410|rope/tangled_rope
container_capacity_mismatch|snare|0.076550|rope/snare
conversational_dogmas_interruption|snare|0.194219|rope/snare/tangled_rope
coordination_attack_vulnerability|snare|0.056876|rope/snare
coordination_fatigue|snare|0.255906|rope/snare
cost_of_observation|tangled_rope|0.004703|rope/tangled_rope
couples_residency_match|snare|0.241095|rope/snare/tangled_rope
cow_field_poop|tangled_rope|0.006410|rope/tangled_rope
creative_commons_licensing|tangled_rope|0.006101|rope/tangled_rope
credibility_inflation|snare|0.080669|rope/snare
crisis_signal_saturation|snare|0.049019|rope/snare
crispr_genomic_rewrite_2026|tangled_rope|0.006241|rope/tangled_rope
critical_actor_overcentralization|snare|0.048694|rope/snare
cross_domain_coupling_spiral|snare|0.053233|rope/snare
cs_ecmo_bridge|tangled_rope|0.000609|rope/tangled_rope
cuba_mandatrophic_collapse|snare|0.256462|rope/snare/tangled_rope
cultural_memory_decay|snare|0.187484|rope/snare
cultural_refragmentation_2026|snare|0.020219|rope/snare
cumbria_mine_rejection|snare|0.278421|rope/snare
cz_plea_agreement_2026|snare|0.219980|rope/snare
data_laundering_pipeline|snare|0.067158|rope/snare
data_replication_paradox|tangled_rope|0.294292|rope/tangled_rope
decision_latency_mismatch|tangled_rope|0.202239|rope/tangled_rope
delta_force_selection_2026|snare|0.077944|rope/snare
dutch_minority_govt_2026|tangled_rope|0.234251|rope/tangled_rope
edelman_2026_developed_stagnation|snare|0.264348|rope/snare/tangled_rope
edelman_2026_developing_volatility|tangled_rope|0.006711|rope/tangled_rope
education_unbundling_implementation|tangled_rope|0.008874|rope/tangled_rope
elite_capture_2026|snare|0.022734|rope/snare
elite_identity_capture_2026|snare|0.051569|rope/snare
em_clinical_guidelines|tangled_rope|0.044022|rope/tangled_rope
emergency_bridge_scaffold|snare|0.222476|scaffold/snare/tangled_rope
emergency_mode_lock_in|snare|0.050675|rope/snare
emergency_oversight_bureau|tangled_rope|0.092942|rope/scaffold/tangled_rope
emergency_powers_ratchet|snare|0.096561|scaffold/snare
emergent_goal_misalignment|snare|0.078407|rope/snare
empty_tomb_transformation|tangled_rope|0.006388|rope/tangled_rope
endocrine_disruption_society|snare|0.179898|rope/snare
endowment_effect|mountain|0.095744|mountain/scaffold
epistemic_free_rider_problem|snare|0.074656|rope/snare
epistemic_overload_collapse|snare|0.050959|rope/snare
epstein_espionage_2026|snare|0.027030|rope/snare
epstein_files_2026|snare|0.030597|rope/snare
epstein_honeytrap|snare|0.084606|rope/snare
erasmus_rejoining_scaffold|tangled_rope|0.140046|rope/scaffold/tangled_rope
ergo_nipopows|mountain|0.061281|mountain/scaffold
ergo_storage_rent|tangled_rope|0.000854|rope/tangled_rope
erised_expectation|snare|0.156057|rope/snare
eu_affordable_housing_initiative|tangled_rope|0.221845|rope/tangled_rope
eu_asylum_outsourcing_framework|snare|0.261890|rope/snare
eu_mercosur_trade_agreement|tangled_rope|0.211712|indexically_opaque/rope/tangled_rope
evfta_trade_agreement|tangled_rope|0.211712|rope/tangled_rope
evidence_half_life|snare|0.086304|rope/snare
evolutionary_mismatch_load|snare|0.262478|rope/snare
expert_disempowerment|snare|0.185296|rope/snare
exploration_vs_exploitation|snare|0.293333|rope/snare/tangled_rope
extraordinary_narrative_shift|tangled_rope|0.002843|rope/tangled_rope
faa_boeing_regulatory_capture|tangled_rope|0.033286|rope/tangled_rope
factional_instability|tangled_rope|0.065870|rope/tangled_rope
fiat_currency_lifecycle|snare|0.105469|rope/snare
fiber_optic_chip_tech|snare|0.293333|rope/snare/tangled_rope
financial_drag|snare|0.090492|rope/snare
fiscal_dominance_trap|snare|0.080454|rope/snare
fmeca_procedures_1980|tangled_rope|0.009721|rope/tangled_rope
fnl_shadow_probe|tangled_rope|0.039845|rope/tangled_rope
fragile_middle_layer_collapse|snare|0.077707|rope/snare
framing_effect|tangled_rope|0.026807|rope/tangled_rope
france_2027_presidential_election|tangled_rope|0.000618|rope/tangled_rope
france_local_elections_march_2026|tangled_rope|0.008345|scaffold/tangled_rope
franchisee_corporate_squeeze|tangled_rope|0.172973|rope/tangled_rope
frontex_pushback_coordination|snare|0.180719|rope/snare
gale_shapley|snare|0.254527|rope/snare
galois_theory_symmetry|mountain|0.090616|mountain/scaffold
gaza_aid_permit_revocation|snare|0.294534|rope/snare/tangled_rope
gemini_scientific_advancement|tangled_rope|0.014263|rope/tangled_rope
generational_replacement_inertia|snare|0.280945|rope/snare
genetic_algorithms_evolution|mountain|0.075118|mountain/scaffold
genetic_predisposition|tangled_rope|0.037440|rope/tangled_rope
genie_ip_constraint|snare|0.055699|scaffold/snare/tangled_rope
germany_tennet_takeover|tangled_rope|0.019493|rope/tangled_rope
ghost_fishing_gear|snare|0.270397|rope/snare
global_economic_anxiety_2026|snare|0.017812|rope/snare
global_stimulus_spree|tangled_rope|0.013477|scaffold/tangled_rope
goal_boundary_poisoning|snare|0.055162|rope/snare
gold_piton_2026|rope|0.033191|piton/rope
goodstein_theorem_finite_proof|tangled_rope|0.008895|rope/tangled_rope
governance_latency_gap|snare|0.174907|rope/snare
governance_overfitting|snare|0.292304|rope/snare
gpt5_codex_dev_cycle|tangled_rope|0.006901|rope/tangled_rope
graph_coloring_complexity|tangled_rope|0.009046|rope/tangled_rope
great_awakening_rekindling|snare|0.131824|rope/snare/tangled_rope
great_mongolian_road_economic_dependency|snare|0.290223|rope/snare/tangled_rope
greenland_defence_pact_2026|snare|0.260350|rope/snare/tangled_rope
greenland_seizure_trade_war|snare|0.260283|rope/snare
grete_samsa_transition|tangled_rope|0.079625|rope/tangled_rope
grievance_stack_overflow|snare|0.068004|rope/snare
gs1_gln_identification|tangled_rope|0.074093|rope/tangled_rope
gs1_standardized_identification|tangled_rope|0.010459|rope/tangled_rope
gs_market_clearing|tangled_rope|0.013648|rope/tangled_rope
guinea_worm_eradication|rope|0.207340|rope/tangled_rope
guthrie_kidnapping_2026|snare|0.097757|rope/snare
hanlons_razor|tangled_rope|0.001464|rope/tangled_rope
hasbro_licensing_restriction|tangled_rope|0.048695|rope/tangled_rope
hegemonic_entropy_2026|tangled_rope|0.143719|rope/tangled_rope
hershey_salt_strategy|tangled_rope|0.234251|rope/tangled_rope
heuristic_optimization|mountain|0.170520|mountain/scaffold
hhs_fetal_tissue_research_ban_2019|snare|0.207450|rope/snare
hidden_interdependency_risk|snare|0.131711|rope/snare
hilberts_hotel_infinity|mountain|0.047939|mountain/scaffold
hoa_covenants|tangled_rope|0.005486|rope/tangled_rope
hollow_state_syndrome|snare|0.054458|rope/snare
hp_liberalism|tangled_rope|0.056266|rope/tangled_rope
huang_expectation_resilience_2026|snare|0.180688|rope/snare
hydra_game|tangled_rope|0.063363|rope/tangled_rope
hypercompression_of_time_horizons|snare|0.063324|rope/snare
hypernormie_equilibrium|snare|0.076002|rope/snare
ice_raids_minnesota_2026|snare|0.231084|rope/snare
ice_safe_departure|snare|0.252104|rope/snare
identity_stack_incompatibility|snare|0.080864|rope/snare
incentive_surface_warping|snare|0.055769|rope/snare
india_semi_mission|tangled_rope|0.056237|rope/tangled_rope
individual_revolution_autonomy|snare|0.189664|rope/snare
indo_german_defense_pact|tangled_rope|0.234251|indexically_opaque/rope/tangled_rope
indonesia_penal_code_2023|snare|0.261890|indexically_opaque/rope/snare
inference_cost_scaling_law|snare|0.087681|rope/snare
information_foraging_theory|mountain|0.127602|mountain/scaffold
infrastructure_interoperability_decay|snare|0.054135|rope/snare
institutional_inertia_lock|tangled_rope|0.082783|rope/tangled_rope
institutional_memory_loss|snare|0.064821|rope/snare
institutional_mutation_domestication|snare|0.237941|rope/snare
institutional_mutation_without_selection|snare|0.049036|rope/snare
insult_wisdom_training|tangled_rope|0.077635|rope/tangled_rope
interface_contract_breakdown|snare|0.081286|rope/snare
internet_evolution_lifecycle|snare|0.199744|rope/snare
interpretive_frame_fragmentation|snare|0.050280|rope/snare
intertemporal_responsibility_gap|snare|0.066106|rope/snare
invisible_infrastructure_dependency|snare|0.054280|rope/snare
iran_guardian_council_vetting|snare|0.266253|rope/snare
iran_hijab_law|snare|0.261890|rope/snare
iran_mandatrophic_collapse|snare|0.236394|rope/snare
iran_war_room_2026|snare|0.099299|rope/snare
iron_law_of_oligarchy|snare|0.242603|rope/snare/tangled_rope
irreversible_policy_commitment|snare|0.254926|rope/snare
isa_education_scaffold|tangled_rope|0.004696|scaffold/tangled_rope
israel_egypt_gas_deal|tangled_rope|0.203751|rope/tangled_rope
israel_electoral_threshold|tangled_rope|0.203751|rope/tangled_rope
israel_override_clause|snare|0.237250|rope/snare
israel_surplus_vote_agreements|tangled_rope|0.002744|rope/tangled_rope
israeli_settlement_policy_authority_restriction|snare|0.293372|rope/snare/tangled_rope
iterated_function_system_convergence|tangled_rope|0.005235|rope/tangled_rope
ivt_accessibility_barrier|tangled_rope|0.001500|rope/tangled_rope
japanese_energy_scaffold_2025|snare|0.186527|rope/snare/tangled_rope
jevons_paradox|tangled_rope|0.037440|rope/tangled_rope
jupiter_composition_knowledge_gap|tangled_rope|0.014263|rope/tangled_rope
keltner_relationship_evaluation|tangled_rope|0.013894|rope/tangled_rope
kjv_linguistic_residue|rope|0.019786|piton/rope
kjv_textual_authority|tangled_rope|0.157860|rope/tangled_rope
labor_union_dues|tangled_rope|0.265983|rope/tangled_rope
landscape_of_fear_2026|tangled_rope|0.156043|rope/tangled_rope
latent_goal_activation|snare|0.053601|rope/snare
latent_regulatory_bomb|snare|0.288867|rope/snare
law_of_diminishing_returns|tangled_rope|0.006346|rope/tangled_rope
lcdm_small_scale_anomalies|tangled_rope|0.124804|rope/tangled_rope
legal_formalism_overhang|snare|0.059537|rope/snare
legibility_trap|snare|0.050807|rope/snare
legitimacy_without_capacity|snare|0.073059|rope/snare
legitimacy_without_effectiveness|snare|0.064524|rope/snare
lehman_repo_105|snare|0.251200|rope/snare
lindy_effect|rope|0.267752|rope/tangled_rope
linguistic_relativity_cultural_framing|tangled_rope|0.033394|rope/tangled_rope
liquidity_illusion|snare|0.087520|rope/snare
litchfield_sensitive_locations_2026|snare|0.138326|rope/snare
lorenz_attractor_dynamics|tangled_rope|0.008910|rope/tangled_rope
lsd_microdosing_professional_openness|tangled_rope|0.009009|rope/tangled_rope
magna_carta_liberties|tangled_rope|0.226688|rope/tangled_rope
maha_recovery_2026|tangled_rope|0.019142|scaffold/tangled_rope
maintenance_capacity_shortfall|snare|0.066997|rope/snare
maladaptive_selection_process|snare|0.063425|rope/snare
mandatrophic_margin_collapse|snare|0.241319|rope/snare
manga_distribution_duopoly|tangled_rope|0.294292|rope/tangled_rope
marriage_market_asymmetry_2026|snare|0.256752|rope/snare
mars_rovers_navigational_autonomy|tangled_rope|0.009726|rope/tangled_rope
mass_market_extinction_2026|snare|0.220646|rope/snare
matching_market_congestion_externality|tangled_rope|0.012820|rope/tangled_rope
max_flow_min_cut|snare|0.124421|rope/snare/tangled_rope
med_diet_consensus_2026|tangled_rope|0.206682|rope/tangled_rope
memetic_fitness_vs_truth|snare|0.065128|rope/snare
meta_governance_overload|snare|0.051839|rope/snare
meta_model_lock_in|snare|0.059699|rope/snare
meta_nuclear_power_agreement|tangled_rope|0.026525|rope/tangled_rope
meta_pay_or_okay_model|snare|0.283952|indexically_opaque/rope/snare
migration_decision_threshold|tangled_rope|0.051986|rope/tangled_rope
mil_std_461g_emi_control|tangled_rope|0.001750|rope/tangled_rope
mil_std_810f_tailoring|tangled_rope|0.005021|rope/tangled_rope
milano_cortina_2026|rope|0.007231|piton/rope
minimax_theorem_game_equilibrium|tangled_rope|0.153840|rope/tangled_rope
minnesota_sovereignty_2026|snare|0.183971|rope/snare
mit_tfus_2026|snare|0.069037|scaffold/snare/tangled_rope
model_autonomy_creep|snare|0.086696|rope/snare
model_collapse_feedback_loop|snare|0.066152|rope/snare
model_of_models_regression|snare|0.088302|rope/snare
moltbook_agent_theater|tangled_rope|0.291516|rope/tangled_rope
moltbook_breach_2026|snare|0.043815|rope/snare
monetary_regime_transition|tangled_rope|0.001464|rope/tangled_rope
moores_law|tangled_rope|0.039896|rope/tangled_rope
moral_outsourcing|snare|0.063923|rope/snare
multi_agent_reward_hacking|snare|0.048359|rope/snare
mvt_theorem_constraint|mountain|0.061387|mountain/scaffold
naming_as_control|snare|0.145057|rope/snare
narcissistic_ego_maintenance|snare|0.293209|rope/snare
narrative_capacity_exhaustion|snare|0.054718|rope/snare
narrative_overfitting|snare|0.048114|rope/snare
nato_arctic_defense_cooperation|tangled_rope|0.008838|rope/tangled_rope
negative_emissions_arbitrage|tangled_rope|0.244112|rope/tangled_rope
net_zero_stabilization|tangled_rope|0.025834|rope/tangled_rope
neural_interoperability|tangled_rope|0.101639|rope/tangled_rope
neurodiversity_spectrum|tangled_rope|0.207280|rope/tangled_rope
news_paywall_inequality|tangled_rope|0.048695|rope/tangled_rope
nfl_superbowl_halftime_exclusivity|tangled_rope|0.055920|rope/tangled_rope
nine_day_buffer|snare|0.289448|rope/snare/tangled_rope
noether_isomorphism_access|tangled_rope|0.002401|rope/tangled_rope
norm_erosion_threshold|snare|0.102898|rope/snare
north_sea_wind_grid|tangled_rope|0.289977|rope/tangled_rope
nsw_transmission_bottleneck|snare|0.050140|scaffold/snare/tangled_rope
nuclear_order_2026|snare|0.274227|rope/snare/tangled_rope
ny_private_school_discount|tangled_rope|0.048695|rope/tangled_rope
nyc_metrocard_art_licensing|tangled_rope|0.004417|rope/tangled_rope
olympic_legacy_curling_investment|tangled_rope|0.009072|rope/tangled_rope
olympic_medal_allocation|tangled_rope|0.012820|rope/tangled_rope
omelet_perfection_complexity|rope|0.138268|rope/tangled_rope
openai_api_access|snare|0.079934|scaffold/snare/tangled_rope
openai_codex_app_constraint|snare|0.073060|scaffold/snare/tangled_rope
openai_implicit_translator|snare|0.253989|rope/snare/tangled_rope
openbsd_netiquette_protocol|snare|0.232104|rope/snare/tangled_rope
openscholar_peer_review|snare|0.063273|scaffold/snare/tangled_rope
optimization_fragility|snare|0.198326|rope/snare
oral_glp1_market_access|snare|0.293333|rope/snare/tangled_rope
orbital_data_center_2026|snare|0.180949|rope/snare
oscar_campaign_spending|tangled_rope|0.008838|rope/tangled_rope
other_peoples_troubles_2026|snare|0.076001|rope/snare
overfitting_to_frameworks|snare|0.169134|rope/snare
paris_municipal_reform_2026|tangled_rope|0.001461|rope/tangled_rope
parkinsons_law|tangled_rope|0.001750|rope/tangled_rope
participatory_observer_hypothesis|tangled_rope|0.011845|rope/tangled_rope
paxsilica_framework|tangled_rope|0.211712|rope/tangled_rope
pe_fund_level_leverage|snare|0.271834|rope/snare
perovskite_self_etching|snare|0.260350|scaffold/snare/tangled_rope
peter_principle|snare|0.194219|rope/snare/tangled_rope
planetary_boundaries|snare|0.254527|rope/snare
plastic_asphalt_mandate|tangled_rope|0.055920|rope/tangled_rope
platonic_coparenting_decoupling|tangled_rope|0.001886|scaffold/tangled_rope
pna|tangled_rope|0.002423|rope/tangled_rope
policy_lag_catastrophe|snare|0.069754|rope/snare
politeness_face_negotiation|tangled_rope|0.056237|rope/tangled_rope
portugal_ad_stability_2026|tangled_rope|0.008345|scaffold/tangled_rope
portugal_polarization_threshold_2026|snare|0.247564|rope/snare/tangled_rope
postman_survival_protocol|rope|0.127465|rope/tangled_rope
power_set_axiomatic_extraction|tangled_rope|0.109749|rope/tangled_rope
power_without_responsibility|snare|0.050917|rope/snare
prestige_signal_inflation|snare|0.097469|rope/snare
price_signal_corruption|snare|0.082284|rope/snare
private_identity_integration|tangled_rope|0.009694|rope/tangled_rope
procedural_compliance_theater|snare|0.156213|rope/snare/tangled_rope
procedural_legitimacy_decay|snare|0.216191|rope/snare
project_vault_extraction_2026|snare|0.110583|rope/snare
protocol_drift_accumulation|snare|0.090775|rope/snare
publishing_embargo|tangled_rope|0.012820|rope/tangled_rope
quantum_entanglement_protocol|rope|0.263526|rope/tangled_rope
quellcrist_falconer_justice|snare|0.237614|rope/snare
qwerty_vs_dvorak|tangled_rope|0.157860|rope/tangled_rope
rare_earth_hydrogen_extraction|tangled_rope|0.048695|rope/tangled_rope
rare_earth_seabed_mining|tangled_rope|0.013968|rope/tangled_rope
rational_inertia_trap|snare|0.162058|rope/snare
recipe_scaling_ai|tangled_rope|0.002279|scaffold/tangled_rope
regulatory_capture|tangled_rope|0.004309|rope/tangled_rope
rent_seeking_equilibrium|snare|0.063923|rope/snare
reputational_cascade_failure|snare|0.061981|rope/snare
responsibility_dilution|snare|0.055328|rope/snare
responsibility_without_power|snare|0.049221|rope/snare
rfc9293_state_machine|rope|0.244281|rope/tangled_rope
riot_incentive_loop_2026|snare|0.064098|rope/snare
risk_socialization_threshold|snare|0.075805|rope/snare
ritual_transition_scaffold|tangled_rope|0.116592|rope/tangled_rope
ritual_without_belief|snare|0.048103|rope/snare
robustness_vs_efficiency_tradeoff|snare|0.200950|rope/snare
rosen_bridge_protocol|tangled_rope|0.002264|rope/tangled_rope
rotation_seven_black_soil|tangled_rope|0.000000|rope/tangled_rope
royal_navy_middle_east_withdrawal|tangled_rope|0.099006|rope/tangled_rope
russells_paradox_self_reference|tangled_rope|0.080836|scaffold/tangled_rope
russian_war_cannibalization|snare|0.266986|rope/snare
sa_renewable_price_differential|tangled_rope|0.026525|rope/tangled_rope
sadhu_integrity_protocol|tangled_rope|0.049971|rope/tangled_rope
sat_csp_complexity|tangled_rope|0.009046|rope/tangled_rope
satellite_d2m_standard|tangled_rope|0.007615|rope/tangled_rope
scam_compound_2026|snare|0.131206|rope/snare
second_order_unintended_consequences|snare|0.066997|rope/snare
semantic_attack_surface|snare|0.051441|rope/snare
semiconductor_mission_2026|tangled_rope|0.055920|rope/tangled_rope
shadow_pricing_failure|snare|0.088994|rope/snare
ship_of_theseus|tangled_rope|0.249677|rope/tangled_rope
shitty_feedback_handling|tangled_rope|0.008469|rope/tangled_rope
shobies_existential_commitment|tangled_rope|0.051255|rope/tangled_rope
shock_propagation_asymmetry|snare|0.058457|rope/snare
signal_without_control|snare|0.274217|rope/snare
silent_dependency_activation|snare|0.266782|rope/snare
skills_based_hiring|rope|0.281626|rope/tangled_rope
skolems_paradox|mountain|0.170520|mountain/scaffold
smartphone_ubiquity|tangled_rope|0.079625|rope/tangled_rope
social_loafing|tangled_rope|0.006320|rope/tangled_rope
social_media_participation_threshold|tangled_rope|0.038995|rope/tangled_rope
social_narrative_casting|tangled_rope|0.069985|rope/tangled_rope
soft_authoritarian_drift|snare|0.074072|rope/snare
sorites_paradox|tangled_rope|0.156043|rope/tangled_rope
south_china_sea_arbitration_2016_2026|snare|0.288612|rope/snare/tangled_rope
sovereignty_as_arbitrage|tangled_rope|0.004765|rope/tangled_rope
spain_digital_offensive_2026|snare|0.224506|rope/snare
st_petersburg_paradox|tangled_rope|0.002320|rope/tangled_rope
stable_marriage_coordination|tangled_rope|0.005983|rope/tangled_rope
status_flattening_effect|snare|0.099058|rope/snare
strange_attractor_dynamics|snare|0.123106|rope/snare/tangled_rope
structural_extraction_without_actor|snare|0.160764|rope/snare
sunk_cost_fallacy|snare|0.127366|rope/snare/tangled_rope
synthetic_data_feedback_loop|snare|0.084928|rope/snare
systemic_blindspot|snare|0.096987|rope/snare
tail_risk_compression|snare|0.118950|rope/snare
taiwan_storm_2026|snare|0.167830|rope/snare
taxonomy_drift|snare|0.231000|rope/snare
tcp_rfc9293_interoperability|rope|0.227256|rope/tangled_rope
teaching_horses_to_sing|tangled_rope|0.003787|rope/tangled_rope
tear_gas_repression_2026|snare|0.069102|rope/snare
temporal_scale_arbitrage|tangled_rope|0.003926|rope/tangled_rope
temporal_scarcity|rope|0.062065|piton/rope
thai_senate_veto_2026|snare|0.083029|rope/snare
the_bacchae_madness_protocol|snare|0.256462|rope/snare
the_calm_protocol_suppression|snare|0.276745|rope/snare
theatrical_neutrality|snare|0.036489|rope/snare
theory_of_visitors|tangled_rope|0.051255|rope/tangled_rope
toxoplasma_hub_2026|snare|0.009372|scaffold/snare
trade_secret_law|tangled_rope|0.234251|rope/tangled_rope
tragedy_of_the_commons|tangled_rope|0.003046|rope/tangled_rope
transformer_self_attention|tangled_rope|0.003676|rope/tangled_rope
transient_event_detection|tangled_rope|0.011587|rope/tangled_rope
trillion_bond_rush_2026|tangled_rope|0.063739|scaffold/tangled_rope
trivial_topology_info_asymmetry|tangled_rope|0.048695|rope/tangled_rope
trump_critical_minerals|tangled_rope|0.159308|rope/tangled_rope
trump_making_china_great_2026|tangled_rope|0.002423|rope/tangled_rope
tsp_computational_complexity|tangled_rope|0.099217|rope/tangled_rope
uk_graduate_visa_salary_threshold|snare|0.207450|rope/snare
uk_necc_formation|tangled_rope|0.197851|rope/tangled_rope
uk_unpaid_care_system|snare|0.286219|rope/snare
ukraine_tight_gas_pilot|tangled_rope|0.203751|rope/tangled_rope
ulysses_aeolus_1904|snare|0.251855|rope/snare/tangled_rope
ulysses_cyclops_1904|snare|0.192969|rope/snare/tangled_rope
ulysses_lestrygonians_1904|snare|0.290304|rope/snare/tangled_rope
ulysses_nausicaa_1904|snare|0.263350|rope/snare/tangled_rope
ulysses_scylla_1904|snare|0.289708|rope/snare/tangled_rope
ulysses_sirens_1904|snare|0.187981|rope/snare/tangled_rope
un_high_seas_treaty_2026|tangled_rope|0.022483|rope/tangled_rope
unclos_2026|tangled_rope|0.104112|rope/tangled_rope
union_protection_underperformance|tangled_rope|0.038995|rope/tangled_rope
unrequited_love_protocol|snare|0.016274|scaffold/snare
unrwa_eviction_order|snare|0.220526|rope/snare
us_greenland_envoy|snare|0.296880|rope/snare
us_israel_faa_502b_nonenforcement|snare|0.132461|rope/snare
us_labor_mobility|tangled_rope|0.026751|rope/tangled_rope
us_sdf_alliance_abandonment_2026|snare|0.296880|rope/snare
us_two_party_duopoly|snare|0.270397|rope/snare
us_vaccine_recommendation_dismantling_2026|snare|0.207450|rope/snare
us_venezuela_oil_pressure|snare|0.266665|rope/snare
us_venezuela_plausible_deniability_2025|snare|0.293002|rope/snare
value_alignment_drift|snare|0.298776|rope/snare
value_extraction_plateau|snare|0.045913|rope/snare
venezuela_oil_privatization_v1|snare|0.207450|rope/snare
viral_emergence_covid19_exemplar|rope|0.169366|piton/rope
viral_transmission_rates|snare|0.237941|rope/snare
visa_ipo_regulatory_compliance|tangled_rope|0.240745|rope/tangled_rope
visa_judgment_sharing_agreement|tangled_rope|0.159938|rope/tangled_rope
wikipedia_notability_requirement_2026|tangled_rope|0.153163|rope/tangled_rope
working_dog_training|tangled_rope|0.014617|rope/tangled_rope
world_factbook_sunset_2026|snare|0.121415|rope/snare
wpl_scotland|snare|0.260350|rope/snare/tangled_rope
xi_mao_ideological_centralization|snare|0.087607|rope/snare
yc_equity_squeeze|tangled_rope|0.048695|rope/tangled_rope
yt_ai_slop_incentive|snare|0.261890|rope/snare
zipfs_law|snare|0.166316|rope/snare
zombie_reasoning_2026|snare|0.028726|rope/snare
=== END TASK 4 ===

=== TASK 5: GAUSSIAN PROFILES ===
EMPIRICAL_PROFILES:
Type|Metric|Mu|Sigma
mountain|extractiveness|0.083411|0.058753
mountain|suppression|0.029612|0.018941
mountain|theater|0.029457|0.054946
rope|extractiveness|0.123333|0.071309
rope|suppression|0.379020|0.283475
rope|theater|0.204706|0.264036
tangled_rope|extractiveness|0.500300|0.187034
tangled_rope|suppression|0.544024|0.201290
tangled_rope|theater|0.249369|0.238293
snare|extractiveness|0.700983|0.131354
snare|suppression|0.754143|0.088138
snare|theater|0.423353|0.304820
scaffold|extractiveness|0.200000|0.120000
scaffold|suppression|0.380000|0.200000
scaffold|theater|0.140000|0.120000
piton|extractiveness|0.650000|0.150000
piton|suppression|0.690000|0.150000
piton|theater|0.850000|0.080000
LARGE_SIGMA_FLAGS:
  LARGE_SIGMA: rope suppression sigma=0.283475
  LARGE_SIGMA: rope theater sigma=0.264036
  LARGE_SIGMA: snare theater sigma=0.304820
ROPE_EPS_DISTRIBUTION:
ROPE_EPS_COUNT: 51
  ROPE_EPS: 0.000000
  ROPE_EPS: 0.020000
  ROPE_EPS: 0.020000
  ROPE_EPS: 0.020000
  ROPE_EPS: 0.040000
  ROPE_EPS: 0.040000
  ROPE_EPS: 0.050000
  ROPE_EPS: 0.050000
  ROPE_EPS: 0.050000
  ROPE_EPS: 0.050000
  ROPE_EPS: 0.050000
  ROPE_EPS: 0.050000
  ROPE_EPS: 0.050000
  ROPE_EPS: 0.050000
  ROPE_EPS: 0.050000
  ROPE_EPS: 0.050000
  ROPE_EPS: 0.080000
  ROPE_EPS: 0.100000
  ROPE_EPS: 0.100000
  ROPE_EPS: 0.100000
  ROPE_EPS: 0.100000
  ROPE_EPS: 0.100000
  ROPE_EPS: 0.100000
  ROPE_EPS: 0.100000
  ROPE_EPS: 0.100000
  ROPE_EPS: 0.110000
  ROPE_EPS: 0.120000
  ROPE_EPS: 0.120000
  ROPE_EPS: 0.150000
  ROPE_EPS: 0.150000
  ROPE_EPS: 0.150000
  ROPE_EPS: 0.150000
  ROPE_EPS: 0.150000
  ROPE_EPS: 0.150000
  ROPE_EPS: 0.150000
  ROPE_EPS: 0.150000
  ROPE_EPS: 0.200000
  ROPE_EPS: 0.200000
  ROPE_EPS: 0.200000
  ROPE_EPS: 0.200000
  ROPE_EPS: 0.200000
  ROPE_EPS: 0.200000
  ROPE_EPS: 0.200000
  ROPE_EPS: 0.200000
  ROPE_EPS: 0.200000
  ROPE_EPS: 0.200000
  ROPE_EPS: 0.220000
  ROPE_EPS: 0.250000
  ROPE_EPS: 0.250000
  ROPE_EPS: 0.250000
  ROPE_EPS: 0.250000
ROPE_EPS_BINS: [0,0.25]=47 [0.25,0.50]=4 [0.50,1.0]=0
OVERRIDE_ROPE_ANALYSIS:
OVERRIDE_ROPE_COUNT: 27
  OVERRIDE_ROPE: asean_ceasefire_2011 sig=coupling_invariant_rope eps=0.150000 hn=0.155706
  OVERRIDE_ROPE: automatic_enrollment_defaults sig=constructed_low_extraction eps=0.050000 hn=0.059972
  OVERRIDE_ROPE: berkshire_compounding_culture sig=constructed_low_extraction eps=0.100000 hn=0.084865
  OVERRIDE_ROPE: boundary_protocol sig=coupling_invariant_rope eps=0.000000 hn=0.155706
  OVERRIDE_ROPE: cancer_chronotherapy_timing sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: cinderella_midnight_deadline sig=coupling_invariant_rope eps=0.100000 hn=0.155706
  OVERRIDE_ROPE: copyleft_viral_licensing sig=constructed_low_extraction eps=0.050000 hn=0.099282
  OVERRIDE_ROPE: cuny_light_2026 sig=constructed_low_extraction eps=0.050000 hn=0.037854
  OVERRIDE_ROPE: decentralized_infrastructure_rope sig=constructed_low_extraction eps=0.080000 hn=0.108191
  OVERRIDE_ROPE: fair_use_doctrine sig=constructed_low_extraction eps=0.100000 hn=0.194897
  OVERRIDE_ROPE: ice_memory_archive sig=coupling_invariant_rope eps=0.100000 hn=0.155706
  OVERRIDE_ROPE: microrobot_manipulation sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: mom_z14_2026 sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: open_source_commons sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: perseverance_rover_autonomy sig=coupling_invariant_rope eps=0.100000 hn=0.155706
  OVERRIDE_ROPE: planetary_diet_constraint_2026 sig=constructed_low_extraction eps=0.100000 hn=0.253492
  OVERRIDE_ROPE: portuguese_presidential_term_limits sig=constructed_low_extraction eps=0.020000 hn=0.178209
  OVERRIDE_ROPE: rafah_crossing_lifeline sig=coupling_invariant_rope eps=0.150000 hn=0.155706
  OVERRIDE_ROPE: rare_earth_coop_2026 sig=constructed_low_extraction eps=0.020000 hn=0.091502
  OVERRIDE_ROPE: spv_variations_us_cold sig=coupling_invariant_rope eps=0.020000 hn=0.155706
  OVERRIDE_ROPE: sts86_ascent_checklist sig=constructed_low_extraction eps=0.050000 hn=0.172182
  OVERRIDE_ROPE: swift_piton_snap sig=coupling_invariant_rope eps=0.040000 hn=0.155706
  OVERRIDE_ROPE: thai_article_112_mountain sig=constructed_low_extraction eps=0.040000 hn=0.185124
  OVERRIDE_ROPE: vertebrate_turning_point_2026 sig=constructed_low_extraction eps=0.050000 hn=0.164023
  OVERRIDE_ROPE: viral_emergence_covid19_exemplar sig=constructed_low_extraction eps=0.150000 hn=0.169366
  OVERRIDE_ROPE: wikipedia_crowdsourcing_2026 sig=constructed_low_extraction eps=0.050000 hn=0.168470
  OVERRIDE_ROPE: wikipedia_noncommercial_model sig=coupling_invariant_rope eps=0.120000 hn=0.155706
NON_OVERRIDE_ROPE_ENTROPY_MEAN: 0.258513 (n=24)
OVERRIDE_ROPE_ENTROPY_MEAN: 0.147837 (n=27)
ALL_TYPE_EPS_STATS:
  mountain: n=129 mean=0.083411 std=0.058753 min=0.000000 max=0.200000
  rope: n=51 mean=0.123333 std=0.071309 min=0.000000 max=0.250000
  tangled_rope: n=333 mean=0.500300 std=0.187034 min=0.000000 max=1.000000
  snare: n=519 mean=0.700983 std=0.131354 min=0.490000 max=1.000000
  scaffold: n=0 (insufficient)
  piton: n=0 (insufficient)
=== END TASK 5 ===

=== TASK 6: CROSS-DIAGNOSTIC CORRELATION ===
HIGH_ENTROPY_COUNT: 15 (threshold=0.4000)
OMEGA_HIGH_ENTROPY: 0/15 (0.00%)
OMEGA_LOW_ENTROPY: 0/1017 (0.00%)
BOLTZMANN_NC_HIGH_ENTROPY: 13/15 (86.67%)
BOLTZMANN_NC_LOW_ENTROPY: 848/1017 (83.38%)
LOW_PURITY_HIGH_ENTROPY: 8/15 (53.33%)
LOW_PURITY_LOW_ENTROPY: 438/1017 (43.07%)
PURITY_AVAILABLE_HIGH: 14/15
PURITY_AVAILABLE_LOW: 993/1017
AVG_PURITY_HIGH_ENTROPY: 0.577702380952381
AVG_PURITY_LOW_ENTROPY: 0.5546711983887193
MULTI_ORBIT_HIGH_ENTROPY: 15/15 (100.00%)
MULTI_ORBIT_LOW_ENTROPY: 774/1017 (76.11%)
=== END TASK 6 ===

=== END DIAGNOSTIC OUTPUT ===
