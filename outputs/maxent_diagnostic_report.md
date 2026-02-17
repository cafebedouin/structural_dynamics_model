
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================
=== MAXENT DIAGNOSTIC OUTPUT ===
SUMMARY: NTotal=1032 MeanEntropy=0.198018 NHighUncertainty=15 NHard=177 NSoft=0

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
  mountain: 128
  rope: 51
  snare: 519
  tangled_rope: 334
RESIDUAL_TYPE_COUNT: 0
=== END TASK 1 ===

=== TASK 2: PER-TYPE ENTROPY BREAKDOWN ===
TYPE_ENTROPY_TABLE:
Type|Count|Mean|Median|Min|Max|StdDev
mountain|128|0.154683|0.155706|0.031866|0.520407|0.044029
rope|51|0.200624|0.155706|0.007705|0.469823|0.116276
snare|519|0.251128|0.290832|0.009353|0.449179|0.124242
tangled_rope|334|0.131701|0.155706|0.000000|0.494303|0.115362
=== END TASK 2 ===

=== TASK 3: HARD DISAGREEMENTS ===
TOTAL_HARD: 177
DISAGREEMENT_PAIRS:
DetType->ShadowType|Count
mountain->rope|1
rope->tangled_rope|15
snare->tangled_rope|135
tangled_rope->mountain|1
tangled_rope->piton|1
tangled_rope->rope|7
tangled_rope->snare|17
HARD_DISAGREEMENT_DETAILS:
Constraint|DetType|ShadowType|ShadowTopP|ShadowConf|Distribution
ai_adoption_stigma|snare|tangled_rope|0.610542|0.626785|tangled_rope:0.611 snare:0.389
ai_performance_watermark|snare|tangled_rope|0.758031|0.691085|tangled_rope:0.758 snare:0.242
arctic_maritime_control|snare|tangled_rope|0.615744|0.628173|tangled_rope:0.616 snare:0.384
arg_ev_tariff|snare|tangled_rope|0.515528|0.613370|tangled_rope:0.516 snare:0.484
astm_d638_tensile_testing|rope|tangled_rope|0.578248|0.598417|rope:0.414 tangled_rope:0.578
atrophied_optimization_piton|tangled_rope|snare|0.903668|0.801222|tangled_rope:0.083 snare:0.904 piton:0.014
board_of_peace_2026|tangled_rope|snare|0.779835|0.681860|tangled_rope:0.209 snare:0.780 piton:0.011
boltzmann_universality_2026|rope|tangled_rope|0.713091|0.648119|rope:0.281 tangled_rope:0.713
brain_network_paradigm_2026|snare|tangled_rope|0.757244|0.690425|tangled_rope:0.757 snare:0.243
broke_vs_poor_grocery_math|snare|tangled_rope|0.639419|0.635134|tangled_rope:0.639 snare:0.361
cancer_prevention|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
carbon_credit_markets_2026|snare|tangled_rope|0.894882|0.812235|tangled_rope:0.895 snare:0.105
carrier_deployment_deterrence|snare|tangled_rope|0.515528|0.613370|tangled_rope:0.516 snare:0.484
carrying_capacity|snare|tangled_rope|0.730742|0.674886|tangled_rope:0.731 snare:0.269
china_ev_export_oversupply|snare|tangled_rope|0.721814|0.669984|tangled_rope:0.722 snare:0.278
china_vactrain_standard|snare|tangled_rope|0.758031|0.691085|tangled_rope:0.758 snare:0.242
clawderberg_recursive_slop|tangled_rope|piton|0.647994|0.505697|tangled_rope:0.141 snare:0.211 piton:0.648
climate_event_attribution|snare|tangled_rope|0.532594|0.614278|tangled_rope:0.533 snare:0.467
cn_tech_decoupling_security_software|snare|tangled_rope|0.536455|0.614592|tangled_rope:0.536 snare:0.464
codex_access|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
coffee_cardiovascular_2026|tangled_rope|rope|0.952357|0.875372|rope:0.952 tangled_rope:0.018 scaffold:0.029
cognac_geopolitical_risk|snare|tangled_rope|0.932076|0.861157|tangled_rope:0.932 snare:0.068
cognitive_bicycle_scaffold|tangled_rope|rope|0.896043|0.774007|rope:0.896 tangled_rope:0.045 scaffold:0.059
cognitive_induction_gap|snare|tangled_rope|0.730742|0.674886|tangled_rope:0.731 snare:0.269
colorado_sbe_decentralization_friction|snare|tangled_rope|0.720543|0.669350|tangled_rope:0.721 snare:0.279
constitutional_consecration|snare|tangled_rope|0.511835|0.613293|tangled_rope:0.512 snare:0.488
constitutional_supremacy|snare|tangled_rope|0.796437|0.717930|tangled_rope:0.796 snare:0.204
consumer_debt_slavery|snare|tangled_rope|0.542320|0.615148|tangled_rope:0.542 snare:0.458
conversational_dogmas_interruption|snare|tangled_rope|0.888274|0.804485|tangled_rope:0.888 snare:0.112
couples_residency_match|snare|tangled_rope|0.845660|0.759819|tangled_rope:0.846 snare:0.154
credentialism_national_security|snare|tangled_rope|0.585516|0.621286|tangled_rope:0.586 snare:0.414
cuban_missile_crisis_excomm_deliberation|rope|tangled_rope|0.527549|0.565077|rope:0.451 tangled_rope:0.528 scaffold:0.022
cultural_homogenization_social_media|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
data_privacy_regulation|snare|tangled_rope|0.615744|0.628173|tangled_rope:0.616 snare:0.384
debt_trap_microfinance|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
deferential_realism_framework|rope|tangled_rope|0.757994|0.658934|rope:0.226 tangled_rope:0.758 scaffold:0.016
dexy_gold_protocol|rope|tangled_rope|0.591877|0.575248|rope:0.386 tangled_rope:0.592 scaffold:0.022
digital_identity_tether|snare|tangled_rope|0.505448|0.613177|tangled_rope:0.505 snare:0.495
dldr_information_policy|rope|tangled_rope|0.511915|0.561947|rope:0.465 tangled_rope:0.512 scaffold:0.023
dn_paywall|snare|tangled_rope|0.622652|0.629988|tangled_rope:0.623 snare:0.377
dunning_kruger_effect|snare|tangled_rope|0.553749|0.616321|tangled_rope:0.554 snare:0.446
ec_meta_manus_block|snare|tangled_rope|0.536455|0.614592|tangled_rope:0.536 snare:0.464
edelman_2026_insularity|snare|tangled_rope|0.653964|0.640033|tangled_rope:0.654 snare:0.346
electrification_scale_2026|rope|tangled_rope|0.699868|0.640295|rope:0.293 tangled_rope:0.700
elliq_ai_companion|snare|tangled_rope|0.667349|0.645007|tangled_rope:0.667 snare:0.333
emergency_bridge_scaffold|snare|tangled_rope|0.864500|0.778476|tangled_rope:0.864 snare:0.135
ergo_lets_protocol|tangled_rope|rope|0.983435|0.943921|rope:0.983
eu_digital_services_act|snare|tangled_rope|0.532594|0.614278|tangled_rope:0.533 snare:0.467
eu_ev_tariff_wall|snare|tangled_rope|0.545324|0.615383|tangled_rope:0.545 snare:0.455
eu_unanimity_rule_foreign_policy|snare|tangled_rope|0.532594|0.614278|tangled_rope:0.533 snare:0.467
eurozone_fragmentation_2026|tangled_rope|snare|0.580970|0.619116|tangled_rope:0.419 snare:0.581
exploration_vs_exploitation|snare|tangled_rope|0.779294|0.705315|tangled_rope:0.779 snare:0.221
fcc_dji_covered_list|snare|tangled_rope|0.594155|0.623007|tangled_rope:0.594 snare:0.406
fda_component_efficacy_standard|snare|tangled_rope|0.545324|0.615383|tangled_rope:0.545 snare:0.455
fiber_optic_chip_tech|snare|tangled_rope|0.779294|0.705315|tangled_rope:0.779 snare:0.221
fine_particle_policy|snare|tangled_rope|0.615744|0.628173|tangled_rope:0.616 snare:0.384
fitts_law_industrial_application|snare|tangled_rope|0.538132|0.614749|tangled_rope:0.538 snare:0.462
fmt_oncology_2026|tangled_rope|rope|0.901528|0.782403|rope:0.902 tangled_rope:0.052 scaffold:0.046
fraser_river_salmon_regulation|snare|tangled_rope|0.506449|0.613185|tangled_rope:0.506 snare:0.494
future_dsm_integration|snare|tangled_rope|0.506449|0.613185|tangled_rope:0.506 snare:0.494
g7_debt_trap|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
gamblers_ruin_stochastic_extinction|tangled_rope|snare|0.705982|0.661946|tangled_rope:0.294 snare:0.706
gaza_border_control_rafah|snare|tangled_rope|0.598709|0.624066|tangled_rope:0.599 snare:0.401
glen_canyon_water_allocation|snare|tangled_rope|0.667349|0.645007|tangled_rope:0.667 snare:0.333
global_digital_divide|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
goodharts_law|snare|tangled_rope|0.757244|0.690425|tangled_rope:0.757 snare:0.243
google_ai_search_overview|snare|tangled_rope|0.536455|0.614592|tangled_rope:0.536 snare:0.464
google_universal_commerce_protocol|snare|tangled_rope|0.610542|0.626785|tangled_rope:0.611 snare:0.389
great_awakening_rekindling|snare|tangled_rope|0.936265|0.867307|tangled_rope:0.936 snare:0.064
great_mongolian_road_economic_dependency|snare|tangled_rope|0.783405|0.708308|tangled_rope:0.783 snare:0.217
greenland_defence_pact_2026|snare|tangled_rope|0.821897|0.738319|tangled_rope:0.822 snare:0.178
greshams_law|snare|tangled_rope|0.529662|0.614106|tangled_rope:0.530 snare:0.470
gs1_standardized_identification|tangled_rope|snare|0.997469|0.989545|snare:0.997
guano_wealth_extraction|snare|tangled_rope|0.615744|0.628173|tangled_rope:0.616 snare:0.384
guinea_worm_eradication|rope|tangled_rope|0.900429|0.796534|rope:0.086 tangled_rope:0.900 scaffold:0.014
hammurabi_lex_talionis|snare|tangled_rope|0.641060|0.635509|tangled_rope:0.641 snare:0.359
hawthorne_effect|snare|tangled_rope|0.664766|0.643995|tangled_rope:0.665 snare:0.335
hegemonic_entropy_2026|tangled_rope|snare|0.941672|0.856901|tangled_rope:0.016 snare:0.942 piton:0.043
horizon_liability_contract|tangled_rope|snare|0.864503|0.778293|tangled_rope:0.135 snare:0.865
hu_2026_election_rules|snare|tangled_rope|0.685029|0.652282|tangled_rope:0.685 snare:0.315
hub_short_form_tv_market_fragmentation|snare|tangled_rope|0.721814|0.669984|tangled_rope:0.722 snare:0.278
india_nuclear_liability_act_2010|snare|tangled_rope|0.515528|0.613370|tangled_rope:0.516 snare:0.484
indian_import_tariffs_eu|snare|tangled_rope|0.545324|0.615383|tangled_rope:0.545 snare:0.455
inner_model_confirmation_bias|snare|tangled_rope|0.615744|0.628173|tangled_rope:0.616 snare:0.384
innovators_dilemma|snare|tangled_rope|0.643635|0.636421|tangled_rope:0.644 snare:0.356
institutional_trust_decay|tangled_rope|snare|0.573179|0.522370|tangled_rope:0.367 snare:0.573 piton:0.060
international_seabed_mining_regime|snare|tangled_rope|0.506449|0.613185|tangled_rope:0.506 snare:0.494
iron_law_of_oligarchy|snare|tangled_rope|0.841496|0.755958|tangled_rope:0.841 snare:0.158
israel_norwegian_law|snare|tangled_rope|0.669279|0.645679|tangled_rope:0.669 snare:0.331
japanese_energy_scaffold_2025|snare|tangled_rope|0.894882|0.812235|tangled_rope:0.895 snare:0.105
job_hunt_volume_system_2026|snare|tangled_rope|0.678857|0.649658|tangled_rope:0.679 snare:0.321
jp_eez_enforcement|snare|tangled_rope|0.508355|0.613178|tangled_rope:0.508 snare:0.492
lindy_effect|rope|tangled_rope|0.840401|0.734744|rope:0.150 tangled_rope:0.840
lung_transplant_protocol|snare|tangled_rope|0.615744|0.628173|tangled_rope:0.616 snare:0.384
manganese_catalysis_2026|tangled_rope|rope|0.936034|0.844367|rope:0.936 tangled_rope:0.022 scaffold:0.042
max_flow_min_cut|snare|tangled_rope|0.941163|0.874774|tangled_rope:0.941 snare:0.059
medical_residency_match|snare|tangled_rope|0.664196|0.643796|tangled_rope:0.664 snare:0.336
metabolic_constraint_cognition|snare|tangled_rope|0.572951|0.619109|tangled_rope:0.573 snare:0.427
mexican_airline_merger|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
mrna_melanoma_therapy|snare|tangled_rope|0.545324|0.615383|tangled_rope:0.545 snare:0.455
mvt_theorem_constraint|tangled_rope|mountain|0.669885|0.629547|mountain:0.670 rope:0.325
narrative_engineering_2026|tangled_rope|rope|0.940204|0.850489|rope:0.940 tangled_rope:0.030 scaffold:0.030
ncaa_eligibility_rules|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
neural_substrate_2026|snare|tangled_rope|0.602019|0.624769|tangled_rope:0.602 snare:0.398
nfl_superbowl_marketing_regulation|snare|tangled_rope|0.615744|0.628173|tangled_rope:0.616 snare:0.384
notary_ink_dependency|tangled_rope|snare|0.495123|0.548380|tangled_rope:0.475 snare:0.495 piton:0.030
omelet_perfection_complexity|rope|tangled_rope|0.941897|0.863192|rope:0.050 tangled_rope:0.942
openai_health_review|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
openai_implicit_translator|snare|tangled_rope|0.829270|0.744706|tangled_rope:0.829 snare:0.171
openai_prism_development|snare|tangled_rope|0.515528|0.613370|tangled_rope:0.516 snare:0.484
openbsd_netiquette_protocol|snare|tangled_rope|0.852540|0.766509|tangled_rope:0.853 snare:0.147
openclaw_data_lock_in|snare|tangled_rope|0.548031|0.615682|tangled_rope:0.548 snare:0.452
openclaw_regulation|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
oral_glp1_market_access|snare|tangled_rope|0.779294|0.705315|tangled_rope:0.779 snare:0.221
p_g_golden_pear_surveillance|snare|tangled_rope|0.615744|0.628173|tangled_rope:0.616 snare:0.384
perovskite_self_etching|snare|tangled_rope|0.821897|0.738319|tangled_rope:0.822 snare:0.178
peter_principle|snare|tangled_rope|0.888274|0.804485|tangled_rope:0.888 snare:0.112
poetic_verse_and_past|snare|tangled_rope|0.667587|0.645111|tangled_rope:0.668 snare:0.332
portugal_polarization_threshold_2026|snare|tangled_rope|0.835850|0.750792|tangled_rope:0.836 snare:0.164
private_credit_market_opacity|snare|tangled_rope|0.515528|0.613370|tangled_rope:0.516 snare:0.484
quantum_entanglement_protocol|rope|tangled_rope|0.833920|0.740593|rope:0.163 tangled_rope:0.834
quine_self_replication|mountain|rope|0.594522|0.479593|mountain:0.297 rope:0.595 tangled_rope:0.104
radiologic_diagnostic_threshold|snare|tangled_rope|0.720543|0.669350|tangled_rope:0.721 snare:0.279
rare_earth_export_restrictions|snare|tangled_rope|0.615744|0.628173|tangled_rope:0.616 snare:0.384
rfc9293_state_machine|rope|tangled_rope|0.859429|0.757673|rope:0.134 tangled_rope:0.859
rogue_wave_control_2026|tangled_rope|rope|0.770427|0.667344|rope:0.770 tangled_rope:0.214 scaffold:0.016
rule_update_failure|tangled_rope|snare|0.655355|0.615028|tangled_rope:0.335 snare:0.655 piton:0.010
rules_based_international_order|snare|tangled_rope|0.667587|0.645111|tangled_rope:0.668 snare:0.332
sapir_whorf_hypothesis|snare|tangled_rope|0.643635|0.636421|tangled_rope:0.644 snare:0.356
scientific_paradigm_lifecycle|tangled_rope|snare|0.735379|0.664409|tangled_rope:0.260 snare:0.735
seedance_export_restriction|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
semiconductor_fabrication_chokepoint|snare|tangled_rope|0.536455|0.614592|tangled_rope:0.536 snare:0.464
shadow_fleet_sanctions_evasion|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
sk_ai_act_2026|snare|tangled_rope|0.515528|0.613370|tangled_rope:0.516 snare:0.484
skills_based_hiring|rope|tangled_rope|0.843250|0.725041|rope:0.137 tangled_rope:0.843 scaffold:0.019
slow_crisis_invisibility|snare|tangled_rope|0.652230|0.639429|tangled_rope:0.652 snare:0.348
sludge_bureaucratic_friction|snare|tangled_rope|0.730742|0.674886|tangled_rope:0.731 snare:0.269
sm_addictive_design|tangled_rope|snare|0.816575|0.734034|tangled_rope:0.183 snare:0.817
somatic_focusing_awareness|rope|tangled_rope|0.722129|0.634441|rope:0.261 tangled_rope:0.722 scaffold:0.017
south_china_sea_arbitration_2016_2026|snare|tangled_rope|0.785891|0.710041|tangled_rope:0.786 snare:0.214
start_treaty|snare|tangled_rope|0.615744|0.628173|tangled_rope:0.616 snare:0.384
sti_clinical_testing_bottleneck|snare|tangled_rope|0.663089|0.643207|tangled_rope:0.663 snare:0.337
strait_coercion_2025|snare|tangled_rope|0.522881|0.613724|tangled_rope:0.523 snare:0.477
strange_attractor_dynamics|snare|tangled_rope|0.941989|0.876098|tangled_rope:0.942 snare:0.058
strange_attractor_systemic_risk|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
strategic_deep_sea_rare_earth_mining|snare|tangled_rope|0.615744|0.628173|tangled_rope:0.616 snare:0.384
streaming_bundling_mandate|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
student_loan_default_cliff|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
student_loan_interest_accrual|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
sunk_cost_fallacy|snare|tangled_rope|0.939247|0.871804|tangled_rope:0.939 snare:0.061
taiwan_ids_program|snare|tangled_rope|0.532594|0.614278|tangled_rope:0.533 snare:0.467
taiwan_university_application_system|snare|tangled_rope|0.759966|0.692365|tangled_rope:0.760 snare:0.240
tcp_rfc9293_interoperability|rope|tangled_rope|0.876510|0.774670|rope:0.117 tangled_rope:0.877
texas_insurance_market_instability|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
traveling_salesperson_problem|snare|tangled_rope|0.694924|0.656670|tangled_rope:0.695 snare:0.305
trump_epa_greenhouse_gas_reversal|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
trump_indian_tariffs_2026|snare|tangled_rope|0.615744|0.628173|tangled_rope:0.616 snare:0.384
tx_hispanic_pivot|snare|tangled_rope|0.720543|0.669350|tangled_rope:0.721 snare:0.279
uk_help_to_buy_scheme|snare|tangled_rope|0.721814|0.669984|tangled_rope:0.722 snare:0.278
uk_hicbc_trap|snare|tangled_rope|0.631800|0.632691|tangled_rope:0.632 snare:0.368
ulysses_eumaeus_1904|tangled_rope|snare|0.729666|0.633294|tangled_rope:0.249 snare:0.730 piton:0.021
ulysses_ithaca_1904|tangled_rope|snare|0.708468|0.602694|tangled_rope:0.256 snare:0.708 piton:0.036
ulysses_lotus_1904|tangled_rope|snare|0.752396|0.648322|tangled_rope:0.227 snare:0.752 piton:0.020
ulysses_rocks_1904|tangled_rope|snare|0.795928|0.689856|tangled_rope:0.191 snare:0.796 piton:0.013
ulysses_tower_1904|tangled_rope|snare|0.574535|0.573589|tangled_rope:0.405 snare:0.575 piton:0.020
us_arms_transfer_policy|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
us_china_chip_tariffs_v2|snare|tangled_rope|0.594155|0.623007|tangled_rope:0.594 snare:0.406
us_employer_health_insurance|snare|tangled_rope|0.697085|0.657678|tangled_rope:0.697 snare:0.303
us_military_recruitment_advertising|snare|tangled_rope|0.506449|0.613185|tangled_rope:0.506 snare:0.494
us_suburban_zoning_2025|snare|tangled_rope|0.503926|0.613162|tangled_rope:0.504 snare:0.496
us_taiwan_arms_sales|snare|tangled_rope|0.573353|0.619106|tangled_rope:0.573 snare:0.427
us_usmca_china_leverage|snare|tangled_rope|0.532594|0.614278|tangled_rope:0.533 snare:0.467
us_visa_lottery|snare|tangled_rope|0.721814|0.669984|tangled_rope:0.722 snare:0.278
utopia_apocalypse_fragility|snare|tangled_rope|0.730742|0.674886|tangled_rope:0.731 snare:0.269
vns_implant_for_trd|snare|tangled_rope|0.536455|0.614592|tangled_rope:0.536 snare:0.464
wpl_scotland|snare|tangled_rope|0.821897|0.738319|tangled_rope:0.822 snare:0.178
yangtze_fishing_ban|snare|tangled_rope|0.570297|0.618651|tangled_rope:0.570 snare:0.430
ROPE_CLUSTER_ONLY: 174
INVOLVES_MTN_SCAFFOLD_PITON: 3
MOUNTAIN_PITON_DISAGREEMENTS:
  MTN_PITON: clawderberg_recursive_slop det=tangled_rope shadow=piton eps=0.85 supp=0.4 theater=0.95 sig=false_ci_rope dist=tangled_rope:0.141 snare:0.211 piton:0.648
  MTN_PITON: mvt_theorem_constraint det=tangled_rope shadow=mountain eps=0.1 supp=0.05 theater=0.1 sig=false_ci_rope dist=mountain:0.670 rope:0.325
  MTN_PITON: quine_self_replication det=mountain shadow=rope eps=0.2 supp=0.05 theater=0.01 sig=false_ci_rope dist=mountain:0.297 rope:0.595 tangled_rope:0.104
MEAN_SHADOW_TOP_P: 0.675829
=== END TASK 3 ===

=== TASK 4: NON-OVERLAPPING POPULATION ===
HARD_TOTAL: 177
MULTI_TYPE_ORBIT: 167
SINGLE_TYPE_ORBIT: 10
OVERLAP_PCT: 94.35
SINGLE_TYPE_DETAILS:
Constraint|DetType|ShadowType|OrbitTypes|Eps|Supp|Theater|NearestBoundary|BoundaryDist
coffee_cardiovascular_2026|tangled_rope|rope|tangled_rope|0.12|0.4|0.1|tangled_rope_supp_floor|0.0
cognitive_bicycle_scaffold|tangled_rope|rope|tangled_rope|0.2|0.3|0.15|rope_chi_ceiling|0.07601386373892077
ergo_lets_protocol|tangled_rope|rope|tangled_rope|0.15|0.1|0.15|mountain_supp_ceiling|0.05
fmt_oncology_2026|tangled_rope|rope|tangled_rope|0.18|0.45|0.12|tangled_rope_supp_floor|0.04999999999999999
horizon_liability_contract|tangled_rope|snare|tangled_rope|0.85|0.95|0.6|snare_suppression_floor|0.35
manganese_catalysis_2026|tangled_rope|rope|tangled_rope|0.18|0.25|0.08|rope_chi_ceiling|0.10341247736502876
mvt_theorem_constraint|tangled_rope|mountain|tangled_rope|0.1|0.05|0.1|mountain_supp_ceiling|0.0
narrative_engineering_2026|tangled_rope|rope|tangled_rope|0.15|0.45|0.05|tangled_rope_supp_floor|0.04999999999999999
rogue_wave_control_2026|tangled_rope|rope|tangled_rope|0.15|0.1|0.05|mountain_supp_ceiling|0.05
sm_addictive_design|tangled_rope|snare|tangled_rope|0.68|0.85|0.4|snare_epsilon_floor|0.22000000000000003
INVERSE_CHECK:
Constraint|DetType|H_norm|OrbitTypes
INVERSE_COUNT: 493
absorbing_markov_chain_trap|snare|0.128297|scaffold/snare/tangled_rope
abstraction_boundary_overrun|snare|0.097059|rope/snare
academic_fashion_modernism_2026|snare|0.030815|rope/snare
access_arbitrage|tangled_rope|0.014293|rope/tangled_rope
ad_fus_coordination|snare|0.269826|scaffold/snare
adaptive_lag_trap|snare|0.079095|rope/snare
adversarial_surface_inflation|snare|0.073530|rope/snare
adversarial_truth_decay|snare|0.045691|rope/snare
adverse_possession|tangled_rope|0.045789|rope/tangled_rope
advice_as_dangerous_gift|tangled_rope|0.015391|rope/tangled_rope
agency_atrophy|snare|0.050571|rope/snare
agent_opt_2026|snare|0.027354|rope/snare
aging_longevity_tests|tangled_rope|0.006743|rope/tangled_rope
ai_banal_capture|snare|0.193095|rope/snare
ai_driven_surveillance_sensor_layer|snare|0.199667|rope/snare
ai_edu_decentralization|tangled_rope|0.001211|rope/tangled_rope
ai_evaluators_matching|snare|0.188824|rope/snare
ai_nonconsensual_content_facilitation|snare|0.206653|rope/snare
ai_professional_displacement|snare|0.170581|rope/snare
ai_scholar_citation_trap|snare|0.072457|scaffold/snare/tangled_rope
ai_superpowers_2026|snare|0.023847|scaffold/snare
ai_training_data_dependency|tangled_rope|0.212853|rope/tangled_rope
airport_slot_use_it_or_lose_it|snare|0.290081|rope/snare
algeria_france_colonial_legacy|snare|0.261284|rope/snare
algorithmic_bias|snare|0.178576|rope/snare
algorithmic_epistemic_capture|snare|0.076037|rope/snare
alignment_tax_tradeoff|snare|0.229277|rope/snare
altruistic_misery_paradox_2026|snare|0.240678|rope/snare
alzheimers_levetiracetam|tangled_rope|0.014196|rope/tangled_rope
alzheimers_nlrp3_inflammasome|snare|0.019507|scaffold/snare
amish_technological_renunciation|snare|0.270511|rope/snare
ancestral_pueblo_hydrology|snare|0.260237|scaffold/snare
anticipatory_capacity_failure|snare|0.076277|rope/snare
appropriations_brinkmanship|snare|0.189842|rope/snare
armra_colostrum_regulation|tangled_rope|0.056303|rope/tangled_rope
arrows_impossibility_theorem|tangled_rope|0.001893|rope/tangled_rope
art_market_decoupling|snare|0.056179|rope/snare
artificial_snow_2026|tangled_rope|0.282966|rope/tangled_rope
asce_7_22_seismic_design|tangled_rope|0.097157|rope/tangled_rope
asymmetric_burden_distribution|snare|0.062069|rope/snare
atrophied_optimization_piton|tangled_rope|0.198778|rope/tangled_rope
attention_as_bottleneck_resource|snare|0.067843|rope/snare
attention_market_cannibalization|snare|0.063372|rope/snare
australia_social_ban_2026|snare|0.198692|rope/snare
authoritarian_power_paradox|snare|0.140632|rope/snare
autonomous_toolchain_sprawl|snare|0.119849|rope/snare
availability_heuristic|tangled_rope|0.161545|rope/tangled_rope
average_is_over_2026|snare|0.262970|rope/snare
axiom_reasoner_2026|tangled_rope|0.001943|scaffold/tangled_rope
bay_of_pigs_operational_silo|snare|0.075016|rope/snare
bayes_theorem_cognitive_bias|tangled_rope|0.014399|rope/tangled_rope
beehiiv_platform_model|tangled_rope|0.046600|rope/tangled_rope
belief_argument_conclusion|snare|0.095619|rope/snare
bgs_eigenvector_thermalization|tangled_rope|0.003192|rope/tangled_rope
big_data_astrophysics_arbitrage|tangled_rope|0.058135|rope/tangled_rope
biological_curiosity|mountain|0.051013|mountain/scaffold
bip_narrative_illusion|snare|0.260142|rope/snare
blackstone_carried_interest_taxation|tangled_rope|0.073422|rope/tangled_rope
blackstone_conflicts_of_interest|snare|0.290081|rope/snare
bnpl_payment_systems|tangled_rope|0.204842|rope/tangled_rope
boom_bust_path_dependency|rope|0.054631|piton/rope
bor_tax_exemption_nl|snare|0.206653|rope/snare
boundary_dissolution_risk|snare|0.060455|rope/snare
brazil_2026_general_elections|tangled_rope|0.022643|rope/tangled_rope
brazil_mexico_financial_requirement|snare|0.206653|rope/snare
buffons_needle_pi_estimation|tangled_rope|0.004353|rope/tangled_rope
burali_forti_paradox|mountain|0.086509|mountain/scaffold
burden_of_proof_engineering_safety|snare|0.238262|rope/snare
burden_of_proof_scientific|tangled_rope|0.204842|rope/tangled_rope
bureaucratic_legibility_collapse|snare|0.057721|rope/snare
bureaucratic_self_preservation|snare|0.082297|rope/snare
bushman_money_magic|snare|0.238262|rope/snare
capability_eval_overhang|snare|0.161307|rope/snare
capital_misallocation_spiral|snare|0.065076|rope/snare
capital_rotation_ai_narrative|tangled_rope|0.204842|rope/tangled_rope
carbon_credit_markets_2026|snare|0.187765|rope/snare/tangled_rope
cartel_drone_surveillance_el_paso|snare|0.276201|rope/snare
cascading_constraint_failure|snare|0.213198|rope/snare
cascading_uncertainty_2026|snare|0.103161|rope/snare
champions_bass_fishing_exclusion|tangled_rope|0.118166|rope/tangled_rope
child_marriage|snare|0.275329|rope/snare
china_africa_zero_tariff_2026|tangled_rope|0.023887|rope/tangled_rope
choice_architecture_design|tangled_rope|0.046600|rope/tangled_rope
citation_collapse_dynamics|snare|0.055416|rope/snare
civilizational_lifecycle_solara|snare|0.099213|rope/snare
civilizational_maintenance_debt|tangled_rope|0.002477|rope/tangled_rope
click_chemistry_paradigm_2026|mountain|0.031866|mountain/scaffold
climate_attribution_2026|mountain|0.129431|mountain/scaffold
cma|tangled_rope|0.238239|rope/tangled_rope
cmr_001|tangled_rope|0.001501|rope/tangled_rope
coalition_disinfo_framework_2026|tangled_rope|0.212853|rope/tangled_rope
cobra_effect|tangled_rope|0.001741|rope/tangled_rope
cognac_geopolitical_risk|snare|0.138843|rope/snare/tangled_rope
coinbase_crypto_volatility|tangled_rope|0.244603|rope/tangled_rope
cold_dark_matter_paradigm|tangled_rope|0.235528|rope/tangled_rope
collective_action_deadlock|snare|0.209045|rope/snare
collective_stupidity_2026|snare|0.024786|rope/snare
college_admissions_market|tangled_rope|0.068848|rope/tangled_rope
colombia_2026_presidential_election|tangled_rope|0.002577|rope/tangled_rope
comitatus_bond|tangled_rope|0.217651|rope/tangled_rope
communal_narcissism_social_trap|snare|0.148151|rope/snare
complexity_debt|snare|0.170722|rope/snare
compounding_logic|tangled_rope|0.001660|rope/tangled_rope
consensus_without_truth|snare|0.092437|rope/snare
constitutional_supremacy|snare|0.282070|rope/snare/tangled_rope
constraint_interaction_explosion|snare|0.207533|rope/snare
constraint_lagrange_multipliers|tangled_rope|0.014196|rope/tangled_rope
constraint_riemann_mapping|tangled_rope|0.014312|rope/tangled_rope
constraint_yoneda|tangled_rope|0.006400|rope/tangled_rope
container_capacity_mismatch|snare|0.076037|rope/snare
conversational_dogmas_interruption|snare|0.195515|rope/snare/tangled_rope
coordination_attack_vulnerability|snare|0.056514|rope/snare
coordination_fatigue|snare|0.253974|rope/snare
cost_of_observation|tangled_rope|0.004672|rope/tangled_rope
couples_residency_match|snare|0.240181|rope/snare/tangled_rope
cow_field_poop|tangled_rope|0.006400|rope/tangled_rope
creative_commons_licensing|tangled_rope|0.006090|rope/tangled_rope
credibility_inflation|snare|0.080014|rope/snare
crisis_signal_saturation|snare|0.048844|rope/snare
crispr_genomic_rewrite_2026|tangled_rope|0.006219|rope/tangled_rope
critical_actor_overcentralization|snare|0.048448|rope/snare
cross_domain_coupling_spiral|snare|0.052992|rope/snare
cs_ecmo_bridge|tangled_rope|0.000604|rope/tangled_rope
cuba_mandatrophic_collapse|snare|0.258122|rope/snare/tangled_rope
cultural_memory_decay|snare|0.185976|rope/snare
cultural_refragmentation_2026|snare|0.020188|rope/snare
cumbria_mine_rejection|snare|0.277256|rope/snare
cz_plea_agreement_2026|snare|0.219905|rope/snare
data_laundering_pipeline|snare|0.066801|rope/snare
data_replication_paradox|tangled_rope|0.295380|rope/tangled_rope
decision_latency_mismatch|tangled_rope|0.203496|rope/tangled_rope
delta_force_selection_2026|snare|0.078025|rope/snare
dutch_minority_govt_2026|tangled_rope|0.235528|rope/tangled_rope
edelman_2026_developed_stagnation|snare|0.263137|rope/snare/tangled_rope
edelman_2026_developing_volatility|tangled_rope|0.006743|rope/tangled_rope
education_unbundling_implementation|tangled_rope|0.008748|rope/tangled_rope
elite_capture_2026|snare|0.022720|rope/snare
elite_identity_capture_2026|snare|0.051191|rope/snare
em_clinical_guidelines|tangled_rope|0.044332|rope/tangled_rope
emergency_bridge_scaffold|snare|0.221524|scaffold/snare/tangled_rope
emergency_mode_lock_in|snare|0.050575|rope/snare
emergency_oversight_bureau|tangled_rope|0.092445|rope/scaffold/tangled_rope
emergency_powers_ratchet|snare|0.096073|scaffold/snare
emergent_goal_misalignment|snare|0.077972|rope/snare
empty_tomb_transformation|tangled_rope|0.006356|rope/tangled_rope
endocrine_disruption_society|snare|0.179246|rope/snare
endowment_effect|mountain|0.098103|mountain/scaffold
epistemic_free_rider_problem|snare|0.074220|rope/snare
epistemic_overload_collapse|snare|0.050769|rope/snare
epstein_espionage_2026|snare|0.026992|rope/snare
epstein_files_2026|snare|0.030549|rope/snare
epstein_honeytrap|snare|0.084706|rope/snare
erasmus_rejoining_scaffold|tangled_rope|0.136124|rope/scaffold/tangled_rope
ergo_nipopows|mountain|0.062983|mountain/scaffold
ergo_storage_rent|tangled_rope|0.000835|rope/tangled_rope
erised_expectation|snare|0.154275|rope/snare
eu_affordable_housing_initiative|tangled_rope|0.223049|rope/tangled_rope
eu_asylum_outsourcing_framework|snare|0.261284|rope/snare
eu_mercosur_trade_agreement|tangled_rope|0.212853|indexically_opaque/rope/tangled_rope
evfta_trade_agreement|tangled_rope|0.212853|rope/tangled_rope
evidence_half_life|snare|0.085692|rope/snare
evolutionary_mismatch_load|snare|0.261756|rope/snare
expert_disempowerment|snare|0.183447|rope/snare
exploration_vs_exploitation|snare|0.294685|rope/snare/tangled_rope
extraordinary_narrative_shift|tangled_rope|0.002832|rope/tangled_rope
faa_boeing_regulatory_capture|tangled_rope|0.033459|rope/tangled_rope
factional_instability|tangled_rope|0.066217|rope/tangled_rope
fiat_currency_lifecycle|snare|0.106051|rope/snare
fiber_optic_chip_tech|snare|0.294685|rope/snare/tangled_rope
financial_drag|snare|0.089801|rope/snare
fiscal_dominance_trap|snare|0.079960|rope/snare
fmeca_procedures_1980|tangled_rope|0.009732|rope/tangled_rope
fnl_shadow_probe|tangled_rope|0.039624|rope/tangled_rope
fragile_middle_layer_collapse|snare|0.077210|rope/snare
framing_effect|tangled_rope|0.026976|rope/tangled_rope
france_2027_presidential_election|tangled_rope|0.000617|rope/tangled_rope
france_local_elections_march_2026|tangled_rope|0.008315|scaffold/tangled_rope
franchisee_corporate_squeeze|tangled_rope|0.174077|rope/tangled_rope
frontex_pushback_coordination|snare|0.179753|rope/snare
gale_shapley|snare|0.254286|rope/snare
galois_theory_symmetry|mountain|0.090904|mountain/scaffold
gaza_aid_permit_revocation|snare|0.295579|rope/snare/tangled_rope
gemini_scientific_advancement|tangled_rope|0.014196|rope/tangled_rope
generational_replacement_inertia|snare|0.280417|rope/snare
genetic_algorithms_evolution|mountain|0.077350|mountain/scaffold
genetic_predisposition|tangled_rope|0.037682|rope/tangled_rope
genie_ip_constraint|snare|0.055220|scaffold/snare/tangled_rope
germany_tennet_takeover|tangled_rope|0.019489|rope/tangled_rope
ghost_fishing_gear|snare|0.269826|rope/snare
global_economic_anxiety_2026|snare|0.017724|rope/snare
global_stimulus_spree|tangled_rope|0.013548|scaffold/tangled_rope
goal_boundary_poisoning|snare|0.054792|rope/snare
gold_piton_2026|rope|0.034657|piton/rope
goodstein_theorem_finite_proof|tangled_rope|0.008770|rope/tangled_rope
governance_latency_gap|snare|0.173500|rope/snare
governance_overfitting|snare|0.290832|rope/snare
gpt5_codex_dev_cycle|tangled_rope|0.006930|rope/tangled_rope
graph_coloring_complexity|tangled_rope|0.009054|rope/tangled_rope
great_awakening_rekindling|snare|0.132693|rope/snare/tangled_rope
great_mongolian_road_economic_dependency|snare|0.291692|rope/snare/tangled_rope
greenland_defence_pact_2026|snare|0.261681|rope/snare/tangled_rope
greenland_seizure_trade_war|snare|0.260142|rope/snare
grete_samsa_transition|tangled_rope|0.079933|rope/tangled_rope
grievance_stack_overflow|snare|0.067519|rope/snare
gs1_gln_identification|tangled_rope|0.073887|rope/tangled_rope
gs1_standardized_identification|tangled_rope|0.010455|rope/tangled_rope
gs_market_clearing|tangled_rope|0.013587|rope/tangled_rope
guinea_worm_eradication|rope|0.203466|rope/tangled_rope
guthrie_kidnapping_2026|snare|0.097669|rope/snare
hanlons_razor|tangled_rope|0.001464|rope/tangled_rope
hasbro_licensing_restriction|tangled_rope|0.049003|rope/tangled_rope
hegemonic_entropy_2026|tangled_rope|0.143099|rope/tangled_rope
hershey_salt_strategy|tangled_rope|0.235528|rope/tangled_rope
heuristic_optimization|mountain|0.173415|mountain/scaffold
hhs_fetal_tissue_research_ban_2019|snare|0.206653|rope/snare
hidden_interdependency_risk|snare|0.130584|rope/snare
hilberts_hotel_infinity|mountain|0.048377|mountain/scaffold
hoa_covenants|tangled_rope|0.005479|rope/tangled_rope
hollow_state_syndrome|snare|0.054092|rope/snare
hp_liberalism|tangled_rope|0.056871|rope/tangled_rope
huang_expectation_resilience_2026|snare|0.178911|rope/snare
hydra_game|tangled_rope|0.063690|rope/tangled_rope
hypercompression_of_time_horizons|snare|0.063059|rope/snare
hypernormie_equilibrium|snare|0.075524|rope/snare
ice_raids_minnesota_2026|snare|0.231878|rope/snare
ice_safe_departure|snare|0.251814|rope/snare
identity_stack_incompatibility|snare|0.080278|rope/snare
incentive_surface_warping|snare|0.055416|rope/snare
india_semi_mission|tangled_rope|0.056600|rope/tangled_rope
individual_revolution_autonomy|snare|0.188824|rope/snare
indo_german_defense_pact|tangled_rope|0.235528|indexically_opaque/rope/tangled_rope
indonesia_penal_code_2023|snare|0.261284|indexically_opaque/rope/snare
inference_cost_scaling_law|snare|0.087036|rope/snare
information_foraging_theory|mountain|0.129431|mountain/scaffold
infrastructure_interoperability_decay|snare|0.053836|rope/snare
institutional_inertia_lock|tangled_rope|0.083710|rope/tangled_rope
institutional_memory_loss|snare|0.064347|rope/snare
institutional_mutation_domestication|snare|0.236896|rope/snare
institutional_mutation_without_selection|snare|0.048684|rope/snare
insult_wisdom_training|tangled_rope|0.077929|rope/tangled_rope
interface_contract_breakdown|snare|0.080667|rope/snare
internet_evolution_lifecycle|snare|0.198276|rope/snare
interpretive_frame_fragmentation|snare|0.050231|rope/snare
intertemporal_responsibility_gap|snare|0.066052|rope/snare
invisible_infrastructure_dependency|snare|0.054061|rope/snare
iran_guardian_council_vetting|snare|0.266352|rope/snare
iran_hijab_law|snare|0.261284|rope/snare
iran_mandatrophic_collapse|snare|0.237395|rope/snare
iran_war_room_2026|snare|0.098707|rope/snare
iron_law_of_oligarchy|snare|0.244042|rope/snare/tangled_rope
irreversible_policy_commitment|snare|0.254433|rope/snare
isa_education_scaffold|tangled_rope|0.004695|scaffold/tangled_rope
israel_egypt_gas_deal|tangled_rope|0.204842|rope/tangled_rope
israel_electoral_threshold|tangled_rope|0.204842|rope/tangled_rope
israel_override_clause|snare|0.236899|rope/snare
israel_surplus_vote_agreements|tangled_rope|0.002743|rope/tangled_rope
israeli_settlement_policy_authority_restriction|snare|0.291031|rope/snare/tangled_rope
iterated_function_system_convergence|tangled_rope|0.005232|rope/tangled_rope
ivt_accessibility_barrier|tangled_rope|0.001501|rope/tangled_rope
japanese_energy_scaffold_2025|snare|0.187765|rope/snare/tangled_rope
jevons_paradox|tangled_rope|0.037682|rope/tangled_rope
jupiter_composition_knowledge_gap|tangled_rope|0.014196|rope/tangled_rope
keltner_relationship_evaluation|tangled_rope|0.013828|rope/tangled_rope
kjv_linguistic_residue|rope|0.020636|piton/rope
kjv_textual_authority|tangled_rope|0.158309|rope/tangled_rope
labor_union_dues|tangled_rope|0.267413|rope/tangled_rope
landscape_of_fear_2026|tangled_rope|0.156481|rope/tangled_rope
latent_goal_activation|snare|0.053320|rope/snare
latent_regulatory_bomb|snare|0.288519|rope/snare
law_of_diminishing_returns|tangled_rope|0.006336|rope/tangled_rope
lcdm_small_scale_anomalies|tangled_rope|0.125350|rope/tangled_rope
legal_formalism_overhang|snare|0.059085|rope/snare
legibility_trap|snare|0.050571|rope/snare
legitimacy_without_capacity|snare|0.072502|rope/snare
legitimacy_without_effectiveness|snare|0.064033|rope/snare
lehman_repo_105|snare|0.252269|rope/snare
lindy_effect|rope|0.265256|rope/tangled_rope
linguistic_relativity_cultural_framing|tangled_rope|0.033567|rope/tangled_rope
liquidity_illusion|snare|0.086845|rope/snare
litchfield_sensitive_locations_2026|snare|0.138110|rope/snare
lorenz_attractor_dynamics|tangled_rope|0.008919|rope/tangled_rope
lsd_microdosing_professional_openness|tangled_rope|0.009026|rope/tangled_rope
magna_carta_liberties|tangled_rope|0.226775|rope/tangled_rope
maha_recovery_2026|tangled_rope|0.019094|scaffold/tangled_rope
maintenance_capacity_shortfall|snare|0.066537|rope/snare
maladaptive_selection_process|snare|0.063024|rope/snare
mandatrophic_margin_collapse|snare|0.240699|rope/snare
manga_distribution_duopoly|tangled_rope|0.295380|rope/tangled_rope
marriage_market_asymmetry_2026|snare|0.255098|rope/snare
mars_rovers_navigational_autonomy|tangled_rope|0.009737|rope/tangled_rope
mass_market_extinction_2026|snare|0.219689|rope/snare
matching_market_congestion_externality|tangled_rope|0.012799|rope/tangled_rope
max_flow_min_cut|snare|0.125226|rope/snare/tangled_rope
med_diet_consensus_2026|tangled_rope|0.207792|rope/tangled_rope
memetic_fitness_vs_truth|snare|0.064737|rope/snare
meta_governance_overload|snare|0.051420|rope/snare
meta_model_lock_in|snare|0.059399|rope/snare
meta_nuclear_power_agreement|tangled_rope|0.026630|rope/tangled_rope
meta_pay_or_okay_model|snare|0.283797|indexically_opaque/rope/snare
migration_decision_threshold|tangled_rope|0.051406|rope/tangled_rope
mil_std_461g_emi_control|tangled_rope|0.001749|rope/tangled_rope
mil_std_810f_tailoring|tangled_rope|0.005037|rope/tangled_rope
milano_cortina_2026|rope|0.007705|piton/rope
minimax_theorem_game_equilibrium|tangled_rope|0.154787|rope/tangled_rope
minnesota_sovereignty_2026|snare|0.183369|rope/snare
mit_tfus_2026|snare|0.068859|scaffold/snare/tangled_rope
model_autonomy_creep|snare|0.086075|rope/snare
model_collapse_feedback_loop|snare|0.065868|rope/snare
model_of_models_regression|snare|0.087608|rope/snare
moltbook_agent_theater|tangled_rope|0.294374|rope/tangled_rope
moltbook_breach_2026|snare|0.043746|rope/snare
monetary_regime_transition|tangled_rope|0.001464|rope/tangled_rope
moores_law|tangled_rope|0.039671|rope/tangled_rope
moral_outsourcing|snare|0.063507|rope/snare
multi_agent_reward_hacking|snare|0.048173|rope/snare
naming_as_control|snare|0.144397|rope/snare
narcissistic_ego_maintenance|snare|0.290985|rope/snare
narrative_capacity_exhaustion|snare|0.054443|rope/snare
narrative_overfitting|snare|0.047821|rope/snare
nato_arctic_defense_cooperation|tangled_rope|0.008857|rope/tangled_rope
negative_emissions_arbitrage|tangled_rope|0.244594|rope/tangled_rope
net_zero_stabilization|tangled_rope|0.025759|rope/tangled_rope
neural_interoperability|tangled_rope|0.101618|rope/tangled_rope
neurodiversity_spectrum|tangled_rope|0.208332|rope/tangled_rope
news_paywall_inequality|tangled_rope|0.049003|rope/tangled_rope
nfl_superbowl_halftime_exclusivity|tangled_rope|0.056303|rope/tangled_rope
nine_day_buffer|snare|0.288141|rope/snare/tangled_rope
noether_isomorphism_access|tangled_rope|0.002402|rope/tangled_rope
norm_erosion_threshold|snare|0.102137|rope/snare
north_sea_wind_grid|tangled_rope|0.290694|rope/tangled_rope
nsw_transmission_bottleneck|snare|0.049711|scaffold/snare/tangled_rope
nuclear_order_2026|snare|0.273120|rope/snare/tangled_rope
ny_private_school_discount|tangled_rope|0.049003|rope/tangled_rope
nyc_metrocard_art_licensing|tangled_rope|0.004427|rope/tangled_rope
olympic_legacy_curling_investment|tangled_rope|0.009008|rope/tangled_rope
olympic_medal_allocation|tangled_rope|0.012799|rope/tangled_rope
omelet_perfection_complexity|rope|0.136808|rope/tangled_rope
openai_api_access|snare|0.079309|scaffold/snare/tangled_rope
openai_codex_app_constraint|snare|0.072457|scaffold/snare/tangled_rope
openai_implicit_translator|snare|0.255294|rope/snare/tangled_rope
openbsd_netiquette_protocol|snare|0.233491|rope/snare/tangled_rope
openscholar_peer_review|snare|0.062703|scaffold/snare/tangled_rope
optimization_fragility|snare|0.197688|rope/snare
oral_glp1_market_access|snare|0.294685|rope/snare/tangled_rope
orbital_data_center_2026|snare|0.180267|rope/snare
oscar_campaign_spending|tangled_rope|0.008857|rope/tangled_rope
other_peoples_troubles_2026|snare|0.075381|rope/snare
overfitting_to_frameworks|snare|0.167570|rope/snare
paris_municipal_reform_2026|tangled_rope|0.001453|rope/tangled_rope
parkinsons_law|tangled_rope|0.001749|rope/tangled_rope
participatory_observer_hypothesis|tangled_rope|0.011866|rope/tangled_rope
paxsilica_framework|tangled_rope|0.212853|rope/tangled_rope
pe_fund_level_leverage|snare|0.270422|rope/snare
perovskite_self_etching|snare|0.261681|scaffold/snare/tangled_rope
peter_principle|snare|0.195515|rope/snare/tangled_rope
planetary_boundaries|snare|0.254286|rope/snare
plastic_asphalt_mandate|tangled_rope|0.056303|rope/tangled_rope
platonic_coparenting_decoupling|tangled_rope|0.001876|scaffold/tangled_rope
pna|tangled_rope|0.002426|rope/tangled_rope
policy_lag_catastrophe|snare|0.069407|rope/snare
politeness_face_negotiation|tangled_rope|0.056600|rope/tangled_rope
portugal_ad_stability_2026|tangled_rope|0.008315|scaffold/tangled_rope
portugal_polarization_threshold_2026|snare|0.249208|rope/snare/tangled_rope
postman_survival_protocol|rope|0.128974|rope/tangled_rope
power_set_axiomatic_extraction|tangled_rope|0.110182|rope/tangled_rope
power_without_responsibility|snare|0.050837|rope/snare
prestige_signal_inflation|snare|0.096635|rope/snare
price_signal_corruption|snare|0.081646|rope/snare
private_identity_integration|tangled_rope|0.009705|rope/tangled_rope
procedural_compliance_theater|snare|0.154497|rope/snare/tangled_rope
procedural_legitimacy_decay|snare|0.214254|rope/snare
project_vault_extraction_2026|snare|0.110045|rope/snare
protocol_drift_accumulation|snare|0.090063|rope/snare
publishing_embargo|tangled_rope|0.012799|rope/tangled_rope
quantum_entanglement_protocol|rope|0.259407|rope/tangled_rope
quellcrist_falconer_justice|snare|0.238262|rope/snare
qwerty_vs_dvorak|tangled_rope|0.158309|rope/tangled_rope
rare_earth_hydrogen_extraction|tangled_rope|0.049003|rope/tangled_rope
rare_earth_seabed_mining|tangled_rope|0.014030|rope/tangled_rope
rational_inertia_trap|snare|0.160403|rope/snare
recipe_scaling_ai|tangled_rope|0.002242|scaffold/tangled_rope
regulatory_capture|tangled_rope|0.004233|rope/tangled_rope
rent_seeking_equilibrium|snare|0.063507|rope/snare
reputational_cascade_failure|snare|0.061848|rope/snare
responsibility_dilution|snare|0.055184|rope/snare
responsibility_without_power|snare|0.049083|rope/snare
rfc9293_state_machine|rope|0.242327|rope/tangled_rope
riot_incentive_loop_2026|snare|0.063804|rope/snare
risk_socialization_threshold|snare|0.075383|rope/snare
ritual_transition_scaffold|tangled_rope|0.117353|rope/tangled_rope
ritual_without_belief|snare|0.047719|rope/snare
robustness_vs_efficiency_tradeoff|snare|0.200265|rope/snare
rosen_bridge_protocol|tangled_rope|0.002213|rope/tangled_rope
rotation_seven_black_soil|tangled_rope|0.000000|rope/tangled_rope
royal_navy_middle_east_withdrawal|tangled_rope|0.099234|rope/tangled_rope
russells_paradox_self_reference|tangled_rope|0.080818|scaffold/tangled_rope
russian_war_cannibalization|snare|0.266420|rope/snare
sa_renewable_price_differential|tangled_rope|0.026630|rope/tangled_rope
sadhu_integrity_protocol|tangled_rope|0.049420|rope/tangled_rope
sat_csp_complexity|tangled_rope|0.009054|rope/tangled_rope
satellite_d2m_standard|tangled_rope|0.007636|rope/tangled_rope
scam_compound_2026|snare|0.131406|rope/snare
second_order_unintended_consequences|snare|0.066537|rope/snare
semantic_attack_surface|snare|0.051169|rope/snare
semiconductor_mission_2026|tangled_rope|0.056303|rope/tangled_rope
shadow_pricing_failure|snare|0.088301|rope/snare
ship_of_theseus|tangled_rope|0.248120|rope/tangled_rope
shitty_feedback_handling|tangled_rope|0.008350|rope/tangled_rope
shobies_existential_commitment|tangled_rope|0.051570|rope/tangled_rope
shock_propagation_asymmetry|snare|0.058298|rope/snare
signal_without_control|snare|0.272252|rope/snare
silent_dependency_activation|snare|0.266417|rope/snare
skills_based_hiring|rope|0.274959|rope/tangled_rope
skolems_paradox|mountain|0.173415|mountain/scaffold
smartphone_ubiquity|tangled_rope|0.079933|rope/tangled_rope
social_loafing|tangled_rope|0.006344|rope/tangled_rope
social_media_participation_threshold|tangled_rope|0.039252|rope/tangled_rope
social_narrative_casting|tangled_rope|0.069794|rope/tangled_rope
soft_authoritarian_drift|snare|0.073530|rope/snare
sorites_paradox|tangled_rope|0.156481|rope/tangled_rope
south_china_sea_arbitration_2016_2026|snare|0.289959|rope/snare/tangled_rope
sovereignty_as_arbitrage|tangled_rope|0.004793|rope/tangled_rope
spain_digital_offensive_2026|snare|0.223549|rope/snare
st_petersburg_paradox|tangled_rope|0.002322|rope/tangled_rope
stable_marriage_coordination|tangled_rope|0.005973|rope/tangled_rope
status_flattening_effect|snare|0.098335|rope/snare
strange_attractor_dynamics|snare|0.123902|rope/snare/tangled_rope
structural_extraction_without_actor|snare|0.159282|rope/snare
sunk_cost_fallacy|snare|0.128196|rope/snare/tangled_rope
synthetic_data_feedback_loop|snare|0.084437|rope/snare
systemic_blindspot|snare|0.096322|rope/snare
tail_risk_compression|snare|0.118425|rope/snare
taiwan_storm_2026|snare|0.167319|rope/snare
taxonomy_drift|snare|0.228999|rope/snare
tcp_rfc9293_interoperability|rope|0.225330|rope/tangled_rope
teaching_horses_to_sing|tangled_rope|0.003781|rope/tangled_rope
tear_gas_repression_2026|snare|0.068899|rope/snare
temporal_scale_arbitrage|tangled_rope|0.003918|rope/tangled_rope
temporal_scarcity|rope|0.063412|piton/rope
thai_senate_veto_2026|snare|0.082297|rope/snare
the_bacchae_madness_protocol|snare|0.258122|rope/snare
the_calm_protocol_suppression|snare|0.276201|rope/snare
theatrical_neutrality|snare|0.036429|rope/snare
theory_of_visitors|tangled_rope|0.051570|rope/tangled_rope
toxoplasma_hub_2026|snare|0.009353|scaffold/snare
trade_secret_law|tangled_rope|0.235528|rope/tangled_rope
tragedy_of_the_commons|tangled_rope|0.003045|rope/tangled_rope
transformer_self_attention|tangled_rope|0.003669|rope/tangled_rope
transient_event_detection|tangled_rope|0.011628|rope/tangled_rope
trillion_bond_rush_2026|tangled_rope|0.061660|scaffold/tangled_rope
trivial_topology_info_asymmetry|tangled_rope|0.049003|rope/tangled_rope
trump_critical_minerals|tangled_rope|0.160374|rope/tangled_rope
trump_making_china_great_2026|tangled_rope|0.002426|rope/tangled_rope
tsp_computational_complexity|tangled_rope|0.099816|rope/tangled_rope
uk_graduate_visa_salary_threshold|snare|0.206653|rope/snare
uk_necc_formation|tangled_rope|0.198904|rope/tangled_rope
uk_unpaid_care_system|snare|0.287246|rope/snare
ukraine_tight_gas_pilot|tangled_rope|0.204842|rope/tangled_rope
ulysses_aeolus_1904|snare|0.249664|rope/snare/tangled_rope
ulysses_cyclops_1904|snare|0.191698|rope/snare/tangled_rope
ulysses_lestrygonians_1904|snare|0.288590|rope/snare/tangled_rope
ulysses_nausicaa_1904|snare|0.261136|rope/snare/tangled_rope
ulysses_scylla_1904|snare|0.287183|rope/snare/tangled_rope
ulysses_sirens_1904|snare|0.186384|rope/snare/tangled_rope
un_high_seas_treaty_2026|tangled_rope|0.022550|rope/tangled_rope
unclos_2026|tangled_rope|0.101098|rope/tangled_rope
union_protection_underperformance|tangled_rope|0.039252|rope/tangled_rope
unrequited_love_protocol|snare|0.016097|scaffold/snare
unrwa_eviction_order|snare|0.220110|rope/snare
us_greenland_envoy|snare|0.295454|rope/snare
us_israel_faa_502b_nonenforcement|snare|0.131573|rope/snare
us_labor_mobility|tangled_rope|0.026909|rope/tangled_rope
us_sdf_alliance_abandonment_2026|snare|0.295454|rope/snare
us_two_party_duopoly|snare|0.269826|rope/snare
us_vaccine_recommendation_dismantling_2026|snare|0.206653|rope/snare
us_venezuela_oil_pressure|snare|0.265966|rope/snare
us_venezuela_plausible_deniability_2025|snare|0.292681|rope/snare
value_alignment_drift|snare|0.296936|rope/snare
value_extraction_plateau|snare|0.045691|rope/snare
venezuela_oil_privatization_v1|snare|0.206653|rope/snare
viral_emergence_covid19_exemplar|rope|0.172624|piton/rope
viral_transmission_rates|snare|0.236896|rope/snare
visa_ipo_regulatory_compliance|tangled_rope|0.240203|rope/tangled_rope
visa_judgment_sharing_agreement|tangled_rope|0.160399|rope/tangled_rope
wikipedia_notability_requirement_2026|tangled_rope|0.153586|rope/tangled_rope
working_dog_training|tangled_rope|0.014523|rope/tangled_rope
world_factbook_sunset_2026|snare|0.120548|rope/snare
wpl_scotland|snare|0.261681|rope/snare/tangled_rope
xi_mao_ideological_centralization|snare|0.087048|rope/snare
yc_equity_squeeze|tangled_rope|0.049003|rope/tangled_rope
yt_ai_slop_incentive|snare|0.261284|rope/snare
zipfs_law|snare|0.164773|rope/snare
zombie_reasoning_2026|snare|0.028703|rope/snare
=== END TASK 4 ===

=== TASK 5: GAUSSIAN PROFILES ===
EMPIRICAL_PROFILES:
Type|Metric|Mu|Sigma
mountain|extractiveness|0.083281|0.058964
mountain|suppression|0.029453|0.018929
mountain|theater|0.028906|0.054804
rope|extractiveness|0.123333|0.071309
rope|suppression|0.379020|0.283475
rope|theater|0.204706|0.264036
tangled_rope|extractiveness|0.499102|0.188031
tangled_rope|suppression|0.542545|0.202793
tangled_rope|theater|0.248922|0.238076
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
  OVERRIDE_ROPE: automatic_enrollment_defaults sig=constructed_low_extraction eps=0.050000 hn=0.063000
  OVERRIDE_ROPE: berkshire_compounding_culture sig=constructed_low_extraction eps=0.100000 hn=0.088360
  OVERRIDE_ROPE: boundary_protocol sig=coupling_invariant_rope eps=0.000000 hn=0.155706
  OVERRIDE_ROPE: cancer_chronotherapy_timing sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: cinderella_midnight_deadline sig=coupling_invariant_rope eps=0.100000 hn=0.155706
  OVERRIDE_ROPE: copyleft_viral_licensing sig=constructed_low_extraction eps=0.050000 hn=0.103315
  OVERRIDE_ROPE: cuny_light_2026 sig=constructed_low_extraction eps=0.050000 hn=0.037303
  OVERRIDE_ROPE: decentralized_infrastructure_rope sig=constructed_low_extraction eps=0.080000 hn=0.112035
  OVERRIDE_ROPE: fair_use_doctrine sig=constructed_low_extraction eps=0.100000 hn=0.198889
  OVERRIDE_ROPE: ice_memory_archive sig=coupling_invariant_rope eps=0.100000 hn=0.155706
  OVERRIDE_ROPE: microrobot_manipulation sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: mom_z14_2026 sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: open_source_commons sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: perseverance_rover_autonomy sig=coupling_invariant_rope eps=0.100000 hn=0.155706
  OVERRIDE_ROPE: planetary_diet_constraint_2026 sig=constructed_low_extraction eps=0.100000 hn=0.256814
  OVERRIDE_ROPE: portuguese_presidential_term_limits sig=constructed_low_extraction eps=0.020000 hn=0.184947
  OVERRIDE_ROPE: rafah_crossing_lifeline sig=coupling_invariant_rope eps=0.150000 hn=0.155706
  OVERRIDE_ROPE: rare_earth_coop_2026 sig=constructed_low_extraction eps=0.020000 hn=0.095978
  OVERRIDE_ROPE: spv_variations_us_cold sig=coupling_invariant_rope eps=0.020000 hn=0.155706
  OVERRIDE_ROPE: sts86_ascent_checklist sig=constructed_low_extraction eps=0.050000 hn=0.177846
  OVERRIDE_ROPE: swift_piton_snap sig=coupling_invariant_rope eps=0.040000 hn=0.155706
  OVERRIDE_ROPE: thai_article_112_mountain sig=constructed_low_extraction eps=0.040000 hn=0.191188
  OVERRIDE_ROPE: vertebrate_turning_point_2026 sig=constructed_low_extraction eps=0.050000 hn=0.168398
  OVERRIDE_ROPE: viral_emergence_covid19_exemplar sig=constructed_low_extraction eps=0.150000 hn=0.172624
  OVERRIDE_ROPE: wikipedia_crowdsourcing_2026 sig=constructed_low_extraction eps=0.050000 hn=0.172890
  OVERRIDE_ROPE: wikipedia_noncommercial_model sig=coupling_invariant_rope eps=0.120000 hn=0.155706
NON_OVERRIDE_ROPE_ENTROPY_MEAN: 0.257670 (n=24)
OVERRIDE_ROPE_ENTROPY_MEAN: 0.149917 (n=27)
ALL_TYPE_EPS_STATS:
  mountain: n=128 mean=0.083281 std=0.058964 min=0.000000 max=0.200000
  rope: n=51 mean=0.123333 std=0.071309 min=0.000000 max=0.250000
  tangled_rope: n=334 mean=0.499102 std=0.188031 min=0.000000 max=1.000000
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
MULTI_ORBIT_LOW_ENTROPY: 773/1017 (76.01%)
=== END TASK 6 ===

=== END DIAGNOSTIC OUTPUT ===
