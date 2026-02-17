
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================
=== MAXENT DIAGNOSTIC OUTPUT ===
SUMMARY: NTotal=1032 MeanEntropy=0.190678 NHighUncertainty=16 NHard=163 NSoft=0

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
  tangled_rope: 282
  unknown: 52
RESIDUAL_TYPE_COUNT: 52
=== END TASK 1 ===

=== TASK 2: PER-TYPE ENTROPY BREAKDOWN ===
TYPE_ENTROPY_TABLE:
Type|Count|Mean|Median|Min|Max|StdDev
mountain|128|0.154421|0.155706|0.031850|0.490424|0.042073
rope|51|0.197029|0.155706|0.003416|0.462174|0.120947
snare|519|0.236030|0.264770|0.007608|0.423203|0.130400
tangled_rope|282|0.132297|0.155706|0.001458|0.450723|0.104809
unknown|52|0.137648|0.086383|0.000000|0.512718|0.153176
=== END TASK 2 ===

=== TASK 3: HARD DISAGREEMENTS ===
TOTAL_HARD: 163
DISAGREEMENT_PAIRS:
DetType->ShadowType|Count
mountain->rope|1
rope->tangled_rope|13
snare->tangled_rope|128
tangled_rope->mountain|1
tangled_rope->rope|7
tangled_rope->snare|13
HARD_DISAGREEMENT_DETAILS:
Constraint|DetType|ShadowType|ShadowTopP|ShadowConf|Distribution
ai_adoption_stigma|snare|tangled_rope|0.616151|0.628231|tangled_rope:0.616 snare:0.384
ai_performance_watermark|snare|tangled_rope|0.747971|0.684823|tangled_rope:0.748 snare:0.252
arctic_maritime_control|snare|tangled_rope|0.609697|0.626624|tangled_rope:0.610 snare:0.390
arg_ev_tariff|snare|tangled_rope|0.515947|0.613385|tangled_rope:0.516 snare:0.484
astm_d638_tensile_testing|rope|tangled_rope|0.582422|0.599385|rope:0.410 tangled_rope:0.582
atrophied_optimization_piton|tangled_rope|snare|0.930569|0.839783|tangled_rope:0.055 snare:0.931 piton:0.014
board_of_peace_2026|tangled_rope|snare|0.821429|0.714592|tangled_rope:0.167 snare:0.821 piton:0.011
boltzmann_universality_2026|rope|tangled_rope|0.724789|0.654960|rope:0.269 tangled_rope:0.725
brain_network_paradigm_2026|snare|tangled_rope|0.758243|0.691062|tangled_rope:0.758 snare:0.242
broke_vs_poor_grocery_math|snare|tangled_rope|0.552810|0.616266|tangled_rope:0.553 snare:0.447
cancer_prevention|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
carbon_credit_markets_2026|snare|tangled_rope|0.886635|0.802564|tangled_rope:0.887 snare:0.113
carrier_deployment_deterrence|snare|tangled_rope|0.515947|0.613385|tangled_rope:0.516 snare:0.484
carrying_capacity|snare|tangled_rope|0.682160|0.651048|tangled_rope:0.682 snare:0.318
china_ev_export_oversupply|snare|tangled_rope|0.708428|0.663105|tangled_rope:0.708 snare:0.292
china_vactrain_standard|snare|tangled_rope|0.747971|0.684823|tangled_rope:0.748 snare:0.252
climate_event_attribution|snare|tangled_rope|0.532852|0.614297|tangled_rope:0.533 snare:0.467
cn_tech_decoupling_security_software|snare|tangled_rope|0.547886|0.615671|tangled_rope:0.548 snare:0.452
codex_access|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
coffee_cardiovascular_2026|tangled_rope|rope|0.954082|0.879236|rope:0.954 tangled_rope:0.017 scaffold:0.029
cognac_geopolitical_risk|snare|tangled_rope|0.928997|0.856683|tangled_rope:0.929 snare:0.071
cognitive_bicycle_scaffold|tangled_rope|rope|0.902244|0.784679|rope:0.902 tangled_rope:0.039 scaffold:0.059
cognitive_induction_gap|snare|tangled_rope|0.682160|0.651048|tangled_rope:0.682 snare:0.318
colorado_sbe_decentralization_friction|snare|tangled_rope|0.672040|0.646866|tangled_rope:0.672 snare:0.328
constitutional_consecration|snare|tangled_rope|0.518440|0.613516|tangled_rope:0.518 snare:0.482
constitutional_supremacy|snare|tangled_rope|0.813011|0.731036|tangled_rope:0.813 snare:0.187
conversational_dogmas_interruption|snare|tangled_rope|0.879646|0.794697|tangled_rope:0.880 snare:0.120
couples_residency_match|snare|tangled_rope|0.861485|0.775404|tangled_rope:0.861 snare:0.138
credentialism_national_security|snare|tangled_rope|0.603414|0.625110|tangled_rope:0.603 snare:0.397
cultural_homogenization_social_media|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
data_privacy_regulation|snare|tangled_rope|0.609697|0.626624|tangled_rope:0.610 snare:0.390
debt_trap_microfinance|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
deferential_realism_framework|rope|tangled_rope|0.733579|0.641013|rope:0.249 tangled_rope:0.734 scaffold:0.017
dexy_gold_protocol|rope|tangled_rope|0.530512|0.559689|rope:0.444 tangled_rope:0.531 scaffold:0.025
digital_identity_tether|snare|tangled_rope|0.514077|0.613365|tangled_rope:0.514 snare:0.486
djia_as_economic_barometer|tangled_rope|snare|0.555963|0.595637|tangled_rope:0.437 snare:0.556
dn_paywall|snare|tangled_rope|0.627037|0.631238|tangled_rope:0.627 snare:0.373
dunning_kruger_effect|snare|tangled_rope|0.550496|0.615941|tangled_rope:0.550 snare:0.449
ec_meta_manus_block|snare|tangled_rope|0.547886|0.615671|tangled_rope:0.548 snare:0.452
edelman_2026_insularity|snare|tangled_rope|0.630445|0.632352|tangled_rope:0.630 snare:0.370
electrification_scale_2026|rope|tangled_rope|0.694720|0.637577|rope:0.298 tangled_rope:0.695
elliq_ai_companion|snare|tangled_rope|0.646038|0.637290|tangled_rope:0.646 snare:0.354
emergency_bridge_scaffold|snare|tangled_rope|0.878387|0.793324|tangled_rope:0.878 snare:0.122
ergo_lets_protocol|tangled_rope|rope|0.984017|0.945983|rope:0.984
eu_digital_services_act|snare|tangled_rope|0.532852|0.614297|tangled_rope:0.533 snare:0.467
eu_ev_tariff_wall|snare|tangled_rope|0.544355|0.615286|tangled_rope:0.544 snare:0.456
eu_unanimity_rule_foreign_policy|snare|tangled_rope|0.532852|0.614297|tangled_rope:0.533 snare:0.467
eurozone_fragmentation_2026|tangled_rope|snare|0.617624|0.627294|tangled_rope:0.382 snare:0.618
exploration_vs_exploitation|snare|tangled_rope|0.768859|0.698137|tangled_rope:0.769 snare:0.231
fcc_dji_covered_list|snare|tangled_rope|0.599999|0.624291|tangled_rope:0.600 snare:0.400
fda_component_efficacy_standard|snare|tangled_rope|0.544355|0.615286|tangled_rope:0.544 snare:0.456
fiber_optic_chip_tech|snare|tangled_rope|0.768859|0.698137|tangled_rope:0.769 snare:0.231
fine_particle_policy|snare|tangled_rope|0.609697|0.626624|tangled_rope:0.610 snare:0.390
fitts_law_industrial_application|snare|tangled_rope|0.517518|0.613466|tangled_rope:0.518 snare:0.482
fmt_oncology_2026|tangled_rope|rope|0.904095|0.786512|rope:0.904 tangled_rope:0.050 scaffold:0.046
g7_debt_trap|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
gaza_border_control_rafah|snare|tangled_rope|0.616456|0.628398|tangled_rope:0.616 snare:0.384
glen_canyon_water_allocation|snare|tangled_rope|0.646038|0.637290|tangled_rope:0.646 snare:0.354
global_digital_divide|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
goodharts_law|snare|tangled_rope|0.758243|0.691062|tangled_rope:0.758 snare:0.242
google_ai_search_overview|snare|tangled_rope|0.547886|0.615671|tangled_rope:0.548 snare:0.452
google_universal_commerce_protocol|snare|tangled_rope|0.616151|0.628231|tangled_rope:0.616 snare:0.384
great_awakening_rekindling|snare|tangled_rope|0.933323|0.862918|tangled_rope:0.933 snare:0.067
great_mongolian_road_economic_dependency|snare|tangled_rope|0.764986|0.695635|tangled_rope:0.765 snare:0.235
greenland_defence_pact_2026|snare|tangled_rope|0.817147|0.734302|tangled_rope:0.817 snare:0.183
greshams_law|snare|tangled_rope|0.511326|0.613266|tangled_rope:0.511 snare:0.489
guano_wealth_extraction|snare|tangled_rope|0.609697|0.626624|tangled_rope:0.610 snare:0.390
guinea_worm_eradication|rope|tangled_rope|0.884110|0.773572|rope:0.100 tangled_rope:0.884 scaffold:0.016
hammurabi_lex_talionis|snare|tangled_rope|0.654364|0.640037|tangled_rope:0.654 snare:0.346
hawthorne_effect|snare|tangled_rope|0.642980|0.636257|tangled_rope:0.643 snare:0.357
horizon_liability_contract|tangled_rope|snare|0.904646|0.824032|tangled_rope:0.095 snare:0.905
hu_2026_election_rules|snare|tangled_rope|0.649981|0.638643|tangled_rope:0.650 snare:0.350
hub_short_form_tv_market_fragmentation|snare|tangled_rope|0.708428|0.663105|tangled_rope:0.708 snare:0.292
india_nuclear_liability_act_2010|snare|tangled_rope|0.515947|0.613385|tangled_rope:0.516 snare:0.484
indian_import_tariffs_eu|snare|tangled_rope|0.544355|0.615286|tangled_rope:0.544 snare:0.456
inner_model_confirmation_bias|snare|tangled_rope|0.609697|0.626624|tangled_rope:0.610 snare:0.390
innovators_dilemma|snare|tangled_rope|0.636479|0.634120|tangled_rope:0.636 snare:0.364
iron_law_of_oligarchy|snare|tangled_rope|0.823783|0.740088|tangled_rope:0.824 snare:0.176
israel_norwegian_law|snare|tangled_rope|0.667832|0.645112|tangled_rope:0.668 snare:0.332
japanese_energy_scaffold_2025|snare|tangled_rope|0.886635|0.802564|tangled_rope:0.887 snare:0.113
job_hunt_volume_system_2026|snare|tangled_rope|0.611527|0.627147|tangled_rope:0.612 snare:0.388
jp_eez_enforcement|snare|tangled_rope|0.513803|0.613313|tangled_rope:0.514 snare:0.486
lindy_effect|rope|tangled_rope|0.837529|0.731735|rope:0.153 tangled_rope:0.838
lung_transplant_protocol|snare|tangled_rope|0.609697|0.626624|tangled_rope:0.610 snare:0.390
manganese_catalysis_2026|tangled_rope|rope|0.939995|0.852971|rope:0.940 tangled_rope:0.018 scaffold:0.042
max_flow_min_cut|snare|tangled_rope|0.937607|0.869313|tangled_rope:0.938 snare:0.062
medical_residency_match|snare|tangled_rope|0.674772|0.647963|tangled_rope:0.675 snare:0.325
mexican_airline_merger|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
mrna_melanoma_therapy|snare|tangled_rope|0.544355|0.615286|tangled_rope:0.544 snare:0.456
mvt_theorem_constraint|tangled_rope|mountain|0.670039|0.630418|mountain:0.670 rope:0.325
narrative_engineering_2026|tangled_rope|rope|0.942119|0.854223|rope:0.942 tangled_rope:0.028 scaffold:0.030
ncaa_eligibility_rules|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
neural_substrate_2026|snare|tangled_rope|0.619517|0.629173|tangled_rope:0.620 snare:0.380
nfl_superbowl_marketing_regulation|snare|tangled_rope|0.609697|0.626624|tangled_rope:0.610 snare:0.390
notary_ink_dependency|tangled_rope|snare|0.562734|0.549277|tangled_rope:0.403 snare:0.563 piton:0.034
omelet_perfection_complexity|rope|tangled_rope|0.938450|0.857118|rope:0.053 tangled_rope:0.938
openai_health_review|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
openai_implicit_translator|snare|tangled_rope|0.823959|0.740069|tangled_rope:0.824 snare:0.176
openai_prism_development|snare|tangled_rope|0.515947|0.613385|tangled_rope:0.516 snare:0.484
openbsd_netiquette_protocol|snare|tangled_rope|0.833864|0.748963|tangled_rope:0.834 snare:0.166
openclaw_data_lock_in|snare|tangled_rope|0.551166|0.616030|tangled_rope:0.551 snare:0.449
openclaw_regulation|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
oral_glp1_market_access|snare|tangled_rope|0.768859|0.698137|tangled_rope:0.769 snare:0.231
p_g_golden_pear_surveillance|snare|tangled_rope|0.609697|0.626624|tangled_rope:0.610 snare:0.390
perovskite_self_etching|snare|tangled_rope|0.817147|0.734302|tangled_rope:0.817 snare:0.183
peter_principle|snare|tangled_rope|0.879646|0.794697|tangled_rope:0.880 snare:0.120
poetic_verse_and_past|snare|tangled_rope|0.600475|0.624492|tangled_rope:0.600 snare:0.400
portugal_polarization_threshold_2026|snare|tangled_rope|0.818029|0.735230|tangled_rope:0.818 snare:0.182
private_credit_market_opacity|snare|tangled_rope|0.515947|0.613385|tangled_rope:0.516 snare:0.484
quantum_entanglement_protocol|rope|tangled_rope|0.862038|0.769023|rope:0.136 tangled_rope:0.862
quine_self_replication|mountain|rope|0.615718|0.509576|mountain:0.307 rope:0.616 tangled_rope:0.072
radiologic_diagnostic_threshold|snare|tangled_rope|0.672040|0.646866|tangled_rope:0.672 snare:0.328
rare_earth_export_restrictions|snare|tangled_rope|0.609697|0.626624|tangled_rope:0.610 snare:0.390
rfc9293_state_machine|rope|tangled_rope|0.863585|0.762378|rope:0.130 tangled_rope:0.864
rogue_wave_control_2026|tangled_rope|rope|0.822748|0.708247|rope:0.823 tangled_rope:0.161 scaffold:0.017
rules_based_international_order|snare|tangled_rope|0.600475|0.624492|tangled_rope:0.600 snare:0.400
sapir_whorf_hypothesis|snare|tangled_rope|0.636479|0.634120|tangled_rope:0.636 snare:0.364
scientific_paradigm_lifecycle|tangled_rope|snare|0.776160|0.690031|tangled_rope:0.219 snare:0.776
seedance_export_restriction|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
semiconductor_fabrication_chokepoint|snare|tangled_rope|0.547886|0.615671|tangled_rope:0.548 snare:0.452
shadow_fleet_sanctions_evasion|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
sk_ai_act_2026|snare|tangled_rope|0.515947|0.613385|tangled_rope:0.516 snare:0.484
skills_based_hiring|rope|tangled_rope|0.807211|0.686289|rope:0.169 tangled_rope:0.807 scaffold:0.024
slow_crisis_invisibility|snare|tangled_rope|0.584105|0.621080|tangled_rope:0.584 snare:0.416
sludge_bureaucratic_friction|snare|tangled_rope|0.682160|0.651048|tangled_rope:0.682 snare:0.318
sm_addictive_design|tangled_rope|snare|0.832672|0.747943|tangled_rope:0.167 snare:0.833
somatic_focusing_awareness|rope|tangled_rope|0.688052|0.613470|rope:0.293 tangled_rope:0.688 scaffold:0.019
south_china_sea_arbitration_2016_2026|snare|tangled_rope|0.773652|0.701397|tangled_rope:0.774 snare:0.226
start_treaty|snare|tangled_rope|0.609697|0.626624|tangled_rope:0.610 snare:0.390
sti_clinical_testing_bottleneck|snare|tangled_rope|0.671425|0.646448|tangled_rope:0.671 snare:0.329
strait_coercion_2025|snare|tangled_rope|0.535475|0.614546|tangled_rope:0.535 snare:0.465
strange_attractor_dynamics|snare|tangled_rope|0.937871|0.869755|tangled_rope:0.938 snare:0.062
strange_attractor_systemic_risk|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
strategic_deep_sea_rare_earth_mining|snare|tangled_rope|0.609697|0.626624|tangled_rope:0.610 snare:0.390
streaming_bundling_mandate|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
student_loan_default_cliff|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
student_loan_interest_accrual|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
sunk_cost_fallacy|snare|tangled_rope|0.936138|0.867080|tangled_rope:0.936 snare:0.064
taiwan_ids_program|snare|tangled_rope|0.532852|0.614297|tangled_rope:0.533 snare:0.467
taiwan_university_application_system|snare|tangled_rope|0.749721|0.685932|tangled_rope:0.750 snare:0.250
tcp_rfc9293_interoperability|rope|tangled_rope|0.887338|0.788287|rope:0.107 tangled_rope:0.887
texas_insurance_market_instability|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
traveling_salesperson_problem|snare|tangled_rope|0.670823|0.646348|tangled_rope:0.671 snare:0.329
trump_epa_greenhouse_gas_reversal|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
trump_indian_tariffs_2026|snare|tangled_rope|0.609697|0.626624|tangled_rope:0.610 snare:0.390
tx_hispanic_pivot|snare|tangled_rope|0.672040|0.646866|tangled_rope:0.672 snare:0.328
uk_help_to_buy_scheme|snare|tangled_rope|0.708428|0.663105|tangled_rope:0.708 snare:0.292
uk_hicbc_trap|snare|tangled_rope|0.625703|0.630896|tangled_rope:0.626 snare:0.374
ulysses_eumaeus_1904|tangled_rope|snare|0.788165|0.671750|tangled_rope:0.189 snare:0.788 piton:0.023
ulysses_ithaca_1904|tangled_rope|snare|0.766291|0.638094|tangled_rope:0.195 snare:0.766 piton:0.039
ulysses_lotus_1904|tangled_rope|snare|0.807787|0.688816|tangled_rope:0.170 snare:0.808 piton:0.022
ulysses_rocks_1904|tangled_rope|snare|0.837083|0.725029|tangled_rope:0.149 snare:0.837 piton:0.014
ulysses_tower_1904|tangled_rope|snare|0.653466|0.592258|tangled_rope:0.323 snare:0.653 piton:0.023
us_arms_transfer_policy|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
us_china_chip_tariffs_v2|snare|tangled_rope|0.599999|0.624291|tangled_rope:0.600 snare:0.400
us_employer_health_insurance|snare|tangled_rope|0.680369|0.650267|tangled_rope:0.680 snare:0.320
us_taiwan_arms_sales|snare|tangled_rope|0.578392|0.619966|tangled_rope:0.578 snare:0.422
us_usmca_china_leverage|snare|tangled_rope|0.532852|0.614297|tangled_rope:0.533 snare:0.467
us_visa_lottery|snare|tangled_rope|0.708428|0.663105|tangled_rope:0.708 snare:0.292
utopia_apocalypse_fragility|snare|tangled_rope|0.682160|0.651048|tangled_rope:0.682 snare:0.318
vns_implant_for_trd|snare|tangled_rope|0.547886|0.615671|tangled_rope:0.548 snare:0.452
wpl_scotland|snare|tangled_rope|0.817147|0.734302|tangled_rope:0.817 snare:0.183
yangtze_fishing_ban|snare|tangled_rope|0.561181|0.617305|tangled_rope:0.561 snare:0.439
ROPE_CLUSTER_ONLY: 161
INVOLVES_MTN_SCAFFOLD_PITON: 2
MOUNTAIN_PITON_DISAGREEMENTS:
  MTN_PITON: mvt_theorem_constraint det=tangled_rope shadow=mountain eps=0.1 supp=0.05 theater=0.1 sig=false_ci_rope dist=mountain:0.670 rope:0.325
  MTN_PITON: quine_self_replication det=mountain shadow=rope eps=0.2 supp=0.05 theater=0.01 sig=false_ci_rope dist=mountain:0.307 rope:0.616 tangled_rope:0.072
MEAN_SHADOW_TOP_P: 0.676746
=== END TASK 3 ===

=== TASK 4: NON-OVERLAPPING POPULATION ===
HARD_TOTAL: 163
MULTI_TYPE_ORBIT: 153
SINGLE_TYPE_ORBIT: 10
OVERLAP_PCT: 93.87
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
INVERSE_COUNT: 604
absorbing_markov_chain_trap|snare|0.123175|scaffold/snare/unknown
abstraction_boundary_overrun|snare|0.069859|rope/snare
academic_fashion_modernism_2026|snare|0.029974|rope/snare
access_arbitrage|tangled_rope|0.015624|rope/tangled_rope
ad_fus_coordination|snare|0.247569|scaffold/snare
adaptive_lag_trap|snare|0.056870|rope/snare
adversarial_surface_inflation|snare|0.052851|rope/snare
adversarial_truth_decay|snare|0.032423|rope/snare
adverse_possession|tangled_rope|0.065755|rope/tangled_rope
advice_as_dangerous_gift|tangled_rope|0.016894|rope/tangled_rope/unknown
agency_atrophy|snare|0.036603|rope/snare
agent_opt_2026|snare|0.023881|rope/snare
aging_longevity_tests|tangled_rope|0.007907|rope/tangled_rope
ai_banal_capture|snare|0.165044|rope/snare
ai_driven_surveillance_sensor_layer|snare|0.174340|rope/snare
ai_edu_decentralization|tangled_rope|0.001458|rope/tangled_rope
ai_evaluators_matching|snare|0.162955|rope/snare
ai_nonconsensual_content_facilitation|snare|0.181713|rope/snare
ai_professional_displacement|snare|0.145766|rope/snare
ai_scholar_citation_trap|snare|0.071079|scaffold/snare/unknown
ai_superpowers_2026|snare|0.020762|scaffold/snare
ai_training_data_dependency|tangled_rope|0.213407|rope/tangled_rope
airport_slot_use_it_or_lose_it|snare|0.270435|rope/snare
algeria_france_colonial_legacy|snare|0.239149|rope/snare
algorithmic_bias|snare|0.138694|rope/snare
algorithmic_epistemic_capture|snare|0.052813|rope/snare
alignment_tax_tradeoff|snare|0.158942|rope/snare
altruistic_misery_paradox_2026|snare|0.197733|rope/snare
alzheimers_levetiracetam|tangled_rope|0.015181|rope/tangled_rope/unknown
alzheimers_nlrp3_inflammasome|snare|0.015388|scaffold/snare
amish_technological_renunciation|snare|0.236566|rope/snare
ancestral_pueblo_hydrology|snare|0.223338|scaffold/snare
anticipatory_capacity_failure|snare|0.054818|rope/snare
appropriations_brinkmanship|snare|0.166607|rope/snare
armra_colostrum_regulation|tangled_rope|0.059802|rope/tangled_rope
arrows_impossibility_theorem|tangled_rope|0.002387|rope/tangled_rope
art_market_decoupling|snare|0.040671|rope/snare
asce_7_22_seismic_design|tangled_rope|0.091601|rope/tangled_rope/unknown
asymmetric_burden_distribution|snare|0.044583|rope/snare
atrophied_optimization_piton|tangled_rope|0.160217|rope/tangled_rope
attention_as_bottleneck_resource|snare|0.047672|rope/snare
attention_market_cannibalization|snare|0.045946|rope/snare
australia_social_ban_2026|snare|0.174211|rope/snare
authoritarian_power_paradox|snare|0.108229|rope/snare
autonomous_toolchain_sprawl|snare|0.081013|rope/snare
availability_heuristic|tangled_rope|0.154836|rope/tangled_rope
average_is_over_2026|snare|0.241877|rope/snare
axiom_reasoner_2026|unknown|0.002353|scaffold/unknown
banach_fixed_point|mountain|0.155706|mountain/unknown
banach_fixed_point_theorem|mountain|0.155706|mountain/unknown
banach_tarski_paradox|mountain|0.155706|mountain/unknown
base_pair_complementarity|mountain|0.155706|mountain/unknown
bay_of_pigs_operational_silo|snare|0.047515|rope/snare
bayes_theorem_cognitive_bias|tangled_rope|0.015736|rope/tangled_rope
beehiiv_platform_model|tangled_rope|0.049129|rope/tangled_rope
belief_argument_conclusion|snare|0.073697|rope/snare
bgs_eigenvector_thermalization|tangled_rope|0.003565|rope/tangled_rope
bgs_spectral_universality|mountain|0.155706|mountain/unknown
bh_merger_gravitational_infall|mountain|0.155706|mountain/unknown
big_data_astrophysics_arbitrage|unknown|0.087334|rope/tangled_rope/unknown
biological_curiosity|mountain|0.050968|mountain/scaffold
bip_narrative_illusion|snare|0.217343|rope/snare
birthday_paradox_collision|mountain|0.155706|mountain/unknown
blackstone_carried_interest_taxation|tangled_rope|0.068195|rope/tangled_rope/unknown
blackstone_conflicts_of_interest|snare|0.270435|rope/snare
blackstone_smd_control|snare|0.289565|rope/snare
bnpl_payment_systems|tangled_rope|0.205463|rope/tangled_rope
board_of_peace_2026|tangled_rope|0.285408|rope/tangled_rope
boom_bust_path_dependency|rope|0.032397|piton/rope
bor_tax_exemption_nl|snare|0.181713|rope/snare
boundary_dissolution_risk|snare|0.043579|rope/snare
brazil_2026_general_elections|tangled_rope|0.033296|rope/tangled_rope
brazil_mexico_financial_requirement|snare|0.181713|rope/snare
brouwer_fixed_point|mountain|0.155706|mountain/unknown
buffons_needle_pi_estimation|tangled_rope|0.004922|rope/tangled_rope
burali_forti_paradox|mountain|0.086319|mountain/scaffold
burden_of_proof_engineering_safety|snare|0.190673|rope/snare
burden_of_proof_scientific|tangled_rope|0.205463|rope/tangled_rope
bureaucratic_legibility_collapse|snare|0.041286|rope/snare
bureaucratic_self_preservation|snare|0.063398|rope/snare
bushman_money_magic|snare|0.190673|rope/snare
busy_beaver_noncomputability|mountain|0.155706|mountain/unknown
c_physical_blue_wavelength|mountain|0.155706|mountain/unknown
cap_theorem|mountain|0.155706|mountain/unknown
capability_eval_overhang|snare|0.113880|rope/snare
capital_misallocation_spiral|snare|0.046531|rope/snare
capital_rotation_ai_narrative|tangled_rope|0.205463|rope/tangled_rope
carbon_credit_markets_2026|snare|0.197436|rope/snare/tangled_rope
cartel_drone_surveillance_el_paso|snare|0.253341|rope/snare
cascading_constraint_failure|snare|0.170034|rope/snare
cascading_uncertainty_2026|snare|0.079531|rope/snare
cbdc_implementation|snare|0.286077|rope/snare
central_limit_theorem|mountain|0.155706|mountain/unknown
chaitins_omega_undecidability|mountain|0.155706|mountain/unknown
challenger_o_ring_integrity|snare|0.286077|rope/snare
champions_bass_fishing_exclusion|tangled_rope|0.121048|rope/tangled_rope
child_marriage|snare|0.256191|rope/snare
china_africa_zero_tariff_2026|tangled_rope|0.025216|rope/tangled_rope/unknown
china_critical_mineral_chokepoint|snare|0.264557|rope/snare
choice_architecture_design|tangled_rope|0.049129|rope/tangled_rope
church_turing_thesis|mountain|0.155706|mountain/unknown
citation_collapse_dynamics|snare|0.039807|rope/snare
civilizational_lifecycle_solara|snare|0.072151|rope/snare
civilizational_maintenance_debt|tangled_rope|0.003203|rope/tangled_rope
click_chemistry_paradigm_2026|mountain|0.031850|mountain/scaffold
climate_attribution_2026|mountain|0.129119|mountain/scaffold
cloudflare_dual_class_asymmetry|snare|0.289565|rope/snare
clt_convergence_2026|mountain|0.155706|mountain/unknown
cma|unknown|0.290452|rope/tangled_rope/unknown
cmr_001|tangled_rope|0.001826|rope/tangled_rope
coalition_disinfo_framework_2026|tangled_rope|0.213407|rope/tangled_rope
cobra_effect|tangled_rope|0.002208|rope/tangled_rope
cognac_geopolitical_risk|snare|0.143317|rope/snare/tangled_rope
coinbase_crypto_volatility|tangled_rope|0.228051|rope/tangled_rope
cold_dark_matter_paradigm|tangled_rope|0.237538|rope/tangled_rope
collatz_conjecture_determinism|mountain|0.155706|mountain/unknown
collective_action_deadlock|snare|0.155472|rope/snare
collective_stupidity_2026|snare|0.024252|rope/snare
college_admissions_market|unknown|0.085805|rope/tangled_rope/unknown
colombia_2026_presidential_election|tangled_rope|0.002949|rope/tangled_rope
comitatus_bond|tangled_rope|0.204065|rope/tangled_rope
communal_narcissism_social_trap|snare|0.107355|rope/snare
complexity_debt|snare|0.133116|rope/snare
compounding_logic|tangled_rope|0.001981|rope/tangled_rope
consensus_without_truth|snare|0.061950|rope/snare
constitutional_supremacy|snare|0.268964|rope/snare/tangled_rope
constraint_galois_solvability|mountain|0.155706|mountain/unknown
constraint_interaction_explosion|snare|0.170489|rope/snare
constraint_lagrange_multipliers|tangled_rope|0.015181|rope/tangled_rope/unknown
constraint_riemann_mapping|tangled_rope|0.014862|rope/tangled_rope/unknown
constraint_tarski_undefinability|mountain|0.155706|mountain/unknown
constraint_twin_prime_conjecture|mountain|0.155706|mountain/unknown
constraint_yoneda|tangled_rope|0.006966|rope/tangled_rope
container_capacity_mismatch|snare|0.052813|rope/snare
conversational_dogmas_interruption|snare|0.205303|rope/snare/tangled_rope
conways_game_of_life_dynamics|mountain|0.155706|mountain/unknown
coordination_attack_vulnerability|snare|0.041056|rope/snare
coordination_fatigue|snare|0.187648|rope/snare
cost_of_observation|unknown|0.007699|rope/unknown
countable_infinity_cardinality|mountain|0.155706|mountain/unknown
couples_residency_match|snare|0.224596|rope/snare/tangled_rope
cow_field_poop|tangled_rope|0.006966|rope/tangled_rope
creative_commons_licensing|tangled_rope|0.006684|rope/tangled_rope
credibility_inflation|snare|0.057771|rope/snare
crisis_signal_saturation|snare|0.035398|rope/snare
crispr_genomic_rewrite_2026|tangled_rope|0.006894|rope/tangled_rope
critical_actor_overcentralization|snare|0.034768|rope/snare
cross_domain_coupling_spiral|snare|0.038461|rope/snare
cs_ecmo_bridge|unknown|0.000792|rope/unknown
cuba_mandatrophic_collapse|snare|0.202189|rope/snare/tangled_rope
cultural_memory_decay|snare|0.129821|rope/snare
cultural_refragmentation_2026|snare|0.019244|rope/snare
cumbria_mine_rejection|snare|0.264328|rope/snare
cz_plea_agreement_2026|snare|0.181681|rope/snare
dark_patterns_manipulation|snare|0.268111|rope/snare
data_laundering_pipeline|snare|0.046760|rope/snare
data_replication_paradox|tangled_rope|0.292465|rope/tangled_rope
decision_latency_mismatch|unknown|0.233579|rope/tangled_rope/unknown
delayed_feedback_instability|snare|0.269503|rope/snare
delta_force_selection_2026|snare|0.050061|rope/snare
dionysiac_frenzy|snare|0.289565|rope/snare/tangled_rope
doomsday_clock_framework|snare|0.295878|rope/snare/tangled_rope
dutch_minority_govt_2026|tangled_rope|0.237538|rope/tangled_rope
dwp_carers_allowance_cliff|snare|0.268111|rope/snare
edelman_2026_developed_stagnation|snare|0.245979|rope/snare/tangled_rope
edelman_2026_developing_volatility|tangled_rope|0.007907|rope/tangled_rope
education_unbundling_implementation|unknown|0.010482|rope/unknown
ehrenfest_barrier|mountain|0.155706|mountain/unknown
elite_capture_2026|snare|0.022286|rope/snare
elite_identity_capture_2026|snare|0.038274|rope/snare
em_clinical_guidelines|tangled_rope|0.051680|rope/tangled_rope
emergency_bridge_scaffold|snare|0.206676|scaffold/snare/tangled_rope
emergency_mode_lock_in|snare|0.036391|rope/snare
emergency_oversight_bureau|unknown|0.083590|rope/scaffold/unknown
emergency_powers_ratchet|snare|0.066526|scaffold/snare
emergent_goal_misalignment|snare|0.051835|rope/snare
empty_tomb_transformation|tangled_rope|0.007153|rope/tangled_rope
endocrine_disruption_society|snare|0.134694|rope/snare
endowment_effect|mountain|0.098004|mountain/scaffold
english_chinese_tense_structure|mountain|0.155706|mountain/unknown
epistemic_free_rider_problem|snare|0.051332|rope/snare
epistemic_overload_collapse|snare|0.036843|rope/snare
epstein_espionage_2026|snare|0.025453|rope/snare
epstein_files_2026|snare|0.029158|rope/snare
epstein_honeytrap|snare|0.059111|rope/snare
erasmus_rejoining_scaffold|unknown|0.162627|rope/scaffold/unknown
ergo_nipopows|mountain|0.062983|mountain/scaffold
ergo_storage_rent|unknown|0.001177|rope/unknown
ergot_grain_poisoning|snare|0.286077|rope/snare
erised_expectation|snare|0.119739|rope/snare
eu_affordable_housing_initiative|tangled_rope|0.224058|rope/tangled_rope
eu_asylum_outsourcing_framework|snare|0.239149|rope/snare
eu_mercosur_trade_agreement|tangled_rope|0.213407|indexically_opaque/rope/tangled_rope
euler_characteristic_topology|mountain|0.155706|mountain/unknown
event_fragmentation|mountain|0.155706|mountain/unknown
evfta_trade_agreement|tangled_rope|0.213407|rope/tangled_rope
evidence_half_life|snare|0.058663|rope/snare
evolutionary_mismatch_load|snare|0.216974|rope/snare
expert_disempowerment|snare|0.135637|rope/snare
extraordinary_narrative_shift|tangled_rope|0.003209|rope/tangled_rope
faa_boeing_regulatory_capture|tangled_rope|0.035846|rope/tangled_rope
factional_instability|unknown|0.082921|rope/tangled_rope/unknown
family_estrangement_ratio|snare|0.286077|rope/snare
family_succession_system|snare|0.289565|rope/snare
fast_growing_hierarchy|mountain|0.155706|mountain/unknown
feigenbaum_universality|mountain|0.155706|mountain/unknown
fermat_proof_barrier|mountain|0.155706|mountain/unknown
fgh_hierarchy_2026|mountain|0.155706|mountain/unknown
fiat_currency_lifecycle|snare|0.067998|rope/snare
financial_drag|snare|0.060568|rope/snare
finite_simple_group_classification|mountain|0.155706|mountain/unknown
fiscal_dominance_trap|snare|0.055773|rope/snare
floating_wall_2026|snare|0.299540|rope/snare
fmeca_procedures_1980|tangled_rope|0.010253|rope/tangled_rope
fnl_shadow_probe|tangled_rope|0.040643|rope/tangled_rope/unknown
four_color_theorem_topological_bound|mountain|0.155706|mountain/unknown
fragile_middle_layer_collapse|snare|0.053975|rope/snare
framing_effect|tangled_rope|0.030478|rope/tangled_rope
france_2027_presidential_election|unknown|0.000757|rope/unknown
france_local_elections_march_2026|tangled_rope|0.008950|scaffold/tangled_rope/unknown
franchisee_corporate_squeeze|tangled_rope|0.177780|rope/tangled_rope
frontex_pushback_coordination|snare|0.156529|rope/snare
fundamental_theorem_of_algebra|mountain|0.155706|mountain/unknown
gale_shapley|snare|0.223482|rope/snare
galois_theory_symmetry|mountain|0.090706|mountain/scaffold
gamblers_ruin_stochastic_extinction|unknown|0.270094|scaffold/unknown
gauss_bonnet_topology|mountain|0.155706|mountain/unknown
gaza_aid_permit_revocation|snare|0.262332|rope/snare/tangled_rope
gemini_scientific_advancement|tangled_rope|0.015181|rope/tangled_rope/unknown
generational_replacement_inertia|snare|0.256436|rope/snare
genetic_algorithms_evolution|mountain|0.077302|mountain/scaffold
genetic_predisposition|tangled_rope|0.044085|rope/tangled_rope
genetic_predisposition_mania|mountain|0.155706|mountain/unknown
genie_ip_constraint|snare|0.052148|scaffold/snare/unknown
geophysics_superionic_core|mountain|0.155706|mountain/unknown
germany_tennet_takeover|tangled_rope|0.019668|rope/tangled_rope/unknown
ghost_fishing_gear|snare|0.247569|rope/snare
gig_economy_algorithmic_management|snare|0.286077|rope/snare
global_economic_anxiety_2026|snare|0.016113|rope/snare
global_stimulus_spree|tangled_rope|0.016802|scaffold/tangled_rope
goal_boundary_poisoning|snare|0.038990|rope/snare
goedels_incompleteness_theorems|mountain|0.155706|mountain/unknown
gold_piton_2026|rope|0.017624|piton/rope
goldbach_conjecture|mountain|0.155706|mountain/unknown
goodstein_theorem_finite_proof|unknown|0.010495|rope/unknown
governance_latency_gap|snare|0.119091|rope/snare
governance_overfitting|snare|0.254767|rope/snare
gpt5_codex_dev_cycle|tangled_rope|0.008117|rope/tangled_rope
graph_coloring_complexity|tangled_rope|0.009679|rope/tangled_rope
great_awakening_rekindling|snare|0.137082|rope/snare/tangled_rope
greenland_defence_pact_2026|snare|0.265698|rope/snare/tangled_rope
greenland_seizure_trade_war|snare|0.217343|rope/snare
grete_samsa_transition|unknown|0.103887|rope/tangled_rope/unknown
grievance_stack_overflow|snare|0.048237|rope/snare
gs1_gln_identification|tangled_rope|0.068443|rope/tangled_rope/unknown
gs1_standardized_identification|unknown|0.011362|rope/unknown
gs_market_clearing|tangled_rope|0.014469|rope/tangled_rope/unknown
guinea_worm_eradication|rope|0.226428|rope/unknown
guthrie_kidnapping_2026|snare|0.072041|rope/snare
halting_problem_undecidability|mountain|0.155706|mountain/unknown
hanlons_razor|tangled_rope|0.001791|rope/tangled_rope
hasbro_licensing_restriction|tangled_rope|0.051622|rope/tangled_rope
hd101584_stellar_evolution|mountain|0.155706|mountain/unknown
hegemonic_entropy_2026|unknown|0.126648|rope/unknown
heine_borel|mountain|0.155706|mountain/unknown
heisenberg_uncertainty|mountain|0.155706|mountain/unknown
hershey_salt_strategy|tangled_rope|0.237538|rope/tangled_rope
heuristic_optimization|mountain|0.172968|mountain/scaffold
hhs_fetal_tissue_research_ban_2019|snare|0.181713|rope/snare
hidden_interdependency_risk|snare|0.088572|rope/snare
hilberts_hotel_infinity|mountain|0.048336|mountain/scaffold
hoa_covenants|unknown|0.008980|rope/unknown
hollow_state_syndrome|snare|0.037883|rope/snare
hp_liberalism|tangled_rope|0.082538|rope/tangled_rope
huang_expectation_resilience_2026|snare|0.131358|rope/snare
hydra_game|unknown|0.081210|rope/tangled_rope/unknown
hypercompression_of_time_horizons|snare|0.044905|rope/snare
hypernormie_equilibrium|snare|0.052112|rope/snare
ice_raids_minnesota_2026|snare|0.195034|rope/snare
ice_safe_departure|snare|0.231355|rope/snare
identity_stack_incompatibility|snare|0.054236|rope/snare
incentive_surface_warping|snare|0.039807|rope/snare
indexical_relativity_core|mountain|0.155706|mountain/unknown
india_semi_mission|tangled_rope|0.068024|rope/tangled_rope
individual_revolution_autonomy|snare|0.162955|rope/snare
indo_german_defense_pact|tangled_rope|0.237538|indexically_opaque/rope/tangled_rope
indonesia_penal_code_2023|snare|0.239149|indexically_opaque/rope/snare
inference_cost_scaling_law|snare|0.058631|rope/snare
information_foraging_theory|mountain|0.129119|mountain/scaffold
informational_time_2026|snare|0.280494|rope/snare
infrastructure_interoperability_decay|snare|0.039259|rope/snare
institutional_inertia_lock|tangled_rope|0.123599|rope/tangled_rope
institutional_memory_loss|snare|0.046352|rope/snare
institutional_mutation_domestication|snare|0.217601|rope/snare
institutional_mutation_without_selection|snare|0.034961|rope/snare
insult_wisdom_training|unknown|0.102063|rope/tangled_rope/unknown
interface_contract_breakdown|snare|0.057790|rope/snare
internet_evolution_lifecycle|snare|0.175542|rope/snare
interpretive_frame_fragmentation|snare|0.035554|rope/snare
intertemporal_responsibility_gap|snare|0.047773|rope/snare
invisible_infrastructure_dependency|snare|0.039809|rope/snare
iran_guardian_council_vetting|snare|0.245003|rope/snare
iran_hijab_law|snare|0.239149|rope/snare
iran_mandatrophic_collapse|snare|0.193270|rope/snare
iran_war_room_2026|snare|0.069950|rope/snare
iron_law_of_oligarchy|snare|0.259912|rope/snare/tangled_rope
irreversible_policy_commitment|snare|0.236959|rope/snare
isa_education_scaffold|tangled_rope|0.005131|scaffold/tangled_rope
israel_egypt_gas_deal|tangled_rope|0.205463|rope/tangled_rope
israel_electoral_threshold|tangled_rope|0.205463|rope/tangled_rope
israel_override_clause|snare|0.215285|rope/snare
israel_surplus_vote_agreements|tangled_rope|0.003147|rope/tangled_rope
israeli_settlement_policy_authority_restriction|snare|0.254224|rope/snare/tangled_rope
iterated_function_system_convergence|tangled_rope|0.005744|rope/tangled_rope
ivt_accessibility_barrier|tangled_rope|0.001826|rope/tangled_rope
japanese_energy_scaffold_2025|snare|0.197436|rope/snare/tangled_rope
jevons_paradox|tangled_rope|0.044085|rope/tangled_rope
jp_nativist_politics|snare|0.285878|indexically_opaque/rope/snare
jupiter_composition_knowledge_gap|tangled_rope|0.015181|rope/tangled_rope/unknown
keltner_relationship_evaluation|tangled_rope|0.014907|rope/tangled_rope/unknown
khantivadin_radical_patience|snare|0.257809|rope/snare
kirby_paris_theorem|mountain|0.155706|mountain/unknown
kjv_linguistic_residue|rope|0.012070|piton/rope
kjv_textual_authority|tangled_rope|0.151932|rope/tangled_rope
kleene_recursion_theorem|mountain|0.155706|mountain/unknown
kolmogorov_complexity|mountain|0.155706|mountain/unknown
labor_union_dues|tangled_rope|0.273035|rope/tangled_rope
landscape_of_fear_2026|tangled_rope|0.150400|rope/tangled_rope
latent_goal_activation|snare|0.039026|rope/snare
latent_regulatory_bomb|snare|0.245236|rope/snare
law_of_diminishing_returns|tangled_rope|0.006915|rope/tangled_rope
lcdm_small_scale_anomalies|tangled_rope|0.122956|rope/tangled_rope
legal_formalism_overhang|snare|0.042753|rope/snare
legibility_trap|snare|0.036603|rope/snare
legitimacy_without_capacity|snare|0.051790|rope/snare
legitimacy_without_effectiveness|snare|0.049063|rope/snare
lehman_repo_105|snare|0.206513|rope/snare
liar_paradox|mountain|0.155706|mountain/unknown
lindy_effect|rope|0.268265|rope/unknown
linguistic_relativity_cultural_framing|tangled_rope|0.035151|rope/tangled_rope
liquidity_illusion|snare|0.060787|rope/snare
litchfield_sensitive_locations_2026|snare|0.109449|rope/snare
lobs_theorem|mountain|0.155706|mountain/unknown
local_vs_global_optima|mountain|0.155706|mountain/unknown
lorenz_attractor_dynamics|tangled_rope|0.009557|rope/tangled_rope
lowenheim_skolem_theorem|mountain|0.155706|mountain/unknown
lsd_microdosing_professional_openness|tangled_rope|0.009523|rope/tangled_rope
lyapunov_stability|mountain|0.155706|mountain/unknown
magna_carta_liberties|tangled_rope|0.212286|rope/tangled_rope
maha_recovery_2026|unknown|0.026034|scaffold/unknown
maintenance_capacity_shortfall|snare|0.047814|rope/snare
maladaptive_selection_process|snare|0.045286|rope/snare
mandatrophic_margin_collapse|snare|0.191781|rope/snare
manga_distribution_duopoly|tangled_rope|0.292465|rope/tangled_rope
marriage_market_asymmetry_2026|snare|0.233296|rope/snare
mars_rovers_navigational_autonomy|tangled_rope|0.010249|rope/tangled_rope
martian_signal_latency|mountain|0.155706|mountain/unknown
mass_market_extinction_2026|snare|0.199652|rope/snare
matching_market_congestion_externality|tangled_rope|0.013229|rope/tangled_rope/unknown
material_tensile_strength|mountain|0.155706|mountain/unknown
max_flow_min_cut|snare|0.130687|rope/snare/tangled_rope
med_diet_consensus_2026|tangled_rope|0.208325|rope/tangled_rope
memetic_fitness_vs_truth|snare|0.046594|rope/snare
meta_governance_overload|snare|0.037026|rope/snare
meta_model_lock_in|snare|0.043406|rope/snare
meta_nuclear_power_agreement|tangled_rope|0.027383|rope/tangled_rope
meta_pay_or_okay_model|snare|0.264660|indexically_opaque/rope/snare
migration_decision_threshold|tangled_rope|0.055934|rope/tangled_rope/unknown
mil_std_461g_emi_control|tangled_rope|0.002070|rope/tangled_rope
mil_std_810f_tailoring|tangled_rope|0.005764|rope/tangled_rope
milano_cortina_2026|rope|0.003416|piton/rope
minimax_theorem_game_equilibrium|tangled_rope|0.157051|rope/tangled_rope
minnesota_sovereignty_2026|snare|0.152919|rope/snare
mit_tfus_2026|snare|0.072399|scaffold/snare/unknown
model_autonomy_creep|snare|0.061038|rope/snare
model_collapse_feedback_loop|snare|0.045901|rope/snare
model_of_models_regression|snare|0.062703|rope/snare
moltbook_breach_2026|snare|0.042230|rope/snare
monetary_regime_transition|tangled_rope|0.001791|rope/tangled_rope
monty_hall_conditional_probability|mountain|0.155706|mountain/unknown
moores_law|tangled_rope|0.041259|rope/tangled_rope/unknown
moral_outsourcing|snare|0.046298|rope/snare
multi_agent_reward_hacking|snare|0.034798|rope/snare
naming_as_control|snare|0.117040|rope/snare
narcissistic_ego_maintenance|snare|0.222092|rope/snare
narrative_capacity_exhaustion|snare|0.039214|rope/snare
narrative_overfitting|snare|0.033946|rope/snare
nato_arctic_defense_cooperation|tangled_rope|0.009399|rope/tangled_rope
negative_emissions_arbitrage|unknown|0.296071|rope/tangled_rope/unknown
net_zero_stabilization|unknown|0.039641|rope/unknown
neural_interoperability|unknown|0.142770|rope/unknown
neurodiversity_spectrum|unknown|0.242879|rope/tangled_rope/unknown
news_paywall_inequality|tangled_rope|0.051622|rope/tangled_rope
nfl_superbowl_halftime_exclusivity|tangled_rope|0.059802|rope/tangled_rope
nine_day_buffer|snare|0.266742|rope/snare/tangled_rope
no_cloning_theorem|mountain|0.155706|mountain/unknown
noether_isomorphism_access|tangled_rope|0.002758|rope/tangled_rope
non_compete_agreements|snare|0.286077|rope/snare
norm_erosion_threshold|snare|0.068103|rope/snare
north_sea_wind_grid|tangled_rope|0.282300|rope/tangled_rope
nsw_transmission_bottleneck|snare|0.048204|scaffold/snare/unknown
nuclear_order_2026|snare|0.250628|rope/snare/tangled_rope
ny_private_school_discount|tangled_rope|0.051622|rope/tangled_rope
nyc_metrocard_art_licensing|tangled_rope|0.004980|rope/tangled_rope
olympic_legacy_curling_investment|tangled_rope|0.009999|rope/tangled_rope/unknown
olympic_medal_allocation|tangled_rope|0.013229|rope/tangled_rope/unknown
omelet_perfection_complexity|rope|0.142882|rope/unknown
openai_api_access|snare|0.077496|scaffold/snare/unknown
openai_codex_app_constraint|snare|0.071079|scaffold/snare/unknown
openai_implicit_translator|snare|0.259931|rope/snare/tangled_rope
openbsd_netiquette_protocol|snare|0.251037|rope/snare/tangled_rope
openscholar_peer_review|snare|0.060942|scaffold/snare/unknown
opioid_political_realignment_2026|snare|0.293922|rope/snare
optimization_fragility|snare|0.160764|rope/snare
orbital_data_center_2026|snare|0.143457|rope/snare
oscar_campaign_spending|tangled_rope|0.009399|rope/tangled_rope
other_peoples_troubles_2026|snare|0.057341|rope/snare
overfitting_to_frameworks|snare|0.120500|rope/snare
pancreatic_cancer_lethality_v1|mountain|0.155706|mountain/unknown
pareto_principle|mountain|0.155706|mountain/unknown
paris_municipal_reform_2026|unknown|0.001744|rope/unknown
parkinsons_law|tangled_rope|0.002070|rope/tangled_rope
participatory_observer_hypothesis|tangled_rope|0.012356|rope/tangled_rope
paxsilica_framework|tangled_rope|0.213407|rope/tangled_rope
pe_fund_level_leverage|snare|0.250280|rope/snare
perovskite_self_etching|snare|0.265698|scaffold/snare/tangled_rope
peter_principle|snare|0.205303|rope/snare/tangled_rope
planetary_boundaries|snare|0.223482|rope/snare
plastic_asphalt_mandate|tangled_rope|0.059802|rope/tangled_rope
platonic_coparenting_decoupling|unknown|0.002255|scaffold/unknown
pna|tangled_rope|0.003189|rope/tangled_rope
poincare_conjecture|mountain|0.155706|mountain/unknown
policy_lag_catastrophe|snare|0.047587|rope/snare
politeness_face_negotiation|tangled_rope|0.068024|rope/tangled_rope
portugal_ad_stability_2026|tangled_rope|0.008950|scaffold/tangled_rope/unknown
portugal_polarization_threshold_2026|snare|0.264770|rope/snare/tangled_rope
postman_survival_protocol|rope|0.122915|rope/unknown
power_set_axiomatic_extraction|tangled_rope|0.108033|rope/tangled_rope
power_without_responsibility|snare|0.036399|rope/snare
prestige_signal_inflation|snare|0.069187|rope/snare
price_signal_corruption|snare|0.058645|rope/snare
prime_number_theorem|mountain|0.155706|mountain/unknown
prisoners_dilemma_equilibrium|mountain|0.155706|mountain/unknown
private_identity_integration|tangled_rope|0.010203|rope/tangled_rope
procedural_compliance_theater|snare|0.123757|rope/snare/tangled_rope
procedural_legitimacy_decay|snare|0.158043|rope/snare
project_vault_extraction_2026|snare|0.083501|rope/snare
protocol_drift_accumulation|snare|0.064317|rope/snare
publishing_embargo|tangled_rope|0.013229|rope/tangled_rope/unknown
pythagorean_geometric_constancy|mountain|0.155706|mountain/unknown
quantum_entanglement_protocol|rope|0.230977|rope/unknown
quantum_measurement_gap|mountain|0.155706|mountain/unknown
quantum_nonlocality_2026|mountain|0.155706|mountain/unknown
quellcrist_falconer_justice|snare|0.190673|rope/snare
qwerty_vs_dvorak|tangled_rope|0.151932|rope/tangled_rope
ramsey_numbers|mountain|0.155706|mountain/unknown
rare_earth_hydrogen_extraction|tangled_rope|0.051622|rope/tangled_rope
rare_earth_seabed_mining|tangled_rope|0.017660|rope/tangled_rope
rational_inertia_trap|snare|0.117371|rope/snare
recipe_scaling_ai|unknown|0.002915|scaffold/unknown
reciprocity_laws_math|mountain|0.155706|mountain/unknown
regulatory_capture|unknown|0.009420|rope/unknown
relativity_of_simultaneity|mountain|0.155706|mountain/unknown
relativity_physical_invariance|mountain|0.155706|mountain/unknown
rent_seeking_equilibrium|snare|0.046298|rope/snare
reputational_cascade_failure|snare|0.044352|rope/snare
responsibility_dilution|snare|0.040221|rope/snare
responsibility_without_power|snare|0.035243|rope/snare
rfc9293_state_machine|rope|0.237622|rope/unknown
rices_theorem_undecidability|mountain|0.155706|mountain/unknown
riot_incentive_loop_2026|snare|0.048381|rope/snare
risk_socialization_threshold|snare|0.052469|rope/snare
ritual_transition_scaffold|tangled_rope|0.137185|rope/tangled_rope/unknown
ritual_without_belief|snare|0.033929|rope/snare
robustness_vs_efficiency_tradeoff|snare|0.156319|rope/snare
rosen_bridge_protocol|unknown|0.003003|rope/unknown
rotation_seven_black_soil|unknown|0.000000|rope/unknown
rotation_seven_isolation|snare|0.264557|rope/snare
royal_navy_middle_east_withdrawal|unknown|0.133032|rope/tangled_rope/unknown
russells_paradox_self_reference|unknown|0.107522|scaffold/unknown
russian_war_cannibalization|snare|0.232824|rope/snare
sa_renewable_price_differential|tangled_rope|0.027383|rope/tangled_rope
sadhu_integrity_protocol|tangled_rope|0.053461|rope/tangled_rope/unknown
sat_csp_complexity|tangled_rope|0.009679|rope/tangled_rope
satellite_d2m_standard|tangled_rope|0.008300|rope/tangled_rope
scam_compound_2026|snare|0.095691|rope/snare
scurvy_maritime_extraction|snare|0.251426|rope/snare
second_order_unintended_consequences|snare|0.047814|rope/snare
semantic_attack_surface|snare|0.037054|rope/snare
semiconductor_mission_2026|tangled_rope|0.059802|rope/tangled_rope
shadow_pricing_failure|snare|0.060118|rope/snare
shannon_entropy_limit|mountain|0.155706|mountain/unknown
shannon_source_coding|mountain|0.155706|mountain/unknown
shitty_feedback_handling|unknown|0.009953|rope/unknown
shobies_existential_commitment|tangled_rope|0.062355|rope/tangled_rope
shock_propagation_asymmetry|snare|0.042342|rope/snare
signal_without_control|snare|0.201990|rope/snare
silent_dependency_activation|snare|0.243813|rope/snare
silicon_lexicon_overload|snare|0.286077|rope/snare
silver_scarcity_2026|mountain|0.155706|mountain/unknown
skolems_paradox|mountain|0.172968|mountain/scaffold
sleep_debt_externality|snare|0.289515|rope/snare
smartphone_ubiquity|unknown|0.103887|rope/tangled_rope/unknown
social_credit_architecture|snare|0.280372|rope/snare
social_loafing|tangled_rope|0.007470|rope/tangled_rope
social_media_participation_threshold|tangled_rope|0.045721|rope/tangled_rope
social_narrative_casting|tangled_rope|0.068319|rope/tangled_rope
soft_authoritarian_drift|snare|0.052851|rope/snare
sorites_paradox|tangled_rope|0.150400|rope/tangled_rope
south_china_sea_arbitration_2016_2026|snare|0.298603|rope/snare/tangled_rope
sovereignty_as_arbitrage|tangled_rope|0.006468|rope/tangled_rope
spain_digital_offensive_2026|snare|0.201922|rope/snare
square_cube_law|mountain|0.155706|mountain/unknown
st_petersburg_paradox|tangled_rope|0.003067|rope/tangled_rope
stable_marriage_coordination|tangled_rope|0.006569|rope/tangled_rope
star_formation_barrier_g0253|mountain|0.155706|mountain/unknown
star_to_black_hole_observational_limit|mountain|0.155706|mountain/unknown
status_flattening_effect|snare|0.067934|rope/snare
strange_attractor_dynamics|snare|0.130245|rope/snare/tangled_rope
structural_extraction_without_actor|snare|0.115298|rope/snare
sturgeons_law|mountain|0.155706|mountain/unknown
suanne_coup_of_peace|snare|0.289114|rope/snare
sunk_cost_fallacy|snare|0.132920|rope/snare/tangled_rope
suslin_hypothesis_undecidability|mountain|0.155706|mountain/unknown
sylow_theorems_group_theory|mountain|0.155706|mountain/unknown
synthetic_data_feedback_loop|snare|0.056563|rope/snare
systemic_blindspot|snare|0.065690|rope/snare
tail_risk_compression|snare|0.077539|rope/snare
taiwan_existential_sovereignty|snare|0.286137|rope/snare
taiwan_grand_bargain|snare|0.280847|rope/snare
taiwan_storm_2026|snare|0.133763|rope/snare
taxonomy_drift|snare|0.169897|rope/snare
tcp_rfc9293_interoperability|rope|0.211713|rope/unknown
teaching_horses_to_sing|unknown|0.005498|rope/tangled_rope/unknown
tear_gas_repression_2026|snare|0.046432|rope/snare
technocratic_overreach|snare|0.295865|rope/snare
temporal_scale_arbitrage|unknown|0.005760|rope/tangled_rope/unknown
temporal_scarcity|rope|0.047322|piton/rope
terrain_inaccessibility_wheeled_vehicles|mountain|0.155706|mountain/unknown
thai_senate_veto_2026|snare|0.063398|rope/snare
the_bacchae_madness_protocol|snare|0.202189|rope/snare
the_calm_protocol_suppression|snare|0.253341|rope/snare
theatrical_neutrality|snare|0.035249|rope/snare
theory_of_visitors|tangled_rope|0.062355|rope/tangled_rope
thermodynamics_entropy|mountain|0.155706|mountain/unknown
three_body_unpredictability|mountain|0.155706|mountain/unknown
toxic_social_infection|snare|0.251426|rope/snare
toxoplasma_hub_2026|snare|0.007608|scaffold/snare
trade_secret_law|tangled_rope|0.237538|rope/tangled_rope
tragedy_of_the_commons|unknown|0.004210|rope/tangled_rope/unknown
transformer_self_attention|unknown|0.005381|rope/tangled_rope/unknown
transient_event_detection|tangled_rope|0.012503|rope/tangled_rope
trillion_bond_rush_2026|unknown|0.086383|scaffold/unknown
trivial_topology_info_asymmetry|tangled_rope|0.051622|rope/tangled_rope
trump_critical_minerals|tangled_rope|0.182377|rope/tangled_rope
trump_making_china_great_2026|tangled_rope|0.003189|rope/tangled_rope
trump_second_term_authoritarianism_2026|snare|0.268111|rope/snare/tangled_rope
tsp_computational_complexity|tangled_rope|0.102299|rope/tangled_rope
tsp_duplicate_elimination|rope|0.291753|rope/unknown
ua_wartime_mobilization|snare|0.268111|rope/snare
uk_graduate_visa_salary_threshold|snare|0.181713|rope/snare
uk_necc_formation|tangled_rope|0.200092|rope/tangled_rope
uk_unpaid_care_system|snare|0.253863|rope/snare
ukraine_tight_gas_pilot|tangled_rope|0.205463|rope/tangled_rope
ulysses_aeolus_1904|snare|0.211954|rope/snare/tangled_rope
ulysses_cyclops_1904|snare|0.159319|rope/snare/tangled_rope
ulysses_lestrygonians_1904|snare|0.262588|rope/snare/tangled_rope
ulysses_nausicaa_1904|snare|0.220921|rope/snare/tangled_rope
ulysses_rocks_1904|tangled_rope|0.274971|rope/tangled_rope
ulysses_scylla_1904|snare|0.239567|rope/snare/tangled_rope
ulysses_sirens_1904|snare|0.154950|rope/snare/tangled_rope
un_high_seas_treaty_2026|tangled_rope|0.023018|rope/tangled_rope
unclos_2026|unknown|0.122488|rope/unknown
union_protection_underperformance|tangled_rope|0.045721|rope/tangled_rope
unrequited_love_protocol|snare|0.013621|scaffold/snare
unrwa_eviction_order|snare|0.197178|rope/snare
us_greenland_envoy|snare|0.279593|rope/snare
us_israel_faa_502b_nonenforcement|snare|0.104809|rope/snare
us_labor_mobility|tangled_rope|0.028959|rope/tangled_rope
us_sdf_alliance_abandonment_2026|snare|0.279593|rope/snare
us_two_party_duopoly|snare|0.247569|rope/snare
us_vaccine_recommendation_dismantling_2026|snare|0.181713|rope/snare
us_venezuela_oil_pressure|snare|0.252057|rope/snare
us_venezuela_plausible_deniability_2025|snare|0.278354|rope/snare
value_alignment_drift|snare|0.274601|rope/snare
value_extraction_plateau|snare|0.032423|rope/snare
van_der_waerden_theorem|mountain|0.155706|mountain/unknown
venezuela_oil_privatization_v1|snare|0.181713|rope/snare
viral_emergence_covid19_exemplar|rope|0.120133|piton/rope
viral_transmission_rates|snare|0.217601|rope/snare
visa_ipo_regulatory_compliance|tangled_rope|0.223861|rope/tangled_rope
visa_judgment_sharing_agreement|tangled_rope|0.153785|rope/tangled_rope
visibility_bias_governance|snare|0.284339|rope/snare/tangled_rope
weierstrass_proof_limits|mountain|0.155706|mountain/unknown
whitehead_problem_undecidability|mountain|0.155706|mountain/unknown
wikipedia_notability_requirement_2026|tangled_rope|0.148271|rope/tangled_rope
working_dog_training|tangled_rope|0.015748|rope/tangled_rope/unknown
world_factbook_sunset_2026|snare|0.095838|rope/snare
wpl_scotland|snare|0.265698|rope/snare/tangled_rope
xi_mao_ideological_centralization|snare|0.066708|rope/snare
yc_equity_squeeze|tangled_rope|0.051622|rope/tangled_rope
yt_ai_slop_incentive|snare|0.239149|rope/snare
zipfs_law|snare|0.125238|rope/snare
zombie_reasoning_2026|snare|0.028099|rope/snare
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
tangled_rope|extractiveness|0.477270|0.180643
tangled_rope|suppression|0.568617|0.200414
tangled_rope|theater|0.243014|0.227249
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
  OVERRIDE_ROPE: automatic_enrollment_defaults sig=constructed_low_extraction eps=0.050000 hn=0.051397
  OVERRIDE_ROPE: berkshire_compounding_culture sig=constructed_low_extraction eps=0.100000 hn=0.058911
  OVERRIDE_ROPE: boundary_protocol sig=coupling_invariant_rope eps=0.000000 hn=0.155706
  OVERRIDE_ROPE: cancer_chronotherapy_timing sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: cinderella_midnight_deadline sig=coupling_invariant_rope eps=0.100000 hn=0.155706
  OVERRIDE_ROPE: copyleft_viral_licensing sig=constructed_low_extraction eps=0.050000 hn=0.088458
  OVERRIDE_ROPE: cuny_light_2026 sig=constructed_low_extraction eps=0.050000 hn=0.037002
  OVERRIDE_ROPE: decentralized_infrastructure_rope sig=constructed_low_extraction eps=0.080000 hn=0.096899
  OVERRIDE_ROPE: fair_use_doctrine sig=constructed_low_extraction eps=0.100000 hn=0.188166
  OVERRIDE_ROPE: ice_memory_archive sig=coupling_invariant_rope eps=0.100000 hn=0.155706
  OVERRIDE_ROPE: microrobot_manipulation sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: mom_z14_2026 sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: open_source_commons sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: perseverance_rover_autonomy sig=coupling_invariant_rope eps=0.100000 hn=0.155706
  OVERRIDE_ROPE: planetary_diet_constraint_2026 sig=constructed_low_extraction eps=0.100000 hn=0.259551
  OVERRIDE_ROPE: portuguese_presidential_term_limits sig=constructed_low_extraction eps=0.020000 hn=0.205244
  OVERRIDE_ROPE: rafah_crossing_lifeline sig=coupling_invariant_rope eps=0.150000 hn=0.155706
  OVERRIDE_ROPE: rare_earth_coop_2026 sig=constructed_low_extraction eps=0.020000 hn=0.079812
  OVERRIDE_ROPE: spv_variations_us_cold sig=coupling_invariant_rope eps=0.020000 hn=0.155706
  OVERRIDE_ROPE: sts86_ascent_checklist sig=constructed_low_extraction eps=0.050000 hn=0.197249
  OVERRIDE_ROPE: swift_piton_snap sig=coupling_invariant_rope eps=0.040000 hn=0.155706
  OVERRIDE_ROPE: thai_article_112_mountain sig=constructed_low_extraction eps=0.040000 hn=0.212451
  OVERRIDE_ROPE: vertebrate_turning_point_2026 sig=constructed_low_extraction eps=0.050000 hn=0.155463
  OVERRIDE_ROPE: viral_emergence_covid19_exemplar sig=constructed_low_extraction eps=0.150000 hn=0.120133
  OVERRIDE_ROPE: wikipedia_crowdsourcing_2026 sig=constructed_low_extraction eps=0.050000 hn=0.160282
  OVERRIDE_ROPE: wikipedia_noncommercial_model sig=coupling_invariant_rope eps=0.120000 hn=0.155706
NON_OVERRIDE_ROPE_ENTROPY_MEAN: 0.254719 (n=24)
OVERRIDE_ROPE_ENTROPY_MEAN: 0.145748 (n=27)
ALL_TYPE_EPS_STATS:
  mountain: n=128 mean=0.083281 std=0.058964 min=0.000000 max=0.200000
  rope: n=51 mean=0.123333 std=0.071309 min=0.000000 max=0.250000
  tangled_rope: n=282 mean=0.477270 std=0.180643 min=0.000000 max=1.000000
  snare: n=519 mean=0.700983 std=0.131354 min=0.490000 max=1.000000
  scaffold: n=0 (insufficient)
  piton: n=0 (insufficient)
=== END TASK 5 ===

=== TASK 6: CROSS-DIAGNOSTIC CORRELATION ===
HIGH_ENTROPY_COUNT: 16 (threshold=0.4000)
OMEGA_HIGH_ENTROPY: 0/16 (0.00%)
OMEGA_LOW_ENTROPY: 0/1016 (0.00%)
BOLTZMANN_NC_HIGH_ENTROPY: 14/16 (87.50%)
BOLTZMANN_NC_LOW_ENTROPY: 847/1016 (83.37%)
LOW_PURITY_HIGH_ENTROPY: 9/16 (56.25%)
LOW_PURITY_LOW_ENTROPY: 437/1016 (43.01%)
PURITY_AVAILABLE_HIGH: 15/16
PURITY_AVAILABLE_LOW: 992/1016
AVG_PURITY_HIGH_ENTROPY: 0.5600222222222223
AVG_PURITY_LOW_ENTROPY: 0.5549153225806434
MULTI_ORBIT_HIGH_ENTROPY: 16/16 (100.00%)
MULTI_ORBIT_LOW_ENTROPY: 854/1016 (84.06%)
=== END TASK 6 ===

=== END DIAGNOSTIC OUTPUT ===
