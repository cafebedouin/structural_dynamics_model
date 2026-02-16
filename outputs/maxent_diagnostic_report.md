
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================
=== MAXENT DIAGNOSTIC OUTPUT ===
SUMMARY: NTotal=1021 MeanEntropy=0.191934 NHighUncertainty=16 NHard=163 NSoft=0

=== TASK 1: MISSING CONSTRAINTS ===
VISIBLE_CLAIMS: 1021
ALL_CLAIMS_RAW: 1023
LIST_FORM_CLAIMS: 1
  LIST_CLAIM: [wikipedia_crowdsourcing_2026]
NON_ATOM_CLAIMS: 0
TESTSET_FILES: 1024
EXPECTED_IDS: 1024
MISSING_FROM_CLAIMS: 136
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
CONSTRAINTS_WITH_DIST: 1021
VISIBLE_BUT_NO_DIST: 0
DET_TYPE_DISTRIBUTION:
  mountain: 125
  rope: 50
  snare: 518
  tangled_rope: 276
  unknown: 52
RESIDUAL_TYPE_COUNT: 52
=== END TASK 1 ===

=== TASK 2: PER-TYPE ENTROPY BREAKDOWN ===
TYPE_ENTROPY_TABLE:
Type|Count|Mean|Median|Min|Max|StdDev
mountain|125|0.155118|0.155706|0.035413|0.480241|0.041720
rope|50|0.201312|0.155706|0.008152|0.460579|0.118958
snare|518|0.237001|0.267631|0.007670|0.422959|0.130147
tangled_rope|276|0.134320|0.155706|0.001516|0.449291|0.105311
unknown|52|0.128282|0.087161|0.000000|0.507344|0.146822
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
ai_adoption_stigma|snare|tangled_rope|0.614094|0.627693|tangled_rope:0.614 snare:0.386
ai_performance_watermark|snare|tangled_rope|0.744848|0.682941|tangled_rope:0.745 snare:0.255
arctic_maritime_control|snare|tangled_rope|0.606789|0.625910|tangled_rope:0.607 snare:0.393
arg_ev_tariff|snare|tangled_rope|0.513877|0.613316|tangled_rope:0.514 snare:0.486
astm_d638_tensile_testing|rope|tangled_rope|0.545505|0.594430|rope:0.447 tangled_rope:0.546
atrophied_optimization_piton|tangled_rope|snare|0.929471|0.838109|tangled_rope:0.057 snare:0.929 piton:0.014
board_of_peace_2026|tangled_rope|snare|0.820956|0.714273|tangled_rope:0.168 snare:0.821 piton:0.011
boltzmann_universality_2026|rope|tangled_rope|0.700753|0.642668|rope:0.293 tangled_rope:0.701
brain_network_paradigm_2026|snare|tangled_rope|0.756051|0.689671|tangled_rope:0.756 snare:0.244
broke_vs_poor_grocery_math|snare|tangled_rope|0.551802|0.616148|tangled_rope:0.552 snare:0.448
cancer_prevention|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
carbon_credit_markets_2026|snare|tangled_rope|0.884652|0.800297|tangled_rope:0.885 snare:0.115
carrier_deployment_deterrence|snare|tangled_rope|0.513877|0.613316|tangled_rope:0.514 snare:0.486
carrying_capacity|snare|tangled_rope|0.680248|0.650238|tangled_rope:0.680 snare:0.320
china_ev_export_oversupply|snare|tangled_rope|0.704635|0.661245|tangled_rope:0.705 snare:0.295
china_vactrain_standard|snare|tangled_rope|0.744848|0.682941|tangled_rope:0.745 snare:0.255
climate_event_attribution|snare|tangled_rope|0.531184|0.614178|tangled_rope:0.531 snare:0.469
cn_tech_decoupling_security_software|snare|tangled_rope|0.548394|0.615727|tangled_rope:0.548 snare:0.452
codex_access|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
coffee_cardiovascular_2026|tangled_rope|rope|0.959098|0.889849|rope:0.959 tangled_rope:0.014 scaffold:0.026
cognac_geopolitical_risk|snare|tangled_rope|0.927364|0.854346|tangled_rope:0.927 snare:0.073
cognitive_bicycle_scaffold|tangled_rope|rope|0.906877|0.792469|rope:0.907 tangled_rope:0.036 scaffold:0.057
cognitive_induction_gap|snare|tangled_rope|0.680248|0.650238|tangled_rope:0.680 snare:0.320
colorado_sbe_decentralization_friction|snare|tangled_rope|0.669644|0.645914|tangled_rope:0.670 snare:0.330
constitutional_consecration|snare|tangled_rope|0.520430|0.613603|tangled_rope:0.520 snare:0.480
constitutional_supremacy|snare|tangled_rope|0.813912|0.731778|tangled_rope:0.814 snare:0.186
conversational_dogmas_interruption|snare|tangled_rope|0.877387|0.792202|tangled_rope:0.877 snare:0.123
couples_residency_match|snare|tangled_rope|0.861950|0.775881|tangled_rope:0.862 snare:0.138
credentialism_national_security|snare|tangled_rope|0.602560|0.624911|tangled_rope:0.603 snare:0.397
cultural_homogenization_social_media|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
data_privacy_regulation|snare|tangled_rope|0.606789|0.625910|tangled_rope:0.607 snare:0.393
debt_trap_microfinance|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
deferential_realism_framework|rope|tangled_rope|0.717282|0.631153|rope:0.265 tangled_rope:0.717 scaffold:0.017
dexy_gold_protocol|rope|tangled_rope|0.509024|0.557865|rope:0.466 tangled_rope:0.509 scaffold:0.025
digital_identity_tether|snare|tangled_rope|0.514607|0.613382|tangled_rope:0.515 snare:0.485
djia_as_economic_barometer|tangled_rope|snare|0.558016|0.595965|tangled_rope:0.435 snare:0.558
dn_paywall|snare|tangled_rope|0.625483|0.630791|tangled_rope:0.625 snare:0.374
dunning_kruger_effect|snare|tangled_rope|0.549964|0.615881|tangled_rope:0.550 snare:0.450
ec_meta_manus_block|snare|tangled_rope|0.548394|0.615727|tangled_rope:0.548 snare:0.452
edelman_2026_insularity|snare|tangled_rope|0.626238|0.631119|tangled_rope:0.626 snare:0.374
electrification_scale_2026|rope|tangled_rope|0.676109|0.629103|rope:0.317 tangled_rope:0.676
elliq_ai_companion|snare|tangled_rope|0.641855|0.635906|tangled_rope:0.642 snare:0.358
emergency_bridge_scaffold|snare|tangled_rope|0.878996|0.794000|tangled_rope:0.879 snare:0.121
ergo_lets_protocol|tangled_rope|rope|0.985000|0.948832|rope:0.985
eu_digital_services_act|snare|tangled_rope|0.531184|0.614178|tangled_rope:0.531 snare:0.469
eu_ev_tariff_wall|snare|tangled_rope|0.543200|0.615173|tangled_rope:0.543 snare:0.457
eu_unanimity_rule_foreign_policy|snare|tangled_rope|0.531184|0.614178|tangled_rope:0.531 snare:0.469
eurozone_fragmentation_2026|tangled_rope|snare|0.619730|0.627870|tangled_rope:0.380 snare:0.620
exploration_vs_exploitation|snare|tangled_rope|0.766564|0.696606|tangled_rope:0.767 snare:0.233
fcc_dji_covered_list|snare|tangled_rope|0.597531|0.623740|tangled_rope:0.598 snare:0.402
fda_component_efficacy_standard|snare|tangled_rope|0.543200|0.615173|tangled_rope:0.543 snare:0.457
fiber_optic_chip_tech|snare|tangled_rope|0.766564|0.696606|tangled_rope:0.767 snare:0.233
fine_particle_policy|snare|tangled_rope|0.606789|0.625910|tangled_rope:0.607 snare:0.393
fitts_law_industrial_application|snare|tangled_rope|0.516784|0.613438|tangled_rope:0.517 snare:0.483
fmt_oncology_2026|tangled_rope|rope|0.911016|0.797988|rope:0.911 tangled_rope:0.045 scaffold:0.044
g7_debt_trap|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
gaza_border_control_rafah|snare|tangled_rope|0.616741|0.628474|tangled_rope:0.617 snare:0.383
glen_canyon_water_allocation|snare|tangled_rope|0.641855|0.635906|tangled_rope:0.642 snare:0.358
global_digital_divide|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
goodharts_law|snare|tangled_rope|0.756051|0.689671|tangled_rope:0.756 snare:0.244
google_ai_search_overview|snare|tangled_rope|0.548394|0.615727|tangled_rope:0.548 snare:0.452
google_universal_commerce_protocol|snare|tangled_rope|0.614094|0.627693|tangled_rope:0.614 snare:0.386
great_awakening_rekindling|snare|tangled_rope|0.931882|0.860802|tangled_rope:0.932 snare:0.068
great_mongolian_road_economic_dependency|snare|tangled_rope|0.761964|0.693659|tangled_rope:0.762 snare:0.238
greenland_defence_pact_2026|snare|tangled_rope|0.814609|0.732193|tangled_rope:0.815 snare:0.185
greshams_law|snare|tangled_rope|0.509966|0.613233|tangled_rope:0.510 snare:0.490
guano_wealth_extraction|snare|tangled_rope|0.606789|0.625910|tangled_rope:0.607 snare:0.393
guinea_worm_eradication|rope|tangled_rope|0.880366|0.768463|rope:0.103 tangled_rope:0.880 scaffold:0.017
hammurabi_lex_talionis|snare|tangled_rope|0.653354|0.639680|tangled_rope:0.653 snare:0.347
hawthorne_effect|snare|tangled_rope|0.639970|0.635280|tangled_rope:0.640 snare:0.360
horizon_liability_contract|tangled_rope|snare|0.902647|0.821539|tangled_rope:0.097 snare:0.903
hu_2026_election_rules|snare|tangled_rope|0.645787|0.637215|tangled_rope:0.646 snare:0.354
hub_short_form_tv_market_fragmentation|snare|tangled_rope|0.704635|0.661245|tangled_rope:0.705 snare:0.295
india_nuclear_liability_act_2010|snare|tangled_rope|0.513877|0.613316|tangled_rope:0.514 snare:0.486
indian_import_tariffs_eu|snare|tangled_rope|0.543200|0.615173|tangled_rope:0.543 snare:0.457
inner_model_confirmation_bias|snare|tangled_rope|0.606789|0.625910|tangled_rope:0.607 snare:0.393
innovators_dilemma|snare|tangled_rope|0.634493|0.633504|tangled_rope:0.634 snare:0.365
iron_law_of_oligarchy|snare|tangled_rope|0.821383|0.738033|tangled_rope:0.821 snare:0.179
israel_norwegian_law|snare|tangled_rope|0.664426|0.643799|tangled_rope:0.664 snare:0.336
japanese_energy_scaffold_2025|snare|tangled_rope|0.884652|0.800297|tangled_rope:0.885 snare:0.115
job_hunt_volume_system_2026|snare|tangled_rope|0.610022|0.626769|tangled_rope:0.610 snare:0.390
jp_eez_enforcement|snare|tangled_rope|0.513524|0.613305|tangled_rope:0.514 snare:0.486
lindy_effect|rope|tangled_rope|0.826589|0.721340|rope:0.164 tangled_rope:0.827
lung_transplant_protocol|snare|tangled_rope|0.606789|0.625910|tangled_rope:0.607 snare:0.393
manganese_catalysis_2026|tangled_rope|rope|0.944086|0.860976|rope:0.944 tangled_rope:0.016 scaffold:0.040
max_flow_min_cut|snare|tangled_rope|0.936524|0.867678|tangled_rope:0.937 snare:0.063
medical_residency_match|snare|tangled_rope|0.677047|0.648896|tangled_rope:0.677 snare:0.323
mexican_airline_merger|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
mrna_melanoma_therapy|snare|tangled_rope|0.543200|0.615173|tangled_rope:0.543 snare:0.457
mvt_theorem_constraint|tangled_rope|mountain|0.635125|0.618015|mountain:0.635 rope:0.360
narrative_engineering_2026|tangled_rope|rope|0.947687|0.865234|rope:0.948 tangled_rope:0.025 scaffold:0.028
ncaa_eligibility_rules|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
neural_substrate_2026|snare|tangled_rope|0.619050|0.629047|tangled_rope:0.619 snare:0.381
nfl_superbowl_marketing_regulation|snare|tangled_rope|0.606789|0.625910|tangled_rope:0.607 snare:0.393
notary_ink_dependency|tangled_rope|snare|0.560008|0.550709|tangled_rope:0.407 snare:0.560 piton:0.033
omelet_perfection_complexity|rope|tangled_rope|0.936565|0.853868|rope:0.055 tangled_rope:0.937
openai_health_review|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
openai_implicit_translator|snare|tangled_rope|0.821794|0.738213|tangled_rope:0.822 snare:0.178
openai_prism_development|snare|tangled_rope|0.513877|0.613316|tangled_rope:0.514 snare:0.486
openbsd_netiquette_protocol|snare|tangled_rope|0.832209|0.747479|tangled_rope:0.832 snare:0.168
openclaw_data_lock_in|snare|tangled_rope|0.548180|0.615698|tangled_rope:0.548 snare:0.452
openclaw_regulation|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
oral_glp1_market_access|snare|tangled_rope|0.766564|0.696606|tangled_rope:0.767 snare:0.233
p_g_golden_pear_surveillance|snare|tangled_rope|0.606789|0.625910|tangled_rope:0.607 snare:0.393
perovskite_self_etching|snare|tangled_rope|0.814609|0.732193|tangled_rope:0.815 snare:0.185
peter_principle|snare|tangled_rope|0.877387|0.792202|tangled_rope:0.877 snare:0.123
poetic_verse_and_past|snare|tangled_rope|0.598464|0.624039|tangled_rope:0.598 snare:0.402
portugal_polarization_threshold_2026|snare|tangled_rope|0.814593|0.732369|tangled_rope:0.815 snare:0.185
private_credit_market_opacity|snare|tangled_rope|0.513877|0.613316|tangled_rope:0.514 snare:0.486
quantum_entanglement_protocol|rope|tangled_rope|0.853215|0.760136|rope:0.145 tangled_rope:0.853
quine_self_replication|mountain|rope|0.631998|0.519759|mountain:0.295 rope:0.632 tangled_rope:0.067
radiologic_diagnostic_threshold|snare|tangled_rope|0.669644|0.645914|tangled_rope:0.670 snare:0.330
rare_earth_export_restrictions|snare|tangled_rope|0.606789|0.625910|tangled_rope:0.607 snare:0.393
rfc9293_state_machine|rope|tangled_rope|0.854233|0.752565|rope:0.139 tangled_rope:0.854
rogue_wave_control_2026|tangled_rope|rope|0.840549|0.726327|rope:0.841 tangled_rope:0.144 scaffold:0.016
rules_based_international_order|snare|tangled_rope|0.598464|0.624039|tangled_rope:0.598 snare:0.402
sapir_whorf_hypothesis|snare|tangled_rope|0.634493|0.633504|tangled_rope:0.634 snare:0.365
scientific_paradigm_lifecycle|tangled_rope|snare|0.776207|0.690127|tangled_rope:0.219 snare:0.776
seedance_export_restriction|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
semiconductor_fabrication_chokepoint|snare|tangled_rope|0.548394|0.615727|tangled_rope:0.548 snare:0.452
shadow_fleet_sanctions_evasion|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
sk_ai_act_2026|snare|tangled_rope|0.513877|0.613316|tangled_rope:0.514 snare:0.486
skills_based_hiring|rope|tangled_rope|0.801207|0.680126|rope:0.174 tangled_rope:0.801 scaffold:0.025
slow_crisis_invisibility|snare|tangled_rope|0.581683|0.620627|tangled_rope:0.582 snare:0.418
sludge_bureaucratic_friction|snare|tangled_rope|0.680248|0.650238|tangled_rope:0.680 snare:0.320
sm_addictive_design|tangled_rope|snare|0.832064|0.747400|tangled_rope:0.168 snare:0.832
somatic_focusing_awareness|rope|tangled_rope|0.671079|0.605208|rope:0.309 tangled_rope:0.671 scaffold:0.019
south_china_sea_arbitration_2016_2026|snare|tangled_rope|0.771930|0.700221|tangled_rope:0.772 snare:0.228
start_treaty|snare|tangled_rope|0.606789|0.625910|tangled_rope:0.607 snare:0.393
sti_clinical_testing_bottleneck|snare|tangled_rope|0.669203|0.645569|tangled_rope:0.669 snare:0.331
strait_coercion_2025|snare|tangled_rope|0.535278|0.614530|tangled_rope:0.535 snare:0.465
strange_attractor_dynamics|snare|tangled_rope|0.936930|0.868333|tangled_rope:0.937 snare:0.063
strange_attractor_systemic_risk|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
strategic_deep_sea_rare_earth_mining|snare|tangled_rope|0.606789|0.625910|tangled_rope:0.607 snare:0.393
streaming_bundling_mandate|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
student_loan_default_cliff|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
student_loan_interest_accrual|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
sunk_cost_fallacy|snare|tangled_rope|0.934878|0.865197|tangled_rope:0.935 snare:0.065
taiwan_ids_program|snare|tangled_rope|0.531184|0.614178|tangled_rope:0.531 snare:0.469
taiwan_university_application_system|snare|tangled_rope|0.745858|0.683589|tangled_rope:0.746 snare:0.254
tcp_rfc9293_interoperability|rope|tangled_rope|0.879928|0.779489|rope:0.114 tangled_rope:0.880
texas_insurance_market_instability|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
traveling_salesperson_problem|snare|tangled_rope|0.669224|0.645716|tangled_rope:0.669 snare:0.331
trump_epa_greenhouse_gas_reversal|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
trump_indian_tariffs_2026|snare|tangled_rope|0.606789|0.625910|tangled_rope:0.607 snare:0.393
tx_hispanic_pivot|snare|tangled_rope|0.669644|0.645914|tangled_rope:0.670 snare:0.330
uk_help_to_buy_scheme|snare|tangled_rope|0.704635|0.661245|tangled_rope:0.705 snare:0.295
uk_hicbc_trap|snare|tangled_rope|0.623213|0.630190|tangled_rope:0.623 snare:0.377
ulysses_eumaeus_1904|tangled_rope|snare|0.787715|0.671530|tangled_rope:0.190 snare:0.788 piton:0.022
ulysses_ithaca_1904|tangled_rope|snare|0.762200|0.635502|tangled_rope:0.200 snare:0.762 piton:0.038
ulysses_lotus_1904|tangled_rope|snare|0.807276|0.688497|tangled_rope:0.171 snare:0.807 piton:0.022
ulysses_rocks_1904|tangled_rope|snare|0.836456|0.724533|tangled_rope:0.149 snare:0.836 piton:0.014
ulysses_tower_1904|tangled_rope|snare|0.654120|0.592661|tangled_rope:0.323 snare:0.654 piton:0.023
us_arms_transfer_policy|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
us_china_chip_tariffs_v2|snare|tangled_rope|0.597531|0.623740|tangled_rope:0.598 snare:0.402
us_employer_health_insurance|snare|tangled_rope|0.676333|0.648585|tangled_rope:0.676 snare:0.324
us_taiwan_arms_sales|snare|tangled_rope|0.575609|0.619485|tangled_rope:0.576 snare:0.424
us_usmca_china_leverage|snare|tangled_rope|0.531184|0.614178|tangled_rope:0.531 snare:0.469
us_visa_lottery|snare|tangled_rope|0.704635|0.661245|tangled_rope:0.705 snare:0.295
utopia_apocalypse_fragility|snare|tangled_rope|0.680248|0.650238|tangled_rope:0.680 snare:0.320
vns_implant_for_trd|snare|tangled_rope|0.548394|0.615727|tangled_rope:0.548 snare:0.452
wpl_scotland|snare|tangled_rope|0.814609|0.732193|tangled_rope:0.815 snare:0.185
yangtze_fishing_ban|snare|tangled_rope|0.557731|0.616845|tangled_rope:0.558 snare:0.442
ROPE_CLUSTER_ONLY: 161
INVOLVES_MTN_SCAFFOLD_PITON: 2
MOUNTAIN_PITON_DISAGREEMENTS:
  MTN_PITON: mvt_theorem_constraint det=tangled_rope shadow=mountain eps=0.1 supp=0.05 theater=0.1 sig=false_ci_rope dist=mountain:0.635 rope:0.360
  MTN_PITON: quine_self_replication det=mountain shadow=rope eps=0.2 supp=0.05 theater=0.01 sig=false_ci_rope dist=mountain:0.295 rope:0.632 tangled_rope:0.067
MEAN_SHADOW_TOP_P: 0.674046
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
INVERSE_COUNT: 599
absorbing_markov_chain_trap|snare|0.122086|scaffold/snare/unknown
abstraction_boundary_overrun|snare|0.070493|rope/snare
academic_fashion_modernism_2026|snare|0.029945|rope/snare
ad_fus_coordination|snare|0.248571|scaffold/snare
adaptive_lag_trap|snare|0.057422|rope/snare
adversarial_surface_inflation|snare|0.053372|rope/snare
adversarial_truth_decay|snare|0.032800|rope/snare
adverse_possession|tangled_rope|0.066999|rope/tangled_rope
advice_as_dangerous_gift|tangled_rope|0.016784|rope/tangled_rope/unknown
agency_atrophy|snare|0.037005|rope/snare
agent_opt_2026|snare|0.023846|rope/snare
aging_longevity_tests|tangled_rope|0.008191|rope/tangled_rope
ai_banal_capture|snare|0.165291|rope/snare
ai_driven_surveillance_sensor_layer|snare|0.174873|rope/snare
ai_edu_decentralization|tangled_rope|0.001516|rope/tangled_rope
ai_evaluators_matching|snare|0.163517|rope/snare
ai_nonconsensual_content_facilitation|snare|0.182234|rope/snare
ai_professional_displacement|snare|0.146369|rope/snare
ai_scholar_citation_trap|snare|0.070425|scaffold/snare/unknown
ai_superpowers_2026|snare|0.020628|scaffold/snare
ai_training_data_dependency|tangled_rope|0.215953|rope/tangled_rope
airport_slot_use_it_or_lose_it|snare|0.272087|rope/snare
algeria_france_colonial_legacy|snare|0.239949|rope/snare
algorithmic_bias|snare|0.139046|rope/snare
algorithmic_epistemic_capture|snare|0.053381|rope/snare
alignment_tax_tradeoff|snare|0.159664|rope/snare
altruistic_misery_paradox_2026|snare|0.198043|rope/snare
alzheimers_levetiracetam|tangled_rope|0.015074|rope/tangled_rope/unknown
alzheimers_nlrp3_inflammasome|snare|0.015686|scaffold/snare
amish_technological_renunciation|snare|0.237546|rope/snare
ancestral_pueblo_hydrology|snare|0.224262|scaffold/snare
anticipatory_capacity_failure|snare|0.055361|rope/snare
appropriations_brinkmanship|snare|0.167148|rope/snare
armra_colostrum_regulation|tangled_rope|0.061327|rope/tangled_rope
arrows_impossibility_theorem|tangled_rope|0.002471|rope/tangled_rope
art_market_decoupling|snare|0.041122|rope/snare
asce_7_22_seismic_design|tangled_rope|0.092878|rope/tangled_rope/unknown
asymmetric_burden_distribution|snare|0.045069|rope/snare
atrophied_optimization_piton|tangled_rope|0.161891|rope/tangled_rope
attention_as_bottleneck_resource|snare|0.048185|rope/snare
attention_market_cannibalization|snare|0.046430|rope/snare
australia_social_ban_2026|snare|0.175470|rope/snare
authoritarian_power_paradox|snare|0.109826|rope/snare
autonomous_toolchain_sprawl|snare|0.081711|rope/snare
availability_heuristic|tangled_rope|0.156935|rope/tangled_rope
average_is_over_2026|snare|0.241613|rope/snare
axiom_reasoner_2026|unknown|0.002405|scaffold/unknown
banach_fixed_point|mountain|0.155706|mountain/unknown
banach_fixed_point_theorem|mountain|0.155706|mountain/unknown
banach_tarski_paradox|mountain|0.155706|mountain/unknown
base_pair_complementarity|mountain|0.155706|mountain/unknown
bay_of_pigs_operational_silo|snare|0.048277|rope/snare
bayes_theorem_cognitive_bias|tangled_rope|0.016159|rope/tangled_rope
beehiiv_platform_model|tangled_rope|0.050267|rope/tangled_rope
belief_argument_conclusion|snare|0.074521|rope/snare
bgs_eigenvector_thermalization|tangled_rope|0.003667|rope/tangled_rope
bgs_spectral_universality|mountain|0.155706|mountain/unknown
bh_merger_gravitational_infall|mountain|0.155706|mountain/unknown
big_data_astrophysics_arbitrage|unknown|0.088775|rope/tangled_rope/unknown
biological_curiosity|mountain|0.055727|mountain/scaffold
bip_narrative_illusion|snare|0.218648|rope/snare
birthday_paradox_collision|mountain|0.155706|mountain/unknown
blackstone_carried_interest_taxation|tangled_rope|0.068727|rope/tangled_rope/unknown
blackstone_conflicts_of_interest|snare|0.272087|rope/snare
blackstone_smd_control|snare|0.291797|rope/snare
bnpl_payment_systems|tangled_rope|0.207780|rope/tangled_rope
board_of_peace_2026|tangled_rope|0.285727|rope/tangled_rope
bor_tax_exemption_nl|snare|0.182234|rope/snare
boundary_dissolution_risk|snare|0.044027|rope/snare
brazil_2026_general_elections|tangled_rope|0.033910|rope/tangled_rope
brazil_mexico_financial_requirement|snare|0.182234|rope/snare
brouwer_fixed_point|mountain|0.155706|mountain/unknown
buffons_needle_pi_estimation|tangled_rope|0.005065|rope/tangled_rope
burali_forti_paradox|mountain|0.090904|mountain/scaffold
burden_of_proof_engineering_safety|snare|0.192513|rope/snare
burden_of_proof_legal_criminal|rope|0.292876|rope/unknown
burden_of_proof_scientific|tangled_rope|0.207780|rope/tangled_rope
bureaucratic_legibility_collapse|snare|0.041730|rope/snare
bureaucratic_self_preservation|snare|0.064119|rope/snare
bushman_money_magic|snare|0.192513|rope/snare
busy_beaver_noncomputability|mountain|0.155706|mountain/unknown
c_physical_blue_wavelength|mountain|0.155706|mountain/unknown
cap_theorem|mountain|0.155706|mountain/unknown
capability_eval_overhang|snare|0.114675|rope/snare
capital_misallocation_spiral|snare|0.046996|rope/snare
capital_rotation_ai_narrative|tangled_rope|0.207780|rope/tangled_rope
carbon_credit_markets_2026|snare|0.199703|rope/snare/tangled_rope
cartel_drone_surveillance_el_paso|snare|0.254601|rope/snare
cascading_constraint_failure|snare|0.171213|rope/snare
cascading_uncertainty_2026|snare|0.080636|rope/snare
cbdc_implementation|snare|0.286677|rope/snare
central_limit_theorem|mountain|0.155706|mountain/unknown
chaitins_omega_undecidability|mountain|0.155706|mountain/unknown
challenger_o_ring_integrity|snare|0.286677|rope/snare
champions_bass_fishing_exclusion|tangled_rope|0.123229|rope/tangled_rope
child_marriage|snare|0.257393|rope/snare
china_critical_mineral_chokepoint|snare|0.265421|rope/snare
choice_architecture_design|tangled_rope|0.050267|rope/tangled_rope
church_turing_thesis|mountain|0.155706|mountain/unknown
citation_collapse_dynamics|snare|0.040240|rope/snare
civilizational_lifecycle_solara|snare|0.073225|rope/snare
civilizational_maintenance_debt|tangled_rope|0.003320|rope/tangled_rope
click_chemistry_paradigm_2026|mountain|0.035413|mountain/scaffold
climate_attribution_2026|mountain|0.138204|mountain/scaffold
cloudflare_dual_class_asymmetry|snare|0.291797|rope/snare
clt_convergence_2026|mountain|0.155706|mountain/unknown
cma|unknown|0.291462|rope/tangled_rope/unknown
cmr_001|tangled_rope|0.001891|rope/tangled_rope
coalition_disinfo_framework_2026|tangled_rope|0.215953|rope/tangled_rope
cobra_effect|tangled_rope|0.002278|rope/tangled_rope
cognac_geopolitical_risk|snare|0.145654|rope/snare/tangled_rope
coinbase_crypto_volatility|tangled_rope|0.229154|rope/tangled_rope
cold_dark_matter_paradigm|tangled_rope|0.240391|rope/tangled_rope
collatz_conjecture_determinism|mountain|0.155706|mountain/unknown
collective_action_deadlock|snare|0.155953|rope/snare
collective_stupidity_2026|snare|0.024205|rope/snare
college_admissions_market|unknown|0.087161|rope/tangled_rope/unknown
colombia_2026_presidential_election|unknown|0.003192|rope/unknown
comitatus_bond|tangled_rope|0.205412|rope/tangled_rope
communal_narcissism_social_trap|snare|0.108166|rope/snare
complexity_debt|snare|0.133703|rope/snare
compounding_logic|tangled_rope|0.002030|rope/tangled_rope
consensus_without_truth|snare|0.062871|rope/snare
constitutional_supremacy|snare|0.268222|rope/snare/tangled_rope
constraint_galois_solvability|mountain|0.155706|mountain/unknown
constraint_interaction_explosion|snare|0.170818|rope/snare
constraint_lagrange_multipliers|tangled_rope|0.015074|rope/tangled_rope/unknown
constraint_riemann_mapping|tangled_rope|0.014817|rope/tangled_rope/unknown
constraint_tarski_undefinability|mountain|0.155706|mountain/unknown
constraint_twin_prime_conjecture|mountain|0.155706|mountain/unknown
constraint_yoneda|tangled_rope|0.007063|rope/tangled_rope
container_capacity_mismatch|snare|0.053381|rope/snare
conversational_dogmas_interruption|snare|0.207798|rope/snare/tangled_rope
conways_game_of_life_dynamics|mountain|0.155706|mountain/unknown
coordination_attack_vulnerability|snare|0.041499|rope/snare
coordination_fatigue|snare|0.188140|rope/snare
cost_of_observation|unknown|0.007846|rope/unknown
countable_infinity_cardinality|mountain|0.155706|mountain/unknown
couples_residency_match|snare|0.224119|rope/snare/tangled_rope
cow_field_poop|tangled_rope|0.007063|rope/tangled_rope
creative_commons_licensing|tangled_rope|0.006755|rope/tangled_rope
credibility_inflation|snare|0.058334|rope/snare
crisis_signal_saturation|snare|0.035801|rope/snare
crispr_genomic_rewrite_2026|tangled_rope|0.006977|rope/tangled_rope
critical_actor_overcentralization|snare|0.035161|rope/snare
cross_domain_coupling_spiral|snare|0.038874|rope/snare
cs_ecmo_bridge|unknown|0.000810|rope/unknown
cuba_mandatrophic_collapse|snare|0.204642|rope/snare/tangled_rope
cultural_memory_decay|snare|0.130453|rope/snare
cultural_refragmentation_2026|snare|0.019211|rope/snare
cumbria_mine_rejection|snare|0.264272|rope/snare
cz_plea_agreement_2026|snare|0.182593|rope/snare
dark_patterns_manipulation|snare|0.270646|rope/snare
data_laundering_pipeline|snare|0.047313|rope/snare
data_replication_paradox|tangled_rope|0.294524|rope/tangled_rope
decision_latency_mismatch|unknown|0.236085|rope/tangled_rope/unknown
delayed_feedback_instability|snare|0.268995|rope/snare
delta_force_selection_2026|snare|0.050934|rope/snare
dionysiac_frenzy|snare|0.291797|rope/snare/tangled_rope
doomsday_clock_framework|snare|0.295342|rope/snare/tangled_rope
dutch_minority_govt_2026|tangled_rope|0.240391|rope/tangled_rope
dwp_carers_allowance_cliff|snare|0.270646|rope/snare
edelman_2026_developed_stagnation|snare|0.246616|rope/snare/tangled_rope
edelman_2026_developing_volatility|tangled_rope|0.008191|rope/tangled_rope
education_unbundling_implementation|unknown|0.010631|rope/unknown
ehrenfest_barrier|mountain|0.155706|mountain/unknown
elite_capture_2026|snare|0.022264|rope/snare
elite_identity_capture_2026|snare|0.038788|rope/snare
em_clinical_guidelines|tangled_rope|0.052891|rope/tangled_rope
emergency_bridge_scaffold|snare|0.206000|scaffold/snare/tangled_rope
emergency_mode_lock_in|snare|0.036815|rope/snare
emergency_oversight_bureau|unknown|0.084238|rope/scaffold/unknown
emergency_powers_ratchet|snare|0.067297|scaffold/snare
emergent_goal_misalignment|snare|0.052558|rope/snare
empty_tomb_transformation|tangled_rope|0.007230|rope/tangled_rope
endocrine_disruption_society|snare|0.135151|rope/snare
endowment_effect|mountain|0.108169|mountain/scaffold
english_chinese_tense_structure|mountain|0.155706|mountain/unknown
epistemic_free_rider_problem|snare|0.051873|rope/snare
epistemic_overload_collapse|snare|0.037248|rope/snare
epstein_espionage_2026|snare|0.025415|rope/snare
epstein_files_2026|snare|0.029104|rope/snare
epstein_honeytrap|snare|0.059895|rope/snare
erasmus_rejoining_scaffold|unknown|0.160696|rope/scaffold/unknown
ergo_nipopows|mountain|0.070866|mountain/scaffold
ergo_storage_rent|unknown|0.001209|rope/unknown
ergot_grain_poisoning|snare|0.286677|rope/snare
erised_expectation|snare|0.120571|rope/snare
eu_affordable_housing_initiative|tangled_rope|0.226786|rope/tangled_rope
eu_asylum_outsourcing_framework|snare|0.239949|rope/snare
eu_mercosur_trade_agreement|tangled_rope|0.215953|indexically_opaque/rope/tangled_rope
euler_characteristic_topology|mountain|0.155706|mountain/unknown
evfta_trade_agreement|tangled_rope|0.215953|rope/tangled_rope
evidence_half_life|snare|0.059267|rope/snare
evolutionary_mismatch_load|snare|0.217034|rope/snare
expert_disempowerment|snare|0.136405|rope/snare
extraordinary_narrative_shift|tangled_rope|0.003208|rope/tangled_rope
faa_boeing_regulatory_capture|tangled_rope|0.036868|rope/tangled_rope
factional_instability|unknown|0.084111|rope/tangled_rope/unknown
family_estrangement_ratio|snare|0.286677|rope/snare
family_succession_system|snare|0.291797|rope/snare
fast_growing_hierarchy|mountain|0.155706|mountain/unknown
feigenbaum_universality|mountain|0.155706|mountain/unknown
fermat_proof_barrier|mountain|0.155706|mountain/unknown
fgh_hierarchy_2026|mountain|0.155706|mountain/unknown
fiat_currency_lifecycle|snare|0.069273|rope/snare
financial_drag|snare|0.061213|rope/snare
finite_simple_group_classification|mountain|0.155706|mountain/unknown
fiscal_dominance_trap|snare|0.056313|rope/snare
floating_wall_2026|snare|0.299480|rope/snare
fmeca_procedures_1980|tangled_rope|0.010476|rope/tangled_rope
fnl_shadow_probe|tangled_rope|0.040408|rope/tangled_rope/unknown
four_color_theorem_topological_bound|mountain|0.155706|mountain/unknown
fragile_middle_layer_collapse|snare|0.054520|rope/snare
framing_effect|tangled_rope|0.031229|rope/tangled_rope
france_2027_presidential_election|unknown|0.000786|rope/unknown
france_local_elections_march_2026|tangled_rope|0.008591|scaffold/tangled_rope/unknown
franchisee_corporate_squeeze|tangled_rope|0.180607|rope/tangled_rope
frontex_pushback_coordination|snare|0.157532|rope/snare
fundamental_theorem_of_algebra|mountain|0.155706|mountain/unknown
gale_shapley|snare|0.224984|rope/snare
galois_theory_symmetry|mountain|0.094738|mountain/scaffold
gamblers_ruin_stochastic_extinction|unknown|0.269072|scaffold/unknown
gauss_bonnet_topology|mountain|0.155706|mountain/unknown
gaza_aid_permit_revocation|snare|0.264616|rope/snare/tangled_rope
gemini_scientific_advancement|tangled_rope|0.015074|rope/tangled_rope/unknown
generational_replacement_inertia|snare|0.258010|rope/snare
genetic_algorithms_evolution|mountain|0.086982|mountain/scaffold
genetic_predisposition|tangled_rope|0.044961|rope/tangled_rope
genetic_predisposition_mania|mountain|0.155706|mountain/unknown
genie_ip_constraint|snare|0.051927|scaffold/snare/unknown
geophysics_superionic_core|mountain|0.155706|mountain/unknown
germany_tennet_takeover|tangled_rope|0.019811|rope/tangled_rope/unknown
ghost_fishing_gear|snare|0.248571|rope/snare
gig_economy_algorithmic_management|snare|0.286677|rope/snare
global_economic_anxiety_2026|snare|0.016106|rope/snare
global_stimulus_spree|tangled_rope|0.017300|scaffold/tangled_rope
goal_boundary_poisoning|snare|0.039425|rope/snare
goedels_incompleteness_theorems|mountain|0.155706|mountain/unknown
gold_piton_2026|rope|0.038885|piton/rope
goldbach_conjecture|mountain|0.155706|mountain/unknown
goodstein_theorem_finite_proof|unknown|0.010649|rope/unknown
governance_latency_gap|snare|0.119801|rope/snare
governance_overfitting|snare|0.254245|rope/snare
gpt5_codex_dev_cycle|tangled_rope|0.008378|rope/tangled_rope
graph_coloring_complexity|tangled_rope|0.009843|rope/tangled_rope
great_awakening_rekindling|snare|0.139198|rope/snare/tangled_rope
greenland_defence_pact_2026|snare|0.267807|rope/snare/tangled_rope
greenland_seizure_trade_war|snare|0.218648|rope/snare
grete_samsa_transition|unknown|0.105138|rope/tangled_rope/unknown
grievance_stack_overflow|snare|0.048730|rope/snare
gs1_gln_identification|tangled_rope|0.069018|rope/tangled_rope/unknown
gs1_standardized_identification|unknown|0.011017|rope/unknown
gs_market_clearing|tangled_rope|0.014368|rope/tangled_rope/unknown
guinea_worm_eradication|rope|0.231537|rope/unknown
guthrie_kidnapping_2026|snare|0.073030|rope/snare
halting_problem_undecidability|mountain|0.155706|mountain/unknown
hanlons_razor|tangled_rope|0.001846|rope/tangled_rope
hasbro_licensing_restriction|tangled_rope|0.052880|rope/tangled_rope
hd101584_stellar_evolution|mountain|0.155706|mountain/unknown
hegemonic_entropy_2026|unknown|0.126185|rope/unknown
heine_borel|mountain|0.155706|mountain/unknown
heisenberg_uncertainty|mountain|0.155706|mountain/unknown
hershey_salt_strategy|tangled_rope|0.240391|rope/tangled_rope
heuristic_optimization|mountain|0.185075|mountain/scaffold
hhs_fetal_tissue_research_ban_2019|snare|0.182234|rope/snare
hidden_interdependency_risk|snare|0.089274|rope/snare
hilberts_hotel_infinity|mountain|0.051802|mountain/scaffold
hoa_covenants|unknown|0.009150|rope/unknown
hollow_state_syndrome|snare|0.038314|rope/snare
hp_liberalism|tangled_rope|0.083723|rope/tangled_rope
huang_expectation_resilience_2026|snare|0.132127|rope/snare
hydra_game|unknown|0.082046|rope/tangled_rope/unknown
hypercompression_of_time_horizons|snare|0.045398|rope/snare
hypernormie_equilibrium|snare|0.052674|rope/snare
ice_raids_minnesota_2026|snare|0.196753|rope/snare
ice_safe_departure|snare|0.232317|rope/snare
identity_stack_incompatibility|snare|0.054920|rope/snare
incentive_surface_warping|snare|0.040240|rope/snare
indexical_relativity_core|mountain|0.155706|mountain/unknown
india_semi_mission|tangled_rope|0.069358|rope/tangled_rope
individual_revolution_autonomy|snare|0.163517|rope/snare
indo_german_defense_pact|tangled_rope|0.240391|indexically_opaque/rope/tangled_rope
indonesia_penal_code_2023|snare|0.239949|indexically_opaque/rope/snare
inference_cost_scaling_law|snare|0.059326|rope/snare
information_foraging_theory|mountain|0.138204|mountain/scaffold
informational_time_2026|snare|0.279882|rope/snare
infrastructure_interoperability_decay|snare|0.039687|rope/snare
institutional_inertia_lock|tangled_rope|0.124916|rope/tangled_rope
institutional_memory_loss|snare|0.046838|rope/snare
institutional_mutation_domestication|snare|0.217871|rope/snare
institutional_mutation_without_selection|snare|0.035385|rope/snare
insult_wisdom_training|unknown|0.103111|rope/tangled_rope/unknown
interface_contract_breakdown|snare|0.058351|rope/snare
internet_evolution_lifecycle|snare|0.176039|rope/snare
interpretive_frame_fragmentation|snare|0.035977|rope/snare
intertemporal_responsibility_gap|snare|0.048273|rope/snare
invisible_infrastructure_dependency|snare|0.040250|rope/snare
iran_guardian_council_vetting|snare|0.246330|rope/snare
iran_hijab_law|snare|0.239949|rope/snare
iran_mandatrophic_collapse|snare|0.195070|rope/snare
iran_war_room_2026|snare|0.070714|rope/snare
iron_law_of_oligarchy|snare|0.261967|rope/snare/tangled_rope
irreversible_policy_commitment|snare|0.237722|rope/snare
isa_education_scaffold|tangled_rope|0.005257|scaffold/tangled_rope
israel_egypt_gas_deal|tangled_rope|0.207780|rope/tangled_rope
israel_electoral_threshold|tangled_rope|0.207780|rope/tangled_rope
israel_override_clause|snare|0.216201|rope/snare
israel_surplus_vote_agreements|tangled_rope|0.003228|rope/tangled_rope
israeli_settlement_policy_authority_restriction|snare|0.254244|rope/snare/tangled_rope
iterated_function_system_convergence|tangled_rope|0.005860|rope/tangled_rope
ivt_accessibility_barrier|tangled_rope|0.001891|rope/tangled_rope
japanese_energy_scaffold_2025|snare|0.199703|rope/snare/tangled_rope
jevons_paradox|tangled_rope|0.044961|rope/tangled_rope
jp_nativist_politics|snare|0.285583|indexically_opaque/rope/snare
jupiter_composition_knowledge_gap|tangled_rope|0.015074|rope/tangled_rope/unknown
keltner_relationship_evaluation|tangled_rope|0.014740|rope/tangled_rope/unknown
khantivadin_radical_patience|snare|0.261630|rope/snare
kirby_paris_theorem|mountain|0.155706|mountain/unknown
kjv_linguistic_residue|rope|0.021536|piton/rope
kjv_textual_authority|tangled_rope|0.153886|rope/tangled_rope
kleene_recursion_theorem|mountain|0.155706|mountain/unknown
kolmogorov_complexity|mountain|0.155706|mountain/unknown
labor_union_dues|tangled_rope|0.275893|rope/tangled_rope
landscape_of_fear_2026|tangled_rope|0.152253|rope/tangled_rope
latent_goal_activation|snare|0.039455|rope/snare
latent_regulatory_bomb|snare|0.246249|rope/snare
law_of_diminishing_returns|tangled_rope|0.007004|rope/tangled_rope
lcdm_small_scale_anomalies|tangled_rope|0.124984|rope/tangled_rope
legal_formalism_overhang|snare|0.043226|rope/snare
legibility_trap|snare|0.037005|rope/snare
legitimacy_without_capacity|snare|0.052314|rope/snare
legitimacy_without_effectiveness|snare|0.049671|rope/snare
lehman_repo_105|snare|0.208800|rope/snare
liar_paradox|mountain|0.155706|mountain/unknown
lindy_effect|rope|0.278660|rope/unknown
linguistic_relativity_cultural_framing|tangled_rope|0.036194|rope/tangled_rope
liquidity_illusion|snare|0.061380|rope/snare
litchfield_sensitive_locations_2026|snare|0.110797|rope/snare
lobs_theorem|mountain|0.155706|mountain/unknown
local_vs_global_optima|mountain|0.155706|mountain/unknown
lorenz_attractor_dynamics|tangled_rope|0.009712|rope/tangled_rope
lowenheim_skolem_theorem|mountain|0.155706|mountain/unknown
lsd_microdosing_professional_openness|tangled_rope|0.009780|rope/tangled_rope
lyapunov_stability|mountain|0.155706|mountain/unknown
magna_carta_liberties|tangled_rope|0.213988|rope/tangled_rope
maha_recovery_2026|unknown|0.023089|scaffold/unknown
maintenance_capacity_shortfall|snare|0.048300|rope/snare
maladaptive_selection_process|snare|0.045748|rope/snare
mandatrophic_margin_collapse|snare|0.191956|rope/snare
manga_distribution_duopoly|tangled_rope|0.294524|rope/tangled_rope
marriage_market_asymmetry_2026|snare|0.233037|rope/snare
mars_rover_navigational_autonomy|tangled_rope|0.010476|rope/tangled_rope
martian_signal_latency|mountain|0.155706|mountain/unknown
mass_market_extinction_2026|snare|0.200147|rope/snare
matching_market_congestion_externality|tangled_rope|0.013170|rope/tangled_rope/unknown
material_tensile_strength|mountain|0.155706|mountain/unknown
max_flow_min_cut|snare|0.132322|rope/snare/tangled_rope
med_diet_consensus_2026|tangled_rope|0.210739|rope/tangled_rope
memetic_fitness_vs_truth|snare|0.047068|rope/snare
meta_governance_overload|snare|0.037478|rope/snare
meta_model_lock_in|snare|0.043871|rope/snare
meta_nuclear_power_agreement|tangled_rope|0.028102|rope/tangled_rope
meta_pay_or_okay_model|snare|0.266060|indexically_opaque/rope/snare
migration_decision_threshold|tangled_rope|0.055736|rope/tangled_rope/unknown
mil_std_461g_emi_control|tangled_rope|0.002125|rope/tangled_rope
mil_std_810f_tailoring|tangled_rope|0.005947|rope/tangled_rope
milano_cortina_2026|rope|0.008152|piton/rope
minimax_theorem_game_equilibrium|tangled_rope|0.159486|rope/tangled_rope
minnesota_sovereignty_2026|snare|0.154850|rope/snare
mit_tfus_2026|snare|0.072320|scaffold/snare/unknown
model_autonomy_creep|snare|0.061603|rope/snare
model_collapse_feedback_loop|snare|0.046400|rope/snare
model_of_models_regression|snare|0.063287|rope/snare
moltbook_breach_2026|snare|0.042241|rope/snare
monetary_regime_transition|tangled_rope|0.001846|rope/tangled_rope
monty_hall_conditional_probability|mountain|0.155706|mountain/unknown
moores_law|tangled_rope|0.040776|rope/tangled_rope/unknown
moral_outsourcing|snare|0.046783|rope/snare
multi_agent_reward_hacking|snare|0.035192|rope/snare
naming_as_control|snare|0.117896|rope/snare
narcissistic_ego_maintenance|snare|0.222918|rope/snare
narrative_capacity_exhaustion|snare|0.039628|rope/snare
narrative_overfitting|snare|0.034345|rope/snare
nato_arctic_defense_cooperation|tangled_rope|0.009661|rope/tangled_rope
negative_emissions_arbitrage|unknown|0.297317|rope/tangled_rope/unknown
net_zero_stabilization|unknown|0.040225|rope/unknown
neural_interoperability|unknown|0.144152|rope/unknown
neurodiversity_spectrum|unknown|0.244925|rope/tangled_rope/unknown
news_paywall_inequality|tangled_rope|0.052880|rope/tangled_rope
nfl_superbowl_halftime_exclusivity|tangled_rope|0.061327|rope/tangled_rope
nine_day_buffer|snare|0.267571|rope/snare/tangled_rope
no_cloning_theorem|mountain|0.155706|mountain/unknown
noether_isomorphism_access|tangled_rope|0.002846|rope/tangled_rope
non_compete_agreements|snare|0.286677|rope/snare
norm_erosion_threshold|snare|0.068772|rope/snare
north_sea_wind_grid|tangled_rope|0.283977|rope/tangled_rope
nsw_transmission_bottleneck|snare|0.047876|scaffold/snare/unknown
nuclear_order_2026|snare|0.251896|rope/snare/tangled_rope
ny_private_school_discount|tangled_rope|0.052880|rope/tangled_rope
nyc_metrocard_art_licensing|tangled_rope|0.005146|rope/tangled_rope
olympic_legacy_curling_investment|tangled_rope|0.009552|rope/tangled_rope/unknown
olympic_medal_allocation|tangled_rope|0.013170|rope/tangled_rope/unknown
omelet_perfection_complexity|rope|0.146132|rope/unknown
openai_api_access|snare|0.077000|scaffold/snare/unknown
openai_codex_app_constraint|snare|0.070425|scaffold/snare/unknown
openai_implicit_translator|snare|0.261787|rope/snare/tangled_rope
openbsd_netiquette_protocol|snare|0.252521|rope/snare/tangled_rope
openscholar_peer_review|snare|0.060289|scaffold/snare/unknown
opioid_political_realignment_2026|snare|0.293326|rope/snare
optimization_fragility|snare|0.161201|rope/snare
orbital_data_center_2026|snare|0.143929|rope/snare
oscar_campaign_spending|tangled_rope|0.009661|rope/tangled_rope
other_peoples_troubles_2026|snare|0.058001|rope/snare
overfitting_to_frameworks|snare|0.121269|rope/snare
pancreatic_cancer_lethality_v1|mountain|0.155706|mountain/unknown
pareto_principle|mountain|0.155706|mountain/unknown
paris_municipal_reform_2026|unknown|0.001792|rope/unknown
parkinsons_law|tangled_rope|0.002125|rope/tangled_rope
participatory_observer_hypothesis|tangled_rope|0.012622|rope/tangled_rope
paxsilica_framework|tangled_rope|0.215953|rope/tangled_rope
pe_fund_level_leverage|snare|0.249987|rope/snare
perovskite_self_etching|snare|0.267807|scaffold/snare/tangled_rope
peter_principle|snare|0.207798|rope/snare/tangled_rope
planetary_boundaries|snare|0.224984|rope/snare
plastic_asphalt_mandate|tangled_rope|0.061327|rope/tangled_rope
platonic_coparenting_decoupling|unknown|0.002312|scaffold/unknown
pna|tangled_rope|0.003290|rope/tangled_rope
poincare_conjecture|mountain|0.155706|mountain/unknown
policy_lag_catastrophe|snare|0.048126|rope/snare
politeness_face_negotiation|tangled_rope|0.069358|rope/tangled_rope
portugal_ad_stability_2026|tangled_rope|0.008591|scaffold/tangled_rope/unknown
portugal_polarization_threshold_2026|snare|0.267631|rope/snare/tangled_rope
postman_survival_protocol|rope|0.114008|rope/unknown
power_set_axiomatic_extraction|tangled_rope|0.109750|rope/tangled_rope
power_without_responsibility|snare|0.036817|rope/snare
prestige_signal_inflation|snare|0.069816|rope/snare
price_signal_corruption|snare|0.059206|rope/snare
prime_number_theorem|mountain|0.155706|mountain/unknown
prisoners_dilemma_equilibrium|mountain|0.155706|mountain/unknown
private_identity_integration|tangled_rope|0.010438|rope/tangled_rope
procedural_compliance_theater|snare|0.124770|rope/snare/tangled_rope
procedural_legitimacy_decay|snare|0.158984|rope/snare
project_vault_extraction_2026|snare|0.084529|rope/snare
protocol_drift_accumulation|snare|0.064909|rope/snare
publishing_embargo|tangled_rope|0.013170|rope/tangled_rope/unknown
pythagorean_geometric_constancy|mountain|0.155706|mountain/unknown
quantum_entanglement_protocol|rope|0.239864|rope/unknown
quantum_measurement_gap|mountain|0.155706|mountain/unknown
quantum_nonlocality_2026|mountain|0.155706|mountain/unknown
quellcrist_falconer_justice|snare|0.192513|rope/snare
qwerty_vs_dvorak|tangled_rope|0.153886|rope/tangled_rope
ramsey_numbers|mountain|0.155706|mountain/unknown
rare_earth_hydrogen_extraction|tangled_rope|0.052880|rope/tangled_rope
rare_earth_seabed_mining|tangled_rope|0.018129|rope/tangled_rope
rational_inertia_trap|snare|0.117966|rope/snare
recipe_scaling_ai|unknown|0.002993|scaffold/unknown
reciprocity_laws_math|mountain|0.155706|mountain/unknown
regulatory_capture|unknown|0.009528|rope/unknown
relativity_of_simultaneity|mountain|0.155706|mountain/unknown
relativity_physical_invariance|mountain|0.155706|mountain/unknown
rent_seeking_equilibrium|snare|0.046783|rope/snare
reputational_cascade_failure|snare|0.044820|rope/snare
responsibility_dilution|snare|0.040652|rope/snare
responsibility_without_power|snare|0.035633|rope/snare
rfc9293_state_machine|rope|0.247435|rope/unknown
rices_theorem_undecidability|mountain|0.155706|mountain/unknown
riot_incentive_loop_2026|snare|0.048949|rope/snare
risk_socialization_threshold|snare|0.052987|rope/snare
ritual_transition_scaffold|tangled_rope|0.137989|rope/tangled_rope/unknown
ritual_without_belief|snare|0.034360|rope/snare
robustness_vs_efficiency_tradeoff|snare|0.156670|rope/snare
rosen_bridge_protocol|unknown|0.003089|rope/unknown
rotation_seven_black_soil|unknown|0.000000|rope/unknown
rotation_seven_isolation|snare|0.265421|rope/snare
royal_navy_middle_east_withdrawal|unknown|0.134673|rope/tangled_rope/unknown
russells_paradox_self_reference|unknown|0.109495|scaffold/unknown
russian_war_cannibalization|snare|0.233656|rope/snare
sa_renewable_price_differential|tangled_rope|0.028102|rope/tangled_rope
sadhu_integrity_protocol|tangled_rope|0.053209|rope/tangled_rope/unknown
sat_csp_complexity|tangled_rope|0.009843|rope/tangled_rope
satellite_d2m_standard|tangled_rope|0.008549|rope/tangled_rope
scam_compound_2026|snare|0.097273|rope/snare
scurvy_maritime_extraction|snare|0.252515|rope/snare
second_order_unintended_consequences|snare|0.048300|rope/snare
semantic_attack_surface|snare|0.037463|rope/snare
semiconductor_mission_2026|tangled_rope|0.061327|rope/tangled_rope
shadow_pricing_failure|snare|0.060814|rope/snare
shannon_entropy_limit|mountain|0.155706|mountain/unknown
shannon_source_coding|mountain|0.155706|mountain/unknown
ship_of_theseus|unknown|0.259208|rope/unknown
shitty_feedback_handling|unknown|0.010114|rope/unknown
shobies_existential_commitment|tangled_rope|0.063414|rope/tangled_rope
shock_propagation_asymmetry|snare|0.042786|rope/snare
signal_without_control|snare|0.202863|rope/snare
silent_dependency_activation|snare|0.245034|rope/snare
silicon_lexicon_overload|snare|0.286677|rope/snare
silver_scarcity_2026|mountain|0.155706|mountain/unknown
skolems_paradox|mountain|0.185075|mountain/scaffold
sleep_debt_externality|snare|0.289014|rope/snare
smartphone_ubiquity|unknown|0.105138|rope/tangled_rope/unknown
social_credit_architecture|snare|0.280723|rope/snare
social_loafing|tangled_rope|0.007685|rope/tangled_rope
social_media_participation_threshold|tangled_rope|0.046702|rope/tangled_rope
social_narrative_casting|tangled_rope|0.067637|rope/tangled_rope
soft_authoritarian_drift|snare|0.053372|rope/snare
sorites_paradox|tangled_rope|0.152253|rope/tangled_rope
south_china_sea_arbitration_2016_2026|snare|0.299779|rope/snare/tangled_rope
spain_digital_offensive_2026|snare|0.202302|rope/snare
square_cube_law|mountain|0.155706|mountain/unknown
st_petersburg_paradox|tangled_rope|0.003159|rope/tangled_rope
stable_marriage_coordination|tangled_rope|0.006634|rope/tangled_rope
star_formation_barrier_g0253|mountain|0.155706|mountain/unknown
star_to_black_hole_observational_limit|mountain|0.155706|mountain/unknown
status_flattening_effect|snare|0.068639|rope/snare
strange_attractor_dynamics|snare|0.131667|rope/snare/tangled_rope
structural_extraction_without_actor|snare|0.116092|rope/snare
sturgeons_law|mountain|0.155706|mountain/unknown
suanne_coup_of_peace|snare|0.290017|rope/snare
sunk_cost_fallacy|snare|0.134803|rope/snare/tangled_rope
suslin_hypothesis_undecidability|mountain|0.155706|mountain/unknown
sylow_theorems_group_theory|mountain|0.155706|mountain/unknown
synthetic_data_feedback_loop|snare|0.057153|rope/snare
systemic_blindspot|snare|0.066311|rope/snare
tail_risk_compression|snare|0.078248|rope/snare
taiwan_existential_sovereignty|snare|0.286821|rope/snare
taiwan_grand_bargain|snare|0.280102|rope/snare
taiwan_storm_2026|snare|0.134409|rope/snare
taxonomy_drift|snare|0.170757|rope/snare
tcp_rfc9293_interoperability|rope|0.220511|rope/unknown
teaching_horses_to_sing|unknown|0.005637|rope/tangled_rope/unknown
tear_gas_repression_2026|snare|0.047115|rope/snare
technocratic_overreach|snare|0.295546|rope/snare
temporal_scale_arbitrage|unknown|0.005903|rope/tangled_rope/unknown
terrain_inaccessibility_wheeled_vehicles|mountain|0.155706|mountain/unknown
thai_senate_veto_2026|snare|0.064119|rope/snare
the_bacchae_madness_protocol|snare|0.204642|rope/snare
the_calm_protocol_suppression|snare|0.254601|rope/snare
theory_of_visitors|tangled_rope|0.063414|rope/tangled_rope
thermodynamics_entropy|mountain|0.155706|mountain/unknown
three_body_unpredictability|mountain|0.155706|mountain/unknown
toxic_social_infection|snare|0.252515|rope/snare
toxoplasma_hub_2026|snare|0.007670|scaffold/snare
trade_secret_law|tangled_rope|0.240391|rope/tangled_rope
tragedy_of_the_commons|unknown|0.004327|rope/tangled_rope/unknown
transformer_self_attention|unknown|0.005505|rope/tangled_rope/unknown
transient_event_detection|tangled_rope|0.012856|rope/tangled_rope
trillion_bond_rush_2026|unknown|0.088689|scaffold/unknown
trivial_topology_info_asymmetry|tangled_rope|0.052880|rope/tangled_rope
trump_critical_minerals|tangled_rope|0.184617|rope/tangled_rope
trump_making_china_great_2026|tangled_rope|0.003290|rope/tangled_rope
trump_second_term_authoritarianism_2026|snare|0.270646|rope/snare/tangled_rope
tsp_computational_complexity|tangled_rope|0.103988|rope/tangled_rope
tsp_duplicate_elimination|rope|0.273673|rope/unknown
ua_wartime_mobilization|snare|0.270646|rope/snare
uk_graduate_visa_salary_threshold|snare|0.182234|rope/snare
uk_necc_formation|tangled_rope|0.202142|rope/tangled_rope
uk_unpaid_care_system|snare|0.255949|rope/snare
ukraine_tight_gas_pilot|tangled_rope|0.207780|rope/tangled_rope
ulysses_aeolus_1904|snare|0.212865|rope/snare/tangled_rope
ulysses_cyclops_1904|snare|0.161188|rope/snare/tangled_rope
ulysses_lestrygonians_1904|snare|0.263096|rope/snare/tangled_rope
ulysses_nausicaa_1904|snare|0.221976|rope/snare/tangled_rope
ulysses_rocks_1904|tangled_rope|0.275467|rope/tangled_rope
ulysses_scylla_1904|snare|0.240539|rope/snare/tangled_rope
ulysses_sirens_1904|snare|0.156355|rope/snare/tangled_rope
un_high_seas_treaty_2026|tangled_rope|0.023605|rope/tangled_rope
unclos_2026|unknown|0.122438|rope/unknown
union_protection_underperformance|tangled_rope|0.046702|rope/tangled_rope
unrequited_love_protocol|snare|0.013575|scaffold/snare
unrwa_eviction_order|snare|0.198088|rope/snare
us_greenland_envoy|snare|0.279099|rope/snare
us_israel_faa_502b_nonenforcement|snare|0.105580|rope/snare
us_labor_mobility|tangled_rope|0.029797|rope/tangled_rope
us_sdf_alliance_abandonment_2026|snare|0.279099|rope/snare
us_two_party_duopoly|snare|0.248571|rope/snare
us_vaccine_recommendation_dismantling_2026|snare|0.182234|rope/snare
us_venezuela_oil_pressure|snare|0.252600|rope/snare
us_venezuela_plausible_deniability_2025|snare|0.279366|rope/snare
value_alignment_drift|snare|0.273968|rope/snare
value_extraction_plateau|snare|0.032800|rope/snare
van_der_waerden_theorem|mountain|0.155706|mountain/unknown
venezuela_oil_privatization_v1|snare|0.182234|rope/snare
viral_emergence_covid19_exemplar|rope|0.191250|piton/rope
viral_transmission_rates|snare|0.217871|rope/snare
visa_ipo_regulatory_compliance|tangled_rope|0.223991|rope/tangled_rope
visa_judgment_sharing_agreement|tangled_rope|0.155837|rope/tangled_rope
visibility_bias_governance|snare|0.284318|rope/snare/tangled_rope
weierstrass_proof_limits|mountain|0.155706|mountain/unknown
whitehead_problem_undecidability|mountain|0.155706|mountain/unknown
wikipedia_notability_requirement_2026|tangled_rope|0.149910|rope/tangled_rope
working_dog_training|tangled_rope|0.015638|rope/tangled_rope/unknown
world_factbook_sunset_2026|snare|0.096979|rope/snare
wpl_scotland|snare|0.267807|rope/snare/tangled_rope
xi_mao_ideological_centralization|snare|0.067569|rope/snare
yc_equity_squeeze|tangled_rope|0.052880|rope/tangled_rope
yt_ai_slop_incentive|snare|0.239949|rope/snare
zipfs_law|snare|0.125695|rope/snare
zombie_reasoning_2026|snare|0.028066|rope/snare
=== END TASK 4 ===

=== TASK 5: GAUSSIAN PROFILES ===
EMPIRICAL_PROFILES:
Type|Metric|Mu|Sigma
mountain|extractiveness|0.082240|0.059057
mountain|suppression|0.029520|0.019094
mountain|theater|0.027600|0.054676
rope|extractiveness|0.120400|0.070625
rope|suppression|0.382000|0.284239
rope|theater|0.182000|0.242289
tangled_rope|extractiveness|0.478877|0.180705
tangled_rope|suppression|0.571486|0.200657
tangled_rope|theater|0.241739|0.228217
snare|extractiveness|0.701023|0.131478
snare|suppression|0.754208|0.088211
snare|theater|0.422529|0.304536
scaffold|extractiveness|0.200000|0.120000
scaffold|suppression|0.380000|0.200000
scaffold|theater|0.140000|0.120000
piton|extractiveness|0.650000|0.150000
piton|suppression|0.690000|0.150000
piton|theater|0.850000|0.080000
LARGE_SIGMA_FLAGS:
  LARGE_SIGMA: rope suppression sigma=0.284239
  LARGE_SIGMA: snare theater sigma=0.304536
ROPE_EPS_DISTRIBUTION:
ROPE_EPS_COUNT: 50
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
  ROPE_EPS: 0.250000
  ROPE_EPS: 0.250000
  ROPE_EPS: 0.250000
  ROPE_EPS: 0.250000
ROPE_EPS_BINS: [0,0.25]=46 [0.25,0.50]=4 [0.50,1.0]=0
OVERRIDE_ROPE_ANALYSIS:
OVERRIDE_ROPE_COUNT: 28
  OVERRIDE_ROPE: asean_ceasefire_2011 sig=coupling_invariant_rope eps=0.150000 hn=0.155706
  OVERRIDE_ROPE: automatic_enrollment_defaults sig=constructed_low_extraction eps=0.050000 hn=0.044927
  OVERRIDE_ROPE: berkshire_compounding_culture sig=constructed_low_extraction eps=0.100000 hn=0.084559
  OVERRIDE_ROPE: boundary_protocol sig=coupling_invariant_rope eps=0.000000 hn=0.155706
  OVERRIDE_ROPE: cancer_chronotherapy_timing sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: cinderella_midnight_deadline sig=coupling_invariant_rope eps=0.100000 hn=0.155706
  OVERRIDE_ROPE: copyleft_viral_licensing sig=constructed_low_extraction eps=0.050000 hn=0.077612
  OVERRIDE_ROPE: cuny_light_2026 sig=constructed_low_extraction eps=0.050000 hn=0.032324
  OVERRIDE_ROPE: decentralized_infrastructure_rope sig=constructed_low_extraction eps=0.080000 hn=0.086025
  OVERRIDE_ROPE: fair_use_doctrine sig=constructed_low_extraction eps=0.100000 hn=0.170814
  OVERRIDE_ROPE: ice_memory_archive sig=coupling_invariant_rope eps=0.100000 hn=0.155706
  OVERRIDE_ROPE: microrobot_manipulation sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: mom_z14_2026 sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: open_source_commons sig=coupling_invariant_rope eps=0.050000 hn=0.155706
  OVERRIDE_ROPE: perseverance_rover_autonomy sig=coupling_invariant_rope eps=0.100000 hn=0.155706
  OVERRIDE_ROPE: planetary_diet_constraint_2026 sig=constructed_low_extraction eps=0.100000 hn=0.247945
  OVERRIDE_ROPE: portuguese_presidential_term_limits sig=constructed_low_extraction eps=0.020000 hn=0.184798
  OVERRIDE_ROPE: rafah_crossing_lifeline sig=coupling_invariant_rope eps=0.150000 hn=0.155706
  OVERRIDE_ROPE: rare_earth_coop_2026 sig=constructed_low_extraction eps=0.020000 hn=0.070635
  OVERRIDE_ROPE: spv_variations_us_cold sig=coupling_invariant_rope eps=0.020000 hn=0.155706
  OVERRIDE_ROPE: sts86_ascent_checklist sig=constructed_low_extraction eps=0.050000 hn=0.178399
  OVERRIDE_ROPE: swift_piton_snap sig=coupling_invariant_rope eps=0.040000 hn=0.155706
  OVERRIDE_ROPE: thai_article_112_mountain sig=constructed_low_extraction eps=0.040000 hn=0.192073
  OVERRIDE_ROPE: udhr_1948 sig=constructed_low_extraction eps=0.100000 hn=0.087950
  OVERRIDE_ROPE: vertebrate_turning_point_2026 sig=constructed_low_extraction eps=0.050000 hn=0.138281
  OVERRIDE_ROPE: viral_emergence_covid19_exemplar sig=constructed_low_extraction eps=0.150000 hn=0.191250
  OVERRIDE_ROPE: wikipedia_crowdsourcing_2026 sig=constructed_low_extraction eps=0.050000 hn=0.142424
  OVERRIDE_ROPE: wikipedia_noncommercial_model sig=coupling_invariant_rope eps=0.120000 hn=0.155706
NON_OVERRIDE_ROPE_ENTROPY_MEAN: 0.277791 (n=22)
OVERRIDE_ROPE_ENTROPY_MEAN: 0.141221 (n=28)
ALL_TYPE_EPS_STATS:
  mountain: n=125 mean=0.082240 std=0.059057 min=0.000000 max=0.200000
  rope: n=50 mean=0.120400 std=0.070625 min=0.000000 max=0.250000
  tangled_rope: n=276 mean=0.478877 std=0.180705 min=0.000000 max=1.000000
  snare: n=518 mean=0.701023 std=0.131478 min=0.490000 max=1.000000
  scaffold: n=0 (insufficient)
  piton: n=0 (insufficient)
=== END TASK 5 ===

=== TASK 6: CROSS-DIAGNOSTIC CORRELATION ===
HIGH_ENTROPY_COUNT: 16 (threshold=0.4000)
OMEGA_HIGH_ENTROPY: 0/16 (0.00%)
OMEGA_LOW_ENTROPY: 0/1005 (0.00%)
BOLTZMANN_NC_HIGH_ENTROPY: 13/16 (81.25%)
BOLTZMANN_NC_LOW_ENTROPY: 835/1005 (83.08%)
LOW_PURITY_HIGH_ENTROPY: 8/16 (50.00%)
LOW_PURITY_LOW_ENTROPY: 433/1005 (43.08%)
PURITY_AVAILABLE_HIGH: 14/16
PURITY_AVAILABLE_LOW: 978/1005
AVG_PURITY_HIGH_ENTROPY: 0.5515476190476191
AVG_PURITY_LOW_ENTROPY: 0.5533133946830253
MULTI_ORBIT_HIGH_ENTROPY: 16/16 (100.00%)
MULTI_ORBIT_LOW_ENTROPY: 846/1005 (84.18%)
=== END TASK 6 ===

=== END DIAGNOSTIC OUTPUT ===
