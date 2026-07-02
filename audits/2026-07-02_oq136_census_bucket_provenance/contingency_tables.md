# OQ-136 contingency tables (raw counts — no rates; see PROPOSAL.md)

git `0ba48b4c6dc9` dirty=True; corpus n=119; seed=20260702; Holm family size=8

## q6_unmeasured (n=26, POWERED)

### axis: model (permutation_chi2_N10000, K=5)
| stratum | in-bucket | rest |
|---|---|---|
| claude-haiku-4-5-20251001 | 16 | 12 |
| claude-sonnet-4-20250514 | 0 | 7 |
| claude-sonnet-4-5-20250929 | 1 | 63 |
| gemini-2.5-flash | 0 | 11 |
| provenance_unauthored | 9 | 0 |

p_raw=9.999e-05, p_holm=0.0007999, clustered=True; most-enriched=provenance_unauthored (members=9, ratio=inf); enrichment gates pass=True

### axis: prompt_commit (permutation_chi2_N10000, K=4)
| stratum | in-bucket | rest |
|---|---|---|
| 046e0a40c34cddf4fff29b8c15f632dbdef31b7a | 0 | 12 |
| 22843cdfd28a814d8f30c35778e75821452545bd | 17 | 57 |
| 8080348c4e16a265fafc924dcde83360dfd170fc | 0 | 24 |
| provenance_unauthored | 9 | 0 |

p_raw=9.999e-05, p_holm=0.0007999, clustered=True; most-enriched=provenance_unauthored (members=9, ratio=inf); enrichment gates pass=True

### axis: topic_family (permutation_chi2_N10000, K=79)
| stratum | in-bucket | rest |
|---|---|---|
| actinide_replenishment_mechanism | 1 | 3 |
| adjunctification_of_university_teaching | 0 | 1 |
| alignment_constraint_narrowing | 0 | 1 |
| animal_status_kernel | 1 | 0 |
| apoe4_mitochondrial_vulnerability | 0 | 1 |
| architectural_pattern_validity | 0 | 1 |
| basic_law_interpretive_authority | 1 | 0 |
| basic_law_interpretive_boundary | 0 | 1 |
| behavioral_adoption_friction | 0 | 1 |
| benchmark_saturation_interpretation | 0 | 1 |
| bitcoin_whitepaper_purpose | 1 | 0 |
| border_control_legitimacy | 1 | 0 |
| capability_endogeneity | 0 | 1 |
| catastrophe_memory_kernel | 0 | 1 |
| dataset_recycling_amplification | 0 | 1 |
| demographic_resource_allocation | 0 | 1 |
| demographic_skill_mismatch | 0 | 1 |
| digital_money_legitimacy | 1 | 4 |
| divine_legitimacy_substrate | 1 | 0 |
| doomsday_clock_metric | 0 | 1 |
| dueling_disappearance_mechanism | 0 | 1 |
| empty_shell_tolerance | 0 | 1 |
| equal_protection_kernel | 1 | 0 |
| fourteenth_amendment_equal_protection | 1 | 0 |
| gendered_outcome_asymmetry | 0 | 1 |
| generality_standard | 1 | 4 |
| geopolitical_settlement_competition | 0 | 1 |
| gita_kurukshetra_discourse | 0 | 1 |
| historical_treaty_substrate | 0 | 1 |
| institutional_barrier_structure | 0 | 1 |
| institutional_trust_erosion | 0 | 1 |
| institutional_verification_collapse | 0 | 1 |
| intervention_target_selection | 0 | 1 |
| jewish_self_determination | 1 | 0 |
| jewish_sovereignty_palestine | 0 | 2 |
| jewish_territorial_claim | 0 | 1 |
| john_1_1_logos | 0 | 1 |
| knowledge_legitimacy_biomedicine | 1 | 3 |
| lausanne_minority_protections | 1 | 0 |
| learning_difficulty_substrate | 1 | 4 |
| llm_synthesis_capacity | 0 | 1 |
| longevity_mismatch | 0 | 1 |
| lycurgan_laws | 0 | 1 |
| maat_order_principle | 1 | 0 |
| marriage_authority | 1 | 0 |
| mitochondrial_demand_signal_deficiency | 0 | 1 |
| model_collapse_feedback | 0 | 1 |
| moral_causation_locus | 1 | 4 |
| nad_precursor_bioavailability | 0 | 1 |
| nicene_creed_authority | 1 | 0 |
| organization_floor | 0 | 1 |
| performance_legitimacy | 1 | 5 |
| polaris_document_status | 2 | 4 |
| press_reformation_causality | 0 | 1 |
| press_reformation_causation | 0 | 2 |
| propagation_speed_asymmetry | 0 | 1 |
| property_sector_overhang | 0 | 1 |
| protein_anabolic_resistance | 0 | 1 |
| radiative_levitation_stratification | 0 | 1 |
| reading_acquisition_legitimacy | 0 | 1 |
| refugee_convention_text | 0 | 1 |
| responsibility_preservation_mechanism | 0 | 1 |
| scale_ceiling | 0 | 1 |
| secession_legitimacy_boundary | 1 | 0 |
| sex_gender_category | 0 | 1 |
| shinbutsu_ontological_commitment | 1 | 0 |
| specification_binding_authority | 0 | 1 |
| speech_protection_kernel | 1 | 0 |
| state_killing_legitimacy | 0 | 1 |
| statutory_debt_ceiling | 0 | 1 |
| synthesis_infrastructure_gap | 0 | 1 |
| third_act_arbitrage | 0 | 1 |
| udhr_article_3 | 0 | 1 |
| validation_judgment_separation | 0 | 1 |
| vedic_corpus_social_prescription | 0 | 1 |
| visual_evidentiary_authority | 1 | 5 |
| womens_financial_autonomy | 0 | 1 |
| zero_mathematical_status | 1 | 1 |
| zionist_legitimacy_basis | 0 | 1 |

p_raw=0.8609, descriptive_clustered=False; most-enriched=polaris_document_status (members=2, ratio=1.79); enrichment gates pass=False

## q6_signature_unknown (n=16, POWERED)

### axis: model (permutation_chi2_N10000, K=5)
| stratum | in-bucket | rest |
|---|---|---|
| claude-haiku-4-5-20251001 | 1 | 27 |
| claude-sonnet-4-20250514 | 1 | 6 |
| claude-sonnet-4-5-20250929 | 13 | 51 |
| gemini-2.5-flash | 1 | 10 |
| provenance_unauthored | 0 | 9 |

p_raw=0.1484, p_holm=0.5935, clustered=False; most-enriched=claude-sonnet-4-5-20250929 (members=13, ratio=1.64); enrichment gates pass=False

### axis: prompt_commit (permutation_chi2_N10000, K=4)
| stratum | in-bucket | rest |
|---|---|---|
| 046e0a40c34cddf4fff29b8c15f632dbdef31b7a | 3 | 9 |
| 22843cdfd28a814d8f30c35778e75821452545bd | 8 | 66 |
| 8080348c4e16a265fafc924dcde83360dfd170fc | 5 | 19 |
| provenance_unauthored | 0 | 9 |

p_raw=0.2372, p_holm=0.5935, clustered=False; most-enriched=046e0a40c34cddf4fff29b8c15f632dbdef31b7a (members=3, ratio=2.15); enrichment gates pass=True

### axis: topic_family (permutation_chi2_N10000, K=79)
| stratum | in-bucket | rest |
|---|---|---|
| actinide_replenishment_mechanism | 1 | 3 |
| adjunctification_of_university_teaching | 0 | 1 |
| alignment_constraint_narrowing | 0 | 1 |
| animal_status_kernel | 0 | 1 |
| apoe4_mitochondrial_vulnerability | 0 | 1 |
| architectural_pattern_validity | 0 | 1 |
| basic_law_interpretive_authority | 0 | 1 |
| basic_law_interpretive_boundary | 0 | 1 |
| behavioral_adoption_friction | 1 | 0 |
| benchmark_saturation_interpretation | 0 | 1 |
| bitcoin_whitepaper_purpose | 0 | 1 |
| border_control_legitimacy | 0 | 1 |
| capability_endogeneity | 1 | 0 |
| catastrophe_memory_kernel | 0 | 1 |
| dataset_recycling_amplification | 1 | 0 |
| demographic_resource_allocation | 0 | 1 |
| demographic_skill_mismatch | 0 | 1 |
| digital_money_legitimacy | 1 | 4 |
| divine_legitimacy_substrate | 0 | 1 |
| doomsday_clock_metric | 1 | 0 |
| dueling_disappearance_mechanism | 0 | 1 |
| empty_shell_tolerance | 0 | 1 |
| equal_protection_kernel | 0 | 1 |
| fourteenth_amendment_equal_protection | 0 | 1 |
| gendered_outcome_asymmetry | 0 | 1 |
| generality_standard | 0 | 5 |
| geopolitical_settlement_competition | 0 | 1 |
| gita_kurukshetra_discourse | 1 | 0 |
| historical_treaty_substrate | 0 | 1 |
| institutional_barrier_structure | 0 | 1 |
| institutional_trust_erosion | 1 | 0 |
| institutional_verification_collapse | 0 | 1 |
| intervention_target_selection | 0 | 1 |
| jewish_self_determination | 0 | 1 |
| jewish_sovereignty_palestine | 0 | 2 |
| jewish_territorial_claim | 0 | 1 |
| john_1_1_logos | 0 | 1 |
| knowledge_legitimacy_biomedicine | 1 | 3 |
| lausanne_minority_protections | 0 | 1 |
| learning_difficulty_substrate | 1 | 4 |
| llm_synthesis_capacity | 0 | 1 |
| longevity_mismatch | 0 | 1 |
| lycurgan_laws | 0 | 1 |
| maat_order_principle | 0 | 1 |
| marriage_authority | 0 | 1 |
| mitochondrial_demand_signal_deficiency | 0 | 1 |
| model_collapse_feedback | 0 | 1 |
| moral_causation_locus | 1 | 4 |
| nad_precursor_bioavailability | 0 | 1 |
| nicene_creed_authority | 0 | 1 |
| organization_floor | 1 | 0 |
| performance_legitimacy | 1 | 5 |
| polaris_document_status | 1 | 5 |
| press_reformation_causality | 0 | 1 |
| press_reformation_causation | 0 | 2 |
| propagation_speed_asymmetry | 0 | 1 |
| property_sector_overhang | 0 | 1 |
| protein_anabolic_resistance | 0 | 1 |
| radiative_levitation_stratification | 0 | 1 |
| reading_acquisition_legitimacy | 0 | 1 |
| refugee_convention_text | 0 | 1 |
| responsibility_preservation_mechanism | 0 | 1 |
| scale_ceiling | 0 | 1 |
| secession_legitimacy_boundary | 0 | 1 |
| sex_gender_category | 0 | 1 |
| shinbutsu_ontological_commitment | 0 | 1 |
| specification_binding_authority | 0 | 1 |
| speech_protection_kernel | 0 | 1 |
| state_killing_legitimacy | 0 | 1 |
| statutory_debt_ceiling | 0 | 1 |
| synthesis_infrastructure_gap | 0 | 1 |
| third_act_arbitrage | 0 | 1 |
| udhr_article_3 | 0 | 1 |
| validation_judgment_separation | 0 | 1 |
| vedic_corpus_social_prescription | 0 | 1 |
| visual_evidentiary_authority | 1 | 5 |
| womens_financial_autonomy | 1 | 0 |
| zero_mathematical_status | 0 | 2 |
| zionist_legitimacy_basis | 0 | 1 |

p_raw=0.9259, descriptive_clustered=False; most-enriched=behavioral_adoption_friction (members=1, ratio=inf); enrichment gates pass=False

## extraction_unnameable (n=3, UNPOWERED — descriptive only)

### axis: model (permutation_chi2_N10000, K=5)
| stratum | in-bucket | rest |
|---|---|---|
| claude-haiku-4-5-20251001 | 3 | 25 |
| claude-sonnet-4-20250514 | 0 | 7 |
| claude-sonnet-4-5-20250929 | 0 | 64 |
| gemini-2.5-flash | 0 | 11 |
| provenance_unauthored | 0 | 9 |

p_raw=0.06179, (unpowered: no test in family); most-enriched=claude-haiku-4-5-20251001 (members=3, ratio=4.64); enrichment gates pass=True

### axis: prompt_commit (permutation_chi2_N10000, K=4)
| stratum | in-bucket | rest |
|---|---|---|
| 046e0a40c34cddf4fff29b8c15f632dbdef31b7a | 0 | 12 |
| 22843cdfd28a814d8f30c35778e75821452545bd | 3 | 71 |
| 8080348c4e16a265fafc924dcde83360dfd170fc | 0 | 24 |
| provenance_unauthored | 0 | 9 |

p_raw=0.7675, (unpowered: no test in family); most-enriched=22843cdfd28a814d8f30c35778e75821452545bd (members=3, ratio=1.63); enrichment gates pass=False

### axis: topic_family (permutation_chi2_N10000, K=79)
| stratum | in-bucket | rest |
|---|---|---|
| actinide_replenishment_mechanism | 0 | 4 |
| adjunctification_of_university_teaching | 0 | 1 |
| alignment_constraint_narrowing | 0 | 1 |
| animal_status_kernel | 1 | 0 |
| apoe4_mitochondrial_vulnerability | 0 | 1 |
| architectural_pattern_validity | 0 | 1 |
| basic_law_interpretive_authority | 0 | 1 |
| basic_law_interpretive_boundary | 0 | 1 |
| behavioral_adoption_friction | 0 | 1 |
| benchmark_saturation_interpretation | 0 | 1 |
| bitcoin_whitepaper_purpose | 0 | 1 |
| border_control_legitimacy | 0 | 1 |
| capability_endogeneity | 0 | 1 |
| catastrophe_memory_kernel | 0 | 1 |
| dataset_recycling_amplification | 0 | 1 |
| demographic_resource_allocation | 0 | 1 |
| demographic_skill_mismatch | 0 | 1 |
| digital_money_legitimacy | 0 | 5 |
| divine_legitimacy_substrate | 0 | 1 |
| doomsday_clock_metric | 0 | 1 |
| dueling_disappearance_mechanism | 0 | 1 |
| empty_shell_tolerance | 0 | 1 |
| equal_protection_kernel | 0 | 1 |
| fourteenth_amendment_equal_protection | 0 | 1 |
| gendered_outcome_asymmetry | 0 | 1 |
| generality_standard | 0 | 5 |
| geopolitical_settlement_competition | 0 | 1 |
| gita_kurukshetra_discourse | 0 | 1 |
| historical_treaty_substrate | 0 | 1 |
| institutional_barrier_structure | 0 | 1 |
| institutional_trust_erosion | 0 | 1 |
| institutional_verification_collapse | 0 | 1 |
| intervention_target_selection | 0 | 1 |
| jewish_self_determination | 1 | 0 |
| jewish_sovereignty_palestine | 0 | 2 |
| jewish_territorial_claim | 0 | 1 |
| john_1_1_logos | 0 | 1 |
| knowledge_legitimacy_biomedicine | 0 | 4 |
| lausanne_minority_protections | 0 | 1 |
| learning_difficulty_substrate | 0 | 5 |
| llm_synthesis_capacity | 0 | 1 |
| longevity_mismatch | 0 | 1 |
| lycurgan_laws | 0 | 1 |
| maat_order_principle | 0 | 1 |
| marriage_authority | 0 | 1 |
| mitochondrial_demand_signal_deficiency | 0 | 1 |
| model_collapse_feedback | 0 | 1 |
| moral_causation_locus | 0 | 5 |
| nad_precursor_bioavailability | 0 | 1 |
| nicene_creed_authority | 0 | 1 |
| organization_floor | 0 | 1 |
| performance_legitimacy | 0 | 6 |
| polaris_document_status | 0 | 6 |
| press_reformation_causality | 0 | 1 |
| press_reformation_causation | 0 | 2 |
| propagation_speed_asymmetry | 0 | 1 |
| property_sector_overhang | 0 | 1 |
| protein_anabolic_resistance | 0 | 1 |
| radiative_levitation_stratification | 0 | 1 |
| reading_acquisition_legitimacy | 0 | 1 |
| refugee_convention_text | 0 | 1 |
| responsibility_preservation_mechanism | 0 | 1 |
| scale_ceiling | 0 | 1 |
| secession_legitimacy_boundary | 1 | 0 |
| sex_gender_category | 0 | 1 |
| shinbutsu_ontological_commitment | 0 | 1 |
| specification_binding_authority | 0 | 1 |
| speech_protection_kernel | 0 | 1 |
| state_killing_legitimacy | 0 | 1 |
| statutory_debt_ceiling | 0 | 1 |
| synthesis_infrastructure_gap | 0 | 1 |
| third_act_arbitrage | 0 | 1 |
| udhr_article_3 | 0 | 1 |
| validation_judgment_separation | 0 | 1 |
| vedic_corpus_social_prescription | 0 | 1 |
| visual_evidentiary_authority | 0 | 6 |
| womens_financial_autonomy | 0 | 1 |
| zero_mathematical_status | 0 | 2 |
| zionist_legitimacy_basis | 0 | 1 |

p_raw=0.1811, descriptive_clustered=False; most-enriched=animal_status_kernel (members=1, ratio=inf); enrichment gates pass=False

## no_agent_seats (n=26, POWERED)

### axis: model (permutation_chi2_N10000, K=5)
| stratum | in-bucket | rest |
|---|---|---|
| claude-haiku-4-5-20251001 | 16 | 12 |
| claude-sonnet-4-20250514 | 0 | 7 |
| claude-sonnet-4-5-20250929 | 0 | 64 |
| gemini-2.5-flash | 1 | 10 |
| provenance_unauthored | 9 | 0 |

p_raw=9.999e-05, p_holm=0.0007999, clustered=True; most-enriched=provenance_unauthored (members=9, ratio=inf); enrichment gates pass=True

### axis: prompt_commit (permutation_chi2_N10000, K=4)
| stratum | in-bucket | rest |
|---|---|---|
| 046e0a40c34cddf4fff29b8c15f632dbdef31b7a | 0 | 12 |
| 22843cdfd28a814d8f30c35778e75821452545bd | 17 | 57 |
| 8080348c4e16a265fafc924dcde83360dfd170fc | 0 | 24 |
| provenance_unauthored | 9 | 0 |

p_raw=9.999e-05, p_holm=0.0007999, clustered=True; most-enriched=provenance_unauthored (members=9, ratio=inf); enrichment gates pass=True

### axis: topic_family (permutation_chi2_N10000, K=79)
| stratum | in-bucket | rest |
|---|---|---|
| actinide_replenishment_mechanism | 1 | 3 |
| adjunctification_of_university_teaching | 0 | 1 |
| alignment_constraint_narrowing | 0 | 1 |
| animal_status_kernel | 1 | 0 |
| apoe4_mitochondrial_vulnerability | 0 | 1 |
| architectural_pattern_validity | 0 | 1 |
| basic_law_interpretive_authority | 1 | 0 |
| basic_law_interpretive_boundary | 0 | 1 |
| behavioral_adoption_friction | 0 | 1 |
| benchmark_saturation_interpretation | 0 | 1 |
| bitcoin_whitepaper_purpose | 1 | 0 |
| border_control_legitimacy | 1 | 0 |
| capability_endogeneity | 0 | 1 |
| catastrophe_memory_kernel | 1 | 0 |
| dataset_recycling_amplification | 0 | 1 |
| demographic_resource_allocation | 0 | 1 |
| demographic_skill_mismatch | 0 | 1 |
| digital_money_legitimacy | 1 | 4 |
| divine_legitimacy_substrate | 1 | 0 |
| doomsday_clock_metric | 0 | 1 |
| dueling_disappearance_mechanism | 0 | 1 |
| empty_shell_tolerance | 0 | 1 |
| equal_protection_kernel | 1 | 0 |
| fourteenth_amendment_equal_protection | 1 | 0 |
| gendered_outcome_asymmetry | 0 | 1 |
| generality_standard | 1 | 4 |
| geopolitical_settlement_competition | 0 | 1 |
| gita_kurukshetra_discourse | 0 | 1 |
| historical_treaty_substrate | 0 | 1 |
| institutional_barrier_structure | 0 | 1 |
| institutional_trust_erosion | 0 | 1 |
| institutional_verification_collapse | 0 | 1 |
| intervention_target_selection | 0 | 1 |
| jewish_self_determination | 1 | 0 |
| jewish_sovereignty_palestine | 0 | 2 |
| jewish_territorial_claim | 0 | 1 |
| john_1_1_logos | 0 | 1 |
| knowledge_legitimacy_biomedicine | 1 | 3 |
| lausanne_minority_protections | 1 | 0 |
| learning_difficulty_substrate | 1 | 4 |
| llm_synthesis_capacity | 0 | 1 |
| longevity_mismatch | 0 | 1 |
| lycurgan_laws | 0 | 1 |
| maat_order_principle | 1 | 0 |
| marriage_authority | 1 | 0 |
| mitochondrial_demand_signal_deficiency | 0 | 1 |
| model_collapse_feedback | 0 | 1 |
| moral_causation_locus | 1 | 4 |
| nad_precursor_bioavailability | 0 | 1 |
| nicene_creed_authority | 1 | 0 |
| organization_floor | 0 | 1 |
| performance_legitimacy | 1 | 5 |
| polaris_document_status | 1 | 5 |
| press_reformation_causality | 0 | 1 |
| press_reformation_causation | 0 | 2 |
| propagation_speed_asymmetry | 0 | 1 |
| property_sector_overhang | 0 | 1 |
| protein_anabolic_resistance | 0 | 1 |
| radiative_levitation_stratification | 0 | 1 |
| reading_acquisition_legitimacy | 0 | 1 |
| refugee_convention_text | 0 | 1 |
| responsibility_preservation_mechanism | 0 | 1 |
| scale_ceiling | 0 | 1 |
| secession_legitimacy_boundary | 1 | 0 |
| sex_gender_category | 0 | 1 |
| shinbutsu_ontological_commitment | 1 | 0 |
| specification_binding_authority | 0 | 1 |
| speech_protection_kernel | 1 | 0 |
| state_killing_legitimacy | 0 | 1 |
| statutory_debt_ceiling | 0 | 1 |
| synthesis_infrastructure_gap | 0 | 1 |
| third_act_arbitrage | 0 | 1 |
| udhr_article_3 | 0 | 1 |
| validation_judgment_separation | 0 | 1 |
| vedic_corpus_social_prescription | 0 | 1 |
| visual_evidentiary_authority | 1 | 5 |
| womens_financial_autonomy | 0 | 1 |
| zero_mathematical_status | 1 | 1 |
| zionist_legitimacy_basis | 0 | 1 |

p_raw=0.7472, descriptive_clustered=False; most-enriched=animal_status_kernel (members=1, ratio=inf); enrichment gates pass=False

## manufactured_consensus_candidate (n=9, POWERED)

### axis: model (permutation_chi2_N10000, K=5)
| stratum | in-bucket | rest |
|---|---|---|
| claude-haiku-4-5-20251001 | 2 | 26 |
| claude-sonnet-4-20250514 | 1 | 6 |
| claude-sonnet-4-5-20250929 | 5 | 59 |
| gemini-2.5-flash | 1 | 10 |
| provenance_unauthored | 0 | 9 |

p_raw=0.9524, p_holm=0.9524, clustered=False; most-enriched=claude-sonnet-4-5-20250929 (members=5, ratio=1.04); enrichment gates pass=False

### axis: prompt_commit (permutation_chi2_N10000, K=4)
| stratum | in-bucket | rest |
|---|---|---|
| 046e0a40c34cddf4fff29b8c15f632dbdef31b7a | 2 | 10 |
| 22843cdfd28a814d8f30c35778e75821452545bd | 7 | 67 |
| 8080348c4e16a265fafc924dcde83360dfd170fc | 0 | 24 |
| provenance_unauthored | 0 | 9 |

p_raw=0.1816, p_holm=0.5935, clustered=False; most-enriched=22843cdfd28a814d8f30c35778e75821452545bd (members=7, ratio=1.28); enrichment gates pass=False

### axis: topic_family (permutation_chi2_N10000, K=79)
| stratum | in-bucket | rest |
|---|---|---|
| actinide_replenishment_mechanism | 1 | 3 |
| adjunctification_of_university_teaching | 0 | 1 |
| alignment_constraint_narrowing | 0 | 1 |
| animal_status_kernel | 0 | 1 |
| apoe4_mitochondrial_vulnerability | 0 | 1 |
| architectural_pattern_validity | 0 | 1 |
| basic_law_interpretive_authority | 0 | 1 |
| basic_law_interpretive_boundary | 1 | 0 |
| behavioral_adoption_friction | 0 | 1 |
| benchmark_saturation_interpretation | 0 | 1 |
| bitcoin_whitepaper_purpose | 0 | 1 |
| border_control_legitimacy | 0 | 1 |
| capability_endogeneity | 0 | 1 |
| catastrophe_memory_kernel | 0 | 1 |
| dataset_recycling_amplification | 0 | 1 |
| demographic_resource_allocation | 1 | 0 |
| demographic_skill_mismatch | 1 | 0 |
| digital_money_legitimacy | 0 | 5 |
| divine_legitimacy_substrate | 0 | 1 |
| doomsday_clock_metric | 0 | 1 |
| dueling_disappearance_mechanism | 0 | 1 |
| empty_shell_tolerance | 0 | 1 |
| equal_protection_kernel | 0 | 1 |
| fourteenth_amendment_equal_protection | 0 | 1 |
| gendered_outcome_asymmetry | 0 | 1 |
| generality_standard | 0 | 5 |
| geopolitical_settlement_competition | 0 | 1 |
| gita_kurukshetra_discourse | 0 | 1 |
| historical_treaty_substrate | 0 | 1 |
| institutional_barrier_structure | 0 | 1 |
| institutional_trust_erosion | 0 | 1 |
| institutional_verification_collapse | 0 | 1 |
| intervention_target_selection | 0 | 1 |
| jewish_self_determination | 0 | 1 |
| jewish_sovereignty_palestine | 1 | 1 |
| jewish_territorial_claim | 0 | 1 |
| john_1_1_logos | 0 | 1 |
| knowledge_legitimacy_biomedicine | 0 | 4 |
| lausanne_minority_protections | 0 | 1 |
| learning_difficulty_substrate | 0 | 5 |
| llm_synthesis_capacity | 0 | 1 |
| longevity_mismatch | 0 | 1 |
| lycurgan_laws | 0 | 1 |
| maat_order_principle | 0 | 1 |
| marriage_authority | 0 | 1 |
| mitochondrial_demand_signal_deficiency | 0 | 1 |
| model_collapse_feedback | 0 | 1 |
| moral_causation_locus | 0 | 5 |
| nad_precursor_bioavailability | 0 | 1 |
| nicene_creed_authority | 0 | 1 |
| organization_floor | 0 | 1 |
| performance_legitimacy | 0 | 6 |
| polaris_document_status | 2 | 4 |
| press_reformation_causality | 0 | 1 |
| press_reformation_causation | 0 | 2 |
| propagation_speed_asymmetry | 0 | 1 |
| property_sector_overhang | 0 | 1 |
| protein_anabolic_resistance | 0 | 1 |
| radiative_levitation_stratification | 1 | 0 |
| reading_acquisition_legitimacy | 0 | 1 |
| refugee_convention_text | 1 | 0 |
| responsibility_preservation_mechanism | 0 | 1 |
| scale_ceiling | 0 | 1 |
| secession_legitimacy_boundary | 0 | 1 |
| sex_gender_category | 0 | 1 |
| shinbutsu_ontological_commitment | 0 | 1 |
| specification_binding_authority | 0 | 1 |
| speech_protection_kernel | 0 | 1 |
| state_killing_legitimacy | 0 | 1 |
| statutory_debt_ceiling | 0 | 1 |
| synthesis_infrastructure_gap | 0 | 1 |
| third_act_arbitrage | 0 | 1 |
| udhr_article_3 | 0 | 1 |
| validation_judgment_separation | 0 | 1 |
| vedic_corpus_social_prescription | 0 | 1 |
| visual_evidentiary_authority | 0 | 6 |
| womens_financial_autonomy | 0 | 1 |
| zero_mathematical_status | 0 | 2 |
| zionist_legitimacy_basis | 0 | 1 |

p_raw=0.4363, descriptive_clustered=False; most-enriched=polaris_document_status (members=2, ratio=6.11); enrichment gates pass=False

## Axis-confounding cross-tab: topic_family x prompt_commit
(families spanning >1 prompt_commit; a family echoing a single generation batch is the confound to read jointly)

families with >1 commit: 9 of 79
- actinide_replenishment_mechanism: {'provenance_unauthored': 1, '22843cdfd28a814d8f30c35778e75821452545bd': 3}
- digital_money_legitimacy: {'22843cdfd28a814d8f30c35778e75821452545bd': 4, 'provenance_unauthored': 1}
- generality_standard: {'provenance_unauthored': 1, '8080348c4e16a265fafc924dcde83360dfd170fc': 4}
- knowledge_legitimacy_biomedicine: {'22843cdfd28a814d8f30c35778e75821452545bd': 3, 'provenance_unauthored': 1}
- learning_difficulty_substrate: {'8080348c4e16a265fafc924dcde83360dfd170fc': 4, 'provenance_unauthored': 1}
- moral_causation_locus: {'8080348c4e16a265fafc924dcde83360dfd170fc': 4, 'provenance_unauthored': 1}
- performance_legitimacy: {'046e0a40c34cddf4fff29b8c15f632dbdef31b7a': 5, 'provenance_unauthored': 1}
- polaris_document_status: {'22843cdfd28a814d8f30c35778e75821452545bd': 5, 'provenance_unauthored': 1}
- visual_evidentiary_authority: {'22843cdfd28a814d8f30c35778e75821452545bd': 5, 'provenance_unauthored': 1}
