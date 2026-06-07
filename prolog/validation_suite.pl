:- module(validation_suite, [run_dynamic_suite/0]).
:- use_module(library(prolog_stack)).
:- use_module(library(time)).
:- use_module(scenario_manager).
:- use_module(data_validation).
:- use_module(report_generator).

:- chdir('../prolog').

:- dynamic test_passed/1, test_failed/3, test_case/4.

% --- Test Case Definitions ---
test_case('testsets/absolutism_attachment.pl', 'absolutism_attachment', 'ABSOLUTISM_ATTACHMENT', 1).
test_case('testsets/administrative_burden_extraction.pl', 'administrative_burden_extraction', 'ADMINISTRATIVE_BURDEN_EXTRACTION', 2).
test_case('testsets/ai_governance_accountability.pl', 'ai_governance_accountability', 'AI_GOVERNANCE_ACCOUNTABILITY', 3).
test_case('testsets/alignment_tax_defection.pl', 'alignment_tax_defection', 'ALIGNMENT_TAX_DEFECTION', 4).
test_case('testsets/asymmetric_collaboration_terms.pl', 'asymmetric_collaboration_terms', 'ASYMMETRIC_COLLABORATION_TERMS', 5).
test_case('testsets/attribution_erosion.pl', 'attribution_erosion', 'ATTRIBUTION_EROSION', 6).
test_case('testsets/automation_velocity_vs_oversight_capacity.pl', 'automation_velocity_vs_oversight_capacity', 'AUTOMATION_VELOCITY_VS_OVERSIGHT_CAPACITY', 7).
test_case('testsets/beautiful_reports_feedback_loop.pl', 'beautiful_reports_feedback_loop', 'BEAUTIFUL_REPORTS_FEEDBACK_LOOP', 8).
test_case('testsets/benchmark_saturation_vs_deployment_gap.pl', 'benchmark_saturation_vs_deployment_gap', 'BENCHMARK_SATURATION_VS_DEPLOYMENT_GAP', 9).
test_case('testsets/british_mandate_scaffolding.pl', 'british_mandate_scaffolding', 'BRITISH_MANDATE_SCAFFOLDING', 10).
test_case('testsets/burnout_as_exit_mechanism.pl', '0', 'BURNOUT_AS_EXIT_MECHANISM', 11).
test_case('testsets/challenge_as_commons_maintenance.pl', '0', 'CHALLENGE_AS_COMMONS_MAINTENANCE', 12).
test_case('testsets/citizen_complicity_structure.pl', 'citizen_complicity_structure', 'CITIZEN_COMPLICITY_STRUCTURE', 13).
test_case('testsets/clinical_deskilling_automation.pl', 'clinical_deskilling_automation', 'CLINICAL_DESKILLING_AUTOMATION', 14).
test_case('testsets/clinical_need_threshold.pl', 'clinical_need_threshold', 'CLINICAL_NEED_THRESHOLD', 15).
test_case('testsets/clinical_translation_gap.pl', 'clinical_translation_gap', 'CLINICAL_TRANSLATION_GAP', 16).
test_case('testsets/cognitive_warfare_collapse.pl', 'cognitive_warfare_collapse', 'COGNITIVE_WARFARE_COLLAPSE', 17).
test_case('testsets/compute_constraint_as_brake.pl', 'compute_constraint_as_brake', 'COMPUTE_CONSTRAINT_AS_BRAKE', 18).
test_case('testsets/congressional_action_window.pl', 'congressional_action_window', 'CONGRESSIONAL_ACTION_WINDOW', 19).
test_case('testsets/conquest_of_labor_exclusion.pl', 'conquest_of_labor_exclusion', 'CONQUEST_OF_LABOR_EXCLUSION', 20).
test_case('testsets/control_mechanism_backfire.pl', 'control_mechanism_backfire', 'CONTROL_MECHANISM_BACKFIRE', 21).
test_case('testsets/data_consent_paradox.pl', '0', 'DATA_CONSENT_PARADOX', 22).
test_case('testsets/data_overload_triage.pl', 'data_overload_triage', 'DATA_OVERLOAD_TRIAGE', 23).
test_case('testsets/deathonomics_collapse.pl', '0', 'DEATHONOMICS_COLLAPSE', 24).
test_case('testsets/delivery_modality_toxicity.pl', 'delivery_modality_toxicity', 'DELIVERY_MODALITY_TOXICITY', 25).
test_case('testsets/demographic_engineering_imperative.pl', 'demographic_engineering_imperative', 'DEMOGRAPHIC_ENGINEERING_IMPERATIVE', 26).
test_case('testsets/dependency_lock_in.pl', 'dependency_lock_in', 'DEPENDENCY_LOCK_IN', 27).
test_case('testsets/digital_slavery_exploitation.pl', '0', 'DIGITAL_SLAVERY_EXPLOITATION', 28).
test_case('testsets/discrimination_substrate.pl', 'discrimination_substrate', 'DISCRIMINATION_SUBSTRATE', 29).
test_case('testsets/dna_repair_substrate_difference.pl', '0', 'DNA_REPAIR_SUBSTRATE_DIFFERENCE', 30).
test_case('testsets/dual_use_complicity.pl', '0', 'DUAL_USE_COMPLICITY', 31).
test_case('testsets/economic_condition_uncertainty.pl', 'economic_condition_uncertainty', 'ECONOMIC_CONDITION_UNCERTAINTY', 32).
test_case('testsets/elite_legitimacy_fracture.pl', '0', 'ELITE_LEGITIMACY_FRACTURE', 33).
test_case('testsets/export_control_reversibility.pl', 't4', 'EXPORT_CONTROL_REVERSIBILITY', 34).
test_case('testsets/extraordinary_measures_duration.pl', 'extraordinary_measures_duration', 'EXTRAORDINARY_MEASURES_DURATION', 35).
test_case('testsets/family_social_conditions_hope.pl', 'family_social_conditions_hope', 'FAMILY_SOCIAL_CONDITIONS_HOPE', 36).
test_case('testsets/gendered_caregiving_penalty.pl', 'gendered_caregiving_penalty', 'GENDERED_CAREGIVING_PENALTY', 37).
test_case('testsets/genetic_mosaicism_timing.pl', '0', 'GENETIC_MOSAICISM_TIMING', 38).
test_case('testsets/genetic_parenthood_valuation.pl', '0', 'GENETIC_PARENTHOOD_VALUATION', 39).
test_case('testsets/grey_market_evasion.pl', '0', 'GREY_MARKET_EVASION', 40).
test_case('testsets/guide_rna_off_target_variance.pl', 'guide_rna_off_target_variance', 'GUIDE_RNA_OFF_TARGET_VARIANCE', 41).
test_case('testsets/healthcare_inequality_amplification.pl', '0', 'HEALTHCARE_INEQUALITY_AMPLIFICATION', 42).
test_case('testsets/ideological_diversity_convergence.pl', 'ideological_diversity_convergence', 'IDEOLOGICAL_DIVERSITY_CONVERGENCE', 43).
test_case('testsets/informed_consent_impossibility.pl', 'informed_consent_impossibility', 'INFORMED_CONSENT_IMPOSSIBILITY', 44).
test_case('testsets/intergenerational_justice_asymmetry.pl', '0', 'INTERGENERATIONAL_JUSTICE_ASYMMETRY', 45).
test_case('testsets/iran_conflict_spending.pl', 'iran_conflict_spending', 'IRAN_CONFLICT_SPENDING', 46).
test_case('testsets/iron_wall_strategy.pl', 'iron_wall_strategy', 'IRON_WALL_STRATEGY', 47).
test_case('testsets/legitimacy_narrative_inversion.pl', '2014', 'LEGITIMACY_NARRATIVE_INVERSION', 48).
test_case('testsets/limit_arrival_timing.pl', 'limit_arrival_timing', 'LIMIT_ARRIVAL_TIMING', 49).
test_case('testsets/logical_coherence_paradox.pl', 'logical_coherence_paradox', 'LOGICAL_COHERENCE_PARADOX', 50).
test_case('testsets/manpower_exhaustion_trap.pl', 'manpower_exhaustion_trap', 'MANPOWER_EXHAUSTION_TRAP', 51).
test_case('testsets/metaphysical_retreat_mechanism.pl', 'metaphysical_retreat_mechanism', 'METAPHYSICAL_RETREAT_MECHANISM', 52).
test_case('testsets/milblogger_legitimacy_erosion.pl', 'milblogger_legitimacy_erosion', 'MILBLOGGER_LEGITIMACY_EROSION', 53).
test_case('testsets/military_defeat_cascade.pl', 'military_defeat_cascade', 'MILITARY_DEFEAT_CASCADE', 54).
test_case('testsets/moral_remainder_requirement.pl', 'moral_remainder_requirement', 'MORAL_REMAINDER_REQUIREMENT', 55).
test_case('testsets/necessity_ambiguity.pl', 'necessity_ambiguity', 'NECESSITY_AMBIGUITY', 56).
test_case('testsets/never_practiced_cohort_formation.pl', 'never_practiced_cohort_formation', 'NEVER_PRACTICED_COHORT_FORMATION', 57).
test_case('testsets/normalization_through_repetition.pl', 'normalization_through_repetition', 'NORMALIZATION_THROUGH_REPETITION', 58).
test_case('testsets/operational_overextension_trap.pl', 'operational_overextension_trap', 'OPERATIONAL_OVEREXTENSION_TRAP', 59).
test_case('testsets/passportization_legal_scaffolding.pl', 'passportization_legal_scaffolding', 'PASSPORTIZATION_LEGAL_SCAFFOLDING', 60).
test_case('testsets/patient_demand_escalation.pl', 'patient_demand_escalation', 'PATIENT_DEMAND_ESCALATION', 61).
test_case('testsets/political_exceptionalism.pl', 'Westphalian', 'POLITICAL_EXCEPTIONALISM', 62).
test_case('testsets/predictive_surveillance_capability.pl', 'predictive_surveillance_capability', 'PREDICTIVE_SURVEILLANCE_CAPABILITY', 63).
test_case('testsets/private_power_vs_subsidiarity_common_good.pl', 'private_power_vs_subsidiarity_common_good', 'PRIVATE_POWER_VS_SUBSIDIARITY_COMMON_GOOD', 64).
test_case('testsets/public_confidence_erosion.pl', 'public_confidence_erosion', 'PUBLIC_CONFIDENCE_EROSION', 65).
test_case('testsets/punishment_regress.pl', 'punishment_regress', 'PUNISHMENT_REGRESS', 66).
test_case('testsets/recursive_capability_threshold.pl', 'recursive_capability_threshold', 'RECURSIVE_CAPABILITY_THRESHOLD', 67).
test_case('testsets/regulatory_adequacy_gap.pl', '0', 'REGULATORY_ADEQUACY_GAP', 68).
test_case('testsets/regulatory_governance_level.pl', 'regulatory_governance_level', 'REGULATORY_GOVERNANCE_LEVEL', 69).
test_case('testsets/regulatory_precaution_threshold.pl', 'regulatory_precaution_threshold', 'REGULATORY_PRECAUTION_THRESHOLD', 70).
test_case('testsets/reproductive_liberty_scope.pl', '0', 'REPRODUCTIVE_LIBERTY_SCOPE', 71).
test_case('testsets/research_autonomy_capture.pl', 'research_autonomy_capture', 'RESEARCH_AUTONOMY_CAPTURE', 72).
test_case('testsets/review_system_collapse.pl', 'review_system_collapse', 'REVIEW_SYSTEM_COLLAPSE', 73).
test_case('testsets/safety_risk_structure.pl', '0', 'SAFETY_RISK_STRUCTURE', 74).
test_case('testsets/scientific_viability_uncertainty.pl', 'scientific_viability_uncertainty', 'SCIENTIFIC_VIABILITY_UNCERTAINTY', 75).
test_case('testsets/scope_restriction.pl', 'scope_restriction', 'SCOPE_RESTRICTION', 76).
test_case('testsets/service_fragility_prevention_shift.pl', 'service_fragility_prevention_shift', 'SERVICE_FRAGILITY_PREVENTION_SHIFT', 77).
test_case('testsets/slippery_slope_mechanism.pl', 'slippery_slope_mechanism', 'SLIPPERY_SLOPE_MECHANISM', 78).
test_case('testsets/social_justice_distribution.pl', '0', 'SOCIAL_JUSTICE_DISTRIBUTION', 79).
test_case('testsets/special_interests_capture.pl', 'special_interests_capture', 'SPECIAL_INTERESTS_CAPTURE', 80).
test_case('testsets/stakeholder_representation_gap.pl', '0', 'STAKEHOLDER_REPRESENTATION_GAP', 81).
test_case('testsets/statutory_ceiling_vs_suspension.pl', 'statutory_ceiling_vs_suspension', 'STATUTORY_CEILING_VS_SUSPENSION', 82).
test_case('testsets/structural_privacy_erosion.pl', 'structural_privacy_erosion', 'STRUCTURAL_PRIVACY_EROSION', 83).
test_case('testsets/supreme_emergency_threshold.pl', 'supreme_emergency_threshold', 'SUPREME_EMERGENCY_THRESHOLD', 84).
test_case('testsets/surveillance_export_proliferation.pl', 'surveillance_export_proliferation', 'SURVEILLANCE_EXPORT_PROLIFERATION', 85).
test_case('testsets/tariff_revenue_volatility.pl', 'tariff_revenue_volatility', 'TARIFF_REVENUE_VOLATILITY', 86).
test_case('testsets/technocratic_paradigm_vs_human_dignity.pl', 'technocratic_paradigm_vs_human_dignity', 'TECHNOCRATIC_PARADIGM_VS_HUMAN_DIGNITY', 87).
test_case('testsets/technology_asymmetry_ukraine_russia.pl', 'technology_asymmetry_ukraine_russia', 'TECHNOLOGY_ASYMMETRY_UKRAINE_RUSSIA', 88).
test_case('testsets/transfer_doctrine.pl', 'transfer_doctrine', 'TRANSFER_DOCTRINE', 89).
test_case('testsets/treasury_cash_discretion.pl', 'treasury_cash_discretion', 'TREASURY_CASH_DISCRETION', 90).
test_case('testsets/treatment_enhancement_boundary.pl', '0', 'TREATMENT_ENHANCEMENT_BOUNDARY', 91).
test_case('testsets/tribal_marker_vs_analytical_work.pl', 'tribal_marker_vs_analytical_work', 'TRIBAL_MARKER_VS_ANALYTICAL_WORK', 92).
test_case('testsets/truth_democracy_disinformation.pl', '0', 'TRUTH_DEMOCRACY_DISINFORMATION', 93).
test_case('testsets/universal_destination_digital_goods.pl', 'universal_destination_digital_goods', 'UNIVERSAL_DESTINATION_DIGITAL_GOODS', 94).
test_case('testsets/verification_authority_fragmentation.pl', 'verification_authority_fragmentation', 'VERIFICATION_AUTHORITY_FRAGMENTATION', 95).
test_case('testsets/verification_regime_feasibility.pl', 'verification_regime_feasibility', 'VERIFICATION_REGIME_FEASIBILITY', 96).
test_case('testsets/war_normalization_ai_weapons.pl', 'war_normalization_ai_weapons', 'WAR_NORMALIZATION_AI_WEAPONS', 97).
test_case('testsets/work_dignity_automation_unemployment.pl', 'work_dignity_automation_unemployment', 'WORK_DIGNITY_AUTOMATION_UNEMPLOYMENT', 98).
test_case('testsets/workforce_feminization_paradox.pl', 'workforce_feminization_paradox', 'WORKFORCE_FEMINIZATION_PARADOX', 99).
test_case('testsets/x_date_timing.pl', 'x_date_timing', 'X_DATE_TIMING', 100).

% --- Test Suite Runner ---
run_dynamic_suite :-
    retractall(test_passed(_)),
    retractall(test_failed(_, _, _)),
    writeln('--- STARTING DYNAMIC VALIDATION ---'),
    forall(test_case(Path, ID, Label, N), run_single_test(Path, ID, Label, N)),
    count_and_report,
    % Call validate_all directly from data_validation module
    data_validation:validate_all.

% --- Single Test Executor ---
%  Per-test timeout guard (60s) prevents any single test from consuming
%  the entire sweep timeout. Elapsed timing aids diagnostic profiling.
run_single_test(Path, ID, _Label, N) :-
    format('~n[~w] EXECUTING: ~w~n', [N, Path]),
    get_time(T0),
    catch(
        call_with_time_limit(
            60,
            (   catch_with_backtrace(
                    ( load_and_run(Path, ID) ->
                        assertz(test_passed(Path)),
                        format('[PASS] ~w~n', [Path])
                    ;   assertz(test_failed(Path, audit_failed, 'load_and_run returned false')),
                        format('[AUDIT FAIL] ~w~n', [Path])
                    ),
                    E,
                    (   assertz(test_failed(Path, exception, E)),
                        format('[FAIL] Exception for ~w: ~w~n', [Path, E]),
                        print_prolog_backtrace(current_output, E)
                    )
                ),
                report_generator:generate_llm_feedback(ID)
            )
        ),
        time_limit_exceeded,
        (   assertz(test_failed(Path, timeout, 'Exceeded 60s per-test limit')),
            format('[TIMEOUT] ~w~n', [Path])
        )
    ),
    get_time(T1),
    Elapsed is T1 - T0,
    format('[ELAPSED] ~w: ~3fs~n', [Path, Elapsed]).

% --- Result Counter & Reporter ---
count_and_report :-
    findall(P, test_passed(P), Ps), length(Ps, PC),
    findall(F, test_failed(F,_,_), Fs), length(Fs, FC),
    writeln(''),
    writeln('=================================================='),
    writeln('           TEST SUITE SUMMARY'),
    writeln('=================================================='),
    format('Passed: ~w~n', [PC]),
    format('Failed: ~w~n', [FC]),
    (FC > 0 -> report_failures ; true),
    writeln('==================================================').

report_failures :-
    writeln('--- FAILED TESTS ---'),
    forall(test_failed(Path, Type, Detail),
           format('~n  - [~w] ~w~n    Reason: ~w~n', [Type, Path, Detail])).

