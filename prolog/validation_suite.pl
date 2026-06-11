:- module(validation_suite, [run_dynamic_suite/0]).
:- use_module(library(prolog_stack)).
:- use_module(library(time)).
:- use_module(scenario_manager).
:- use_module(data_validation).
:- use_module(report_generator).

:- chdir('../prolog').

:- dynamic test_passed/1, test_failed/3, test_case/4.

% --- Test Case Definitions ---
test_case('testsets/adjunctification_of_university_teaching.pl', 'adjunctification_of_university_teaching', 'ADJUNCTIFICATION_OF_UNIVERSITY_TEACHING', 1).
test_case('testsets/agenda_conditioning.pl', 'agenda_conditioning', 'AGENDA_CONDITIONING', 2).
test_case('testsets/agricultural_contract_grower_lockin.pl', 'agricultural_contract_grower_lockin', 'AGRICULTURAL_CONTRACT_GROWER_LOCKIN', 3).
test_case('testsets/ai_governance_accountability.pl', 'ai_governance_accountability', 'AI_GOVERNANCE_ACCOUNTABILITY', 4).
test_case('testsets/collapse_mechanism_ambiguity.pl', 'collapse_mechanism_ambiguity', 'COLLAPSE_MECHANISM_AMBIGUITY', 5).
test_case('testsets/collapse_timing_uncertainty.pl', 'collapse_timing_uncertainty', 'COLLAPSE_TIMING_UNCERTAINTY', 6).
test_case('testsets/company_town_scrip_economy.pl', 'company_town_scrip_economy', 'COMPANY_TOWN_SCRIP_ECONOMY', 7).
test_case('testsets/competition_timeline_pressure.pl', 'competition_timeline_pressure', 'COMPETITION_TIMELINE_PRESSURE', 8).
test_case('testsets/demographic_skill_mismatch.pl', 'demographic_skill_mismatch', 'DEMOGRAPHIC_SKILL_MISMATCH', 9).
test_case('testsets/digital_colonialism_data_extraction.pl', 'digital_colonialism_data_extraction', 'DIGITAL_COLONIALISM_DATA_EXTRACTION', 10).
test_case('testsets/digital_power_concentration.pl', 'digital_power_concentration', 'DIGITAL_POWER_CONCENTRATION', 11).
test_case('testsets/eldercare_guardianship_capture.pl', 'eldercare_guardianship_capture', 'ELDERCARE_GUARDIANSHIP_CAPTURE', 12).
test_case('testsets/employment_boundary_contradictions.pl', 'unknown_interval', 'EMPLOYMENT_BOUNDARY_CONTRADICTIONS', 13).
test_case('testsets/employment_boundary_flat_control.pl', 'employment_boundary_flat_control', 'EMPLOYMENT_BOUNDARY_FLAT_CONTROL', 14).
test_case('testsets/formalist_employment_reading.pl', 'formalist_employment_reading', 'FORMALIST_EMPLOYMENT_READING', 15).
test_case('testsets/garment_supplychain_audit_theater.pl', 'garment_supplychain_audit_theater', 'GARMENT_SUPPLYCHAIN_AUDIT_THEATER', 16).
test_case('testsets/gig_platform_algorithmic_management.pl', 'gig_platform_algorithmic_management', 'GIG_PLATFORM_ALGORITHMIC_MANAGEMENT', 17).
test_case('testsets/human_dignity_ai_governance_contradictions.pl', 'unknown_interval', 'HUMAN_DIGNITY_AI_GOVERNANCE_CONTRADICTIONS', 18).
test_case('testsets/human_dignity_ai_governance_flat_control.pl', 'human_dignity_ai_governance_flat_control', 'HUMAN_DIGNITY_AI_GOVERNANCE_FLAT_CONTROL', 19).
test_case('testsets/hybrid_security_reading.pl', 'hybrid_security_reading', 'HYBRID_SECURITY_READING', 20).
test_case('testsets/information_suppression.pl', 'information_suppression', 'INFORMATION_SUPPRESSION', 21).
test_case('testsets/magisterial_integralist_reading.pl', 'magisterial_integralist_reading', 'MAGISTERIAL_INTEGRALIST_READING', 22).
test_case('testsets/medical_debt_collection_escalation.pl', 'medical_debt_collection_escalation', 'MEDICAL_DEBT_COLLECTION_ESCALATION', 23).
test_case('testsets/optimization_artifact_risk.pl', 'optimization_artifact_risk', 'OPTIMIZATION_ARTIFACT_RISK', 24).
test_case('testsets/organization_floor.pl', 'organization_floor', 'ORGANIZATION_FLOOR', 25).
test_case('testsets/platform_flexibility_precarity_tradeoff.pl', 'platform_flexibility_precarity_tradeoff', 'PLATFORM_FLEXIBILITY_PRECARITY_TRADEOFF', 26).
test_case('testsets/pluralist_pragmatic_reading.pl', 'pluralist_pragmatic_reading', 'PLURALIST_PRAGMATIC_READING', 27).
test_case('testsets/post_1998_convergence.pl', 'post_1998_convergence', 'POST_1998_CONVERGENCE', 28).
test_case('testsets/probation_supervision_intensification.pl', 'probation_supervision_intensification', 'PROBATION_SUPERVISION_INTENSIFICATION', 29).
test_case('testsets/proxy_measurement_validity.pl', 'proxy_measurement_validity', 'PROXY_MEASUREMENT_VALIDITY', 30).
test_case('testsets/recalibration_interpretive_validity.pl', 'recalibration_interpretive_validity', 'RECALIBRATION_INTERPRETIVE_VALIDITY', 31).
test_case('testsets/regime_change_structural_break.pl', 'regime_change_structural_break', 'REGIME_CHANGE_STRUCTURAL_BREAK', 32).
test_case('testsets/regulatory_measurement_gap.pl', 'regulatory_measurement_gap', 'REGULATORY_MEASUREMENT_GAP', 33).
test_case('testsets/reprogramming_safety_toxicity.pl', 'reprogramming_safety_toxicity', 'REPROGRAMMING_SAFETY_TOXICITY', 34).
test_case('testsets/retirement_security_deficit.pl', 'retirement_security_deficit', 'RETIREMENT_SECURITY_DEFICIT', 35).
test_case('testsets/scale_ceiling.pl', 'scale_ceiling', 'SCALE_CEILING', 36).
test_case('testsets/secular_humanist_reading.pl', 'secular_humanist_reading', 'SECULAR_HUMANIST_READING', 37).
test_case('testsets/solar_integration_mechanism.pl', 'solar_integration_mechanism', 'SOLAR_INTEGRATION_MECHANISM', 38).
test_case('testsets/substantive_employment_reading.pl', 'substantive_employment_reading', 'SUBSTANTIVE_EMPLOYMENT_READING', 39).
test_case('testsets/surveillance_control_freedom.pl', 'surveillance_control_freedom', 'SURVEILLANCE_CONTROL_FREEDOM', 40).
test_case('testsets/techno_optimist_reading.pl', 'techno_optimist_reading', 'TECHNO_OPTIMIST_READING', 41).
test_case('testsets/technocratic_paradigm_resistance.pl', 'technocratic_paradigm_resistance', 'TECHNOCRATIC_PARADIGM_RESISTANCE', 42).
test_case('testsets/technocratic_paradigm_vs_human_primacy.pl', 'technocratic_paradigm_vs_human_primacy', 'TECHNOCRATIC_PARADIGM_VS_HUMAN_PRIMACY', 43).
test_case('testsets/tenant_displacement_renovation_eviction.pl', 'tenant_displacement_renovation_eviction', 'TENANT_DISPLACEMENT_RENOVATION_EVICTION', 44).
test_case('testsets/thermal_dissipation_constraint.pl', 'thermal_dissipation_constraint', 'THERMAL_DISSIPATION_CONSTRAINT', 45).
test_case('testsets/topology_selection.pl', 'topology_selection', 'TOPOLOGY_SELECTION', 46).
test_case('testsets/transfer_gap_physics.pl', 'transfer_gap_physics', 'TRANSFER_GAP_PHYSICS', 47).
test_case('testsets/truth_as_common_good.pl', 'truth_as_common_good', 'TRUTH_AS_COMMON_GOOD', 48).
test_case('testsets/truth_democracy_disinformation.pl', 'truth_democracy_disinformation', 'TRUTH_DEMOCRACY_DISINFORMATION', 49).
test_case('testsets/union_decertification_campaign.pl', 'union_decertification_campaign', 'UNION_DECERTIFICATION_CAMPAIGN', 50).
test_case('testsets/veto_asymmetry.pl', 'veto_asymmetry', 'VETO_ASYMMETRY', 51).
test_case('testsets/voltage_regulation_tradeoff.pl', 'voltage_regulation_tradeoff', 'VOLTAGE_REGULATION_TRADEOFF', 52).
test_case('testsets/wage_convergence_mechanism.pl', 'wage_convergence_mechanism', 'WAGE_CONVERGENCE_MECHANISM', 53).
test_case('testsets/wage_convergence_sustainability.pl', 'wage_convergence_sustainability', 'WAGE_CONVERGENCE_SUSTAINABILITY', 54).
test_case('testsets/war_normalization_ai_weapons.pl', 'war_normalization_ai_weapons', 'WAR_NORMALIZATION_AI_WEAPONS', 55).
test_case('testsets/war_normalization_autonomous_weapons.pl', 'war_normalization_autonomous_weapons', 'WAR_NORMALIZATION_AUTONOMOUS_WEAPONS', 56).
test_case('testsets/work_dignity_automation.pl', 'work_dignity_automation', 'WORK_DIGNITY_AUTOMATION', 57).
test_case('testsets/work_displacement_dignity.pl', 'work_displacement_dignity', 'WORK_DISPLACEMENT_DIGNITY', 58).

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

