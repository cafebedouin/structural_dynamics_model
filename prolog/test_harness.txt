:- module(test_harness, [
    load_scenario/1,
    run_all_tests/1,
    quick_check/1
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(v3_1_coercion_projection).
:- use_module(v3_1_data_repair).
:- use_module(data_verification).
:- use_module(pattern_analysis).
:- use_module(intent_engine).
:- use_module(constraint_bridge).
:- use_module(drl_core).
:- use_module(uke_dr_bridge).
:- use_module(report_generator).

/* ================================================================
   v3.2 HARD-STOP TEST HARNESS
   ================================================================ */

run_all_tests(IntervalID) :-
    format('~n>>> INITIATING v3.1.1 DR-AUDIT SUITE: ~w~n', [IntervalID]),

    % Step 1: Data Repair (Epistemic Imputation)
    format('[STEP 1] Auditing and Repairing Measurements...~n'),
    v3_1_data_repair:repair_interval(IntervalID),

    % Step 2: Hard-Stop Verification Gate
    % v3.2 Change: This is now a strict boolean gate.
    format('[STEP 2] Verifying Data Integrity...~n'),
    (   data_verification:verify_all 
    ->  format('[OK] Verification passed. Proceeding to audit.~n')
    ;   (   format('[CRITICAL FAIL] Data integrity verification failed for ~w.~n', [IntervalID]),
            format('Summary generation aborted to prevent halluncinated analysis.~n'),
            !, fail  % Hard-stop the pipeline here
        )
    ),

    % Step 3: Compute System Dynamics (Only reached if Step 2 passes)
    format('[STEP 3] Computing System Gradients...~n'),
    pattern_analysis:analyze_interval(IntervalID),

    % Step 4: DRL Ontological Audit
    format('[STEP 4] DRL Ontological Audit...~n'),
    (   setof((C, Err, Sev), drl_core:dr_mismatch(C, Err, Sev), Errors)
    ->  forall(member((C, Err, Sev), Errors),
               format('  ! ALERT [~w]: ~w detected for ~w~n', [Sev, Err, C]))
    ;   format('  [OK] No Ontological Fraud detected.~n')
    ),

    % Step 5: Extracting Omegas
    format('[STEP 5] Extracting Omega Variables (Ω)...~n'),
    (   setof((OID, Type, Desc), narrative_ontology:omega_variable(OID, Type, Desc), Omegas)
    ->  forall(member((OID, Type, Desc), Omegas),
               format('  - Ω_~w [~w]: ~w~n', [OID, Type, Desc]))
    ;   format('  No reasoning blockers (Omegas) identified.~n')
    ),

    % Step 6: Final Reporting
    format('[STEP 6] Generating Executive Summary...~n'),
    report_generator:generate_full_report(IntervalID),

    % Step 7: Recursive Feedback
    format('[STEP 7] Generating LLM Refinement Manifest...~n'),
    report_generator:generate_llm_feedback(IntervalID).

%% quick_check(+IntervalID)
%  Diagnostic for identifying the primary "Binding Mountain" intensity.
quick_check(IntervalID) :-
    format('--- Diagnostic: ~w ---~n', [IntervalID]),
    (   drl_core:dr_type(Name, mountain),
        narrative_ontology:constraint_metric(Name, intensity, Intensity),
        Intensity > 0.8
    ->  format('CRITICAL MOUNTAIN: ~w (~2f)~n', [Name, Intensity])
    ;   format('No binding mountains detected.~n')).

load_scenario(lehman) :-
    consult('lehman_data.pl'),
    format('~n[SCENARIO] Loaded: Lehman Terminal Collapse (2008)~n').

/* ================================================================
   4. Interactive Help
   ================================================================ */

help :-
    format('~n--- v3.1 Structural Analysis Harness ---~n'),
    format('1. load_scenario(iran|lehman).~n'),
    format('2. run_all_tests(IntervalID).~n'),
    format('3. quick_check(IntervalID).~n').
