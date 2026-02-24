:- module(test_harness, [
    load_scenario/1,
    run_all_tests/1,
    quick_check/1
]).

/* ================================================================
   1. Module Loading (The Integrated Stack)
   ================================================================ */

:- use_module(narrative_ontology).      % Schema & Global Expansion
:- use_module(v3_1_config).             % Consolidated Weights
:- use_module(v3_1_coercion_projection).% Vector Projection
:- use_module(v3_1_data_repair).        % Imputation for missing data
:- use_module(data_verification).       % Deep data integrity checks
:- use_module(pattern_analysis).        % Gradient & Completeness
:- use_module(intent_engine).           % Classification
:- use_module(constraint_bridge).       % CE v2.0 Bridge
:- use_module(report_generator).        % High-level Orchestration

/* ================================================================
   2. Scenario Management
   ================================================================ */

%% load_scenario(+ScenarioName)
%  Consults data packs. Ontology expansion routes facts automatically.
load_scenario(iran) :-
    consult('iran_khamenei_succession_data.pl'),
    format('~n[SCENARIO] Loaded: Iran Succession (2019-2026)~n').

load_scenario(lehman) :-
    consult('lehman_data.pl'),
    format('~n[SCENARIO] Loaded: Lehman Terminal Collapse (2008)~n').

/* ================================================================
   3. Execution & Validation Suite
   ================================================================ */

%% run_all_tests(+IntervalID)
%  Orchestrates the sequence from raw data to structural report.
run_all_tests(IntervalID) :-
    format('~n>>> INITIATING v3.1 TEST SUITE: ~w~n', [IntervalID]),
    
    % Step 1: Data Repair (Fill missing vector components)
    format('[STEP 1] Auditing and Repairing Measurements...~n'),
    v3_1_data_repair:repair_interval(IntervalID),

    % Step 2: Deep Verification
    format('[STEP 2] Verifying Data Integrity...~n'),
    (data_verification:verify_all -> format('[OK] Verification passed.~n') 
    ; format('[FAIL] Verification failed. Check logs.~n'), fail),

    % Step 3: Compute System Dynamics
    format('[STEP 3] Computing System Gradients...~n'),
    analyze_interval(IntervalID), %

    % Step 4: Classification & Report
    format('[STEP 4] Generating Executive Summary...~n'),
    generate_full_report(IntervalID). %

%% quick_check(+IntervalID)
%  Diagnostic for identifying the primary "Binding Mountain" intensity.
quick_check(IntervalID) :-
    format('--- Diagnostic: ~w ---~n', [IntervalID]),
    % Uses bridge to map constraints to diagnostic states
    (constraint_bridge:constraint_status(Name, binding_limit, Intensity) ->
        (Intensity > 0.8 -> format('CRITICAL MOUNTAIN: ~w (~2f)~n', [Name, Intensity]) ; true)
    ;   format('No binding mountains detected.~n')).

/* ================================================================
   4. Interactive Help
   ================================================================ */

help :-
    format('~n--- v3.1 Structural Analysis Harness ---~n'),
    format('1. load_scenario(iran|lehman).~n'),
    format('2. run_all_tests(IntervalID).~n'),
    format('3. quick_check(IntervalID).~n').
