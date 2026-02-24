:- module(v3_1_stack, [
    initialize_system/0,
    run_scenario/2
]).

% 1. Load Core Knowledge Schema
:- use_module(narrative_ontology).      % Schema & Global Expansion
:- use_module(v3_1_config).             % Grounded Weights & Thresholds

% 2. Load Management & Control (NEW)
:- use_module(scenario_manager, []).    % Lifecycle Controller

% 3. Load Functional Engines (Imported but silenced for namespace safety)
:- use_module(v3_1_coercion_projection, []).
:- use_module(v3_1_data_repair, []).
:- use_module(data_verification, []).
:- use_module(pattern_analysis, []).
:- use_module(intent_engine, []).

% 4. Load Diagnostic & UI
:- use_module(constraint_bridge, []).
:- use_module(report_generator, []).
:- use_module(test_harness, []).

/* ================================================================
   SCENARIO ALIASES
   ================================================================ */

%% run_scenario(+File, +IntervalID)
%  Convenience alias for the Scenario Manager's load_and_run predicate.
run_scenario(File, IntervalID) :-
    scenario_manager:load_and_run(File, IntervalID).

/* ================================================================
   INITIALIZATION
   ================================================================ */

initialize_system :-
    format('~n====================================================~n'),
    format('   v3.1 STRUCTURAL ANALYSIS STACK INITIALIZED      ~n'),
    format('====================================================~n'),
    format('Namespace:  Consolidated & Grounded (v3.1)~n'),
    format('Control:    Scenario Manager Active~n'),
    format('Usage:      run_scenario(\'file.pl\', interval_id).~n'),
    format('====================================================~n').

:- initialize_system.
