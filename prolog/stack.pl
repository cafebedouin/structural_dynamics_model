:- module(stack, [
    initialize_system/0,
    run_scenario/2
]).

% 1. Load Core Knowledge Schema
:- use_module(narrative_ontology).      % Schema & Global Expansion
:- use_module(config).             % Grounded Weights & Thresholds

% 2. Load Data & Priors
:- use_module(cache_registry, []).   % Central memo-cache invalidation surface
:- use_module(corpus_loader).        % Centralized testset loading
:- use_module(domain_priors).
:- use_module(constraint_instances).

% 3. Load Core Logic
:- use_module(constraint_indexing).
:- use_module(boltzmann_compliance, []).
:- use_module(guard_exclusions, []).   % OQ-114 per-story guard exclusions (fail-closed contract)
:- use_module(signature_detection, []).
:- use_module(abductive_helpers, []).   % OQ-115: realize the module under [stack]; signature_grade/2 calls it qualified+unguarded
:- use_module(cs_pattern_detection, []).
:- use_module(cs_drift_engine, []).
:- use_module(cs_axiom_engine, []).
:- use_module(cs_kernel_registry, []).   % loaded as side-effect of cs_axiom_engine; explicit for clarity
:- use_module(cs_trifurcation, []).      % OQ-55 within-kernel A/B/C disagreement router
:- use_module(cs_drift_mismatch, []).
:- use_module(purity_scoring, []).
:- use_module(structural_signatures).   % facade kept for backward-compat qualified calls
:- use_module(drl_core).
:- use_module(stakeholder_seats, []).   % OQ-83: per-(C,Name) observer layer
:- use_module(drl_composition, []).
:- use_module(drl_counterfactual, []).
:- use_module(drl_boltzmann_analysis, []).
:- use_module(drl_purity_network, []).
:- use_module(drl_fpn, []).
:- use_module(drl_modal_logic, []).    % facade kept for backward-compat qualified calls
:- use_module(drl_audit_core, []).

% 4. Load Management & Control (NEW)
:- use_module(scenario_manager, []).    % Lifecycle Controller

% 5. Load Functional Engines (Imported but silenced for namespace safety)
:- use_module(coercion_projection, []).
:- use_module(data_repair, []).
:- use_module(data_verification, []).
:- use_module(pattern_analysis, []).
:- use_module(intent_engine, []).
:- use_module(drift_events, []).
:- use_module(transition_paths, []).
:- use_module(temporal_residual, []).   % Type-A observer residual detector (OQ-83; category-B, read-only)
:- use_module(network_dynamics, []).
:- use_module(drift_report, []).
:- use_module(drl_lifecycle, []).       % facade — reexports from above

% 4. Load Diagnostic & UI
:- use_module(reading_diff, []).        % cyclopean disparity operator (authored-cells-only)
:- use_module(axiom_diff, []).          % reading_diff lifted to the cs_axiom layer
:- use_module(bifurcation_export, []).
:- use_module(persistence_export, []).
:- use_module(arakelov_height, []).
:- use_module(sheaf_analysis, []).
:- use_module(constraint_bridge, []).
:- use_module(uke_dr_bridge, []).
:- use_module(report_generator, []).
:- use_module(test_harness, []).

% Add the directory of this file to the library search path
:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(library, Dir)).

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
    format('   STRUCTURAL ANALYSIS STACK INITIALIZED             ~n'),
    format('====================================================~n'),
    format('Control:    Scenario Manager Active~n'),
    format('Usage:      run_scenario(\'file.pl\', interval_id).~n'),
    format('====================================================~n').

:- initialize_system.
