% ============================================================================
% CONSTRAINT STORY: canada_goose_realignment_2026
% ============================================================================
% Version: 3.4
% Logic: 3.3
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_canada_goose_realignment_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * SUMMARY:
 * Patrick Bourke's appointment as President of North America marks a 
 * strategic shift toward retail expansion and cost management.
 */

/* ==========================================================================
   2. BASE PROPERTIES
   ========================================================================== */

domain_priors:base_extractiveness(canada_goose_2026, 0.25). % Moderate (Luxury pricing).
domain_priors:suppression_score(canada_goose_2026, 0.15).   % Low (Consumer choice).
domain_priors:theater_ratio(canada_goose_2026, 0.60).      % High "Brand Heat" focus.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(canada_goose_2026, extractiveness, 0.25).
narrative_ontology:constraint_metric(canada_goose_2026, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(canada_goose_2026, theater_ratio, 0.6).

% Defined by the fiscal Q3 period.
narrative_ontology:has_sunset_clause(canada_goose_2026).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

% PERSPECTIVE 1: THE INVESTOR (ROPE)
% Investors view Bourke's strategy as a Rope—coordinating cost 
% discipline and market expansion.
constraint_indexing:constraint_classification(canada_goose_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(historical), 
            exit_options(mobile), 
            spatial_scope(regional))).

% PERSPECTIVE 2: THE MARKET (SCAFFOLD)
% Because this is a specific fiscal realignment phase, it is 
% classified as a Scaffold.
constraint_indexing:constraint_classification(canada_goose_2026, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))) :-
    narrative_ontology:has_sunset_clause(canada_goose_2026).

narrative_ontology:interval(canada_goose_2026, 0, 10).
