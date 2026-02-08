% ============================================================================
% CONSTRAINT STORY: artificial_scarcity_scaffold
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(artificial_scarcity_scaffold, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: artificial_scarcity_scaffold
 * human_readable: The Resource-Migration Scaffold
 * domain: technological/economic
 * * SUMMARY:
 * This constraint represents a temporary period of enforced artificial scarcity 
 * designed to facilitate a transition from a legacy resource to a new, 
 * abundant alternative. It functions as a coordination mechanism that 
 * temporarily suppresses choice to prevent a "tragedy of the commons" 
 * during the migration phase.
 * * KEY AGENTS:
 * - Resource Consumer: Subject (Powerless)
 * - Management Protocol: Beneficiary (Institutional)
 * - Systems Architect: Architect (Organized)
 */

/* ==========================================================================
   2. BASE PROPERTIES (REVISED)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(artificial_scarcity_scaffold, 0.48). 
domain_priors:suppression_score(artificial_scarcity_scaffold, 0.75). 
domain_priors:theater_ratio(artificial_scarcity_scaffold, 0.20).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(artificial_scarcity_scaffold, extractiveness, 0.48).
narrative_ontology:constraint_metric(artificial_scarcity_scaffold, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(artificial_scarcity_scaffold, theater_ratio, 0.2).

% Constraint metric facts — primary keys used by the classification engine.
% Mandatory for Scaffold classification: A sunset clause exists for the transition.
narrative_ontology:has_sunset_clause(artificial_scarcity_scaffold).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the powerless agent, the high cost and lack of choice feel like a predatory trap.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views this as a vital coordination Rope for systemic stability.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ARCHITECT (SCAFFOLD)
% Suppression is tolerated because it is temporary and linked to a sunset clause.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, scaffold, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(continental))) :-
    narrative_ontology:has_sunset_clause(artificial_scarcity_scaffold).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(artificial_scarcity_scaffold_tests).

test(scaffold_trigger) :-
    % Verify that the presence of a sunset clause triggers the Scaffold classification.
    constraint_indexing:constraint_classification(artificial_scarcity_scaffold, scaffold, 
        context(agent_power(organized), _, _, _)).

test(threshold_validation) :-
    % Ensure base extraction meets high-extraction (>= 0.46) logic gates.
    domain_priors:base_extractiveness(artificial_scarcity_scaffold, E),
    E >= 0.46.

:- end_tests(artificial_scarcity_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score of 0.48 triggers the requirement for an Omega variable.
 * High suppression (0.75) is classified as a Scaffold rather than a Snare 
 * because the architect index acknowledges the sunset clause.
 * * PERSPECTIVAL GAP:
 * 
 * The Consumer experiences a Snare due to the immediate financial and 
 * choice-based friction. The Architect sees a Scaffold—a 
 * temporary, necessary structure that supports the "bridge" to abundance.
 * * [RESOLVED MANDATROPHY]:
 * The classification of 'Scaffold' prevents the system from mislabeling 
 * this high-suppression state as pure, permanent extraction (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_scarcity_sunset,
    'Will the scarcity expire as scheduled, or will the "Scaffold" become a permanent "Piton"?',
    'Auditing transition milestones against the hard-coded sunset timestamp.',
    'If sunset honored: Transition succeeds. If delayed: Transition to Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing and structural linter.
narrative_ontology:interval(artificial_scarcity_scaffold, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time: Modeling a functional start (0.10) drifting toward 
% the current base management (0.20) as the scaffold matures.
narrative_ontology:measurement(scarcity_tr_t0, artificial_scarcity_scaffold, theater_ratio, 0, 0.10).
narrative_ontology:measurement(scarcity_tr_t5, artificial_scarcity_scaffold, theater_ratio, 5, 0.15).
narrative_ontology:measurement(scarcity_tr_t10, artificial_scarcity_scaffold, theater_ratio, 10, 0.20).

% Extraction over time: Tracking the ramp-up of resource redirection/fees 
% during the 10-year migration interval.
narrative_ontology:measurement(scarcity_ex_t0, artificial_scarcity_scaffold, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(scarcity_ex_t5, artificial_scarcity_scaffold, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(scarcity_ex_t10, artificial_scarcity_scaffold, base_extractiveness, 10, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
