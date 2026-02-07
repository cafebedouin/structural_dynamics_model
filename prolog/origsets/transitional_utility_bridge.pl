% ============================================================================
% CONSTRAINT STORY: transitional_utility_bridge
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(transitional_utility_bridge, []).

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
 * * constraint_id: transitional_utility_bridge
 * human_readable: The Epochal Migration Scaffold
 * domain: technological/economic
 * * SUMMARY:
 * A temporary regulatory and technical subsidy designed to move a population 
 * from a carbon-intensive legacy grid to a decentralized renewable network. 
 * It imposes high costs and limited choices (Suppression) but is bound by a 
 * hard-coded termination date once the new network reaches critical mass.
 * * KEY AGENTS:
 * - Resident Consumer: Subject (Powerless)
 * - Transition Authority: Beneficiary (Institutional)
 * - Systems Architect: Architect (Organized)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is significant (0.48) to fund the transition, triggering the Omega requirement.
domain_priors:base_extractiveness(transitional_utility_bridge, 0.48). 
domain_priors:suppression_score(transitional_utility_bridge, 0.75). % High coercion during migration.
domain_priors:theater_ratio(transitional_utility_bridge, 0.30).    % Low theater; the function is active and temporary.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(transitional_utility_bridge, extractiveness, 0.48).
narrative_ontology:constraint_metric(transitional_utility_bridge, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(transitional_utility_bridge, theater_ratio, 0.3).

% Mandatory for Scaffold classification.
narrative_ontology:has_sunset_clause(transitional_utility_bridge). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The resident feels the weight of high surcharges and lack of alternative providers.
constraint_indexing:constraint_classification(transitional_utility_bridge, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The Authority views the bridge as vital coordination to prevent grid collapse.
constraint_indexing:constraint_classification(transitional_utility_bridge, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ARCHITECT (SCAFFOLD)
% High suppression is explicitly tolerated because it declines over the time horizon.
constraint_indexing:constraint_classification(transitional_utility_bridge, scaffold, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(continental))) :-
    narrative_ontology:has_sunset_clause(transitional_utility_bridge).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transitional_utility_bridge_tests).

test(scaffold_identification) :-
    % Verify the constraint is identified as a Scaffold for the organized architect.
    constraint_indexing:constraint_classification(transitional_utility_bridge, scaffold, 
        context(agent_power(organized), _, _, _)).

test(sunset_dependency) :-
    % Ensure Scaffold status fails if the sunset clause is removed.
    \+ (retract(narrative_ontology:has_sunset_clause(transitional_utility_bridge)),
        constraint_indexing:constraint_classification(transitional_utility_bridge, scaffold, _)).

:- end_tests(transitional_utility_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Assigned 0.48 extraction to reflect the 'participation tax' required for migration.
 * The high suppression score (0.75) reflects the active closing of legacy 
 * options to force the transition.
 * * PERSPECTIVAL GAP:
 * The Resident feels a Snare due to immediate financial pressure. The Architect 
 * sees a Scaffold because the constraint is self-liquidating.
 * * [RESOLVED MANDATROPHY]:
 * The classification as a Scaffold prevents this from being mislabeled as 
 * pure extraction (Snare), as the system acknowledges the coordination 
 * intent is tied to a sunset clause.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for base_extractiveness > 0.46.
omega_variable(
    omega_sunset_integrity,
    'Will the sunset clause be honored or will the bridge become a permanent Piton?',
    'Auditing the institutional reserve funds vs. progress toward the threshold.',
    'If honored: Transition succeeds. If extended: Systemic extraction (Snare).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(transitional_utility_bridge, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
