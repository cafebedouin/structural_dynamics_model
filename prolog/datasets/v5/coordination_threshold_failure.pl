% ============================================================================
% CONSTRAINT STORY: coordination_threshold_failure
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(coordination_threshold_failure, []).

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
 * * constraint_id: coordination_threshold_failure
 * human_readable: The Critical Mass Chasm
 * domain: social/technological
 * * SUMMARY:
 * This constraint represents the structural "valley" where a network or 
 * protocol provides zero utility until a specific participant threshold is 
 * reached. It acts as a Snare for early adopters who pay the "participation 
 * tax" without reward, while the system remains a Mountain of inertia for 
 * the unaligned masses.
 * * KEY AGENTS:
 * - Early Adopter: Subject (Powerless)
 * - Protocol Foundation: Beneficiary (Institutional)
 * - Network Economist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.78) as early energy is consumed without generating 
% network value, effectively "burning" the optionality of the first-movers.
domain_priors:base_extractiveness(coordination_threshold_failure, 0.78). 
domain_priors:suppression_score(coordination_threshold_failure, 0.62).
domain_priors:theater_ratio(coordination_threshold_failure, 0.35).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(coordination_threshold_failure, extractiveness, 0.78).
narrative_ontology:constraint_metric(coordination_threshold_failure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(coordination_threshold_failure, theater_ratio, 0.35).

% This is classified as a Scaffold because it is intended to be temporary 
% until the threshold is cleared.
narrative_ontology:has_sunset_clause(coordination_threshold_failure). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the early adopter, the lack of coordination is a snare: they are 
% committed to a protocol that currently provides no exit or return.
constraint_indexing:constraint_classification(coordination_threshold_failure, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views this as a necessary Rope—a coordination ritual 
% that filters for high-conviction participants to ensure long-term stability.
constraint_indexing:constraint_classification(coordination_threshold_failure, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ARCHITECT (SCAFFOLD)
% Because a sunset clause exists (the threshold), it is a temporary Scaffold 
% supporting the transition to a high-utility network.
constraint_indexing:constraint_classification(coordination_threshold_failure, scaffold, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(continental))) :-
    narrative_ontology:has_sunset_clause(coordination_threshold_failure).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.78) triggers the hybrid Tangled Rope signature, 
% identifying the predatory nature of the pre-threshold state.
constraint_indexing:constraint_classification(coordination_threshold_failure, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(coordination_threshold_failure, E), E >= 0.50,
    domain_priors:suppression_score(coordination_threshold_failure, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coordination_threshold_failure_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject and Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(coordination_threshold_failure, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coordination_threshold_failure, rope, 
        context(agent_power(institutional), _, _, _)).

test(scaffold_validation) :-
    % Ensure that the presence of a sunset clause triggers the Scaffold classification.
    constraint_indexing:constraint_classification(coordination_threshold_failure, scaffold, 
        context(agent_power(organized), _, _, _)).

:- end_tests(coordination_threshold_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.78) represents the 'Mandatrophy' threshold where 
 * the lack of coordination consumes the resources of the early participants.
 * * PERSPECTIVAL GAP:
 * The Individual feels a Snare because they pay the cost of the threshold failure. 
 * The Institution feels a Rope because the threshold ensures the system's 
 * legitimacy and filtering coordination.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the Scaffold classification. The extraction is tolerated and 
 * indexed as a temporary support structure that is expected to decline 
 * once the coordination threshold is met and the sunset clause is triggered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_threshold_validity,
    'Is the threshold a mathematical necessity (Mountain) or an arbitrary barrier (Snare)?',
    'Simulation of network utility at varying participant densities (N=10 to N=10^6).',
    'If utility scales linearly: Snare of design. If utility is step-function: Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(coordination_threshold_failure, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
