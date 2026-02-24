% ============================================================================
% CONSTRAINT STORY: path_dependence_lock_in
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(path_dependence_lock_in, []).

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
 * * constraint_id: path_dependence_lock_in
 * human_readable: The QWERTY Inertia Trap
 * domain: technological/economic
 * * SUMMARY:
 * This constraint represents the structural inability to pivot to a superior 
 * standard because the accumulated network effects and sunk costs of the 
 * current (inferior) path make the cost of transition prohibitive. It 
 * functions as a Mountain for users but is maintained as a Rope for the 
 * coordination of the existing market ecosystem.
 * * KEY AGENTS:
 * - New Market Entrant: Subject (Powerless)
 * - Legacy Standard Holder: Beneficiary (Institutional)
 * - Systems Archeologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.65) because the lock-in forces a "competence tax" on 
% all participants who must use suboptimal tools.
domain_priors:base_extractiveness(path_dependence_lock_in, 0.65). 
domain_priors:suppression_score(path_dependence_lock_in, 0.78). % Alternatives are visible but economically invisible.
domain_priors:theater_ratio(path_dependence_lock_in, 0.45).    % Moderate theater; it is a structural reality.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(path_dependence_lock_in, extractiveness, 0.65).
narrative_ontology:constraint_metric(path_dependence_lock_in, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(path_dependence_lock_in, theater_ratio, 0.45).

% This is not a scaffold; it is a permanent byproduct of historical contingency.
% narrative_ontology:has_sunset_clause(path_dependence_lock_in). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To the powerless individual, the path-dependence is an immutable law of the environment.
constraint_indexing:constraint_classification(path_dependence_lock_in, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutional actors view the lock-in as a Rope—a reliable coordination standard that lowers transaction costs.
constraint_indexing:constraint_classification(path_dependence_lock_in, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.65) and suppression (0.78) trigger the hybrid Tangled Rope signature.
constraint_indexing:constraint_classification(path_dependence_lock_in, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(path_dependence_lock_in, E), E >= 0.50,
    domain_priors:suppression_score(path_dependence_lock_in, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(path_dependence_lock_in_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Mountain for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(path_dependence_lock_in, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(path_dependence_lock_in, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(path_dependence_lock_in, E),

    (E =< 0.05 -> true ; E >= 0.46). % Ensure high extraction is correctly modeled.

:- end_tests(path_dependence_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.65) represents the "shadow cost" of using a 
 * suboptimal legacy standard compared to the theoretical ideal.
 * * PERSPECTIVAL GAP:
 * The Individual feels a Mountain because the "gravity" of the network 
 * effect is inescapable. The Institution sees a Rope because the standard 
 * provides a predictable environment for global trade and development.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via Tangled Rope classification. This recognizes that the lock-in 
 * is not just a "trap" (Snare) but a foundational coordination layer that 
 * allows the modern economy to function, even if it is inefficient.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_lock_in_origin,
    'Is the lock-in a result of natural path-contingency (Mountain) or intentional anti-competitive design (Snare)?',
    'Historical standard-setting minutes and early-market adoption patterns.',
    'If contingent: Mountain. If designed: Snare of architecture.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(path_dependence_lock_in, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
