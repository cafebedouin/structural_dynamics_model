% ============================================================================
% CONSTRAINT STORY: second_order_unintended_consequences
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(second_order_unintended_consequences, []).

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
 * * constraint_id: second_order_unintended_consequences
 * human_readable: The Cobra Effect Trap
 * domain: social/economic/technological
 * * SUMMARY:
 * A scenario where a "Rope" designed to solve a primary problem (e.g., incentive 
 * structures, automated filtering, or optimization targets) creates unexpected 
 * secondary effects that worsen the systemic state. This coordination tool 
 * becomes a "Snare" for the subject, as the system liquidates their agency 
 * to address the original goal, trapping them in a territory where they must 
 * manage the fallout of the "solution" rather than the problem itself. 
 *
 * * KEY AGENTS:
 * - Policy Participant: Subject (Powerless)
 * - System Optimizer: Beneficiary (Institutional)
 * - Complexity Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.85) reflects the liquidation of primary goal-agency to 
% maintain the "optimized" but counter-productive state.
domain_priors:base_extractiveness(second_order_unintended_consequences, 0.85). 
domain_priors:suppression_score(second_order_unintended_consequences, 0.73). % Corrective feedback is suppressed by the inertia of the "Optimized" model.
domain_priors:theater_ratio(second_order_unintended_consequences, 0.90).    % Extreme theater: "Impact Reports" showing success in the 1st order while 2nd order failure occurs.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(second_order_unintended_consequences, extractiveness, 0.85).
narrative_ontology:constraint_metric(second_order_unintended_consequences, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(second_order_unintended_consequences, theater_ratio, 0.9).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The participant is trapped: the "solution" has made their primary task 
% harder, but they are mandated to follow the new protocol.
constraint_indexing:constraint_classification(second_order_unintended_consequences, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The Optimizer views the policy as a Rope—the essential coordination 
% substrate for achieving the primary target at scale.
constraint_indexing:constraint_classification(second_order_unintended_consequences, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.90) > 0.70 triggers Piton: the "Success Metric" 
% is an inertial spike; it signals progress while siphoning 0.85 of the agency.
constraint_indexing:constraint_classification(second_order_unintended_consequences, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.85) masking as functional coordination (Rope).
constraint_indexing:constraint_classification(second_order_unintended_consequences, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(second_order_unintended_consequences, E), E >= 0.50,
    domain_priors:suppression_score(second_order_unintended_consequences, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_order_consequences_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless participant vs Rope for the institutional optimizer.
    constraint_indexing:constraint_classification(second_order_unintended_consequences, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_order_unintended_consequences, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.90) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(second_order_unintended_consequences, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(second_order_unintended_consequences, E),

    E > 0.70.

:- end_tests(second_order_consequences_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of a primary optimization is achieved by liquidating 
 * the subject's agency to survive the secondary consequences.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Policy Participant feels a Snare because the "fix" has created a 
 * new, worse reality they cannot escape. The Optimizer sees a Rope 
 * because the primary metric is being met, coordinating institutional 
 * success regardless of the peripheral fallout.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Progress Report" is no longer functional (Theater 0.90); 
 * it is an inert spike siphoning 0.85 of the system's corrective surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_consequence_predictability,
    'Can digital twin simulations restore the Rope, or is complexity a "Mountain" of entropy (Snare vs Mountain)?',
    'Tracking the delta between simulated and actual 2nd-order outcomes in 2026-style market interventions.',
    'If simulations fail: Mountain of Chaos. If they hold: Snare of current modeling technique.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(second_order_unintended_consequences, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
