% ============================================================================
% CONSTRAINT STORY: second_order_unintended_consequences
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_second_order_unintended_consequences, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

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

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(second_order_unintended_consequences, extractiveness, 0.85).
narrative_ontology:constraint_metric(second_order_unintended_consequences, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(second_order_unintended_consequences, theater_ratio, 0.9).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(second_order_unintended_consequences, piton).
narrative_ontology:human_readable(second_order_unintended_consequences, "The Cobra Effect Trap").
narrative_ontology:topic_domain(second_order_unintended_consequences, "social/economic/technological").

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(second_order_unintended_consequences).
narrative_ontology:constraint_beneficiary(second_order_unintended_consequences, system_optimizer).
narrative_ontology:constraint_victim(second_order_unintended_consequences, policy_participant).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
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
% Detects high extraction (0.85) and suppression (0.73) masking as functional
% coordination (Rope), correctly identifying it as a Tangled Rope.
constraint_indexing:constraint_classification(second_order_unintended_consequences, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_order_consequences_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless participant vs Rope for the institutional optimizer.
    constraint_indexing:constraint_classification(second_order_unintended_consequences, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_order_unintended_consequences, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(second_order_unintended_consequences, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.90) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(second_order_unintended_consequences, piton,
        context(agent_power(analytical), _, _, _)).

:- end_tests(second_order_consequences_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This model represents a classic "Cobra Effect" or Goodhart's Law scenario.
 * The base extraction of 0.85 is extremely high, representing the total
 * liquidation of an agent's ability to pursue their actual goals in favor of
 * servicing a proxy metric that has become counter-productive. The high
 * suppression (0.73) shows that the system actively resists feedback that
 * would correct this, while the extreme theater (0.90) shows that the system's
 * primary output is now performative reporting on the broken metric.
 *
 * * PERSPECTIVAL GAP:
 * The Policy Participant feels a Snare because the "fix" has created a
 * new, worse reality they cannot escape. The Optimizer sees a Rope
 * because the primary metric is being met, coordinating institutional
 * success regardless of the peripheral fallout. The Analytical observer sees
 * both a Piton (the performative metric) and a Tangled Rope (the overall
 * extractive-coordinative structure).
 *
 * * [RESOLVED MANDATROPHY]:
 * The extreme extraction (0.85) represents a Mandatrophy state. This is
 * resolved not by lowering the score, but by using the classification system
 * to correctly identify the pathology. The system is not just a Snare; it's a
 * combination of a Tangled Rope (a coordination mechanism that has become
 * asymmetrically extractive) and a Piton (an inert, theatrical goalpost).
 * This nuanced classification prevents mislabeling the system's failure as
 * simple malice (Snare) or its claimed function as reality (Rope).
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
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint began as a well-intentioned policy (low extraction/theater)
% but degraded as second-order effects were ignored and performative compliance
% took over, leading to metric substitution and extraction accumulation.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(sou_tr_t0, second_order_unintended_consequences, theater_ratio, 0, 0.10).
narrative_ontology:measurement(sou_tr_t5, second_order_unintended_consequences, theater_ratio, 5, 0.50).
narrative_ontology:measurement(sou_tr_t10, second_order_unintended_consequences, theater_ratio, 10, 0.90).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(sou_ex_t0, second_order_unintended_consequences, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(sou_ex_t5, second_order_unintended_consequences, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(sou_ex_t10, second_order_unintended_consequences, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The incentive structure acts as an enforcement mechanism for behavior.
narrative_ontology:coordination_type(second_order_unintended_consequences, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */