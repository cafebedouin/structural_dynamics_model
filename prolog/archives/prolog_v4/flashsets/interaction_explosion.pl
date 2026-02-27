% ============================================================================
% CONSTRAINT STORY: interaction_explosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_interaction_explosion, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: interaction_explosion
 *   human_readable: The Combinatorial Complexity Trap
 *   domain: technological/social
 *
 * SUMMARY:
 *   The Combinatorial Complexity Trap arises when multiple simple
 *   coordination mechanisms interact non-linearly, leading to a feedback
 *   explosion of emergent constraints. This can occur in various
 *   technological and social systems, such as online platforms, software
 *   development projects, and organizational structures. The increasing
 *   complexity can overwhelm users, developers, and managers, leading to
 *   decreased efficiency, increased instability, and potential system
 *   failure.
 *
 * KEY AGENTS:
 *   - Early Adopters: Benefit initially but become constrained by complexity (moderate/constrained).
 *   - Late Adopters: Trapped by network effects and bear the cost of complexity (powerless/trapped).
 *   - Platform Owners: Benefit from engagement but responsible for stability (institutional/arbitrage).
 *   - System Stability: The overall health of the system as the victim (powerless/trapped).
 *   - Analytical Observer: Sees the system as a tangled rope (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(interaction_explosion, 0.55).
domain_priors:suppression_score(interaction_explosion, 0.6).
domain_priors:theater_ratio(interaction_explosion, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(interaction_explosion, extractiveness, 0.55).
narrative_ontology:constraint_metric(interaction_explosion, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(interaction_explosion, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(interaction_explosion, tangled_rope).
narrative_ontology:human_readable(interaction_explosion, "The Combinatorial Complexity Trap").
narrative_ontology:topic_domain(interaction_explosion, "technological/social").

domain_priors:requires_active_enforcement(interaction_explosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(interaction_explosion, early_adopters).
narrative_ontology:constraint_beneficiary(interaction_explosion, platform_owners).
narrative_ontology:constraint_victim(interaction_explosion, late_adopters).
narrative_ontology:constraint_victim(interaction_explosion, system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of late adopters who are forced to participate in the complex system without fully understanding it. They are trapped by the network effects and bear the costs of increased complexity and potential instability.
constraint_indexing:constraint_classification(interaction_explosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Early adopters benefit initially from the increased functionality and network effects, but they are also constrained by the increasing complexity and the potential for system failure. They experience a mix of coordination and extraction.
constraint_indexing:constraint_classification(interaction_explosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Platform owners benefit from the increased engagement and data generated by the complex system. They have the ability to arbitrage the system and profit from its growth, but also bear the responsibility for maintaining its stability.
constraint_indexing:constraint_classification(interaction_explosion, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer sees the system as a tangled rope, recognizing the interplay of coordination and extraction. They understand the emergent constraints and the potential for instability, but also appreciate the value and innovation that the complex system provides.
constraint_indexing:constraint_classification(interaction_explosion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(interaction_explosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(interaction_explosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(interaction_explosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(interaction_explosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(interaction_explosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.55) because while there are significant benefits to the platform owner and early adopters, the late adopters bear the costs of navigating a complex system. Suppression is also moderate (0.60) as the complex interactions make it difficult for users to understand or exit the system.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different positions within the system. Platform owners see the system as a coordination mechanism (rope), while late adopters experience it as a snare. Early adopters and the analytical observer recognize the tangled nature of the system.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (early adopters, platform owners) receive value and exhibit lower directionality. Victims (late adopters, system stability) experience higher directionality. Platform owners have more exit options as they are at the center of the design and can respond or redirect the system, while late adopters are trapped within the system's limitations.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the system as pure coordination by recognizing the negative impacts on late adopters and overall system stability. It also prevents mislabeling it as pure extraction by acknowledging the benefits and innovation that the complex system provides.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identifying_critical_threshold,
    'At what point does the complexity of the interacting mechanisms exceed a manageable threshold?',
    'Empirical analysis of system performance and user experience as new mechanisms are added.',
    'Identifying the threshold will allow for better governance and management of the complex system. Exceeding the threshold may lead to system collapse or widespread user dissatisfaction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identifying_critical_threshold, empirical, 'Determines the point at which complexity becomes unmanageable.').

omega_variable(
    predicting_emergent_behavior,
    'Can we effectively predict the emergent behavior of interacting coordination mechanisms?',
    'Developing more sophisticated models and simulations of complex systems.',
    'Improved predictability will allow for proactive mitigation of negative consequences and better design of complex systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(predicting_emergent_behavior, conceptual, 'Assess the ability to forecast emergent system behaviors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(interaction_explosion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inte_tr_t0, interaction_explosion, theater_ratio, 0, 0.1).
narrative_ontology:measurement(inte_tr_t5, interaction_explosion, theater_ratio, 5, 0.2).
narrative_ontology:measurement(inte_tr_t10, interaction_explosion, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(inte_be_t0, interaction_explosion, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(inte_be_t5, interaction_explosion, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(inte_be_t10, interaction_explosion, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(interaction_explosion, global_infrastructure).
narrative_ontology:affects_constraint(interaction_explosion, network_effect_lockin).
narrative_ontology:affects_constraint(interaction_explosion, information_overload).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
