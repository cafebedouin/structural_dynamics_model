% ============================================================================
% CONSTRAINT STORY: pareto_principle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pareto_principle, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pareto_principle
 *   human_readable: The Pareto Principle (80/20 Rule)
 *   domain: statistical/economic/social
 *
 * SUMMARY:
 *   The Pareto Principle, also known as the 80/20 rule, is a statistical
 *   observation stating that, for many events, roughly 80% of the effects
 *   come from 20% of the causes. In economics, it might mean that 20% of
 *   customers generate 80% of revenue. In software development, 20% of the
 *   code causes 80% of the errors. The principle is used to highlight areas
 *   of focus for maximum impact. It is not a strict mathematical law but
 *   rather a general observation of distribution.
 *
 * KEY AGENTS:
 *   - Strategic Actors: Benefit from understanding and applying the principle (institutional/arbitrage)
 *   - The Overworked: Contribute disproportionately, may feel trapped (powerless/trapped)
 *   - Analytical Observer: Understands and interprets the principle (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pareto_principle, 0.3).
domain_priors:suppression_score(pareto_principle, 0.15).
domain_priors:theater_ratio(pareto_principle, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pareto_principle, extractiveness, 0.3).
narrative_ontology:constraint_metric(pareto_principle, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(pareto_principle, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pareto_principle, rope).
narrative_ontology:human_readable(pareto_principle, "The Pareto Principle (80/20 Rule)").
narrative_ontology:topic_domain(pareto_principle, "statistical/economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pareto_principle, strategic_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Pareto Principle, when understood and applied correctly, can be a useful tool for resource allocation and strategic decision-making. From this perspective, it facilitates coordination and efficiency.
constraint_indexing:constraint_classification(pareto_principle, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% For individuals contributing the 80%, the Pareto Principle may manifest as a sense of disproportionate effort yielding limited reward. While they may not be directly extracted from, their contributions are essential. Often, individuals in this role cannot exit the system.
constraint_indexing:constraint_classification(pareto_principle, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Organizations and individuals who understand and leverage the Pareto Principle can optimize their efforts by focusing on the 20% of activities that yield the most significant results. They benefit from the coordination and efficiency gains, and can exit if better opportunities arise.
constraint_indexing:constraint_classification(pareto_principle, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pareto_principle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pareto_principle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pareto_principle, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(pareto_principle, TR),
    TR >= 0.70.

:- end_tests(pareto_principle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.30) as the principle itself doesn't extract value but highlights existing distributions. Suppression is low (0.15) as there's no direct coercion; it's simply an observation. Theater ratio is now high (0.75) because the principle is often used performatively, with organizations claiming to apply it without truly understanding or acting on its implications.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap exists because the principle benefits those who leverage it for strategic advantage. Those contributing the majority of the effort (80%) may not perceive the same benefit and may feel that they carry a disproportionate load.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic actors are the beneficiaries, gaining efficiency and focus. The 'overworked' are not directly victims but may experience disproportionate effort, contributing to a sense of inequity. The analytical observer provides a neutral, objective assessment of the principle.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by recognizing the inherent distribution of effects. The Pareto Principle isn't a coercive force but an observation; those who strategically benefit are leveraging an existing pattern, not necessarily extracting unfairly. The 'overworked' are not actively suppressed but represent the other end of a distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pareto_principle, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pareto_principle, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
