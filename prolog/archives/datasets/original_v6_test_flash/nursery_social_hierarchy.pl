% ============================================================================
% CONSTRAINT STORY: nursery_social_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nursery_social_hierarchy, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nursery_social_hierarchy
 *   human_readable: Social Hierarchy of Playthings
 *   domain: social/economic
 *
 * SUMMARY:
 *   A status-based framework within the nursery where mechanical complexity
 *   is equated with "reality." This system extracts dignity from simple toys
 *   to fuel the social dominance of modern, technical objects.
 *
 * KEY AGENTS:
 *   - Complex Toys: Beneficiary (institutional/arbitrage)
 *   - Dominant Children: Beneficiary (powerful/mobile)
 *   - Simple Toys: Victim (powerless/trapped)
 *   - Submissive Children: Victim (powerless/trapped)
 *   - Analytical Observer: Neutral observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nursery_social_hierarchy, 0.6).
domain_priors:suppression_score(nursery_social_hierarchy, 0.7).
domain_priors:theater_ratio(nursery_social_hierarchy, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nursery_social_hierarchy, extractiveness, 0.6).
narrative_ontology:constraint_metric(nursery_social_hierarchy, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(nursery_social_hierarchy, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nursery_social_hierarchy, tangled_rope).
narrative_ontology:human_readable(nursery_social_hierarchy, "Social Hierarchy of Playthings").
narrative_ontology:topic_domain(nursery_social_hierarchy, "social/economic").

domain_priors:requires_active_enforcement(nursery_social_hierarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nursery_social_hierarchy, complex_toys).
narrative_ontology:constraint_beneficiary(nursery_social_hierarchy, dominant_children).
narrative_ontology:constraint_victim(nursery_social_hierarchy, simple_toys).
narrative_ontology:constraint_victim(nursery_social_hierarchy, submissive_children).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Simple toys are trapped in their low status, unable to change their perceived lack of complexity.
constraint_indexing:constraint_classification(nursery_social_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Submissive children are trapped in their low social status, unable to challenge the dominant children and their association with 'superior' toys.
constraint_indexing:constraint_classification(nursery_social_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Complex toys benefit from the social hierarchy, gaining prestige and dominance.
constraint_indexing:constraint_classification(nursery_social_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Dominant children benefit from the social hierarchy by wielding the complex toys to assert their dominance.
constraint_indexing:constraint_classification(nursery_social_hierarchy, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% Analytical observer sees the entangled nature of the hierarchy, with both benefits and costs.
constraint_indexing:constraint_classification(nursery_social_hierarchy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nursery_social_hierarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nursery_social_hierarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nursery_social_hierarchy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nursery_social_hierarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nursery_social_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate, as the hierarchy isn't absolute but strongly favors complexity. Suppression is high, as simple toys and submissive children have difficulty escaping their low status.
 *
 * PERSPECTIVAL GAP:
 *   The simple toys and submissive children experience pure extraction, while complex toys and dominant children see the hierarchy as a system of coordination that benefits them.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by whether the agent benefits or is victimized by the social hierarchy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_definition,
    'What exactly constitutes ''complexity'' in the eyes of a child? Is it purely mechanical, or does perceived complexity factor in?',
    'Observational studies of children interacting with toys of varying complexity, surveys assessing their perceptions.',
    'If perceived complexity dominates, the hierarchy could shift based on marketing or narratives. If purely mechanical, interventions become more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complexity_definition, empirical, 'Definition of complexity and its impact on the hierarchy.').

omega_variable(
    hierarchy_permanence,
    'Is the nursery hierarchy a temporary phase or does it have lasting effects on social development?',
    'Longitudinal studies tracking the social trajectories of children involved in the nursery hierarchy.',
    'If temporary, intervention is less critical. If permanent, strategies for dismantling the hierarchy become crucial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hierarchy_permanence, empirical, 'Long-term effects of the nursery social hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nursery_social_hierarchy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nurs_tr_t0, nursery_social_hierarchy, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nurs_tr_t5, nursery_social_hierarchy, theater_ratio, 5, 0.3).
narrative_ontology:measurement(nurs_tr_t10, nursery_social_hierarchy, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(nurs_be_t0, nursery_social_hierarchy, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(nurs_be_t5, nursery_social_hierarchy, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(nurs_be_t10, nursery_social_hierarchy, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nursery_social_hierarchy, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
