% ============================================================================
% CONSTRAINT STORY: ulysses_chp05
% ============================================================================
% Version: 0.2 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp05, []).

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
 *   constraint_id: ulysses_chp05
 *   human_readable: The Narcotic Social Rope (Lotus Eaters)
 *   domain: social/religious/technological
 *
 * SUMMARY:
 *   Leopold Bloom navigates the "lethargy" and "flowers of idleness" of 1904
 *   Dublin, encountering social and religious norms that offer comfort but
 *   also restrict individual freedom and potential. The Lotus Eaters episode
 *   explores the subtle coercion of social and religious institutions that,
 *   while providing a sense of belonging, can also trap individuals in cycles
 *   of complacency and stagnation.
 *
 * KEY AGENTS:
 *   - Isolated Individual: The individual trapped in a cycle of loneliness and stagnation (powerless/trapped).
 *   - Established Social Groups: Groups benefiting from maintaining the status quo (institutional/constrained).
 *   - Religious Institutions: Institutions benefiting from maintaining the status quo (institutional/constrained).
 *   - Detached Observer: The analyst observing the dynamics (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp05, 0.35).
domain_priors:suppression_score(ulysses_chp05, 0.25).
domain_priors:theater_ratio(ulysses_chp05, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp05, extractiveness, 0.35).
narrative_ontology:constraint_metric(ulysses_chp05, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(ulysses_chp05, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp05, rope).
narrative_ontology:human_readable(ulysses_chp05, "The Narcotic Social Rope (Lotus Eaters)").
narrative_ontology:topic_domain(ulysses_chp05, "social/religious/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp05, established_social_groups).
narrative_ontology:constraint_beneficiary(ulysses_chp05, religious_institutions).
narrative_ontology:constraint_victim(ulysses_chp05, isolated_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The isolated individual, lacking social connections, is trapped in a cycle of loneliness and stagnation, unable to escape the inertia of their situation. The individual experiences the Lotus Eaters constraint as a snare, as they are unable to break free from the allure of these fleeting comforts and build meaningful connections.
constraint_indexing:constraint_classification(ulysses_chp05, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Established social groups and institutions benefit from maintaining the status quo, as it reinforces their power and influence. Social groups can exert influence over individuals, subtly discouraging them from seeking alternatives.
constraint_indexing:constraint_classification(ulysses_chp05, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% The detached observer, analyzing the social dynamics from a distance, recognizes the complex interplay of factors that contribute to the perpetuation of the Narcotic Social Rope. Viewing this as a coordination mechanism for minimizing short-term individual anxiety and maximizing long-term societal predictability.
constraint_indexing:constraint_classification(ulysses_chp05, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp05_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp05, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp05, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(ulysses_chp05_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Lotus Eaters scenario represents a moderate extractiveness (0.35) due to the subtle nature of social and religious coercion. Individuals are not overtly forced to conform, but the pressure to fit in and the fear of social isolation can be powerful motivators. Suppression (0.25) is moderate as well; there are alternatives, but they are not always easily accessible or socially acceptable. Theater (0.40) is also moderate, as the outward rituals and practices of social and religious institutions often serve a deeper purpose of maintaining social order and reinforcing group identity.
 *
 * PERSPECTIVAL GAP:
 *   The isolated individual sees a Snare, trapped by their lack of connections and the difficulty of breaking free. Established groups and institutions see a Rope, a way to coordinate and maintain social order. The analytical observer sees the bigger picture and understands the interplay of factors that contribute to perpetuating the Narcotic Social Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Established social groups and religious institutions benefit as individuals are drawn into social norms.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling coordination as pure extraction by acknowledging that the Lotus Eaters constraint provides a real, albeit limited, sense of belonging and social connection. While it restricts individual freedom and potential, it also offers comfort and stability. It is a rope insofar as it provides a mechanism for maintaining social cohesion, although one that can be subtly coercive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp05, 1904, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp05, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
