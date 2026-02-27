% ============================================================================
% CONSTRAINT STORY: altruistic_misery_paradox_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_altruistic_misery_paradox_2026, []).

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
 *   constraint_id: altruistic_misery_paradox_2026
 *   human_readable: The Paradox of Altruistic Misery
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The paradox of altruistic misery describes the situation where
 *   individuals feel compelled by social pressures to prioritize the
 *   happiness and well-being of others through self-sacrifice, often to their
 *   own detriment. This creates a tension between the benefits of altruism
 *   for society and the potential costs for the individual.
 *
 * KEY AGENTS:
 *   - Altruistic Individuals: Primary target (powerless/trapped) - Bear the costs of self-sacrifice.
 *   - Recipients of Altruism: Primary beneficiary (powerful/mobile) - Benefit from the selflessness of others.
 *   - Society at Large: Secondary beneficiary (institutional/arbitrage) - Benefits from the overall increase in social cohesion and well-being facilitated by altruistic behavior.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(altruistic_misery_paradox_2026, 0.55).
domain_priors:suppression_score(altruistic_misery_paradox_2026, 0.6).
domain_priors:theater_ratio(altruistic_misery_paradox_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(altruistic_misery_paradox_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(altruistic_misery_paradox_2026, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(altruistic_misery_paradox_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(altruistic_misery_paradox_2026, tangled_rope).
narrative_ontology:human_readable(altruistic_misery_paradox_2026, "The Paradox of Altruistic Misery").
narrative_ontology:topic_domain(altruistic_misery_paradox_2026, "social/psychological").

domain_priors:requires_active_enforcement(altruistic_misery_paradox_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(altruistic_misery_paradox_2026, recipients_of_altruism).
narrative_ontology:constraint_beneficiary(altruistic_misery_paradox_2026, society_at_large).
narrative_ontology:constraint_victim(altruistic_misery_paradox_2026, altruistic_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The altruistic individual, feeling socially obligated to prioritize others' needs, often finds themselves trapped in a cycle of self-sacrifice and suppressed desires. They experience a high degree of extraction with limited means of escape.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Society benefits from altruistic behavior, as it fosters cooperation, mutual support, and social cohesion. From an institutional perspective, altruism is seen as a coordinating mechanism that promotes the well-being of the collective.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational perspective, the paradox of altruistic misery is a complex interplay between coordination and extraction. While altruism fosters social bonds and mutual support, it also creates a system where some individuals bear a disproportionate burden of self-sacrifice, leading to potential exploitation and unhappiness. The system requires active enforcement through social norms and expectations.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(altruistic_misery_paradox_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(altruistic_misery_paradox_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(altruistic_misery_paradox_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because while altruistic individuals do experience a loss of personal well-being, their actions contribute to a greater social good. Suppression is also high (0.60) because social norms and expectations often discourage individuals from prioritizing their own needs and desires.
 *
 * PERSPECTIVAL GAP:
 *   The altruistic individual experiences the constraint as a snare because they are trapped in a cycle of self-sacrifice. Society at large benefits and thus views altruism as a rope, a coordinating mechanism that promotes the well-being of the collective. The analytical observer sees the complex interplay between the benefits of altruism and the potential costs to the individual, resulting in a tangled rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic reflects the structural relationships between the agents. The altruistic individual, as the primary target, has a high 'd' value, indicating that they bear the majority of the costs. The recipients of altruism and society at large, as the beneficiaries, have low 'd' values, indicating that they receive the majority of the benefits. The analytical observer, having a global view, sees both the benefits and costs, resulting in an intermediate 'd' value.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that altruism is neither purely beneficial nor purely exploitative, but rather a complex mix of both. The tangled rope classification accurately reflects this complexity, acknowledging both the coordination aspects of altruism and the potential for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrinsic_vs_extrinsic_motivation,
    'To what extent is the altruistic behavior driven by intrinsic motivation (genuine empathy) versus extrinsic pressure (social obligation)?',
    'Psychological studies examining the correlation between reported altruistic behavior and measures of empathy, social anxiety, and fear of social disapproval.',
    'If primarily intrinsic: the constraint may be better characterized as a form of genuine coordination (rope). If primarily extrinsic: the constraint is more accurately described as a system of extraction (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_vs_extrinsic_motivation, empirical, 'Degree to which altruistic behavior is intrinsically or extrinsically motivated.').

omega_variable(
    cultural_variation_in_altruistic_expectations,
    'How do cultural norms and expectations regarding altruism vary across different societies and communities?',
    'Cross-cultural anthropological studies examining the prevalence and valuation of altruistic behavior in different societies.',
    'If cultural norms are highly demanding: the extraction experienced by altruistic individuals will be more severe (stronger snare). If cultural norms are more balanced: the extraction will be less pronounced (weaker snare or tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_variation_in_altruistic_expectations, empirical, 'Cultural variation in altruistic expectations and norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(altruistic_misery_paradox_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(altr_tr_t0, altruistic_misery_paradox_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(altr_tr_t5, altruistic_misery_paradox_2026, theater_ratio, 5, 0.3).
narrative_ontology:measurement(altr_tr_t10, altruistic_misery_paradox_2026, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(altr_be_t0, altruistic_misery_paradox_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(altr_be_t5, altruistic_misery_paradox_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(altr_be_t10, altruistic_misery_paradox_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(altruistic_misery_paradox_2026, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
