% ============================================================================
% CONSTRAINT STORY: happiness_of_others
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_happiness_of_others, []).

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
 *   constraint_id: happiness_of_others
 *   human_readable: The Social Responsibility for the Happiness of Others
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint models the social norm that one is responsible for, and
 *   can directly cause, the happiness of another person. This expectation,
 *   while seemingly benign, can lead to various forms of manipulation,
 *   codependency, and emotional exploitation. The perceived social obligation
 *   to make others happy places a burden on individuals, often at the expense
 *   of their own well-being.
 *
 * KEY AGENTS:
 *   - Empathetic Individuals: Primary victim (powerless/trapped) - Feel obligated to make others happy, leading to exploitation.
 *   - Codependents: Secondary victim (moderate/constrained) - Derive self-worth from making others happy, limiting exit options.
 *   - Manipulators/Coercers: Primary beneficiary (institutional/arbitrage) - Leverage the social expectation for personal gain.
 *   - Analytical Observer: Detached observer (analytical/analytical) - Assesses the balance between social cohesion and individual exploitation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(happiness_of_others, 0.55).
domain_priors:suppression_score(happiness_of_others, 0.6).
domain_priors:theater_ratio(happiness_of_others, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(happiness_of_others, extractiveness, 0.55).
narrative_ontology:constraint_metric(happiness_of_others, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(happiness_of_others, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(happiness_of_others, tangled_rope).
narrative_ontology:human_readable(happiness_of_others, "The Social Responsibility for the Happiness of Others").
narrative_ontology:topic_domain(happiness_of_others, "social/psychological").

domain_priors:requires_active_enforcement(happiness_of_others).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(happiness_of_others, manipulators).
narrative_ontology:constraint_beneficiary(happiness_of_others, coercers).
narrative_ontology:constraint_victim(happiness_of_others, empathetic_individuals).
narrative_ontology:constraint_victim(happiness_of_others, codependents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individuals with high empathy feel trapped by the social expectation to be responsible for the happiness of others, leading to exploitation and emotional exhaustion. The constant pressure to fulfill others' needs suppresses their own well-being, leaving them with limited exit options and high extraction.
constraint_indexing:constraint_classification(happiness_of_others, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Codependent individuals are constrained by their relationships, where they derive their self-worth from making others happy. This dynamic creates a tangled rope, as they experience both extraction (emotional drain, neglect of self) and coordination (validation, a sense of purpose). Their exit options are limited by psychological dependence.
constraint_indexing:constraint_classification(happiness_of_others, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% The narrative that one is responsible for the happiness of others is a rope for those who leverage it to control and manipulate others. Media and other cultural forces benefit from this social expectation. They are able to arbitrage relationships and social standing based on the perception that they are the key to someone else's happiness. This is a coordination mechanism for maintaining social cohesion, albeit with extractive side effects.
constraint_indexing:constraint_classification(happiness_of_others, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From an analytical perspective, this social expectation is a tangled rope, balancing the need for social cohesion with the risk of individual exploitation. The narrative supports coordinated social bonds but leads to extraction from those who bear a disproportionate burden of others' emotional well-being.
constraint_indexing:constraint_classification(happiness_of_others, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(happiness_of_others_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(happiness_of_others, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(happiness_of_others, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(happiness_of_others, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(happiness_of_others_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The constraint extracts emotional energy, time, and personal resources from those who feel responsible for others' happiness. Suppression (0.60): Individuals often suppress their own needs and boundaries to conform to the social expectation and avoid guilt or judgment. Theater ratio (0.40): While there may be performative aspects of showing care and concern, the underlying expectation is often enforced through genuine emotional manipulation and codependent relationships.
 *
 * PERSPECTIVAL GAP:
 *   The empathetic individual experiences the constraint as a snare, feeling trapped and exploited. The codependent individual sees a tangled rope, where they derive a sense of purpose but also suffer emotional exhaustion. Those who leverage the expectation may see it as a rope - a coordination mechanism to foster social bonds or maintain power. The analytical observer sees the tangled rope, recognizing the inherent trade-offs between social cohesion and individual autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries leverage the expectation for control or validation, while victims experience emotional and personal costs. The power dynamics within relationships and social structures dictate who benefits and who bears the burden of this social norm.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint must be differentiated from genuine acts of kindness and compassion. The extraction lies in the expectation and obligation, rather than voluntary acts of support. The key is to distinguish between empathy-driven behavior (which can be healthy) and obligation-driven behavior (which can lead to exploitation and burnout). This framework prevents mislabeling healthy relationship as purely extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_autonomy_vs_social_cohesion,
    'What is the optimal balance between individual autonomy and the social expectation of contributing to others'' happiness?',
    'Cross-cultural studies and philosophical inquiry into ethical frameworks that prioritize both individual rights and social responsibility.',
    'If individual autonomy is prioritized: Reduced social cohesion and increased isolation. If social expectation dominates: Increased risk of exploitation and emotional burnout.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_autonomy_vs_social_cohesion, conceptual, 'Balance between individual autonomy and social expectations.').

omega_variable(
    definition_of_happiness,
    'How is ''happiness'' defined in this social context, and who gets to define it?',
    'Sociological research into the cultural and individual variations in understanding and expressing happiness.',
    'If happiness is narrowly defined and imposed: Increased social pressure and conformity. If happiness is broadly and individually defined: Reduced social pressure but potential for social fragmentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_happiness, conceptual, 'Definitional uncertainty of happiness in social context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(happiness_of_others, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(happ_tr_t0, happiness_of_others, theater_ratio, 0, 0.25).
narrative_ontology:measurement(happ_tr_t5, happiness_of_others, theater_ratio, 5, 0.35).
narrative_ontology:measurement(happ_tr_t10, happiness_of_others, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(happ_be_t0, happiness_of_others, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(happ_be_t5, happiness_of_others, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(happ_be_t10, happiness_of_others, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(happiness_of_others, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
