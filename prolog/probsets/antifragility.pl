% ============================================================================
% CONSTRAINT STORY: antifragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_antifragility, []).

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
 *   constraint_id: antifragility
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that increase in capability or resilience
 *   in response to stressors and volatility. It involves adaptation,
 *   evolution, and the exploitation of disorder. This constraint benefits
 *   adaptive systems and innovators, while potentially harming fragile
 *   incumbents who are unable to adapt. The effect can be perceived
 *   differently based on the agent and timescale. The interplay between these
 *   perspectives is a key aspect of understanding the dynamics of
 *   antifragility.
 *
 * KEY AGENTS:
 *   - Adaptive Systems: Beneficiary (institutional/arbitrage) - Gain resilience and capability from stressors.
 *   - Fragile Incumbents: Victim (powerless/trapped) - Suffer from disorder and volatility.
 *   - Innovators: Mixed (moderate/mobile) - Benefit from opportunities, face risks.
 *   - Long-Term Observer: Analytical viewpoint of evolution.
 *   - Government Intervention: Support mechanisms to scaffold progress
 *   - Established Institutions: Obsolete structures that turn from asset to liability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility, 0.35).
domain_priors:suppression_score(antifragility, 0.2).
domain_priors:theater_ratio(antifragility, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility, extractiveness, 0.35).
narrative_ontology:constraint_metric(antifragility, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(antifragility, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility, tangled_rope).
narrative_ontology:human_readable(antifragility, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility, adaptive_systems).
narrative_ontology:constraint_beneficiary(antifragility, innovators).
narrative_ontology:constraint_victim(antifragility, fragile_incumbents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From a long-term perspective, antifragility is a rope, representing the natural process of evolution and adaptation that drives systems towards greater resilience and robustness. Institutions may benefit as adaptive capability emerges.
constraint_indexing:constraint_classification(antifragility, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% For fragile incumbents, antifragility represents a snare, as they are the direct victims of the stressors and volatility that drive antifragile systems. They are often trapped by their existing structures and unable to adapt.
constraint_indexing:constraint_classification(antifragility, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% For innovators, antifragility is a tangled rope. They benefit from the opportunities created by disorder, but also face the risk of failure and displacement. Mobile and adaptable, they can navigate the volatility and extract value.
constraint_indexing:constraint_classification(antifragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% From the perspective of governmental entities, antifragility can be viewed as a scaffold. Short-term support mechanisms for struggling entities might become tools for long term adaptation and growth. These systems have an intended sunset when the adaptation is complete, though they often persist.
constraint_indexing:constraint_classification(antifragility, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% For established institutions, antifragility can become a piton. What once was a helpful adaptation now becomes an obsolete structure, which presents an alternative vector for fragility that is no longer helpful. 
constraint_indexing:constraint_classification(antifragility, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% From an analytical viewpoint, the natural process of antifragility occurs regardless of intervention. As such, it is a mountain, something that cannot be changed.
constraint_indexing:constraint_classification(antifragility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(antifragility, TR),
    TR >= 0.70.

:- end_tests(antifragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): The extraction represents the resources and opportunities that are transferred from fragile systems to antifragile ones during periods of volatility. It's moderate because not all resources are lost, and some incumbents may adapt. Suppression (0.20): There is some suppression of alternatives as fragile systems are forced to compete with antifragile ones. However, this suppression is relatively low, as there are often multiple pathways for adaptation and innovation. Theater Ratio (0.75): The high theater ratio reflects that the scaffolding provided by government intervention is often performative, with limited functional aspects. Actual adaptation and increased resilience are the key outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap highlights the contrasting experiences of different agents. Fragile incumbents perceive antifragility as a snare, representing the loss of resources and opportunities. Innovators, on the other hand, see it as a tangled rope, with both risks and rewards. The long-term observer views it as a rope, representing the overall advancement of systems over time. This difference stems from their varying positions within the system and their ability to adapt and exploit volatility.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (adaptive systems and innovators) experience lower or negative extraction due to their ability to exploit disorder. Victims (fragile incumbents) experience high extraction as they bear the brunt of volatility. The analytical observer sees the overall process as a natural law. Governmental entities will act to prevent too much extractive action by providing a structured scaffolding for development.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how a seemingly positive concept (antifragility) can have negative consequences for certain agents. The mandatrophy lies in distinguishing between genuine adaptation and mere exploitation of others' misfortune. The analytical framework allows us to classify these distinct experiences and avoid mislabeling one as the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stressor_frequency,
    'What is the optimal frequency of stressors for promoting antifragility?',
    'Empirical analysis of systems under varying stressor frequencies.',
    'Determines whether systems become more resilient or collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stressor_frequency, empirical, 'The optimal frequency of stressors for antifragile systems.').

omega_variable(
    incumbent_adaptability,
    'To what extent can fragile incumbents adapt and become antifragile?',
    'Case studies of successful and unsuccessful adaptations by incumbents.',
    'Determines the degree to which antifragility is a disruptive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_adaptability, empirical, 'The adaptability of fragile incumbents to become antifragile.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility, theater_ratio, 0, 0.7).
narrative_ontology:measurement(anti_tr_t5, antifragility, theater_ratio, 5, 0.73).
narrative_ontology:measurement(anti_tr_t10, antifragility, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_be_t5, antifragility, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(anti_be_t10, antifragility, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility, resource_allocation).
narrative_ontology:affects_constraint(antifragility, resilience).
narrative_ontology:affects_constraint(antifragility, robustness).
narrative_ontology:affects_constraint(antifragility, stability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
