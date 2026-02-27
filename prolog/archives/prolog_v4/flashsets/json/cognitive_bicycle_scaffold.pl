% ============================================================================
% CONSTRAINT STORY: cognitive_bicycle_scaffold
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cognitive_bicycle_scaffold, []).

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
 *   constraint_id: cognitive_bicycle_scaffold
 *   human_readable: The Bicycle of the Mind
 *   domain: technological/cognitive
 *
 * SUMMARY:
 *   A tool-based constraint where an agent utilizes an AI assistant to
 *   amplify reasoning capacity. The constraint aims to enhance cognitive
 *   abilities but relies on the AI's accuracy and reliability. It is a
 *   scaffold as the tool ideally helps one to internalize new skills.
 *
 * KEY AGENTS:
 *   - reasoning_agent: Beneficiary, utilizes the AI for amplified reasoning (moderate/mobile)
 *   - AI_assistant_developer: Creator and maintainer of the AI tool (institutional/arbitrage)
 *   - independent_reasoning_skills: Victim, potential degradation due to over-reliance (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_bicycle_scaffold, 0.2).
domain_priors:suppression_score(cognitive_bicycle_scaffold, 0.1).
domain_priors:theater_ratio(cognitive_bicycle_scaffold, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, extractiveness, 0.2).
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_bicycle_scaffold, scaffold).
narrative_ontology:human_readable(cognitive_bicycle_scaffold, "The Bicycle of the Mind").
narrative_ontology:topic_domain(cognitive_bicycle_scaffold, "technological/cognitive").

narrative_ontology:has_sunset_clause(cognitive_bicycle_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cognitive_bicycle_scaffold, reasoning_agent).
narrative_ontology:constraint_victim(cognitive_bicycle_scaffold, independent_reasoning_skills).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The reasoning agent experiences the AI assistant as a temporary scaffold, amplifying their cognitive abilities. They have the option to stop using the tool.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, scaffold,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% The AI assistant developers experience the 'cognitive bicycle' as a form of coordination. They may transition to new tools at any point, thus 'arbitrage'.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Independent reasoning skills, as an abstract concept, are powerless and trapped. Over-reliance on the tool can degrade these skills, representing a snare.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_bicycle_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(cognitive_bicycle_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.2): The agent might become over-reliant on the tool, leading to a decline in their independent reasoning skills. Suppression (0.1): The agent is free to not use the tool. Theater Ratio (0.2): The stated function and actual function are relatively aligned
 *
 * PERSPECTIVAL GAP:
 *   The reasoning agent experiences the AI assistant as a scaffold, enhancing their capabilities and enabling them to perform complex tasks. The AI developer perceives the cognitive enhancement as a type of coordination, as it facilitates broader access to advanced reasoning tools and contributes to the growth of the AI ecosystem. The independent reasoning skills, however, may degrade over time due to over-reliance on the tool.
 *
 * DIRECTIONALITY LOGIC:
 *   The reasoning agent benefits from enhanced cognitive abilities. AI developer sees the tool as a positive feedback loop that allows for further iteration. Independent reasoning skills are negatively impacted by potential over-reliance.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable as the extractiveness is low.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_bicycle_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_bicycle_scaffold, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
