% ============================================================================
% CONSTRAINT STORY: cost_of_observation
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Indexical Relativity & Evolutionary Pressure
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_observation_cost, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cost_of_observation
 * human_readable: The Evolutionary Cost of Observation
 * domain: philosophical/evolutionary/cybernetic
 * * SUMMARY:
 * This meta-constraint describes the "tax" paid by any situated observer. 
 * While a constraint may exist sub specie aeternitatis as a neutral Mountain, 
 * the introduction of an observer transforms that mountain into a cost function. 
 * The "Local Frame" creates a density of constraints that extract time, 
 * energy, and agency from the agent.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(evolutionary_pressure_01, 0, 10).
narrative_ontology:constraint_claim(cost_of_observation, mountain).

% Base extractiveness score (0.85)
% Rationale: High because observation is never free. Metabolic and cognitive 
% load required to navigate constraints is an inherent extraction.
domain_priors:base_extractiveness(cost_of_observation, 0.85).

% Suppression score (0.4)
domain_priors:suppression_score(cost_of_observation, 0.4).

% This cost emerges naturally from the physics of situated agency.
domain_priors:emerges_naturally(cost_of_observation).

% Metrics for DR-Audit
narrative_ontology:constraint_metric(cost_of_observation, extractiveness, 0.85).
narrative_ontology:constraint_metric(cost_of_observation, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(cost_of_observation, [evolutionary_efficiency, entropy_export]).
constraint_victim(cost_of_observation, [the_situated_agent, frontline_practitioners]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: The Aeternal Observer - Mountain
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    cost_of_observation,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: The Individual Agent - Snare
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    cost_of_observation,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(cost_of_observation, E),
    E > 0.8, % High extraction in local frame = Snare
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: The Evolutionary Designer - Rope
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    cost_of_observation,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(continental)
    )
) :-
    !.

/* ==========================================================================
   4. TESTS (Mandatrophy Resolution)
   ========================================================================== */

:- begin_tests(observation_cost_tests).

test(multi_perspective_variance) :-
    % Verify that high extraction (0.85) is resolved via indexical shift
    constraint_indexing:constraint_classification(cost_of_observation, Type1, 
        context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(cost_of_observation, Type2, 
        context(analytical, civilizational, analytical, global)),
    Type1 = snare,
    Type2 = mountain.

test(local_pressure_scaling) :-
    % Verify base extractiveness meets the 0.85 threshold
    domain_priors:base_extractiveness(cost_of_observation, E),
    E == 0.85.

:- end_tests(observation_cost_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * [RESOLVED MANDATROPHY]: The extraction score of 0.85 reflects the 
 * unavoidable "metabolic tax" of agency. For the situated agent in the 
 * Local Frame, this is a NOOSE because it forces the expenditure of finite 
 * resources. For the Aeternal observer, it remains a neutral MOUNTAIN.
 * * OMEGAS:
 * omega_variable(metabolic_efficiency_limit,
 * "Is there a theoretical floor to the cost of observation?",
 * resolution_mechanism("Biophysical measurement of cognitive efficiency"),
 * impact("Determines if the Snare is a physical constant."),
 * confidence_without_resolution(low)
 * ).
 */

/* ==========================================================================
   6. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE:
 * ?- [constraint_observation_cost].
 * ?- run_tests(observation_cost_tests).
 */
