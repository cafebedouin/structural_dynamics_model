% ============================================================================
% CONSTRAINT STORY: zipfs_law
% Status: [RESOLVED MANDATROPHY]
% ============================================================================
% Generated: 2026-01-22
% Model: Gemini 2.0 Flash
% Source: George Kingsley Zipf (1949) / Quantitative Linguistics
% ============================================================================

:- module(constraint_zipfs_law, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for 2026 DR-Audit Suite) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:category_of/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: zipfs_law
 * human_readable: Zipf's Law (The Power Law of Information)
 * domain: technological/social
 * temporal_scope: Permanent (Universal Law of Information)
 * spatial_scope: Global (Natural Language and Large Data)
 * * SUMMARY:
 * Zipf's Law states that word frequency is inversely proportional to rank. 
 * While it provides a "Rope" for algorithmic efficiency (indexing/compression), 
 * it asymmetrically extracts attention from the "Long Tail" of minority 
 * content, creating a "Snare" of obscurity for niche participants.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(zipfs_law, 0, 10).
narrative_ontology:constraint_claim(zipfs_law, tangled_rope).

% Base extractiveness (0.65): Zipf's Law extracts visibility from the tail 
% and concentrates it in the "Head".
domain_priors:base_extractiveness(zipfs_law, 0.65).

% Suppression (0.70): The power-law suppresses "Equal Distribution" alternatives 
% in complex communicative systems.
domain_priors:suppression_score(zipfs_law, 0.70).
domain_priors:requires_active_enforcement(zipfs_law).

% Mandatory Asymmetry Hooks
constraint_beneficiary(zipfs_law, search_platform_monopolies).
constraint_victim(zipfs_law, niche_linguistic_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

% INDIVIDUAL: Experiences the 'Long Tail' as a Snare of obscurity.
constraint_indexing:constraint_classification(zipfs_law, snare, 
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))).

% INSTITUTIONAL: Uses the distribution as a Rope for computational efficiency.
constraint_indexing:constraint_classification(zipfs_law, rope, 
    context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global))).

% ANALYTICAL: Recognizes the Tangled Rope nature of the power law.
constraint_indexing:constraint_classification(zipfs_law, tangled_rope, 
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(zipfs_law_tests).

test(multi_perspective_variance) :-
    % Creator (Snare) vs Engineer (Rope) vs Analyst (Tangled Rope)
    constraint_indexing:constraint_classification(zipfs_law, T1, context(individual_powerless, immediate, constrained, local)),
    constraint_indexing:constraint_classification(zipfs_law, T2, context(institutional, biographical, mobile, global)),
    constraint_indexing:constraint_classification(zipfs_law, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3, T1 \= T3.

test(power_extractiveness_variance) :-
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextInstitutional = context(institutional, biographical, mobile, global),
    constraint_indexing:extractiveness_for_agent(zipfs_law, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(zipfs_law, ContextInstitutional, Score2),
    Score1 > Score2.

:- end_tests(zipfs_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 1. TANGLED ROPE RATIONALE: Zipf's Law is the ultimate structural hybrid. 
 * It enables the coordination of large-scale information (Rope) but extracts 
 * the visibility of the majority to subsidize the elite few (Snare).
 * 2. MANDATROPHY RESOLUTION: The extraction score (0.50) isn't "predatory" by 
 * intent but functions as a Snare for niche subjects because of the 
 * mathematical inevitability of the attention bottleneck.
 * 3. OMEGA:
 * omega_variable(algorithmic_intervention,
 * "Can algorithmic reranking untangle the extraction of Zipf's Law?",
 * resolution_mechanism("Long-term audit of recommendation engine diversity vs. power-law decay"),
 * impact("If yes: Tangled Rope becomes Rope. If no: It remains a Snare for the tail."),
 * confidence_without_resolution(medium)).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */
% Resolving the Perspectival Gap identified in Audit 410
omega_variable(
    omega_extraction_blindness_zipfs_law,
    'Is the attention concentration a byproduct of information physics (Mountain) or algorithmic reinforcement (Snare)?',
    resolution_mechanism('Audit of ranking variance across diverse platform architectures'),
    impact('If physics: Mountain. If reinforcement: Snare/Tangled Rope.'),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
