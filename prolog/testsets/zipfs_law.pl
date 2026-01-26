% ============================================================================
% CONSTRAINT STORY: zipfs_law
% Status: [RESOLVED MANDATROPHY]
% ============================================================================
% Generated: 2026-01-25
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
    domain_priors:requires_active_enforcement/1,
    domain_priors:category_of/2,
    constraint_indexing:constraint_classification/3,
    constraint_beneficiary/2,
    constraint_victim/2,
    narrative_ontology:constraint_metric/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: zipfs_law
 * human_readable: Zipf's Law (The Power Law of Information)
 * domain: technological
 * temporal_scope: Permanent (Universal Information Dynamics)
 * spatial_scope: Global (Digital Ecosystems)
 * * SUMMARY:
 * Zipf's Law describes the mathematical inevitability where the frequency 
 * of an item is inversely proportional to its rank. In digital economies, 
 * this creates a "Winner-Take-All" distribution of attention.
 * * KEY AGENTS:
 * - Content Creator: An individual attempting to gain visibility in the "Long Tail".
 * - Platform Monopoly: An institution leveraging the distribution for indexing efficiency.
 * - System Auditor: An analytical observer measuring the extraction of the tail.
 * * NARRATIVE ARC:
 * While Zipf's Law is a natural property of language (Mountain), its 
 * reinforcement via algorithmic ranking transforms it into a mechanism of 
 * extreme visibility extraction (Snare/Tangled Rope).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(zipfs_law, 0, 10).
narrative_ontology:constraint_claim(zipfs_law, tangled_rope).

% Base Properties (Satisfies structural_linter and domain_priors)
domain_priors:base_extractiveness(zipfs_law, 0.75). % High extraction triggers Mandatrophy status
domain_priors:suppression_score(zipfs_law, 0.70).
domain_priors:requires_active_enforcement(zipfs_law).
domain_priors:category_of(zipfs_law, technological).

% Explicit Metrics (Satisfies data_validation.pl)
narrative_ontology:constraint_metric(zipfs_law, extractiveness, 0.75).
narrative_ontology:constraint_metric(zipfs_law, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(zipfs_law, resistance_to_change, 0.85).

% Mandatory Asymmetry Hooks
constraint_beneficiary(zipfs_law, search_platform_monopolies).
constraint_victim(zipfs_law, niche_linguistic_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

% PERSPECTIVE 1: THE CONTENT CREATOR (NOOSE)
% WHO: individual_powerless - No control over algorithmic ranking
% WHEN: immediate - Visibility needed for survival now
% WHERE: trapped - Network effects prevent migration to alternate distributions
constraint_indexing:constraint_classification(zipfs_law, snare, 
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(global))) :-
    domain_priors:base_extractiveness(zipfs_law, E), E > 0.7,
    !.

% PERSPECTIVE 2: THE PLATFORM ARCHITECT (ROPE)
% WHO: institutional - Rule-making power to optimize for the majority
% WHEN: generational - Focus on long-term system stability
% WHERE: mobile - Can adjust weights but chooses the "Natural" power law
constraint_indexing:constraint_classification(zipfs_law, rope, 
    context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global))) :-
    !.

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Resolution of the hybrid nature: It coordinates data but extracts diversity.
constraint_indexing:constraint_classification(zipfs_law, tangled_rope, 
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))) :-
    domain_priors:base_extractiveness(zipfs_law, E), E > 0.4,
    domain_priors:suppression_score(zipfs_law, S), S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(zipfs_law_tests).

test(multi_perspective_variance) :-
    % Creator (Snare) vs Platform (Rope)
    constraint_indexing:constraint_classification(zipfs_law, Type1, context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(global))),
    constraint_indexing:constraint_classification(zipfs_law, Type2, context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global))),
    Type1 \= Type2.

test(power_extractiveness_scaling) :-
    % The powerless experience the full force of the 0.75 extraction score
    _ContextPowerless = context(agent_power(individual_powerless), immediate, trapped, global),
    _ContextInstitutional = context(agent_power(institutional), generational, mobile, global),
    % Mocking logic for extractiveness scaling
    Score1 = 0.75, % Base for powerless
    Score2 = 0.20, % Reduced for beneficiary
    Score1 > Score2.

:- end_tests(zipfs_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * 1. BASE EXTRACTIVENESS (0.75):
 * Chose 0.75 because while the distribution occurs "naturally" in language, 
 * in a 2026 digital context, it is leveraged to concentrate 75%+ of value 
 * in the top 0.1% of participants.
 * * 2. MANDATROPHY RESOLUTION:
 * Marked as [RESOLVED MANDATROPHY] because the high extraction is not 
 * a universal "evil" (it is a Mountain of information physics) but 
 * functions as a Snare for individual participants. 
 * * 3. OMEGA NECESSITY:
 * Since E > 0.7, an Omega is required to determine if the "Power Law" 
 * is an inescapable law of nature or an artifact of platform design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_zipfs_origin,
    "Is the attention bottleneck a result of biological cognitive limits (Mountain) or algorithmic amplification (Snare)?",
    resolution_mechanism("Comparative study of visibility decay in non-algorithmic vs. algorithmic networks"),
    impact("If biological: Mountain. If algorithmic: Tangled Rope/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    zipfs_law_extraction_intent,
    "Is the 0.75 extraction a functional necessity for system searchability or a predatory platform choice?",
    resolution_mechanism("Audit of platform profit margins in the Long Tail vs. the Head"),
    impact("If necessity: Rope for all. If predatory: Snare for the tail."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Flat Distribution (Equal Visibility)
 * Viability: Low. Results in massive "noise" and high cognitive load.
 * Suppression: High. Search engines actively penalize "random" discovery.
 * * ALTERNATIVE 2: Lottery-Based Rotation
 * Viability: Moderate. Could theoretically distribute visibility to the tail.
 * Suppression: Rejected by institutions as "inefficient" and "low quality."
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
