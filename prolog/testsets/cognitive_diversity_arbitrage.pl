% ============================================================================
% CONSTRAINT STORY: cognitive_diversity_arbitrage
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Caroline Williams, "There’s no such thing as a normal brain"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_cognitive_diversity_arbitrage, []).

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
 * * constraint_id: cognitive_diversity_arbitrage
 * human_readable: Cognitive Diversity Arbitrage
 * domain: economic/social
 * temporal_scope: 21st Century (The Post-Normalcy Era)
 * spatial_scope: Modern Workplace / Global Knowledge Economy
 * * SUMMARY:
 * As science moves past the assumption of a "normal" brain, the workplace is 
 * transitioning toward a model of "Cognitive Diversity Arbitrage." 
 * This involves identifying "unusual brains" (autism, ADHD, dyslexia) not as 
 * problems to be solved, but as strategic assets with unique strengths that 
 * can benefit an entire organization if properly supported.
 * * KEY AGENTS:
 * - The Neurodivergent Talent: Possession of "unique strengths" but requiring specific support levels.
 * - The Arbitraging Firm: Corporations that reframe "disorders" as "natural variation" to gain a competitive edge.
 * - The Traditional Manager: Still clinging to the "normal brain" assumption that neatly conforms to society.
 * * NARRATIVE ARC:
 * The "hard line" between normal and abnormal is dissolving into a spectrum. 
 * This allows firms to "arbitrage" the unique cognitive wiring of their workforce, 
 * moving from a "Noose" of forced conformity to a "Rope" of collaborative asset 
 * management.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(cognitive_diversity_arbitrage, 0, 10).
narrative_ontology:constraint_claim([cognitive_diversity_arbitrage], [economic_asset_realization]).

% Base extractiveness score: 0.72 (High)
% Rationale: Corporations extract high value from the "unique strengths" of 
% neurodivergent employees. If support is not 
% commensurate with the "asset" value, this is a high-extraction scenario.
domain_priors:base_extractiveness(cognitive_diversity_arbitrage, 0.72).

% Suppression score: 0.45 (Moderate)
% Rationale: While neurodiversity is embedded in medical literature, many 
% workplaces still suppress "unusual" traits in favor of social conformity.
domain_priors:suppression_score(cognitive_diversity_arbitrage, 0.45).

% Enforcement requirements
% Requires active enforcement to move from "illness" labels to "spectrum" support.
domain_priors:requires_active_enforcement(cognitive_diversity_arbitrage).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(cognitive_diversity_arbitrage, extractiveness, 0.72).
narrative_ontology:constraint_metric(cognitive_diversity_arbitrage, suppression_requirement, 0.45).

% BENEFICIARIES & VICTIMS
% The Firm benefits from "diverse brains" as an asset that benefits everyone.
constraint_beneficiary(cognitive_diversity_arbitrage, arbitraging_corporation).
% Individuals who are "treated as though something was wrong with them" 
% suffer the extraction of their identity and dignity.
constraint_victim(cognitive_diversity_arbitrage, unsupported_neurodivergent_employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE STRATEGIC CEO - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power over the org)
   WHEN: generational (Reshaping how we think about work)
   WHERE: mobile (Can choose to support or ignore diversity)
   SCOPE: global (The global search for talent assets)
   
   WHY THIS CLASSIFICATION:
   For the CEO, neurodiversity is a Rope—a functional mechanism to improve 
   innovation and problem-solving by leveraging "natural variation."
   
   NARRATIVE EVIDENCE:
   "diverse brains aren’t a problem to be solved, but an asset that, if properly 
   supported, could benefit everyone."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cognitive_diversity_arbitrage,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(cognitive_diversity_arbitrage, E),
    E < 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE "MASKING" EMPLOYEE - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Treating unusual brains as an illness)
   WHEN: biographical (The duration of a career under pressure)
   WHERE: trapped (Needs the job, but must conform to "normal")
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   When the workplace ignores the spectrum and demands conformity, the 
   "unusual brain" becomes a Noose that extracts mental health and effort.
   
   NARRATIVE EVIDENCE:
   "treated as though something was wrong with them... neatly conformed 
   to society."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cognitive_diversity_arbitrage,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(cognitive_diversity_arbitrage, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE TRADITIONAL BUREAUCRAT - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerful (Enforcing standard "normal" rules)
   WHEN: historical (The long era of the "normal" brain assumption)
   WHERE: trapped (Incapable of seeing traits as a spectrum)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   To the traditionalist, the concept of a "normal brain" is a Mountain—an 
   immutable natural law. Deviations are "disorders" to be cured, not assets.
   
   NARRATIVE EVIDENCE:
   "Once upon a time, science worked on the assumption that there was such a 
   thing as a 'normal' brain."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cognitive_diversity_arbitrage,
    mountain,
    context(
        agent_power(individual_powerful),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(cognitive_diversity_arbitrage, S),
    S > 0.4,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(cognitive_diversity_arbitrage_tests).

test(multi_perspective_variance) :-
    % CEO (Rope) vs Employee (Noose) vs Traditionalist (Mountain)
    constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, rope, context(institutional, _, mobile, _)),
    constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, noose, context(individual_powerless, _, trapped, _)),
    constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, mountain, context(individual_powerful, _, trapped, _)).

test(power_extractiveness_scaling) :-
    % Powerless individuals experience the extraction of their "normalcy"
    E = 0.72,
    E > 0.5.

test(spectrum_mobility_insight) :-
    % Transition from Level 3 to Level 1 support changes the Rope density.
    true.

:- end_tests(cognitive_diversity_arbitrage_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS (0.72): High score chosen because "unique strengths" 
 * represent high-value cognitive labor often obtained without commensurate 
 * structural support.
 * * 2. PERSPECTIVE SELECTION:
 * The "CEO" perspective highlights the asset-framing.
 * The "Masking Employee" highlights the extractive risk of the old illness model.
 * * 3. MANDATROPHY STATUS: Triggered because the value of "unusual brains" is 
 * being harvested as a systemic asset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    cognitive_diversity_arbitrage_extraction_intent,
    "Is the harvesting of 'unique strengths' a form of predatory cognitive extraction or a collaborative asset realization?",
    resolution_mechanism("Audit of salary/support ratios for neurodivergent vs neurotypical roles with similar output"),
    impact("If predatory: Noose/Mandatrophy. If collaborative: Rope."),
    confidence_without_resolution(medium)
).

omega_variable(
    support_efficacy_threshold,
    "Does 'proper support' actually neutralize the challenges of unusual brains, or just mask them for corporate benefit?",
    resolution_mechanism("Long-term burnout rates among neurodivergent employees in 'inclusive' workplaces"),
    impact("If it neutralizes: Rope. If it masks: Noose."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The Illness/Pathology Model
 * Viability: Historically dominant for decades.
 * Suppression: Now being "done away with" by medical literature like the DSM-5.
 * * CONCLUSION:
 * The move from the "Normalcy Mountain" to the "Diversity Rope" is the core 
 * transformation of 21st-century organizational psychology.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraints/cognitive_diversity_arbitrage].
 * 2. Multi-perspective: ?- multi_index_report(cognitive_diversity_arbitrage).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
