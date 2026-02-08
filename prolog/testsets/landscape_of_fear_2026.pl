% ============================================================================
% CONSTRAINT STORY: landscape_of_fear_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "How fear drastically shapes ecosystems" by Michael Le Page
% ============================================================================

:- module(landscape_of_fear_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: landscape_of_fear_2026
 * human_readable: The Landscape of Fear
 * domain: biological/technological
 * temporal_scope: 1995-2026
 * spatial_scope: Global ecosystems (e.g., Yellowstone, British Columbia)
 * * SUMMARY:
 * The "landscape of fear" describes how the mere presence or perception of predators 
 * constrains prey behavior, affecting feeding patterns, reproductive rates, and 
 * survival far beyond direct predation. This concept shifted the 
 * biological paradigm from counting "kills" to mapping the psychological 
 * constraints predators impose on an environment.
 * * KEY AGENTS:
 * - The Apex Predator (Wolf/Hawk): The source of the constraint; 
 * enforces boundaries through the threat of violence.
 * - The Prey (Elk/Song Sparrow): The subject of the constraint; 
 * must navigate the trade-off between feeding and vigilance.
 * - The Biologist (Laundré/Zanette): The analytical observer who 
 * identifies the causal mechanisms of fear-based ecological shifts.
 * * NARRATIVE ARC:
 * Following the 1995 reintroduction of wolves to Yellowstone, scientists noticed 
 * elk numbers falling faster than predation rates alone could explain. 
 * By 2001, the term "landscape of fear" was coined, and subsequent 21st-century 
 * experiments proved that fear alone—even without physical contact—can reduce 
 * survival rates by over 50%.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(landscape_of_fear_2026, 0, 10).
narrative_ontology:constraint_claim([landscape_of_fear_2026], [biological_regulation]).

% Base extractiveness score (0.4 = Moderate)
% Rationale: Predators extract time and reproductive potential from prey 
% through the psychological burden of vigilance.
domain_priors:base_extractiveness(landscape_of_fear_2026, 0.4).

% Suppression score (0.7 = High)
% Rationale: The "Landscape of Fear" effectively suppresses the "best food 
% in town" for the prey, as they will completely avoid high-quality areas 
% where fear is high.
domain_priors:suppression_score(landscape_of_fear_2026, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(landscape_of_fear_2026, extractiveness, 0.4).
narrative_ontology:constraint_metric(landscape_of_fear_2026, suppression_requirement, 0.7).

% Enforcement requirements
% Emerges naturally through evolutionary predator-prey dynamics.
domain_priors:emerges_naturally(landscape_of_fear_2026).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(landscape_of_fear_2026, ecosystem_stability). % Fear prevents overgrazing
constraint_victim(landscape_of_fear_2026, prey_populations). % Reduced reproduction and feeding

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ELK/SPARROW - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (Prey animal at bottom of hierarchy)
   WHEN: immediate (Short-term survival focus)
   WHERE: trapped (Ecologically bound to a specific territory)
   SCOPE: local (Immediate proximity of predator signs)
   
   WHY THIS CLASSIFICATION:
   For the prey, fear is a "Snare." It is a coercive psychological pressure 
   that strangles their ability to feed and reproduce, forcing them into 
   low-quality habitats even when superior food is nearby.
   
   NARRATIVE EVIDENCE:
   "Fewer eggs were laid, fewer hatched and fewer hatchlings survived... less than 
   half as many lived".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    landscape_of_fear_2026,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(landscape_of_fear_2026, S),
    S > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE APEX PREDATOR (Wolf) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-enforcer/Shaper of the ecosystem)
   WHEN: biographical (Duration of the predator's reign)
   WHERE: mobile (Can move across the landscape to create fear)
   SCOPE: regional (Entire park or ecosystem)
   
   WHY THIS CLASSIFICATION:
   For the predator, fear is a "Rope"—a functional tool for coordinating 
   prey behavior and ensuring the predator's own energy-efficient survival 
   by making prey easier to manage or manipulate.
   
   NARRATIVE EVIDENCE:
   The predator doesn't need to kill to have an impact; the "mere fear" is 
   the tool used to shape the landscape.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    landscape_of_fear_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: BIOLOGIST (Liana Zanette) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Scientific observer)
   WHEN: historical (20+ years of experimental data)
   WHERE: analytical (Observer of global biological laws)
   SCOPE: global (The theory applies across species and habitats)
   
   WHY THIS CLASSIFICATION:
   For the analyst, the landscape of fear is a "Mountain"—an immutable 
   biophysical law. It is a fundamental truth of ecological systems that 
   fear shapes life just as surely as physical death does.
   
   NARRATIVE EVIDENCE:
   "Fear can have an even bigger impact than direct predation... It is all 
   about food".
   -------------------------------------------------------------------------- */


constraint_indexing:constraint_classification(
    landscape_of_fear_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(landscape_of_fear_2026_tests).

test(multi_perspective_fear) :-
    constraint_indexing:constraint_classification(landscape_of_fear_2026, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(landscape_of_fear_2026, Type2, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2.

test(fear_vs_predation_impact) :-
    % Tests if fear (0.7 suppression) is considered a high-impact constraint
    domain_priors:suppression_score(landscape_of_fear_2026, S),
    S > 0.5.

:- end_tests(landscape_of_fear_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.4): 
 * Reasoning: Predation is highly extractive, but *fear* is a psychological 
 * tax that takes labor (vigilance) and resources (forgone food) from the prey.
 * * 2. SUPPRESSION SCORE (0.7):
 * Reasoning: The article explicitly states that prey will "completely avoid" 
 * some areas. This is the suppression of alternative feeding sites.
 * * 3. PERSPECTIVE SELECTION:
 * Analyzed the prey (Snare), the predator as rule-setter (Rope), and the 
 * scientist as observer of the biological law (Mountain).
 * * 4. AMBIGUITIES:
 * It is unclear if the "Landscape of Fear" is equally oppressive for all prey 
 * species; the article focuses on elk and song sparrows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fear_acclimation_threshold,
    "Do prey eventually 'habituate' to predator sounds/signs, reducing the fear-constraint over generations?",
    resolution_mechanism("Long-term (multi-generational) study of prey response to chronic predator presence"),
    impact("If habituation occurs: Snare → Rope. If not: Persistent Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    human_as_super_predator,
    "Do human-induced 'landscapes of fear' (noises, urban light) create more extraction than natural ones?",
    resolution_mechanism("Comparative analysis of prey reproductive rates in human-dominated vs. wild landscapes"),
    impact("If human fear is higher: We are an unmatched extractive Snare."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Direct Predation Model
 * Viability: Historically the standard view; predators only affect numbers via kills.
 * Suppression: Actively dismantled by 21st-century experiments proving 
 * causality between fear and survival.
 * Evidence: "Laundré and others’ observations suggested this was wrong".
 * * CONCLUSION:
 * The "Landscape of Fear" transformed a perceived stochastic kill-rate (Mountain) 
 * into a persistent psychological Snare (for prey) and a biological Rope (for 
 * ecosystem management).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [landscape_of_fear_2026].
% Test: ?- run_tests(landscape_of_fear_2026_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Derived from None classification in biological domain
domain_priors:theater_ratio(landscape_of_fear_2026, 0.17).
narrative_ontology:constraint_metric(landscape_of_fear_2026, theater_ratio, 0.17).

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Enforcement flag (required for tangled_rope gate) ---
% Tangled rope requires: constraint_beneficiary + constraint_victim + requires_active_enforcement
domain_priors:requires_active_enforcement(landscape_of_fear_2026).
