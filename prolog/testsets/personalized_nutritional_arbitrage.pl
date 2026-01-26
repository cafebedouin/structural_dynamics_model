% ============================================================================
% CONSTRAINT STORY: personalized_nutritional_arbitrage
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Tim Spector (Zoe co-founder) / Helen Thomson / Microbiome special issue
% ============================================================================

:- module(constraint_personalized_nutritional_arbitrage, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
 * 
 * constraint_id: personalized_nutritional_arbitrage
 * human_readable: Personalized Nutritional Arbitrage (Microbiome-Guided Diet)
 * domain: economic/healthcare/biological
 * temporal_scope: 21st Century (The "Zoe" era)
 * spatial_scope: Global (Digital health platforms)
 * 
 * SUMMARY:
 * This constraint represents the ability of individuals and companies to "arbitrage" 
 * health outcomes by tailoring dietary intake to specific microbial signatures 
 * rather than following generic nutritional rules. This shifts nutrition from a
 * "one-size-fits-all" model to a data-driven, personalized approach.
 * 
 * KEY AGENTS:
 * - The Generic Diet Follower (Individual Powerless): Lacks access to personalized data, suffers from "one-size-fits-all" advice.
 * - The "Optimized" Individual (Individual Moderate): Uses unique microbial fingerprint to bypass generic diets.
 * - Nutrition Platforms (Institutional): Commercial entities that extract data to provide health insights.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(personalized_nutritional_arbitrage, 0, 10).
narrative_ontology:constraint_claim(personalized_nutritional_arbitrage, tangled_rope).

% Base extractiveness: 0.65 (Moderate-High)
% Rationale: Platforms like Zoe extract biological data (blood, stool) 
% to monetize health insights, creating a data-driven health economy.
domain_priors:base_extractiveness(personalized_nutritional_arbitrage, 0.65).

% Suppression: 0.4 (Moderate-Low)
% Rationale: While genetic sequencing is advanced, generic "rule-based" nutrition 
% still dominates public policy, suppressing the rapid adoption of personalized advice.
domain_priors:suppression_score(personalized_nutritional_arbitrage, 0.4).

% Enforcement: Requires active individual tracking (daily logging, periodic testing).
domain_priors:requires_active_enforcement(personalized_nutritional_arbitrage).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(personalized_nutritional_arbitrage, nutrition_tech_companies).
constraint_victim(personalized_nutritional_arbitrage, generic_diet_followers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE GENERIC DIET FOLLOWER - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Lacks access to personalized data or its interpretation)
   WHEN: immediate (Experiencing real-time metabolic consequences)
   WHERE: constrained (Must follow generic labels/advice)
   
   WHY THIS CLASSIFICATION:
   Generic "rule-based" nutrition acts as a 'Snare' for those whose unique 
   microbial fingerprint reacts poorly to standard diets. They suffer extraction
   of health without the ability to "arbitrage" for better outcomes, trapped
   by advice that harms them.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    personalized_nutritional_arbitrage,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE "OPTIMIZED" INDIVIDUAL - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has the agency to track and adapt)
   WHEN: biographical (Daily health management)
   WHERE: arbitrage (Can switch between diets based on real-time data)
   
   WHY THIS CLASSIFICATION:
   For the user with the right tools, nutrition is no longer a set of 
   external "rules" but a 'Rope'—a functional tool to influence immunity 
   and mental well-being. It allows them to coordinate their biology with their
   dietary intake for optimal health.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    personalized_nutritional_arbitrage,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: NUTRITION PLATFORMS (e.g., Zoe) - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional (Commercial entities that extract data to provide health scores)
   WHEN: generational (Building a new health data economy)
   WHERE: arbitrage (Monetizes biological data for insights)
   
   WHY THIS CLASSIFICATION:
   For nutrition platforms, personalized arbitrage is a 'Tangled Rope'. It's a 'Rope'
   because it offers a powerful new way to understand and improve health, creating
   a new market. It becomes 'Tangled' because it relies on the extraction of sensitive
   biological data, creating ethical and privacy concerns around data monetization.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    personalized_nutritional_arbitrage,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(personalized_nutritional_arbitrage_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(personalized_nutritional_arbitrage_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. CLASSIFICATION RATIONALE:
 *    - Generic Diet Follower (Snare): Trapped by generic advice that harms them.
 *    - Optimized Individual (Rope): A tool for personalized health.
 *    - Nutrition Platforms (Tangled Rope): A beneficial tool with data extraction.
 * 
 * 2. TANGLED ROPE: The overall constraint is a 'Tangled Rope' because it offers
 *    powerful health benefits ('Rope') but relies on the systematic extraction of
 *    sensitive biological data ('Snare') from its users for monetization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the ethical and practical implications of data-driven health.
 */

omega_variable(
    microbial_data_portability,
    "Can individuals 'exit' a proprietary nutrition platform (e.g., Zoe) without losing their personalized microbial history and insights, or is it a vendor lock-in 'Snare'?",
    resolution_mechanism("Audit of data export standards and interoperability across digital health platforms; legal precedents on data ownership."),
    impact("If data is portable: The platform is a 'Rope'. If not: It's a 'Snare' (vendor lock-in)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Rule-Based Public Nutrition Guidelines
 *    Viability: The 20th-century standard for public health.
 *    Suppression: Being actively "transformed" and replaced by personalized biotech, as it fails to account for individual biological variability.
 *
 * CONCLUSION:
 * Personalized Nutritional Arbitrage creates a 'Rope' for those who can access
 * and afford it, but it also creates a new 'Snare' for those trapped by
 * generic advice that might not suit their unique biology.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/personalized_nutritional_arbitrage].
 * 2. Multi-perspective: ?- multi_index_report(personalized_nutritional_arbitrage).
 * 3. Run tests: ?- run_tests(personalized_nutritional_arbitrage_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */