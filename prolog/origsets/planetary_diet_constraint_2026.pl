% ============================================================================
% CONSTRAINT STORY: planetary_diet_constraint_2026
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "The one diet that’s good for everything" by Carissa Wong (Jan 2026)
% ============================================================================

:- module(constraint_planetary_diet_constraint_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
 * 
 * constraint_id: planetary_diet_constraint_2026
 * human_readable: Planetary Boundary Dietary Alignment
 * domain: ecological/economic/social
 * temporal_scope: 2020-2050 (The Great Transition)
 * spatial_scope: Global
 * 
 * SUMMARY:
 * The article identifies the Mediterranean diet as being "good for the planet." 
 * This creates a "Planetary Health" constraint where individual 
 * consumption is indexed against global ecological survival. The constraint 
 * shifts from a personal health choice to a collective survival mandate, 
 * where high-meat and high-dairy consumption are increasingly classified 
 * as ecological "theft" or systemically unviable.
 * 
 * KEY AGENTS:
 * - Citizen in a High-Meat Culture (Individual Powerless): Faces pressure to change ingrained dietary habits.
 * - Institutional Planner (Institutional): Global governance/Climate Scientist who views the diet as a necessary Rope.
 * - Industrial Meat Producer (Individual Powerful): Views the constraint as a Snare strangling their business model.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(planetary_diet_constraint_2026, 0, 10).
narrative_ontology:constraint_claim(planetary_diet_constraint_2026, tangled_rope).

% Base extractiveness: 0.1 (Very Low)
% The benefit is largely communal/existential (planetary health). 
% Extraction is directed away from future generations and toward current 
% sustainable habits.
domain_priors:base_extractiveness(planetary_diet_constraint_2026, 0.1).

% Suppression: 0.6 (High-Moderate)
% Scientific consensus is increasingly marginalizing high-emission 
% dietary paths as "unsustainable" or "dangerous" to the biophysical 
% stability of the Earth.
domain_priors:suppression_score(planetary_diet_constraint_2026, 0.6).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(planetary_diet_constraint_2026, extractiveness, 0.1).
narrative_ontology:constraint_metric(planetary_diet_constraint_2026, suppression_requirement, 0.6).

% Enforcement: Requires active enforcement (Carbon taxes, subsidies, cultural shaming).
domain_priors:requires_active_enforcement(planetary_diet_constraint_2026).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(planetary_diet_constraint_2026, future_humanity).
constraint_victim(planetary_diet_constraint_2026, industrial_animal_agriculture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: CITIZEN IN A HIGH-MEAT CULTURE - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Ingrained cultural and dietary habits)
   WHEN: immediate (Facing daily choices about food consumption)
   WHERE: constrained (Limited by availability and affordability of alternatives)
   
   WHY THIS CLASSIFICATION:
   For a citizen in a high-meat culture, the dietary shift is a 'Snare'. It 
   strangles their cultural traditions, personal preferences, and often
   their economic habits (e.g., traditional farming). The pressure to change
   feels like an unwanted and potentially impossible imposition.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    planetary_diet_constraint_2026,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: INSTITUTIONAL PLANNER - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Policy makers/Climate scientists)
   WHEN: historical (100-year plan for planetary stability)
   WHERE: arbitrage (Moving resources between sectors)
   
   WHY THIS CLASSIFICATION:
   For the planner, dietary shifts are a 'Rope'—a tool for managing the 
   global carbon budget. It is a functional coordination mechanism to 
   reach net-zero goals without total systemic collapse.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    planetary_diet_constraint_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: INDUSTRIAL MEAT PRODUCER - Snare
   --------------------------------------------------------------------------
   WHO: powerful (Large-scale business interests)
   WHEN: biographical (Short-term ROI/Business life)
   WHERE: constrained (Capital is tied up in physical infrastructure)
   
   WHY THIS CLASSIFICATION:
   The mandatory shift toward "minimal meat and dairy" acts as a 
   'Snare' for the producer. It is an extractive mechanism that devalues 
   their assets and forces a transition they may not survive.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    planetary_diet_constraint_2026,
    snare,
    context(
        agent_power(powerful),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(planetary_diet_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(planetary_diet_constraint_2026, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planetary_diet_constraint_2026, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(planetary_diet_constraint_2026, Type3, context(agent_power(powerful), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

:- end_tests(planetary_diet_tests).

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
 * 1. INDIVIDUAL POWERLESS PERSPECTIVE: Added 'Citizen in a High-Meat Culture'
 *    to represent the individual experiencing the dietary shift as a 'Snare'
 *    that impacts their daily life and cultural traditions.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Citizen (Snare): Cultural and personal habits are strangled.
 *    - Planner (Rope): A tool for global coordination.
 *    - Producer (Snare): Business model is threatened.
 * 
 * 3. TANGLED ROPE: The planetary diet is a 'Tangled Rope'. It's a vital 'Rope'
 *    for global ecological health, but its implementation creates a 'Snare'
 *    for individuals and industries deeply ingrained in high-meat consumption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the physical attainability and social acceptability of the dietary transition.
 */

omega_variable(
    planetary_diet_scalability,
    "Is it biophysically possible to feed 10 billion people the 'Mediterranean gold standard' without exceeding other planetary boundaries (e.g., phosphorus or water)?",
    resolution_mechanism("Integrated assessment models comparing Mediterranean crop yields and water requirements at scale."),
    impact("If no: The 'Rope' is actually a 'Snare' for the global south, creating new forms of food insecurity. If yes: It remains a viable global 'Rope'."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: High-Tech Veganism (Precision Fermentation)
 *    Viability: Potentially lower land-use footprint than Mediterranean, but still niche.
 *    Suppression: Often ignored in favor of "Whole Foods" Mediterranean narratives, or cultural resistance to processed alternatives.
 *
 * CONCLUSION:
 * The planetary diet is a 'Tangled Rope' that attempts to coordinate human consumption
 * with ecological limits. The suppression of high-meat diets creates a 'Snare'
 * for certain industries and cultures, but is presented as a necessary 'Rope'
 * for planetary survival.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/planetary_diet_constraint_2026].
 * 2. Multi-perspective: ?- multi_index_report(planetary_diet_constraint_2026).
 * 3. Run tests: ?- run_tests(planetary_diet_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */