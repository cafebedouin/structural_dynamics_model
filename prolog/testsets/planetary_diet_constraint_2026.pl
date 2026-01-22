% ============================================================================
% CONSTRAINT STORY: planetary_diet_constraint_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "The one diet that’s good for everything" by Carissa Wong (Jan 2026)
% ============================================================================

:- module(planetary_diet_constraint_2026, []).

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
 * * constraint_id: planetary_diet_constraint_2026
 * human_readable: Planetary Boundary Dietary Alignment
 * domain: technological/ecological/economic
 * temporal_scope: 2020-2050 (The Great Transition)
 * spatial_scope: Global
 * * SUMMARY:
 * The article identifies the Mediterranean diet as being "good for the planet" 
 *. This creates a "Planetary Health" constraint where individual 
 * consumption is indexed against global ecological survival. The constraint 
 * shifts from a personal health choice to a collective survival mandate, 
 * where high-meat and high-dairy consumption are increasingly classified 
 * as ecological "theft" or systemically unviable.
 * * KEY AGENTS:
 * - Institutional Planner: (Global governance/Climate Scientist) - Views 
 * the diet as a necessary Rope to avoid climate tipping points.
 * - The Industrial Meat Producer: (Individual Powerful/Institutional) - 
 * Views the constraint as a Noose strangling their existing business model.
 * - The Global Consumer: (Individual Moderate) - Experiences the shift 
 * as a Mountain (the climate reality) or a Rope (meaningful lifestyle change).
 * * NARRATIVE ARC:
 * The shift from seeing diets as purely physiological to seeing them as 
 * planetary inputs. The "scientific gold standard" now requires both 
 * internal health and external sustainability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(planetary_diet_constraint_2026, 0, 10).

% Base extractiveness score: 0.1 (Very Low)
% Rationale: The benefit is largely communal/existential (planetary health). 
% Extraction is directed away from future generations and toward current 
% sustainable habits.
domain_priors:base_extractiveness(planetary_diet_constraint_2026, 0.1).

% Suppression score: 0.6 (High-Moderate)
% Rationale: Scientific consensus is increasingly marginalizing high-emission 
% dietary paths as "unsustainable" or "dangerous" to the biophysical 
% stability of the Earth.
domain_priors:suppression_score(planetary_diet_constraint_2026, 0.6).

% Enforcement: Requires active enforcement (Carbon taxes, subsidies, cultural shaming).
domain_priors:requires_active_enforcement(planetary_diet_constraint_2026).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(planetary_diet_constraint_2026, future_humanity).
constraint_beneficiary(planetary_diet_constraint_2026, biodiversity_preservation).
constraint_victim(planetary_diet_constraint_2026, industrial_animal_agriculture).
constraint_victim(planetary_diet_constraint_2026, cultural_traditions_reliant_on_heavy_meat).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: INSTITUTIONAL PLANNER - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Policy makers/Climate scientists)
   WHEN: historical (100-year plan for planetary stability)
   WHERE: arbitrage (Moving resources between sectors)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the planner, dietary shifts are a "Rope"—a tool for managing the 
   global carbon budget. It is a functional coordination mechanism to 
   reach net-zero goals without total systemic collapse.
   
   NARRATIVE EVIDENCE:
   The diet brings "planetary benefits". Scientists use this 
   evidence to back up policy interventions.
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
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: INDUSTRIAL MEAT PRODUCER - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerful (Large-scale business interests)
   WHEN: biographical (Short-term ROI/Business life)
   WHERE: constrained (Capital is tied up in physical infrastructure)
   SCOPE: national/global
   
   WHY THIS CLASSIFICATION:
   The mandatory shift toward "minimal meat and dairy" acts as a 
   Noose for the producer. It is an extractive mechanism that devalues 
   their assets and forces a transition they may not survive.
   
   NARRATIVE EVIDENCE:
   "Filled with fibre... minimal meat and dairy". This specific 
   composition is a direct threat to the core revenue of this agent.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    planetary_diet_constraint_2026,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: ECOLOGICAL REALIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observer of thermodynamics)
   WHEN: civilizational (Species-level survival)
   WHERE: analytical (No exit from the biosphere)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   From the view of planetary boundaries, the resource cost of certain 
   foods is a "Mountain." The biosphere has finite limits on carbon and 
   nitrogen; the necessity to eat within those limits is a natural law 
   rather than a policy choice.
   
   NARRATIVE EVIDENCE:
   The diet is "also good for the planet", implying an 
   extant planetary need that the diet merely satisfies.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    planetary_diet_constraint_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(planetary_diet_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(planetary_diet_constraint_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(planetary_diet_constraint_2026, noose, context(agent_power(individual_powerful), _, _, _)),
    constraint_indexing:constraint_classification(planetary_diet_constraint_2026, mountain, context(agent_power(analytical), _, _, _)).

test(suppression_impact) :-
    % High suppression (0.6) forces agents with low exit options into Noose types.
    domain_priors:suppression_score(planetary_diet_constraint_2026, S),
    S > 0.5.

:- end_tests(planetary_diet_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.1): Low because the "extraction" is actually a reduction 
 * in the "theft" from future generations' resources.
 * 2. PERSPECTIVE: I added the "Industrial Meat Producer" to show how a 
 * "good" constraint (Rope for the planet) can be a Noose for agents 
 * specialized in a legacy system.
 * 3. SUPPRESSION: Set to 0.6 because the narrative of "there is no alternative" 
 * for planetary survival is a powerful suppressive force against 
 * alternative dietary models.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    planetary_diet_scalability,
    "Is it biophysically possible to feed 10 billion people the 'Mediterranean gold standard' without exceeding other planetary boundaries (e.g., phosphorus or water)?",
    resolution_mechanism("Integrated assessment models comparing Mediterranean crop yields and water requirements at scale"),
    impact("If no: The Rope is actually a Noose for the global south. If yes: It remains a viable global Rope."),
    confidence_without_resolution(medium)
).

omega_variable(
    meat_substitution_technology,
    "Will lab-grown meat allow for 'meat-heavy' diets to become 'planetary-neutral' (Mountain-bypass)?",
    resolution_mechanism("Life-cycle analysis of industrial lab-meat energy consumption vs. traditional grazing"),
    impact("If yes: The 'minimal meat' constraint dissolves from a Noose into a non-issue."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: High-Tech Veganism (Precision Fermentation)
 * Viability: Potentially lower land-use than Mediterranean.
 * Suppression: Often ignored in favor of "Whole Foods" Mediterranean narratives.
 * * ALTERNATIVE 2: Regenerative Grazing
 * Viability: Claims to sequester carbon.
 * Suppression: Directly contradicted by the "minimal meat and dairy" mandate.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [planetary_diet_constraint_2026].
% Run tests: ?- run_tests(planetary_diet_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
