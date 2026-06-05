% ============================================================================
% CONSTRAINT STORY: rotation_seven_black_soil
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "Rotation Seven" narrative
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_rotation_seven_black_soil, []).

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
 * 
 * constraint_id: rotation_seven_black_soil
 * human_readable: R7 Black Soil Toxicity
 * domain: biological/environmental/social
 * temporal_scope: Biographical (years of accumulation)
 * spatial_scope: Rotation Seven Station Greenhouse
 * 
 * SUMMARY:
 * The black soil in Section 7-B contains toxins that lead to kidney failure 
 * and death. While the station's protocols manage how children 
 * interact with the soil, the soil's actual biological effect on the human 
 * body is an unchangeable physical reality. This creates a Mandatrophic Mountain.
 * 
 * KEY AGENTS:
 * - Anna (Individual Powerless): Exposed to the soil; her body is the site of the constraint.
 * - Supervisor Kwan (Institutional): Manages the victims, but cannot manage the toxin itself.
 * - The Station's High Command (Analytical): Strategically plans for the multi-year abandonment of Sector 7.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(rotation_seven_black_soil, 0, 10).
narrative_ontology:constraint_claim(rotation_seven_black_soil, mountain).

% Base extractiveness: 0.95 (Extreme).
% It extracts the vital organs (kidneys) and eventually the entire life of 
% the agent. This is a severe, irreversible extraction.
domain_priors:base_extractiveness(rotation_seven_black_soil, 0.95).

% Suppression: 0.1 (Low).
% It is not a rule being "hidden"; it is a physical fact that manifests in 
% dark urine and yellow skin. The effects are evident, not suppressed.
domain_priors:suppression_score(rotation_seven_black_soil, 0.1).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(rotation_seven_black_soil, extractiveness, 0.95).
narrative_ontology:constraint_metric(rotation_seven_black_soil, suppression_requirement, 0.1).

% Enforcement: Emerges naturally from biological consequences; no active human enforcement needed.
domain_priors:emerges_naturally(rotation_seven_black_soil).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(rotation_seven_black_soil, new_urban_centers).
constraint_victim(rotation_seven_black_soil, sector_seven_residents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: ANNA (EXPOSED INDIVIDUAL) - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Exposed to the soil; her body is the site of the constraint)
   WHEN: biographical (Years of accumulation leading to kidney failure)
   WHERE: trapped (Cannot negotiate with her enzyme levels or the toxin)
   
   WHY THIS CLASSIFICATION:
   For Anna, the black soil toxicity is a 'Snare'. Her body is slowly being
   poisoned, and there is no escape from the biological consequence. The toxin
   strangles her life functions, leading to an inevitable and irreversible decline.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rotation_seven_black_soil,
    snare,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: SUPERVISOR KWAN (INSTITUTIONAL MANAGER) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Manages the victims, but cannot manage the toxin itself)
   WHEN: immediate (Implementing safety protocols and managing affected individuals)
   WHERE: constrained (Bound by station protocols and limited medical resources)
   
   WHY THIS CLASSIFICATION:
   For Supervisor Kwan, the protocols for managing the black soil's effects are a 'Rope'.
   It is a tool for coordinating the station's response to an intractable problem.
   He uses these protocols to maintain order, manage exposure, and process victims,
   even though he cannot solve the underlying toxicity.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rotation_seven_black_soil,
    rope,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE STATION'S HIGH COMMAND (ANALYTICAL STRATEGIST) - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Strategically plans for the multi-year abandonment of Sector 7)
   WHEN: generational (Planning the long-term survival of the colony)
   WHERE: analytical (Views the toxin as an immutable structural invariant)
   
   WHY THIS CLASSIFICATION:
   To the Station's High Command, the black soil toxicity is a 'Mountain'.
   It is an immutable physical reality that dictates long-term strategic decisions,
   such as the abandonment of Sector 7. No amount of protocol or effort can change
   the toxin's chemical signature or its biological effects.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rotation_seven_black_soil,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(generational),
        exit_options(analytical),
        spatial_scope(regional)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(rotation_seven_black_soil_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(rotation_seven_black_soil, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_black_soil, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_black_soil, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(rotation_seven_black_soil_tests).

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
 * 1. MANDATROPHY STATUS: High extractiveness (0.95) confirms this as a severe
 *    Mandatrophic constraint. It is 'RESOLVED' by classifying the toxin as a
 *    'Mountain' that forces strategic shifts, even as it acts as a 'Snare'
 *    for individuals.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Anna (Snare): Slow, inevitable poisoning and loss of life.
 *    - Supervisor Kwan (Rope): Protocols for managing an intractable crisis.
 *    - High Command (Mountain): Immutable physical reality dictating strategy.
 * 
 * 3. CORE INSIGHT: The R7 Black Soil Toxicity is a 'Mountain' of biological
 *    reality that cannot be overcome, only managed. This forces institutions
 *    to use 'Ropes' (protocols) to cope, but ultimately creates a 'Snare' for
 *    the individuals whose lives are extracted by its immutable effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */

omega_variable(
    toxicity_reversibility,
    "Is the black soil toxicity truly an irreversible 'Mountain' of biological decay, or is there a yet-undiscovered 'Rope' (e.g., advanced bioremediation, nanomedicine) that could detoxify the soil or reverse its effects on the human body?",
    resolution_mechanism("Long-term research into advanced detoxification methods, medical breakthroughs in kidney regeneration, and geological analysis of soil composition over generational timescales."),
    impact("If irreversible: It remains an enduring 'Mountain'. If remediable: It transforms into a manageable 'Rope' or 'Tangled Rope'."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Sector 7 Abandonment (Strategic Retreat)
 *    Viability: This is the long-term strategic plan of High Command, framed as the only viable exit.
 *    Suppression: Temporarily suppressed by the immediate need for food production and the social costs of displacement.
 *
 * CONCLUSION:
 * The R7 Black Soil Toxicity is a 'Mountain' that actively suppresses most
 * alternatives. While 'Strategic Retreat' is a viable 'Rope' for the High Command,
 * it creates a 'Snare' for the residents who must endure the toxicity or face
 * forced relocation.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/rotation_seven_black_soil].
 * 2. Multi-perspective: ?- multi_index_report(rotation_seven_black_soil).
 * 3. Run tests: ?- run_tests(rotation_seven_black_soil_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */