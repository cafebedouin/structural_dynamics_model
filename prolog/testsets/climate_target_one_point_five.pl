% ============================================================================
% CONSTRAINT STORY: climate_target_one_point_five
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "The totemic 1.5°C climate target" by Madeleine Cuff
% ============================================================================

:- module(climate_target_one_point_five, []).

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
 * * constraint_id: climate_target_one_point_five
 * human_readable: The 1.5°C Global Warming Target
 * domain: political/technological
 * temporal_scope: 2000s-2026
 * spatial_scope: Global (UN Negotiating Blocs)
 * * SUMMARY:
 * The 1.5°C target is a policy constraint that lowered the global "safe" warming threshold from 2°C to 1.5°C. 
 * Championed by the Alliance of Small Island States (AOSIS), it redefines acceptable climate risk based on the 
 * survival of low-lying island nations rather than the economic convenience of larger powers.
 * * KEY AGENTS:
 * - AOSIS (Alliance of Small Island States): Negotiating bloc representing nations most vulnerable to sea-level rise.
 * - Lower-Income Nation Delegates: Agents who initially viewed the target as an economic developmental threat.
 * - Global Institutions (UN/Paris Agreement): Facilitators of the multi-lateral policy shift.
 * * NARRATIVE ARC:
 * Originally, 2°C was the "safe" threshold, but research suggested it threatened the total loss of island nations. 
 * AOSIS fought an "uphill battle" to insert 1.5°C into the 2015 Paris Agreement despite intense anger from other delegations. 
 * By 2026, it serves as the primary metric for climate policy, even as the world risks crossing it.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(climate_target_one_point_five, 0, 10).
narrative_ontology:constraint_claim([climate_target_one_point_five], [climate_policy_target]).

% Base extractiveness score (0.3 = Moderate)
% Rationale: The target extracts economic potential and development freedom from high-emission paths, but functions as a survival mechanism.
domain_priors:base_extractiveness(climate_target_one_point_five, 0.3).

% Suppression score (0.6 = High-Moderate)
% Rationale: The previous 2°C "safe" threshold has been largely suppressed and delegitimized by the 1.5°C consensus.
domain_priors:suppression_score(climate_target_one_point_five, 0.6).

% Enforcement requirements
% Requires active enforcement through UN summits and international agreements.
domain_priors:requires_active_enforcement(climate_target_one_point_five).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(climate_target_one_point_five, extractiveness, 0.3).
narrative_ontology:constraint_metric(climate_target_one_point_five, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(climate_target_one_point_five, small_island_states). % Survival of their geography
constraint_victim(climate_target_one_point_five, fossil_fuel_reliant_economies). % Economic constraints

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: AOSIS NEGOTIATOR (James Fletcher) - Rope
   --------------------------------------------------------------------------
   
   WHO: collective_organized (UN negotiating bloc)
   WHEN: generational (Future of island survival)
   WHERE: trapped (Physically located on low-lying islands)
   SCOPE: global (Negotiating at COP Paris)
   
   WHY THIS CLASSIFICATION:
   For AOSIS, 1.5°C is a "Rope"—a lifeline coordination mechanism used to pull the world away from 
   a 2°C "death sentence" for their territories. It is the only functional tool for survival.
   
   NARRATIVE EVIDENCE:
   "A 2°C warming limit 'would devastate many small island developing countries'".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    climate_target_one_point_five,
    rope,
    context(
        agent_power(collective_organized),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: LOWER-INCOME NATION DELEGATE - Noose
   --------------------------------------------------------------------------
   
   WHO: institutional (State representative)
   WHEN: immediate (Short-term economic/developmental goals)
   WHERE: constrained (Bound by UN diplomatic pressure)
   SCOPE: national (Representing home country interests)
   
   WHY THIS CLASSIFICATION:
   For agents focused on rapid economic industrialization, the 1.5°C target is a "Noose." 
   It tightens emission limits far more severely than the previous 2°C target, 
   perceived as a strangulation of economic growth.
   
   NARRATIVE EVIDENCE:
   "He was wagging his finger in my face and saying, 'You small island states will get 1.5°C over my dead body'".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    climate_target_one_point_five,
    noose,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PLANETARY ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Scientific observer)
   WHEN: historical (Century-scale climate dynamics)
   WHERE: analytical (Observer stance)
   SCOPE: global (Earth system scale)
   
   WHY THIS CLASSIFICATION:
   From a purely biophysical perspective, the planetary response to warming (like sea-level rise) is a "Mountain"—an 
   unchangeable physical reality. The target is merely a recognition of these immutable natural laws.
   
   NARRATIVE EVIDENCE:
   "Research was beginning to suggest... threatening sea level rise that would wipe out low-lying islands".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    climate_target_one_point_five,
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

:- begin_tests(climate_target_tests).

test(multi_perspective_policy) :-
    % Test that different perspectives yield different classifications
    constraint_indexing:constraint_classification(climate_target_one_point_five, Type1, context(agent_power(collective_organized), _, _, _)),
    constraint_indexing:constraint_classification(climate_target_one_point_five, Type2, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2.

test(time_immutability_shift) :-
    % Short-term institutional views it as a policy burden (Noose/Rope)
    % while long-term analytical views the impact as a fact (Mountain)
    constraint_indexing:constraint_classification(climate_target_one_point_five, mountain, context(_, historical, _, _)).

:- end_tests(climate_target_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.3):
 * Reasoning: Chose moderate because the target extracts economic freedom from high-emission states but gives survival to low-lying ones.
 * * 2. PERSPECTIVE SELECTION:
 * Focused on the AOSIS bloc (Lifeline/Rope) vs. the opposing delegations (Strangulation/Noose) to highlight the political tension described in the text.
 * * 3. CLASSIFICATION RATIONALE:
 * The 1.5°C target is a "Rope" for survival when viewed through the lens of those who have no exit from the geography of the islands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    target_physical_attainability,
    "Is the 1.5°C target physically achievable given current global emission inertia?",
    resolution_mechanism("Monitoring global temperature anomalies and greenhouse gas concentrations over 2026-2030"),
    impact("If unachievable: The target moves from a Rope to a symbolic Noose. If achievable: Remains a Rope."),
    confidence_without_resolution(medium)
).

omega_variable(
    economic_extraction_ratio,
    "Will the 1.5°C target result in a net transfer of wealth from low-income nations or a transition to new green wealth?",
    resolution_mechanism("Audit of global climate finance flows and green industrial development in lower-income nations"),
    impact("If purely extractive: Noose. If generative: Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The 2°C Threshold
 * Viability: The original "safe" consensus among scientists and policymakers in the early 21st century.
 * Suppression: Explicitly rejected by AOSIS and updated research showing it was "too severe".
 * Evidence: "Most scientists and policy-makers were focused on 2°C" until the research shifted.
 * * CONCLUSION:
 * The 1.5°C target successfully suppressed the 2°C alternative by redefining "safety" through the lens of survival for the most vulnerable.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [climate_target_one_point_five].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
