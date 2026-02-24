% ============================================================================
% CONSTRAINT STORY: taiwan_existential_sovereignty
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Geopolitical Strategy Analysis / Cross-Strait Relations
% ============================================================================

:- module(constraint_taiwan_sovereignty, []).

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
 * * constraint_id: taiwan_existential_sovereignty
 * human_readable: The Taiwan Existential Sovereignty Constraint
 * domain: political/economic/technological
 * temporal_scope: 1949-Present
 * spatial_scope: Taiwan / Taiwan Strait
 * * SUMMARY:
 * This constraint models the condition of Taiwan's existence under persistent 
 * existential threat. It is characterized by the "Silicon Shield" (economic 
 * indispensability) acting as a Rope, while the military imbalance and 
 * diplomatic isolation function as a Snare. For the population, the 
 * "Status Quo" has become an unyielding Mountain—the only navigable terrain 
 * between the catastrophic extraction of a conflict and the loss of autonomy.
 * * KEY AGENTS:
 * - The Taiwanese Citizen: Individual powerless; lives within the "Status Quo" 
 * as a fact of nature while managing daily life under the shadow of the Strait.
 * - The Taiwanese Government: Institutional; manages the "Silicon Shield" and 
 * international alignments as a functional coordination Rope.
 * - PRC (Strategic Actor): Institutional/External; the source of the 
 * coercive Snare designed to extract political unification.
 * * NARRATIVE ARC:
 * Taiwan’s autonomy is maintained through a delicate "Rope" of global 
 * semiconductor dominance. However, as the military "Gray Zone" tactics 
 * (salami-slicing) increase, this Rope is being tested by a tightening 
 * "Snare" of blockade simulations and diplomatic suppression.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the Deferential Realism suite
narrative_ontology:interval(taiwan_status_quo_cycle, 0, 10).
narrative_ontology:constraint_claim(taiwan_existential_sovereignty, rope).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.7. High; the constant threat "extracts" massive resources 
% into defense spending and psychological resilience, while diplomatic 
% isolation extracts the ability to participate in global governance.
domain_priors:base_extractiveness(taiwan_existential_sovereignty, 0.7).

% Suppression score (0.0-1.0)
% Rationale: 0.8. High; any move toward "formal independence" or 
% "unification under PRC terms" is strictly suppressed by the opposing 
% force, leaving only the thin "Status Quo" path.
domain_priors:suppression_score(taiwan_existential_sovereignty, 0.8).

% Enforcement requirements
% Requires active enforcement (ADIZ patrols, median line monitoring, 
% and international trade agreements).
domain_priors:requires_active_enforcement(taiwan_existential_sovereignty).

% Metrics for Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(taiwan_existential_sovereignty, global_semiconductor_stability).
constraint_beneficiary(taiwan_existential_sovereignty, us_pacific_presence).
constraint_victim(taiwan_existential_sovereignty, taiwanese_diplomatic_freedom).
constraint_victim(taiwan_existential_sovereignty, cross_strait_stability_predictability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE TAIWANESE CITIZEN - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - Cannot change the geopolitical reality; 
         must live within the existing tensions.
   WHEN: immediate - Tactical focus on peace, economy, and democratic life.
   WHERE: trapped - Bound by the physical geography of the island.
   SCOPE: local - Daily survival and societal flourishing.
   
   WHY THIS CLASSIFICATION:
   For the citizen, the threat from the mainland is a Mountain. It has 
   existed for generations; it is a permanent feature of the sky and the sea. 
   One does not "resolve" the Mountain; one simply builds a life around it.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    taiwan_existential_sovereignty,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(taiwan_existential_sovereignty),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: TAIWANESE TECH SECTOR (TSMC) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Holds the "Silicon Shield" which coordinates 
         global interest in Taiwan's safety.
   WHEN: biographical - Planning multi-decade fabrication horizons.
   WHERE: mobile - Establishing global foundries while keeping core tech local.
   SCOPE: global - The backbone of the modern electronic world.
   
   WHY THIS CLASSIFICATION:
   To the tech sector and the government that shields it, this is a Rope. 
   It is a functional coordination mechanism that makes the "extraction" 
   of a conflict too expensive for the global community to allow. It is 
   a thread that binds the world's survival to Taiwan's autonomy.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    taiwan_existential_sovereignty,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(taiwan_existential_sovereignty, E),
    E < 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: PRC MILITARY STRATEGIST - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the "Gray Zone" effectiveness.
   WHEN: historical - Evaluating the 100-year goal of "reunification."
   WHERE: analytical - Seeking to close the "First Island Chain" gap.
   SCOPE: national - The core interest of the PRC state.
   
   WHY THIS CLASSIFICATION:
   The strategist sees the Snare. By slowly tightening the physical and 
   diplomatic constraints—economic coercion, military drills, and 
   international exclusion—they aim to "choke" Taiwan's options until 
   the extraction of unification becomes the only remaining path.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    taiwan_existential_sovereignty,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(taiwan_existential_sovereignty, S),
    S > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TAIWANESE STATE / DEMOCRACY (INSTITUTIONAL) - Rope
   --------------------------------------------------------------------------
   WHO: agent_power(institutional) - The governing bodies and global partners.
   WHEN: generational - Managing a decades-long state of "Permanent Liminality."
   WHERE: arbitrage - Managing the friction between major global powers.
   SCOPE: global - A pivot point for the global economy and security.
   
   WHY THIS CLASSIFICATION:
   Sovereignty is a Rope. It is a high-fidelity coordination mechanism 
   designed to "pull" international support and domestic consensus into a 
   stable status quo. The "Silicon Shield" provides the economic slack 
   necessary to prevent kinetic escalation, while democratic signaling 
   tightens the reputational cost for any agent attempting to force a 
   unilateral change to the geopolitical terrain.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    taiwan_existential_sovereignty,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:requires_active_enforcement(taiwan_existential_sovereignty),
    !.

% Explicit priors reflecting the coordination energy of the "Silicon Shield."
domain_priors:base_extractiveness(taiwan_existential_sovereignty, 0.3).
domain_priors:suppression_score(taiwan_existential_sovereignty, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(taiwan_existential_sovereignty, extractiveness, 0.7).
narrative_ontology:constraint_metric(taiwan_existential_sovereignty, suppression_requirement, 0.8).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(taiwan_sovereignty_tests).

test(shield_utility_variance) :-
    % Tech sector (Rope) vs Citizen (Mountain)
    constraint_indexing:constraint_classification(taiwan_existential_sovereignty, T1, context(institutional, biographical, mobile, global)),
    constraint_indexing:constraint_classification(taiwan_existential_sovereignty, T2, context(powerless, immediate, trapped, local)),
    T1 \= T2.

test(high_extraction_signature) :-
    % The threat level indicates high baseline extraction of national focus.
    domain_priors:base_extractiveness(taiwan_existential_sovereignty, E),
    E > 0.6.

:- end_tests(taiwan_sovereignty_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. THE SILICON SHIELD AS ROPE: The most vital insight is that Taiwan's 
 * primary security constraint is an economic coordination mechanism. If the 
 * world did not need the chips, the Snare would have closed decades ago.
 * 2. THE MOUNTAIN OF THE STATUS QUO: For the average person, the "Status 
 * Quo" is not a choice; it is the physical terrain of their existence.
 * 3. SUPPRESSION (0.8): The high suppression reflects the total lack of 
 * "exit options" for Taiwan that do not involve extreme risk.
 */

omega_variable(
    semiconductor_diversification_impact,
    "Will the construction of foundries in the US and Germany (Rope loosening) 
     actually tighten the Snare by reducing Taiwan's strategic value?",
    resolution_mechanism("Monitor international response to cross-strait 
    tensions as global 'fab' capacity outside Taiwan increases"),
    impact("If response weakens: The Silicon Shield was a temporary Rope. 
            If it holds: The Rope is more than just economic."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Formal Independence
 * Viability: Historically viable as a sovereign desire; practically 
 * suppressed by the threat of immediate kinetic invasion (Mountain).
 * * ALTERNATIVE 2: "One Country, Two Systems" (Unification)
 * Viability: PRC's proposed Rope; rejected by Taiwan after the 
 * Hong Kong experience (seen as a Snare).
 * * CONCLUSION:
 * The active suppression of both "Extreme" alternatives creates the 
 * unique "Status Quo" Mountain that defines modern Taiwan.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [taiwan_existential_sovereignty].
% 2. Analyze: ?- multi_index_report(taiwan_existential_sovereignty).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
