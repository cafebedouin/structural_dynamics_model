% ============================================================================
% CONSTRAINT STORY: gale_shapley
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Market Design / Alvin Roth / Lloyd Shapley
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_gale_shapley, []).

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
 * constraint_id: gale_shapley
 * human_readable: Gale-Shapley Stable Matching
 * domain: economic/mathematical
 * temporal_scope: 1952-Present (NRMP Context)
 * spatial_scope: Global Labor Markets
 * 
 * SUMMARY:
 * The algorithm guarantees a stable matching where no two agents have a mutual 
 * incentive to defect. In medical residency, 
 * this "Stability" is a non-negotiable fact (Mountain) that removes 
 * individual bargaining power (Snare).
 * 
 * KEY AGENTS:
 * - The_Algorithm (Analytical): Mathematical guarantor.
 * - Hospital_Systems (Institutional): Use stability to ensure labor.
 * - Medical_Applicants (Individual Powerless): Geographic autonomy extracted.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(gale_shapley, 0, 10).
narrative_ontology:constraint_claim(gale_shapley, tangled_rope).

% Base extractiveness: 0.8. 
% High extraction of individual agency to maintain market stability.
domain_priors:base_extractiveness(gale_shapley, 0.8).

% Suppression score: 0.8.
% "Unmatched" alternatives (black markets, side deals) are often strictly 
% prohibited to maintain market "thickness" and stability.
domain_priors:suppression_score(gale_shapley, 0.8).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(gale_shapley, extractiveness, 0.8).
narrative_ontology:constraint_metric(gale_shapley, suppression_requirement, 0.8).

% Enforcement: Usually requires active enforcement (Clearinghouses/Regulations).
domain_priors:requires_active_enforcement(gale_shapley).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(gale_shapley, hospital_administrators).
constraint_victim(gale_shapley, residency_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE UNMATCHED APPLICANT - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Lacks credentials or signaling power)
   WHEN: immediate (During a specific matching cycle)
   WHERE: trapped (Excluded from the market due to no match)
   
   WHY THIS CLASSIFICATION:
   For the agent who fails to secure a match in a "thick" market, the system is a 'Snare'.
   The high suppression of alternatives means that failing to match is equivalent to total exclusion.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gale_shapley,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE INSTITUTION / CLEARINGHOUSE - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Seeks to curate a specific cohort or allocate resources)
   WHEN: historical (Over multiple matching cycles)
   WHERE: arbitrage (Balances competing preferences and allocates resources)
   
   WHY THIS CLASSIFICATION:
   For the institution, the matching market is a 'Rope'. It's a tool to
   efficiently allocate resources and talent, ensuring stable and optimal outcomes.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gale_shapley,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ECONOMIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observes the underlying principles of market design)
   WHEN: civilizational (Studying economic laws across different societies)
   WHERE: analytical (Universal principles of matching theory)
   
   WHY THIS CLASSIFICATION:
   The economist sees the "laws of matching" as a 'Mountain'. The need 
   for thickness, the danger of market congestion, and the inevitability 
   of signaling costs are natural features of information-dense environments.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gale_shapley,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(gale_shapley_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(gale_shapley, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gale_shapley, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(gale_shapley, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(gale_shapley_tests).

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
 * 1. MANDATROPHY STATUS: High extractiveness (0.8) and suppression (0.8)
 *    confirm this as a severe Mandatrophic constraint. It is 'RESOLVED' by
 *    recognizing that the stability for the system (Rope) comes at the cost of
 *    individual agency (Snare).
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Unmatched Applicant (Snare): Excluded from the market.
 *    - Institution (Rope): A tool for efficient allocation.
 *    - Economist (Mountain): Immutable laws of market design.
 * 
 * 3. CORE INSIGHT: The Gale-Shapley algorithm creates a 'Tangled Rope'.
 *    It's a 'Rope' that provides stability and efficiency for institutions,
 *    but it becomes a 'Snare' for individuals who are left unmatched,
 *    highlighting the trade-off between systemic stability and individual choice.
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
    stability_premium,
    "Does the 'Stability' of the match benefit the Receiver (Applicant) enough to offset the loss of 'Agency' (Direct Negotiation)?",
    resolution_mechanism("Comparison of career outcomes and satisfaction in matched vs. unmatched/alternative entry labor markets over time."),
    impact("If agency > stability: The algorithm functions as a Snare for applicants. If stability > agency: It primarily functions as a Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Decentralized Negotiation
 *    Viability: Historically, direct negotiation was attempted but led to market unraveling (early offers, early acceptances). It is a viable *concept* but leads to instability in practice.
 *    Suppression: Actively suppressed by institutional agreements (e.g., NRMP rules) due to the fear of "market unraveling" and chaotic labor poaching, which benefits institutions.
 *
 * CONCLUSION:
 * The Gale-Shapley algorithm is a 'Rope' designed to prevent the 'Snare' of market
 * unraveling. However, by suppressing decentralized negotiation, it creates a
 * 'Snare' of its own for applicants who lose agency and are left unmatched.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/gale_shapley].
 * 2. Multi-perspective: ?- multi_index_report(gale_shapley).
 * 3. Run tests: ?- run_tests(gale_shapley_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
