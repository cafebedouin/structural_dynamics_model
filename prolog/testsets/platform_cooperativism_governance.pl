% ============================================================================
% CONSTRAINT STORY: platform_cooperativism_governance
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Domain Analysis of Worker-Owned Platforms (e.g., The Drivers Cooperative, Stocksy)
% ============================================================================

:- module(constraint_platform_cooperativism_governance, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: platform_cooperativism_governance
 * human_readable: Democratic Worker Governance in Platform Cooperativism
 * domain: economic/social/technological
 * temporal_scope: 2014-Present
 * spatial_scope: Global (Distributed Digital Labor)
 * 
 * SUMMARY:
 * Platform Cooperativism is a model where a digital platform is owned and governed 
 * by its workers. The core constraint is a "democratic mandate" that requires profits 
 * to be distributed to members and algorithms to be accountable to the collective.
 * 
 * KEY AGENTS:
 * - The Worker-Owner (Individual Moderate): Possesses voting rights and a share of the surplus.
 * - The VC-Backed Competitor (Institutional): Views the coop's lack of "blitzscaling" as a weakness.
 * - The Non-Member Gig Worker (Individual Powerless): Works for a competitor like Uber, sees the coop as an inaccessible alternative.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(platform_cooperativism_governance, 0, 10).
narrative_ontology:constraint_claim(platform_cooperativism_governance, rope).
narrative_ontology:human_readable(platform_cooperativism_governance, "Democratic Worker Governance in Platform Cooperativism").

% Base extractiveness score: 0.05 (Very Low)
% Rationale: By design, extraction is minimized as the surplus is returned 
% to the creators of the value (the workers).
domain_priors:base_extractiveness(platform_cooperativism_governance, 0.05).

% Suppression score: 0.2 (Low)
% Rationale: Transparency is a core tenet. Alternatives (proprietary platforms) 
% are visible but rejected on ideological grounds by members.
domain_priors:suppression_score(platform_cooperativism_governance, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(platform_cooperativism_governance, extractiveness, 0.05).
narrative_ontology:constraint_metric(platform_cooperativism_governance, suppression_requirement, 0.2).

% Enforcement: Requires active enforcement via bylaws, democratic voting, and transparent accounting.
domain_priors:requires_active_enforcement(platform_cooperativism_governance).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(platform_cooperativism_governance, worker_members).
narrative_ontology:constraint_victim(platform_cooperativism_governance, external_capital_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COOP MEMBER (WORKER-OWNER) - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate - Has agency through voting and ownership.
   WHEN: biographical - Investing in a long-term, stable livelihood.
   WHERE: mobile - While committed, the lack of extraction makes them less trapped.
   
   WHY THIS CLASSIFICATION:
   For the member, the coop is a 'Rope'. It is a coordination mechanism that 
   secures their labor, guarantees fair payment, and ensures they aren't 
   "algorithmically deactivated" without recourse. It provides a stable economic structure.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    platform_cooperativism_governance,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE NON-MEMBER GIG WORKER - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - Works for a competitor like Uber or DoorDash.
   WHEN: immediate - Focused on the next gig and daily earnings.
   WHERE: trapped - Subject to the algorithm of their current platform.
   
   WHY THIS CLASSIFICATION:
   For a worker on an extractive platform, the nearby cooperative is a 'Mountain'.
   It is a separate, inaccessible system with high barriers to entry (membership fees,
   democratic participation requirements) that seems distant and irrelevant to their
   immediate reality of algorithmic management.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    platform_cooperativism_governance,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE VC-BACKED COMPETITOR - Snare
   --------------------------------------------------------------------------
   WHO: institutional - Powerful incumbent with massive capital.
   WHEN: generational - Protecting a dominant market model.
   WHERE: arbitrage - Uses capital to lower prices and squeeze out the coop.
   
   WHY THIS CLASSIFICATION:
   To the incumbent, the coop's mandate is a 'Snare'—not for the workers, but 
   for the business model itself. Because the coop cannot extract profit to 
   subsidize predatory pricing, the incumbent views the coop's own fairness 
   rules as the very thing that will "choke" its ability to compete on capital.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    platform_cooperativism_governance,
    snare,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(platform_coop_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(platform_cooperativism_governance, Type1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(platform_cooperativism_governance, Type2, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_cooperativism_governance, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type1 \= Type3,
    Type2 \= Type3.

:- end_tests(platform_coop_tests).

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
 * 1. PERSPECTIVE SELECTION: Added the 'Non-Member Gig Worker' as the required
 *    'powerless' agent. This highlights that even a beneficial 'Rope'
 *    can appear as an inaccessible 'Mountain' to those outside its system.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Member (Rope): A tool for economic self-determination.
 *    - Non-Member (Mountain): An irrelevant, inaccessible alternative.
 *    - Competitor (Snare): A business model that cannot engage in extractive, hyper-competitive practices.
 * 
 * 3. EXTRACTIVENESS (0.05): This is the defining feature. By design, 
 *    cooperatives aim for zero surplus extraction by outside capital. The primary
 *    conflict is structural and ideological, not extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether this non-extractive model can survive in an
 * ecosystem dominated by extractive capital.
 */
omega_variable(
    capital_access_omega,
    "Can a non-extractive Rope (Coop) scale and survive in a financial ecosystem designed for extractive Snares (VC funding)?",
    resolution_mechanism("Monitor survival and market-share rates of platform coops vs. venture-backed startups in the same sector over a 10-year period."),
    impact("If Yes: The Coop is a viable Rope for the future of work. If No: The Coop is a niche Mountain that cannot achieve systemic scale."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Venture-Capital Backed Gig Platforms (e.g., Uber, DoorDash)
 *    Viability: The dominant, default model for digital platforms.
 *    Suppression: Not suppressed; it is the incumbent. The cooperative model is the one attempting to challenge this alternative.
 *
 * CONCLUSION:
 * Platform Cooperativism is an attempt to build a 'Rope' for workers in an environment
 * dominated by extractive 'Snares'. Its success depends on whether the coordination 
 * benefits of the 'Rope' (trust, lower churn, worker loyalty) can overcome the 
 * 'Mountain' of capital-access friction in a VC-dominated world.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/platform_cooperativism_governance].
 * 2. Multi-perspective: ?- multi_index_report(platform_cooperativism_governance).
 * 3. Run tests: ?- run_tests(platform_coop_tests).
 */

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
% Coordination mechanism in economic domain — moderate institutional framing
domain_priors:theater_ratio(platform_cooperativism_governance, 0.12).
narrative_ontology:constraint_metric(platform_cooperativism_governance, theater_ratio, 0.12).
