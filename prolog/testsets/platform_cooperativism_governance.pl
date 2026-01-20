% ============================================================================
% CONSTRAINT STORY: platform_cooperativism_governance
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Domain Analysis of Worker-Owned Platforms (e.g., The Drivers Cooperative, Stocksy)
% ============================================================================

:- module(constraint_platform_coop, []).

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
 * * constraint_id: platform_cooperativism_governance
 * human_readable: Democratic Worker Governance and Surplus Retention
 * domain: economic/social/technological
 * temporal_scope: 2014-Present (Post-Scholz/Schneider Manifesto)
 * spatial_scope: Global (Distributed Digital Labor)
 * * SUMMARY:
 * Platform Cooperativism is a model where the digital platform (the app, the 
 * marketplace) is owned and governed by its workers, users, or customers. 
 * The core constraint is a "democratic mandate" that requires profits to be 
 * reinvested or distributed to members, and algorithms to be transparent 
 * and accountable to the collective.
 * * KEY AGENTS:
 * - Worker-Owner: Individual moderate; possesses voting rights and a 
 * share of the surplus.
 * - Coop Board: Institutional; elected body that manages the platform's 
 * strategic direction while remaining accountable to members.
 * - VC-Backed Competitor: External analytical; views the coop's lack of 
 * "blitzscaling" ability as a competitive weakness.
 * * NARRATIVE ARC:
 * This system functions as a Rope (functional coordination) designed to 
 * replace the extractive Noose of traditional gig platforms. However, the 
 * consensus-building requirements can feel like a Mountain (unchangeable 
 * friction) for agents accustomed to fast, centralized decision-making.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(cooperative_labor_era, 0, 10).
narrative_ontology:constraint_claim(platform_cooperativism_governance, rope).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.05. Very low; extraction is minimized as the surplus 
% is returned to the creators of the value.
domain_priors:base_extractiveness(platform_cooperativism_governance, 0.05).

% Suppression score (0.0-1.0)
% Rationale: 0.2. Low; transparency is a core tenet, and alternatives 
% (proprietary platforms) are visible but rejected on ideological grounds.
domain_priors:suppression_score(platform_cooperativism_governance, 0.2).

% Enforcement requirements
% Requires active enforcement: Bylaws, democratic voting protocols, 
% and transparent accounting systems must be maintained.
domain_priors:requires_active_enforcement(platform_cooperativism_governance).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(platform_cooperativism_governance, extractiveness, 0.05).
narrative_ontology:constraint_metric(platform_cooperativism_governance, suppression_requirement, 0.2).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(platform_cooperativism_governance, worker_members).
constraint_beneficiary(platform_cooperativism_governance, community_wealth_stability).
constraint_victim(platform_cooperativism_governance, venture_capital_returns).
constraint_victim(platform_cooperativism_governance, decision_making_velocity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COOP MEMBER (DRIVER/ARTIST) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has agency through voting; not a mere subject.
   WHEN: biographical - Investing in a long-term, stable livelihood.
   WHERE: mobile - While committed, the lack of extraction makes them less 
         "trapped" than on extractive platforms.
   SCOPE: local/regional - Often focused on specific local markets.
   
   WHY THIS CLASSIFICATION:
   For the member, the coop is a Rope. It is a coordination mechanism that 
   secures their labor and ensures they aren't "algorithmically deactivated" 
   without recourse. It provides a helpful, functional structure for their 
   economic life.
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
) :-
    domain_priors:base_extractiveness(platform_cooperativism_governance, E),
    E < 0.2,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: STARTUP TECH EXECUTIVE - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerful - Accustomed to high-velocity, top-down control.
   WHEN: immediate - Driven by quarterly metrics and rapid growth.
   WHERE: constrained - Bound by the "consensus" requirement of the coop.
   SCOPE: global - Attempting to scale a model.
   
   WHY THIS CLASSIFICATION:
   To an executive from the proprietary world, the coop's democratic 
   governance is a Mountain. The need to ballot the membership for major 
   architectural changes feels like an unyielding, unchangeable friction of 
   the landscape that prevents "pivoting" or "blitzscaling."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    platform_cooperativism_governance,
    mountain,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(global)
    )
) :-
    domain_priors:requires_active_enforcement(platform_cooperativism_governance),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: EXTRACTIVE COMPETITOR (UBER/DELIVEROO) - Noose
   --------------------------------------------------------------------------
   
   WHO: institutional - Powerful incumbent with massive capital.
   WHEN: generational - Protecting a dominant market model.
   WHERE: arbitrage - Uses capital to lower prices and squeeze out the coop.
   SCOPE: global - Universal market dominance.
   
   WHY THIS CLASSIFICATION:
   To the incumbent, the "coop mandate" is a Noose—not for the workers, but 
   for the business itself. Because the coop cannot extract profit to 
   subsidize predatory pricing, the incumbent views the coop's own 
   fairness rules as the very thing that will eventually "choke" its 
   ability to survive in a hyper-competitive capital market.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    platform_cooperativism_governance,
    noose,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(platform_cooperativism_governance, E),
    E < 0.1,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(platform_coop_tests).

test(multi_perspective_trust_gap) :-
    % Member (Rope) vs Tech Exec (Mountain)
    constraint_indexing:constraint_classification(platform_cooperativism_governance, T1, context(individual_moderate, biographical, mobile, regional)),
    constraint_indexing:constraint_classification(platform_cooperativism_governance, T2, context(individual_powerful, immediate, constrained, global)),
    T1 \= T2.

test(zero_extraction_signature) :-
    domain_priors:base_extractiveness(platform_cooperativism_governance, E),
    E < 0.1.

:- end_tests(platform_coop_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.05): This is the defining feature. By design, 
 * cooperatives aim for zero surplus extraction by outside capital.
 * 2. PERSPECTIVE INVERSION: I chose the "Tech Executive" and "Incumbent" 
 * perspectives to show that what is a Rope (freedom) for the worker 
 * can appear as a Noose or Mountain for capital-intensive agents.
 * 3. SUPPRESSION (0.2): Suppression is low because coops usually embrace 
 * open-source principles and "exit to community" ideologies.
 */

omega_variable(
    capital_access_omega,
    "Can a non-extractive Rope (Coop) scale in a financial ecosystem 
     designed for extractive Nooses (VC)?",
    resolution_mechanism("Monitor survival rates of platform coops vs. 
    venture-backed startups in the same sector over a 10-year period"),
    impact("If Yes: The Coop is a viable Rope for the future of work. 
            If No: The Coop is a niche Mountain that cannot scale."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: B-Corporations (Benefit Corporations)
 * Viability: High. Allows for profit-taking but mandates social goals. 
 * Easier to get capital than a pure coop.
 * Suppression: None; it is often the path taken when coops fail to 
 * secure funding.
 * * CONCLUSION:
 * Platform Cooperativism is the "Inverse Noose." It intentionally 
 * constrains capital to protect labor. Its success depends on whether 
 * the coordination benefits of the "Rope" (trust, lower churn) can 
 * outweigh the "Mountain" of capital-access friction.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [platform_cooperativism_governance].
% 2. Analyze: ?- multi_index_report(platform_cooperativism_governance).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
