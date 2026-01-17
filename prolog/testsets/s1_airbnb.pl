% ============================================================================
% CONSTRAINT STORY: airbnb_str_regulation
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Airbnb, Inc. Form S-1 (November 16, 2020)
% ============================================================================

:- module(airbnb_str_regulation, []).

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
 * * constraint_id: airbnb_str_regulation
 * human_readable: Short-Term Rental (STR) Regulatory Framework
 * domain: economic/political/legal
 * temporal_scope: November 2020 (IPO) - Ongoing
 * spatial_scope: Global / Municipal (Local Zoning)
 * * SUMMARY:
 * A global patchwork of municipal laws, zoning restrictions, and tax requirements 
 * that govern home-sharing. These regulations range from simple registration to 
 * "One Host, One Home" policies and outright bans in high-density areas 
 *. The framework aims to balance the "sharing economy" 
 * with housing affordability and neighborhood character.
 * * KEY AGENTS:
 * - Individual Hosts: 90% of the platform, typically non-professional.
 * - Professional Hosts: Property management companies and boutique hotels.
 * - Local Municipalities: Rule-makers (e.g., San Francisco, NYC).
 * - Hotel Incumbents: Lobbyists for stricter oversight to equalize competition.
 * * NARRATIVE ARC:
 * Airbnb identifies regulation as a primary risk factor in its S-1, noting that 
 * complex and evolving laws may limit host participation or lead to significant 
 * penalties. The constraint acts as a filter that determines 
 * who can legally participate in the market.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(airbnb_ipo_era, 0, 10).
narrative_ontology:constraint_claim(airbnb_str_regulation, mountain).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Cities extract occupancy taxes and permit fees; hotels extract market 
% share by capping STR supply.
domain_priors:base_extractiveness(airbnb_str_regulation, 0.6).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Registration requirements and caps on nights explicitly suppress the 
% "casual" nature of the original P2P model.
domain_priors:suppression_score(airbnb_str_regulation, 0.8).

% Enforcement requirements
domain_priors:requires_active_enforcement(airbnb_str_regulation).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(airbnb_str_regulation, extractiveness, 0.6).
narrative_ontology:constraint_metric(airbnb_str_regulation, suppression_requirement, 0.8).

% Beneficiaries and Victims
constraint_beneficiary(airbnb_str_regulation, local_government_treasuries).
constraint_beneficiary(airbnb_str_regulation, hotel_industry).
constraint_victim(airbnb_str_regulation, individual_casual_hosts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Individual Casual Host - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Lacks lobbying power; subject to city decree.
   WHEN: biographical - Hosting is a supplemental income strategy.
   WHERE: trapped - Bound by the zoning of their primary residence.
   SCOPE: local - Impacted by neighborhood-specific ordinances.
   
   WHY THIS CLASSIFICATION:
   For the casual host, these rules are an unchangeable "Mountain." The cost 
   of compliance (permits, taxes) and the risk of fines are external 
   realities they must accept or exit the platform.
   
   NARRATIVE EVIDENCE:
   "Laws... may limit the ability or willingness of hosts to share their 
   spaces... and expose our hosts or us to significant penalties".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    airbnb_str_regulation,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        constraint_beneficiary(airbnb_str_regulation, municipal_government),
        constraint_victim(airbnb_str_regulation, individual_casual_host),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(airbnb_str_regulation, S),
    S > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Professional Host - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_moderate / institutional - Owns/manages multiple units.
   WHEN: immediate - Focused on quarterly returns and occupancy.
   WHERE: mobile / arbitrage - Can move capital to "light-regulation" cities.
   SCOPE: regional / national.
   
   WHY THIS CLASSIFICATION:
   Professional hosts see "One Host, One Home" laws as a "Noose"—a policy 
   specifically designed to throttle their business model in favor of the 
   hotel industry and housing activists.
   
   NARRATIVE EVIDENCE:
   Regulations are used to "limit external real estate investment and restore 
   the peer-to-peer... market to its original 'sharing' roots".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    airbnb_str_regulation,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        constraint_beneficiary(airbnb_str_regulation, incumbent_hotels),
        constraint_victim(airbnb_str_regulation, professional_host),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(airbnb_str_regulation, E),
    E > 0.5,
    domain_priors:requires_active_enforcement(airbnb_str_regulation),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: City Council / Regulator - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making authority.
   WHEN: historical - Managing long-term urban development and housing.
   WHERE: analytical - Balancing multiple stakeholder interests.
   SCOPE: regional / national.
   
   WHY THIS CLASSIFICATION:
   For regulators, the framework is a "Rope"—a necessary coordination tool 
   to mitigate noise complaints, ensure public safety, and prevent the 
   conversion of residential housing into "unregulated hotels".
   
   NARRATIVE EVIDENCE:
   Regulation addresses "gentrification, increasing risks to public safety 
   and worsening the housing affordability crisis".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    airbnb_str_regulation,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        constraint_beneficiary(airbnb_str_regulation, the_community),
        constraint_victim(airbnb_str_regulation, none),
        spatial_scope(regional)
    )
) :-
    % Classification logic for functional coordination
    true,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(airbnb_str_regulation_tests).

test(multi_perspective_divergence) :-
    constraint_indexing:constraint_classification(airbnb_str_regulation, Type1, 
        context(individual_powerless, biographical, trapped, _, _, local)),
    constraint_indexing:constraint_classification(airbnb_str_regulation, Type2, 
        context(individual_moderate, immediate, mobile, _, _, regional)),
    constraint_indexing:constraint_classification(airbnb_str_regulation, Type3, 
        context(institutional, historical, analytical, _, _, regional)),
    Type1 = mountain,
    Type2 = noose,
    Type3 = rope.

test(host_extractiveness_variance) :-
    % Professional hosts feel higher extraction relative to their business model (Noose)
    constraint_indexing:constraint_classification(airbnb_str_regulation, noose, context(individual_moderate, _, mobile, _, _, _)).

:- end_tests(airbnb_str_regulation_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. BASE EXTRACTIVENESS (0.6): High because regulations often include both 
 * direct fees (permits) and indirect economic losses for hosts (caps) 
 * that benefit specific incumbents.
 * 2. PERSPECTIVE SELECTION: Analyzed the divergence between "casual" hosts 
 * (powerless) and "professional" hosts (moderate power) because the 
 * S-1 highlights the "One Host, One Home" trend as a specific risk.
 * * CONFIDENCE:
 * High: Regulatory risk as a primary headwind.
 * Medium: The specific classification of "Rope" for regulators depends on 
 * their actual intent vs. capture by hotel interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    housing_market_correlation,
    "Do STR restrictions actually lower long-term rents, or do they simply reallocate traveler revenue to hotels?",
    resolution_mechanism("Post-regulation rent tracking in synthetic control cities (e.g., NYC post-2023)"),
    impact("If it lowers rents: it's a Rope for the city. If it only benefits hotels: it's a Noose for hosts."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Voluntary Platform Self-Regulation
 * Viability: Airbnb has introduced "Host standards" and safety features.
 * Suppression: Cities often reject this as "insufficient" for tax and zoning enforcement.
 * * CONCLUSION:
 * The active rejection of platform self-regulation in favor of state-mandated 
 * registration moves the constraint from a potential Rope to a Noose/Mountain 
 * for participants.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [airbnb_str_regulation].
% Multi-perspective: ?- constraint_indexing:multi_index_report(airbnb_str_regulation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
