% ============================================================================
% CONSTRAINT STORY: coinbase_regulatory_uncertainty
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Coinbase Global, Inc. Form S-1 (February 25, 2021)
% ============================================================================

:- module(coinbase_reg_uncertainty, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: coinbase_regulatory_uncertainty
 * human_readable: Crypto-Regulatory Ambiguity
 * domain: political/legal
 * temporal_scope: 2021-Ongoing
 * spatial_scope: Global / National (US focus)
 * * SUMMARY:
 * The lack of a clear, unified legal framework for crypto assets, specifically 
 * whether certain assets are "securities" under the SEC or "commodities" under 
 * the CFTC. This creates a state of permanent "legal 
 * debt" for the firm.
 * * KEY AGENTS:
 * - SEC/Regulators: The institutional "mountain-makers" who enforce 
 * traditional rules on new technology.
 * - Coinbase Compliance Team: The engineers trying to build a "Rope" (compliance 
 * framework) through an shifting landscape.
 * - Asset Issuers: Third parties whose tokens may be reclassified as illegal 
 * securities, impacting Coinbase's inventory.
 * * NARRATIVE ARC:
 * Coinbase admits that they are operating in a "gray zone". The S-1 
 * details a landscape where a single regulatory change could render their 
 * core product offerings illegal or prohibitively expensive.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

narrative_ontology:interval(coinbase_reg_pivot, 0, 10).
narrative_ontology:constraint_claim(coinbase_regulatory_uncertainty, mountain).

% Base extractiveness score (0.0 - 1.0)
% Rationale: High potential for "punitive extraction" via fines, legal 
% fees, and mandatory compliance overhead.
domain_priors:base_extractiveness(coinbase_regulatory_uncertainty, 0.7).

% Suppression score (0.0 - 1.0)
% Rationale: Very high; you cannot "vote" or "exit" your way out of 
% federal oversight if you want to remain a US public company.
domain_priors:suppression_score(coinbase_regulatory_uncertainty, 0.9).

% Enforcement requirements
domain_priors:requires_active_enforcement(coinbase_regulatory_uncertainty).

% Metrics
narrative_ontology:constraint_metric(coinbase_regulatory_uncertainty, extractiveness, 0.7).
narrative_ontology:constraint_metric(coinbase_regulatory_uncertainty, suppression_requirement, 0.9).

% Beneficiaries and Victims
constraint_beneficiary(coinbase_regulatory_uncertainty, traditional_financial_incumbents).
constraint_victim(coinbase_regulatory_uncertainty, innovation_agility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Coinbase General Counsel - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_powerful / institutional.
   WHEN: historical.
   WHERE: arbitrage - Actively lobbying and litigating to shape the "Rope".
   
   WHY THIS CLASSIFICATION:
   For the legal team, regulation is a "Rope." They believe that by being 
   the "most compliant" exchange, they create a competitive moat that 
   strangles less-regulated competitors while pulling institutional capital 
   into the fold.
   
   NARRATIVE EVIDENCE:
   "We have a long history of proactively engaging with regulators... we believe 
   our compliance-first approach is a key differentiator".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    coinbase_regulatory_uncertainty,
    rope,
    context(
        agent_power(individual_powerful),
        time_horizon(historical),
        exit_options(arbitrage),
        constraint_beneficiary(coinbase_regulatory_uncertainty, coinbase_market_share),
        constraint_victim(coinbase_regulatory_uncertainty, offshore_exchanges),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(coinbase_regulatory_uncertainty, E),
    E < 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Public Unitholder - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless.
   WHEN: biographical.
   WHERE: constrained - Can sell the stock, but cannot affect the SEC.
   
   WHY THIS CLASSIFICATION:
   For the investor, the SEC is a "Mountain." It is a massive, external 
   force of nature that could trigger a "Noose" event (delisting, fines) 
   at any moment, and there is absolutely nothing the investor can do to 
   mitigate that risk except wait.
   
   NARRATIVE EVIDENCE:
   "Any enforcement action... could result in significant fines... and harm 
   our business, reputation, and financial results".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    coinbase_regulatory_uncertainty,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(constrained),
        constraint_beneficiary(coinbase_regulatory_uncertainty, regulatory_state),
        constraint_victim(coinbase_regulatory_uncertainty, coinbase_shareholder),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(coinbase_regulatory_uncertainty, S),
    S > 0.8,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(coinbase_reg_tests).

test(compliance_moat_logic) :-
    % Verify that powerful agents view regulation as a strategic tool (Rope)
    constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, rope, 
        context(individual_powerful, _, arbitrage, _, _, _)).

test(shareholder_risk_audit) :-
    % Verify that the powerless agent views the legal gray zone as a Mountain
    constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, mountain, 
        context(individual_powerless, _, constrained, _, _, _)).

:- end_tests(coinbase_reg_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * KEY DECISIONS:
 * 1. PERSPECTIVAL DIVERGENCE: Coinbase's management views regulation as 
 * their "unfair advantage" (Rope), while the S-1 warning reveals it is 
 * the investor's greatest threat (Mountain).
 * 2. EXTRACTIVENESS (0.7): Reflects the high "tax" of being a pioneer in 
 * a regulated space—legal fees are a primary operating expense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    sec_howey_redefinition,
    "Will the SEC successfully apply the 1946 Howey Test to modern decentralized protocols in a way that bans secondary trading?",
    resolution_mechanism("Monitor the outcome of SEC v. Ripple and subsequent appellate rulings"),
    impact("If Yes: The Mountain becomes a Noose (Coinbase delists major assets). If No: The Rope holds."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Comprehensive Federal Crypto Legislation
 * Viability: Proposed in various bills but not yet enacted.
 * Suppression: "Regulation by enforcement" (the current path) actively 
 * suppresses this legislative alternative by setting precedents in court.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [coinbase_regulatory_uncertainty].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% ==========================================================================
% [SKELETON REPAIR] Missing Perspectival Pillars Added: NOOSE
% To be calibrated: Define the narrative justification for these perspectives.
% ==========================================================================
constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, noose, agent_power(individual_powerless)).
