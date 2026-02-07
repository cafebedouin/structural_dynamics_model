% ============================================================================
% CONSTRAINT STORY: blackstone_carried_interest_taxation
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: The Blackstone Group L.P. Form S-1 (March 22, 2007)
% ============================================================================

:- module(blackstone_carried_interest_taxation, []).

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
 * * constraint_id: blackstone_carried_interest_taxation
 * human_readable: Carried Interest Partnership Taxation
 * domain: economic/political
 * temporal_scope: 2007 (Pre-IPO filing context)
 * spatial_scope: National (United States)
 * * SUMMARY:
 * The regulatory and tax framework that treats "carried interest" (performance 
 * fees) as capital gains rather than ordinary income for partners in investment 
 * firms. Blackstone's structure as a publicly traded partnership 
 * relies on this classification to avoid entity-level taxation and maintain 
 * high distributions to its "Senior Managing Directors".
 * * KEY AGENTS:
 * - Blackstone Senior Managing Directors: The primary beneficiaries of the "carried interest" tax rate.
 * - Public Common Unitholders: New investors who bear the risk of tax law changes but have limited voting rights.
 * - U.S. Congress/IRS: The institutional "rule-makers" who can reclassify this income.
 * * NARRATIVE ARC:
 * Blackstone is transitioning to a public entity while attempting to preserve 
 * the tax advantages of a private partnership. The S-1 
 * highlights that any legislative change to the taxation of carried interest 
 * could "materially increase" the tax liability and reduce the value of 
 * common units.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(blackstone_ipo_restructuring, 0, 10).
narrative_ontology:constraint_claim(blackstone_carried_interest_taxation, rope).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: The system allows partners to retain a high percentage of earnings 
% (low extraction from them), but extracts revenue from the public treasury 
% via lower tax receipts.
domain_priors:base_extractiveness(blackstone_carried_interest_taxation, 0.3).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: The "partnership" structure is heavily defended by legal 
% opinions, and alternative corporate structures are actively avoided to 
% maintain tax efficiency.
domain_priors:suppression_score(blackstone_carried_interest_taxation, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, extractiveness, 0.3).
narrative_ontology:constraint_metric(blackstone_carried_interest_taxation, suppression_requirement, 0.7).

% Enforcement requirements
domain_priors:requires_active_enforcement(blackstone_carried_interest_taxation).

% Metrics for Executive Summary
% Beneficiaries and Victims
constraint_beneficiary(blackstone_carried_interest_taxation, senior_managing_directors).
constraint_victim(blackstone_carried_interest_taxation, public_tax_revenue).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Senior Managing Director - Rope
   --------------------------------------------------------------------------
   
   WHO: powerful - Significant influence over the firm's structure.
   WHEN: biographical - Long-term wealth accumulation strategy.
   WHERE: arbitrage - Can navigate complex tax codes to optimize returns.
   SCOPE: global - Managing assets across multiple jurisdictions.
   
   WHY THIS CLASSIFICATION:
   For the partners, this tax framework is a "Rope"—a functional coordination 
   mechanism that allows them to align their interests with fund investors 
   while maximizing their net-of-tax compensation.
   
   NARRATIVE EVIDENCE:
   "We intend to manage our affairs so that we will not be treated as a 
   corporation for U.S. federal income tax purposes".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    blackstone_carried_interest_taxation,
    rope,
    context(
        agent_power(powerful),
        time_horizon(biographical),
        exit_options(arbitrage),
        constraint_beneficiary(blackstone_carried_interest_taxation, senior_managing_directors),
        constraint_victim(blackstone_carried_interest_taxation, none),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(blackstone_carried_interest_taxation, E),
    E < 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Common Unitholder (Retail) - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - No voting power; subject to partnership terms.
   WHEN: immediate - Focused on the yield and unit price of the IPO.
   WHERE: constrained - Can sell units, but cannot change the firm's tax status.
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   For the retail investor, the tax structure is a "Mountain." It is a pre-existing 
   and highly complex "risk factor" they must accept. They have no power to 
   influence the legislative battle over carried interest and must simply 
   weather any "storms" (law changes).
   
   NARRATIVE EVIDENCE:
   "Our common unitholders will have only limited voting rights and will have 
   no right to elect our general partner".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    blackstone_carried_interest_taxation,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(blackstone_carried_interest_taxation, blackstone_management),
        constraint_victim(blackstone_carried_interest_taxation, retail_investor),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(blackstone_carried_interest_taxation, S),
    S > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: Reformist Legislator - Snare
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power (Congress).
   WHEN: historical - Looking at the 100-year evolution of the tax code.
   WHERE: analytical - Evaluating the "fairness" of the loophole.
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   From the perspective of a critic, this is a "Snare." It is an asymmetric 
   mechanism where the wealthiest 1% of the firm "extracts" a lower tax rate 
   that is unavailable to the general public, effectively strangling the 
   principle of horizontal equity in taxation.
   
   NARRATIVE EVIDENCE:
   The S-1 notes that "The U.S. Congress has recently considered... legislation 
   that would... treat as ordinary income the 'carried interest'".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    blackstone_carried_interest_taxation,
    snare,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        constraint_beneficiary(blackstone_carried_interest_taxation, private_equity_industry),
        constraint_victim(blackstone_carried_interest_taxation, public_equity_principles),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(blackstone_carried_interest_taxation, E),
    E > 0.2,
    domain_priors:requires_active_enforcement(blackstone_carried_interest_taxation),
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(blackstone_tax_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, Type1, 
        context(powerful, biographical, arbitrage, _, _, global)),
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, Type2, 
        context(powerless, immediate, constrained, _, _, national)),
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, Type3, 
        context(institutional, historical, analytical, _, _, national)),
    Type1 = rope,
    Type2 = mountain,
    Type3 = snare.

test(tax_benefit_asymmetry) :-
    % Powerful agents should experience lower extraction (Rope) than the system's impact on others
    constraint_indexing:constraint_classification(blackstone_carried_interest_taxation, rope, context(powerful, _, _, _, _, _)).

:- end_tests(blackstone_tax_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.3): Low because the constraint *prevents* extraction from 
 * the partners, though it represents a high "opportunity cost" for the IRS.
 * 2. SUPPRESSION (0.7): Blackstone uses high-level legal opinions (Skadden, Arps) 
 * to validate this structure, effectively suppressing the visibility of 
 * alternative, simpler corporate taxes.
 * * PERSPECTIVE RATIONALE:
 * - Partners: See it as a Rope (coordination).
 * - Retail: See it as a Mountain (unalterable risk).
 * - Legislators: See it as a Snare (asymmetric advantage).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legislative_volatility,
    "Will Congress reclassify carried interest as ordinary income within the 'biographical' window of the IPO?",
    resolution_mechanism("Monitor the introduction of bills in the Senate Finance Committee specifically targeting PTPs"),
    impact("If resolved as 'Yes': Blackstone's valuation drops (Mountain becomes Snare). If 'No': Rope holds."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: C-Corporation Structure
 * Viability: Standard for most public companies; would provide voting rights 
 * to all.
 * Suppression: Actively rejected due to "double taxation" of dividends 
 * which would harm senior management returns.
 * * CONCLUSION:
 * The presence of a viable (but rejected) C-Corp alternative confirms that 
 * the Partnership structure is a "Rope" for those in power and a "Snare" 
 * for those concerned with tax equity.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [blackstone_carried_interest_taxation].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
