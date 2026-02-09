% ============================================================================
% CONSTRAINT STORY: visa_judgment_sharing_agreement
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Visa Inc. Amendment No. 6 to Form S-1 
% ============================================================================

:- module(visa_judgment_sharing_agreement, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: visa_judgment_sharing_agreement
 * human_readable: Judgment Sharing Agreement (AMEX Case)
 * domain: legal/economic
 * temporal_scope: July 2007 (Filing date) 
 * spatial_scope: National (United States Judiciary) 
 * * SUMMARY:
 * A contractual coordination mechanism between Visa U.S.A. Inc. and its 
 * signatory member banks to allocate financial liability arising from 
 * antitrust litigation (specifically the American Express case). 
 * This agreement prevents individual member banks from being single-handedly 
 * destroyed by joint and several liability.
 * * KEY AGENTS:
 * - Visa U.S.A. Inc.: The central entity coordinating the defense.
 * - Signatory Banks: Member institutions sharing the risk of a multi-billion dollar judgment.
 * - American Express (AMEX): The plaintiff whose legal victory creates the liability pressure.
 * * NARRATIVE ARC:
 * To facilitate the IPO, Visa must ringfence and quantify its massive legal 
 * liabilities. The Judgment Sharing Agreement (JSA) transforms an unpredictable 
 * external threat (antitrust judgment) into a structured internal obligation 
 * (shared loss) .
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(visa_litigation_ringfencing, 0, 10).
narrative_ontology:constraint_claim(visa_judgment_sharing_agreement, rope).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: The agreement extracts capital from banks to pay settlements, but 
% the flow is proportional to their historical involvement and protects the 
% system from collapse.
domain_priors:base_extractiveness(visa_judgment_sharing_agreement, 0.4).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Banks that want to remain part of the Visa network for the IPO 
% restructure essentially have no alternative but to sign.
domain_priors:suppression_score(visa_judgment_sharing_agreement, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(visa_judgment_sharing_agreement, extractiveness, 0.4).
narrative_ontology:constraint_metric(visa_judgment_sharing_agreement, suppression_requirement, 0.7).

% Enforcement requirements
domain_priors:requires_active_enforcement(visa_judgment_sharing_agreement).

% Metrics for Executive Summary
% Beneficiaries and Victims
narrative_ontology:constraint_beneficiary(visa_judgment_sharing_agreement, visa_inc_shareholders).
narrative_ontology:constraint_victim(visa_judgment_sharing_agreement, signatory_member_banks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Small Signatory Bank - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Financial institution but small relative to the system.
   WHEN: immediate - Concerned with the upcoming quarterly impact of loss sharing.
   WHERE: trapped - Leaving the network during an IPO restructuring is prohibitively costly.
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   For a smaller bank, this is a "Snare." They are forced to sign an agreement 
   that binds them to pay for the "sins" of the network's past antitrust 
   behavior, with the terms dictated by the larger Visa corporate entity 
   to ensure the IPO's success.
   
   NARRATIVE EVIDENCE:
   The agreement is "by and between Visa U.S.A. Inc. and the signatory banks 
   thereto," implying a take-it-or-leave-it participation requirement.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    visa_judgment_sharing_agreement,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(visa_judgment_sharing_agreement, visa_inc),
        constraint_victim(visa_judgment_sharing_agreement, small_bank),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(visa_judgment_sharing_agreement, S),
    S > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Visa Chief Legal Officer - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Crafting the legal architecture of the IPO.
   WHEN: biographical - Aiming for a stable, public Visa Inc. over the next 20 years.
   WHERE: arbitrage - Using contract law to mitigate judicial risk.
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   From the institutional perspective, the JSA is a "Rope." It is a functional 
   coordination tool that prevents the "systemic risk" of one bank failing and 
   dragging down the network. It allows the IPO to proceed by providing 
   certainty to new investors.
   
   NARRATIVE EVIDENCE:
   "Judgment Sharing Agreement among Defendants... incorporated by reference" 
   as a key exhibit for investor transparency.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    visa_judgment_sharing_agreement,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        constraint_beneficiary(visa_judgment_sharing_agreement, global_payment_system),
        constraint_victim(visa_judgment_sharing_agreement, none),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(visa_judgment_sharing_agreement, E),
    E < 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: The Judiciary / SEC Observer - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observing the legal reality of the filings.
   WHEN: historical - The antitrust case is a settled "fact" of the 2000s era.
   WHERE: analytical - Not a party to the contract.
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   For an observer, these liabilities and the agreements to share them are a 
   "Mountain." They are the fixed landscape of Visa's history that cannot be 
   erased; they must be navigated and disclosed as immutable risks.
   
   NARRATIVE EVIDENCE:
   Referenced repeatedly across multiple amendments (No. 1 through No. 6) 
   as a permanent part of the registration statement.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    visa_judgment_sharing_agreement,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        constraint_beneficiary(visa_judgment_sharing_agreement, none),
        constraint_victim(visa_judgment_sharing_agreement, none),
        spatial_scope(national)
    )
) :-
    true,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(visa_jsa_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(visa_judgment_sharing_agreement, Type1, 
        context(individual_moderate, immediate, trapped, _, _, national)),
    constraint_indexing:constraint_classification(visa_judgment_sharing_agreement, Type2, 
        context(institutional, biographical, arbitrage, _, _, global)),
    Type1 = snare,
    Type2 = rope.

test(systemic_stability_logic) :-
    % Testing that institutional power views this as a functional tool (Rope)
    constraint_indexing:constraint_classification(visa_judgment_sharing_agreement, rope, 
        context(institutional, _, _, _, _, _)).

:- end_tests(visa_jsa_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.4): Significant because it mandates large payments 
 * from banks to cover settlements.
 * 2. CLASSIFICATION: Chose "Rope" for the institution because the JSA is 
 * what enables the "Global Restructuring Agreement". Without it, 
 * the IPO (a larger Rope) would fail.
 * * AMBIGUITIES:
 * The S-1 does not disclose the *exact* percentages of liability sharing, 
 * which are likely in the "confidential treatment" portions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    allocation_fairness,
    "Is the liability allocation based on revenue (Rope) or on political leverage within the Visa association (Snare)?",
    resolution_mechanism("Audit of the confidential exhibit filed with the SEC Secretary"),
    impact("If leverage-based: it is a Snare for smaller banks. If revenue-based: it is a Rope for the network."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Individual Litigation
 * Viability: Every bank defends itself against AMEX. 
 * Suppression: Rejected because it would lead to inconsistent judgments and 
 * potentially bankrupt smaller members, ruining the Visa brand before the IPO.
 * * CONCLUSION:
 * The rejection of individual litigation effectively forces the JSA into a 
 * "Mountain" status for those who want to remain in the payment ecosystem.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [visa_judgment_sharing_agreement].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, snare, agent_power(powerless)).

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
% Coordination mechanism in legal domain — moderate institutional framing
domain_priors:theater_ratio(visa_judgment_sharing_agreement, 0.14).
narrative_ontology:constraint_metric(visa_judgment_sharing_agreement, theater_ratio, 0.14).
