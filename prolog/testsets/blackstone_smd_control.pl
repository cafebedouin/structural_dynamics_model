% ============================================================================
% CONSTRAINT STORY: blackstone_smd_control
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: The Blackstone Group L.P. Form S-1 (March 22, 2007)
% ============================================================================

:- module(blackstone_smd_control, []).

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
 * * constraint_id: blackstone_smd_control
 * human_readable: Senior Managing Director Voting Control
 * domain: corporate_governance
 * temporal_scope: 2007 (IPO Restructuring)
 * spatial_scope: National (Delaware/US)
 * * SUMMARY:
 * A governance structure where Senior Managing Directors (SMDs) retain 
 * absolute control over the firm's general partner through a "multi-class" 
 * unit structure. This prevents public unitholders from 
 * electing directors or influencing management decisions.
 * * KEY AGENTS:
 * - Senior Managing Directors (SMDs): The controlling group of existing partners.
 * - Public Unitholders: New investors with almost zero voting power.
 * - Blackstone Group Management L.L.C.: The general partner entity.
 * * NARRATIVE ARC:
 * Unlike traditional corporations where shareholders can vote for boards, 
 * Blackstone's IPO is structured so that public investors provide capital 
 * but no oversight. This creates a permanent wall between 
 * the "founders" and the "public".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(blackstone_governance_lock, 0, 10).
narrative_ontology:constraint_claim(blackstone_smd_control, noose).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High power asymmetry; SMDs extract capital and fees while 
% denying public investors any governance rights.
domain_priors:base_extractiveness(blackstone_smd_control, 0.8).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Public unitholders have no physical or legal mechanism to 
% override the general partner; alternatives like proxy contests are barred.
domain_priors:suppression_score(blackstone_smd_control, 0.9).

% Enforcement requirements
domain_priors:requires_active_enforcement(blackstone_smd_control).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(blackstone_smd_control, extractiveness, 0.8).
narrative_ontology:constraint_metric(blackstone_smd_control, suppression_requirement, 0.9).

% Beneficiaries and Victims
constraint_beneficiary(blackstone_smd_control, senior_managing_directors).
constraint_victim(blackstone_smd_control, public_common_unitholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Public Unitholder - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - No right to elect the board or remove management.
   WHEN: biographical - Stuck with the governance structure for the life of the investment.
   WHERE: trapped - While units can be sold, the governance constraint is inescapable for the asset.
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   For the public investor, this is a "Noose." They are economically bound 
   to the performance of the firm but have zero strategic leverage. Any 
   mismanagement by the SMDs cannot be corrected through voting.
   
   NARRATIVE EVIDENCE:
   "Our common unitholders will have only limited voting rights and will 
   have no right to elect our general partner".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    blackstone_smd_control,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        constraint_beneficiary(blackstone_smd_control, smd_partners),
        constraint_victim(blackstone_smd_control, public_investor),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(blackstone_smd_control, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Stephen Schwarzman / SMDs - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_powerful - Retains nearly total control over the entity.
   WHEN: historical - Preserving the "culture" and "long-term focus" of the firm.
   WHERE: arbitrage - Using Delaware partnership law to bypass corporate norms.
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   For the SMDs, this structure is a "Rope." It coordinates the transition 
   to a public company without subjecting the partnership to the "short-term 
   pressures" or "hostile takeovers" associated with typical public markets.
   
   NARRATIVE EVIDENCE:
   The control is justified as ensuring "the long-term alignment of interests" 
   of the managing directors with the firm's growth.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    blackstone_smd_control,
    rope,
    context(
        agent_power(individual_powerful),
        time_horizon(historical),
        exit_options(arbitrage),
        constraint_beneficiary(blackstone_smd_control, blackstone_firm_stability),
        constraint_victim(blackstone_smd_control, none),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(blackstone_smd_control, E),
    E < 0.9,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(blackstone_control_tests).

test(voting_power_gap) :-
    % Test that powerless agents experience this as a Noose (high extraction of agency)
    constraint_indexing:constraint_classification(blackstone_smd_control, noose, 
        context(individual_powerless, _, trapped, _, _, _)).

test(smd_strategic_rope) :-
    % Test that powerful agents see this as a functional tool
    constraint_indexing:constraint_classification(blackstone_smd_control, rope, 
        context(individual_powerful, _, arbitrage, _, _, _)).

:- end_tests(blackstone_control_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.8): Ranked high because this structure "extracts" the 
 * fundamental democratic right of shareholders in a public entity.
 * 2. SUPPRESSION (0.9): The Delaware Limited Partnership structure is 
 * legally "hardened" against shareholder activism.
 * * AMBIGUITIES:
 * The "limited circumstances" under which unitholders *might* vote are 
 * highly restricted and likely irrelevant to daily operations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    gp_fiduciary_duty_erosion,
    "Will Delaware courts uphold the partnership's right to contractually waive fiduciary duties owed to unitholders?",
    resolution_mechanism("Monitor subsequent litigation in the Delaware Chancery Court (e.g., Dieckman v. Regency)"),
    impact("If duties can be waived: it is a total Noose. If not: a slight Rope exists for investors."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: "One Share, One Vote" Corporation
 * Viability: The standard for 99% of NYSE listings.
 * Suppression: Actively rejected to prevent external interference with 
 * SMD decision-making.
 * * CONCLUSION:
 * Rejection of standard voting rights confirms this as an asymmetric Noose 
 * for the public unitholder.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [blackstone_smd_control].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
