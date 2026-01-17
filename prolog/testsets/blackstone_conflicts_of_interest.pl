% ============================================================================
% CONSTRAINT STORY: blackstone_conflicts_of_interest
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: The Blackstone Group L.P. Form S-1 (March 22, 2007)
% ============================================================================

:- module(blackstone_conflicts_of_interest, []).

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
 * * constraint_id: blackstone_conflicts_of_interest
 * human_readable: Fiduciary Conflict Allocation
 * domain: corporate_governance / legal
 * temporal_scope: 2007 (IPO Restructuring)
 * spatial_scope: National (Delaware/US)
 * * SUMMARY:
 * A structural legal constraint where the interests of Blackstone's investment 
 * funds (and the Limited Partners therein) are explicitly prioritized over 
 * the interests of the public common unitholders. This 
 * creates a "fiduciary hierarchy" where the public investor is legally 
 * positioned at the bottom.
 * * KEY AGENTS:
 * - The General Partner: Controlled by Senior Managing Directors (SMDs).
 * - Limited Partners (LPs): Investors in Blackstone's funds (e.g., pension funds).
 * - Public Unitholders: New investors in the holding company (The Blackstone Group L.P.).
 * * NARRATIVE ARC:
 * Blackstone operates a business where it owes conflicting duties to different 
 * parties. The S-1 explicitly resolves these conflicts by stating 
 * that the General Partner's duty to the funds will override its duty to the 
 * public unitholders, effectively institutionalizing an "asymmetric loyalty" 
 *.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

narrative_ontology:interval(blackstone_conflict_era, 0, 10).
narrative_ontology:constraint_claim(blackstone_conflicts_of_interest, noose).

% Base extractiveness score (0.0 - 1.0)
% Rationale: High extraction of trust and legal recourse; public unitholders 
% lose the benefit of "undivided loyalty" standard in fiduciary law.
domain_priors:base_extractiveness(blackstone_conflicts_of_interest, 0.75).

% Suppression score (0.0 - 1.0)
% Rationale: The partnership agreement contractually "waives" or "limits" 
% traditional fiduciary duties, suppressing alternative legal remedies.
domain_priors:suppression_score(blackstone_conflicts_of_interest, 0.85).

% Enforcement requirements
domain_priors:requires_active_enforcement(blackstone_conflicts_of_interest).

% Metrics
narrative_ontology:constraint_metric(blackstone_conflicts_of_interest, extractiveness, 0.75).
narrative_ontology:constraint_metric(blackstone_conflicts_of_interest, suppression_requirement, 0.85).

% Beneficiaries and Victims
constraint_beneficiary(blackstone_conflicts_of_interest, fund_limited_partners).
constraint_beneficiary(blackstone_conflicts_of_interest, senior_managing_directors).
constraint_victim(blackstone_conflicts_of_interest, public_common_unitholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Public Unitholder - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless.
   WHEN: biographical.
   WHERE: trapped - The liability for conflicts is "locked in" by the S-1.
   
   WHY THIS CLASSIFICATION:
   For the public investor, this is a "Noose." They are knowingly entering a 
   contract where the other party is legally permitted to act against their 
   interests to satisfy a "higher" duty to fund investors.
   
   NARRATIVE EVIDENCE:
   "Our general partner... will be entitled to have regard to the interests 
   of... our investment funds... even if those interests are in conflict 
   with our interests or the interests of our common unitholders".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    blackstone_conflicts_of_interest,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        constraint_beneficiary(blackstone_conflicts_of_interest, fund_lps),
        constraint_victim(blackstone_conflicts_of_interest, public_investor),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(blackstone_conflicts_of_interest, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Fund Limited Partner (LP) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Powerful institutional investors (pensions, endowments).
   WHEN: historical.
   WHERE: mobile - Can move capital between different PE firms.
   
   WHY THIS CLASSIFICATION:
   For the LPs, this is a "Rope." It coordinates the firm's loyalty 
   towards the performance of the funds, ensuring that the "IPO-ing" holding 
   company doesn't divert resources or focus away from the LP's returns.
   
   NARRATIVE EVIDENCE:
   The structure ensures that "fiduciary duties... to our investment funds 
   will not be diminished by the fact that we are a public company".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    blackstone_conflicts_of_interest,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        constraint_beneficiary(blackstone_conflicts_of_interest, fund_performance),
        constraint_victim(blackstone_conflicts_of_interest, none),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(blackstone_conflicts_of_interest, E),
    E < 0.8,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(blackstone_conflict_tests).

test(fiduciary_priority_inversion) :-
    % Test that institutional LPs experience a Rope (protection) 
    % while Public Investors experience a Noose (extraction of duty)
    constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, rope, 
        context(institutional, _, _, _, _, _)),
    constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, noose, 
        context(individual_powerless, _, _, _, _, _)).

:- end_tests(blackstone_conflict_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.75): This reflects the "valuation discount" public 
 * investors must accept because they lack traditional legal protection.
 * 2. CLASSIFICATION RATIONALE: The S-1 describes a deliberate contractual 
 * "sacrifice" of the public shareholder's interests to maintain the 
 * viability of the fund-management business.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    gp_good_faith_definition,
    "What constitutes 'good faith' when the GP is explicitly allowed to prioritize one group over another?",
    resolution_mechanism("Delaware case law interpretation of the 'implied covenant of good faith' in LP agreements"),
    impact("If interpreted narrowly: it is a Mountain (absolute power). If broadly: a Rope (check on power)."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Independent Conflict Committees
 * Viability: Blackstone mentions them, but they are not mandatory for all 
 * decisions.
 * Suppression: The GP retains final discretion in many cases, suppressing 
 * the "independent oversight" alternative.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [blackstone_conflicts_of_interest].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
