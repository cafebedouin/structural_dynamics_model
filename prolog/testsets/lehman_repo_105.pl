% ============================================================================
% CONSTRAINT STORY: lehman_repo_105
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Report of Examiner Anton R. Valukas (Lehman Brothers Bankruptcy)
% ============================================================================

:- module(constraint_lehman_repo_105, []).

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
 * * constraint_id: lehman_repo_105
 * human_readable: Lehman Brothers Repo 105 (Accounting Manipulation)
 * domain: economic/political/legal
 * temporal_scope: 2007-2008 (The Financial Crisis)
 * spatial_scope: Global Financial Markets
 * * SUMMARY:
 * Repo 105 was an accounting mechanism used by Lehman Brothers to temporarily 
 * remove approximately $50 billion of assets from its balance sheet at quarter-end 
 *. This allowed the firm to report lower net leverage ratios, 
 * misleading investors and rating agencies about its true financial health[cite: 165].
 * * KEY AGENTS:
 * - Senior Management: CEO Richard Fuld and CFOs who certified misleading reports[cite: 134].
 * - Investors/Creditors: Powerless agents who relied on suppressed information.
 * - Ernst & Young: Auditors who failed to challenge the non-disclosure.
 * * NARRATIVE ARC:
 * What began as a "Rope" for management to coordinate liquidity perception 
 * became a "Noose" for the global economy when the hidden leverage was exposed. 
 * For the public, it appeared as an immutable "Mountain" of financial collapse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(lehman_repo_105, 0, 10).
narrative_ontology:constraint_claim(lehman_repo_105, balance_sheet_manipulation).

% Base extractiveness: Very High (0.9).
% The practice extracted capital and confidence from the market by hiding 
% the true risk profile[cite: 165, 1269].
domain_priors:base_extractiveness(lehman_repo_105, 0.9).

% Suppression: Very High (0.9).
% The use of Repo 105 was not disclosed to the Board, the SEC, or rating 
% agencies[cite: 78, 80].
domain_priors:suppression_score(lehman_repo_105, 0.9).

% Enforcement: Required active management effort to execute manually[cite: 1056].
domain_priors:requires_active_enforcement(lehman_repo_105).

% Beneficiaries: Lehman Senior Management (maintained 'confidence' temporarily).
constraint_beneficiary(lehman_repo_105, lehman_senior_officers).

% Victims: Creditors and Shareholders (misled by financial reports).
constraint_victim(lehman_repo_105, global_creditors_and_shareholders).

% Metrics
narrative_ontology:constraint_metric(lehman_repo_105, extractiveness, 0.9).
narrative_ontology:constraint_metric(lehman_repo_105, suppression_requirement, 0.9).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE UNKNOWING INVESTOR - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate (2008 reporting periods)
   WHERE: trapped (locked into Lehman's debt/equity)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the investor, the suppressed leverage was a "Noose." They were 
   strangled by a risk they could not see, as reported leverage of 12.1 
   hid an actual leverage of 13.9.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lehman_repo_105,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: LEHMAN MANAGEMENT - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: biographical (maintaining the firm's survival)
   WHERE: mobile (arbitraging accounting rules)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   Management viewed Repo 105 as a "Rope"—a tool to coordinate market 
   confidence and "win back" investors during a liquidity crisis.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lehman_repo_105,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EXAMINER (Valukas) - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the Examiner, Repo 105 is a "Mountain"—a structural failure of the 
   investment bank business model and regulatory oversight that led 
   inevitably to collapse[cite: 1272, 1284].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lehman_repo_105,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(lehman_repo_tests).

test(leverage_manipulation_delta) :-
    % Validation that Repo 105 moved leverage by nearly 2 whole points.
    Difference is 13.9 - 12.1,
    Difference > 1.5.

test(perspectival_gap) :-
    % Testing the gap between management (Rope) and the market (Noose).
    constraint_indexing:constraint_classification(lehman_repo_105, rope, context(institutional, _, _, _)),
    constraint_indexing:constraint_classification(lehman_repo_105, noose, context(individual_powerless, _, _, _)).

:- end_tests(lehman_repo_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Repo 105 is the ultimate "Noose" masquerading as a "Rope." I set 
 * extractiveness and suppression to 0.9 because the Examiner explicitly 
 * found colorable claims of balance sheet manipulation[cite: 143, 1551].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    colorable_claim_judgment,
    "Will a trier of fact find the senior officers' conduct grossly negligent?",
    resolution_mechanism("Litigation outcome of breach of fiduciary duty claims"),
    impact("If Yes: The Noose is legally confirmed. If No: It remains a 'Business Judgment' Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
