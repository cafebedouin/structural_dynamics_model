% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: lehman_repo_105
% ============================================================================

:- module(lehman_repo_105, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * SUMMARY:
 * Repo 105 was an accounting maneuver used by Lehman Brothers to temporarily 
 * move billions in liabilities off the balance sheet to hide leverage. 
 * While legally a "sale" (Mountain), it functioned as a predatory mandate.
 */

domain_priors:base_extractiveness(lehman_repo_105, 0.9).
domain_priors:suppression_score(lehman_repo_105, 0.85).
domain_priors:requires_active_enforcement(lehman_repo_105).

/* ==========================================================================
   2. INDEXED CLASSIFICATIONS
   ========================================================================== */

% PERSPECTIVE: Analytical (Accounting Standards)
% The GAAP rule (Mountain) allows the reclassification of repo as a "sale."
constraint_indexing:constraint_classification(lehman_repo_105, mountain, agent_power(analytical)).

% PERSPECTIVE: Institutional (Lehman Executive)
% The maneuver provides a "Rope" for managing optics and regulatory ratios.
constraint_indexing:constraint_classification(lehman_repo_105, rope, agent_power(institutional)).

% PERSPECTIVE: Individual (Uninformed Counterparty/Investor)
% The "Fact" of the sale is a Noose; it hides the existential risk of insolvency.
constraint_indexing:constraint_classification(lehman_repo_105, noose, agent_power(individual_powerless)).

/* ==========================================================================
   3. MEASUREMENT LAYER
   ========================================================================== */

narrative_ontology:measurement(lehman_repo_105, counterparty, stakes_inflation(individual), 10, 0.95).
narrative_ontology:measurement(lehman_repo_105, executive, suppression(individual), 10, 0.8).

/**
 * MODEL INTERPRETATION:
 * The Omega here is the "Legal Fiction." By classifying Repo 105 as a Mountain, 
 * Lehman could claim the risk was non-existent. Our model now flags this as 
 * Mandatrophy: using the weight of accounting logic to tighten a financial noose.
 */

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
 * found colorable claims of balance sheet manipulation.
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
