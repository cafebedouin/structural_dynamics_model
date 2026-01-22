% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: lehman_repo_105
% ============================================================================
% Revised: 2026-01-20 (v3.1 Hardened Standard)
% Source: Valukas Report / Lehman Brothers Examiner's Report
% ============================================================================

:- module(lehman_repo_105, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_metric/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: lehman_repo_105
 * human_readable: Lehman Brothers Repo 105
 * domain: economic/legal
 * temporal_scope: 2007-2008 CE
 * spatial_scope: Global Financial Markets / US-UK
 * * SUMMARY:
 * Repo 105 was an accounting maneuver used to temporarily move billions in 
 * liabilities off-balance sheet to hide leverage. 
 * While legally classified as a "sale" (Mountain), it functioned as a 
 * predatory mandate to deceive investors (Noose).
 * * KEY AGENTS:
 * - Lehman_Executives: Institutional; managed optics using "Ropes".
 * - Accounting_Standards: Analytical; GAAP rules providing the "Mountain".
 * - Market_Investors: Powerless; exit options hidden by the "Noose".
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Structural Anchor for System Extraction
narrative_ontology:interval(lehman_repo_105, 2007, 2008).

% Base extractiveness: 0.9. 
% Rationale: High strategic extraction of investor trust via hidden leverage.
domain_priors:base_extractiveness(lehman_repo_105, 0.9).
domain_priors:suppression_score(lehman_repo_105, 0.85).
domain_priors:requires_active_enforcement(lehman_repo_105).

% Beneficiaries & Victims
narrative_ontology:constraint_beneficiary(lehman_repo_105, lehman_senior_management).
narrative_ontology:constraint_victim(lehman_repo_105, counterparty_investors).

% v3.1 Corrected Metrics
narrative_ontology:constraint_metric(lehman_repo_105, extractiveness, 0.9).
narrative_ontology:constraint_metric(lehman_repo_105, suppression_requirement, 0.85).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Accounting Standards (GAAP) - MOUNTAIN
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    lehman_repo_105, 
    mountain, 
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Lehman Executive - ROPE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    lehman_repo_105, 
    rope, 
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: Uninformed Investor - NOOSE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    lehman_repo_105, 
    noose, 
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(lehman_repo_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(lehman_repo_105, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(lehman_repo_105, T2, context(agent_power(individual_powerless), _, _, _)),
    T1 \= T2.

:- end_tests(lehman_repo_tests).

/* ==========================================================================
   5. OMEGA VARIABLES (Ω)
   ========================================================================== */

narrative_ontology:omega_variable(
    colorable_claim_judgment,
    "Will a trier of fact find the senior officers' conduct grossly negligent?",
    resolution_mechanism("Litigation outcome of breach of fiduciary duty claims"),
    impact("If Yes: The Noose is legally confirmed. If No: It remains a 'Business Judgment' Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. MODEL INTERPRETATION (Hardened Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Repo 105 is the ultimate "Noose" masquerading as a "Rope". 
 * I set extractiveness to 0.9 because the Examiner found balance sheet manipulation.
 */

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES: Standard Repo Accounting
 * Suppression: Suppressed by management's desire to hit leverage targets 
 * without selling assets at a loss.
 */

intent_viable_alternative(lehman_repo_105, standard_repo, 'Transparent leverage reporting').
intent_alternative_rejected(lehman_repo_105, standard_repo, 'Fear of credit rating downgrade').

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
