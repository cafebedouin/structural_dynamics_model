% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: gale_shapley_matching
% ============================================================================
% Revised: 2026-01-20 (v3.1 Hardened Standard)
% ============================================================================

:- module(constraint_gale_shapley_matching, []).

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
 * SUMMARY:
 * The Gale-Shapley algorithm guarantees a stable matching where no two agents 
 * have a mutual incentive to defect. In medical residency matches, this 
 * "Stability" is a non-negotiable fact of the labor market (Mountain).
 * * KEY AGENTS:
 * - The Algorithm: Mathematical guarantor of stability (Mountain).
 * - Hospital Systems: Institutions using stability to ensure labor supply (Rope).
 * - Medical Couples: Subjects whose geographic autonomy is extracted (Noose).
 */

domain_priors:base_extractiveness(gale_shapley, 0.8).
domain_priors:suppression_score(gale_shapley, 0.8).
domain_priors:requires_active_enforcement(gale_shapley).

/* ==========================================================================
   2. INDEXED CLASSIFICATIONS (The Three-Legged Stool)
   ========================================================================== */

% PERSPECTIVE: Analytical (Mathematical Logic)
% The existence of a stable matching is a fixed property of the system.
constraint_indexing:constraint_classification(gale_shapley, mountain, agent_power(analytical)).

% PERSPECTIVE: Institutional (NRMP / Hospital Administrators)
% Stability is the "Rope" that prevents "unraveling" and chaotic poaching.
constraint_indexing:constraint_classification(gale_shapley, rope, agent_power(institutional)).

% PERSPECTIVE: Individual (Residency Applicant / Couples)
% The algorithm is a Noose; the "Exit Option" (negotiating location) is removed.
constraint_indexing:constraint_classification(gale_shapley, noose, agent_power(individual_powerless)).

/* ==========================================================================
   3. MEASUREMENT LAYER (v3.1 Coercion Metrics)
   ========================================================================== */

% High stakes for residents (career trajectory) and high suppression of exit options.
narrative_ontology:measurement(gale_shapley, applicant, stakes_inflation(individual), 10, 0.9).
narrative_ontology:measurement(gale_shapley, nrmp, suppression(individual), 10, 0.8).

/* ==========================================================================
   4. MODEL INTERPRETATION (Hardened Commentary)
   ========================================================================== */

/**
 * MODEL INTERPRETATION:
 * This domain exhibits "Mathematical Mandatrophy." The stability of the match 
 * is used to justify the removal of individual bargaining power. 
 * * The indexical shift is binary:
 * 1. For the Proposer (Institutions), it is a Rope (Optimal Outcome).
 * 2. For the Receiver (Applicants), it is a Noose (Worst of all stable outcomes).
 * * By promoting the "Noose" from a comment to a classification fact, the model 
 * can now measure the human cost of "Stability."
 */

/* ==========================================================================
   5. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * ALTERNATIVE: Decentralized Negotiation
 * Viability: Historically existed before the match; allowed for individual agency.
 * Suppression: Rejected due to "Market Unraveling" (Institutional risk).
 */

intent_viable_alternative(gale_shapley, decentralized_negotiation, 'Direct individual bargaining').
intent_alternative_rejected(gale_shapley, decentralized_negotiation, 'Institutional fear of labor poaching and market instability').

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
