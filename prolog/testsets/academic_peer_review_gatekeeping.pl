% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping
% ============================================================================
% Revised: 2026-01-20 (v3.1 Hardened Standard)
% ============================================================================

:- module(constraint_peer_review, []).

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
 * SUMMARY:
 * Peer review is the process by which scholarly work is evaluated by experts 
 * before publication. While intended as a quality filter, it functions as 
 * a primary mandate for "institutional truth." 
 * * KEY AGENTS:
 * - The Junior Professor: Individual powerless; professional survival is bound 
 * to unchangeable review metrics (Mountain).
 * - The Scientific Society: Institutional; uses review as a coordination 
 * mechanism to filter noise and maintain standards (Rope).
 * - The Journal Publisher: Institutional; monetizes uncompensated elite labor, 
 * transforming the coordination rope into an extractive loop (Noose).
 */

% Base extractiveness: 0.7. Rationale: Relies on massive uncompensated labor 
% to generate private publisher value.
domain_priors:base_extractiveness(academic_peer_review_gatekeeping, 0.7).
domain_priors:suppression_score(academic_peer_review_gatekeeping, 0.5).
domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping).

/* ==========================================================================
   2. INDEXED CLASSIFICATIONS (The Three-Legged Stool)
   ========================================================================== */

% PERSPECTIVE: Analytical (The Junior Professor)
% For those whose career depends on it, the process is an unyielding Mountain.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, mountain, agent_power(individual_powerless)).

% PERSPECTIVE: Institutional (The Scientific Society)
% Peer review is a Rope—a vital coordination mechanism for verifiable truth.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, rope, agent_power(institutional)).

% PERSPECTIVE: Individual (The Open-Science Activist)
% The system is a Noose; it chokes off access via paywalls and extracts labor.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, noose, agent_power(analytical)).

/* ==========================================================================
   3. MEASUREMENT LAYER (v3.1 Coercion Metrics)
   ========================================================================== */

% High stakes for faculty survival and high labor extraction by publishers.
narrative_ontology:measurement(academic_peer_review_gatekeeping, faculty, stakes_inflation(individual), 10, 0.9).
narrative_ontology:measurement(academic_peer_review_gatekeeping, publisher, suppression(innovation), 10, 0.7).

/* ==========================================================================
   4. MODEL INTERPRETATION (Hardened Commentary)
   ========================================================================== */

/**
 * MODEL INTERPRETATION:
 * This domain illustrates "Prestige Mandatrophy." The "Fact" of quality control 
 * (Mountain) is used to stabilize the industry (Rope) while masking the 
 * underlying extraction of academic margin (Noose).
 * * The Omega here is the "Free Labor Paradox": The most elite laborers 
 * work for free to produce a product they must then pay to access.
 */

/* ==========================================================================
   5. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * ALTERNATIVE: Post-Publication Peer Review (Pre-prints)
 * Viability: High (arXiv/bioRxiv). Immediate dissemination with community correction.
 * Suppression: Rejected by institutional committees relying on "Journal Prestige".
 */

intent_viable_alternative(academic_peer_review_gatekeeping, pre_prints, 'Community-led open review').
intent_alternative_rejected(academic_peer_review_gatekeeping, pre_prints, 'Institutional reliance on impact factor metrics').

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
