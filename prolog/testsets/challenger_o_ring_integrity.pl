% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: challenger_o_ring_integrity
% ============================================================================
% Revised: 2026-01-20 (v3.1 Hardened Standard)
% ============================================================================

:- module(constraint_challenger_integrity, []).

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
 * The Challenger disaster was caused by the failure of O-ring seals in the 
 * Solid Rocket Booster (SRB) due to record-low temperatures at launch. 
 * While a physical limit (Mountain), the disaster was a result of 
 * "normalization of deviance" within NASA management.
 * * KEY AGENTS:
 * - The SRB O-Rings: Physical components governed by thermodynamics (Mountain).
 * - NASA Management: Institutional; utilized "Launch Success" as a "Rope" 
 * to coordinate political and funding support.
 * - The STS-51-L Crew: Individual powerless; trapped by the institutional 
 * decision to override engineering warnings (Noose).
 */

% Base extractiveness: 0.8. Rationale: High extraction of safety margin 
% to fulfill institutional schedule mandates.
domain_priors:base_extractiveness(challenger_o_ring_integrity, 0.8).
domain_priors:suppression_score(challenger_o_ring_integrity, 0.7).
domain_priors:requires_active_enforcement(challenger_o_ring_integrity).

/* ==========================================================================
   2. INDEXED CLASSIFICATIONS (The Three-Legged Stool)
   ========================================================================== */

% PERSPECTIVE: Analytical (Engineering/Physics)
% The temperature-dependent elasticity of the polymer is an unyielding Mountain.
constraint_indexing:constraint_classification(challenger_o_ring_integrity, mountain, agent_power(analytical)).

% PERSPECTIVE: Institutional (NASA Program Management)
% The flight schedule provided the "Rope" for inter-agency coordination.
constraint_indexing:constraint_classification(challenger_o_ring_integrity, rope, agent_power(institutional)).

% PERSPECTIVE: Individual (The Crew)
% For the astronauts, the lack of an abort option during SRB burn was a Noose.
constraint_indexing:constraint_classification(challenger_o_ring_integrity, noose, agent_power(individual_powerless)).

/* ==========================================================================
   3. MEASUREMENT LAYER (v3.1 Coercion Metrics)
   ========================================================================== */

% Extreme stakes for the crew (terminal) and suppression of the engineer's dissent.
narrative_ontology:measurement(challenger_o_ring_integrity, crew, stakes_inflation(individual), 10, 1.0).
narrative_ontology:measurement(challenger_o_ring_integrity, management, suppression(dissent), 10, 0.9).

/* ==========================================================================
   4. MODEL INTERPRETATION (Hardened Commentary)
   ========================================================================== */

/**
 * MODEL INTERPRETATION:
 * This domain captures "Normalised Mandatrophy." A physical Mountain (O-ring 
 * physics) was ignored by an Institutional Rope (the schedule) until the 
 * gap between reality and management belief collapsed into a Noose.
 * * The Omega resolved here is "Bureaucratic Physics": The belief that 
 * administrative mandates (Ropes) can override physical constraints (Mountains).
 */

/* ==========================================================================
   5. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * ALTERNATIVE: Launch Postponement
 * Viability: High (Engineering recommendation).
 * Suppression: Suppressed by "Schedule Pressure" and the need for a 
 * "Teacher in Space" public relations win.
 */

intent_viable_alternative(challenger_o_ring_integrity, postponement, 'Delay launch until ambient temperature rises').
intent_alternative_rejected(challenger_o_ring_integrity, postponement, 'Political pressure and fear of program funding erosion').

/* ==========================================================================
   6. TESTSET INTEGRATION
   ========================================================================== */
% (Self-contained test for the three-legged stool)
:- begin_tests(challenger_tests).
test(stool_completeness) :-
    constraint_indexing:constraint_classification(challenger_o_ring_integrity, mountain, _),
    constraint_indexing:constraint_classification(challenger_o_ring_integrity, rope, _),
    constraint_indexing:constraint_classification(challenger_o_ring_integrity, noose, _).
:- end_tests(challenger_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
