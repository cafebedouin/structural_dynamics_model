% ============================================================================
% CONSTRAINT STORY: planetary_boundaries
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini-v3.1-Hardened
% Source: Stockholm Resilience Centre / Structural Dynamics Fleet
% ============================================================================

:- module(planetary_boundaries, []).

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
 * Planetary Boundaries define the "safe operating space for humanity" across 
 * nine Earth system processes (e.g., climate change, biodiversity loss). 
 * While biophysical in nature, they function as a primary mandate for 
 * global institutional coordination.
 * * KEY AGENTS:
 * - The Biosphere: The physical substrate (Mountain).
 * - Global Governance: Institutions using the boundaries to coordinate policy (Rope).
 * - Developing Populations: Subjects whose economic margin is capped by the boundary (Noose).
 */

% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
domain_priors:base_extractiveness(planetary_boundaries, 0.8).
domain_priors:suppression_score(planetary_boundaries, 0.8).
domain_priors:requires_active_enforcement(planetary_boundaries).

/* ==========================================================================
   2. INDEXED CLASSIFICATIONS (The Three-Legged Stool)
   ========================================================================== */

% PERSPECTIVE: Analytical (Sub specie aeternitatis)
% The boundaries are non-negotiable biophysical limits.
constraint_indexing:constraint_classification(planetary_boundaries, mountain, agent_power(analytical)).

% PERSPECTIVE: Institutional (Global Policy Makers)
% The boundaries provide the "Rope" for international treaties and carbon markets.
constraint_indexing:constraint_classification(planetary_boundaries, rope, agent_power(institutional)).

% PERSPECTIVE: Individual (Economic Subject in Developing Nation)
% The "Fact" of the boundary is used to enforce a development ceiling.
constraint_indexing:constraint_classification(planetary_boundaries, noose, agent_power(individual_powerless)).

/* ==========================================================================
   3. MEASUREMENT LAYER (v3.1 Coercion Metrics)
   ========================================================================== */

narrative_ontology:measurement(planetary_boundaries, biosphere, stakes_inflation(individual), 10, 0.9).
narrative_ontology:measurement(planetary_boundaries, governance, suppression(individual), 10, 0.8).

/* ==========================================================================
   4. GENERATING MODEL COMMENTARY (Section 5 of Template)
   ========================================================================== */

/**
 * MODEL INTERPRETATION:
 * This domain is the quintessential example of Mandatrophy. The user is right: 
 * the "Noose" dimension is not the first thought, but it is the critical one. 
 * By defining the Noose perspective, we reveal that the "safe operating space" 
 * for the planet is often a "narrowed operating space" for the marginalized. 
 * * The Omega resolved here is the conflict between "Physics" (Mountain) and 
 * "Policy" (Rope/Noose). The model now understands that the Mountain's weight 
 * is the source of the Noose's tension.
 */

/* ==========================================================================
   5. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * ALTERNATIVE: Technological Decoupling
 * If radical innovation allows for economic growth without crossing biophysical 
 * limits, the "Noose" perspective would transition back to a "Rope."
 * Currently, the data shows high suppression of this alternative.
 */

intent_viable_alternative(planetary_boundaries, technological_innovation, 'Radical resource decoupling').
intent_alternative_rejected(planetary_boundaries, technological_innovation, 'Thermodynamic and temporal constraints').

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
