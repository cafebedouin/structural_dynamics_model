% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: china_critical_mineral_chokepoint
% ============================================================================
% Revised: 2026-01-20 (v3.1 Hardened Standard)
% ============================================================================

:- module(constraint_critical_minerals, []).

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
 * China's dominance over the processing and supply of critical minerals 
 * (Rare Earths, Lithium, Cobalt) creates a global strategic chokepoint. 
 * While based on geology (Mountain), it is utilized as an instrument of 
 * diplomatic and economic leverage.
 * * KEY AGENTS:
 * - Global Tech/Defense Industry: Individual powerless; dependent on 
 * unchangeable supply chain realities (Mountain).
 * - Chinese State Planning: Institutional; uses market dominance as a 
 * "Rope" to coordinate international policy and industrial strategy.
 * - Emerging Market Competitors: Individual; margin and development are 
 * extracted or suppressed by predatory pricing and export quotas (Noose).
 */

% Base extractiveness: 0.85. Rationale: High strategic extraction via 
% value-chain dominance and export controls.
domain_priors:base_extractiveness(china_critical_mineral_chokepoint, 0.85).
domain_priors:suppression_score(china_critical_mineral_chokepoint, 0.70).
domain_priors:requires_active_enforcement(china_critical_mineral_chokepoint).

/* ==========================================================================
   2. INDEXED CLASSIFICATIONS (The Three-Legged Stool)
   ========================================================================== */

% PERSPECTIVE: Analytical (Geological/Infrastructure Reality)
% The concentrated processing infrastructure is an unyielding Mountain.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, mountain, agent_power(analytical)).

% PERSPECTIVE: Institutional (Diplomatic/State Level)
% Dominance provides a "Rope"—a tool for coordinating trade war responses.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, rope, agent_power(institutional)).

% PERSPECTIVE: Individual (Downstream Manufacturers)
% For the single firm, the chokepoint is a Noose; exit options are 
% non-existent in the short-to-medium term.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, noose, agent_power(individual_powerless)).

/* ==========================================================================
   3. MEASUREMENT LAYER (v3.1 Coercion Metrics)
   ========================================================================== */

% High strategic stakes for global transition and state-level suppression of alternatives.
narrative_ontology:measurement(china_critical_mineral_chokepoint, downstream_firm, stakes_inflation(individual), 10, 0.9).
narrative_ontology:measurement(china_critical_mineral_chokepoint, state_actor, suppression(competition), 10, 0.8).

/* ==========================================================================
   4. MODEL INTERPRETATION (Hardened Commentary)
   ========================================================================== */

/**
 * MODEL INTERPRETATION:
 * This domain represents "Resource Mandatrophy." The "Fact" of geological 
 * distribution (Mountain) is leveraged to create a "Geopolitical Rope" that 
 * tightens into a "Supply Noose" for dependent agents.
 * * The Omega resolved here is "Artificial Scarcity": The physical presence 
 * of the mineral is a Mountain, but the *access* to it is a managed Rope.
 */

/* ==========================================================================
   5. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * ALTERNATIVE: Supply Chain Diversification (Friend-shoring)
 * Viability: Medium. Requires decades of infrastructure investment.
 * Suppression: High. Suppressed by the "Mountain" of existing Chinese 
 * economies of scale and environmental regulatory advantages.
 */

intent_viable_alternative(china_critical_mineral_chokepoint, diversification, 'Development of domestic processing capacity').
intent_alternative_rejected(china_critical_mineral_chokepoint, diversification, 'Prohibitive capital costs and environmental lead times').

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
