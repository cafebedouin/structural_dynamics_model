% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: china_critical_mineral_chokepoint
% ============================================================================
% Revised: 2026-01-20 (v3.1 Hardened Standard)
% Source: Global Supply Chain Analysis / Geopolitical Strategy
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
 * * constraint_id: china_critical_mineral_chokepoint
 * human_readable: China Critical Mineral Chokepoint
 * domain: economic/technological/political
 * temporal_scope: 2010-Present
 * spatial_scope: Global
 * * SUMMARY:
 * China's dominance over the processing and supply of critical minerals creates 
 * a global strategic chokepoint. 
 * While based on geology (Mountain), it functions as an instrument of 
 * diplomatic leverage (Rope/Noose).
 * * KEY AGENTS:
 * - Tech_Defense_Industry: Powerless; dependent on supply chain (Mountain).
 * - Chinese_State_Planning: Institutional; uses dominance for coordination (Rope).
 * - Downstream_Manufacturers: Individual; margins extracted by quotas (Noose).
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Structural Anchor for System Extraction
narrative_ontology:interval(china_critical_mineral_chokepoint, 2010, 2026).

% Base extractiveness: 0.85. 
% Rationale: High strategic extraction via value-chain dominance.
domain_priors:base_extractiveness(china_critical_mineral_chokepoint, 0.85).
domain_priors:suppression_score(china_critical_mineral_chokepoint, 0.70).
domain_priors:requires_active_enforcement(china_critical_mineral_chokepoint).

% Beneficiaries & Victims
narrative_ontology:constraint_beneficiary(china_critical_mineral_chokepoint, state_industrial_planners).
narrative_ontology:constraint_victim(china_critical_mineral_chokepoint, downstream_tech_manufacturers).

% v3.1 Corrected Metrics
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, extractiveness, 0.85).
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, suppression_requirement, 0.70).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Geological/Infrastructure Analyst - MOUNTAIN
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    china_critical_mineral_chokepoint, 
    mountain, 
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Diplomatic/State Actor - ROPE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    china_critical_mineral_chokepoint, 
    rope, 
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: Downstream Firm - NOOSE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    china_critical_mineral_chokepoint, 
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

:- begin_tests(critical_mineral_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, T2, context(agent_power(individual_powerless), _, _, _)),
    T1 \= T2.

:- end_tests(critical_mineral_tests).

/* ==========================================================================
   5. OMEGA VARIABLES (Ω)
   ========================================================================== */

narrative_ontology:omega_variable(
    artificial_scarcity_threshold,
    "Is the chokepoint a result of physical depletion (Mountain) or managed market access (Rope/Noose)?",
    resolution_mechanism("Audit of export quota adjustments vs. global geological reserve reports"),
    impact("If Mountain: Requires deep-sea/extra-planetary mining. If Noose: Requires trade reform."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES: Supply Chain Diversification (Friend-shoring)
 * Suppression: Suppressed by Chinese economies of scale and environmental regulatory advantages.
 */

intent_viable_alternative(china_critical_mineral_chokepoint, diversification, 'Development of domestic processing capacity').
intent_alternative_rejected(china_critical_mineral_chokepoint, diversification, 'Prohibitive capital costs and environmental lead times').

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
