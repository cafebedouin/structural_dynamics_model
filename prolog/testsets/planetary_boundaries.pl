% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: planetary_boundaries
% ============================================================================
% Revised: 2026-01-20 (v3.1 Hardened Standard)
% Source: Stockholm Resilience Centre / Structural Dynamics Fleet
% ============================================================================

:- module(planetary_boundaries, []).

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
 * * constraint_id: planetary_boundaries
 * human_readable: Planetary Boundaries
 * domain: environmental/political
 * temporal_scope: 2009-Present
 * spatial_scope: Global
 * * SUMMARY:
 * Planetary Boundaries define the "safe operating space for humanity" across 
 * nine Earth system processes. While biophysical (Mountain), 
 * they function as a mandate for global coordination (Rope) that can cap 
 * economic margins for developing populations (Noose).
 * * KEY AGENTS:
 * - Biosphere: The physical substrate (Mountain).
 * - Global_Governance: Institutional; coordination via policy (Rope).
 * - Developing_Populations: Powerless; economic margin capped (Noose).
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================= */

% Structural Anchor for System Extraction
narrative_ontology:interval(planetary_boundaries, 2009, 2026).

% Base extractiveness: 0.8. 
% Rationale: High extraction of developmental potential to maintain biophysical stability.
domain_priors:base_extractiveness(planetary_boundaries, 0.8).
domain_priors:suppression_score(planetary_boundaries, 0.8).
domain_priors:requires_active_enforcement(planetary_boundaries).

% Beneficiaries & Victims
narrative_ontology:constraint_beneficiary(planetary_boundaries, global_governance_bodies).
narrative_ontology:constraint_victim(planetary_boundaries, developing_economic_subjects).

% v3.1 Corrected Metrics
narrative_ontology:constraint_metric(planetary_boundaries, extractiveness, 0.8).
narrative_ontology:constraint_metric(planetary_boundaries, suppression_requirement, 0.8).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Sub Specie Aeternitatis - MOUNTAIN
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    planetary_boundaries, 
    mountain, 
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Global Policy Makers - ROPE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    planetary_boundaries, 
    rope, 
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: Economic Subject (Developing Nation) - NOOSE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    planetary_boundaries, 
    noose, 
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(planetary_boundary_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(planetary_boundaries, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(planetary_boundaries, T2, context(agent_power(individual_powerless), _, _, _)),
    T1 \= T2.

:- end_tests(planetary_boundary_tests).

/* ==========================================================================
   5. OMEGA VARIABLES (Ω)
   ========================================================================== */

narrative_ontology:omega_variable(
    physics_vs_policy_tension,
    "To what degree is a boundary violation a physical 'Mountain' collapse versus a policy-driven 'Noose' tightening?",
    resolution_mechanism("Real-time monitoring of Earth system process thresholds vs. policy enforcement latency"),
    impact("If Mountain: Inevitable collapse. If Noose: Reformable developmental ceilings."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. MODEL INTERPRETATION (Hardened Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini-v3.1-Hardened
 * * KEY DECISIONS:
 * The "safe operating space" for the planet is often a "narrowed operating space" 
 * for the marginalized. This represents quintessential Mandatrophy where 
 * biophysical facts justify developmental extraction.
 */

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES: Technological Decoupling
 * Suppression: Currently suppressed by thermodynamic constraints and 
 * high capital/temporal requirements for radical innovation.
 */

intent_viable_alternative(planetary_boundaries, technological_innovation, 'Radical resource decoupling').
intent_alternative_rejected(planetary_boundaries, technological_innovation, 'Thermodynamic and temporal constraints').

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
