% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: challenger_o_ring_integrity
% ============================================================================
% Revised: 2026-01-20 (v3.1 Hardened Standard)
% Source: Rogers Commission Report / Engineering Analysis
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
 * * constraint_id: challenger_o_ring_integrity
 * human_readable: Challenger SRB O-Ring Integrity
 * domain: technological/institutional
 * temporal_scope: 1986-01-28
 * spatial_scope: Kennedy Space Center, FL
 * * SUMMARY:
 * The disaster was caused by the failure of O-ring seals due to record-low 
 * temperatures. While a physical limit (Mountain), 
 * it became a "Noose" due to the "normalization of deviance" in NASA 
 * management.
 * * KEY AGENTS:
 * - SRB_O_Rings: Thermodynamics (Mountain).
 * - NASA_Management: Institutional; schedule coordination (Rope).
 * - STS_51_L_Crew: Individual powerless; trapped by override (Noose).
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Structural Anchor
narrative_ontology:interval(challenger_o_ring_integrity, 1986, 1986).

% Base extractiveness: 0.8. 
% Rationale: Extreme extraction of safety margin to fulfill institutional mandates.
domain_priors:base_extractiveness(challenger_o_ring_integrity, 0.8).
domain_priors:suppression_score(challenger_o_ring_integrity, 0.7).
domain_priors:requires_active_enforcement(challenger_o_ring_integrity).

% Beneficiaries & Victims
narrative_ontology:constraint_beneficiary(challenger_o_ring_integrity, institutional_schedule).
narrative_ontology:constraint_victim(challenger_o_ring_integrity, sts_51_l_crew).

% v3.1 Corrected Metrics
narrative_ontology:constraint_metric(challenger_o_ring_integrity, extractiveness, 0.8).
narrative_ontology:constraint_metric(challenger_o_ring_integrity, suppression_requirement, 0.7).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Engineering/Physics - MOUNTAIN
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    challenger_o_ring_integrity, 
    mountain, 
    context(
        agent_power(analytical),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: NASA Management - ROPE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    challenger_o_ring_integrity, 
    rope, 
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: STS-51-L Crew - NOOSE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    challenger_o_ring_integrity, 
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

:- begin_tests(challenger_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(challenger_o_ring_integrity, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(challenger_o_ring_integrity, T2, context(agent_power(individual_powerless), _, _, _)),
    T1 \= T2.

:- end_tests(challenger_tests).

/* ==========================================================================
   5. OMEGA VARIABLES (Ω)
   ========================================================================== */

narrative_ontology:omega_variable(
    normalization_of_deviance_threshold,
    "At what point does an incremental 'Rope' adjustment become a catastrophic 'Noose'?",
    resolution_mechanism("Post-hoc analysis of safety waiver trends vs. hardware failure rates"),
    impact("If identified: Predicts system collapse. If unknown: Ensures surprise catastrophe."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES: Launch Postponement
 * Suppression: Suppressed by political schedule pressure and the 'Teacher in Space' PR goal.
 */

intent_viable_alternative(challenger_o_ring_integrity, postponement, 'Delay launch for temperature rise').
intent_alternative_rejected(challenger_o_ring_integrity, postponement, 'Schedule pressure and funding fears').

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
