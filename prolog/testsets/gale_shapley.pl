% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: gale_shapley_matching
% ============================================================================
% Revised: 2026-01-20 (v3.1 Hardened Standard)
% Source: Stable Matching and the NRMP Analysis
% ============================================================================

:- module(constraint_gale_shapley_matching, []).

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
 * * constraint_id: gale_shapley_matching
 * human_readable: Gale-Shapley Stable Matching
 * domain: economic/mathematical
 * temporal_scope: 1952-Present (NRMP Context)
 * spatial_scope: Global Labor Markets
 * * SUMMARY:
 * The algorithm guarantees a stable matching where no two agents have a mutual 
 * incentive to defect. In medical residency, 
 * this "Stability" is a non-negotiable fact (Mountain) that removes 
 * individual bargaining power (Noose).
 * * KEY AGENTS:
 * - The_Algorithm: Mathematical guarantor (Mountain).
 * - Hospital_Systems: Institutional; use stability to ensure labor (Rope).
 * - Medical_Applicants: Powerless; geographic autonomy extracted (Noose).
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Structural Anchor for System Extraction
narrative_ontology:interval(gale_shapley_matching, 1952, 2026).

% Base extractiveness: 0.8. 
% Rationale: High extraction of individual agency to maintain market stability.
domain_priors:base_extractiveness(gale_shapley_matching, 0.8).
domain_priors:suppression_score(gale_shapley_matching, 0.8).
domain_priors:requires_active_enforcement(gale_shapley_matching).

% Beneficiaries & Victims
narrative_ontology:constraint_beneficiary(gale_shapley_matching, hospital_administrators).
narrative_ontology:constraint_victim(gale_shapley_matching, residency_applicants).

% v3.1 Corrected Metrics
narrative_ontology:constraint_metric(gale_shapley_matching, extractiveness, 0.8).
narrative_ontology:constraint_metric(gale_shapley_matching, suppression_requirement, 0.8).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Mathematical Logic - MOUNTAIN
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    gale_shapley_matching, 
    mountain, 
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Hospital Systems - ROPE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    gale_shapley_matching, 
    rope, 
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: Residency Applicant - NOOSE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    gale_shapley_matching, 
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

:- begin_tests(matching_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(gale_shapley_matching, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(gale_shapley_matching, T2, context(agent_power(individual_powerless), _, _, _)),
    T1 \= T2.

:- end_tests(matching_tests).

/* ==========================================================================
   5. OMEGA VARIABLES (Ω)
   ========================================================================== */

narrative_ontology:omega_variable(
    stability_premium,
    "Does the 'Stability' of the match benefit the Receiver (Applicant) enough to offset the loss of 'Agency' (Direct Negotiation)?",
    resolution_mechanism("Comparison of career outcomes in matched vs. unmatched/alternative entry labor markets"),
    impact("If agency > stability: The algorithm is a Noose. If stability > agency: It is a Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES: Decentralized Negotiation
 * Suppression: Suppressed by institutional fear of "Market Unraveling" and chaotic labor poaching.
 */

intent_viable_alternative(gale_shapley_matching, decentralized_negotiation, 'Direct individual bargaining').
intent_alternative_rejected(gale_shapley_matching, decentralized_negotiation, 'Institutional fear of labor poaching').

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
