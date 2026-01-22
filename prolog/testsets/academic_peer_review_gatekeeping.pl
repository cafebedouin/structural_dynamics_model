% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping
% ============================================================================
% Revised: 2026-01-20 (v3.1 Hardened Standard)
% Source: Academic Publishing Industry Analysis
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
 * * constraint_id: academic_peer_review_gatekeeping
 * human_readable: Academic Peer Review Gatekeeping
 * domain: educational/economic
 * temporal_scope: 1945-Present
 * spatial_scope: Global Academic Institutions
 * * SUMMARY:
 * Peer review is the evaluative filter for scholarly publication. 
 * While intended for quality control, it functions as a primary mandate for 
 * "institutional truth" and career gatekeeping.
 * * KEY AGENTS:
 * - Junior_Professor: Powerless; career survival tied to unyielding metrics.
 * - Scientific_Society: Institutional; uses review for truth coordination.
 * - Journal_Publisher: Institutional; monetizes uncompensated elite labor.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Structural Anchor
narrative_ontology:interval(academic_peer_review_gatekeeping, 1945, 2026).

% Base extractiveness: 0.7. 
% Rationale: Relies on massive uncompensated labor to generate private publisher value.
domain_priors:base_extractiveness(academic_peer_review_gatekeeping, 0.7).
domain_priors:suppression_score(academic_peer_review_gatekeeping, 0.5).
domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping).

% Beneficiaries & Victims
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping, journal_publishers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping, junior_professors).

% v3.1 Corrected Metrics
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, extractiveness, 0.7).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, suppression_requirement, 0.5).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Junior Professor - MOUNTAIN
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    academic_peer_review_gatekeeping, 
    mountain, 
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Scientific Society - ROPE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    academic_peer_review_gatekeeping, 
    rope, 
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: Open-Science Activist - NOOSE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    academic_peer_review_gatekeeping, 
    noose, 
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, T1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, T2, context(agent_power(institutional), _, _, _)),
    T1 \= T2.

:- end_tests(academic_peer_review_tests).

/* ==========================================================================
   5. OMEGA VARIABLES (Ω)
   ========================================================================== */

narrative_ontology:omega_variable(
    free_labor_paradox,
    "Will elite academic labor continue to self-extract for prestige when AI-driven alternatives emerge?",
    resolution_mechanism("Monitor participation rates in uncompensated 'Top-Tier' editorial boards vs open-access community boards"),
    impact("If labor stays: Noose hardens into Mountain. If labor leaves: Noose collapses."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES: Post-Publication Peer Review (Pre-prints)
 * Suppression: Institutional committees often ignore pre-print citations in tenure files.
 */

intent_viable_alternative(academic_peer_review_gatekeeping, pre_prints, 'Community-led open review').
intent_alternative_rejected(academic_peer_review_gatekeeping, pre_prints, 'Institutional reliance on journal impact factor').

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

