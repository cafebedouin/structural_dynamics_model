% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping
% ============================================================================
% Generated: January 24, 2026
% Model: Gemini 2.0 Flash
% Source: Academic Peer Review Gatekeeping / Open Access Debate
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(academic_peer_review_gatekeeping, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: academic_peer_review_gatekeeping
 * human_readable: Academic Peer Review and Journal Gatekeeping
 * domain: economic/social/technological
 * temporal_scope: 1945-2026
 * spatial_scope: Global
 * * SUMMARY:
 * A system where researchers provide free labor to for-profit journals, 
 * who then sell that research back to institutions at a high markup.
 * * KEY AGENTS:
 * - Junior Professors: Must publish to survive (Tenure).
 * - Journal Publishers: Profit from free labor and high subscription fees.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding
narrative_ontology:interval(academic_peer_review_gatekeeping, 0, 10).

% CLAIM REPAIR: Hard-coded to 'snare' to match high extractiveness (0.75).
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping, snare).

% Base Properties
domain_priors:base_extractiveness(academic_peer_review_gatekeeping, 0.75).
domain_priors:suppression_score(academic_peer_review_gatekeeping, 0.55).
domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping).

% Mandatory Asymmetry Hooks
constraint_beneficiary(academic_peer_review_gatekeeping, journal_publishers).
constraint_victim(academic_peer_review_gatekeeping, junior_professors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* PERSPECTIVE 1: JUNIOR PROFESSOR - Mountain */
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, mountain, 
    context(agent_power(individual_powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(national))) :- !.

/* PERSPECTIVE 2: UNIVERSITY PUBLISHER - Rope */
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, rope, 
    context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global))) :- !.

/* PERSPECTIVE 3: ANALYTICAL OBSERVER - Tangled Rope */
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, tangled_rope, 
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping, E), E > 0.4,
    domain_priors:suppression_score(academic_peer_review_gatekeeping, S), S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (Verified Syntax)
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, T1, 
        context(agent_power(individual_powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(national))),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, T2, 
        context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global))),
    T1 \= T2.

test(power_extractiveness_scaling) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping, Score),
    Score > 0.5.

:- end_tests(academic_peer_review_gatekeeping_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

omega_variable(
    academic_peer_review_gatekeeping_extraction_intent,
    'Is the 0.75 extraction a functional necessity or purely predatory?',
    resolution_mechanism('Audit of publisher profit margins vs editorial reinvestment'),
    impact('If necessity: Mountain. If predatory: Snare.'),
    confidence_without_resolution(medium)
).

omega_variable(
    free_labor_paradox,
    'Will academics continue self-extraction for prestige as AI-driven alternatives emerge?',
    resolution_mechanism('Participation rates in open-access boards'),
    impact('If they stay: Snare hardens. If they leave: Snare collapses.'),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * ALTERNATIVE: Platinum Open Access. 
 * Viability is high, but suppression via 'hybrid' fees remains significant.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% TO USE: ?- [academic_peer_review_gatekeeping].
% RUN TESTS: ?- run_tests(academic_peer_review_gatekeeping_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
