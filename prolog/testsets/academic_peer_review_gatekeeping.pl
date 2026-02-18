% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
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
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: academic_peer_review_gatekeeping
 * human_readable: Academic Peer Review and Journal Gatekeeping
 * domain: economic/social/technological
 * * SUMMARY:
 * A system where researchers provide free labor (peer review and editing) to 
 * for-profit publishers, who then sell access to that research back to 
 * the researchers' own institutions at significant markups.
 * * KEY AGENTS:
 * - Junior Professors: Subject (Powerless) - Must publish to secure tenure.
 * - Journal Publishers: Beneficiary (Institutional) - Capture value from free labor.
 * - Library Consortia: Auditor (Analytical) - Evaluate cost-benefit of subscriptions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(academic_peer_review_gatekeeping, 0.75). 
domain_priors:suppression_score(academic_peer_review_gatekeeping, 0.55).   
domain_priors:theater_ratio(academic_peer_review_gatekeeping, 0.45).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, extractiveness, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, theater_ratio, 0.45).

% Constraint classification claim
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping, "economic/social/technological").

% Constraint metric facts
domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping).

% Beneficiaries and Victims (Required for high-extraction constraints)
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping, journal_publishers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping, junior_professors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE JUNIOR PROFESSOR (SNARE/MOUNTAIN)
% Viewed as an immutable limit of the "publish or perish" landscape.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE PUBLISHER (ROPE)
% Viewed as essential infrastructure for global scientific coordination.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects both the coordination value (peer review quality) and the high extraction.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_tests).

test(perspectival_gap) :-
    % Verify gap between powerless (Mountain) and institutional (Rope).
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(extraction_threshold) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping, E),
    E >= 0.46. % Confirms Snare/Tangled/Mountain territory.

:- end_tests(academic_peer_review_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction is rated high (0.75) because the publisher captures nearly 
 * all financial surplus of the labor provided. The suppression (0.55) reflects
 * the "Prestige Lock-in"—it is difficult for individual actors to exit to 
 * open-access alternatives without sacrificing career advancement.
 *
 * MANDATROPHY ANALYSIS:
 * Tangled Rope classification is applied by the Analytical Observer because 
 * despite high extraction, the journals still provide a genuine coordination 
 * function (standardized peer review and archiving) that has not yet been 
 * fully decoupled from the extractive business model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_academic_extraction_intent,
    'Is the 0.75 extraction a functional necessity or purely predatory?',
    'Audit of publisher profit margins vs editorial reinvestment',
    'If necessity: Mountain. If predatory: Snare.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_academic_prestige_decay,
    'Will citation metrics shift to platform-agnostic models?',
    'Tracking institutional tenure requirements for 2026-2030',
    'If shift occurs: Snare collapses to Scaffold/Rope. If not: Snare hardens.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (Goodhart drift from function to prestige-theater)
narrative_ontology:measurement(academic_tr_t0, academic_peer_review_gatekeeping, theater_ratio, 0, 0.20).
narrative_ontology:measurement(academic_tr_t5, academic_peer_review_gatekeeping, theater_ratio, 5, 0.35).
narrative_ontology:measurement(academic_tr_t10, academic_peer_review_gatekeeping, theater_ratio, 10, 0.45).

% Extraction over time (Accumulation of rent-seeking via hybrid APC fees)
narrative_ontology:measurement(academic_ex_t0, academic_peer_review_gatekeeping, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(academic_ex_t5, academic_peer_review_gatekeeping, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(academic_ex_t10, academic_peer_review_gatekeeping, base_extractiveness, 10, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
