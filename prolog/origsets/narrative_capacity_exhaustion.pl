% ============================================================================
% CONSTRAINT STORY: narrative_capacity_exhaustion
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(narrative_capacity_exhaustion, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: narrative_capacity_exhaustion
 * human_readable: The Storytelling Burnout
 * domain: social/technological/psychological
 * * SUMMARY:
 * A scenario where the "Rope" of digital content creation and storytelling 
 * becomes a competitive treadmill that requires more narrative output than 
 * the subject has lived experience to support. This coordination tool for 
 * social presence becomes a "Snare" as the subject's authentic agency is 
 * liquidated to satisfy the constant demand for "new content," trapping 
 * the user in a territory of performative exhaustion where "meaning" is 
 * sacrificed for "frequency."
 * * KEY AGENTS:
 * - Content Creator: Subject (Powerless)
 * - Engagement Algorithm: Beneficiary (Institutional)
 * - Cultural Saturation Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the total siphoning of the subject's 
% creative surplus to maintain the institutional "engagement" metric.
domain_priors:base_extractiveness(narrative_capacity_exhaustion, 0.88). 
domain_priors:suppression_score(narrative_capacity_exhaustion, 0.77). % Offline or "slow" creativity is suppressed by algorithmically-induced invisibility.
domain_priors:theater_ratio(narrative_capacity_exhaustion, 0.91).    % Extreme theater: "Creator Wellness" grants masking the structural extraction of burnout.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(narrative_capacity_exhaustion, extractiveness, 0.88).
narrative_ontology:constraint_metric(narrative_capacity_exhaustion, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(narrative_capacity_exhaustion, theater_ratio, 0.91).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The creator is trapped: they must keep telling stories to remain relevant, 
% but the pace liquidates their capacity for actual thought.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the exhaustion as a Rope—the essential coordination 
% substrate for maintaining a 24/7 global stream of "fresh" data.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.91) > 0.70 triggers Piton: the "Authenticity Guide" 
% is an inertial spike; it signals creative freedom while 0.88 extraction occurs.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.88) masking as functional scaling (Rope).
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(narrative_capacity_exhaustion, E), E >= 0.50,
    domain_priors:suppression_score(narrative_capacity_exhaustion, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(narrative_exhaustion_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless creator vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(narrative_capacity_exhaustion, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(narrative_capacity_exhaustion, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.91) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(narrative_capacity_exhaustion, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(narrative_capacity_exhaustion, E),

    E > 0.70.

:- end_tests(narrative_exhaustion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of a global creator economy is achieved by liquidating 
 * the subject's primary creative and psychological agency.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Content Creator feels a Snare because they have become a "content machine" 
 * with no lived reality to report. The Algorithm sees a Rope because the 
 * exhaustion coordinates a perfectly steady and predictable data feed.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Creator Support Tool" is no longer functional (Theater 0.91); 
 * it is an inert spike siphoning 0.88 of the species' collective surplus. 
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_synthetic_narrative_ceiling,
    'Can AI-generated stories restore the Rope, or is "meaning" a human biological "Snare" (Snare vs Mountain)?',
    'Tracking the engagement half-life of 100% synthetic vs 100% human-original content threads.',
    'If engagement holds: Snare of current technique. If it drops: Mountain of Human Meaning.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(narrative_capacity_exhaustion, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
