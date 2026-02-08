% ============================================================================
% CONSTRAINT STORY: cultural_memory_decay
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(cultural_memory_decay, []).

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
 * * constraint_id: cultural_memory_decay
 * human_readable: The Digital Dark Age Siphon
 * domain: social/technological/historical
 * * SUMMARY:
 * A scenario where the shift from durable physical records to ephemeral, 
 * proprietary digital formats leads to the systematic loss of historical 
 * context. This functions as a Snare for future generations who lose access 
 * to their own heritage, while serving as a Rope for platforms that monetize 
 * "the present moment" and control access to the fragmented past.
 * * KEY AGENTS:
 * - Heritage Seeker: Subject (Powerless)
 * - Cloud Archival Platform: Beneficiary (Institutional)
 * - Digital Archeologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.81) because the decay liquidates the species' 
% historical capital to fuel the short-term subscriptions of digital landlords.
domain_priors:base_extractiveness(cultural_memory_decay, 0.81). 
domain_priors:suppression_score(cultural_memory_decay, 0.65). 
domain_priors:theater_ratio(cultural_memory_decay, 0.72). % Piton threshold (> 0.70) triggered by "Permanent Storage" marketing.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(cultural_memory_decay, extractiveness, 0.81).
narrative_ontology:constraint_metric(cultural_memory_decay, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cultural_memory_decay, theater_ratio, 0.72).

% This is an entropic property of modern information systems.
% narrative_ontology:has_sunset_clause(cultural_memory_decay). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: their personal and collective history is locked 
% in bit-rotting formats or behind paywalls they cannot perpetually sustain.
constraint_indexing:constraint_classification(cultural_memory_decay, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the centralized digital transition as a Rope—the only 
% way to coordinate global information access and searchability at scale.
constraint_indexing:constraint_classification(cultural_memory_decay, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.72) > 0.70 triggers Piton: "Digital Preservation" 
% branding is an inertial spike that masks the reality of format obsolescence.
constraint_indexing:constraint_classification(cultural_memory_decay, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(cultural_memory_decay, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.81) and coordination intent as a Tangled Rope.
constraint_indexing:constraint_classification(cultural_memory_decay, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(cultural_memory_decay, E), E >= 0.50,
    domain_priors:suppression_score(cultural_memory_decay, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_memory_decay_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(cultural_memory_decay, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cultural_memory_decay, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater results in Piton detection for analytical auditors.
    constraint_indexing:constraint_classification(cultural_memory_decay, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.81) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(cultural_memory_decay, E),

    E > 0.70.

:- end_tests(cultural_memory_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.81) reflects a "Mandatrophy" state where the 
 * coordination of information has effectively consumed the information's longevity.
 * 
 * * PERSPECTIVAL GAP:
 * The Heritage Seeker feels a Snare because their "forever" digital photos 
 * are contingent on a platform's solvency. The Archive Platform sees a 
 * Rope because centralized clouds provide the most efficient way to sync 
 * and coordinate current human activity.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the preservation logic is no longer functional (Theater 0.72); 
 * it is an inert spike siphoning 0.81 of our cultural heritage for 
 * short-term platform metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_format_longevity,
    'Can decentralized protocols stop the decay, or is entropy an irreducible limit (Snare vs Mountain)?',
    'Tracking the accessibility of 20-year-old decentralized vs centralized data formats.',
    'If decentralized data survives: Snare of current policy. If all fails: Mountain of Information Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing and structural linter.
narrative_ontology:interval(cultural_memory_decay, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
