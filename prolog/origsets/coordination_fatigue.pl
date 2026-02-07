% ============================================================================
% CONSTRAINT STORY: coordination_fatigue
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(coordination_fatigue, []).

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
 * * constraint_id: coordination_fatigue
 * human_readable: The Consensus Exhaustion Loop
 * domain: social/organizational/technological
 * * SUMMARY:
 * A scenario where the energy required to maintain consensus and synchronization 
 * across a hyper-connected network exceeds the creative output of its members.
 * As the "Rope" of coordination thickens, it siphons the subject's agency into 
 * endless administrative overhead, transforming the system into a Snare.
 * * KEY AGENTS:
 * - Distributed Contributor: Subject (Powerless)
 * - Protocol Governance: Beneficiary (Institutional)
 * - Network Health Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.78) as the system siphons the subject's primary 
% cognitive labor into the maintenance of the coordination itself.
domain_priors:base_extractiveness(coordination_fatigue, 0.78). 
domain_priors:suppression_score(coordination_fatigue, 0.62). 
domain_priors:theater_ratio(coordination_fatigue, 0.74). % Piton threshold (> 0.70)

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(coordination_fatigue, extractiveness, 0.78).
narrative_ontology:constraint_metric(coordination_fatigue, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(coordination_fatigue, theater_ratio, 0.74).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The contributor is trapped: the cost of leaving or diverging from the 
% collective sync is higher than the diminishing return of staying.
constraint_indexing:constraint_classification(coordination_fatigue, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The governance layer views high synchronization as a Rope—the only way 
% to ensure protocol stability and prevent fragmentation at scale.
constraint_indexing:constraint_classification(coordination_fatigue, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.74) > 0.70 triggers Piton: the "consensus rituals" are 
% now a non-functional spike of logic that maintains the illusion of progress.
constraint_indexing:constraint_classification(coordination_fatigue, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(coordination_fatigue, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.78) and coordination intent as a Tangled Rope.
constraint_indexing:constraint_classification(coordination_fatigue, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(coordination_fatigue, E), E >= 0.50,
    domain_priors:suppression_score(coordination_fatigue, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coordination_fatigue_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional governance.
    constraint_indexing:constraint_classification(coordination_fatigue, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coordination_fatigue, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater results in Piton detection for analytical auditors.
    constraint_indexing:constraint_classification(coordination_fatigue, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.78) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(coordination_fatigue, E),

    E > 0.70.

:- end_tests(coordination_fatigue_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.78) reflects a "Mandatrophy" state where the 
 * cognitive energy of the network is consumed by the friction of synchronization.
 * 
 * * PERSPECTIVAL GAP:
 * The Distributed Contributor feels a Snare because they spend more time in 
 * "alignment" than in "creation." The Protocol Governance sees a 
 * Rope because consensus is the foundation of their institutional power.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the coordination is no longer functional relative to reality 
 * (Theater 0.74); the system is an inert spike siphoning the creative surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_synchronization_limit,
    'Is there a hard biological limit to coordination, or is this a design flaw (Snare vs Mountain)?',
    'Auditing the delta between synchronization speed and creative throughput over time.',
    'If throughput remains flat: Mountain of Biology. If throughput drops: Snare of Design.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing and structural linter.
narrative_ontology:interval(coordination_fatigue, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
