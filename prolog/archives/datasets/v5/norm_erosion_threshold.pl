% ============================================================================
% CONSTRAINT STORY: norm_erosion_threshold
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(norm_erosion_threshold, []).

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
 * * constraint_id: norm_erosion_threshold
 * human_readable: The Social Fabric Breach
 * domain: social/political/behavioral
 * * SUMMARY:
 * This constraint models the non-linear breakdown of social cooperation when 
 * the perceived rate of "rule-breaking" or "defection" crosses a critical 
 * threshold. While high-trust norms act as a Rope for seamless coordination, 
 * the erosion process creates a Snare where individuals are forced into 
 * defensive, low-trust behaviors to avoid exploitation, liquidating 
 * collective social capital.
 * * KEY AGENTS:
 * - High-Trust Participant: Subject (Powerless)
 * - Norm Arbitrageur / Defector: Beneficiary (Institutional)
 * - Cultural Sociologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.85) as the collapse of trust siphons the surplus of 
% honest cooperation into the "security costs" and "arbitrage gains" of 
% a low-trust environment.
domain_priors:base_extractiveness(norm_erosion_threshold, 0.85). 
domain_priors:suppression_score(norm_erosion_threshold, 0.70). 
domain_priors:theater_ratio(norm_erosion_threshold, 0.78). % High theater: performative adherence to dead norms.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(norm_erosion_threshold, extractiveness, 0.85).
narrative_ontology:constraint_metric(norm_erosion_threshold, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(norm_erosion_threshold, theater_ratio, 0.78).

% This is an emergent social phase transition.
% narrative_ontology:has_sunset_clause(norm_erosion_threshold). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the honest participant, the erosion is a snare: they are trapped between 
% being exploited by defectors or abandoning their values to survive.
constraint_indexing:constraint_classification(norm_erosion_threshold, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The arbitrageur views the remaining trust as a Rope—it is the coordination 
% substrate they exploit to extract maximum private value before the crash.
constraint_indexing:constraint_classification(norm_erosion_threshold, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.78) > 0.70 triggers Piton: the outward symbols of the 
% norm remain, but they no longer coordinate actual behavior.
constraint_indexing:constraint_classification(norm_erosion_threshold, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.85) and coordination intent as a Tangled Rope.
constraint_indexing:constraint_classification(norm_erosion_threshold, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(norm_erosion_threshold, E), E >= 0.50,
    domain_priors:suppression_score(norm_erosion_threshold, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(norm_erosion_threshold_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional defector.
    constraint_indexing:constraint_classification(norm_erosion_threshold, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(norm_erosion_threshold, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_detection) :-
    % Ensure high theater ratio (0.78) triggers Piton classification.
    constraint_indexing:constraint_classification(norm_erosion_threshold, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.85) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(norm_erosion_threshold, E),

    E > 0.70.

:- end_tests(norm_erosion_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) reflects a "Mandatrophy" state where the 
 * social "coordination" is actually a parasitic liquidation of trust.
 * 
 * * PERSPECTIVAL GAP:
 * The High-Trust Participant feels a Snare because their cooperation is 
 * turned against them. The Norm Arbitrageur sees a Rope because the 
 * existing trust facilitates their extractive transactions.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the norm is no longer functional relative to its social purpose 
 * (Theater 0.78); it is an inert spike siphoning 0.85 of the collective 
 * surplus into private hands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_norm_recovery,
    'Can trust be restored through enforcement, or is erosion permanent (Snare vs Mountain)?',
    'Tracking the delta between enforcement cost and norm compliance over a decade.',
    'If trust returns: Snare of current policy. If trust remains low: Mountain of Social Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(norm_erosion_threshold, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
