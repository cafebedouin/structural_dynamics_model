% ============================================================================
% CONSTRAINT STORY: swift_piton_snap
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_piton_snap, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: swift_piton_snap
 * human_readable: The Great Decoupling
 * domain: technological/economic
 * * SUMMARY:
 * The sudden failure of the SWIFT legacy protocol as global liquidity migrates 
 * to real-time P2P settlement layers.
 * The "Piton" (legacy code) snaps under the tension of maintenance debt and 
 * alternative sovereignty.
 * * KEY AGENTS:
 * - The Global Citizen: Subject (Powerful) - Now capable of instant, low-fee exit.
 * - The Central Bank: Beneficiary (Powerless) - Unable to enforce the legacy Rope.
 * - The Network Architect: Architect (Organized) - Managing the new Scaffold.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(swift_piton_snap, 0.04). % Low: The new system is a "Mountain" of code/physics.
domain_priors:suppression_score(swift_piton_snap, 0.10).   % Low: High degree of freedom and alternative protocols.
domain_priors:theater_ratio(swift_piton_snap, 0.05).       % Minimal: The system works or it doesn't; no theater.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(swift_piton_snap, extractiveness, 0.04).
narrative_ontology:constraint_metric(swift_piton_snap, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(swift_piton_snap, theater_ratio, 0.05).

% Binary flags
narrative_ontology:has_sunset_clause(swift_piton_snap).    % The transition phase is temporary.
domain_priors:requires_active_enforcement(swift_piton_snap). % Automated smart-contract enforcement.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LEGACY BANKER (SNARE)
% For the institution that can no longer extract rent, the new network is a Snare—it traps their old business model.
constraint_indexing:constraint_classification(swift_piton_snap, snare, 
    context(agent_power(institutional), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE MOBILE SUBJECT (MOUNTAIN)
% For the citizen, the protocol is now as fundamental and unchangeable as a Mountain (fixed physical/logical limit).
constraint_indexing:constraint_classification(swift_piton_snap, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE TRANSITION ARCHITECT (SCAFFOLD)
% The system is a temporary support structure until the new global standard hardens.
constraint_indexing:constraint_classification(swift_piton_snap, scaffold, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(global))) :-
    narrative_ontology:has_sunset_clause(swift_piton_snap).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(piton_snap_tests).

test(extraction_drop) :-
    % Verify that base extractiveness has dropped below the Snare/Tangled threshold.
    domain_priors:base_extractiveness(swift_piton_snap, E), E =< 0.05.

test(mountain_classification) :-
    % Verify the transition to a Mountain-type constraint for the powerless user.
    constraint_indexing:constraint_classification(swift_piton_snap, mountain, context(agent_power(powerless), _, _, _)).

:- end_tests(piton_snap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Snap" occurs when base_extractiveness drops (0.04) and suppression 
 * evaporates (0.10).
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the emergence of a new "Mountain." 
 * Coordination is no longer enforced by institutional theater (Piton) but by 
 * the mathematical necessity of the code.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_re_institutionalization,
    'Will the new Mountain eventually be "Embraced" by new giants to become a Tangled Rope?',
    'Analysis of whale-concentration in the new network vs. original decentralization targets.',
    'If concentration: The snap leads to a new EEE cycle; If distributed: Civilizational Rope.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(swift_piton_snap, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
