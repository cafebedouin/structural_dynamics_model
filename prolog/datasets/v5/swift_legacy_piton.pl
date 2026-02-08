% ============================================================================
% CONSTRAINT STORY: swift_legacy_piton
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_swift_legacy, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: swift_legacy_piton
 * human_readable: The SWIFT Inertia
 * domain: technological/economic
 * * SUMMARY:
 * A global financial messaging standard that has outlived its technical 
 * utility but remains mandatory due to the "Maintenance Debt" of the global 
 * banking system. 
 * It functions as a Piton—inertial maintenance of a non-functional or 
 * decaying constraint.
 * * KEY AGENTS:
 * - The Remitter: Subject (Powerless) - Faces 3-5 day delays and opaque fees.
 * - The Legacy Bank: Beneficiary (Institutional) - Relies on the Piton for gatekeeping.
 * - The Fintech Disruptor: Auditor (Analytical) - Observes the "Piton" gap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(swift_legacy_piton, 0.49). % High: Opaque correspondent banking fees.
domain_priors:suppression_score(swift_legacy_piton, 0.90).   % Extremely High: National economies disconnected if removed.
domain_priors:theater_ratio(swift_legacy_piton, 0.75).       % High: Modern "updates" (gpi) are often theatrical patches on 1970s tech.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(swift_legacy_piton, extractiveness, 0.49).
narrative_ontology:constraint_metric(swift_legacy_piton, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(swift_legacy_piton, theater_ratio, 0.75).

% Binary flags
domain_priors:requires_active_enforcement(swift_legacy_piton). % Sanctions and compliance regimes.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================= */

% PERSPECTIVE 1: THE GLOBAL SOUTH MERCHANT (SNARE)
% To the merchant, SWIFT is a Snare—a mandatory, slow, and extractive trap with no viable state-sanctioned exit.
constraint_indexing:constraint_classification(swift_legacy_piton, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE CENTRAL BANK (ROPE)
% To the state, it remains a Rope—the only reliable way to coordinate global liquidity and enforcement.
constraint_indexing:constraint_classification(swift_legacy_piton, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Detection of "Piton" status: High theater_ratio indicates the protocol is maintained by inertia, not efficiency.
constraint_indexing:constraint_classification(swift_legacy_piton, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(swift_legacy_piton, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(swift_piton_tests).

test(piton_classification) :-
    % Verify the analytical observer detects the Piton signature.
    constraint_indexing:constraint_classification(swift_legacy_piton, piton, context(agent_power(analytical), _, _, _)).

test(suppression_validation) :-
    % Verify the system recognizes the high suppression as a "Snare" for the powerless.
    domain_priors:suppression_score(swift_legacy_piton, S), S > 0.80.

:- end_tests(swift_piton_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The theater_ratio (0.75) reflects how SWIFT markets "innovation" (like SWIFT gpi) 
 * to mask the underlying maintenance debt of 50-year-old messaging standards.
 * * [RESOLVED MANDATROPHY]
 * Mandatrophy is resolved by the "Piton" classification. The system is no 
 * longer a "Rope" of efficiency; it is a structural fossil that persists 
 * because the cost of "untethering" the global economy is too high.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
% High-extraction (> 0.46) requires omega variable.
omega_variable(
    omega_piton_snap,
    'When does the friction of the Piton exceed the cost of migrating to an Alternative Scaffold?',
    'Comparative transaction volume growth: SWIFT vs. mBridge/ISO20022/P2P networks.',
    'If snap occurs: Sudden collapse of the legacy Rope; If delayed: Continued Piton-decay.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(swift_legacy_piton, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
