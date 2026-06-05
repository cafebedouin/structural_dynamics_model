% ============================================================================
% CONSTRAINT STORY: signal_without_control
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(signal_without_control, []).

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
 * * constraint_id: signal_without_control
 * human_readable: The Passive Observational Trap
 * domain: technological/social
 * * SUMMARY:
 * This constraint represents a state where an agent has access to high-fidelity 
 * data streams (signals) regarding a system's state but is structurally barred 
 * from adjusting the system's parameters (control). It creates a "dashboard 
 * without a steering wheel," leading to high psychological and economic extraction.
 * * KEY AGENTS:
 * - Data Consumer: Subject (Powerless)
 * - Platform Architect: Beneficiary (Institutional)
 * - Information Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.79) because the signal presence increases engagement/stress 
% without enabling agency, siphoning attention/resources.
domain_priors:base_extractiveness(signal_without_control, 0.79). 
domain_priors:suppression_score(signal_without_control, 0.60).
domain_priors:theater_ratio(signal_without_control, 0.85). % High theater: the "dashboard" looks functional.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(signal_without_control, extractiveness, 0.79).
narrative_ontology:constraint_metric(signal_without_control, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(signal_without_control, theater_ratio, 0.85).

% Not a scaffold; this is a permanent design feature of extractive platforms.
% narrative_ontology:has_sunset_clause(signal_without_control). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped by the signal; they cannot ignore it, yet cannot act on it.
constraint_indexing:constraint_classification(signal_without_control, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the signal as a 'Rope' for user retention and coordination.
constraint_indexing:constraint_classification(signal_without_control, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.85) > 0.70 triggers Piton: a non-functional inertial spike.
constraint_indexing:constraint_classification(signal_without_control, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(signal_without_control, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.79) triggers the hybrid Tangled Rope signature.
constraint_indexing:constraint_classification(signal_without_control, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(signal_without_control, E), E >= 0.50,
    domain_priors:suppression_score(signal_without_control, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(signal_without_control_tests).

test(perspectival_gap) :-
    % Verify the Snare vs Rope conflict.
    constraint_indexing:constraint_classification(signal_without_control, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(signal_without_control, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_threshold) :-
    % Ensure high theater ratio results in Piton classification for auditors.
    constraint_indexing:constraint_classification(signal_without_control, piton, 
        context(agent_power(analytical), _, _, _)).

:- end_tests(signal_without_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.79) represents the massive cognitive and emotional 
 * rent paid by subjects who monitor signals they cannot influence. 
 * * PERSPECTIVAL GAP:
 * To the 'powerless', the signal is a Snare—an obligatory trap for 
 * their attention. To the 'institutional' actor, it is a Rope—a mechanism for 
 * platform stability and data harvesting.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the Piton classification. While the signal appears to be part 
 * of a coordination system, the high theater ratio (0.85) confirms that 
 * the 'control' function is atrophied or non-existent, leaving only 
 * the extractive 'signal' residue.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_control_access,
    'Is the lack of control a technical latency (Mountain) or an intentional lockout (Snare)?',
    'Introduction of an "Action API" for a controlled subset of users.',
    'If action efficacy is non-zero: Snare. If action efficacy is zero: Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(signal_without_control, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
