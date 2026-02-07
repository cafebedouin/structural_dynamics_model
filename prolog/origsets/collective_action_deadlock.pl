% ============================================================================
% CONSTRAINT STORY: collective_action_deadlock
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(collective_action_deadlock, []).

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
 * * constraint_id: collective_action_deadlock
 * human_readable: The Infinite Deliberation Loop
 * domain: political/social
 * * SUMMARY:
 * This constraint represents a state where a group is unable to coordinate 
 * a response to a critical threat because the internal rules for decision-making 
 * favor a veto by any single stakeholder. This stalemate functions as a 'Mountain' 
 * of impossibility for participants while appearing as a 'Snare' for the system's future.
 * * KEY AGENTS:
 * - Stakeholder: Subject (Powerless)
 * - Veto Player: Beneficiary (Institutional)
 * - Game Theorist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.75) as the deadlock consumes time/resources while 
% the underlying crisis worsens, effectively extracting future optionality.
domain_priors:base_extractiveness(collective_action_deadlock, 0.75). 
domain_priors:suppression_score(collective_action_deadlock, 0.65).
domain_priors:theater_ratio(collective_action_deadlock, 0.72). % High theater: constant meetings with no output.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(collective_action_deadlock, extractiveness, 0.75).
narrative_ontology:constraint_metric(collective_action_deadlock, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(collective_action_deadlock, theater_ratio, 0.72).

% This is not a scaffold; it is a structural failure of consensus mechanisms.
% narrative_ontology:has_sunset_clause(collective_action_deadlock). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To an individual stakeholder, the deadlock feels like an unalterable natural law.
constraint_indexing:constraint_classification(collective_action_deadlock, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Veto players view the deadlock as a 'Rope'—a necessary protection of their 
% institutional interests and a coordination mechanism for stability.
constraint_indexing:constraint_classification(collective_action_deadlock, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.72) > 0.70 triggers Piton: The process is an inertial, non-functional spike.
constraint_indexing:constraint_classification(collective_action_deadlock, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(collective_action_deadlock, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.75) and suppression (0.65) trigger the Tangled Rope hybrid.
constraint_indexing:constraint_classification(collective_action_deadlock, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(collective_action_deadlock, E), E >= 0.50,
    domain_priors:suppression_score(collective_action_deadlock, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_action_deadlock_tests).

test(perspectival_gap) :-
    % Verify Mountain for the powerless and Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(collective_action_deadlock, mountain, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_action_deadlock, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater results in Piton detection by systems auditors.
    constraint_indexing:constraint_classification(collective_action_deadlock, piton, 
        context(agent_power(analytical), _, _, _)).

:- end_tests(collective_action_deadlock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.75) represents the 'Mandatrophy' threshold where 
 * the cost of inaction exceeds the value of maintaining the consensus rule.
 * * PERSPECTIVAL GAP:
 * The Stakeholder feels a Mountain (the rules are fixed and impossible to change). 
 * The Veto Player feels a Rope (the rules coordinate and protect their specific agency).
 * * [RESOLVED MANDATROPHY]:
 * Resolved by identifying the system as a Piton for analytical observers. 
 * The "coordination" is performative (theater); the actual structure 
 * is a non-functional inertial lock that extracts the system's ability to adapt.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_veto_logic,
    'Is the deadlock a result of genuine value conflict (Mountain) or strategic rent-seeking (Snare)?',
    'Shadow negotiation audit to identify undisclosed payoffs for veto relief.',
    'If payoffs found: Snare. If value conflict persists: Mountain of pluralism.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(collective_action_deadlock, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
