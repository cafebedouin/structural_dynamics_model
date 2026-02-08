% ============================================================================
% CONSTRAINT STORY: delayed_feedback_instability
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(delayed_feedback_instability, []).

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
 * * constraint_id: delayed_feedback_instability
 * human_readable: The Oscillation Trap
 * domain: systems_engineering/economics/ecology
 * * SUMMARY:
 * This constraint occurs in systems where there is a significant temporal lag 
 * between an action and its observable outcome. This delay causes agents to 
 * over-correct (or under-correct), leading to violent oscillations and 
 * systemic instability. It functions as a Snare for the individual trying to 
 * maintain balance, while appearing as a Rope for institutions that profit 
 * from the resulting volatility or use it to justify centralized control.
 * * KEY AGENTS:
 * - Reactive Agent: Subject (Powerless)
 * - Volatility Arbitrageur: Beneficiary (Institutional)
 * - Control Systems Engineer: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.80) because the "blindness" caused by the delay allows 
% the system to siphon the agent's resources through forced, inefficient 
% over-corrections.
domain_priors:base_extractiveness(delayed_feedback_instability, 0.80). 
domain_priors:suppression_score(delayed_feedback_instability, 0.65). 
domain_priors:theater_ratio(delayed_feedback_instability, 0.45). % Moderate theater: Crisis management narratives.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(delayed_feedback_instability, extractiveness, 0.8).
narrative_ontology:constraint_metric(delayed_feedback_instability, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(delayed_feedback_instability, theater_ratio, 0.45).

% This is an inherent property of time-lagged feedback loops.
% narrative_ontology:has_sunset_clause(delayed_feedback_instability). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The agent is trapped: every effort to stabilize the system only increases 
% the magnitude of the next swing because the data they act on is stale.
constraint_indexing:constraint_classification(delayed_feedback_instability, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the instability as a Rope—a way to coordinate 
% market shifts or justify "emergency" powers to manage the chaos they 
% structurally ignore.
constraint_indexing:constraint_classification(delayed_feedback_instability, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a mathematical perspective, the Nyquist-Shannon limit and phase 
% shifts in feedback loops represent an irreducible Mountain of physics.
constraint_indexing:constraint_classification(delayed_feedback_instability, mountain, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (TANGLED ROPE)
% High extraction (0.80) triggers the hybrid Tangled Rope signature at the 
% civilizational scale.
constraint_indexing:constraint_classification(delayed_feedback_instability, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(delayed_feedback_instability, E), E >= 0.50,
    domain_priors:suppression_score(delayed_feedback_instability, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(delayed_feedback_instability_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless agent vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(delayed_feedback_instability, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(delayed_feedback_instability, rope, 
        context(agent_power(institutional), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.80) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(delayed_feedback_instability, E),

    E > 0.70.

:- end_tests(delayed_feedback_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.80) reflects a "Mandatrophy" state where the 
 * "coordination" is actually a parasitic exploitation of signal lag.
 * 
 * * PERSPECTIVAL GAP:
 * The Reactive Agent feels a Snare because they are biologically or 
 * economically punished for systemic delays they cannot control. The 
 * Volatility Arbitrageur sees a Rope because predictable cycles of 
 * over-correction allow for massive resource capture and coordination.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This recognizes that 
 * the instability is the "tangled" inverse of a system's coordination 
 * speed—the slower the feedback, the more extractive the resulting Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_lag_reduction,
    'Can zero-latency communication or predictive modeling eliminate the delay (Snare or Mountain)?',
    'Auditing the delta between predictive AI accuracy and actual system lag.',
    'If AI stabilizes: Snare of policy/tech. If AI fails: Mountain of Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(delayed_feedback_instability, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
