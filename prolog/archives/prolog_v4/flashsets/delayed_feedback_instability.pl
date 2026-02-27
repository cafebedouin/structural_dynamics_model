% ============================================================================
% CONSTRAINT STORY: delayed_feedback_instability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_delayed_feedback_instability, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: delayed_feedback_instability
 *   human_readable: The Oscillation Trap
 *   domain: systems_engineering/economics/ecology
 *
 * SUMMARY:
 *   This constraint occurs in systems where there is a significant temporal
 *   lag between an action and its observable outcome. This delay leads to
 *   oscillations, as actors over- or under-correct based on outdated
 *   information, creating instability. Short-term actors benefit from
 *   exploiting these oscillations, while long-term stability suffers.
 *
 * KEY AGENTS:
 *   - short_term_actors: Primary beneficiary (institutional/arbitrage) — can exploit system oscillations for immediate gain
 *   - long_term_system_stability: Primary victim (powerless/trapped) — suffers from system oscillations
 *   - regional_regulators: Moderate observer (moderate/constrained) — attempts to stabilize the system but is limited by the time delay
 *   - analytical_observer: Understands the system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(delayed_feedback_instability, 0.55).
domain_priors:suppression_score(delayed_feedback_instability, 0.4).
domain_priors:theater_ratio(delayed_feedback_instability, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(delayed_feedback_instability, extractiveness, 0.55).
narrative_ontology:constraint_metric(delayed_feedback_instability, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(delayed_feedback_instability, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(delayed_feedback_instability, tangled_rope).
narrative_ontology:human_readable(delayed_feedback_instability, "The Oscillation Trap").
narrative_ontology:topic_domain(delayed_feedback_instability, "systems_engineering/economics/ecology").

domain_priors:requires_active_enforcement(delayed_feedback_instability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(delayed_feedback_instability, short_term_actors).
narrative_ontology:constraint_victim(delayed_feedback_instability, long_term_system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Long-term system stability is trapped and powerless to avoid the oscillations. Future generations bear the cost.
constraint_indexing:constraint_classification(delayed_feedback_instability, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Short-term actors can arbitrage the system by exploiting the delayed feedback, benefiting in the short term.
constraint_indexing:constraint_classification(delayed_feedback_instability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% From a civilizational perspective, the delayed feedback creates a mixed system of coordination and extraction, a tangled rope where the system oscillates.
constraint_indexing:constraint_classification(delayed_feedback_instability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Regional regulators who are aware of the system are constrained and can only observe the oscillations.
constraint_indexing:constraint_classification(delayed_feedback_instability, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(delayed_feedback_instability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(delayed_feedback_instability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(delayed_feedback_instability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(delayed_feedback_instability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(delayed_feedback_instability, TR),
    TR >= 0.70.

:- end_tests(delayed_feedback_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): High because the system benefits those taking short-term action at the expense of long-term sustainability. Suppression (0.40): The suppression comes from the lack of timely information and the inability to predict future states accurately.
 *
 * PERSPECTIVAL GAP:
 *   The short-term actors see a Rope, because they are extracting resources and benefits from the oscillations, they see the system as enabling their profit. Long-term system stability sees a Snare because the system is consistently undermined by the oscillations. The Analytical Observer sees a Tangled Rope because they understand both dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Short term actors can arbitrage the system for benefit while long term stability has no escape and bears the cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The system is a Tangled Rope because there is a mix of extracting value but also coordination since the delay creates problems and then short term players find opportunities to use it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feedback_delay_time,
    'What is the time constant for feedback in this system?',
    'Empirical data analysis to determine the time constant.',
    'High delay means more severe extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feedback_delay_time, empirical, 'Uncertainty in the exact delay time.').

omega_variable(
    adaptive_capacity,
    'Can the system adapt to these oscillations?',
    'Observe system behavior over a long timescale',
    'Adaptive capacity would change the classification to a rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_capacity, empirical, 'Whether the system has inherent resilience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(delayed_feedback_instability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dela_tr_t0, delayed_feedback_instability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dela_tr_t5, delayed_feedback_instability, theater_ratio, 5, 0.15).
narrative_ontology:measurement(dela_tr_t10, delayed_feedback_instability, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(dela_be_t0, delayed_feedback_instability, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(dela_be_t5, delayed_feedback_instability, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(dela_be_t10, delayed_feedback_instability, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(delayed_feedback_instability, resource_allocation).
narrative_ontology:affects_constraint(delayed_feedback_instability, resource_depletion).
narrative_ontology:affects_constraint(delayed_feedback_instability, market_manipulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
