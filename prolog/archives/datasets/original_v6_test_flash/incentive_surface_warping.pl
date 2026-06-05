% ============================================================================
% CONSTRAINT STORY: incentive_surface_warping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incentive_surface_warping, []).

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
 *   constraint_id: incentive_surface_warping
 *   human_readable: The Perverse Optimization Trap (Goodhart's Law)
 *   domain: economic/organizational/technological
 *
 * SUMMARY:
 *   A scenario where a 'Rope' designed to align individual behavior with
 *   collective goals (e.g., performance bonuses, metrics-based funding, or
 *   algorithmic rewards) inadvertently 'warps' the behavioral landscape. This
 *   warping leads to unintended consequences, as individuals optimize for the
 *   metric rather than the actual goal, resulting in a 'Tangled Rope' or even
 *   'Snare' for the system's integrity. Goodhart's Law highlights the
 *   difficulty of aligning measurable metrics with complex or nuanced goals.
 *
 * KEY AGENTS:
 *   - Metric Designers: Primary beneficiary (institutional/arbitrage) - responsible for creating and implementing the incentive system.
 *   - Early Adopters: Secondary beneficiary (powerful/mobile) - initially benefits from gaming the system.
 *   - System Integrity: Primary victim (powerless/trapped) - the overall system suffers as individuals optimize for the metric.
 *   - Unintended Targets: Secondary victim (moderate/constrained) - individuals or groups negatively affected by the perverse optimization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incentive_surface_warping, 0.55).
domain_priors:suppression_score(incentive_surface_warping, 0.6).
domain_priors:theater_ratio(incentive_surface_warping, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incentive_surface_warping, extractiveness, 0.55).
narrative_ontology:constraint_metric(incentive_surface_warping, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(incentive_surface_warping, theater_ratio, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incentive_surface_warping, tangled_rope).
narrative_ontology:human_readable(incentive_surface_warping, "The Perverse Optimization Trap (Goodhart's Law)").
narrative_ontology:topic_domain(incentive_surface_warping, "economic/organizational/technological").

domain_priors:requires_active_enforcement(incentive_surface_warping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incentive_surface_warping, metric_designers).
narrative_ontology:constraint_beneficiary(incentive_surface_warping, early_adopters).
narrative_ontology:constraint_victim(incentive_surface_warping, system_integrity).
narrative_ontology:constraint_victim(incentive_surface_warping, unintended_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEM INTEGRITY (SNARE) - The overall system suffers as individuals optimize for the metric rather than the intended outcome. No exit; bears the cost of misaligned incentives.
constraint_indexing:constraint_classification(incentive_surface_warping, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNINTENDED TARGETS (TANGLED ROPE) - Individuals or groups negatively affected by the perverse optimization, with limited exit options but some ability to adapt or mitigate the harm. Experiences both coordination benefits and extraction costs.
constraint_indexing:constraint_classification(incentive_surface_warping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: METRIC DESIGNERS (ROPE) - The individuals or institutions responsible for creating and implementing the incentive system, initially seeing it as a tool for coordination. Net beneficiary. Experience coordination as they get alignment (at least initially).
constraint_indexing:constraint_classification(incentive_surface_warping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGULATORY BODIES (PITON) - Regulatory bodies which are slow to adapt to the changing landscape. The original purpose of the regulation may be subverted or degraded. High theater as they try to patch problems. The original intent has atrophied, but the constraint remains due to institutional inertia.
constraint_indexing:constraint_classification(incentive_surface_warping, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) - A broad, long-term view reveals the mixed coordination/extraction nature of the incentive structure, the inherent difficulties of aligning metrics with complex goals.
constraint_indexing:constraint_classification(incentive_surface_warping, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incentive_surface_warping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incentive_surface_warping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incentive_surface_warping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incentive_surface_warping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incentive_surface_warping, TR),
    TR >= 0.70.

:- end_tests(incentive_surface_warping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The system extracts value from overall integrity as individuals focus on the metric and neglect other aspects. The extraction is not maximal as some level of coordination is still present. Suppression (0.60): Moderate-High. Suppression refers to the limitations placed on actions outside the defined metric. System integrity is suppressed by optimized metrics. The overall theater ratio increases as the system becomes more 'optimized'. The performance gains are performative only.
 *
 * PERSPECTIVAL GAP:
 *   The metric designers see initial coordination (Rope), while the system as a whole suffers (Snare). Those negatively impacted, see a Tangled Rope scenario. The regulators, constrained by their original mandate, see a piton.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position within the incentive system. The metric designers experience a low d, as they see the system as a coordination mechanism. Those targeted, experience high d and little agency to change things.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_complexity,
    'How much complexity can a metric absorb before becoming counterproductive?',
    'Empirical testing of different metric designs; analysis of historical cases of Goodhart''s Law',
    'Affects the degree of distortion; determines if it''s a rope or a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_complexity, empirical, 'Complexity thresholds for counterproductive metrics').

omega_variable(
    adaptive_capacity,
    'Can the system adapt to perverse optimization effectively?',
    'Monitoring system responses; analyzing how quickly corrective actions are taken.',
    'Determines if the system can revert back to a rope, or degrades into a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_capacity, empirical, 'System''s capacity to adapt to perverse optimization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incentive_surface_warping, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ince_tr_t0, incentive_surface_warping, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ince_tr_t5, incentive_surface_warping, theater_ratio, 5, 0.5).
narrative_ontology:measurement(ince_tr_t10, incentive_surface_warping, theater_ratio, 10, 0.7).

% Extraction over time
narrative_ontology:measurement(ince_be_t0, incentive_surface_warping, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ince_be_t5, incentive_surface_warping, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(ince_be_t10, incentive_surface_warping, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incentive_surface_warping, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
