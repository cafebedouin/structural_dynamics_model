% ============================================================================
% CONSTRAINT STORY: anticipatory_capacity_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anticipatory_capacity_failure, []).

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
 *   constraint_id: anticipatory_capacity_failure
 *   human_readable: The Blindside Equilibrium
 *   domain: organizational/technological/cognitive
 *
 * SUMMARY:
 *   An organization or system optimizes for efficiency in its current
 *   environment. This optimization drives out slack resources, including the
 *   cognitive surplus and adaptability necessary to respond to novel
 *   challenges. The system is then vulnerable to 'out-of-distribution' shocks
 *   for which it is unprepared.
 *
 * KEY AGENTS:
 *   - Short-Term Shareholders: Primary beneficiary, arbitrage exit.
 *   - Incumbent Leadership: Secondary beneficiary, short-term rewards, mobile exit.
 *   - Long-Term Organizational Health: Primary victim, trapped.
 *   - Adaptive Capacity: Secondary victim, constrained.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anticipatory_capacity_failure, 0.55).
domain_priors:suppression_score(anticipatory_capacity_failure, 0.65).
domain_priors:theater_ratio(anticipatory_capacity_failure, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anticipatory_capacity_failure, extractiveness, 0.55).
narrative_ontology:constraint_metric(anticipatory_capacity_failure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(anticipatory_capacity_failure, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anticipatory_capacity_failure, tangled_rope).
narrative_ontology:human_readable(anticipatory_capacity_failure, "The Blindside Equilibrium").
narrative_ontology:topic_domain(anticipatory_capacity_failure, "organizational/technological/cognitive").

domain_priors:requires_active_enforcement(anticipatory_capacity_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anticipatory_capacity_failure, short_term_shareholders).
narrative_ontology:constraint_beneficiary(anticipatory_capacity_failure, incumbent_leadership).
narrative_ontology:constraint_victim(anticipatory_capacity_failure, long_term_organizational_health).
narrative_ontology:constraint_victim(anticipatory_capacity_failure, adaptive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Long-term organizational health is trapped by the short-term optimization. Lacks agency to escape the negative consequences. Fully exposed to the risks. Cannot easily reorganize or exit without significant damage.
constraint_indexing:constraint_classification(anticipatory_capacity_failure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Adaptive capacity is constrained because resources are directed toward current operations. Some ability to adapt but severely limited. Benefits slightly from existing infrastructure but mostly a victim of resource misallocation and cognitive narrowing.
constraint_indexing:constraint_classification(anticipatory_capacity_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Short-term shareholders benefit directly from the efficient operations. They can arbitrage their position, taking profits and exiting before major disruptions occur. Sees the constraint as a pure coordination gain.
constraint_indexing:constraint_classification(anticipatory_capacity_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Incumbent leadership benefits in the short-term due to meeting efficiency goals and increasing shareholder value, even though this strategy harms long-term adaptability. Leadership has the mobility to move to another role before the organization collapses. They perceive the situation as pure coordination.
constraint_indexing:constraint_classification(anticipatory_capacity_failure, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical observer, with a civilizational time horizon and the ability to analyze the system, sees this as a Tangled Rope. There is a coordination function to maximize present efficiency, but it comes at the cost of long-term adaptability and overall health.
constraint_indexing:constraint_classification(anticipatory_capacity_failure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anticipatory_capacity_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(anticipatory_capacity_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(anticipatory_capacity_failure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(anticipatory_capacity_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anticipatory_capacity_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) and Suppression (0.65) reflect the high cost of rigidity and loss of cognitive flexibility. The theater ratio is relatively low (0.40) because the system genuinely is becoming more efficient according to standard metrics, it just cannot adjust to anything unexpected.
 *
 * PERSPECTIVAL GAP:
 *   Short-term shareholders and leadership view the optimization as a positive coordination mechanism (Rope), enhancing efficiency and maximizing shareholder value. Long-term organizational health and adaptability see it as a destructive snare, as they lose the capacity to respond to changing conditions. The analytical observer sees the mixed picture: increased efficiency, but at the cost of long-term resilience (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reflect the structural relationship. Short-term shareholders benefit (low d), long-term organizational health bears the cost (high d). Incumbent leadership benefits in the short-term, which still creates a positive directionality but is not at the extremes.
 *
 * MANDATROPHY ANALYSIS:
 *   The system prevents mislabeling coordination as pure extraction (or vice versa) because the perspective is critical. While short-term efficiency improves (coordination), long-term adaptability declines (extraction). Without considering all perspectives, one might incorrectly classify the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disruption_probability,
    'What is the probability of a significant disruption that would render the current optimization obsolete?',
    'Historical analysis of similar systems, Monte Carlo simulations of potential future scenarios, expert elicitation on emerging threats.',
    'High probability would shift the classification more towards Snare for the long-term organizational health perspective. Low probability might justify a pure Rope classification from an efficiency perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disruption_probability, empirical, 'Probability of a disruption that invalidates current optimization.').

omega_variable(
    cognitive_surplus_recoverability,
    'How easily can the cognitive surplus and adaptability be recovered once lost to optimization?',
    'Case studies of organizational change, experiments in cognitive retraining, simulation of alternative decision-making processes.',
    'If easily recoverable, this might be a Scaffold - a temporary measure. If difficult or impossible, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_surplus_recoverability, empirical, 'Ease of cognitive surplus and adaptability recovery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anticipatory_capacity_failure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, anticipatory_capacity_failure, theater_ratio, 0, 0.2).
narrative_ontology:measurement(anti_tr_t5, anticipatory_capacity_failure, theater_ratio, 5, 0.3).
narrative_ontology:measurement(anti_tr_t10, anticipatory_capacity_failure, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, anticipatory_capacity_failure, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(anti_be_t5, anticipatory_capacity_failure, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(anti_be_t10, anticipatory_capacity_failure, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anticipatory_capacity_failure, resource_allocation).
narrative_ontology:affects_constraint(anticipatory_capacity_failure, cognitive_entrenchment).
narrative_ontology:affects_constraint(anticipatory_capacity_failure, organizational_rigidity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
