% ============================================================================
% CONSTRAINT STORY: atrophied_optimization_piton
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [DRAFT]
% ============================================================================

:- module(constraint_atrophied_optimization_piton, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: atrophied_optimization_piton
 *   human_readable: The Ghost in the Habit
 *   domain: technological/social
 *
 * SUMMARY:
 *   An AI optimization loop is deactivated, but its workflows, KPIs, and
 *   nudges remain mandatory. This creates a 'ghost in the habit,' a piton
 *   where a former optimization process has atrophied, leaving behind theater
 *   and extraction without functional benefit. Workers and customers are
 *   victims, while management and process consultants may benefit from the
 *   status quo.
 *
 * KEY AGENTS:
 *   - Workers: Primary target (powerless/trapped) - forced to adhere to obsolete protocols.
 *   - Management: Beneficiary (institutional/constrained) - maintain existing workflow due to institutional inertia.
 *   - Customers: Secondary target (moderate/constrained) - receive suboptimal service due to the atrophied process.
 *   - Process Consultants: Beneficiary (powerful/arbitrage) - may benefit from maintaining the system through contracts.
 *   - Analytical Observer: Sees full structure (analytical/analytical) - recognizes the dysfunction and inefficiency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(atrophied_optimization_piton, 0.2).
domain_priors:suppression_score(atrophied_optimization_piton, 0.75).
domain_priors:theater_ratio(atrophied_optimization_piton, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(atrophied_optimization_piton, extractiveness, 0.2).
narrative_ontology:constraint_metric(atrophied_optimization_piton, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(atrophied_optimization_piton, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(atrophied_optimization_piton, piton).
narrative_ontology:human_readable(atrophied_optimization_piton, "The Ghost in the Habit").
narrative_ontology:topic_domain(atrophied_optimization_piton, "technological/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(atrophied_optimization_piton, management).
narrative_ontology:constraint_beneficiary(atrophied_optimization_piton, process_consultants).
narrative_ontology:constraint_victim(atrophied_optimization_piton, workers).
narrative_ontology:constraint_victim(atrophied_optimization_piton, customers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The worker is trapped within the system, forced to adhere to obsolete protocols. They experience the constraint as a snare.
constraint_indexing:constraint_classification(atrophied_optimization_piton, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Management continues the old workflow, finding it difficult to change. They are institutionally constrained, perpetuating the piton.
constraint_indexing:constraint_classification(atrophied_optimization_piton, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The observer sees the atrophied system, a remnant of a past optimization effort. The system's functional component has decayed, leaving behind a mere shell.
constraint_indexing:constraint_classification(atrophied_optimization_piton, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(atrophied_optimization_piton_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(atrophied_optimization_piton, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(atrophied_optimization_piton, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(atrophied_optimization_piton, TR),
    TR >= 0.70.

:- end_tests(atrophied_optimization_piton_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.20): The extraction is relatively low, as the AI optimization loop is no longer active. The primary extraction comes from the suppression of alternative, potentially more efficient workflows. Suppression (0.75): High, as workers are often required to continue following the established processes, even if they are no longer optimal. Theater Ratio (0.85): Very high, as the processes have become performative, with little functional benefit.
 *
 * PERSPECTIVAL GAP:
 *   The worker experiences the constraint as a snare, as they are trapped within the system and forced to adhere to obsolete protocols. Management sees the constraint as a piton, a remnant of a past optimization effort that is difficult to change. The analytical observer recognizes the dysfunction and inefficiency of the atrophied system.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the agent's position in relation to the atrophied system. Workers and customers bear the costs of the inefficient system, while management may benefit from the continued operation of the existing workflow. The process consultants might benefit through continued service contracts. An analytical observer can see all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by highlighting the piton nature of the constraint. While some extraction and suppression are present, the primary characteristic is the atrophied nature of the system, indicating a degraded or inertial constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(atrophied_optimization_piton, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(atro_tr_t0, atrophied_optimization_piton, theater_ratio, 0, 0.4).
narrative_ontology:measurement(atro_tr_t5, atrophied_optimization_piton, theater_ratio, 5, 0.6).
narrative_ontology:measurement(atro_tr_t10, atrophied_optimization_piton, theater_ratio, 10, 0.85).

% Extraction over time
narrative_ontology:measurement(atro_be_t0, atrophied_optimization_piton, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(atro_be_t5, atrophied_optimization_piton, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(atro_be_t10, atrophied_optimization_piton, base_extractiveness, 10, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(atrophied_optimization_piton, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
