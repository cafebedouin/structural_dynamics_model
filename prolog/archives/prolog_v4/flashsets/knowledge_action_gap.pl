% ============================================================================
% CONSTRAINT STORY: knowledge_action_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_action_gap, []).

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
 *   constraint_id: knowledge_action_gap
 *   human_readable: The Informational Friction Barrier
 *   domain: social/technological
 *
 * SUMMARY:
 *   This constraint represents the structural disconnect between having
 *   access to perfect information regarding a systemic risk (e.g., climate
 *   change, public health) and the inability to coordinate a response due to
 *   high switching costs, cognitive load, or entrenched habit-loops. It is
 *   classified as a tangled rope because it involves both a coordination
 *   failure and asymmetric extraction. Status quo actors and habitual
 *   consumers benefit from maintaining the current system, while future
 *   generations and informed individuals bear the costs of inaction. The high
 *   suppression reflects the difficulty of overcoming entrenched behaviors
 *   and institutional inertia.
 *
 * KEY AGENTS:
 *   - Status Quo Actors: Primary beneficiary (institutional/arbitrage) - maintain their advantageous position due to collective inertia.
 *   - Habitual Consumers: Primary beneficiary (powerful/mobile) - benefit from immediate rewards and inertia of bad habit loops
 *   - Future Generations: Primary victim (powerless/trapped) - will bear most of the cost.
 *   - Informed but Inert Individuals: Secondary victim (moderate/constrained) - understand risk, but can't effect change.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_action_gap, 0.6).
domain_priors:suppression_score(knowledge_action_gap, 0.7).
domain_priors:theater_ratio(knowledge_action_gap, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_action_gap, extractiveness, 0.6).
narrative_ontology:constraint_metric(knowledge_action_gap, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(knowledge_action_gap, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_action_gap, tangled_rope).
narrative_ontology:human_readable(knowledge_action_gap, "The Informational Friction Barrier").
narrative_ontology:topic_domain(knowledge_action_gap, "social/technological").

domain_priors:requires_active_enforcement(knowledge_action_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_action_gap, status_quo_actors).
narrative_ontology:constraint_beneficiary(knowledge_action_gap, habitual_consumers).
narrative_ontology:constraint_victim(knowledge_action_gap, future_generations).
narrative_ontology:constraint_victim(knowledge_action_gap, informed_but_inert_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future generations bear the cost of inaction on systemic risks and have no agency or ability to exit the situation.
constraint_indexing:constraint_classification(knowledge_action_gap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Individuals who understand the risks but are constrained by habit, cognitive load, or switching costs experience the constraint as a tangled rope. They benefit from the lower immediate cost of inaction but are harmed in the long run.
constraint_indexing:constraint_classification(knowledge_action_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Actors who benefit from the existing system (e.g., industries profiting from fossil fuels) see the constraint as a rope because it maintains their advantageous position. They benefit from the collective inertia and the difficulty of coordinating change.
constraint_indexing:constraint_classification(knowledge_action_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Individuals locked into detrimental habit loops (e.g. fast food) benefit from the immedate reward and face short term cost of adaptation. However, the long term cost (health issues) are very high. Benefit derived from inertial force.
constraint_indexing:constraint_classification(knowledge_action_gap, piton,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% The analytical observer sees the knowledge-action gap as a tangled rope because it involves both coordination failures (difficulty of collective action) and asymmetric extraction (benefits accruing to those maintaining the status quo at the expense of future generations).
constraint_indexing:constraint_classification(knowledge_action_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_action_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(knowledge_action_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_action_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_action_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(knowledge_action_gap, TR),
    TR >= 0.70.

:- end_tests(knowledge_action_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: (0.6) - High extraction because the beneficiaries are extracting significant value by pushing long term costs. Suppression (0.7) - high level of institutional inertia. Theater Ratio - (0.4).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different structural positions of the agents. Future generations see a snare because they are trapped and bear the full costs. Status quo actors see a rope because it coordinates to their benefit. Analytical observer sees a tangled rope because there is extraction and an attempt to coordinate to solve a problem.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by whether the constraint enables extraction toward or away from an agent. Beneficiaries profit, those being extracted against become victims.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the DR framework, this constraint could be misclassified as a coordination problem. However, by analyzing the power, time, exit, and scope, it is clear that there is a structural imbalance in extraction. Long term effect outweigh any current benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_tolerance,
    'What level of economic or cognitive switching cost is tolerable for individuals to alter their behavior?',
    'Behavioral economic analysis and sociological surveys.',
    'Impacts policy decision regarding interventions, taxes, etc.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_tolerance, empirical, 'Tolerance for changing behaviors given increased costs.').

omega_variable(
    collective_action_threshold,
    'What percentage of actors are needed to initiate change?',
    'Network analysis of historical regime changes. Agent-based modelling of behavior',
    'Impacts mobilization effort, policy decision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'What is needed for critical mass to affect change').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_action_gap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(know_tr_t0, knowledge_action_gap, theater_ratio, 0, 0.2).
narrative_ontology:measurement(know_tr_t5, knowledge_action_gap, theater_ratio, 5, 0.3).
narrative_ontology:measurement(know_tr_t10, knowledge_action_gap, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(know_be_t0, knowledge_action_gap, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(know_be_t5, knowledge_action_gap, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(know_be_t10, knowledge_action_gap, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_action_gap, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
