% ============================================================================
% CONSTRAINT STORY: max_flow
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [DRAFT]
% ============================================================================

:- module(constraint_max_flow, []).

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
 *   constraint_id: max_flow
 *   human_readable: Socio-Technical Bottlenecks Governed by Max-Flow Principles
 *   domain: technological/economic
 *
 * SUMMARY:
 *   This constraint models the socio-technical application of the Max-Flow
 *   Min-Cut theorem, not the mathematical theorem itself. It describes
 *   situations where bottlenecks in a system, as defined by the max-flow
 *   min-cut theorem, are controlled by certain actors, creating opportunities
 *   for extraction from downstream users. The classic example is a toll
 *   bridge where an actor extracts rents. Another case is a chokepoint in a
 *   network or supply chain.
 *
 * KEY AGENTS:
 *   - Bottleneck Controllers: Actors who control the bottleneck. (institutional/arbitrage)
 *   - Downstream Users: Those affected by the bottleneck. (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(max_flow, 0.5).
domain_priors:suppression_score(max_flow, 0.6).
domain_priors:theater_ratio(max_flow, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(max_flow, extractiveness, 0.5).
narrative_ontology:constraint_metric(max_flow, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(max_flow, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(max_flow, tangled_rope).
narrative_ontology:human_readable(max_flow, "Socio-Technical Bottlenecks Governed by Max-Flow Principles").
narrative_ontology:topic_domain(max_flow, "technological/economic").

domain_priors:requires_active_enforcement(max_flow).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(max_flow, bottleneck_controllers).
narrative_ontology:constraint_victim(max_flow, downstream_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Downstream users are trapped and fully exposed to the extraction.
constraint_indexing:constraint_classification(max_flow, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Bottleneck controllers benefit, can arbitrage, and see coordination.
constraint_indexing:constraint_classification(max_flow, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Analytical observer sees tangled rope with some extraction.
constraint_indexing:constraint_classification(max_flow, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(max_flow_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(max_flow, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(max_flow, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(max_flow, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(max_flow_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.5 because the bottleneck allows for extraction. Suppression is 0.6 because downstream users are partially suppressed. Theater is 0.2 because there is relatively little theater.
 *
 * PERSPECTIVAL GAP:
 *   Downstream users see a snare, while bottleneck controllers see a rope. The analytical observer sees a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Bottleneck controllers benefit. Downstream users bear cost.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    flow_capacity,
    'What is the true flow capacity of the system?',
    'Empirical measurement of flow under stress.',
    'Determines the severity of the bottleneck.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flow_capacity, empirical, 'The real-world flow capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(max_flow, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(max__tr_t0, max_flow, theater_ratio, 0, 0.1).
narrative_ontology:measurement(max__tr_t5, max_flow, theater_ratio, 5, 0.2).
narrative_ontology:measurement(max__tr_t10, max_flow, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(max__be_t0, max_flow, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(max__be_t5, max_flow, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(max__be_t10, max_flow, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(max_flow, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
