% ============================================================================
% CONSTRAINT STORY: container_capacity_mismatch
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_container_capacity_mismatch, []).

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
 *   constraint_id: container_capacity_mismatch
 *   human_readable: The Volume-Infrastructure Paradox
 *   domain: logistics/physical_infrastructure/economic
 *
 * SUMMARY:
 *   The Volume-Infrastructure Paradox describes a situation where the rate of
 *   commodity or data production exceeds the capacity of the infrastructure
 *   designed to transport or process it. This leads to inefficiencies,
 *   bottlenecks, and potentially economic disruptions. This constraint is
 *   active in logistics, data infrastructure, and commodity markets.
 *
 * KEY AGENTS:
 *   - Commodity Producers: Primary beneficiary (institutional/arbitrage) - benefits from increased production.
 *   - Downstream Consumers: Primary victim (powerless/trapped) - suffers from price increases and limited availability.
 *   - Early Infrastructure Providers: Secondary actor (powerful/constrained) - benefits initially, but constrained by capacity limits. First mover advantage.
 *   - Late Infrastructure Adopters: Piton, constrained by network effects.
 *   - Analytical Observer: Analytical observer (analytical/analytical) - analyzes structural problems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(container_capacity_mismatch, 0.6).
domain_priors:suppression_score(container_capacity_mismatch, 0.7).
domain_priors:theater_ratio(container_capacity_mismatch, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(container_capacity_mismatch, extractiveness, 0.6).
narrative_ontology:constraint_metric(container_capacity_mismatch, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(container_capacity_mismatch, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(container_capacity_mismatch, tangled_rope).
narrative_ontology:human_readable(container_capacity_mismatch, "The Volume-Infrastructure Paradox").
narrative_ontology:topic_domain(container_capacity_mismatch, "logistics/physical_infrastructure/economic").

domain_priors:requires_active_enforcement(container_capacity_mismatch).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(container_capacity_mismatch, commodity_producers).
narrative_ontology:constraint_beneficiary(container_capacity_mismatch, early_infrastructure_providers).
narrative_ontology:constraint_victim(container_capacity_mismatch, downstream_consumers).
narrative_ontology:constraint_victim(container_capacity_mismatch, late_infrastructure_adopters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Downstream consumers are trapped by the limited capacity, leading to increased prices and reduced availability. They lack the power to change the infrastructure and are directly affected by the volume-infrastructure mismatch. They cannot arbitrage the situation due to limited alternatives.
constraint_indexing:constraint_classification(container_capacity_mismatch, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Commodity producers benefit from the initial hyper-efficiency. They can arbitrage the situation by finding alternative routes or methods for distribution, although not infinitely scalable, or investing in solutions. The constraint acts as a coordination mechanism for increased output and profit.
constraint_indexing:constraint_classification(container_capacity_mismatch, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Early infrastructure providers initially benefit from increased demand and usage, but are constrained by the eventual capacity mismatch. They experience both benefits and challenges related to the increased volume. Expansion and upgrade of infrastructure incur costs.
constraint_indexing:constraint_classification(container_capacity_mismatch, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Later adopters of infrastructure are constrained to a state of technical debt, forced to participate in an outmoded architecture or lose network effects.
constraint_indexing:constraint_classification(container_capacity_mismatch, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% From a civilizational perspective, the volume-infrastructure paradox reflects a recurring pattern where innovation outpaces infrastructure development, leading to bottlenecks and inefficiencies. Analyzes structural features that causes the mismatch.
constraint_indexing:constraint_classification(container_capacity_mismatch, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(container_capacity_mismatch_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(container_capacity_mismatch, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(container_capacity_mismatch, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(container_capacity_mismatch, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(container_capacity_mismatch, TR),
    TR >= 0.70.

:- end_tests(container_capacity_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.60 - reflects the increased costs and reduced availability experienced by downstream consumers due to the infrastructure bottleneck.  Suppression: 0.70 - represents the limited alternatives available to consumers and the constraints faced by later infrastructure adopters.  Theater Ratio: 0.30 - reflects the minimal performative elements in basic transport; most activity is purely functional.
 *
 * PERSPECTIVAL GAP:
 *   Downstream consumers experience the paradox as a snare due to limited exit options and high costs. Commodity producers view it as a rope, enabling higher profits. Early infrastructure providers see it as a tangled rope, balancing increased demand with capacity constraints. The analytical observer classifies it as a tangled rope due to its systemic nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Commodity producers have high mobility and capacity for arbitrage, making this rope from their perspective. Downstream consumers have low mobility and are trapped; this is a snare for them. The early infrastructure adopters are constrained to upgrade or lose market share to innovative technology, a tangible rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by considering the benefits to commodity producers. While downstream consumers experience extraction, the constraint also enables economic activity and innovation. The early infrastructure adoption aspect also contributes to the mixed nature, justifying the tangled rope classification from the observer's perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scalability_limits,
    'What are the fundamental scalability limits of the current infrastructure?',
    'Technological assessments and simulations to identify bottleneck areas.',
    'Determines the potential scope and severity of the volume-infrastructure mismatch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scalability_limits, empirical, 'Identifies the scalability limits of the existing infrastructure.').

omega_variable(
    alternative_infrastructure,
    'What alternative infrastructure solutions are available or under development?',
    'Research and evaluation of emerging technologies and approaches.',
    'Impacts the potential for mitigating the volume-infrastructure mismatch through technological innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_infrastructure, empirical, 'Evaluates the availability and effectiveness of alternative infrastructure solutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(container_capacity_mismatch, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cont_tr_t0, container_capacity_mismatch, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cont_tr_t5, container_capacity_mismatch, theater_ratio, 5, 0.2).
narrative_ontology:measurement(cont_tr_t10, container_capacity_mismatch, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(cont_be_t0, container_capacity_mismatch, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cont_be_t5, container_capacity_mismatch, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cont_be_t10, container_capacity_mismatch, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(container_capacity_mismatch, resource_allocation).
narrative_ontology:affects_constraint(container_capacity_mismatch, supply_chain_fragility).
narrative_ontology:affects_constraint(container_capacity_mismatch, energy_grid_vulnerability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
