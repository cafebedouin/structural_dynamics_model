% ============================================================================
% CONSTRAINT STORY: cascading_constraint_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cascading_constraint_failure, []).

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
 *   constraint_id: cascading_constraint_failure
 *   human_readable: The Dominos of Systemic Collapse
 *   domain: technological/infrastructural/economic
 *
 * SUMMARY:
 *   This constraint describes the scenario where a system, built on tightly
 *   coupled interdependent parts (Ropes), faces the risk of cascading failure
 *   when one part fails. This system comprises complex technological,
 *   infrastructural and economic elements. The failure in one element causes
 *   stress on linked elements leading to a 'domino effect' of widespread
 *   systemic collapse. The lack of systemic robustness or substitutability
 *   amplifies the extractiveness of the system.
 *
 * KEY AGENTS:
 *   - General Population: Primary victim (powerless/trapped) - Bears brunt of collapse.
 *   - Future Generations: Secondary victim (powerless/trapped) - Inherits a degraded system.
 *   - Analytical Observer: Assesses the risk (analytical/analytical).
 *   - Legacy Institutions: Those entrenched in the system (institutional/constrained).
 *   - Incumbent Power Structures: Beneficiaries who maintain the system despite its fragility (institutional/arbitrage).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cascading_constraint_failure, 0.6).
domain_priors:suppression_score(cascading_constraint_failure, 0.7).
domain_priors:theater_ratio(cascading_constraint_failure, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cascading_constraint_failure, extractiveness, 0.6).
narrative_ontology:constraint_metric(cascading_constraint_failure, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(cascading_constraint_failure, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cascading_constraint_failure, tangled_rope).
narrative_ontology:human_readable(cascading_constraint_failure, "The Dominos of Systemic Collapse").
narrative_ontology:topic_domain(cascading_constraint_failure, "technological/infrastructural/economic").

domain_priors:requires_active_enforcement(cascading_constraint_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cascading_constraint_failure, incumbent_power_structures).
narrative_ontology:constraint_victim(cascading_constraint_failure, general_population).
narrative_ontology:constraint_victim(cascading_constraint_failure, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The general population is trapped within the system and bears the full cost of collapse, experiencing it as a snare.
constraint_indexing:constraint_classification(cascading_constraint_failure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Future generations are even more powerless and trapped, bearing the long-term consequences of systemic collapse as a snare.
constraint_indexing:constraint_classification(cascading_constraint_failure, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% An analytical observer recognizes the complex interdependencies and potential for cascade failures, seeing it as a tangled rope with both coordinating and extracting elements. The system benefits from interdependencies until a critical failure.
constraint_indexing:constraint_classification(cascading_constraint_failure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Institutions that have become reliant on the existing system may see it as a Piton, no longer effectively coordinating but still maintained through inertia and resistance to change. They're aware of the degradation, but constrained in ability to adapt.
constraint_indexing:constraint_classification(cascading_constraint_failure, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cascading_constraint_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cascading_constraint_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cascading_constraint_failure, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cascading_constraint_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cascading_constraint_failure, TR),
    TR >= 0.70.

:- end_tests(cascading_constraint_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is relatively high because a cascading failure leads to widespread loss of essential services, economic instability, and social disruption. Suppression (0.7) is also high due to limited alternatives once the system begins to fail; exiting becomes extremely difficult. The theater_ratio is now 0.75, reflecting the increasing performative actions taken to maintain the system's appearance of stability.
 *
 * PERSPECTIVAL GAP:
 *   The general population and future generations experience this as a snare (unavoidable harm), while the analytical observer recognizes the potential for both coordination and extraction (tangled rope). Institutions, being constrained to legacy systems, can see the reality more clearly, and recognize that the system has become a piton. Incumbent power structures benefit from maintaining the status quo, even with its inherent risks.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the powerless (general population and future generations) is high because their exit options are limited. The analytical observer has the lowest directionality due to an ability to analyze the risk and propose solutions, but may still be impacted by the actual collapse. Institutions are higher, as they are constrained by their historical relationships to the system. Incumbent power structures have low directionality as they benefit from the system's current configuration.
 *
 * MANDATROPHY ANALYSIS:
 *   The system is classified as a tangled rope because the risks of interdependency have been unaddressed, and the benefits accrue to specific power structures. If there were robust oversight and proactive management of interdependencies, the system might resemble a scaffold or even a rope. However, without those elements, the system becomes a trap for the general populace. This clarifies that system failures are not unavoidable as systems can be designed to avoid catastrophic failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_interdependency_identification,
    'Which interdependencies are most critical to system stability?',
    'Network analysis, stress testing, and simulations.',
    'Identification of key failure points that could trigger cascading collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_interdependency_identification, empirical, 'Identification of critical interdependencies within the system.').

omega_variable(
    robustness_vs_efficiency_tradeoff,
    'What is the optimal balance between system efficiency and robustness?',
    'Cost-benefit analysis, resilience engineering.',
    'Determines the level of redundancy and diversity needed to prevent cascading failures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(robustness_vs_efficiency_tradeoff, conceptual, 'Balancing efficiency and robustness in system design.').

omega_variable(
    adaptive_capacity,
    'How effectively can the system adapt to unforeseen shocks?',
    'Monitoring of adaptive responses, feedback loops.',
    'Determines the system''s ability to recover from disruptions and prevent further cascading failures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_capacity, empirical, 'Adaptive capacity of the system to shocks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cascading_constraint_failure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(casc_tr_t0, cascading_constraint_failure, theater_ratio, 0, 0.4).
narrative_ontology:measurement(casc_tr_t5, cascading_constraint_failure, theater_ratio, 5, 0.6).
narrative_ontology:measurement(casc_tr_t10, cascading_constraint_failure, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(casc_be_t0, cascading_constraint_failure, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(casc_be_t5, cascading_constraint_failure, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(casc_be_t10, cascading_constraint_failure, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cascading_constraint_failure, global_infrastructure).
narrative_ontology:affects_constraint(cascading_constraint_failure, fragile_supply_chains).
narrative_ontology:affects_constraint(cascading_constraint_failure, climate_change_impacts).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
