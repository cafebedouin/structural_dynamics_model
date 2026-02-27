% ============================================================================
% CONSTRAINT STORY: maladaptive_selection_process
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maladaptive_selection_process, []).

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
 *   constraint_id: maladaptive_selection_process
 *   human_readable: The Evolutionary Dead-End
 *   domain: organizational/technological
 *
 * SUMMARY:
 *   This scenario describes an evolutionary dead-end, where the selection
 *   criteria within a system become misaligned with long-term survival or
 *   utility. Initially, a set of criteria may be adaptive, encouraging
 *   positive outcomes. However, over time, these criteria can become
 *   decoupled from real value, leading to a selection process that favors
 *   short-term gains at the expense of long-term system health. The system
 *   appears to be a rope to the early adopters.
 *
 * KEY AGENTS:
 *   - Early Adopters: Primary beneficiary (institutional/arbitrage) - Gains initial advantages from the selection process.
 *   - System Longterm Viability: Primary victim (powerless/trapped) - Suffers from the consequences of the maladaptive selection.
 *   - Future Generations: Victim (moderate/constrained) - Inherits a system burdened by the maladaptive process.
 *   - Incumbent Authorities: Beneficiary (institutional/constrained) - Those who benefit are often institutional.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maladaptive_selection_process, 0.6).
domain_priors:suppression_score(maladaptive_selection_process, 0.45).
domain_priors:theater_ratio(maladaptive_selection_process, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maladaptive_selection_process, extractiveness, 0.6).
narrative_ontology:constraint_metric(maladaptive_selection_process, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(maladaptive_selection_process, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maladaptive_selection_process, tangled_rope).
narrative_ontology:human_readable(maladaptive_selection_process, "The Evolutionary Dead-End").
narrative_ontology:topic_domain(maladaptive_selection_process, "organizational/technological").

domain_priors:requires_active_enforcement(maladaptive_selection_process).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maladaptive_selection_process, early_adopters).
narrative_ontology:constraint_beneficiary(maladaptive_selection_process, incumbent_authorities).
narrative_ontology:constraint_victim(maladaptive_selection_process, system_longterm_viability).
narrative_ontology:constraint_victim(maladaptive_selection_process, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The long-term viability of the system is trapped and bears the full cost of the maladaptive selection process.
constraint_indexing:constraint_classification(maladaptive_selection_process, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% Early adopters benefit immediately from the selection process, gaining advantages and resources in the short term.
constraint_indexing:constraint_classification(maladaptive_selection_process, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% An analytical observer sees the mixed coordination and extraction. There is an initial coordination benefit (early adopters are incentivized, but that benefit becomes decoupled from long-term utility or viability.
constraint_indexing:constraint_classification(maladaptive_selection_process, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Future generations are constrained by the path dependency created and suffer the consequences of the maladaptive selection. They may see little benefit but have to bear the cost.
constraint_indexing:constraint_classification(maladaptive_selection_process, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maladaptive_selection_process_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maladaptive_selection_process, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maladaptive_selection_process, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(maladaptive_selection_process, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(maladaptive_selection_process, TR),
    TR >= 0.70.

:- end_tests(maladaptive_selection_process_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.60) because the system siphons value from long-term sustainability to short-term gains. Suppression is also moderate (0.45), as the selection pressure limits alternative, potentially more sustainable paths. The theater ratio is relatively low (0.75), indicating that the maladaptive process is primarily functional rather than performative, at least initially. The theater has increased over the interval as the misalignment between short-term gains and long-term sustainability has become more pronounced, leading to more performative actions to justify the system.
 *
 * PERSPECTIVAL GAP:
 *   Early adopters see the system as a rope, as they benefit from the existing selection criteria. Future generations experience it as a snare because they are trapped with the consequences. Analytical observers can see the system as tangled rope due to the mixed nature of coordination and extraction. Future generations may eventually view it as a piton.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (early adopters) experience the system as coordination, while victims (long-term viability) experience it as extraction. The analytical observer can see the mixed effects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    viability_metric_definition,
    'What is the correct long-term viability metric?',
    'Longitudinal data analysis',
    'Determines whether the system is truly maladaptive or simply optimizes for a different metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(viability_metric_definition, empirical, 'The definition of long-term viability').

omega_variable(
    switching_cost_reduction,
    'Are there interventions that can reduce the cost of switching to a more adaptive system?',
    'Cost-benefit analysis',
    'Affects the practicality of reversing the maladaptive trend.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_reduction, preference, 'The possibility of system switch.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maladaptive_selection_process, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mala_tr_t0, maladaptive_selection_process, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mala_tr_t5, maladaptive_selection_process, theater_ratio, 5, 0.5).
narrative_ontology:measurement(mala_tr_t10, maladaptive_selection_process, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(mala_be_t0, maladaptive_selection_process, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mala_be_t5, maladaptive_selection_process, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(mala_be_t10, maladaptive_selection_process, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maladaptive_selection_process, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
