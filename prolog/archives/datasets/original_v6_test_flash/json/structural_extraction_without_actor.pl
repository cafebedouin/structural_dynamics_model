% ============================================================================
% CONSTRAINT STORY: structural_extraction_without_actor
% ============================================================================
% Version: 0.2 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_structural_extraction_without_actor, []).

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
 *   constraint_id: structural_extraction_without_actor
 *   human_readable: The Inertial Rent Siphon
 *   domain: economic/social
 *
 * SUMMARY:
 *   This constraint represents a legacy fee-structure or bureaucratic
 *   requirement where the original "service provider" or "beneficiary" has
 *   long since vanished or automated, yet the extraction continues via
 *   institutional inertia. Examples include obsolete taxes, fees for services
 *   no longer rendered, or bureaucratic overhead for defunct programs. The
 *   primary characteristic is the disconnect between the extraction and any
 *   tangible benefit to the payer. The system is actively enforced by the
 *   bureaucracy.
 *
 * KEY AGENTS:
 *   - Taxpayers: Primary target (powerless/trapped) - bear the cost.
 *   - Consumers: Secondary target (powerless/trapped) - bear indirect costs.
 *   - Bureaucracy: Maintains the structure (institutional/constrained) - benefits from inertia, even though they do not benefit in a conventional way.
 *   - Legacy Holders: Beneficiaries (institutional/arbitrage) - benefit from the rent-seeking behavior.
 *   - Analytical Observer: Sees the full structure (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_extraction_without_actor, 0.75).
domain_priors:suppression_score(structural_extraction_without_actor, 0.8).
domain_priors:theater_ratio(structural_extraction_without_actor, 0.9).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_extraction_without_actor, extractiveness, 0.75).
narrative_ontology:constraint_metric(structural_extraction_without_actor, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(structural_extraction_without_actor, theater_ratio, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_extraction_without_actor, tangled_rope).
narrative_ontology:human_readable(structural_extraction_without_actor, "The Inertial Rent Siphon").
narrative_ontology:topic_domain(structural_extraction_without_actor, "economic/social").

domain_priors:requires_active_enforcement(structural_extraction_without_actor).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_extraction_without_actor, legacy_holders).
narrative_ontology:constraint_victim(structural_extraction_without_actor, taxpayers).
narrative_ontology:constraint_victim(structural_extraction_without_actor, consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The taxpayer is trapped within the system and cannot easily avoid the extraction. They bear the cost without any corresponding benefit.
constraint_indexing:constraint_classification(structural_extraction_without_actor, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The bureaucracy maintains the structure due to inertia, even though the original purpose is gone. They are constrained to maintain the system even if it's inefficient.
constraint_indexing:constraint_classification(structural_extraction_without_actor, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer recognizes the extraction but may be unable to change it due to the complexity of the system.
constraint_indexing:constraint_classification(structural_extraction_without_actor, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Consumers are forced to pay higher prices or fees due to the legacy cost. They have no practical ability to avoid the fee, and receive no services.
constraint_indexing:constraint_classification(structural_extraction_without_actor, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% Legacy holders benefit from the rent-seeking behavior of the inertial system. They can arbitrage the system to their advantage.
constraint_indexing:constraint_classification(structural_extraction_without_actor, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_extraction_without_actor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_extraction_without_actor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_extraction_without_actor, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_extraction_without_actor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(structural_extraction_without_actor, TR),
    TR >= 0.70.

:- end_tests(structural_extraction_without_actor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.75) because there is no reciprocal benefit for the extraction. The suppression (0.80) is also high because the taxpayers and consumers have limited ability to avoid the extraction. The theater ratio (0.90) is high because whatever justification originally existed is now purely performative. The system requires active enforcement to maintain the extraction.
 *
 * PERSPECTIVAL GAP:
 *   The taxpayer sees pure extraction (snare) because they are forced to pay without receiving anything in return. The bureaucracy sees a piton because they are maintaining a system that no longer serves its original purpose. The legacy holders see a rope because they are able to arbitrage the system to their advantage. The analytical observer sees the tangled rope, recognizing the extraction and the coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Taxpayers and consumers are victims with no exit, so their directionality is high, leading to a snare classification. The bureaucracy is constrained and maintains the system, so they are a piton. The legacy holders are able to arbitrage the system, so their directionality is low, leading to a rope. The analytical observer is able to recognize the state of affairs and the coordination, leading to a tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The original classification as a piton was incorrect because it did not account for the active enforcement and the beneficiaries of the system. The system is actively extracting resources and requires enforcement, which makes it a tangled rope. The addition of the legacy holders as beneficiaries resolves the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_will_threshold,
    'What level of political will is required to overcome the bureaucratic inertia?',
    'Case studies of successful policy reforms.',
    'Determines the feasibility of eliminating the rent siphon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_will_threshold, preference, 'Political will needed to dismantle the system.').

omega_variable(
    switching_cost_threshold,
    'What is the transaction cost to move to an alternative system?',
    'Cost-benefit analysis.',
    'Determines if the cost of fixing the problem is worth it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_threshold, empirical, 'Transaction cost to change to alternative approach').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_extraction_without_actor, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(struct_tr_t0, structural_extraction_without_actor, theater_ratio, 0, 0.6).
narrative_ontology:measurement(struct_tr_t5, structural_extraction_without_actor, theater_ratio, 5, 0.8).
narrative_ontology:measurement(struct_tr_t10, structural_extraction_without_actor, theater_ratio, 10, 0.9).

% Extraction over time
narrative_ontology:measurement(struct_ex_t0, structural_extraction_without_actor, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(struct_ex_t5, structural_extraction_without_actor, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(struct_ex_t10, structural_extraction_without_actor, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_extraction_without_actor, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
