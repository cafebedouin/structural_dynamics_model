% ============================================================================
% CONSTRAINT STORY: power_without_responsibility
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_power_without_responsibility, []).

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
 *   constraint_id: power_without_responsibility
 *   human_readable: The Asymmetric Mandate
 *   domain: political/organizational/legal
 *
 * SUMMARY:
 *   A scenario where a "Rope" designed to grant an agent the authority to
 *   manage high-stakes systems (e.g., emergency powers, automated
 *   enforcement, or sovereign immunity) lacks a corresponding accountability
 *   mechanism. This asymmetry can lead to significant extraction from the
 *   governed population, as the agent's decisions are not subject to
 *   effective checks and balances.
 *
 * KEY AGENTS:
 *   - Mandated Agent: The agent granted authority (institutional/arbitrage).
 *   - Governed Population: The population subject to the agent's authority (powerless/trapped).
 *   - Analytical Observer: The observer analyzing the structure (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(power_without_responsibility, 0.6).
domain_priors:suppression_score(power_without_responsibility, 0.7).
domain_priors:theater_ratio(power_without_responsibility, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(power_without_responsibility, extractiveness, 0.6).
narrative_ontology:constraint_metric(power_without_responsibility, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(power_without_responsibility, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(power_without_responsibility, tangled_rope).
narrative_ontology:human_readable(power_without_responsibility, "The Asymmetric Mandate").
narrative_ontology:topic_domain(power_without_responsibility, "political/organizational/legal").

domain_priors:requires_active_enforcement(power_without_responsibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(power_without_responsibility, mandated_agent).
narrative_ontology:constraint_victim(power_without_responsibility, governed_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The governed population experiences the asymmetric mandate as a snare. They are subject to the decisions of the mandated agent but lack effective recourse or means of holding them accountable.
constraint_indexing:constraint_classification(power_without_responsibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The mandated agent experiences the mandate as a rope, granting them the authority to manage high-stakes systems with minimal accountability. They benefit from the lack of constraints on their actions.
constraint_indexing:constraint_classification(power_without_responsibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% An analytical observer recognizes the structure as a Tangled Rope, where the mandated agent has significant power but limited accountability, leading to potential extraction from the governed population.
constraint_indexing:constraint_classification(power_without_responsibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(power_without_responsibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(power_without_responsibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(power_without_responsibility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(power_without_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(power_without_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate-high (0.6) because the mandated agent can exert significant influence on the governed population without being held accountable. The suppression is also high (0.7) because the governed population has limited means of challenging the agent's actions. The theater ratio is low (0.3) because the mandated agent's actions have real consequences.
 *
 * PERSPECTIVAL GAP:
 *   The governed population perceives the situation as a snare because they are subject to the agent's decisions without recourse. The mandated agent views it as a rope, providing them with the necessary authority. The analytical observer sees the tangled rope, recognizing the imbalance between power and accountability.
 *
 * DIRECTIONALITY LOGIC:
 *   The mandated agent benefits from the mandate (low d), while the governed population bears the costs (high d). The analytical observer recognizes the overall structure and extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accountability_mechanism_effectiveness,
    'How effective are the existing accountability mechanisms in practice?',
    'Empirical analysis of cases where the mandated agent''s actions were challenged or reviewed.',
    'If effective, the constraint may be closer to a rope. If ineffective, it is a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_mechanism_effectiveness, empirical, 'Effectiveness of accountability mechanisms').

omega_variable(
    mandated_agent_discretion,
    'To what extent is the mandated agent''s discretion limited by law or regulation?',
    'Legal analysis of the statutes and regulations governing the mandated agent''s powers.',
    'Greater limits on discretion may shift the constraint towards a scaffold or rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandated_agent_discretion, conceptual, 'Extent of mandated agent discretion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(power_without_responsibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(powe_tr_t0, power_without_responsibility, theater_ratio, 0, 0.1).
narrative_ontology:measurement(powe_tr_t5, power_without_responsibility, theater_ratio, 5, 0.2).
narrative_ontology:measurement(powe_tr_t10, power_without_responsibility, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(powe_be_t0, power_without_responsibility, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(powe_be_t5, power_without_responsibility, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(powe_be_t10, power_without_responsibility, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(power_without_responsibility, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
