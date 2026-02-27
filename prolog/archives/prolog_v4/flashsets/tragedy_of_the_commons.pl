% ============================================================================
% CONSTRAINT STORY: tragedy_of_the_commons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tragedy_of_the_commons, []).

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
 *   constraint_id: tragedy_of_the_commons
 *   human_readable: The Tragedy of the Commons
 *   domain: economic/social
 *
 * SUMMARY:
 *   The tragedy of the commons describes a situation where individual users
 *   of a shared resource, acting independently and rationally in their own
 *   self-interest, behave contrary to the common good by depleting or
 *   spoiling that resource through their collective action. This constraint
 *   highlights the tension between individual incentives and collective
 *   outcomes, often leading to unsustainable resource use.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary beneficiaries in the short term (moderate/mobile)
 *   - Shared Resource: Primary victim (powerless/trapped)
 *   - Future Generations: Secondary victim (powerless/trapped)
 *   - Regulatory Body: Attempts to coordinate (institutional/arbitrage)
 *   - Established Norms: Degraded coordination mechanism (powerful/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tragedy_of_the_commons, 0.6).
domain_priors:suppression_score(tragedy_of_the_commons, 0.7).
domain_priors:theater_ratio(tragedy_of_the_commons, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tragedy_of_the_commons, extractiveness, 0.6).
narrative_ontology:constraint_metric(tragedy_of_the_commons, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(tragedy_of_the_commons, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tragedy_of_the_commons, tangled_rope).
narrative_ontology:human_readable(tragedy_of_the_commons, "The Tragedy of the Commons").
narrative_ontology:topic_domain(tragedy_of_the_commons, "economic/social").

domain_priors:requires_active_enforcement(tragedy_of_the_commons).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tragedy_of_the_commons, individual_users).
narrative_ontology:constraint_victim(tragedy_of_the_commons, shared_resource).
narrative_ontology:constraint_victim(tragedy_of_the_commons, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The shared resource is trapped and powerless to prevent its own depletion. Its time horizon is generational, as depletion impacts future users. High extraction and suppression.
constraint_indexing:constraint_classification(tragedy_of_the_commons, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Individual users benefit in the short term from exploiting the resource, but are constrained by the actions of others and face long-term consequences. They can exit by reducing their resource use, but often don't. Moderate power, immediate time horizon, mobile exit.
constraint_indexing:constraint_classification(tragedy_of_the_commons, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% A regulatory body attempts to coordinate resource use and prevent depletion, but faces challenges in enforcement and may be subject to capture or arbitrage. Institutional power, generational time horizon, arbitrage exit.
constraint_indexing:constraint_classification(tragedy_of_the_commons, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Established norms of resource use, which were once effective, have become degraded and are no longer sufficient to prevent depletion. Powerful actors are constrained by existing legal frameworks, even when those frameworks are ineffective. Civilizational time horizon, global scope, constrained exit.
constraint_indexing:constraint_classification(tragedy_of_the_commons, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tragedy_of_the_commons_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tragedy_of_the_commons, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tragedy_of_the_commons, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tragedy_of_the_commons, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tragedy_of_the_commons, TR),
    TR >= 0.70.

:- end_tests(tragedy_of_the_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high due to the depletion of the resource, benefiting individual users but harming the collective. Suppression is moderate as regulations and social norms attempt to curb overuse, but are often ineffective. The theater ratio is moderate, as some regulations are performative rather than truly effective.
 *
 * PERSPECTIVAL GAP:
 *   The shared resource perspective sees a pure snare, while individual users experience a tangled rope due to the short-term benefits but long-term consequences. The regulatory body tries to establish a rope but is often constrained. The established norms are seen as a piton due to their degraded effectiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual users benefit from exploiting the resource, giving them a lower d. The shared resource is trapped, experiencing a high d. The regulatory body attempts to mediate the situation. The norms exert extraction, with high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The tragedy of the commons is classified as a tangled rope because there is a coordination problem (resource use) and an asymmetric extraction (resource depletion). Mislabeling this as a pure snare would ignore the coordination aspect, while mislabeling it as a rope would ignore the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discount_rate,
    'What is the appropriate discount rate for future costs?',
    'Ethical and economic analysis of intergenerational equity',
    'A high discount rate justifies continued depletion; a low rate requires conservation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discount_rate, preference, 'The appropriate discount rate for future costs associated with resource depletion.').

omega_variable(
    enforcement_cost,
    'What is the cost of enforcing resource use restrictions?',
    'Cost-benefit analysis of different enforcement mechanisms',
    'High enforcement costs may make regulation infeasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost, empirical, 'The cost of enforcing resource use restrictions.').

omega_variable(
    resource_substitutability,
    'How easily can alternative resources be substituted?',
    'Technological and market analysis',
    'High substitutability reduces the impact of resource depletion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_substitutability, empirical, 'The ease with which alternative resources can be substituted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tragedy_of_the_commons, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trag_tr_t0, tragedy_of_the_commons, theater_ratio, 0, 0.2).
narrative_ontology:measurement(trag_tr_t5, tragedy_of_the_commons, theater_ratio, 5, 0.3).
narrative_ontology:measurement(trag_tr_t10, tragedy_of_the_commons, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(trag_be_t0, tragedy_of_the_commons, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(trag_be_t5, tragedy_of_the_commons, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(trag_be_t10, tragedy_of_the_commons, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tragedy_of_the_commons, resource_allocation).
narrative_ontology:affects_constraint(tragedy_of_the_commons, climate_change).
narrative_ontology:affects_constraint(tragedy_of_the_commons, overfishing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
