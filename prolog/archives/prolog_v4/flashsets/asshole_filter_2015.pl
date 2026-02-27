% ============================================================================
% CONSTRAINT STORY: asshole_filter_2015
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asshole_filter_2015, []).

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
 *   constraint_id: asshole_filter_2015
 *   human_readable: The Asshole Filter
 *   domain: psychological/social
 *
 * SUMMARY:
 *   The 'asshole filter' describes how seemingly neutral organizational
 *   structures can inadvertently favor transgressive individuals. This
 *   happens when ethical boundaries are weakly enforced, allowing those who
 *   disregard them to gain an advantage. Simultaneously, individuals who
 *   respect ethical standards are disadvantaged or driven away, leading to a
 *   degradation of organizational culture.
 *
 * KEY AGENTS:
 *   - Transgressive Individuals: Primary beneficiary (powerful/arbitrage) - thrive in the absence of boundaries.
 *   - Non-Transgressive Individuals: Primary victim (powerless/trapped) - repelled by the lack of ethical standards.
 *   - Ethical Organizational Culture: Secondary victim (institutional/constrained) - harmed by the erosion of values.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asshole_filter_2015, 0.6).
domain_priors:suppression_score(asshole_filter_2015, 0.7).
domain_priors:theater_ratio(asshole_filter_2015, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asshole_filter_2015, extractiveness, 0.6).
narrative_ontology:constraint_metric(asshole_filter_2015, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(asshole_filter_2015, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asshole_filter_2015, tangled_rope).
narrative_ontology:human_readable(asshole_filter_2015, "The Asshole Filter").
narrative_ontology:topic_domain(asshole_filter_2015, "psychological/social").

domain_priors:requires_active_enforcement(asshole_filter_2015).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asshole_filter_2015, transgressive_individuals).
narrative_ontology:constraint_victim(asshole_filter_2015, non_transgressive_individuals).
narrative_ontology:constraint_victim(asshole_filter_2015, ethical_organizational_culture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of individuals who are repelled by the transgressive behavior and cannot thrive in the environment.
constraint_indexing:constraint_classification(asshole_filter_2015, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of individuals who benefit from the lack of boundaries and are rewarded for their behavior.
constraint_indexing:constraint_classification(asshole_filter_2015, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Perspective of the intended ethical culture of the organization, which is harmed by the filter.
constraint_indexing:constraint_classification(asshole_filter_2015, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of an observer analyzing the system and recognizing the unintended consequences.
constraint_indexing:constraint_classification(asshole_filter_2015, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asshole_filter_2015_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asshole_filter_2015, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asshole_filter_2015, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asshole_filter_2015, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(asshole_filter_2015_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness reflects the degree to which the organization extracts value from ethical individuals while disproportionately rewarding transgressive behavior. The suppression represents the limitations on ethical individuals' ability to thrive or challenge the behavior. The high extractiveness indicates a significant benefit to the transgressive individuals at the expense of the ethical organizational culture.
 *
 * PERSPECTIVAL GAP:
 *   Transgressive individuals perceive the system as a rope, enabling their advancement. Non-transgressive individuals experience it as a snare, trapping them in an unethical environment. The ethical organizational culture is harmed over time.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural relationships: transgressive individuals benefit and have arbitrage options, resulting in a low 'd' value. Ethical individuals are targeted, with limited or no exit options, leading to a high 'd' value.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification prevents mislabeling the system as either pure coordination or pure extraction by recognizing the mixed incentives and consequences. It acknowledges that while some benefit from the system, others are significantly harmed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundaries_definition,
    'How are the boundaries defined, and what level of transgression is tolerated?',
    'Analyzing the code of conduct and observed behavior.',
    'Determines the level of extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundaries_definition, empirical, 'The clarity and enforcement of organizational boundaries.').

omega_variable(
    reward_systems,
    'What behaviors are rewarded, and how do those rewards contribute to the filter?',
    'Examining promotion and recognition criteria.',
    'Highlights the incentives that drive the filter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reward_systems, empirical, 'Organizational reward structures and their effects on behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asshole_filter_2015, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(assh_tr_t0, asshole_filter_2015, theater_ratio, 0, 0.2).
narrative_ontology:measurement(assh_tr_t5, asshole_filter_2015, theater_ratio, 5, 0.3).
narrative_ontology:measurement(assh_tr_t10, asshole_filter_2015, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(assh_be_t0, asshole_filter_2015, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(assh_be_t5, asshole_filter_2015, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(assh_be_t10, asshole_filter_2015, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asshole_filter_2015, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
