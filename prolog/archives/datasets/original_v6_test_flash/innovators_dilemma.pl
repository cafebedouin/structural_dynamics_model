% ============================================================================
% CONSTRAINT STORY: innovators_dilemma
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_innovators_dilemma, []).

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
 *   constraint_id: innovators_dilemma
 *   human_readable: The Innovator's Dilemma
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Innovator's Dilemma describes how successful companies, by rationally
 *   listening to their best customers and investing in high-margin products,
 *   create a structural inability to respond to 'disruptive' technologies
 *   that start in low-margin, niche markets. This creates a tension between
 *   short-term profits and long-term survival.
 *
 * KEY AGENTS:
 *   - Incumbent Shareholders: Benefit from short-term profits (institutional/arbitrage)
 *   - Incumbent Employees: Suffer job losses (powerless/trapped)
 *   - Incumbent Customers: Initially benefit, but ultimately suffer (moderate/constrained)
 *   - Disruptive Innovators: Benefit from incumbent's inability to adapt (powerful/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(innovators_dilemma, 0.55).
domain_priors:suppression_score(innovators_dilemma, 0.65).
domain_priors:theater_ratio(innovators_dilemma, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(innovators_dilemma, extractiveness, 0.55).
narrative_ontology:constraint_metric(innovators_dilemma, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(innovators_dilemma, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(innovators_dilemma, tangled_rope).
narrative_ontology:human_readable(innovators_dilemma, "The Innovator's Dilemma").
narrative_ontology:topic_domain(innovators_dilemma, "economic/technological").

domain_priors:requires_active_enforcement(innovators_dilemma).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(innovators_dilemma, incumbent_shareholders).
narrative_ontology:constraint_beneficiary(innovators_dilemma, disruptive_innovators).
narrative_ontology:constraint_victim(innovators_dilemma, incumbent_employees).
narrative_ontology:constraint_victim(innovators_dilemma, incumbent_customers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Incumbent employees face job losses and skill obsolescence as a result of the incumbent's inability to adapt to disruptive technologies. They are trapped by their specialized skills and location.
constraint_indexing:constraint_classification(innovators_dilemma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Incumbent customers initially benefit from the incumbent's focus on sustaining innovations, but ultimately suffer as the incumbent fails to adopt disruptive technologies that better meet their needs or offer lower prices. They are constrained by switching costs and network effects.
constraint_indexing:constraint_classification(innovators_dilemma, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Incumbent shareholders initially benefit from the incumbent's focus on high-margin sustaining innovations, but ultimately bear the cost of the incumbent's failure to adopt disruptive technologies. They can arbitrage by investing in disruptive innovators.
constraint_indexing:constraint_classification(innovators_dilemma, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Disruptive innovators benefit from the incumbent's inability to respond to their innovations. They are mobile and can enter the market with lower prices or better features.
constraint_indexing:constraint_classification(innovators_dilemma, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical observer sees the Innovator's Dilemma as a tangled rope, a hybrid of coordination and extraction. It coordinates the rational behavior of companies to focus on high-margin products, but it extracts from incumbent employees and customers.
constraint_indexing:constraint_classification(innovators_dilemma, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(innovators_dilemma_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(innovators_dilemma, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(innovators_dilemma, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(innovators_dilemma, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(innovators_dilemma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the incumbent is extracting value from employees and customers in the long run, even though it is providing value in the short run. Suppression is high (0.65) because the incumbent is suppressing disruptive innovations by focusing on sustaining innovations. Theater Ratio is moderate (0.30) because incumbents genuinely believe they are acting in the best interests of all stakeholders.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent shareholders see the dilemma as a coordination problem: they are rationally allocating resources to the most profitable opportunities. The disruptive innovators see the dilemma as an opportunity to disrupt the market. The incumbent employees and customers see the dilemma as a snare, trapping them in a declining ecosystem.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent shareholders have arbitrage exit options and benefit in the short-term, yielding a low directionality. Incumbent employees are trapped and bear the costs, leading to a high directionality. Customers are constrained, leading to a moderate directionality. Disruptive innovators are mobile and benefit, leading to a low directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rate_of_disruption,
    'How rapidly are disruptive technologies emerging in a given industry?',
    'Analyze the frequency and impact of new technologies entering the market.',
    'High rate of disruption exacerbates the dilemma, making it more difficult for incumbents to adapt. Low rate reduces the pressure on incumbents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rate_of_disruption, empirical, 'Determines the pressure on incumbents to adapt.').

omega_variable(
    incumbent_adaptability,
    'What organizational structures and processes enable incumbents to adapt to disruption?',
    'Case studies of successful and unsuccessful incumbent responses to disruption.',
    'High adaptability reduces the extractiveness of the dilemma. Low adaptability makes it more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_adaptability, empirical, 'Incumbent''s capacity to adapt to disruption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(innovators_dilemma, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inno_tr_t0, innovators_dilemma, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inno_tr_t5, innovators_dilemma, theater_ratio, 5, 0.25).
narrative_ontology:measurement(inno_tr_t10, innovators_dilemma, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(inno_be_t0, innovators_dilemma, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(inno_be_t5, innovators_dilemma, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(inno_be_t10, innovators_dilemma, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(innovators_dilemma, resource_allocation).
narrative_ontology:affects_constraint(innovators_dilemma, creative_destruction).
narrative_ontology:affects_constraint(innovators_dilemma, technological_lock_in).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
