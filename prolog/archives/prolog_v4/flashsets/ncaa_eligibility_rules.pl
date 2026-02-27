% ============================================================================
% CONSTRAINT STORY: ncaa_eligibility_rules
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ncaa_eligibility_rules, []).

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
 *   constraint_id: ncaa_eligibility_rules
 *   human_readable: NCAA Eligibility Rules for Student Athletes
 *   domain: economic
 *
 * SUMMARY:
 *   The NCAA eligibility rules govern who can participate in college
 *   athletics, restricting the economic opportunities of student-athletes
 *   while generating significant revenue for universities and the NCAA. This
 *   creates a structural tension between the benefits to the institutions and
 *   the costs to the athletes.
 *
 * KEY AGENTS:
 *   - Student Athletes: Primary target (powerless/trapped) – limited economic opportunities
 *   - NCAA: Primary beneficiary (institutional/arbitrage) – revenue generation and control
 *   - Universities: Secondary beneficiary (institutional/constrained) – revenue and talent acquisition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ncaa_eligibility_rules, 0.65).
domain_priors:suppression_score(ncaa_eligibility_rules, 0.7).
domain_priors:theater_ratio(ncaa_eligibility_rules, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ncaa_eligibility_rules, extractiveness, 0.65).
narrative_ontology:constraint_metric(ncaa_eligibility_rules, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ncaa_eligibility_rules, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ncaa_eligibility_rules, tangled_rope).
narrative_ontology:human_readable(ncaa_eligibility_rules, "NCAA Eligibility Rules for Student Athletes").
narrative_ontology:topic_domain(ncaa_eligibility_rules, "economic").

domain_priors:requires_active_enforcement(ncaa_eligibility_rules).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ncaa_eligibility_rules, ncaa).
narrative_ontology:constraint_beneficiary(ncaa_eligibility_rules, universities).
narrative_ontology:constraint_victim(ncaa_eligibility_rules, student_athletes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Student athletes are trapped by the rules, with limited ability to monetize their skills outside of the NCAA framework. They bear the costs of restricted economic opportunities.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The NCAA benefits from the eligibility rules, as they ensure control over the athlete pool and maintain the amateurism model, enabling revenue generation.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Universities benefit from the rules by attracting talented athletes and generating revenue, but are also constrained by the need to comply with NCAA regulations.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a broad perspective, the rules present a mixed bag of coordination (organizing college sports) and extraction (limiting athlete compensation), creating a tangled rope.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ncaa_eligibility_rules_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ncaa_eligibility_rules, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ncaa_eligibility_rules, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ncaa_eligibility_rules, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ncaa_eligibility_rules_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because student-athletes generate significant revenue for the NCAA and universities, while their compensation is limited. Suppression is also high due to the lack of alternatives for athletes seeking to play at a high level. The theater ratio is moderate, reflecting that while there is some emphasis on amateurism, the commercial aspects of college sports are significant.
 *
 * PERSPECTIVAL GAP:
 *   Student athletes experience the rules as a snare because they are trapped with limited outside options. The NCAA sees the rules as a rope that coordinates the athletic landscape. Universities view it as a tangled rope, because they benefit from revenue but are also constrained by compliance.
 *
 * DIRECTIONALITY LOGIC:
 *   The NCAA and universities benefit from the rules, leading to a low directionality value. Student athletes are targeted by the rules, resulting in a high directionality value. The analytical observer sees a mixed effect, resulting in an intermediate value.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    athlete_compensation_limits,
    'What is the optimal level of compensation for student-athletes that balances their economic rights with the amateurism model?',
    'Economic modeling, legal precedent, athlete surveys',
    'Determines whether the rules are a justified coordination mechanism or an exploitative snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(athlete_compensation_limits, preference, 'Level of athlete compensation').

omega_variable(
    ncaa_enforcement_effectiveness,
    'How effectively are NCAA eligibility rules enforced, and what are the consequences of non-compliance?',
    'Review of enforcement actions, legal cases, compliance audits',
    'Impacts the perceived fairness and legitimacy of the rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ncaa_enforcement_effectiveness, empirical, 'Effectiveness of rule enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ncaa_eligibility_rules, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ncaa_tr_t0, ncaa_eligibility_rules, theater_ratio, 0, 0.5).
narrative_ontology:measurement(ncaa_tr_t10, ncaa_eligibility_rules, theater_ratio, 10, 0.45).
narrative_ontology:measurement(ncaa_tr_t20, ncaa_eligibility_rules, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(ncaa_be_t0, ncaa_eligibility_rules, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(ncaa_be_t10, ncaa_eligibility_rules, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(ncaa_be_t20, ncaa_eligibility_rules, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ncaa_eligibility_rules, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
