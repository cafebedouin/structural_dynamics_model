% ============================================================================
% CONSTRAINT STORY: unconditional_university_offers_uk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_university_offers_uk, []).

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
 *   constraint_id: unconditional_university_offers_uk
 *   human_readable: Use of Unconditional Offers in UK University Admissions
 *   domain: economic/social
 *
 * SUMMARY:
 *   Following the removal of student number caps in 2015, UK universities
 *   increasingly used "unconditional offers" to secure student admissions in
 *   a more competitive market. This practice involves offering students a
 *   place regardless of their final exam results, creating a strong incentive
 *   for them to accept the offer early. This constraint story explores the
 *   structural dynamics and indexical classifications associated with this
 *   practice.
 *
 * KEY AGENTS:
 *   - UK Universities: Primary beneficiary (institutional/arbitrage) - benefit from increased enrollment and financial stability.
 *   - Prospective Students: Primary victim (powerless/trapped) - potentially pressured to accept offers early, limiting their options.
 *   - University Admissions Officers: Moderate influence, constrained by market pressure but seeking higher academic standing
 *   - Overall Quality of Education: Secondary victim (powerless/trapped) - potential decline in academic standards if universities prioritize enrollment over student quality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_university_offers_uk, 0.55).
domain_priors:suppression_score(unconditional_university_offers_uk, 0.45).
domain_priors:theater_ratio(unconditional_university_offers_uk, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_university_offers_uk, extractiveness, 0.55).
narrative_ontology:constraint_metric(unconditional_university_offers_uk, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(unconditional_university_offers_uk, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_university_offers_uk, tangled_rope).
narrative_ontology:human_readable(unconditional_university_offers_uk, "Use of Unconditional Offers in UK University Admissions").
narrative_ontology:topic_domain(unconditional_university_offers_uk, "economic/social").

domain_priors:requires_active_enforcement(unconditional_university_offers_uk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_university_offers_uk, uk_universities).
narrative_ontology:constraint_victim(unconditional_university_offers_uk, prospective_students).
narrative_ontology:constraint_victim(unconditional_university_offers_uk, overall_quality_of_education).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of a student feeling pressured to accept an unconditional offer, potentially limiting their options and leading to a suboptimal educational path. Trapped in the sense that they may feel they cannot refuse a guaranteed place.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of a university admissions officer, balancing the need to meet enrollment targets with concerns about the overall academic profile of the student body. Constrained by market pressures but also benefits from increased enrollment.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of UK universities benefiting from increased enrollment and financial stability due to unconditional offers, acting as a coordination mechanism to secure students in a competitive market. Arbitrage in the sense that they are exploiting the competitive environment.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Analytical perspective observing the long-term effects on educational standards and student outcomes. Sees a tangled rope as it provides universities with revenue but may lower overall quality of graduates.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_university_offers_uk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unconditional_university_offers_uk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unconditional_university_offers_uk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_university_offers_uk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_university_offers_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55 - The system extracts from prospective students by limiting their choices and potentially leading them to accept suboptimal educational paths. Universities extract by prioritizing quantity over quality, which may lead to an overall decline in education standards. Suppression: 0.45 - The system suppresses student's choice, as well as suppressing focus on results in A levels. Students can see an unconditional offer as the 'easy route' therefore putting in less effort with A Level exams.
 *
 * PERSPECTIVAL GAP:
 *   Students feel trapped by the offer, while the universities see it as a way to secure their intakes. Analysts see the long-term implications. Some universities are now backtracking, but this is difficult to do due to financial pressures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural positions of the agents. Universities benefit from increased enrollment (low d), while students potentially bear the cost of limited choice (high d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_impact_on_educational_standards,
    'What is the long-term impact of unconditional offers on the quality of education and student outcomes?',
    'Longitudinal studies tracking student performance and career outcomes for students admitted via unconditional offers versus traditional offers.',
    'If negative impact is significant: classification shifts towards a snare. If minimal impact: classification remains tangled rope or potentially shifts towards a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_impact_on_educational_standards, empirical, 'Long-term effect on educational standards.').

omega_variable(
    student_choice_vs_pressure,
    'To what extent are students genuinely exercising free choice when accepting unconditional offers, versus feeling pressured to accept the first offer they receive?',
    'Surveys and interviews with students to assess their decision-making process when faced with unconditional offers.',
    'If students feel significantly pressured: classification shifts towards a snare from the student perspective. If students feel empowered: classification remains tangled rope or potentially shifts towards a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(student_choice_vs_pressure, empirical, 'Genuine student choice vs. pressure to accept.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_university_offers_uk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_university_offers_uk, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unco_tr_t5, unconditional_university_offers_uk, theater_ratio, 5, 0.3).
narrative_ontology:measurement(unco_tr_t10, unconditional_university_offers_uk, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_university_offers_uk, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unco_be_t5, unconditional_university_offers_uk, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(unco_be_t10, unconditional_university_offers_uk, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_university_offers_uk, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
