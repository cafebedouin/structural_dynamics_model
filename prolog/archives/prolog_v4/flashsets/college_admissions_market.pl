% ============================================================================
% CONSTRAINT STORY: college_admissions_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_college_admissions_market, []).

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
 *   constraint_id: college_admissions_market
 *   human_readable: The US Elite College Admissions Market
 *   domain: social/economic
 *
 * SUMMARY:
 *   The US elite college admissions system functions as a high-stakes,
 *   many-to-one matching market. While it ostensibly aims to identify and
 *   select the most promising students, it also generates significant
 *   extraction and suppression, particularly for low-income and middle-class
 *   applicants. Elite colleges benefit from the intense competition for
 *   admission, while test-prep companies capitalize on anxieties surrounding
 *   standardized tests. The system reinforces social hierarchies and
 *   perpetuates inequalities in access to higher education.
 *
 * KEY AGENTS:
 *   - Low-Income Applicants: Primary target (powerless/trapped) – lack resources to navigate the system.
 *   - Middle-Class Applicants: Secondary target (moderate/constrained) – squeezed by high costs and competition.
 *   - Elite Colleges: Primary beneficiary (institutional/arbitrage) – maintain prestige and attract resources.
 *   - College Administrators: Maintain complex bureaucratic processes (powerful/constrained)
 *   - Test Prep Companies: Secondary beneficiary (powerful/arbitrage) – profit from demand for higher scores.
 *   - Public Education System: Collateral victim (moderate/constrained) - resources diverted to college prep instead of broader educational goals.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(college_admissions_market, 0.65).
domain_priors:suppression_score(college_admissions_market, 0.75).
domain_priors:theater_ratio(college_admissions_market, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(college_admissions_market, extractiveness, 0.65).
narrative_ontology:constraint_metric(college_admissions_market, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(college_admissions_market, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(college_admissions_market, tangled_rope).
narrative_ontology:human_readable(college_admissions_market, "The US Elite College Admissions Market").
narrative_ontology:topic_domain(college_admissions_market, "social/economic").

domain_priors:requires_active_enforcement(college_admissions_market).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(college_admissions_market, elite_colleges).
narrative_ontology:constraint_beneficiary(college_admissions_market, college_administrators).
narrative_ontology:constraint_beneficiary(college_admissions_market, test_prep_companies).
narrative_ontology:constraint_victim(college_admissions_market, low_income_applicants).
narrative_ontology:constraint_victim(college_admissions_market, middle_class_applicants).
narrative_ontology:constraint_victim(college_admissions_market, public_education_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Low-income applicants often lack the resources to navigate the complex admissions process, making them vulnerable to extraction. They have limited exit options due to financial constraints and inadequate access to quality education and test preparation.
constraint_indexing:constraint_classification(college_admissions_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Middle-class applicants face a mixed situation. They have some resources but are often squeezed by the high costs of test preparation, application fees, and the need to compete with wealthier applicants. Their exit options are constrained by social pressures and the perceived benefits of attending elite institutions.
constraint_indexing:constraint_classification(college_admissions_market, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Elite colleges benefit from the high demand for admission, allowing them to select the most desirable students and maintain their prestige. They have arbitrage opportunities by leveraging their brand and endowment to attract donations and research funding.
constraint_indexing:constraint_classification(college_admissions_market, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% College administrators are powerful, and benefit from the complex admissions process through increased budgets, staff and influence, but are simultaneously constrained by ranking pressures.
constraint_indexing:constraint_classification(college_admissions_market, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Test prep companies benefit directly from the competitive admissions landscape. They arbitrage the demand for higher scores into revenue.
constraint_indexing:constraint_classification(college_admissions_market, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a broad analytical perspective, the elite college admissions market functions as a tangled rope: it coordinates the matching of students to colleges but also extracts resources, perpetuates inequalities, and reinforces social hierarchies.
constraint_indexing:constraint_classification(college_admissions_market, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(college_admissions_market_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(college_admissions_market, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(college_admissions_market, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(college_admissions_market, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(college_admissions_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The system extracts significant resources from applicants in the form of application fees, test preparation costs, and opportunity costs. It also extracts social capital from the public education system by diverting resources to college preparation. Suppression (0.75): High. The admissions process suppresses alternative pathways to success by creating a strong incentive to attend elite institutions. It also suppresses the potential of low-income and middle-class applicants by favoring wealthier applicants with access to better education and test preparation. Theater ratio (0.40): Moderate. While there is some functional matching of students to colleges, the admissions process also involves a significant amount of performative activity, such as elaborate application essays and extracurricular activities designed to impress admissions committees.
 *
 * PERSPECTIVAL GAP:
 *   The elite colleges view the admissions process as a coordination mechanism (rope) for selecting talented students and maintaining their prestige. Test-prep companies see it as an opportunity to arbitrage demand for higher scores into revenue. In contrast, low-income applicants experience it as a snare, trapping them in a cycle of disadvantage. Middle-class applicants see it as a tangled rope, offering some opportunities but also extracting significant resources and creating intense pressure. The analytical observer sees the overall system as a tangled rope, coordinating matching while also extracting resources and reinforcing inequality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the power and exit options of each agent. Elite colleges and test-prep companies have high power and arbitrage options, giving them low directionality values and classifying their perspectives as rope. Low-income applicants have low power and are trapped, giving them high directionality values and classifying their perspective as a snare. Middle-class applicants have moderate power and are constrained, resulting in a tangled rope classification. The analytical observer has high power to analyze the system and has an analytical exit, thus they see the tangled rope. College Administrators are simultaneously powerful due to the number of applications that they receive, but are contrained by external rankings and internal demands, and thus their perspective is also tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The system could be misconstrued as a pure snare, but in fact it provides a coordination mechanism (matching of students to colleges). The tangled rope classification acknowledges both the coordination and the extraction. The mandatrophy is resolved by acknowledging that some agents (elite colleges, test-prep companies) benefit from the system, while others (low-income applicants) are harmed by it. The analytical perspective accounts for both sides of the equation, recognizing that the system coordinates matching while simultaneously extracting resources and reinforcing inequality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merit_definition,
    'What constitutes ''merit'' in college admissions?',
    'Sociological research on the correlation between admissions criteria and long-term success; analysis of alternative admissions models.',
    'If ''merit'' is redefined to prioritize factors other than test scores and GPA, the extraction from low-income applicants might decrease. If ''merit'' remains narrowly defined, the system will continue to favor privileged applicants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merit_definition, conceptual, 'Definition of merit').

omega_variable(
    affirmative_action_impact,
    'What is the impact of affirmative action policies on equity and diversity in elite colleges?',
    'Statistical analysis of admissions data before and after the implementation or repeal of affirmative action policies; qualitative research on the experiences of underrepresented students.',
    'If affirmative action policies are found to be effective, their continuation or expansion might mitigate the extractive aspects of the admissions market. If they are found to be ineffective or harmful, alternative policies might be needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(affirmative_action_impact, empirical, 'Impact of affirmative action').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(college_admissions_market, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coll_tr_t0, college_admissions_market, theater_ratio, 0, 0.3).
narrative_ontology:measurement(coll_tr_t10, college_admissions_market, theater_ratio, 10, 0.35).
narrative_ontology:measurement(coll_tr_t20, college_admissions_market, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(coll_be_t0, college_admissions_market, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(coll_be_t10, college_admissions_market, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(coll_be_t20, college_admissions_market, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(college_admissions_market, resource_allocation).
narrative_ontology:affects_constraint(college_admissions_market, wealth_inequality).
narrative_ontology:affects_constraint(college_admissions_market, educational_inequality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
