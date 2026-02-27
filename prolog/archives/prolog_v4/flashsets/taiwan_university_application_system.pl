% ============================================================================
% CONSTRAINT STORY: taiwan_university_application_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_university_application_system, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: taiwan_university_application_system
 *   human_readable: Taiwan's Application-Based University Admission System
 *   domain: social/economic
 *
 * SUMMARY:
 *   Taiwan's 'Application-Based Admission' (個人申請) system for university
 *   entrance was intended to provide a holistic assessment of students beyond
 *   standardized tests. However, it has created a complex interplay of
 *   benefits and burdens, leading to a system that advantages students from
 *   well-resourced families, while disadvantaging those from under-resourced
 *   backgrounds. The emphasis on application materials and extracurricular
 *   activities creates opportunities for cram schools and exacerbates
 *   existing inequalities in access to resources and guidance. This is a
 *   tangled rope.
 *
 * KEY AGENTS:
 *   - Students from Under-Resourced Families: Primary victims (powerless/trapped) - face barriers in preparing application materials due to limited resources.
 *   - Students from Well-Resourced Families: Moderate (moderate/constrained) - benefit from access to resources but still constrained by competition.
 *   - Elite Universities: Primary beneficiaries (institutional/arbitrage) - maintain selectivity and reputation.
 *   - Cram School Industry: Secondary beneficiaries (organized/constrained) - opportunities from specialized services.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_university_application_system, 0.55).
domain_priors:suppression_score(taiwan_university_application_system, 0.6).
domain_priors:theater_ratio(taiwan_university_application_system, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_university_application_system, extractiveness, 0.55).
narrative_ontology:constraint_metric(taiwan_university_application_system, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(taiwan_university_application_system, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_university_application_system, tangled_rope).
narrative_ontology:human_readable(taiwan_university_application_system, "Taiwan's Application-Based University Admission System").
narrative_ontology:topic_domain(taiwan_university_application_system, "social/economic").

domain_priors:requires_active_enforcement(taiwan_university_application_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_university_application_system, elite_universities).
narrative_ontology:constraint_beneficiary(taiwan_university_application_system, students_from_well_resourced_families).
narrative_ontology:constraint_beneficiary(taiwan_university_application_system, cram_school_industry).
narrative_ontology:constraint_victim(taiwan_university_application_system, students_from_under_resourced_families).
narrative_ontology:constraint_victim(taiwan_university_application_system, equal_opportunity_in_education).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Students from under-resourced families face significant barriers in preparing application materials (essays, extracurricular activities, portfolio) due to limited access to resources and guidance. They are trapped within a system that structurally disadvantages them, diminishing their chances of university admission compared to their peers from well-resourced backgrounds. The system extracts their potential due to the inequalities it perpetuates.
constraint_indexing:constraint_classification(taiwan_university_application_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Students from well-resourced families benefit from access to cram schools, extracurricular activity programs, and essay-writing services, which enhances their application profiles. While they also face pressure to perform well and secure university admission, they have the advantage of social and economic capital. This group is constrained by the need to compete but also benefits from the system's structure. The constrained exist options means their d is closer to victim than benefactor.
constraint_indexing:constraint_classification(taiwan_university_application_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Elite universities benefit from the application-based system by having a larger pool of applicants to select from, allowing them to maintain their selectivity and reputation. They are able to arbitrage this position of advantage. The ranking of the university is then maintained in the long term.
constraint_indexing:constraint_classification(taiwan_university_application_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The cram school industry (補習班) is a beneficiary due to the increased emphasis on application materials and extracurricular activities, creates an opportunity for cram schools to offer specialized services that prepare students for university applications. The constrained exit reflects the continued need to adapt to regulation, curriculum changes, and the competitive pressures of the sector.
constraint_indexing:constraint_classification(taiwan_university_application_system, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the system represents a tangled rope, as it has elements of both coordination (holistic assessment of students) and extraction (disproportionate advantage for students from well-resourced families). The global scope reflects that the observer accounts for relative standing of the universities.
constraint_indexing:constraint_classification(taiwan_university_application_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_university_application_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_university_application_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_university_application_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_university_application_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(taiwan_university_application_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The system has a moderate extractiveness (0.55), as it disproportionately advantages students from well-resourced families and suppresses opportunity for under-resourced families. The suppression is high (0.60) because the system does not offer mechanisms to compensate for the unequal resources between families, making those from poor backgrounds trapped within the system. The theater is medium (0.40), as the stated purpose is holistic assessment but the system performance is highly skewed.
 *
 * PERSPECTIVAL GAP:
 *   Students from under-resourced families experience the system as a snare, as they are structurally disadvantaged and have limited exit options. Students from well-resourced families experience the system as a tangled rope because they benefit from their access to resources, but are still constrained to compete with each other. Elite universities see the system as a rope because it helps them maintain their selectivity and ranking. The cram school industry experience it as a tangled rope because they must still adapt to regulation and curriculum changes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position, power, and exit options. Elite universities can arbitrage (low d), families with higher resource levels are constrained (medium d), and families with lower levels of resource are trapped (high d). Each perspective's classification reflects their experienced level of extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    holistic_assessment_effectiveness,
    'To what extent does the application-based system genuinely assess students'' holistic qualities beyond standardized test scores, and to what extent does it merely reflect socioeconomic advantages?',
    'Empirical studies comparing the long-term academic and career outcomes of students admitted through the application-based system versus those admitted through traditional test-based methods, controlling for socioeconomic background.',
    'If the system genuinely assesses holistic qualities, it may be classified as a rope. If it merely reflects socioeconomic advantages, it will be confirmed as a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holistic_assessment_effectiveness, empirical, 'The true effectiveness of holistic assessment by the system.').

omega_variable(
    equal_opportunity_interventions,
    'What interventions can effectively mitigate the unequal access to resources and guidance for students from under-resourced families, ensuring a more level playing field in the application process?',
    'Pilot programs providing free or subsidized application support services (essay writing workshops, extracurricular activity programs) to students from under-resourced families, followed by rigorous evaluation of their impact on university admission rates.',
    'If effective interventions are identified, it may be possible to reclassify the system as a scaffold. If no effective interventions are found, the system will be classified as a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equal_opportunity_interventions, empirical, 'The effectiveness of interventions to address inequalities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_university_application_system, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taiw_tr_t0, taiwan_university_application_system, theater_ratio, 0, 0.3).
narrative_ontology:measurement(taiw_tr_t5, taiwan_university_application_system, theater_ratio, 5, 0.35).
narrative_ontology:measurement(taiw_tr_t10, taiwan_university_application_system, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(taiw_be_t0, taiwan_university_application_system, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(taiw_be_t5, taiwan_university_application_system, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(taiw_be_t10, taiwan_university_application_system, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
