% ============================================================================
% CONSTRAINT STORY: uk_student_visa_dependents
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_student_visa_dependents, []).

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
 *   constraint_id: uk_student_visa_dependents
 *   human_readable: UK policy restricting dependents of international students
 *   domain: political/economic
 *
 * SUMMARY:
 *   In an effort to reduce net migration figures, the UK government
 *   implemented a policy preventing international students on taught
 *   postgraduate courses (like one-year master's degrees) from bringing
 *   family members (dependents) with them. This policy disproportionately
 *   affects students from certain regions and creates significant hardship
 *   for affected families.
 *
 * KEY AGENTS:
 *   - International Students' Dependents: Primary victim (powerless/trapped) - bears the direct cost of family separation.
 *   - International Students: Secondary victim (moderate/constrained) - experiences financial and emotional strain.
 *   - UK Universities: Primary beneficiary (institutional/arbitrage) - benefit from continued enrollment of international students.
 *   - UK Government: Secondary beneficiary (institutional/constrained) - benefits from perceived control of immigration figures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_student_visa_dependents, 0.65).
domain_priors:suppression_score(uk_student_visa_dependents, 0.7).
domain_priors:theater_ratio(uk_student_visa_dependents, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_student_visa_dependents, extractiveness, 0.65).
narrative_ontology:constraint_metric(uk_student_visa_dependents, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(uk_student_visa_dependents, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_student_visa_dependents, snare).
narrative_ontology:human_readable(uk_student_visa_dependents, "UK policy restricting dependents of international students").
narrative_ontology:topic_domain(uk_student_visa_dependents, "political/economic").

domain_priors:requires_active_enforcement(uk_student_visa_dependents).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_student_visa_dependents, uk_government).
narrative_ontology:constraint_beneficiary(uk_student_visa_dependents, uk_universities).
narrative_ontology:constraint_victim(uk_student_visa_dependents, international_students_dependents).
narrative_ontology:constraint_victim(uk_student_visa_dependents, international_students).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: International Students' Dependents (Snare) - Dependents are directly impacted, often forced to separate from their family unit due to the policy, with limited exit options.
constraint_indexing:constraint_classification(uk_student_visa_dependents, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: International Students (Tangled Rope) - Students face higher financial and emotional burdens due to separation from family, limiting their opportunities, but they also benefit from accessing UK education and future job opportunities, creating mixed extraction and coordination.
constraint_indexing:constraint_classification(uk_student_visa_dependents, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: UK Universities (Rope) - Universities benefit from maintaining high international student enrollment which supports their financial position. The policy is a coordination mechanism to maintain access to this pool of students while potentially appeasing public opinion on immigration.
constraint_indexing:constraint_classification(uk_student_visa_dependents, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4: UK Government (Tangled Rope) - The government benefits politically from appearing to control immigration figures, but the policy may harm the UK's reputation as a welcoming destination for international talent and damage the economy and university funding in the long term. Therefore there is both extraction and coordination.
constraint_indexing:constraint_classification(uk_student_visa_dependents, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 5: Analytical Observer (Tangled Rope) - The analytical observer sees a tangle of motivations and outcomes, with the policy acting as a snare for families and students while providing some benefits for the UK government and universities.
constraint_indexing:constraint_classification(uk_student_visa_dependents, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_student_visa_dependents_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_student_visa_dependents, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_student_visa_dependents, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_student_visa_dependents, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(uk_student_visa_dependents_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The policy imposes considerable financial and emotional strain on international students and their families. Suppression (0.70): High. Affected individuals have limited options to circumvent the policy. Theater ratio (0.30): Low. While there is public rhetoric about controlling immigration, the policy's primary effect is extracting resources from students. Low performative content since the actual impact has been low.
 *
 * PERSPECTIVAL GAP:
 *   International students and their dependents experience the policy as a snare, while the UK government and universities see it as a tangled rope that has both positive and negative effects. The analytical observer recognizes the extractiveness inherent in the policy.
 *
 * DIRECTIONALITY LOGIC:
 *   The policy extracts resources (emotional well-being, finances) from international students and their families while providing benefits (reduced migration figures, continued tuition revenue) to the UK government and universities. The perspectives reflect these differential experiences. The directionality values reflect the power dynamics and exit options of each agent.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_economic_impact,
    'What is the long-term economic impact of reduced international student enrollment on UK universities and the broader economy?',
    'Longitudinal study tracking enrollment rates, university funding, and contributions of international graduates to the UK economy.',
    'If negative impact is significant: Government perspective shifts to snare or piton. If negligible: Government perspective remains tangled rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_economic_impact, empirical, 'Economic impact assessment').

omega_variable(
    reputational_damage,
    'To what extent does the policy damage the UK''s reputation as a welcoming destination for international talent?',
    'Surveys of prospective international students and analysis of application trends from different countries.',
    'If reputational damage is high: Government perspective shifts towards snare. If low: Government perspective remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reputational_damage, empirical, 'Reputational impact survey').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_student_visa_dependents, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uk_s_tr_t0, uk_student_visa_dependents, theater_ratio, 0, 0.2).
narrative_ontology:measurement(uk_s_tr_t1, uk_student_visa_dependents, theater_ratio, 1, 0.25).
narrative_ontology:measurement(uk_s_tr_t2, uk_student_visa_dependents, theater_ratio, 2, 0.3).

% Extraction over time
narrative_ontology:measurement(uk_s_be_t0, uk_student_visa_dependents, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(uk_s_be_t1, uk_student_visa_dependents, base_extractiveness, 1, 0.6).
narrative_ontology:measurement(uk_s_be_t2, uk_student_visa_dependents, base_extractiveness, 2, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
