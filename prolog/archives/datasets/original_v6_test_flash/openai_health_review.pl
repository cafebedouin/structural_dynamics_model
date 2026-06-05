% ============================================================================
% CONSTRAINT STORY: openai_health_review
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openai_health_review, []).

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
 *   constraint_id: openai_health_review
 *   human_readable: OpenAI ChatGPT Health Review Service
 *   domain: technological/economic
 *
 * SUMMARY:
 *   OpenAI's ChatGPT Health Review Service offers personalized diet and
 *   lifestyle recommendations based on users' health test results. This
 *   service presents a complex interplay of benefits and risks, acting as a
 *   tangled rope. While it has the potential to democratize access to
 *   personalized health information, it also raises concerns about data
 *   privacy, algorithmic bias, and the potential for over-reliance on
 *   AI-driven health advice.
 *
 * KEY AGENTS:
 *   - OpenAI: Primary beneficiary (institutional/arbitrage) - Gains revenue, data, and brand enhancement.
 *   - Patients: Primary victim (powerless/trapped) - Potential for misdiagnosis or inadequate treatment due to over-reliance or data bias.
 *   - Independent Health Professionals: Secondary victim (moderate/constrained) - Face increased competition.
 *   - Early Adopters: Beneficiary (moderate/mobile) - Tech-savvy individuals benefit from convenience and personalized recommendations.
 *   - Analytical Observer: Sees the system as a tangled rope (analytical/analytical) - Balances the potential benefits with the risks of extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_health_review, 0.55).
domain_priors:suppression_score(openai_health_review, 0.4).
domain_priors:theater_ratio(openai_health_review, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_health_review, extractiveness, 0.55).
narrative_ontology:constraint_metric(openai_health_review, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(openai_health_review, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openai_health_review, tangled_rope).
narrative_ontology:human_readable(openai_health_review, "OpenAI ChatGPT Health Review Service").
narrative_ontology:topic_domain(openai_health_review, "technological/economic").

domain_priors:requires_active_enforcement(openai_health_review).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openai_health_review, openai).
narrative_ontology:constraint_beneficiary(openai_health_review, early_adopters).
narrative_ontology:constraint_victim(openai_health_review, patients).
narrative_ontology:constraint_victim(openai_health_review, independent_health_professionals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Patients with limited access to healthcare or health literacy may become overly reliant on the service, potentially leading to misdiagnosis or inadequate treatment. They lack the power to challenge or verify the service's recommendations.
constraint_indexing:constraint_classification(openai_health_review, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Small practices and independent health professionals may face increased competition, potentially impacting their patient base and revenue. They are constrained by their resources and established practices.
constraint_indexing:constraint_classification(openai_health_review, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% OpenAI benefits from the service through increased revenue, data collection, and enhanced brand reputation. They can arbitrage the data for future developments.
constraint_indexing:constraint_classification(openai_health_review, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Tech-savvy individuals who are early adopters of the service may experience benefits from the personalized recommendations and convenience. They have the option to switch to other services if they are not satisfied.
constraint_indexing:constraint_classification(openai_health_review, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% An analytical observer sees that while there are benefits to personalized recommendations, the lack of transparency and potential for bias create a tangled rope, extracting value from patients while also offering some benefit.
constraint_indexing:constraint_classification(openai_health_review, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_health_review_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openai_health_review, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openai_health_review, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openai_health_review, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(openai_health_review_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is rated at 0.55 because OpenAI extracts value from user data and the service could potentially misinform patients. The service suppresses some aspects of traditional medical advice, rated at 0.4. Theater ratio is low because recommendations are data driven but could potentially be gamed.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives differ significantly depending on the stakeholder. OpenAI sees a coordination mechanism (rope), while patients may feel trapped in a snare. Independent health professionals experience a mixed situation (tangled rope), while early adopters see a valuable tool.
 *
 * DIRECTIONALITY LOGIC:
 *   OpenAI benefits directly from the service through revenue and data collection, resulting in a beneficiary relationship. Patients, particularly those with limited access to healthcare, may be vulnerable to misdiagnosis or inadequate treatment, leading to a victim relationship. Independent health professionals face increased competition and reduced patient base, also a victim relationship.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_bias_risk,
    'To what extent does the training data used by the LLM contain biases that could lead to inaccurate or discriminatory health recommendations?',
    'Detailed analysis of the training data and rigorous testing of the service across diverse populations.',
    'If significant bias exists, the service could reinforce existing health disparities. If bias is minimal, the service could potentially democratize access to personalized health advice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_bias_risk, empirical, 'Quantify the risk of bias in the LLM''s training data.').

omega_variable(
    over_reliance_factor,
    'How likely are individuals to become overly reliant on the service, neglecting traditional medical advice or ignoring concerning symptoms?',
    'Surveys and behavioral studies to assess the impact of the service on patient behavior and decision-making.',
    'High reliance could lead to delayed diagnoses and poorer health outcomes. Low reliance suggests the service is being used as a complementary tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(over_reliance_factor, empirical, 'Determine the potential for over-reliance on the service.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openai_health_review, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(open_tr_t0, openai_health_review, theater_ratio, 0, 0.2).
narrative_ontology:measurement(open_tr_t5, openai_health_review, theater_ratio, 5, 0.3).
narrative_ontology:measurement(open_tr_t10, openai_health_review, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(open_be_t0, openai_health_review, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(open_be_t5, openai_health_review, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(open_be_t10, openai_health_review, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openai_health_review, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
