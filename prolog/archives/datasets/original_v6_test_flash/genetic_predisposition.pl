% ============================================================================
% CONSTRAINT STORY: genetic_predisposition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genetic_predisposition, []).

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
 *   constraint_id: genetic_predisposition
 *   human_readable: Socio-Economic Response to Genetic Predisposition
 *   domain: technological/social/economic
 *
 * SUMMARY:
 *   This constraint models not the biological fact of genetic predisposition
 *   (a Mountain), but the socio-economic system built upon it. It arises from
 *   the availability of genetic information and its potential use in
 *   healthcare, insurance, employment, and other sectors. The system exhibits
 *   mixed effects, offering potential benefits in personalized medicine and
 *   disease prevention but also creating risks of discrimination and social
 *   inequality. The constraint is actively enforced by various actors,
 *   including genetic testing companies, insurance companies, and
 *   personalized medicine providers, each with their own incentives and
 *   perspectives.
 *
 * KEY AGENTS:
 *   - Individuals with Identified Genetic Risks: Primary victim (powerless/trapped) - Face potential discrimination and limited opportunities.
 *   - Genetic Testing Companies: Primary beneficiary (institutional/arbitrage) - Profit from the sale of genetic information.
 *   - Insurance Companies: Secondary beneficiary (powerful/constrained) - Use genetic information to assess risk and adjust premiums.
 *   - Personalized Medicine Providers: Mixed role (moderate/mobile) - Benefit from tailored treatments but face limitations and costs.
 *   - Social Mobility: Impacted victim (powerless/constrained) - Genetic information may further limit opportunities for those already disadvantaged.
 *   - Equal Opportunity: Impacted victim (powerless/constrained) - Genetic information may create new forms of discrimination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genetic_predisposition, 0.55).
domain_priors:suppression_score(genetic_predisposition, 0.45).
domain_priors:theater_ratio(genetic_predisposition, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genetic_predisposition, extractiveness, 0.55).
narrative_ontology:constraint_metric(genetic_predisposition, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(genetic_predisposition, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genetic_predisposition, tangled_rope).
narrative_ontology:human_readable(genetic_predisposition, "Socio-Economic Response to Genetic Predisposition").
narrative_ontology:topic_domain(genetic_predisposition, "technological/social/economic").

domain_priors:requires_active_enforcement(genetic_predisposition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genetic_predisposition, genetic_testing_companies).
narrative_ontology:constraint_beneficiary(genetic_predisposition, insurance_companies).
narrative_ontology:constraint_beneficiary(genetic_predisposition, personalized_medicine_providers).
narrative_ontology:constraint_victim(genetic_predisposition, individuals_with_identified_genetic_risks).
narrative_ontology:constraint_victim(genetic_predisposition, social_mobility).
narrative_ontology:constraint_victim(genetic_predisposition, equal_opportunity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of individuals who, upon learning of their genetic predispositions, face increased insurance premiums, limited employment opportunities, and social stigma. They are largely trapped, lacking the power to alter their genetic makeup or escape the consequences of genetic discrimination.
constraint_indexing:constraint_classification(genetic_predisposition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of genetic testing companies that benefit from the demand for genetic information. They experience the constraint as coordination, as the system facilitates the provision of genetic data, enabling them to expand their business and influence in healthcare and other sectors. They arbitrage the information asymmetry.
constraint_indexing:constraint_classification(genetic_predisposition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of insurance companies that can use genetic information to assess risk and adjust premiums. They benefit from risk stratification but are also constrained by regulations and public scrutiny, facing reputational risks if perceived as engaging in genetic discrimination. They experience extraction and coordination.
constraint_indexing:constraint_classification(genetic_predisposition, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of personalized medicine providers who can tailor treatments based on genetic profiles. They benefit from the increased demand for individualized healthcare but are also constrained by the cost and complexity of genetic testing and the limitations of current personalized medicine approaches. They can move between treatment options and specialize, representing moderate extraction.
constraint_indexing:constraint_classification(genetic_predisposition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% Perspective of an analytical observer who sees the mixed effects of genetic predisposition, including potential benefits in healthcare and potential risks of social inequality and discrimination. They aim to understand the underlying structural dynamics and long-term consequences.
constraint_indexing:constraint_classification(genetic_predisposition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_predisposition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genetic_predisposition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_predisposition, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(genetic_predisposition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genetic_predisposition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The system extracts value from individuals with identified genetic risks through higher insurance premiums, limited employment opportunities, and social stigma. This extraction is not total, as regulations and ethical considerations may mitigate the extent of discrimination. Suppression (0.45): Moderate. The system suppresses opportunities for individuals with genetic predispositions by creating barriers to insurance, employment, and social mobility. However, the system does not completely eliminate these opportunities, as regulations and ethical norms may provide some protection. Theater Ratio (0.30): Low. The system is primarily driven by economic and medical considerations, with limited performative aspects. The emphasis is on using genetic information to assess risk and tailor treatments, rather than engaging in symbolic gestures or rituals.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the conflicting interests of different actors in the system. Genetic testing companies and insurance companies benefit from the availability of genetic information, while individuals with identified genetic risks bear the costs of discrimination and limited opportunities. Personalized medicine providers occupy a mixed role, benefiting from tailored treatments but also facing limitations and costs. The analytical observer sees the mixed effects of genetic predisposition, including potential benefits and risks.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality of the constraint reflects the structural relationships between different actors in the system. Genetic testing companies are beneficiaries because they profit from the sale of genetic information. Insurance companies are beneficiaries because they can use genetic information to assess risk and adjust premiums. Individuals with identified genetic risks are victims because they face potential discrimination and limited opportunities.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification reflects the mixed effects of the system. The system offers potential benefits in personalized medicine and disease prevention but also creates risks of discrimination and social inequality. The mandate trophy is resolved by recognizing that the system is not purely extractive or purely beneficial but rather a complex combination of both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    predictive_accuracy_threshold,
    'What level of predictive accuracy is required to justify socio-economic interventions based on genetic predispositions?',
    'Statistical analysis of the relationship between genetic markers and disease outcomes, cost-benefit analysis of interventions.',
    'Determines the extent to which genetic information can be used for risk assessment and personalized medicine. If too low, interventions may be ineffective or harmful; if too high, valuable opportunities for prevention and treatment may be missed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predictive_accuracy_threshold, empirical, 'Predictive accuracy required for socio-economic interventions.').

omega_variable(
    genetic_discrimination_regulation,
    'What is the appropriate level of regulation to prevent genetic discrimination without hindering innovation in healthcare?',
    'Comparative analysis of different regulatory approaches, stakeholder consultation, ethical analysis.',
    'Shapes the balance between individual rights and public health. Insufficient regulation may lead to genetic discrimination; excessive regulation may stifle research and development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genetic_discrimination_regulation, preference, 'Regulation level for preventing genetic discrimination.').

omega_variable(
    social_equity_impact,
    'How can we mitigate the potential for genetic information to exacerbate existing social inequalities?',
    'Social science research, policy analysis, community engagement.',
    'Determines the degree to which genetic information contributes to or reduces social inequality. If unaddressed, genetic information may amplify existing disparities; if addressed effectively, it may promote greater equity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_equity_impact, conceptual, 'Mitigation of genetic information''s impact on social inequalities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genetic_predisposition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genetic_predisposition, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gene_tr_t5, genetic_predisposition, theater_ratio, 5, 0.2).
narrative_ontology:measurement(gene_tr_t10, genetic_predisposition, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genetic_predisposition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gene_be_t5, genetic_predisposition, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(gene_be_t10, genetic_predisposition, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genetic_predisposition, information_standard).
narrative_ontology:affects_constraint(genetic_predisposition, healthcare_access).
narrative_ontology:affects_constraint(genetic_predisposition, employment_discrimination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
