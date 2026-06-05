% ============================================================================
% CONSTRAINT STORY: uneven_risk_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uneven_risk_distribution, []).

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
 *   constraint_id: uneven_risk_distribution
 *   human_readable: Uneven Distribution of Modifiable Health Risk Factors
 *   domain: public_health/epidemiology
 *
 * SUMMARY:
 *   The persistent uneven distribution of modifiable health risk factors
 *   across different populations presents a complex challenge for public
 *   health. This constraint reflects the interplay of socioeconomic factors,
 *   access to resources, and the effectiveness of public health
 *   interventions. While healthcare providers and researchers benefit from
 *   addressing these issues, vulnerable populations bear the brunt of the
 *   consequences. This story analyzes the different perspectives and offers
 *   insight into potential omega variables that contribute to the complexity.
 *
 * KEY AGENTS:
 *   - Vulnerable Populations: Primary target (powerless/trapped) — bear the burden of higher risk factor prevalence due to systemic disadvantages.
 *   - Public Health Agencies: Moderate influence (moderate/constrained) — strive to address health disparities but are constrained by limited resources and political pressures.
 *   - Healthcare Providers: Beneficiary (institutional/arbitrage) — benefit from treating patients with modifiable health risk factors and can choose target populations.
 *   - Public Health Researchers: Beneficiary (institutional/arbitrage) — benefit from studying the distribution of modifiable health risk factors and can choose research questions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uneven_risk_distribution, 0.55).
domain_priors:suppression_score(uneven_risk_distribution, 0.4).
domain_priors:theater_ratio(uneven_risk_distribution, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uneven_risk_distribution, extractiveness, 0.55).
narrative_ontology:constraint_metric(uneven_risk_distribution, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(uneven_risk_distribution, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uneven_risk_distribution, tangled_rope).
narrative_ontology:human_readable(uneven_risk_distribution, "Uneven Distribution of Modifiable Health Risk Factors").
narrative_ontology:topic_domain(uneven_risk_distribution, "public_health/epidemiology").

domain_priors:requires_active_enforcement(uneven_risk_distribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uneven_risk_distribution, healthcare_providers).
narrative_ontology:constraint_beneficiary(uneven_risk_distribution, public_health_researchers).
narrative_ontology:constraint_victim(uneven_risk_distribution, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Vulnerable populations, often trapped by socioeconomic circumstances, bear the brunt of modifiable health risk factors. They have limited access to resources and face systemic barriers to adopting healthier behaviors.
constraint_indexing:constraint_classification(uneven_risk_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Public health agencies are constrained by budgets, political pressures, and the complexity of addressing social determinants of health. While they strive to reduce health disparities, their efforts are often met with limited success, creating a tangled web of interventions and unintended consequences. They benefit from identifying and characterizing these distributions, but the populations they are meant to serve often bear the costs of inaction.
constraint_indexing:constraint_classification(uneven_risk_distribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Healthcare providers benefit from treating patients with modifiable health risk factors. They have arbitrage in that they can choose which services to offer, and what populations to target with interventions. This perspective sees the distribution as an opportunity to provide needed services.
constraint_indexing:constraint_classification(uneven_risk_distribution, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Public health researchers benefit from studying the distribution of modifiable health risk factors. They have arbitrage in that they can choose which research questions to pursue, and what populations to target with interventions. This perspective sees the distribution as an opportunity to conduct research and publish findings.
constraint_indexing:constraint_classification(uneven_risk_distribution, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a high-level perspective, the uneven distribution of modifiable health risk factors reflects both coordination and extraction. It highlights the challenges of promoting health equity and the need for comprehensive, evidence-based interventions that address social determinants of health.
constraint_indexing:constraint_classification(uneven_risk_distribution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uneven_risk_distribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uneven_risk_distribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uneven_risk_distribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uneven_risk_distribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(uneven_risk_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is rated at 0.55 because there's a significant transfer of negative health outcomes to vulnerable populations (victim) while healthcare providers and researchers (beneficiaries) gain from addressing and studying these issues. Suppression (0.40) is moderate as some efforts are made to address these issues but systemic barriers persist, limiting true mobility. Theater ratio is 0.30 because interventions are not always effective in addressing the root causes of the disparities.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises because different actors experience this constraint differently. Vulnerable populations (snare) are trapped with limited ability to change their circumstances. Public health agencies (tangled_rope) face the challenges and benefits of addressing this distribution. Researchers and providers benefit from treating and studying these distributions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's power and exit options. Vulnerable populations are powerless and trapped resulting in a high d value. Public health researchers and healthcare providers have arbitrage and are thus beneficiaries with a low d value. Public Health Agencies are constrained resulting in a moderate d value.
 *
 * MANDATROPHY ANALYSIS:
 *   The distribution of risk factors is not purely extractive, as coordination mechanisms exist (public health interventions, research efforts). However, these mechanisms often fail to fully address the root causes, leading to a tangled rope situation where the benefits are unevenly distributed and the extraction remains significant for vulnerable populations. The presence of beneficiaries (researchers and healthcare) is balanced by the victims, thus clarifying the tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_determinants_impact,
    'What is the relative contribution of specific social determinants of health (e.g., poverty, education, housing) to the observed uneven distribution of modifiable risk factors?',
    'Longitudinal studies, quasi-experimental designs, and systems modeling to quantify the impact of various social determinants on health risk factors.',
    'Understanding the relative impact of social determinants will inform targeted interventions and policy changes to address root causes of health disparities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_determinants_impact, empirical, 'Impact of social determinants on risk factor distribution').

omega_variable(
    intervention_effectiveness_variability,
    'How does the effectiveness of interventions aimed at reducing modifiable risk factors vary across different populations and settings?',
    'Systematic reviews and meta-analyses of intervention studies, stratified by population characteristics and contextual factors.',
    'Identifying factors that moderate intervention effectiveness will improve the design and implementation of tailored interventions that are more likely to succeed in specific contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_effectiveness_variability, empirical, 'Variability in intervention effectiveness across populations').

omega_variable(
    policy_impact_assessment,
    'What are the unintended consequences of policies aimed at reducing modifiable risk factors (e.g., soda taxes, smoking bans)?',
    'Policy evaluations that assess the impact of policies on both intended and unintended outcomes, including health equity and economic consequences.',
    'Identifying unintended consequences will inform policy adjustments to maximize benefits and minimize harms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_impact_assessment, empirical, 'Unintended consequences of risk-reduction policies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uneven_risk_distribution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unev_tr_t0, uneven_risk_distribution, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unev_tr_t5, uneven_risk_distribution, theater_ratio, 5, 0.25).
narrative_ontology:measurement(unev_tr_t10, uneven_risk_distribution, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(unev_be_t0, uneven_risk_distribution, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(unev_be_t5, uneven_risk_distribution, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(unev_be_t10, uneven_risk_distribution, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uneven_risk_distribution, resource_allocation).
narrative_ontology:affects_constraint(uneven_risk_distribution, healthcare_access_disparities).
narrative_ontology:affects_constraint(uneven_risk_distribution, food_desert_entrapment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
