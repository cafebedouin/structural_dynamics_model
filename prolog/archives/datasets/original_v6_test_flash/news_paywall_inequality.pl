% ============================================================================
% CONSTRAINT STORY: news_paywall_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_news_paywall_inequality, []).

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
 *   constraint_id: news_paywall_inequality
 *   human_readable: Information Asymmetry due to News Paywalls
 *   domain: social
 *
 * SUMMARY:
 *   News paywalls, while intended to sustain quality journalism, create an
 *   information asymmetry. Those who can afford subscriptions gain access to
 *   in-depth reporting and diverse perspectives, while those who cannot rely
 *   on free, often less reliable or biased, sources. This disparity can
 *   exacerbate existing inequalities and hinder informed public discourse.
 *
 * KEY AGENTS:
 *   - News Organizations: Beneficiary (institutional/arbitrage) - Benefit through revenue, enabling quality journalism.
 *   - Low-Income Individuals: Victim (powerless/trapped) - Cannot afford access, limited to lower quality information.
 *   - Middle-Income Individuals: Moderate/constrained - Limited budget, can only afford selective access.
 *   - Public Discourse: The overall discourse suffers from information asymmetry.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(news_paywall_inequality, 0.55).
domain_priors:suppression_score(news_paywall_inequality, 0.6).
domain_priors:theater_ratio(news_paywall_inequality, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(news_paywall_inequality, extractiveness, 0.55).
narrative_ontology:constraint_metric(news_paywall_inequality, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(news_paywall_inequality, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(news_paywall_inequality, tangled_rope).
narrative_ontology:human_readable(news_paywall_inequality, "Information Asymmetry due to News Paywalls").
narrative_ontology:topic_domain(news_paywall_inequality, "social").

domain_priors:requires_active_enforcement(news_paywall_inequality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(news_paywall_inequality, news_organizations).
narrative_ontology:constraint_victim(news_paywall_inequality, low_income_individuals).
narrative_ontology:constraint_victim(news_paywall_inequality, public_discourse).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of low-income individuals who are effectively trapped due to financial constraints, limiting their access to quality information and participation in informed public discourse.
constraint_indexing:constraint_classification(news_paywall_inequality, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of middle-income individuals who are constrained by the cost of multiple subscriptions but may have some ability to choose which paywalls to access, thus experiencing a mix of coordination (access to some quality information) and extraction (financial burden).
constraint_indexing:constraint_classification(news_paywall_inequality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of news organizations that benefit from paywalls through increased revenue, enabling them to fund quality journalism. They experience the constraint as a coordination mechanism to sustain their operations.
constraint_indexing:constraint_classification(news_paywall_inequality, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of the overall public discourse, which is affected by the asymmetry. Some coordinated benefit is gained through higher quality journalism financed by paywalls, but asymmetric extraction occurs because the overall information landscape and equity are degraded.
constraint_indexing:constraint_classification(news_paywall_inequality, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Perspective of an analytical observer who recognizes the tangled_rope nature of news paywalls: they provide necessary funding for quality journalism (coordination) but exacerbate information inequality (extraction).
constraint_indexing:constraint_classification(news_paywall_inequality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(news_paywall_inequality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(news_paywall_inequality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(news_paywall_inequality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(news_paywall_inequality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(news_paywall_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55 - Moderate-high. News organizations extract value from readers through subscriptions. Lower income individuals disproportionately pay the cost. Suppression: 0.60 - Moderate-High. There are few high-quality substitutes for paid news content; free alternatives often lack depth, reliability, or are biased. Theater ratio: 0.30 - Low. Functional, revenue is directly related to the ability to produce quality content, not just a theatrical exercise.
 *
 * PERSPECTIVAL GAP:
 *   Low income individuals experience it as a Snare, trapped without the financial means for good quality coverage. News organizations see it as Rope, enabling the sustainable production of journalism. Middle-income sees a tangled rope, as they can access some coverage, but at a price and limited access. Analytical observer sees tangled rope because paywalls support journalistic activity but simultaneously degrade equal access.
 *
 * DIRECTIONALITY LOGIC:
 *   News organizations are beneficiaries because they gain revenue and sustain their operations. Low-income individuals are victims because they are denied access to quality information. Middle-income are constrained because they can choose but may not have full access. Public discourse suffers from the information asymmetry resulting from a divided access to quality journalism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_information_access,
    'What is the threshold of access to quality information necessary for informed participation in democratic processes?',
    'Empirical studies on the correlation between access to quality news and civic engagement, voting behavior, and policy preferences.',
    'If the threshold is high, the current paywall structure is more extractive. If the threshold is low, the system has sufficient arbitrage capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_information_access, empirical, 'Threshold for sufficient quality information access.').

omega_variable(
    alternative_funding_models,
    'Can alternative funding models for journalism, such as public funding or philanthropy, effectively replace paywalls without compromising journalistic independence?',
    'Comparative analysis of news organizations funded through different models, assessing their financial stability, editorial independence, and quality of reporting.',
    'If alternative models are viable, the dependence on paywalls decreases, potentially turning this to a scaffold. If they are not, the current tangled rope is the only remaining option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_models, conceptual, 'Viability of alternative funding models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(news_paywall_inequality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(news_tr_t0, news_paywall_inequality, theater_ratio, 0, 0.2).
narrative_ontology:measurement(news_tr_t5, news_paywall_inequality, theater_ratio, 5, 0.25).
narrative_ontology:measurement(news_tr_t10, news_paywall_inequality, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(news_be_t0, news_paywall_inequality, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(news_be_t5, news_paywall_inequality, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(news_be_t10, news_paywall_inequality, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(news_paywall_inequality, information_standard).
narrative_ontology:affects_constraint(news_paywall_inequality, filter_bubble_effect).
narrative_ontology:affects_constraint(news_paywall_inequality, misinformation_vulnerability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
