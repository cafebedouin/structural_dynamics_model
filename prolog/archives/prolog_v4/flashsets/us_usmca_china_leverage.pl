% ============================================================================
% CONSTRAINT STORY: us_usmca_china_leverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_usmca_china_leverage, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_usmca_china_leverage
 *   human_readable: US leveraging of USMCA ratification to constrain Canadian foreign policy on China
 *   domain: geopolitical
 *
 * SUMMARY:
 *   During the Trump administration, the United States government exerted
 *   significant pressure on Canada to adopt a more confrontational stance
 *   towards China, leveraging the ratification process of the United
 *   States-Mexico-Canada Agreement (USMCA). This created a complex
 *   geopolitical dynamic where Canada's economic interests were directly tied
 *   to its foreign policy decisions concerning China.
 *
 * KEY AGENTS:
 *   - US Government: Primary beneficiary (institutional/arbitrage) - benefits from advancing its geopolitical interests and protecting domestic industries.
 *   - Canadian Government: Primary target (moderate/constrained) - constrained in its foreign policy options and economic sovereignty.
 *   - Canadian Industries Relying on China: Secondary target (powerless/trapped) - industries negatively impacted by strained relations between Canada and China.
 *   - US Industries Competing with China: Secondary beneficiary (powerful/mobile) - industries that gain a competitive advantage from Canada's policies that align with US interests.
 *   - Chinese Government: Indirectly affected (powerful/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_usmca_china_leverage, 0.6).
domain_priors:suppression_score(us_usmca_china_leverage, 0.7).
domain_priors:theater_ratio(us_usmca_china_leverage, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_usmca_china_leverage, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_usmca_china_leverage, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_usmca_china_leverage, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_usmca_china_leverage, tangled_rope).
narrative_ontology:human_readable(us_usmca_china_leverage, "US leveraging of USMCA ratification to constrain Canadian foreign policy on China").
narrative_ontology:topic_domain(us_usmca_china_leverage, "geopolitical").

domain_priors:requires_active_enforcement(us_usmca_china_leverage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_usmca_china_leverage, us_government).
narrative_ontology:constraint_beneficiary(us_usmca_china_leverage, us_industries_competing_with_china).
narrative_ontology:constraint_victim(us_usmca_china_leverage, canadian_sovereignty).
narrative_ontology:constraint_victim(us_usmca_china_leverage, canadian_industries_relying_on_china).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Canadian sovereignty is significantly constrained due to its economic reliance on the US market and its relative lack of leverage in renegotiating trade agreements. Trapped exit due to geographical proximity and trade dependence.
constraint_indexing:constraint_classification(us_usmca_china_leverage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The Canadian government benefits from the overall USMCA agreement but is constrained in its foreign policy options regarding China. Constrained exit due to need for US market access, but can still maneuver within certain bounds.
constraint_indexing:constraint_classification(us_usmca_china_leverage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The US government benefits from the USMCA agreement and also leverages it to advance its geopolitical interests regarding China. Arbitrage exit as the US has numerous other tools in its foreign policy arsenal. The US experiences this as a coordination mechanism to advance its strategic goals.
constraint_indexing:constraint_classification(us_usmca_china_leverage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Chinese government is indirectly affected by this constraint. While not directly part of USMCA, it is subject to the effects of Canada’s constrained foreign policy. Mobile exit as China has other trading partners.
constraint_indexing:constraint_classification(us_usmca_china_leverage, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical observer sees the USMCA leverage as a tangled rope, exhibiting both coordination and extraction between the US and Canada, with broader implications for global geopolitics and trade relations.
constraint_indexing:constraint_classification(us_usmca_china_leverage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_usmca_china_leverage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_usmca_china_leverage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_usmca_china_leverage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_usmca_china_leverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_usmca_china_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is assessed at 0.60 due to the significant pressure exerted by the US government, limiting Canada's policy independence. Suppression is 0.70 as Canada's economic dependence on the US market leaves few alternatives. The theater ratio is 0.30, indicating a moderate level of performative actions taken to appease the US while attempting to maintain some independence.
 *
 * PERSPECTIVAL GAP:
 *   The US government perceives the situation as a coordination mechanism to advance its geopolitical goals, while the Canadian government experiences it as a constraint on its sovereignty. Canadian industries relying on China are negatively impacted and experience a snare. The analytical observer sees the situation as a tangled rope, with both coordination and extraction present.
 *
 * DIRECTIONALITY LOGIC:
 *   The US benefits from increased alignment in foreign policy, providing it with a strategic advantage. Canada bears the cost of limited sovereignty and potential economic repercussions. Canadian industries depending on China bear extraction, while US industries benefit. This dynamic is reflected in the different perspectives and resulting classifications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_trade_agreement_leverage,
    'To what extent will the US continue to use trade agreements as leverage for geopolitical goals?',
    'Analysis of future trade agreements and their specific clauses, tracking the degree of alignment with US foreign policy goals.',
    'High future leverage: further erosion of national sovereignty for trade partners. Low future leverage: trade agreements focus primarily on economic benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_trade_agreement_leverage, empirical, 'The degree to which trade agreements will be used for geopolitical goals.').

omega_variable(
    canadian_market_diversification,
    'How successful will Canada be in diversifying its trade relationships away from the US?',
    'Tracking changes in Canadian trade flows with other countries (EU, Asia) and investments in diversifying export markets.',
    'High diversification: reduced US leverage over Canadian foreign policy. Low diversification: continued vulnerability to US pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canadian_market_diversification, empirical, 'Canada''s ability to diversify its trade relationships.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_usmca_china_leverage, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_u_tr_t0, us_usmca_china_leverage, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_u_tr_t2, us_usmca_china_leverage, theater_ratio, 2, 0.3).
narrative_ontology:measurement(us_u_tr_t4, us_usmca_china_leverage, theater_ratio, 4, 0.35).

% Extraction over time
narrative_ontology:measurement(us_u_be_t0, us_usmca_china_leverage, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(us_u_be_t2, us_usmca_china_leverage, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(us_u_be_t4, us_usmca_china_leverage, base_extractiveness, 4, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(us_usmca_china_leverage, us_china_trade_war).
narrative_ontology:affects_constraint(us_usmca_china_leverage, huawei_5g_ban).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
