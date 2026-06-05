% ============================================================================
% CONSTRAINT STORY: pe_fund_level_leverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pe_fund_level_leverage, []).

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
 *   constraint_id: pe_fund_level_leverage
 *   human_readable: Shadow Leverage via fund-level debt in Private Equity
 *   domain: economic
 *
 * SUMMARY:
 *   Private equity firms are increasingly using fund-level leverage, such as
 *   Net Asset Value (NAV) loans, to borrow against the entire portfolio of a
 *   fund. This allows them to increase returns and deploy capital more
 *   quickly, but it also introduces systemic risk and can extract value from
 *   limited partners (LPs) and portfolio companies.
 *
 * KEY AGENTS:
 *   - Private Equity Firms (GPs): Primary beneficiary (institutional/arbitrage).
 *   - Fund Lenders: Secondary beneficiary (institutional/arbitrage).
 *   - Limited Partners (LPs): Primary victim (powerless/trapped).
 *   - Portfolio Companies: Secondary victim (moderate/constrained).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pe_fund_level_leverage, 0.55).
domain_priors:suppression_score(pe_fund_level_leverage, 0.4).
domain_priors:theater_ratio(pe_fund_level_leverage, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pe_fund_level_leverage, extractiveness, 0.55).
narrative_ontology:constraint_metric(pe_fund_level_leverage, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(pe_fund_level_leverage, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pe_fund_level_leverage, tangled_rope).
narrative_ontology:human_readable(pe_fund_level_leverage, "Shadow Leverage via fund-level debt in Private Equity").
narrative_ontology:topic_domain(pe_fund_level_leverage, "economic").

domain_priors:requires_active_enforcement(pe_fund_level_leverage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pe_fund_level_leverage, private_equity_firms).
narrative_ontology:constraint_beneficiary(pe_fund_level_leverage, fund_lenders).
narrative_ontology:constraint_victim(pe_fund_level_leverage, limited_partners).
narrative_ontology:constraint_victim(pe_fund_level_leverage, portfolio_companies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% LPs (pension funds, endowments) are often locked into long-term commitments and lack transparency into fund-level leverage. They bear the increased risk without commensurate reward.
constraint_indexing:constraint_classification(pe_fund_level_leverage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Portfolio companies may benefit from GPs having more capital to deploy and improve operational capabilities. However, they also bear additional financial risk from the debt incurred at the fund level, potentially leading to pressure to increase cash flow.
constraint_indexing:constraint_classification(pe_fund_level_leverage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% GPs benefit from increased returns from the leverage and the ability to deploy capital more quickly. They also take on risk if the fund performs poorly.
constraint_indexing:constraint_classification(pe_fund_level_leverage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Fund lenders benefit from the interest payments and fees generated from the NAV loans.
constraint_indexing:constraint_classification(pe_fund_level_leverage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Fund-level leverage is a tangled rope because it provides benefits to GPs and Lenders by allowing them to increase returns, and simultaneously introduces systemic risk and extracts value from LPs through obscured leverage and potential portfolio company distress.
constraint_indexing:constraint_classification(pe_fund_level_leverage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pe_fund_level_leverage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pe_fund_level_leverage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pe_fund_level_leverage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pe_fund_level_leverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(pe_fund_level_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. GPs and Lenders benefit financially, while LPs face increased risk and potentially lower returns due to hidden leverage. Portfolio companies may face increased pressure. Suppression (0.40): Moderate. LPs have limited ability to monitor or control fund-level leverage due to opaque reporting and long-term commitments. The use of fund-level leverage can obscure the true financial risk of a fund, suppressing alternatives for LPs. Theater Ratio (0.30): Relatively low. While there is some 'theater' in the marketing of fund performance, the leverage decisions are functionally driven by financial considerations.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the asymmetric information and power dynamics. GPs and lenders see a coordination mechanism (Rope) that allows them to enhance returns and deploy capital effectively. However, LPs are often unaware of the extent of leverage and the associated risks, leading to a Snare classification. Portfolio companies find themselves in a mixed position (Tangled Rope) facing increased pressure, but also potential benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the flow of benefits and risks. GPs and lenders are positioned as beneficiaries with arbitrage options. LPs are victims with limited exit options, leading to high directionality and a snare perspective. Portfolio companies have constrained exit options, and bear costs while also potentially benefitting from greater GP funding power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transparency_standards,
    'What transparency standards regarding fund-level leverage would adequately protect LPs?',
    'Analysis of historical fund performance under different leverage and transparency regimes; LP surveys regarding information needs.',
    'If high transparency: shifts classification towards rope. If low transparency: reinforces snare classification for LPs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_standards, empirical, 'Transparency standards impact on LP vulnerability.').

omega_variable(
    systemic_risk_threshold,
    'At what level of aggregate fund-level leverage does systemic risk become unacceptably high?',
    'Stress testing of PE fund portfolios under various economic scenarios; Monte Carlo simulation of default cascades.',
    'If low systemic risk: fund-level leverage is primarily a coordination mechanism. If high systemic risk: fund-level leverage introduces substantial negative externalities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_risk_threshold, empirical, 'Aggregate leverage risk tolerance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pe_fund_level_leverage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pe_f_tr_t0, pe_fund_level_leverage, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pe_f_tr_t5, pe_fund_level_leverage, theater_ratio, 5, 0.2).
narrative_ontology:measurement(pe_f_tr_t10, pe_fund_level_leverage, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(pe_f_be_t0, pe_fund_level_leverage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pe_f_be_t5, pe_fund_level_leverage, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(pe_f_be_t10, pe_fund_level_leverage, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pe_fund_level_leverage, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
