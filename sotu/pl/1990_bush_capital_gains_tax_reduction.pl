% ============================================================================
% CONSTRAINT STORY: 1990_bush_capital_gains_tax_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1990_bush_capital_gains_tax_reduction, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: 1990_bush_capital_gains_tax_reduction
 *   human_readable: Capital Gains Tax Reduction as Entrepreneurial Incentive (1990 Bush Tax Cut)
 *   domain: economics/fiscal_policy
 *
 * SUMMARY:
 *   The 1990 Bush capital gains tax reduction represents a price-based
 *   incentive mechanism designed to shift tax incidence away from investment
 *   returns, thereby lowering the after-tax hurdle rate for entrepreneurial
 *   ventures and productive capital deployment. The constraint exhibits the
 *   full typology of Deferential Realism classifications depending on
 *   observer position. From the perspective of wage-dependent workers and
 *   low-income households, the tax cut is pure extraction — they are trapped
 *   in labor-income taxation while capital gains are subsidized, bearing the
 *   burden through either higher payroll taxes or reduced public services.
 *   From the perspective of small business owners and venture founders, it is
 *   pure coordination — a mechanism that solves the collective action problem
 *   of ensuring risk-taking is rewarded sufficiently to justify the failure
 *   rate of entrepreneurial ventures. From the fiscal authority's
 *   perspective, it is hybrid: the coordination function (incentivizing
 *   entrepreneurship) coexists with extractive cost (reduced revenue that
 *   must be made up through wage taxation or deficit financing). The
 *   constraint's theater ratio has increased over the 30-year interval from
 *   0.35 to 0.52, reflecting that the 'global competitiveness' justification
 *   has become increasingly performative as the empirical relationship
 *   between US capital gains rates and actual investment flows has weakened.
 *   The analytical observer risks naturalizing a contingent policy choice as
 *   an immutable economic law — the inevitable trade-off between taxation and
 *   incentives — when the structural reality is a policy decision with
 *   measurable winners and losers.
 *
 * KEY AGENTS:
 *   - Small Business Owners and Venture Founders: Primary beneficiaries (institutional/arbitrage) — experience increased after-tax returns from successful exits; can arbitrage internationally if US rates become disadvantageous
 *   - Risk Capital Investors: Secondary beneficiaries (powerful/mobile) — institutional investors funding entrepreneurial ventures benefit from both lower capital gains taxes and from supporting the primary beneficiary class
 *   - Wage-Dependent Workers: Primary victims (powerless/trapped) — bear extraction through higher payroll tax burden or reduced public services; have no alternative income source outside labor markets
 *   - Low-Income Households: Secondary victims (moderate/constrained) — constrained by limited accumulated capital; cannot access capital gains benefits; face higher relative tax burden
 *   - Federal Treasury: Institutional actor (institutional/constrained) — bears fiscal cost through reduced revenue; constrained exit: cannot easily raise rates without reducing incentive function
 *   - Global Competitive Market: Institutional frame (organized/constrained) — maintains justification for capital gains cuts even as empirical relationship to actual capital flows has degraded; constrained by international tax competition dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1990_bush_capital_gains_tax_reduction, 0.48).
domain_priors:suppression_score(1990_bush_capital_gains_tax_reduction, 0.35).
domain_priors:theater_ratio(1990_bush_capital_gains_tax_reduction, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1990_bush_capital_gains_tax_reduction, extractiveness, 0.48).
narrative_ontology:constraint_metric(1990_bush_capital_gains_tax_reduction, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(1990_bush_capital_gains_tax_reduction, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1990_bush_capital_gains_tax_reduction, tangled_rope).
narrative_ontology:human_readable(1990_bush_capital_gains_tax_reduction, "Capital Gains Tax Reduction as Entrepreneurial Incentive (1990 Bush Tax Cut)").
narrative_ontology:topic_domain(1990_bush_capital_gains_tax_reduction, "economics/fiscal_policy").

domain_priors:requires_active_enforcement(1990_bush_capital_gains_tax_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1990_bush_capital_gains_tax_reduction, small_business_owners).
narrative_ontology:constraint_beneficiary(1990_bush_capital_gains_tax_reduction, risk_capital_investors).
narrative_ontology:constraint_beneficiary(1990_bush_capital_gains_tax_reduction, venture_founders).
narrative_ontology:constraint_victim(1990_bush_capital_gains_tax_reduction, federal_revenue_stability).
narrative_ontology:constraint_victim(1990_bush_capital_gains_tax_reduction, wage_earners).
narrative_ontology:constraint_victim(1990_bush_capital_gains_tax_reduction, low_income_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE-DEPENDENT WORKER (SNARE) — Cannot exit the labor-income tax structure. Trapped in wage taxation while capital gains are subsidized. Bears extraction through higher payroll taxes or reduced public services. No alternative income source; extraction is maximal relative to trapped exit capacity.
constraint_indexing:constraint_classification(1990_bush_capital_gains_tax_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-INCOME HOUSEHOLDS (SNARE) — Constrained by limited accumulated capital. Cannot access capital gains benefits; face higher relative tax burden. Extraction occurs through reallocation of tax burden from capital to wage income. Generational extraction: benefits of capital appreciation accrue to heirs of existing asset owners, not to new entrants.
constraint_indexing:constraint_classification(1990_bush_capital_gains_tax_reduction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL BUSINESS OWNERS (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: lower capital gains tax increases after-tax returns, incentivizing risk-taking and business formation. Extraction runs toward them. High arbitrage capacity — can exit or reallocate capital internationally if rates become disadvantageous. Experiences positive chi (net subsidy).
constraint_indexing:constraint_classification(1990_bush_capital_gains_tax_reduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL TREASURY (TANGLED ROPE) — Bears dual structure: genuine coordination function (incentivizes productive investment and entrepreneurship, which generates future tax base) alongside extractive cost (reduced current revenue, shift of burden to wage taxation). Constrained exit: cannot simply raise rates without reducing entrepreneurial incentive, creating fiscal constraint. Generational extraction: current debt accumulation imposes obligations on future taxpayers.
constraint_indexing:constraint_classification(1990_bush_capital_gains_tax_reduction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL INVESTORS (TANGLED ROPE) — Powerful actors with mobile capital (arbitrage exit). Benefit from both capital gains tax cuts AND from the primary beneficiary support (they fund entrepreneurial ventures). Complex extraction: some institutional investors benefit from lower capital gains rates (portfolio management), while others benefit from entrepreneurial success they fund. Mobile enough to arbitrage: can move capital to other jurisdictions if US rates increase relative to competitors.
constraint_indexing:constraint_classification(1990_bush_capital_gains_tax_reduction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL COMPETITIVE FRAMING (PITON) — Theater ratio (0.52) reflects that the 'global competitiveness' justification for capital gains cuts has become increasingly performative over time. The original 1990 framing — capital gains cuts are necessary to compete globally — persists despite evidence that capital deployment patterns are driven more by market fundamentals, talent availability, and regulatory environment than by marginal tax rate differences. The constraint persists through institutional inertia (deficit politics) even as the coordination function (incentivizing global capital attraction) faces diminishing returns. Piton classification: maintains high cultural signaling ('pro-business,' 'pro-growth') despite degraded functional relationship to actual entrepreneurial outcomes.
constraint_indexing:constraint_classification(1990_bush_capital_gains_tax_reduction, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, capital gains taxation presents an inherent trade-off: if you tax capital gains, you reduce incentives for productive investment; if you don't, you generate fiscal cost and wealth concentration. This perspective risks seeing the capital gains tax structure as an immutable law of economics — the inevitable consequence of trying to tax returns to capital. However, the structural data contradicts this mountain classification: the fiscal extraction (reduced revenue, burden shift to wages) is contingent on policy choice, not inherent to economics. This is a false summit: naturalized policy preference.
constraint_indexing:constraint_classification(1990_bush_capital_gains_tax_reduction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1990_bush_capital_gains_tax_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1990_bush_capital_gains_tax_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1990_bush_capital_gains_tax_reduction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1990_bush_capital_gains_tax_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1990_bush_capital_gains_tax_reduction, TR),
    TR >= 0.70.

:- end_tests(1990_bush_capital_gains_tax_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The capital gains reduction transfers tax burden from capital income to wage income and generates fiscal deficits passed to future taxpayers. The extraction is not maximal because the primary beneficiaries (entrepreneurs) do generate genuine economic value and tax base growth, creating a coordination function. However, extractiveness has risen from 0.25 to 0.48 over the 30-year interval, indicating that the fiscal cost has accumulated without equivalent revenue replacement or corresponding increases in entrepreneurial output relative to baseline expectations. Suppression (0.35): Moderate. The tax code's complexity suppresses alternatives by creating high barriers to understanding the incentive structure and high compliance costs for small operators. However, suppression is not severe — the capital gains mechanism is explicit, investors can arbitrage (exit to other countries or portfolio strategies), and some competitive pressure exists from other nations' tax policies. Theater ratio (0.52): Moderate. The 'global competitiveness' justification for capital gains cuts has become performative: it continues to justify the mechanism despite weakening empirical evidence that marginal US tax rate differentials drive international capital flows. The theater increased over the interval as competing policy narratives (fiscal sustainability, inequality, productive investment efficiency) emerged without displacing the original narrative. The constraint persists through institutional inertia rather than demonstrated functional correlation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Entrepreneurs see pure coordination (Rope) — a mechanism that solves their collective action problem of ensuring risk-taking is rewarded. The fiscal authority sees hybrid constraint (Tangled Rope) — genuine coordination function coexisting with unsustainable revenue loss. Wage workers see pure extraction (Snare) — they are trapped in labor taxation while capital income is subsidized. Low-income households see generational extraction (Snare at generational time horizon) — accumulated wealth from past generations now benefits from lower taxes while new entrants face barriers to entry. Institutional investors see nuanced Tangled Rope — they benefit from the primary incentive AND from entrepreneurial success, but face some fiscal sustainability constraints. The global competitiveness frame sees degraded piton (theater ratio shows institutional inertia replacing functional relationship). The analytical observer risks naturalizing the choice as natural law (Mountain) — seeing the capital gains tax cut as an immutable economic trade-off rather than contingent policy design. The false summit is the risk: treating 'you can't tax capital gains heavily because it reduces investment' as a law of economics rather than an empirical claim about one policy mechanism's effects.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the extraction flow. Wage workers (powerless/trapped) experience d ≈ 0.95 (full target of extraction), producing high f(d) ≈ 1.42, which scales χ upward. Entrepreneurs (institutional/arbitrage) experience d ≈ 0.05 (full beneficiary), producing f(d) ≈ -0.12, which produces negative χ (net subsidy). The fiscal authority (institutional/constrained) experiences d ≈ 0.60 (partial target — revenue loss creates fiscal pressure), producing f(d) ≈ 0.65, yielding moderate χ. Scope modifier σ(S) = 1.0 for national scope applies uniformly. The critical asymmetry: those who can exit (entrepreneurs, institutional investors) experience low extraction; those who cannot (wage workers) experience high extraction. The constraint persists because the beneficiaries have mobility and power, while victims are dispersed and powerless.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that the classification depends critically on whose perspective is privileged. If the analytical frame privileges entrepreneurial incentive effectiveness, the classification is Rope (pure coordination). If the frame privileges fiscal sustainability and distributional fairness, the classification is Snare (pure extraction with a false coordination claim). If the frame privileges the hybrid structure (genuine entrepreneurial incentive function coexisting with revenue loss), the classification is Tangled Rope. The 'correct' type is not objectively determinable from the base properties alone — it depends on which values (entrepreneurship incentive versus fiscal sustainability) are weighted as primary. The mandatrophy resolution is to acknowledge this: the Tangled Rope classification is structurally accurate because both the coordination function (incentivizing entrepreneurship) AND the extraction mechanism (shifting burden to wages, reducing public investment) are genuine and irreducible. The constraint is not pure extraction hiding as incentive (that would be Snare with false coordination claim), nor is it pure incentive with negligible extraction (that would be Rope). It authentically exhibits both structures simultaneously. The perspectival gap reveals the political choice: how much extraction are we willing to tolerate to fund entrepreneurship incentives?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    entrepreneurial_incentive_responsiveness,
    'What is the actual elasticity of business formation and risk capital deployment with respect to capital gains tax rates?',
    'Quasi-experimental analysis using cross-state variation in capital gains taxation, interrupted time-series around major federal tax changes, and international comparisons controlling for institutional differences',
    'If elasticity is high (>0.5): capital gains cuts genuinely coordinate entrepreneurial incentives (validates Rope perspective). If elasticity is low (<0.1): cuts are pure extraction with weak coordination function (upgrades to Snare from multiple perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrepreneurial_incentive_responsiveness, empirical, 'Elasticity of business formation with respect to capital gains tax rate').

omega_variable(
    real_economic_output_attribution,
    'What fraction of increased business formation and entrepreneurial activity following capital gains tax cuts is attributable to the tax cut itself versus general economic conditions, technology cycles, or other concurrent policy changes?',
    'Regression discontinuity design around exact tax change dates; factor analysis of business formation timing relative to multiple policy and market events; counterfactual modeling of alternative fiscal scenarios',
    'If cap gains cuts explain >50% of entrepreneurial increase: strong coordination function (validates Rope/Tangled Rope). If <10%: false attribution (the coordination claim is theater, supporting Piton perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(real_economic_output_attribution, empirical, 'Attribution of entrepreneurial activity to capital gains tax cuts versus other factors').

omega_variable(
    wealth_concentration_feedback,
    'Does the capital gains tax cut primarily benefit existing asset owners versus new entrepreneurs? What is the ratio of wealth accrual by existing portfolio holders to wealth creation by new ventures?',
    'Decomposition of capital gains realizations by source (portfolio appreciation vs business growth); analysis of asset concentration trends pre/post tax cuts; tracking of who benefits from business exits and acquisitions',
    'If majority benefits flow to existing asset owners: suppression increases (extraction mechanism strengthens as new entrants face higher barriers to compete with subsidized incumbents). If majority flows to new entrepreneurs: suppression decreases (genuine incentive for new entry).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wealth_concentration_feedback, empirical, 'Distribution of capital gains benefits between existing asset owners and new entrepreneurs').

omega_variable(
    fiscal_sustainability_threshold,
    'At what level of capital gains tax reduction does the fiscal extraction (revenue loss) become unsustainable relative to public investment in human capital, infrastructure, and research?',
    'Long-term fiscal modeling; comparison of US federal investment in education/infrastructure/R&D versus historical levels and OECD peers; estimation of opportunity cost of foregone revenue',
    'If current rate is below sustainability threshold: constraint is moderate Tangled Rope (coordination benefits outweigh fiscal cost). If above threshold: constraint is severe Snare (extraction dominates; coordination function is rationalization for fiscal transfer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_threshold, empirical, 'Fiscal sustainability threshold for capital gains tax reduction').

omega_variable(
    alternative_incentive_effectiveness,
    'Are direct incentive mechanisms (startup grants, accelerators, IP protection, research tax credits) equally or more effective at catalyzing entrepreneurship per dollar of fiscal cost than capital gains tax reduction?',
    'Comparative effectiveness analysis of direct versus tax-based entrepreneurship incentives; cost-per-startup-formed analysis; venture formation and survival rates under different incentive regimes',
    'If direct mechanisms are more effective: capital gains cuts represent pure fiscal extraction hidden behind entrepreneur framing (Snare with false coordination claim). If less effective: capital gains cuts have genuine coordination advantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_incentive_effectiveness, empirical, 'Comparative effectiveness of alternative entrepreneurship incentive mechanisms').

omega_variable(
    tax_avoidance_reallocation_effect,
    'What fraction of capital gains from the tax cut reflects genuine new investment versus reallocation of existing income streams to the lower-taxed capital gains category through tax planning?',
    'Analysis of income timing and classification changes around tax rate changes; tracking of C-corp to S-corp/partnership conversions; executive compensation structure shifts (salary to stock options); forensic accounting of income source switching',
    'If reallocation >30%: true extractiveness is much higher than reported (the constraint is pure extraction disguised as incentive). If reallocation <5%: genuine new investment incentive effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_avoidance_reallocation_effect, empirical, 'Fraction of capital gains reduction attributable to tax avoidance reallocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1990_bush_capital_gains_tax_reduction, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cgtax_tr_t0, 1990_bush_capital_gains_tax_reduction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cgtax_tr_t5, 1990_bush_capital_gains_tax_reduction, theater_ratio, 5, 0.48).
narrative_ontology:measurement(cgtax_tr_t10, 1990_bush_capital_gains_tax_reduction, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(cgtax_be_t0, 1990_bush_capital_gains_tax_reduction, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cgtax_be_t5, 1990_bush_capital_gains_tax_reduction, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(cgtax_be_t10, 1990_bush_capital_gains_tax_reduction, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1990_bush_capital_gains_tax_reduction, resource_allocation).
narrative_ontology:affects_constraint(1990_bush_capital_gains_tax_reduction, wealth_concentration_mechanism).
narrative_ontology:affects_constraint(1990_bush_capital_gains_tax_reduction, federal_fiscal_sustainability).
narrative_ontology:affects_constraint(1990_bush_capital_gains_tax_reduction, intergenerational_equity_extraction).

% DUAL FORMULATION NOTE:
% The capital gains tax reduction can be decomposed into two structurally distinct constraints: (1) entrepreneurial_incentive_mechanism (ε ≈ 0.15, Rope) — the price signal reducing after-tax hurdle rates for business formation; (2) fiscal_burden_reallocation (ε ≈ 0.65, Snare) — the revenue loss and shift to wage taxation. The combined story treats them as integrated (Tangled Rope), but analytical decomposition reveals the coordination and extraction functions can be unbundled: direct grants and R&D credits could achieve entrepreneurial incentive without fiscal extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1990_bush_capital_gains_tax_reduction, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
