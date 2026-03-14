% ============================================================================
% CONSTRAINT STORY: pension_fund_liability_mismatch
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pension_fund_liability_mismatch, []).

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
 *   constraint_id: pension_fund_liability_mismatch
 *   human_readable: Pension Fund Liability Mismatch: Coordination and Extraction in Longevity Risk Transfer
 *   domain: financial_systems/institutional_governance
 *
 * SUMMARY:
 *   Pension fund liability mismatch arises when the duration and
 *   characteristics of invested assets diverge from the duration and
 *   structure of promised liabilities. In a defined-benefit system, this
 *   creates a coordination problem: the plan sponsor must invest resources
 *   strategically to ensure assets will be available to pay promised
 *   benefits. However, the constraint embeds extraction: younger workers and
 *   future generations subsidize unfunded legacy liabilities; beneficiaries
 *   bear longevity risk through benefit adjustments; and the regulatory
 *   apparatus enforces liability-matching rules that extract governance
 *   overhead while constraining return-seeking strategies. The extractiveness
 *   value (0.58) reflects that the mismatch is neither pure coordination
 *   (rope) nor pure extraction (snare), but a hybrid that genuinely
 *   coordinates around longevity risk while systematically extracting value
 *   from powerless agents. Theater has increased as regulatory complexity has
 *   outpaced actual risk mitigation effectiveness — pension governance now
 *   requires substantial compliance infrastructure that performs governance
 *   ritual without commensurately improving benefit security.
 *
 * KEY AGENTS:
 *   - Retired Plan Members: Primary victim (powerless/trapped) — stuck with frozen benefits and longevity risk; cannot exit or renegotiate
 *   - Younger Active Members: Secondary victim (moderate/constrained) — cross-subsidize legacy liabilities and bear contribution increases; have some exit through career change or shifting to DC plans
 *   - Pension Fund Sponsors: Primary beneficiary (institutional/arbitrage) — can adjust liability assumptions, extend contribution schedules, or transfer liabilities to insurance firms
 *   - Regulatory and Governance Framework: Organized enforcer (organized/constrained) — maintains liability-matching standards and funding rules; coordinates disclosure but extracts compliance costs
 *   - Alternative De-Risking Industry: Organized pathway (organized/mobile) — buyout firms and annuity providers offering liability transfer mechanisms with sunset logic
 *   - Traditional DB Plan Structure: Degraded institution (institutional/arbitrage) — persists through inertia despite declining function; maintains high theater relative to risk mitigation achieved
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing longevity risk as immutable law rather than institutional commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pension_fund_liability_mismatch, 0.58).
domain_priors:suppression_score(pension_fund_liability_mismatch, 0.52).
domain_priors:theater_ratio(pension_fund_liability_mismatch, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pension_fund_liability_mismatch, extractiveness, 0.58).
narrative_ontology:constraint_metric(pension_fund_liability_mismatch, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(pension_fund_liability_mismatch, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pension_fund_liability_mismatch, tangled_rope).
narrative_ontology:human_readable(pension_fund_liability_mismatch, "Pension Fund Liability Mismatch: Coordination and Extraction in Longevity Risk Transfer").
narrative_ontology:topic_domain(pension_fund_liability_mismatch, "financial_systems/institutional_governance").

domain_priors:requires_active_enforcement(pension_fund_liability_mismatch).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pension_fund_liability_mismatch, pension_fund_sponsors).
narrative_ontology:constraint_beneficiary(pension_fund_liability_mismatch, institutional_investors).
narrative_ontology:constraint_victim(pension_fund_liability_mismatch, plan_beneficiaries).
narrative_ontology:constraint_victim(pension_fund_liability_mismatch, younger_cohorts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETIRED PLAN MEMBER (SNARE) — Trapped in the liability mismatch with no exit. Cannot switch pension providers or renegotiate benefits. Bears longevity risk through benefit freezes, COLA reductions, and delayed payment schedules. Maximum extraction with zero agency. Sees constraint as immutable because exit requires abandoning earned benefits.
constraint_indexing:constraint_classification(pension_fund_liability_mismatch, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: YOUNGER ACTIVE MEMBER (TANGLED ROPE) — Constrained by career path dependence and inability to redirect pension contributions. Benefits from coordination (employer matches, diversified investments) but bears extraction through contribution increases, benefit accruals that may not materialize, and cross-subsidization of unfunded legacy liabilities. Mixed experience — some benefit, significant cost.
constraint_indexing:constraint_classification(pension_fund_liability_mismatch, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PENSION FUND SPONSOR (ROPE) — Institutional beneficiary with arbitrage options. Can adjust liability assumptions, extend contribution schedules, reduce future accruals, or seek liability transfer deals. Experiences the constraint as coordination: matching investments to liabilities enables financial stability and reduces equity volatility. Net beneficiary from liability matching frameworks.
constraint_indexing:constraint_classification(pension_fund_liability_mismatch, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AND GOVERNANCE FRAMEWORK (TANGLED ROPE) — Organized actors (regulators, accounting standards boards, pension protection corporations) coordinate liability disclosure and funding rules while extracting governance overhead and imposing rigid asset-liability matching that constrains portfolio optimization. Active enforcement maintains the constraint; some genuine coordination benefit (risk containment) alongside extractive effects (reduced return-seeking, regulatory compliance costs).
constraint_indexing:constraint_classification(pension_fund_liability_mismatch, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE DE-RISKING PATHWAY (SCAFFOLD) — Pension buyout firms, annuity providers, and liability-driven investment strategies represent sunset mechanisms. As these alternatives mature, plan sponsors can transfer liabilities to specialized providers, reducing the need for long-term coordination of in-house mismatches. Theater is moderate (buyouts involve substantial fees and administrative theater); sunset is structural (growing liability transfer market). Effective extraction declines as exits become available.
constraint_indexing:constraint_classification(pension_fund_liability_mismatch, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL DEFINED-BENEFIT MODEL (PITON) — The DB pension structure persists through institutional inertia and cultural expectation despite degraded functionality: equity returns insufficient to fund promised benefits, plan design changes render original contracts hollow, and administrative overhead is high relative to actual risk mitigation achieved. The theater ratio (0.64) reflects performative governance around a mechanism that no longer solves the problem it was designed for. Maintained because alternatives (DC plans, auto-IRAs) are still developing.
constraint_indexing:constraint_classification(pension_fund_liability_mismatch, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MORTALITY RISK VIEW (MOUNTAIN) — From a civilizational perspective, longevity risk is an irreducible feature of any system that promises lifetime payments. The mismatch between asset duration (finite) and liability duration (stochastic but potentially infinite) is structurally immutable. This view risks naturalizing what is contingent: the obligation to provide lifetime benefits is a human choice, not a law of nature. The engine flags this as a false summit, revealing that 'inherent longevity risk' naturalizes institutional commitment rather than physical law.
constraint_indexing:constraint_classification(pension_fund_liability_mismatch, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pension_fund_liability_mismatch_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pension_fund_liability_mismatch, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pension_fund_liability_mismatch, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pension_fund_liability_mismatch, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pension_fund_liability_mismatch, TR),
    TR >= 0.70.

:- end_tests(pension_fund_liability_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The liability mismatch began as a genuine coordination challenge: matching assets to liabilities reduces portfolio volatility and ensures benefit security. But as equity returns have declined and longevity has increased, the mismatch has become structural. Younger workers now contribute at rates that would have been unthinkable 30 years ago, and their accruals may not materialize if plans close to new entrants (common since 2000s). The trajectory from 0.35 to 0.58 reflects that the coordination function has degraded while the extraction mechanism has persisted and intensified. Suppression (0.52): Moderate-high. Significant barriers prevent workers from exiting: earned benefits cannot be forfeited, alternative retirement systems are underdeveloped, career switching is expensive, and regulatory frameworks penalize plan termination. But suppression is not absolute — some workers can shift to DC plans, and buyout options are expanding. Theater ratio (0.64): Moderate-high and rising. Traditional DB pension governance involves substantial compliance (actuarial valuations, funding reports, trustee meetings) that performs risk management without proportionally improving outcomes. The theater increase from 0.48 to 0.64 reflects that regulatory complexity has grown while actual benefit security has declined in many plans — the theater is now the primary output relative to the actual risk mitigation achieved.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals perspectival divergence rooted in structural position rather than disagreement about facts. All perspectives agree on the core structural facts: liabilities exceed expected asset returns; workers cannot easily exit; regulatory frameworks enforce coordination rules. But the experienced classification differs radically. Retired members with trapped exit see snare (pure extraction, no coordination benefit). Younger members with constrained exit see tangled rope (mixed coordination and extraction). Sponsors with arbitrage exit see rope (primarily coordination). Regulators see enforcement as coordination (themselves); beneficiaries see it as extraction overhead. De-risking industry sees temporary constraint with sunset. DB plan advocates see degraded ritual. The analytical observer risks seeing natural law. The gap is not disagreement but structural difference in extraction flow direction and exit capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position: who benefits, who bears costs, and what options exist for exit. Retired members are victims with zero exit (d ≈ 0.95, f(d) ≈ 1.42, maximum experienced extraction). Younger members are mixed (both contributors to current retirees and future beneficiaries); constrained exit yields d ≈ 0.65, f(d) ≈ 1.00, moderate extraction. Plan sponsors are beneficiaries with arbitrage options (can transfer liabilities, adjust assumptions); d ≈ 0.15, f(d) ≈ -0.01, experienced as coordination or slight subsidy. Organized agents (regulators, de-risking firms) with mobile/constrained exit experience moderate d. The scope modifier σ(S) = 1.0 (national) reflects that the constraint operates primarily within national regulatory frameworks, though pension systems increasingly operate across borders. The pipeline computes χ = ε × f(d) × σ(S) for each perspective, showing why the same structural mismatch is experienced as snare by powerless agents and rope by institutional beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: The constraint resolves the tension between calling it 'coordination' (DB pensions genuinely coordinate longevity risk) and calling it 'extraction' (the system extracts value from younger workers and locked-in beneficiaries). The resolution is perspectival: it IS coordination from the plan sponsor's view (they are solving a real problem — matching assets to liabilities). It IS extraction from the powerless member's view (they bear the full cost of the mismatch with no exit). It IS a mixed tangled_rope from the younger worker's view (they benefit from coordination structure but subsidize legacy costs). The mandatrophy collapses when we recognize that the two descriptions are compatible — they describe different aspects of the same constraint from different structural positions. The constraint is coordinative for those with exit options and extractive for those without. The classification is not 'which one is it?' but 'from which structural position are you measuring?' The claimed type (tangled_rope) captures this hybrid correctly: genuine coordination function + asymmetric extraction + active enforcement to maintain the mismatch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    longevity_assumption_uncertainty,
    'How much of the measured liability mismatch reflects genuine longevity improvement vs. outdated mortality assumption updates?',
    'Historical comparison of assumed vs actual lifespans; analysis of assumption revision cycles; correlation with population-level mortality data',
    'If mismatch is primarily assumption lag: constraint is temporary (scaffold). If mismatch reflects genuine uninsurable longevity trends: constraint is structural (tangled_rope or snare). Classification shifts with resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(longevity_assumption_uncertainty, empirical, 'Whether mismatch reflects assumption lag or genuine longevity risk').

omega_variable(
    asset_return_realism,
    'Are expected asset returns used in liability matching realistic given current market valuations and demographic headwinds?',
    'Stress-test return assumptions against historical ranges; Monte Carlo analysis of portfolio outcomes; comparison across pension systems using different return assumptions',
    'If returns are optimistic: mismatch is understated, extraction heavier than measured. If returns are conservative: mismatch reflects realistic risk allocation. Impacts whether constraint is classified as snare (if returns are wishful) or rope (if realistic).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asset_return_realism, empirical, 'Whether return assumptions are realistic for liability matching').

omega_variable(
    cross_generational_fairness_asymmetry,
    'Does the liability mismatch systematically transfer intergenerational risk from older to younger cohorts, and is this transfer intentional or emergent?',
    'Generational accounting analysis; comparison of benefit-contribution ratios across cohorts; policy document analysis of design intent; interview data from plan design decision-makers',
    'If intentional intergenerational transfer: constraint is foundational (tangled_rope). If unintended consequence: constraint is failure mode (snare). If mitigated by sunset mechanisms: constraint is scaffold. Intent affects classification and mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_generational_fairness_asymmetry, conceptual, 'Whether intergenerational transfer is intentional or emergent').

omega_variable(
    alternative_de_risking_sufficiency,
    'Can pension buyout and annuity transfer markets actually absorb the volume and risk profile of liability transfers without systemic cost increases?',
    'Analysis of liability transfer market growth rates; pricing data for buyouts and annuities; correlation between transfer volume and cost; stress-test of insurance industry capacity',
    'If sufficient: scaffold sunset is real and structural. If insufficient: buyout pathway is theater (expensive alternative theater for same fundamental constraint), reducing confidence in scaffold classification. Impacts generational outlook for constraint resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_de_risking_sufficiency, empirical, 'Whether de-risking market can absorb pension liabilities').

omega_variable(
    regulatory_capture_in_liability_matching,
    'Do accounting and regulatory standards for liability matching reflect genuine risk management needs or serve primarily to favor pension fund sponsors and insurance firms?',
    'Historical analysis of regulatory changes and industry lobbying; comparison of risk-adjusted returns across different matching strategies; cost-benefit analysis of specific regulations from beneficiary perspective',
    'If genuinely protective: regulatory framework is coordination mechanism. If captured: framework is extraction vehicle, increasing suppression and shifting classification toward snare. Directly affects directionality and institutional perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_in_liability_matching, conceptual, 'Whether liability matching standards reflect genuine risk management or regulatory capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pension_fund_liability_mismatch, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfm_tr_t0, pension_fund_liability_mismatch, theater_ratio, 0, 0.48).
narrative_ontology:measurement(pfm_tr_t10, pension_fund_liability_mismatch, theater_ratio, 10, 0.56).
narrative_ontology:measurement(pfm_tr_t20, pension_fund_liability_mismatch, theater_ratio, 20, 0.64).
narrative_ontology:measurement(pfm_tr_t30, pension_fund_liability_mismatch, theater_ratio, 30, 0.6).

% Extraction over time
narrative_ontology:measurement(pfm_be_t0, pension_fund_liability_mismatch, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pfm_be_t10, pension_fund_liability_mismatch, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pfm_be_t20, pension_fund_liability_mismatch, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(pfm_be_t30, pension_fund_liability_mismatch, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pension_fund_liability_mismatch, resource_allocation).
narrative_ontology:affects_constraint(pension_fund_liability_mismatch, longevity_risk_transfer).
narrative_ontology:affects_constraint(pension_fund_liability_mismatch, defined_contribution_system_adequacy).
narrative_ontology:affects_constraint(pension_fund_liability_mismatch, intergenerational_fiscal_sustainability).

% DUAL FORMULATION NOTE:
% The liability mismatch is upstream of specific de-risking mechanisms and downstream of demographic/economic trends. Separate constraint stories should address: (1) longevity_risk_transfer (ε ≈ 0.42, the insurance industry's side of liability buyouts) and (2) intergenerational_fiscal_sustainability (ε ≈ 0.65, whether the mismatch is symptom of deeper fiscal imbalance or contained within pension systems). These stories decompose the mismatch along domain lines per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pension_fund_liability_mismatch, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
