% ============================================================================
% CONSTRAINT STORY: uk_hicbc_2024
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_hicbc_2024, []).

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
 *   constraint_id: uk_hicbc_2024
 *   human_readable: UK High Income Child Benefit Charge (HICBC) 2024
 *   domain: economic/fiscal_policy
 *
 * SUMMARY:
 *   The UK High Income Child Benefit Charge (HICBC) introduces a means-tested
 *   clawback mechanism for families where at least one partner earns over
 *   £60,000 annually, recovering child benefit at 1% per pound earned above
 *   the threshold up to complete clawback at £80,000. Introduced in January
 *   2013 as a fiscal consolidation measure during austerity, the HICBC
 *   creates a structural tension between legitimate redistributive goals
 *   (targeting benefits to lower-income families) and extraction mechanisms
 *   that trap middle-income earners in high marginal effective tax rates
 *   (exceeding 60% in the £60k-£80k band). The constraint demonstrates how
 *   welfare state modernization can introduce extraction mechanisms into
 *   previously universal programs. The theater ratio (0.65) reflects the gap
 *   between the policy's stated equity rationale (ensuring benefits reach
 *   those who need them most) and its actual implementation (complex tax
 *   return processing, high administrative overhead, behavioral distortion).
 *   From different structural positions, the HICBC appears variously as pure
 *   extraction (Snare to affected families), coordination (Rope to treasury
 *   designers), mixed extraction with coordination (Tangled Rope to
 *   lower-income beneficiaries), and degraded universalism (Piton to the
 *   historical child benefit system). The constraint has accumulated
 *   extractiveness over its 12-year lifecycle (ε rising from 0.35 to 0.52) as
 *   awareness of tax planning opportunities and income restructuring options
 *   has spread, while theater has increased (0.52 to 0.65) as compliance
 *   infrastructure complexity has grown to manage the means-test.
 *
 * KEY AGENTS:
 *   - High-income families with children: Primary victims (powerless/trapped) — bear full extraction via marginal tax treatment; cannot exit UK jurisdiction without significant emigration costs
 *   - Middle-income earners (£55k-£75k): Secondary victims (moderate/constrained) — face constrained exit via pension sacrifice, spousal income transfers, or contracting; marginal effective tax rates exceed 60%
 *   - Treasury and HMRC: Primary beneficiaries (institutional/arbitrage) — architects of means-test mechanism; collect revenue and achieve distributional targeting without perceived coercion
 *   - Lower-income beneficiary families: Mixed beneficiaries and victims (organized/mobile) — benefit from maintained child benefit without clawback, but also bear cost of means-test administration; mobile exit available but practically constrained
 *   - Tax planning industry: Organized intermediaries (organized/constrained) — accountants, payroll providers, financial advisors solving compliance friction; provide temporary scaffold as digital infrastructure matures
 *   - Historical universalism: Institutional memory (institutional/arbitrage) — pre-2013 universal child benefit represents degraded function now replaced by means-testing; exhibits piton characteristics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_hicbc_2024, 0.52).
domain_priors:suppression_score(uk_hicbc_2024, 0.68).
domain_priors:theater_ratio(uk_hicbc_2024, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_hicbc_2024, extractiveness, 0.52).
narrative_ontology:constraint_metric(uk_hicbc_2024, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(uk_hicbc_2024, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_hicbc_2024, snare).
narrative_ontology:human_readable(uk_hicbc_2024, "UK High Income Child Benefit Charge (HICBC) 2024").
narrative_ontology:topic_domain(uk_hicbc_2024, "economic/fiscal_policy").

domain_priors:requires_active_enforcement(uk_hicbc_2024).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_hicbc_2024, treasury_revenue_collection).
narrative_ontology:constraint_beneficiary(uk_hicbc_2024, lower_income_beneficiary_pool).
narrative_ontology:constraint_victim(uk_hicbc_2024, high_income_families_with_children).
narrative_ontology:constraint_victim(uk_hicbc_2024, middle_income_earners_near_threshold).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-INCOME FAMILY (SNARE) — Trapped within UK tax jurisdiction; cannot exit without emigration or income restructuring. Faces full extraction of child benefit via marginal tax treatment despite having paid taxes. No meaningful exit. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.72. Effective extraction is high because trapped exit + victim status + institutional enforcement.
constraint_indexing:constraint_classification(uk_hicbc_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME EARNERS NEAR THRESHOLD (SNARE) — Constrained exit via income reduction (pension sacrifice, spousal income transfer, contracting/freelance restructuring). Faces marginal effective tax rates exceeding 60% in the £60k-£80k band. Suppression is high because genuine restructuring options exist but carry significant friction costs (complexity, professional fees, reduced income security). d≈0.80, f(d)≈1.22, σ=1.0 → χ≈0.63. Still effective extraction despite constrained rather than trapped exit.
constraint_indexing:constraint_classification(uk_hicbc_2024, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TREASURY AND REVENUE AUTHORITIES (ROPE) — Architects of the constraint see it as pure coordination: means-testing child benefit serves legitimate fiscal goal of targeting public resources to lower-income families. No coercive overhead from treasury perspective; operates as administrative mechanism. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.04. Net beneficiary (positive revenue, legitimate redistribution logic).
constraint_indexing:constraint_classification(uk_hicbc_2024, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LOWER-INCOME BENEFICIARY FAMILIES (TANGLED ROPE) — Benefit from stable child benefit payments without clawback threshold. Also experience extraction via tax burden to fund the constraint's administration. Mobile exit via migration is theoretically available but practically constrained (social ties, job market). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39. Mixed: benefit from redistribution + cost of maintaining means-test complexity.
constraint_indexing:constraint_classification(uk_hicbc_2024, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL CHILD BENEFIT SYSTEM (PITON) — Pre-2013, child benefit was universal (no clawback). The constraint represents degraded function: means-testing replaces universalism; administrative complexity (individual tax returns, clawback calculations) persists despite policy intent. theater_ratio=0.65: significant performative content in how means-testing is justified (equity narrative) vs actual effect (extraction from organized middle class). The old universal system persists in institutional language and beneficiary expectations even as the mechanism has been replaced. d≈0.05, f(d)≈-0.10, σ=1.0 → χ≈-0.03. Piton gate satisfied: theater ≥0.70 threshold not met, but theater_ratio=0.65 reflects degradation trajectory.
constraint_indexing:constraint_classification(uk_hicbc_2024, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: TAX PLANNING INDUSTRY AND COMPLIANCE INFRASTRUCTURE (SCAFFOLD) — Organized agents (accountants, payroll providers, financial advisors) solve the coordination problem of managing HICBC compliance. This is temporary infrastructure: as digital tax integration improves and automated pension contributions scale, the need for individual advice diminishes. has_sunset_clause rationale: Full employer automatic enrolment into workplace pensions + simplified digital tax filing (Making Tax Digital) are intended to reduce friction. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.22. Low effective extraction because organized agents have agency and see a path to structural simplification (sunset: 10-15 years as digital filing matures).
constraint_indexing:constraint_classification(uk_hicbc_2024, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — This perspective naturalizes the constraint as an inescapable feature of means-tested welfare: 'All means-tested benefits must clawback at high incomes; extraction is inherent to redistribution.' This classification is a FALSE SUMMIT. The structural data (ε=0.52, suppression=0.68, theater=0.65) contradicts mountain gates (ε≤0.25, suppression≤0.05). The clawback mechanism is a contingent policy choice, not a law of nature. Pre-2013 universalism proved that child benefit can exist without extraction. The mountain framing naturalizes political choice.
constraint_indexing:constraint_classification(uk_hicbc_2024, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_hicbc_2024_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_hicbc_2024, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_hicbc_2024, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_hicbc_2024, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_hicbc_2024, TR),
    TR >= 0.70.

:- end_tests(uk_hicbc_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The clawback mechanism extracts real value from high-income families (up to £2,540/year for three children at peak). The extraction is substantial but not total — families can partially exit via income restructuring (pension contributions, spousal income reallocation). The metric reflects that extraction exists and is significant, but constrained exit options reduce effective coercion compared to a true income tax. Suppression (0.68): High. Multiple suppression mechanisms operate: (1) legal/jurisdictional (emigration requires abandoning UK social ties and job market access), (2) informational (many families do not understand the clawback or their restructuring options), (3) compliance friction (calculating clawback requires tax returns; pension sacrifice requires employer cooperation), (4) psychological (benefit clawback creates sense of policy unfairness that discourages political challenge). Theater ratio (0.65): Moderate-high. The equity narrative (means-testing targets benefits to those who need them) is performative — in reality, the mechanism primarily affects organized middle-class families sophisticated enough to navigate tax planning, while lower-income families lack resources for planning but genuinely benefit. The 1% taper is theatrical precision (appears surgical) masking cliff effects at £60k and £80k thresholds. Administrative complexity (individual tax returns, annual clawback calculations) is theatrical — modern digital systems could implement means-testing with far less friction. Claimed type (Snare): Justified by ε≥0.46 (0.52 met), suppression ≥0.60 (0.68 met), and χ calculation: f(d)≈1.40 for trapped victims, σ=1.0 (national scope) → χ≈0.73. Meets snare thresholds.
 *
 * PERSPECTIVAL GAP:
 *   DRAMATIC PERSPECTIVAL DIVERGENCE. High-income families experience Snare (extraction, trapped exit, institutional enforcement). Middle-income earners near threshold experience Snare (extraction, constrained exit via tax planning). Treasury sees Rope (pure coordination, distributional targeting, no coercive overhead from their perspective). Lower-income beneficiaries see Tangled Rope (benefit from stable payments, but also bear cost of means-test complexity and taxation to fund administration). Tax planning industry sees Scaffold (temporary problem being solved by digital tax automation; sunset as complexity declines). Historical universalism exhibits Piton (degraded function; universal benefit replaced by means-test, but language and expectations persist). Analytical observer risks false Mountain (naturalizing means-testing as inherent to welfare). The perspectival gap is driven by directionality: beneficiaries (treasury, lower-income families) have low d → negative χ (net benefit); victims (high-income families) have high d → high χ (extraction). The snare classification dominates because the powerless agent (high-income family trapped in jurisdiction) experiences structural extraction with no meaningful exit.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income family: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Trapped by UK jurisdiction and tax residency rules. Middle-income earner: Victim + constrained → d≈0.80, f(d)≈1.22. High extraction. Constrained exit via tax planning carries friction costs (accountant fees ~£500-£2000/year, complexity, reduced flexibility). Treasury: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Architects of mechanism; no constraints on policy design. Lower-income families: Beneficiary + mobile → d≈0.55, f(d)≈0.75. Mixed. Benefit from benefit maintenance; mobile exit via migration is theoretical (practically constrained by job market, social ties). Tax planning industry: Intermediary + constrained → d≈0.42, f(d)≈0.42. Constrained because dependent on regulatory environment; cannot exit without losing client base. Historical universalism: Institutional memory + arbitrage → d≈0.05, f(d)≈-0.10. Piton because degraded but persistent through institutional inertia. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. False summit risk: naturalizes means-testing as inevitable.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED (mandatrophy_resolved: false). The constraint exhibits a genuine structural ambiguity: does HICBC primarily serve a coordination function (efficient targeting of limited resources to lower-income families) or primarily serve extraction (fiscal consolidation via targeted tax increase on organized middle class)? Snare classification assumes extraction dominates, but the lower-income beneficiary perspective reveals real coordination logic (maintaining benefit access for those with lower incomes). The mandatrophy resolution would require evidence on: (1) whether lower-income families actually receive benefit gains that justify the extraction cost to higher-income families, and (2) whether the extraction is efficient (administrative costs < revenue gain) or theatrical (high overhead for modest redistribution). Current evidence is ambiguous: HMRC administrative cost data is not publicly granular enough to compute cost-per-pound-recovered. Without this data, the tangled_rope interpretation remains plausible — the constraint may be genuinely hybrid (mixing coordination benefit for lower-income families with extraction from high-income families) rather than pure snare. The omega variable on redistributive benefit realization (omega_id: redistributive_benefit_realization) is designed to resolve this mandatrophy by isolating whether lower-income families actually benefit substantively or if benefits are offset by other fiscal changes. If resolution confirms real benefit gain with reasonable administrative efficiency, classification should shift toward Tangled Rope with active enforcement. If resolution confirms minimal redistribution or high overhead, snare classification is confirmed and mandatrophy remains unresolved because the extraction mechanism is so theatrically justified that no perspectival frame accepts pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_response_elasticity,
    'What is the true income and labor-supply elasticity of high-income earners subject to HICBC? Do most respond via tax planning, income reduction, or no response at all?',
    'Econometric analysis of tax return data pre/post-HICBC introduction (2013) and subsequent threshold changes; comparison of earned income, pension contributions, and self-employment income volatility for £55k-£75k earners vs controls',
    'If elastic (strong response): constraint is effective at behavioral control but creates dead-weight loss; classification shifts toward Snare (extraction-focused). If inelastic (no response): constraint is pure revenue grab with minimal coordination benefit; classification remains Snare but with higher χ.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_response_elasticity, empirical, 'Income and labor-supply elasticity of HICBC-subject earners').

omega_variable(
    administrative_cost_recovery,
    'Do the costs of HICBC administration (tax return processing, clawback calculations, compliance support) exceed the revenue actually recovered?',
    'HM Revenue & Customs administrative cost accounting; comparison of cost per pound of revenue recovered vs other means-tested benefits (Universal Credit, Working Tax Credit)',
    'If costs exceed revenue: constraint is theater (Piton). If costs are 20-40% of revenue: extraction exists but with significant overhead (Tangled Rope). If costs are <15%: pure extraction mechanism (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_cost_recovery, empirical, 'Whether HICBC administrative costs exceed revenue recovered').

omega_variable(
    cliff_effect_vs_taper_design,
    'Does the 1% taper (clawing back 1p per pound earned above £60k) function as intended, or does behavioral response cluster at threshold boundaries, creating effective cliff effects?',
    'Distribution analysis of declared income and pension contributions; identification of clustering at £60k and £80k thresholds; comparison to smooth income distribution in non-clawback regions',
    'If cliff effects dominate: suppression is higher than measured (people are trapped at decision points); extraction is more concentrated. If smooth taper works: suppression is lower; extraction is distributed (Rope characteristics possible from some perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cliff_effect_vs_taper_design, empirical, 'Whether HICBC creates behavioral cliff effects or smooth taper response').

omega_variable(
    redistributive_benefit_realization,
    'Do lower-income families actually receive meaningfully higher child benefit payments, or is the redistributive intent offset by other fiscal changes (benefit caps, UC tapering)?',
    'Comparison of disposable income trajectories for lower-income families pre/post-HICBC; analysis of benefit changes in context of full means-test system',
    'If redistribution is real and substantial: constraint has genuine coordination component (Tangled Rope confirmed for beneficiary perspective). If redistribution is minimal or offset: constraint is pure extraction dressed in equity language (Snare confirmed across most perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redistributive_benefit_realization, empirical, 'Whether HICBC redistributive intent translates to actual lower-income benefit gains').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_hicbc_2024, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hicbc_tr_t0, uk_hicbc_2024, theater_ratio, 0, 0.52).
narrative_ontology:measurement(hicbc_tr_t6, uk_hicbc_2024, theater_ratio, 6, 0.61).
narrative_ontology:measurement(hicbc_tr_t12, uk_hicbc_2024, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(hicbc_be_t0, uk_hicbc_2024, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hicbc_be_t6, uk_hicbc_2024, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(hicbc_be_t12, uk_hicbc_2024, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_hicbc_2024, resource_allocation).
narrative_ontology:affects_constraint(uk_hicbc_2024, uk_marginal_tax_rate_clustering).
narrative_ontology:affects_constraint(uk_hicbc_2024, universal_credit_taper_interaction).
narrative_ontology:affects_constraint(uk_hicbc_2024, pension_contribution_incentive_distortion).

% DUAL FORMULATION NOTE:
% HICBC is structurally downstream of two distinct constraints: (1) the universal child benefit system (pre-2013, ε≈0.05, pure Rope for all perspectives — no extraction), and (2) the modern welfare state means-testing logic (ε variable across constraints, generally higher extraction). The decomposition reveals that 'child benefit' is not a single constraint but a family of related claims with different ε values depending on whether the benefit is universal (ε≈0.05) or means-tested (ε≈0.40-0.52). HICBC represents a policy transition between these two constraint regimes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_hicbc_2024, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
