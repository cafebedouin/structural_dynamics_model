% ============================================================================
% CONSTRAINT STORY: qualified_business_income_deduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_business_income_deduction, []).

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
 *   constraint_id: qualified_business_income_deduction
 *   human_readable: Qualified Business Income Deduction (Section 199A)
 *   domain: tax_policy/economic_inequality
 *
 * SUMMARY:
 *   The Qualified Business Income (QBI) Deduction, enacted as Section 199A of
 *   the Tax Cuts and Jobs Act of 2017, permits eligible taxpayers to deduct
 *   up to 20% of qualified business income from pass-through entities
 *   (S-corporations, partnerships, sole proprietorships, LLCs). The deduction
 *   is scheduled to expire December 31, 2025, creating a built-in sunset. The
 *   constraint exhibits hybrid coordination-extraction dynamics: it
 *   simplifies taxation of pass-through businesses (genuine coordination
 *   function) while creating asymmetric tax relief concentrated among
 *   high-income owners, funded through reduced revenue borne
 *   disproportionately by W2 wage earners. The deduction's interaction with
 *   W2 wage and business asset limitations, combined with high compliance
 *   complexity and IRS guidance theater, creates differential access and
 *   effective extraction rates across income levels.
 *
 * KEY AGENTS:
 *   - W2 Wage Earners: Primary victims (powerless/trapped) — bear tax burden from reduced QBI revenue without access to deduction
 *   - Low-Income Self-Employed: Secondary victims (moderate/constrained) — theoretically eligible but face compliance barriers and phase-out restrictions; also receive minimal benefit
 *   - High-Income Pass-Through Owners: Primary beneficiaries (institutional/arbitrage) — capture bulk of deduction benefit; can structure business form to optimize access
 *   - Tax Preparation Industry: Institutional beneficiary (institutional/arbitrage) — captures compliance costs through premium consulting and tax filing services
 *   - Tax Reform Coalition: Organized opposition (organized/constrained) — progressive policy organizations advocating for deduction elimination or modification before sunset
 *   - IRS and Treasury: Institutional administrator (institutional/arbitrage) — issues complex guidance; maintains theater of compliance and validation
 *   - Analytical Observer: Cross-position analysis (analytical/analytical) — identifies coordination function coupled with asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_business_income_deduction, 0.52).
domain_priors:suppression_score(qualified_business_income_deduction, 0.48).
domain_priors:theater_ratio(qualified_business_income_deduction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_business_income_deduction, extractiveness, 0.52).
narrative_ontology:constraint_metric(qualified_business_income_deduction, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(qualified_business_income_deduction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_business_income_deduction, tangled_rope).
narrative_ontology:human_readable(qualified_business_income_deduction, "Qualified Business Income Deduction (Section 199A)").
narrative_ontology:topic_domain(qualified_business_income_deduction, "tax_policy/economic_inequality").

domain_priors:requires_active_enforcement(qualified_business_income_deduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_business_income_deduction, pass_through_business_owners).
narrative_ontology:constraint_beneficiary(qualified_business_income_deduction, high_income_self_employed).
narrative_ontology:constraint_victim(qualified_business_income_deduction, w2_wage_earners).
narrative_ontology:constraint_victim(qualified_business_income_deduction, low_income_business_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: W2 WAGE EARNER (SNARE) — Cannot exit wage employment without massive career disruption. Bears full cost of QBI deduction through higher effective tax rates and reduced public revenue. No coordination benefit. Maximum extraction from a structural position of immobility.
constraint_indexing:constraint_classification(qualified_business_income_deduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-INCOME SELF-EMPLOYED (TANGLED ROPE) — Can theoretically access QBI deduction but faces high compliance costs, audit risk, and contingent access (income phase-outs and W2 wage/asset limitations). Benefits from some coordination (simplified business taxation framework) alongside extraction through asymmetric deduction access. Constrained by tax complexity and resource barriers.
constraint_indexing:constraint_classification(qualified_business_income_deduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-INCOME PASS-THROUGH OWNER (ROPE) — Primary beneficiary. Experiences QBI deduction as coordination mechanism: enables business taxation simplification while providing substantial tax relief. Can exit through incorporation or other strategies if needed. Arbitrage access to multiple tax structures. Extraction runs toward this agent.
constraint_indexing:constraint_classification(qualified_business_income_deduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TAX REFORM COALITION (SCAFFOLD) — Organized agents (tax reform advocates, progressive policy organizations) see QBI deduction as temporary artifact of 2017 Tax Cuts and Jobs Act with sunset provision (expires 2025 without extension). Constraint has built-in termination mechanism. Coalition perceives window for reform before permanent tax code restructuring.
constraint_indexing:constraint_classification(qualified_business_income_deduction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TAX CODE ADMINISTRATION (PITON) — The deduction persists through institutional inertia and political incumbency despite high compliance theater. IRS guidance has proliferated (substantial theater in Treasury regulations, Notice 2018-40, Rev. Proc. 2019-11). The administrative function (calculating and validating qualified business income) has largely been outsourced to private tax preparation industry. Theater ratio (0.58) reflects that substantial administrative effort is devoted to determining eligibility rather than producing revenue optimization.
constraint_indexing:constraint_classification(qualified_business_income_deduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees coordination function (simplification of pass-through taxation) coupled with significant asymmetric extraction (wealth concentration, revenue loss disproportionately borne by wage earners). Classification is not mountain (contingent on policy design) or pure snare (some legitimate business coordination benefit exists). Pure rope classification fails because extraction mechanism is robust and structural, not marginal.
constraint_indexing:constraint_classification(qualified_business_income_deduction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_business_income_deduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qualified_business_income_deduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qualified_business_income_deduction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_business_income_deduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(qualified_business_income_deduction, TR),
    TR >= 0.70.

:- end_tests(qualified_business_income_deduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The deduction generates substantial revenue cost (~$1.5 trillion over ten years) concentrated among high-income pass-through owners, reducing average tax rates on business income while W2 rates remain unchanged. Base extraction value reflects that this is legitimate coordination (pass-through business simplification) embedded in significant wealth transfer. The value increased from 0.35 (2017) to 0.52 (2025) as utilization stabilized and high-income owners optimized structures. Not higher (0.66+) because W2 wage limitations and income phase-outs provide partial constraints on unbounded extraction. Suppression (0.48): Moderate. Barriers to access include: (1) compliance complexity requiring professional tax advice, (2) W2 wage limitations that reduce deduction for service businesses, (3) income phase-outs starting at $182.9K (2023), (4) business asset limitations ($220K threshold), (5) audit risk. These are real barriers but not absolute — high-income owners can and do access deduction through structure optimization. Theater ratio (0.58): Moderate-high. Reflects proliferation of IRS guidance (Notice 2018-40, Treasury Regulations 1.199A, multiple revenue procedures) that creates appearance of rigorous validation but largely documents boundaries rather than enforcing them. Professional tax preparation industry captures premium for navigating this theater. Theater has grown as guidance accumulated over interval.
 *
 * PERSPECTIVAL GAP:
 *   The gap between W2 earners' Snare (no benefit, bears cost, trapped) and high-income owners' Rope (coordination benefit, benefits from deduction, arbitrage access) is the core analytical insight. Both groups experience the same constraint structure, but their structural positions invert the directionality: what the beneficiary experiences as legitimate simplification, the trapped agent experiences as extraction. The Scaffold perspective (organized coalition sees sunset) contrasts with Piton perspective (administrative system perpetuates through inertia). The analytical observer's Tangled Rope classification unifies these perspectives by recognizing both genuine coordination and genuine asymmetric extraction are structurally real — the constraint is not pure Rope (misleading the coordination) or pure Snare (ignoring the coordination function). The perspectival gap reveals that tax policy designed as 'simplification' becomes mechanism for wealth concentration when combined with asymmetric access.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by beneficiary/victim status and exit capacity. High-income pass-through owners (beneficiary + arbitrage exit) have d ≈ 0.15, producing negative or minimal f(d). W2 earners (victim + trapped exit) have d ≈ 0.90, producing maximum f(d) ≈ 1.42. Low-income self-employed (ambiguous: theoretically beneficiary but victim in practice via phase-outs and compliance barriers) have d ≈ 0.55-0.65, constrained exit. The analytical observer (victim of revenue loss to public goods, analytical exit) has d ≈ 0.72. Directionality overrides are not needed — the derivation chain accurately reflects structural relationships. Beneficiary/victim declarations directly drive the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by distinguishing coordination function (pass-through simplification) from extraction mechanism (wealth concentration via tax relief concentrated on high-income owners). The Tangled Rope classification captures both: χ ≈ 0.52 × 0.40 × 1.0 ≈ 0.21 for high-income owners (low extraction experienced), but χ ≈ 0.52 × 1.42 × 1.0 ≈ 0.74 for trapped W2 earners (high extraction experienced). The constraint is not mislabeled as pure Rope (which would ignore extraction) or pure Snare (which would ignore coordination). The 2025 sunset creates genuine scaffold element that distinguishes this from permanent extraction mechanisms. The false summit risk (mountain classification naturalizing as 'inevitable business taxation complexity') is rejected because the policy design is contingent: alternative structures (reducing pass-through rates, eliminating W2 wage limits, means-testing) could reduce extraction while preserving coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_limit_compliance,
    'Are the W2 wage and business asset limitations effectively preventing high-income pass-through owners from extracting full deduction benefit, or do sophisticated structures routinely circumvent these limits?',
    'IRS audit data on W2 wage limitation disallowances; empirical frequency of S-corp vs LLC election patterns among high-income self-employed; analysis of employed family members and wage allocation structures',
    'If limitations are effective: extraction is moderate, classification remains Tangled Rope. If routinely circumvented: extraction increases to 0.65+, reclassify as Snare for high-income earners; suppression becomes irrelevant (no real barrier exists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_limit_compliance, empirical, 'Effectiveness of W2 wage limitations in preventing deduction extraction').

omega_variable(
    substitution_vs_income_effect,
    'Does the QBI deduction expand pass-through business formation (income effect, creating coordination) or primarily reduce tax collection from existing businesses that would have organized regardless (pure extraction)?',
    'Time-series analysis of pass-through business formation rates pre/post 2017 TCJA; comparison to W2 employment trends; causal identification via difference-in-differences across states or industry cohorts',
    'If significant expansion: coordination component is real, Tangled Rope confirmed. If minimal expansion: deduction is pure transfer to existing businesses, reclassify as Snare; extraction rises to 0.70+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_income_effect, empirical, 'Whether QBI deduction expands business formation or transfers to existing businesses').

omega_variable(
    sunset_credibility,
    'Is the 2025 sunset of the individual QBI deduction (while corporate rate remains 21%) politically credible, or is permanent extension highly probable?',
    'Legislative history; polling on tax reform sentiment; structural political economy of pass-through vs corporate lobbying; budget impact analysis',
    'If sunset enforced: Scaffold perspective gains force, constraint has real termination date. If sunset becomes permanent through political pressure: Piton classification strengthens (institutional inertia overcomes sunset mechanism), extraction becomes institutionalized long-term.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_credibility, preference, 'Political credibility of QBI deduction sunset in 2025').

omega_variable(
    incidence_distribution,
    'What fraction of QBI deduction benefit flows to top 1% vs top 10% vs middle-income pass-through owners? Does distribution justify asymmetric extraction classification?',
    'IRS tax statistics by income bracket; Treasury analysis of QBI deduction utilization; cross-tabulation of deduction amount against filing income percentiles',
    'If >70% flows to top 1%: extraction mechanism is clearly concentrated, Snare perspective valid for vast majority. If more evenly distributed: some middle-income benefit exists, Tangled Rope better describes average experience.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incidence_distribution, empirical, 'Income distribution of QBI deduction benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_business_income_deduction, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qbid_tr_t0, qualified_business_income_deduction, theater_ratio, 0, 0.4).
narrative_ontology:measurement(qbid_tr_t4, qualified_business_income_deduction, theater_ratio, 4, 0.52).
narrative_ontology:measurement(qbid_tr_t8, qualified_business_income_deduction, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(qbid_be_t0, qualified_business_income_deduction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qbid_be_t4, qualified_business_income_deduction, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(qbid_be_t8, qualified_business_income_deduction, base_extractiveness, 8, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_business_income_deduction, resource_allocation).
narrative_ontology:affects_constraint(qualified_business_income_deduction, pass_through_entity_taxation).
narrative_ontology:affects_constraint(qualified_business_income_deduction, wage_income_tax_equity).
narrative_ontology:affects_constraint(qualified_business_income_deduction, tax_compliance_burden).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
