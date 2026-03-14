% ============================================================================
% CONSTRAINT STORY: progressive_taxation_fairness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_progressive_taxation_fairness, []).

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
 *   constraint_id: progressive_taxation_fairness
 *   human_readable: Progressive Taxation Fairness Constraint
 *   domain: economic_policy/taxation
 *
 * SUMMARY:
 *   Progressive taxation operates as a hybrid coordination-extraction
 *   mechanism. It coordinates public goods provision and income
 *   redistribution (genuine coordination function) while simultaneously
 *   extracting from high earners and capital owners through confiscatory
 *   marginal rates, enforcement asymmetries, and capital controls. The
 *   constraint exhibits structural tangling: the system cannot function as
 *   pure coordination because high earners would exit or evade; it cannot
 *   function as pure extraction because legitimate public goods require
 *   sustained cooperation and investment. The theater_ratio (0.65) reflects
 *   the gap between progressive taxation's rhetorical commitment to fairness
 *   and actual redistributive outcomes, which are increasingly captured or
 *   circumvented through tax avoidance. Over the 60-year interval (roughly
 *   1965-2025), both extractiveness and theater have increased as
 *   globalization enables capital mobility and asymmetries compound, while
 *   marginal rate enforcement has become increasingly theatrical (high
 *   nominal rates, low effective rates through deductions).
 *
 * KEY AGENTS:
 *   - High-Income Earners: Primary victims (moderate/constrained) — face progressive extraction with constrained exit options within jurisdiction; can relocate or restructure with significant costs
 *   - Capital Owners: Primary victims (powerful/arbitrage) — have global arbitrage options and can structure assets across jurisdictions; benefit from coordination but exploit extraction gaps
 *   - Low-Income Households: Primary beneficiaries (powerless/trapped) — trapped in jurisdiction; benefit from redistribution and public goods provision; experience constraint as enabling
 *   - Government Fiscal Authority: Institutional beneficiary (institutional/constrained) — coordinates public goods and welfare while extracting rents through efficiency gaps and political allocation
 *   - Public Goods/Welfare Programs: Beneficiary collective (powerless/trapped) — receive resources; experience constraint as coordination mechanism enabling service provision
 *   - Progressive Tax Ideology: Institutional-cultural actor (institutional/mobile) — maintains rhetorical commitment despite degraded functional efficacy; exhibits piton characteristics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(progressive_taxation_fairness, 0.58).
domain_priors:suppression_score(progressive_taxation_fairness, 0.52).
domain_priors:theater_ratio(progressive_taxation_fairness, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(progressive_taxation_fairness, extractiveness, 0.58).
narrative_ontology:constraint_metric(progressive_taxation_fairness, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(progressive_taxation_fairness, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(progressive_taxation_fairness, tangled_rope).
narrative_ontology:human_readable(progressive_taxation_fairness, "Progressive Taxation Fairness Constraint").
narrative_ontology:topic_domain(progressive_taxation_fairness, "economic_policy/taxation").

domain_priors:requires_active_enforcement(progressive_taxation_fairness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(progressive_taxation_fairness, low_income_households).
narrative_ontology:constraint_beneficiary(progressive_taxation_fairness, public_goods_provision).
narrative_ontology:constraint_beneficiary(progressive_taxation_fairness, welfare_programs).
narrative_ontology:constraint_victim(progressive_taxation_fairness, high_income_earners).
narrative_ontology:constraint_victim(progressive_taxation_fairness, capital_owners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-INCOME EARNER (SNARE) — Faces progressive tax extraction with high marginal rates and limited exit options within jurisdiction. Can relocate or restructure assets (constrained, not trapped), but relocation costs are substantial. Experiences the constraint as primarily extractive with minimal coordination benefit. Suppression through tax complexity, enforcement mechanisms, and capital controls.
constraint_indexing:constraint_classification(progressive_taxation_fairness, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: GLOBAL CAPITAL OWNER (TANGLED ROPE) — Has significant arbitrage options across jurisdictions with different tax regimes. Benefits from coordination aspects (stable infrastructure, rule of law, educated labor force funded by public goods), while simultaneously extracting through tax avoidance strategies (profit shifting, transfer pricing). Both genuine coordination function and asymmetric extraction are present. High effective extraction due to arbitrage exit options and global scope.
constraint_indexing:constraint_classification(progressive_taxation_fairness, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LOW-INCOME HOUSEHOLD (ROPE) — Trapped in jurisdiction by employment, family, and geographic constraints. Experiences progressive taxation primarily as coordination: contributes proportionally less, receives disproportionately more public goods and welfare transfers. Experiences constraint as enabling, not extractive. The system coordinates resource redistribution and ensures access to essential services.
constraint_indexing:constraint_classification(progressive_taxation_fairness, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: GOVERNMENT FISCAL AUTHORITY (TANGLED ROPE) — Coordinates public goods provision and welfare redistribution (genuine coordination function), while simultaneously extracting rents through tax collection efficiency gaps, bureaucratic overhead, and political allocation of tax revenue to favored constituencies. Faces pressure from both high-income taxpayers (exit threat through capital mobility) and low-income beneficiaries (demand for services). Constrained by both sovereign debt constraints and democratic legitimacy requirements.
constraint_indexing:constraint_classification(progressive_taxation_fairness, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE TAX IDEOLOGY (PITON) — The normative commitment to progressive taxation has partially atrophied from a functional coordination mechanism into performative legitimacy theater. Marginal rates are high in rhetoric but lower in practice through deductions, credits, and enforcement gaps. The theater of fairness persists (high theater_ratio) while actual redistribution is increasingly captured or circumvented. Maintained through institutional inertia and rhetorical commitment despite degraded functional efficacy.
constraint_indexing:constraint_classification(progressive_taxation_fairness, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global civilizational perspective, progressive taxation coordinates resource redistribution and funds public goods (coordination function), while simultaneously enabling extraction through capital mobility asymmetries, tax competition between jurisdictions, and regulatory arbitrage. The constraint exhibits both genuine coordination and structural extraction. The system is tangled rather than pure extraction or pure coordination.
constraint_indexing:constraint_classification(progressive_taxation_fairness, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(progressive_taxation_fairness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(progressive_taxation_fairness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(progressive_taxation_fairness, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(progressive_taxation_fairness, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(progressive_taxation_fairness, TR),
    TR >= 0.70.

:- end_tests(progressive_taxation_fairness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts from high-income earners and capital owners through progressive marginal rates, capital controls, and enforcement mechanisms. However, the extraction is not total (not 0.70+) because coordination aspects are genuine — public goods provision, rule of law, educated labor force, and infrastructure funded through taxation benefit all participants including high earners. The extractiveness has increased over time as capital mobility enables avoidance and jurisdictions compete, forcing higher nominal rates to maintain revenue. Suppression (0.52): Moderate. Barriers to exit include relocation costs, family/community ties, asset illiquidity, and legal/regulatory obstacles to capital flight. However, suppression is not total because global capital mobility and tax planning strategies provide partial exit. High-income earners face constraints but have options; low-income earners face traps but lack the resources to exercise exit. Theater ratio (0.65): Moderately high. The gap between statutory and effective tax rates reflects theater — marginal rates are high in rhetoric but lower in practice through tax deductions, credits, accelerated depreciation, and carried interest rules. The rhetoric of fairness persists while actual redistribution is eroded. Theater has increased over the interval as tax avoidance strategies have proliferated.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap reveals the constraint's tangled nature. High-income earners perceive extraction dominance (Snare classification: trapped, moderate power, biographical horizon). Low-income households perceive coordination dominance (Rope classification: genuine enablement of welfare and public goods access). Capital owners with global options perceive genuine tangling — they benefit from coordination (infrastructure, educated labor, rule of law) while exploiting extraction gaps (arbitrage, profit shifting). The government perceives tangling from a different angle — coordinating provision while simultaneously threatened by exit (capital flight) and managing demand from beneficiaries. The analytical observer at civilizational scope sees the constraint as tangled globally: jurisdictions coordinate for mutual benefit while competing through tax rates, creating extraction through regulatory arbitrage. The piton perspective (progressive tax ideology) reveals that the nominally functional system is increasingly maintained through theater rather than effective operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position and exit options. High-income earners are victims with constrained exit (d ≈ 0.75), experiencing higher effective extraction. Capital owners are structural beneficiaries with arbitrage options despite victim status in the tax code (d ≈ 0.55, offset by arbitrage exit), experiencing moderate extraction. Low-income households are beneficiaries with trapped exit (d ≈ 0.15), experiencing negative effective extraction (or coordination-as-benefit). The government is a complex institutional actor with constrained exit on both sides (threatened by capital flight, accountable to beneficiaries), producing moderate d (≈ 0.50). The progressivity of the constraint creates asymmetric directionality: the same tax system produces high chi for high earners and low/negative chi for low earners, depending on f(d) and their structural position. Suppression applies uniformly (not scaled by individual position), but experienced extraction is scaled by directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: Progressive taxation satisfies all three tangled rope gates: (1) Genuine coordination function exists — public goods provision, welfare redistribution, rule of law funding — these coordinate the collective benefit structure. (2) Asymmetric extraction is structural — high earners and capital owners bear extraction while low earners receive subsidy; the system is fundamentally asymmetric. (3) Active enforcement is required — tax collection, compliance monitoring, enforcement against avoidance are continuous and nontrivial. The mandatrophy is resolved by recognizing that the constraint simultaneously solves a collective action problem (funding public goods is a prisoner's dilemma without taxation) AND extracts rents (through enforcement inefficiency, administrative overhead, and political allocation of tax benefits). Neither classification (pure Rope or pure Snare) is correct; the system is tangled. The increasingness of both extractiveness and theater over the interval suggests that the tangling is becoming more asymmetric — the coordination benefit persists while extraction mechanisms proliferate through tax avoidance and enforcement gaps.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_mobility_asymmetry,
    'Does the observed capital flight and profit shifting represent rational response to extractive taxation or structural reality of global capital mobility?',
    'Comparative analysis of effective tax rates (statutory vs. actual) across jurisdictions; measurement of capital flight volumes and elasticity to tax changes; cross-country regression of investment flows on tax differentials',
    'If primarily extractive response: suppression value should increase to 0.65+, shifting more perspectives toward Snare. If primarily structural inevitability: suppression remains moderate, confirming Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_mobility_asymmetry, empirical, 'Whether capital mobility response indicates extraction or structural reality').

omega_variable(
    enforcement_mechanism_efficacy,
    'What proportion of the tax-evasion suppression is structural (inherent difficulty of verifying offshore income) versus institutional choice (insufficient enforcement resources)?',
    'Audit rate analysis; comparison of effective enforcement between countries with high vs low compliance infrastructure investment; measurement of revenue recovery from increased enforcement spending',
    'If structural: suppression is inherent to complexity (theater increases). If institutional choice: suppression is politically determined (extraction mechanism is amplified through deliberate underenforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_efficacy, empirical, 'Structural vs institutional sources of enforcement gaps').

omega_variable(
    redistribution_effectiveness,
    'Does progressive taxation actually reduce inequality, or does the extraction offset and rhetorical theater obscure minimal net redistribution?',
    'Time-series analysis of Gini coefficient before/after-tax; comparison of post-tax inequality across countries with different progressivity levels; decomposition of inequality change into tax vs transfer components',
    'If effective redistribution: Rope classification gains weight (genuine coordination outcome). If theater dominates: Piton classification gains weight (performative inequality reduction with inertial maintenance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(redistribution_effectiveness, empirical, 'Whether progressive taxation achieves actual or performative redistribution').

omega_variable(
    tax_base_elasticity,
    'At what progressivity level does behavioral response (avoidance, evasion, labor supply reduction) reverse the redistributive function?',
    'Laffer curve estimation for different tax bases; elasticity measurement of taxable income, capital gains, and labor supply to marginal rate changes; cross-country comparison of revenue optimization points',
    'If reversal occurs at moderate progressivity (30-40% marginal rates): constraint may be Scaffold with sunset (progressive taxation becomes counterproductive). If reversal is asymptotic and high (60%+): Tangled Rope holds across all practical progressivity ranges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_base_elasticity, empirical, 'Tax base elasticity and revenue-optimization threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(progressive_taxation_fairness, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prog_tr_t0, progressive_taxation_fairness, theater_ratio, 0, 0.45).
narrative_ontology:measurement(prog_tr_t20, progressive_taxation_fairness, theater_ratio, 20, 0.55).
narrative_ontology:measurement(prog_tr_t40, progressive_taxation_fairness, theater_ratio, 40, 0.65).
narrative_ontology:measurement(prog_tr_t60, progressive_taxation_fairness, theater_ratio, 60, 0.68).

% Extraction over time
narrative_ontology:measurement(prog_be_t0, progressive_taxation_fairness, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(prog_be_t20, progressive_taxation_fairness, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(prog_be_t40, progressive_taxation_fairness, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(prog_be_t60, progressive_taxation_fairness, base_extractiveness, 60, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(progressive_taxation_fairness, resource_allocation).
narrative_ontology:affects_constraint(progressive_taxation_fairness, capital_mobility_constraint).
narrative_ontology:affects_constraint(progressive_taxation_fairness, tax_competition_between_jurisdictions).
narrative_ontology:affects_constraint(progressive_taxation_fairness, income_inequality_reduction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(progressive_taxation_fairness, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
