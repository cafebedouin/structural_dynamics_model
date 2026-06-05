% ============================================================================
% CONSTRAINT STORY: 1962_kennedy_permanent_unemployment_compensation_strengthening
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1962_kennedy_permanent_unemployment_compensation_strengthening, []).

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
 *   constraint_id: 1962_kennedy_permanent_unemployment_compensation_strengthening
 *   human_readable: Permanent Strengthening of Unemployment Compensation System
 *   domain: economic_policy/labor/social_insurance
 *
 * SUMMARY:
 *   The permanent strengthening of unemployment compensation represents a
 *   fundamental shift in how liberal democracies approach income security
 *   during involuntary joblessness. Rather than treating unemployment support
 *   as a temporary emergency measure requiring repeated congressional action
 *   during recessions, this constraint institutionalizes adequacy as
 *   automatic: benefits are formula-driven, scale with unemployment
 *   conditions, and maintain workers' purchasing power without requiring
 *   political intervention. This structural reform delinks benefit adequacy
 *   from the electoral cycle and creates a genuine automatic stabilizer for
 *   aggregate demand. The constraint exhibits the full range of DR
 *   classifications because it simultaneously coordinates genuine collective
 *   action (risk pooling across the employed population) and extracts
 *   asymmetrically (permanent tax obligations on contributors and federal
 *   fiscal burden). The perspectival gap reveals that the constraint's
 *   character depends on observer position: jobless workers see pure
 *   coordination; contributors see mixed extraction and benefit; powerful
 *   firms see competitive pressure and demand stabilization; the government
 *   sees fiscal obligation and political relief; and low-wage workers without
 *   alternative income sources see insufficient protection requiring
 *   joblessness as the trigger condition.
 *
 * KEY AGENTS:
 *   - Jobless Workers: Primary beneficiary (powerless/trapped) — benefit from adequate income protection during unemployment; coordination solves their individual income security problem
 *   - Employed Payroll Tax Contributors: Primary victim (moderate/constrained) — bear permanent tax obligations; benefit from unemployment risk pooling and demand stabilization
 *   - Business Enterprises: Secondary actor (powerful/mobile) — bear employer-side payroll contributions; benefit from consumer base stability
 *   - Federal Government: Institutional actor (institutional/constrained) — manages fiscal burden and creates automatic stabilizer mechanism; constrained by budget sustainability
 *   - Low-Wage Workers Without Savings: Vulnerable population (powerless/trapped) — benefit from benefits only when unemployed, remain trapped in precarity otherwise
 *   - Analytical Observer: Civilizational perspective (institutional/arbitrage) — sees constraint as mixed coordination-extraction system with distributional consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1962_kennedy_permanent_unemployment_compensation_strengthening, 0.35).
domain_priors:suppression_score(1962_kennedy_permanent_unemployment_compensation_strengthening, 0.48).
domain_priors:theater_ratio(1962_kennedy_permanent_unemployment_compensation_strengthening, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1962_kennedy_permanent_unemployment_compensation_strengthening, extractiveness, 0.35).
narrative_ontology:constraint_metric(1962_kennedy_permanent_unemployment_compensation_strengthening, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(1962_kennedy_permanent_unemployment_compensation_strengthening, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1962_kennedy_permanent_unemployment_compensation_strengthening, tangled_rope).
narrative_ontology:human_readable(1962_kennedy_permanent_unemployment_compensation_strengthening, "Permanent Strengthening of Unemployment Compensation System").
narrative_ontology:topic_domain(1962_kennedy_permanent_unemployment_compensation_strengthening, "economic_policy/labor/social_insurance").

domain_priors:requires_active_enforcement(1962_kennedy_permanent_unemployment_compensation_strengthening).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1962_kennedy_permanent_unemployment_compensation_strengthening, jobless_workers).
narrative_ontology:constraint_beneficiary(1962_kennedy_permanent_unemployment_compensation_strengthening, aggregate_demand_stabilizers).
narrative_ontology:constraint_victim(1962_kennedy_permanent_unemployment_compensation_strengthening, payroll_tax_contributors).
narrative_ontology:constraint_victim(1962_kennedy_permanent_unemployment_compensation_strengthening, federal_budget_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JOBLESS WORKER (ROPE) — Trapped in joblessness (biological and economic need for income), but the permanent automatic system coordinates genuine collective action: pooling risk across the employed population to ensure purchasing power during involuntary unemployment. The worker benefits from both the coordination function (risk pooling) and the adequacy design (maintaining living standards). Experiences this as pure coordination despite being trapped — the constraint exists to solve their coordination problem with employed contributors.
constraint_indexing:constraint_classification(1962_kennedy_permanent_unemployment_compensation_strengthening, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYED PAYROLL TAX CONTRIBUTOR (TANGLED ROPE) — Constrained by mandatory payroll taxation and social insurance norms. Experiences both coordination (unemployment risk pooling protects the contributor if they become jobless) and asymmetric extraction (permanent strengthening increases their marginal tax burden compared to minimal pre-reform system). The constraint coordinates genuine risk-sharing while extracting automatic revenue. Moderate power because contributors can exit partially through income mobility or tax arbitrage but face high social and legal barriers.
constraint_indexing:constraint_classification(1962_kennedy_permanent_unemployment_compensation_strengthening, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BUSINESS ENTERPRISE (TANGLED ROPE) — Mobile relative to payroll taxation (can relocate, adjust compensation structures, invest in automation) but benefits from a stabilized consumer base. Permanent unemployment strengthening coordinates demand stability (employees of other firms remain solvent customers) while extracting via higher employer-side payroll contributions. Powerful agents can model the cost-benefit calculus and may exit through relocation or capital substitution. Net extraction depends on elasticity of labor demand and capital mobility.
constraint_indexing:constraint_classification(1962_kennedy_permanent_unemployment_compensation_strengthening, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT (SCAFFOLD) — Constrained by fiscal sustainability and electoral cycles. Permanent automatic unemployment strengthening is a scaffold: it solves the crisis-driven politics of temporary supplemental benefits by converting emergency measures into structural formula-driven payments. Theater is low (the mechanism is mechanically simple: triggers automatically when unemployment rises, recedes when it falls). The scaffold has an implicit sunset: if structural unemployment falls substantially or if labor dynamics change fundamentally, the need for generous permanent benefits may decline. Federal coordination function is creating counter-cyclical automatic stabilization; extraction is the permanent fiscal burden. Generational perspective reveals that this is temporary support for a specific historical phase of labor market volatility.
constraint_indexing:constraint_classification(1962_kennedy_permanent_unemployment_compensation_strengthening, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LOW-WAGE WORKERS WITHOUT SAVINGS (SNARE) — Trapped in precarious employment and dependency on wages for survival. From their generational perspective, permanent unemployment strengthening alone is insufficient — the constraint extracts by requiring joblessness as the trigger condition. Even with adequate benefits, the worker must become involuntarily unemployed to receive support. This perspective sees pure extraction: the constraint requires a catastrophic status change (job loss) to deliver protection. Suppression is high because the worker cannot avoid the employment system or build independent security.
constraint_indexing:constraint_classification(1962_kennedy_permanent_unemployment_compensation_strengthening, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: INSURANCE INDUSTRY INTERMEDIARY (PITON) — Arbitrage position (can route capital flows, lobby for policy design that maintains intermediary role). Permanent unemployment strengthening converts unemployment support from state-managed direct provision to a hybrid system where private unemployment insurance, state programs, and federal supplements coexist in a complex regulatory architecture. The intermediary maintains roles in claims adjudication, fraud detection, and fund management, but the actual coordination function (risk pooling) is performed directly by the social insurance mechanism. Theater is high (intermediaries perform compliance, dispute resolution, actuarial framing) but function is degraded — the direct coordination mechanism is simpler and more effective. Piton classification reflects institutional inertia: intermediaries persist through regulatory capture and path dependency rather than because their services are essential to the core coordination.
constraint_indexing:constraint_classification(1962_kennedy_permanent_unemployment_compensation_strengthening, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global scope, permanent unemployment strengthening is a mixed coordination-extraction system. It coordinates counter-cyclical demand stabilization (genuine collective action problem solved: individual workers cannot maintain purchasing power during unemployment, yet aggregate demand maintenance benefits all participants). It simultaneously extracts by creating permanent fiscal obligations and implicit redistribution from capital to labor through payroll taxation. Global perspective reveals competitiveness effects: permanent high unemployment benefits create wage-floor effects and labor cost differentials relative to lower-protection jurisdictions. The constraint is neither pure coordination nor pure extraction but a structured hybrid.
constraint_indexing:constraint_classification(1962_kennedy_permanent_unemployment_compensation_strengthening, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1962_kennedy_permanent_unemployment_compensation_strengthening_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1962_kennedy_permanent_unemployment_compensation_strengthening, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1962_kennedy_permanent_unemployment_compensation_strengthening, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(1962_kennedy_permanent_unemployment_compensation_strengthening, TR),
    TR >= 0.70.

:- end_tests(1962_kennedy_permanent_unemployment_compensation_strengthening_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint coordinates genuine collective action (unemployment risk pooling, demand stabilization) but creates permanent asymmetric obligations on contributors and the federal budget. The extractiveness is not high because the coordination function is real and benefits contributors through reduced unemployment risk for themselves. It is not low because the permanent tax obligations and fiscal burden represent genuine extraction from specific groups (payroll taxpayers, federal budget bearers). Theater ratio (0.38): Low-moderate. The mechanism is relatively simple and mechanically driven: unemployment triggers automatic benefit increases; low unemployment triggers decreases. Theater increases modestly over the interval as administrative complexity grows (compliance frameworks, fraud detection) but the core coordination is functional, not theatrical. Suppression (0.48): Moderate. Contributors face mandatory payroll taxation and are suppressed by social norms of supporting the jobless, but are not completely suppressed — they can partially exit through income mobility, tax arbitrage, or capital relocation. Jobless workers face suppression from biological need and employment-dependency but are partially liberated by adequate benefits.
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival gap is between jobless workers and payroll contributors. Jobless workers see pure coordination (rope) — the constraint solves their collective income security problem with no extraction apparent. Payroll contributors see tangled rope — genuine coordination of unemployment risk alongside permanent tax extraction. Powerful firms see tangled rope with mobile escape options — the constraint coordinates demand stability while extracting through payroll costs, but firms can arbitrage through relocation or substitution. Low-wage workers without savings see snare — they benefit only when unemployed (crisis state) and remain trapped in precarity otherwise. The federal government sees scaffold — a temporary solution to the crisis-driven politics of supplemental benefits, with an implicit sunset as labor market conditions evolve. Insurance intermediaries see piton — a degraded institutional role maintained through regulatory capture rather than essential function. The analytical observer sees tangled rope with competitive effects — the constraint coordinates genuine stabilization while extracting through permanent fiscal obligations and creating wage-floor effects that may harm low-skill employment.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across agent types. Jobless workers with trapped exit options experience low d (they are pure beneficiaries of the coordination function) and thus low χ. Payroll contributors with constrained exit options experience high d (they bear costs) and moderate χ reflecting their partial benefit from unemployment risk pooling. Employers with mobile exit options experience moderate d (they bear contributions but benefit from demand stability) and χ reflects the competitive calculus. The federal government as institutional actor with arbitrage options experiences low d (it gains policy authority and fiscal capacity) despite bearing budget costs — institutional actors can use fiscal obligations for other political goals. Low-wage workers without savings experience high d (they must become unemployed to benefit) but are structurally trapped, increasing their experienced extraction. The piton perspective (insurance intermediaries) has low d (they benefit from regulatory role) despite not bearing primary costs — their arbitrage position allows them to maintain rents through institutional design rather than genuine coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that permanent unemployment strengthening is genuinely a tangled rope: it coordinates real collective action (unemployment risk pooling, automatic demand stabilization) while simultaneously extracting through permanent tax obligations and redistributing income from contributors to beneficiaries. The perspectival gap reflects real structural differences in how agents experience the constraint, not measurement error. The jobless worker perspective (rope) is valid because the constraint solves their coordination problem. The contributor perspective (tangled rope) is valid because they bear costs alongside benefits. The low-wage worker without savings perspective (snare) is valid because they must become unemployed to benefit. These are not contradictions but structural facts: the same constraint coordinates for some agents while extracting from others. The tangled rope classification at the analytical level captures this hybrid: there is genuine coordination (eliminating the constraint would harm all agents through loss of demand stabilization and unemployment risk pooling) and genuine extraction (the permanent fiscal burden and redistribution create asymmetric costs). The constraint cannot be classified as pure coordination (rope) because the costs are not equally distributed and some agents bear significant extraction. It cannot be classified as pure extraction (snare) because the coordination function is real and benefits even contributors through reduced unemployment risk. Tangled rope is the accurate classification because both functions are present and essential to the constraint's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adequacy_threshold_definition,
    'What percentage of prior wage constitutes ''adequate'' purchasing power maintenance — and how is that adequacy measured across income levels?',
    'Empirical comparison of jobless-period consumption patterns and poverty rates at different replacement ratios (50%, 66%, 80%, 90%); correlation with return-to-work employment quality',
    'If adequacy threshold is high (80%+): constraints on work incentives and labor market friction increase (snare extraction becomes salient). If threshold is low (50%): adequacy goal is not achieved, constraint becomes facade. Threshold directly determines whether this is genuine coordination or partially extractive benefit reduction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adequacy_threshold_definition, empirical, 'Definition of adequate benefit level to maintain purchasing power').

omega_variable(
    fiscal_sustainability_horizon,
    'Over what time horizon is permanent strengthening fiscally sustainable without requiring payroll tax increases that exceed labor cost absorption capacity?',
    'Long-run labor demand elasticity modeling; historical comparison to other nations'' permanent unemployment insurance (Germany, Netherlands, Denmark); sensitivity analysis on full-employment unemployment rates and benefit duration',
    'If sustainable indefinitely: constraint is genuine coordination (risk pooling cost is manageable). If unsustainable beyond 20-30 years: constraint is partially extractive (transfers intergenerational fiscal burden to future workers). Affects whether scaffold classification (with sunset) is more appropriate than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_horizon, empirical, 'Long-term fiscal sustainability of permanent generous benefits').

omega_variable(
    labor_market_incentive_effects,
    'Do permanent adequate unemployment benefits reduce job-search intensity and employment duration in ways that harm both individual worker prospects and aggregate labor utilization?',
    'Quasi-experimental analysis of benefit changes (state-level variation, temporary federal supplements); comparison of return-to-work employment quality and wage trajectories; job-search behavior modeling',
    'If incentive effects are significant: constraint creates snare-like properties even for beneficiaries (workers become trapped in unemployment by reduced work incentives, leading to worse long-term outcomes). If effects are minimal: coordination function is preserved. Affects classification from jobless worker perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_incentive_effects, empirical, 'Work incentive effects of permanent adequate unemployment benefits').

omega_variable(
    political_economy_lock_in,
    'Once permanent generous unemployment benefits are institutionalized, can they be reduced or modified without triggering profound political backlash and institutional path dependence?',
    'Historical analysis of benefit reductions in other nations; political economy modeling of constituency lock-in; welfare state retrenchment literature',
    'If lock-in is strong: this constraint becomes piton-like (the permanent mechanism, once established, persists regardless of changing labor market conditions, creating theatrical maintenance costs). If lock-in is weak: the scaffold''s sunset is credible (reform can adjust benefits as conditions change). Affects characterization of sustainability and institutional flexibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_economy_lock_in, preference, 'Political sustainability and reversibility of permanent benefit systems').

omega_variable(
    wage_compression_and_labor_supply,
    'Does permanent adequate unemployment insurance create wage floors and labor supply elasticity changes that compress wage inequality but also reduce low-skill employment opportunities?',
    'Comparative labor market data across high vs. low unemployment-protection jurisdictions; wage distribution analysis; employment rate analysis by skill level',
    'If compression occurs with employment loss for low-skill workers: constraint extracts from that population (snare properties). If compression occurs with maintained employment: constraint achieves coordination without extraction. Affects whether the constraint genuinely benefits all workers or creates hidden extraction channels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_compression_and_labor_supply, empirical, 'Wage and employment distributional effects of permanent benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1962_kennedy_permanent_unemployment_compensation_strengthening, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(puc_tr_t0, 1962_kennedy_permanent_unemployment_compensation_strengthening, theater_ratio, 0, 0.22).
narrative_ontology:measurement(puc_tr_t5, 1962_kennedy_permanent_unemployment_compensation_strengthening, theater_ratio, 5, 0.32).
narrative_ontology:measurement(puc_tr_t10, 1962_kennedy_permanent_unemployment_compensation_strengthening, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(puc_be_t0, 1962_kennedy_permanent_unemployment_compensation_strengthening, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(puc_be_t5, 1962_kennedy_permanent_unemployment_compensation_strengthening, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(puc_be_t10, 1962_kennedy_permanent_unemployment_compensation_strengthening, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1962_kennedy_permanent_unemployment_compensation_strengthening, resource_allocation).
narrative_ontology:affects_constraint(1962_kennedy_permanent_unemployment_compensation_strengthening, counter_cyclical_fiscal_policy).
narrative_ontology:affects_constraint(1962_kennedy_permanent_unemployment_compensation_strengthening, labor_market_wage_determination).
narrative_ontology:affects_constraint(1962_kennedy_permanent_unemployment_compensation_strengthening, federal_budget_sustainability).

% DUAL FORMULATION NOTE:
% Permanent unemployment strengthening is upstream of counter-cyclical fiscal policy (it represents one instantiation of automatic stabilizers) and downstream of labor market dynamics (wage determination and employment levels affect the baseline unemployment rate and thus benefit distribution). The constraint family links unemployment insurance design to fiscal policy mechanisms and labor market coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1962_kennedy_permanent_unemployment_compensation_strengthening, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
