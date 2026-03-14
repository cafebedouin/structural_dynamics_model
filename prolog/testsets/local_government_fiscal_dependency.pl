% ============================================================================
% CONSTRAINT STORY: local_government_fiscal_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_local_government_fiscal_dependency, []).

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
 *   constraint_id: local_government_fiscal_dependency
 *   human_readable: Local Government Fiscal Dependency on State and Federal Transfers
 *   domain: political_economy/fiscal_federalism
 *
 * SUMMARY:
 *   Local government fiscal dependency represents a structural constraint
 *   where municipalities' capacity to provide basic services depends
 *   critically on transfers from state and federal governments. This
 *   constraint emerged from the combination of urbanization, service cost
 *   inflation, and tax base concentration, and has intensified over the past
 *   three decades as property tax revenues stagnated while mandated service
 *   costs (education, healthcare, infrastructure maintenance) escalated
 *   faster than local economic growth. The constraint exhibits mixed
 *   properties: genuine coordination function (transfers enable service
 *   standardization, risk-pooling across poor and wealthy jurisdictions,
 *   national redistribution), but also significant extraction (state/federal
 *   leverage over local autonomy, conditionality compliance burdens,
 *   asymmetric fiscal pressure on weaker municipalities). The extractiveness
 *   value (0.58) reflects that while substantial coordination benefits exist,
 *   the asymmetric bargaining power and conditionality structures create
 *   significant value capture at higher tiers of government. The suppression
 *   value (0.68) indicates high structural barriers to exit: localities
 *   cannot unilaterally raise adequate independent revenue, cannot borrow at
 *   sustainable rates without transfer security, and cannot reduce service
 *   obligations without political crisis. Theater ratio (0.55) reflects
 *   moderate performative content in the transfer bureaucracy — elaborate
 *   reporting and compliance systems that consume resources relative to their
 *   coordination value, but with genuine underlying coordination function
 *   (not pure ritual like piton structures).
 *
 * KEY AGENTS:
 *   - Fiscally Dependent Rural Municipality: Primary victim (powerless/trapped) — cannot raise revenue independently, cannot exit service obligations, no bargaining power in transfer negotiations. Experiences maximum extraction.
 *   - State Government: Primary beneficiary (institutional/arbitrage) — extracts policy compliance through transfer conditions, shifts service cost burden downward, maintains leverage over local autonomy. Can arbitrage funding to alternative uses.
 *   - Federal Government: Secondary beneficiary (institutional/arbitrage) — coordinates national policy objectives through conditional transfers, maintains federalism leverage, deferring costs to states. Maximum exit options.
 *   - Mid-Sized City Government: Moderate victim-beneficiary (moderate/constrained) — has some revenue autonomy but faces compliance burdens and unfunded mandates. Constrained by political cost of service reductions or rate increases.
 *   - Wealthy Suburban Municipality: Moderate beneficiary-victim (powerful/mobile) — benefits from transfer system's coordination function but experiences progressive transfer formulas as extraction. Mobile: can influence policy through tax base mobility and exit threats.
 *   - Municipal Reform Coalition: Organized agent (organized/constrained) — advocates for expanded local revenue authority and transfer reform. Perceives sunset: increased local tax autonomy would reduce dependency over time. Constrained by state/federal resistance.
 *   - Transfer Bureaucracy: Institutional actor (institutional/arbitrage) — maintains compliance and reporting infrastructure. Piton perspective: system persists through inertia, with high theater relative to coordination value. Arbitrage: budget allocation to alternative programs.
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — risks naturalizing policy-contingent arrangements as immutable fiscal laws of nature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(local_government_fiscal_dependency, 0.58).
domain_priors:suppression_score(local_government_fiscal_dependency, 0.68).
domain_priors:theater_ratio(local_government_fiscal_dependency, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(local_government_fiscal_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(local_government_fiscal_dependency, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(local_government_fiscal_dependency, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(local_government_fiscal_dependency, tangled_rope).
narrative_ontology:human_readable(local_government_fiscal_dependency, "Local Government Fiscal Dependency on State and Federal Transfers").
narrative_ontology:topic_domain(local_government_fiscal_dependency, "political_economy/fiscal_federalism").

domain_priors:requires_active_enforcement(local_government_fiscal_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(local_government_fiscal_dependency, state_government).
narrative_ontology:constraint_beneficiary(local_government_fiscal_dependency, federal_government).
narrative_ontology:constraint_victim(local_government_fiscal_dependency, local_municipal_autonomy).
narrative_ontology:constraint_victim(local_government_fiscal_dependency, rural_municipalities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FISCALLY DEPENDENT MUNICIPALITY (SNARE) — Rural and post-industrial towns cannot exit revenue dependency on state/federal transfers without ceasing basic service provision. Trapped by structural revenue gaps and legal restrictions on local taxation. Suppression is near-total: cannot raise adequate revenue independently, cannot borrow long-term at viable rates, cannot reduce service obligations. Maximum extraction relative to bargaining power.
constraint_indexing:constraint_classification(local_government_fiscal_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MID-SIZED CITY GOVERNMENT (TANGLED ROPE) — Has some revenue autonomy through property tax base and economic activity, but faces compliance costs, reporting requirements, and unfunded mandates from state/federal transfers. Experiences both genuine coordination (transfer formulas enable service delivery) and asymmetric extraction (compliance burden falls disproportionately on weaker municipalities). Constrained exit: could theoretically decrease service levels or raise local rates, but political cost is severe.
constraint_indexing:constraint_classification(local_government_fiscal_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE GOVERNMENT (ROPE) — Experiences the constraint as a coordination mechanism: state transfers enable local service delivery standardization and aggregate planning. State has exit options through budget reallocation or service devolution. Net beneficiary from the arrangement — extracts compliance leverage without bearing local service costs. Can arbitrage: shift funding burden to localities or federal government.
constraint_indexing:constraint_classification(local_government_fiscal_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT (ROPE) — Coordinates national policy objectives through conditional transfers. Has maximum exit options (federalism allows fund reallocation). Net beneficiary — extracts policy compliance through transfer conditions while deferring service costs to state/local levels. Can arbitrage: shift funding to states that comply with federal priorities.
constraint_indexing:constraint_classification(local_government_fiscal_dependency, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MUNICIPAL REFORM COALITION (SCAFFOLD) — Organized local governments (leagues of cities, county associations) negotiate conditional transfer increases and advocate for new revenue sources (local sales tax authority, property tax relief). Perceive the dependency constraint as a temporary coordination failure with a sunset: devolving new revenue sources or replacing transfer conditionality with block grants would reduce structural dependency. Has sunset clause logic: increased local revenue autonomy reduces extraction mechanism over time.
constraint_indexing:constraint_classification(local_government_fiscal_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: WEALTHY SUBURBAN MUNICIPALITY (TANGLED ROPE) — Has substantial property tax base and economic activity. Experiences transfers as mostly coordination benefit, but also faces extraction: transfer formulas often penalize high-capacity jurisdictions to subsidize low-capacity ones (equity redistribution). Powerful position creates mobile exit: could vote with feet on state/federal policy. Experiences both genuine coordination (risk-pooling, service equity) and asymmetric extraction (progressive transfer formulas).
constraint_indexing:constraint_classification(local_government_fiscal_dependency, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: TRANSFER BUREAUCRACY (PITON) — State and federal transfer systems maintain elaborate compliance, reporting, and allocation infrastructure that persists largely through institutional inertia. Theater ratio is high: eligibility verification, allocation formulas, and progress reporting consume significant resources relative to actual coordination function. Many transfer programs could be consolidated or replaced with simpler mechanisms but persist because bureaucracies have become constituted through them. Degraded institutional form maintained by path dependence.
constraint_indexing:constraint_classification(local_government_fiscal_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (NATURAL LAW VIEW) — From a civilizational/universal perspective, local fiscal dependency follows from an immutable structural fact: local revenue bases (property tax, small business activity) have inelastic caps relative to service costs (education, infrastructure, social services inflate faster than property values). This perspective sees the dependency as a natural law of federalism — inherent to any system where service needs are distributed but revenue sources are localized. However, the analytical frame risks naturalizing what is actually a contingent policy choice (transfer regimes, tax authority allocation, service responsibility assignment).
constraint_indexing:constraint_classification(local_government_fiscal_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(local_government_fiscal_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(local_government_fiscal_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(local_government_fiscal_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(local_government_fiscal_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(local_government_fiscal_dependency, TR),
    TR >= 0.70.

:- end_tests(local_government_fiscal_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The value reflects moderate-to-high extraction. Base calculation starts from the revenue gap (local revenues typically fund 40-60% of service costs in fiscally weak jurisdictions, with transfers covering the remainder). The extraction component derives from the asymmetric conditionality and compliance costs: state/federal transfers come with specific use restrictions, reporting requirements, and policy compliance mandates that localities cannot modify. Over the measurement interval (30 years), extractiveness has increased from 0.35 to 0.58, reflecting rising service cost inflation (especially education and healthcare) that widens the revenue gap and deepens dependency. The increase is not exponential (extractiveness plateaus by year 20-30) because legislative pushback and municipal coalition organizing create some constraints on further deepening. Suppression (0.68): Reflects substantial barriers to exit. Localities cannot unilaterally raise sufficient independent revenue due to legal caps on tax authority (property tax limitations exist in many U.S. states), inelastic tax bases (property values inflate slowly; commercial activity is concentrated), and the economic reality that local taxation above certain levels triggers tax base mobility (wealth flight to lower-tax jurisdictions). Localities cannot borrow long-term without transfer security — credit markets price in transfer dependency as default risk. Localities cannot reduce service obligations due to education mandates, healthcare requirements, and infrastructure maintenance obligations that flow from federal and state law. These are genuine structural barriers, not merely political pressure. Theater ratio (0.55): Moderate-high. The transfer system maintains substantial reporting, eligibility verification, and allocation complexity. However, this is not pure theater — the complexity reflects genuine coordination challenges (measuring fiscal capacity, allocating limited transfers equitably, tracking compliance with conditional funds). The theater has increased slightly over time (0.38 to 0.55) as compliance requirements have expanded and transfer formulas have become more complex. Compared to the pure piton theater ratio (0.70+), this reflects a constraint with real coordination content, not pure ritual.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap appears between the fiscal weak (snare perspective) and the state/federal beneficiaries (rope perspective). Weak municipalities experience the constraint as pure extraction because they have no meaningful exit options and the coordination benefits are minimal (they receive transfers but also bear full service cost burden and compliance costs). State/federal actors experience rope because they gain leverage with manageable coordination burden. Mid-sized cities occupy the middle: tangled rope, experiencing both genuine coordination (service standardization, equity) and extraction (compliance burden, unfunded mandates). The gap is not about different measurement of the same extraction but about different structural positions creating genuinely different experienced constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the structural relationships. State/federal governments are institutional beneficiaries with arbitrage exit (can reallocate transfers) — canonical d ≈ 0.00-0.15, f(d) ≈ -0.12. Financially weak municipalities are powerless victims with trapped exit (cannot raise revenue, cannot exit services) — canonical d ≈ 0.95, f(d) ≈ 1.42. Mid-sized cities are moderate actors, constrained exit (can adjust services at political cost) — derived d ≈ 0.55, f(d) ≈ 0.75. Wealthy suburbs are powerful beneficiaries with mobile exit (can threaten tax base departure) — derived d ≈ 0.30, f(d) ≈ 0.15. These directionality values are not overridden; they derive cleanly from the beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely hybrid: it has real coordination function (enables service delivery to fiscally weak areas, coordinates national standards) and real extraction mechanism (asymmetric conditionality, compliance burden, fiscal pressure on weak municipalities). The snare perspective (from the financially dependent municipality) sees primarily extraction because the coordination benefits are invisible to them — they receive funds but immediately spend them on mandated services. The rope perspective (from state/federal government) sees primarily coordination because they manage the aggregate system and benefit from the leverage. The tangled rope classification (at the analytical context) resolves the mandatrophy by declaring that BOTH functions are structural and real. No reduction to pure rope (which would ignore the extraction) or pure snare (which would ignore the genuine service coordination) is accurate. The constraint's function is genuinely mixed, and the perspectival gap between powerless and institutional actors creates the appearance of disagreement about what the constraint 'really is' — the answer is that it really is both, perspectivally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revenue_capacity_elasticity,
    'Is local revenue dependency driven by fundamental fiscal physics (inelastic local tax bases cannot fund inflation-driven service costs) or by contingent policy choices (permissive transfer conditionality, restricted local taxation authority)?',
    'Comparative analysis of federalism systems with different local tax authority allocations; identification of countries/states where local revenue autonomy successfully funds service obligations without transfers',
    'If physics: dependency is mountain (immutable). If policy: dependency is snare (extractive), and the policy choices are the actual constraint. The analytical mountain perspective becomes false naturalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revenue_capacity_elasticity, empirical, 'Whether local fiscal dependency is structural or policy-contingent').

omega_variable(
    transfer_conditionality_enforcement,
    'Do state/federal transfer conditions genuinely coordinate service delivery (authentic rope function) or primarily enforce political compliance and budget manipulation (snare extraction)?',
    'Content analysis of transfer conditions and their correlation with outcome improvements; identification of conditions that have coordination benefits vs conditions that are purely redistributive or compliance-enforcing',
    'If coordination: tangled rope correctly captures mixed function. If primarily compliance: snare classification more accurate from state/local perspective. Shifts mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_conditionality_enforcement, empirical, 'Whether transfer conditions coordinate or enforce compliance').

omega_variable(
    municipal_exit_feasibility,
    'Can municipalities meaningfully exit dependency through alternative revenue sources (local taxation, public-private partnerships, service fee restructuring) or is exit blocked by legal/economic barriers?',
    'Case studies of municipalities that successfully reduced transfer dependency; identification of barriers (legal caps on local tax authority, economic base constraints, service cost inflation) that prevent exit',
    'If exit feasible: exit_options should be constrained/mobile, not trapped. Refines directionality. If exit blocked: trapped classification confirmed, suppression reassessed upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(municipal_exit_feasibility, empirical, 'Whether municipalities can feasibly exit transfer dependency').

omega_variable(
    transfer_formula_equity_mechanics,
    'Do transfer formulas produce genuine equity (narrowing fiscal disparities) or gaming and moral hazard (incentivizing low-revenue maintenance, discouraging local revenue growth)?',
    'Longitudinal analysis of fiscal disparities before/after transfer formula changes; behavioral response analysis of localities to formula incentives; comparison of equity outcomes across different formula designs',
    'If genuine equity: coordination benefit increases, tangled rope confirmed. If gaming dominates: extraction mechanism confirmed, snare classification gains support. Affects beneficiary/victim classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_formula_equity_mechanics, empirical, 'Whether transfer formulas produce equity or gaming').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(local_government_fiscal_dependency, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lgfd_tr_t0, local_government_fiscal_dependency, theater_ratio, 0, 0.38).
narrative_ontology:measurement(lgfd_tr_t10, local_government_fiscal_dependency, theater_ratio, 10, 0.48).
narrative_ontology:measurement(lgfd_tr_t20, local_government_fiscal_dependency, theater_ratio, 20, 0.55).
narrative_ontology:measurement(lgfd_tr_t30, local_government_fiscal_dependency, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(lgfd_be_t0, local_government_fiscal_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lgfd_be_t10, local_government_fiscal_dependency, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(lgfd_be_t20, local_government_fiscal_dependency, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(lgfd_be_t30, local_government_fiscal_dependency, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(local_government_fiscal_dependency, resource_allocation).
narrative_ontology:affects_constraint(local_government_fiscal_dependency, municipal_service_mandate_compliance).
narrative_ontology:affects_constraint(local_government_fiscal_dependency, property_tax_limitation_regimes).
narrative_ontology:affects_constraint(local_government_fiscal_dependency, state_education_funding_equity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
