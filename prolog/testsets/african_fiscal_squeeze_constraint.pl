% ============================================================================
% CONSTRAINT STORY: african_fiscal_squeeze_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_african_fiscal_squeeze_constraint, []).

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
 *   constraint_id: african_fiscal_squeeze_constraint
 *   human_readable: African Fiscal Squeeze: Debt Service vs. Development Investment
 *   domain: economic_policy/development_finance
 *
 * SUMMARY:
 *   The African fiscal squeeze represents a structural constraint where
 *   African governments' sovereign debt obligations to external creditors
 *   systematically crowd out public health, education, and infrastructure
 *   spending. This is not a natural economic law but a contingent policy
 *   arrangement: the constraint emerges from (1) the historical accumulation
 *   of external debt often incurred for purposes of limited development
 *   benefit, (2) the enforcement of debt service through international
 *   financial institutions with conditionality tied to creditor interests,
 *   and (3) the absence of alternative financing mechanisms available to
 *   African states. The constraint exhibits classic tangled-rope
 *   characteristics: it contains a genuine coordination function (capital
 *   markets require returns; borrowing does enable infrastructure investment)
 *   overlaid with asymmetric extraction (creditors capture returns while
 *   populations bear costs of reduced public services). The theater ratio
 *   (0.58) reflects that structural adjustment conditionality and debt
 *   sustainability frameworks persist despite limited demonstrated
 *   effectiveness in achieving stated policy goals — the procedural ritual
 *   maintains the constraint even as its functional value has eroded.
 *
 * KEY AGENTS:
 *   - African Populations and Public Services: Primary victims (powerless/trapped) — citizens and health/education systems unable to exit state structures; bear full costs of reduced public spending
 *   - African Governments: Primary beneficiary-victims (organized/constrained) — received capital inflows for past infrastructure; now constrained by service obligations; have limited negotiating power and restructuring options
 *   - External Creditors and Financial Institutions: Primary beneficiary (institutional/arbitrage) — capture predictable returns through debt service; maintain diverse portfolio options; high exit optionality
 *   - Multilateral Financial Institutions (IMF/World Bank): Secondary beneficiary (institutional/arbitrage) — enforce conditionality that benefits creditor interests; maintain institutional relevance through structural adjustment programs
 *   - Debt Relief Coalition: Organized challengers (organized/constrained) — civil society, some governments, religious organizations advocating relief and restructuring; have limited formal power but growing normative influence
 *   - Global Financial System: Systemic beneficiary (institutional/arbitrage) — capital discipline enforced on African governments; African fiscal orthodoxy protects broader financial stability expectations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(african_fiscal_squeeze_constraint, 0.58).
domain_priors:suppression_score(african_fiscal_squeeze_constraint, 0.65).
domain_priors:theater_ratio(african_fiscal_squeeze_constraint, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(african_fiscal_squeeze_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(african_fiscal_squeeze_constraint, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(african_fiscal_squeeze_constraint, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(african_fiscal_squeeze_constraint, tangled_rope).
narrative_ontology:human_readable(african_fiscal_squeeze_constraint, "African Fiscal Squeeze: Debt Service vs. Development Investment").
narrative_ontology:topic_domain(african_fiscal_squeeze_constraint, "economic_policy/development_finance").

domain_priors:requires_active_enforcement(african_fiscal_squeeze_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(african_fiscal_squeeze_constraint, external_creditors).
narrative_ontology:constraint_beneficiary(african_fiscal_squeeze_constraint, global_financial_infrastructure).
narrative_ontology:constraint_victim(african_fiscal_squeeze_constraint, african_populations).
narrative_ontology:constraint_victim(african_fiscal_squeeze_constraint, public_health_systems).
narrative_ontology:constraint_victim(african_fiscal_squeeze_constraint, education_access).
narrative_ontology:constraint_victim(african_fiscal_squeeze_constraint, infrastructure_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFRICAN CITIZENS/PUBLIC SERVICES (SNARE) — Trapped in allocation squeeze where debt service crowds out health, education, and infrastructure spending. No exit option: citizens cannot exit the state, cannot refinance sovereign debt unilaterally, cannot dissolve obligations. Bear full costs of fiscal contraction. Maximum experienced extraction from constrained budget.
constraint_indexing:constraint_classification(african_fiscal_squeeze_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: AFRICAN GOVERNMENTS (TANGLED ROPE) — Structured as both beneficiaries (past borrowing financed infrastructure, governance capacity) and victims (servicing debt constrains current spending). Have some agency through fiscal policy choice and debt restructuring negotiation, but face high barriers: capital flight risk, credit rating downgrades, IMF conditionality. Constrained exit.
constraint_indexing:constraint_classification(african_fiscal_squeeze_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: EXTERNAL CREDITORS / FINANCIAL INSTITUTIONS (ROPE) — Primary beneficiaries. Experience the constraint as legitimate coordination: debt service maintains financial system stability, enforces fiscal discipline, ensures capital flow returns. High exit optionality via portfolio diversification, debt trading, restructuring escape clauses. Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(african_fiscal_squeeze_constraint, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEBT RELIEF/REFORM COALITION (SCAFFOLD) — Organized agents (IMF debt relief mechanisms, Paris Club restructuring, Jubilee movement) see fiscal squeeze as solvable through temporary intervention: debt-for-health swaps, grace periods, multilateral forgiveness. See sunset: as Africa's export capacity and governance improve, debt-to-revenue ratios decline naturally. Constrained but organized.
constraint_indexing:constraint_classification(african_fiscal_squeeze_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STRUCTURAL ADJUSTMENT FRAMEWORK (PITON) — IMF/World Bank policy regime was designed to enforce fiscal discipline and capital market integration; now largely performative. Debt sustainability analyses persist despite weak predictive power; conditions are maintained through institutional inertia rather than demonstrated effectiveness. Theater ratio high as conditionality theater replaces functional fiscal reform. Degraded institution persists.
constraint_indexing:constraint_classification(african_fiscal_squeeze_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Universal perspective risks treating fiscal squeeze as immutable: 'every developing nation must service debt; borrowing always constrains future spending; global capital requires returns.' This naturalizes the constraint's institutional structure. However, structural data (beneficiary/victim declarations, enforcement requirements) reveals the mountain as a false summit — the fiscal squeeze is a contingent policy regime, not a law of finance.
constraint_indexing:constraint_classification(african_fiscal_squeeze_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(african_fiscal_squeeze_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(african_fiscal_squeeze_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(african_fiscal_squeeze_constraint, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(african_fiscal_squeeze_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(african_fiscal_squeeze_constraint, TR),
    TR >= 0.70.

:- end_tests(african_fiscal_squeeze_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over 25-year interval. The constraint has become more extractive as external debt accumulated (2000-2022) and African governments' fiscal space contracted. External debt service as percentage of government revenue has risen from ~8% (2000) to ~18% (2022) on average across sub-Saharan Africa, while public health and education spending stagnated. The extractiveness value reflects this asymmetric burden: creditors capture predictable nominal returns while African populations absorb the opportunity cost of foregone public investment. Suppression (0.65): Moderate-high. Significant structural barriers prevent exit: (1) Capital flight and credit rating downgrades constrain alternative financing; (2) IMF/World Bank conditionality tied to debt service creates formal constraints on fiscal policy autonomy; (3) Commodity-dependent revenue volatility makes debt servicing unpredictable without external financing; (4) Domestic taxation capacity is limited by political economy barriers and capital mobility. Suppression is not total — some restructuring is possible, and some African states have begun domestic resource mobilization — but barriers are substantial. Theater ratio (0.58): Moderate. Structural adjustment frameworks and debt sustainability analyses persist despite weak predictive power. Conditionality theater (structural benchmarks, performance criteria, mission cycles) replaces functional fiscal reform in many cases. However, the theater is less than pure degradation (piton-level 0.70+) because some conditions do enforce real constraints and debt service does represent real creditor enforcement rather than purely performative ritual.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates classic perspectival divergence. External creditors perceive a well-functioning coordination mechanism (Rope) — debt service maintains financial stability and enforces fiscal discipline. African governments perceive mixed extraction and coordination (Tangled Rope) — past borrowing enabled infrastructure but current service obligations constrain development. African citizens perceive pure extraction (Snare) — they bear costs of reduced health/education spending with no exit and no benefit from past borrowing. The Scaffold perspective (debt relief coalition) sees a solvable temporary problem with sunset — as African export capacity improves and governance deepens, debt-to-revenue ratios decline naturally, requiring only transition financing. The Piton perspective (structural adjustment regime) observes its own degradation — the institutional machinery persists through inertia but no longer delivers its stated function. The analytical Mountain perspective risks naturalizing policy as law ('debt service is inevitable') but the structural data reveals this as false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and agent power/exit positions. External creditors (institutional/arbitrage) have low directionality (d ≈ 0.05-0.15) — they are beneficiaries with high exit optionality, resulting in effective extraction flowing TOWARD them (negative chi from their perspective, as constraints subsidize them). African populations (powerless/trapped) have maximum directionality (d ≈ 0.95) — they are victims with no exit, experiencing extraction directly and maximally. African governments occupy the middle: they are both partial beneficiary (past capital) and partial victim (current obligations), with constrained exit (d ≈ 0.55-0.65). The derived chi values reflect this: creditors perceive low extraction (constraint as coordination), populations perceive maximum extraction (constraint as snare), governments perceive moderate extraction (constraint as tangled rope). The perspectival gap is the mathematical signature of asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that (1) the Tangled Rope classification correctly captures both the genuine coordination function (capital enables past development) and the asymmetric extraction (populations bear costs of service), and (2) the divergent perspectives from creditors (Rope) vs. populations (Snare) reveal the constraint's true extractive structure. The beneficiary (creditor) perspective converges to lower-extraction types precisely because extraction flows toward them; the victim (population) perspective converges to higher-extraction types because extraction flows away from them. This is not a failure to categorize but the framework working as designed: the perspectival presheaf over the constraint reveals its asymmetric structure. The false summit (Mountain from analytical perspective) is particularly instructive: the risk that policy makers will naturalize the fiscal squeeze as inevitable ('debt service is how markets work') when it is actually a contingent institutional arrangement. The mandatrophy is resolved by recognizing that the constraint's type depends irreducibly on position, and no single perspective reveals the full structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_sustainability_threshold,
    'What debt-to-revenue ratio threshold distinguishes sustainable borrowing from extractive debt accumulation?',
    'Longitudinal analysis of growth outcomes vs. debt servicing burden across African countries; correlation between debt levels and development indicators (HDI, school enrollment, health spending)',
    'If threshold is 120%: many African states are in extraction trap. If threshold is 250%+: current levels are sustainable, and fiscal squeeze is discretionary policy choice. Impacts whether constraint is structural or policy-contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_sustainability_threshold, empirical, 'Debt-to-revenue threshold for sustainability vs. extraction').

omega_variable(
    alternative_financing_availability,
    'Are genuinely alternative financing sources (domestic taxation, resource mobilization, diaspora bonds) structurally available, or is external debt the binding constraint?',
    'Comparative fiscal capacity analysis: tax-to-GDP potential vs. actual taxation; resource revenue capture; domestic credit availability; remittance mobilization feasibility',
    'If alternatives are available: fiscal squeeze is policy choice by African governments (shift to Tangled Rope from government perspective). If alternatives blocked by creditor pressure or structural barriers: squeeze is genuinely extractive (Snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_financing_availability, empirical, 'Whether alternative financing sources are structurally available').

omega_variable(
    debt_conditionality_capture,
    'Do IMF/World Bank conditions genuinely enforce fiscal discipline, or have they become a constraint captured by creditor interests?',
    'Analysis of compliance correlation: countries under strict conditionality vs. growth outcomes; comparison of conditionality outcomes to stated policy objectives; evolution of condition design over time',
    'If conditions enforce discipline: institutional framework is legitimate coordination (Scaffold or Rope from government perspective). If conditions serve creditor extraction: they are an enforcement mechanism for the Snare (shift government perspective toward higher extraction perception).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_conditionality_capture, empirical, 'Whether debt conditionality enforces discipline or captures policy').

omega_variable(
    domestic_debt_vs_external,
    'What proportion of the fiscal squeeze derives from external debt service vs. domestic debt obligations and inflation-driven real burden?',
    'Debt composition analysis: external vs. domestic share; real interest rate burden; currency depreciation impact; domestic debt maturity structure',
    'If external is dominant (>60%): squeeze is externally imposed extraction. If domestic is dominant: squeeze is endogenous policy (higher government agency, lower victim perception). Affects victim classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_debt_vs_external, empirical, 'Proportion of fiscal squeeze attributable to external vs. domestic debt').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(african_fiscal_squeeze_constraint, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(affs_tr_t2000, african_fiscal_squeeze_constraint, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(affs_tr_t2008, african_fiscal_squeeze_constraint, theater_ratio, 2008, 0.48).
narrative_ontology:measurement(affs_tr_t2015, african_fiscal_squeeze_constraint, theater_ratio, 2015, 0.55).
narrative_ontology:measurement(affs_tr_t2022, african_fiscal_squeeze_constraint, theater_ratio, 2022, 0.58).

% Extraction over time
narrative_ontology:measurement(affs_be_t2000, african_fiscal_squeeze_constraint, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(affs_be_t2008, african_fiscal_squeeze_constraint, base_extractiveness, 2008, 0.44).
narrative_ontology:measurement(affs_be_t2015, african_fiscal_squeeze_constraint, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement(affs_be_t2022, african_fiscal_squeeze_constraint, base_extractiveness, 2022, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(african_fiscal_squeeze_constraint, resource_allocation).
narrative_ontology:affects_constraint(african_fiscal_squeeze_constraint, health_system_capacity_constraint).
narrative_ontology:affects_constraint(african_fiscal_squeeze_constraint, education_access_bottleneck).
narrative_ontology:affects_constraint(african_fiscal_squeeze_constraint, infrastructure_investment_gap).
narrative_ontology:affects_constraint(african_fiscal_squeeze_constraint, capital_flight_regulator).

% DUAL FORMULATION NOTE:
% The fiscal squeeze is downstream of historical debt accumulation (external_debt_regime) and structural adjustment policy (structural_adjustment_framework). Each story has distinct ε: historical borrowing choices (ε≈0.25, past coordination), structural adjustment conditionality (ε≈0.42, coordination+extraction hybrid), current fiscal crowding-out (ε≈0.58, extraction-dominant hybrid). The family tracks how coordination mechanisms degrade into extraction as debt burden accumulates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(african_fiscal_squeeze_constraint, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
