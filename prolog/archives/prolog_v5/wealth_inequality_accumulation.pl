% ============================================================================
% CONSTRAINT STORY: wealth_inequality_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wealth_inequality_accumulation, []).

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
 *   constraint_id: wealth_inequality_accumulation
 *   human_readable: Wealth Inequality Accumulation Mechanism
 *   domain: economic/political
 *
 * SUMMARY:
 *   Wealth inequality accumulation represents a structural constraint that
 *   simultaneously functions as coordination mechanism (capital markets
 *   enabling productive investment) and extraction mechanism (compounding
 *   returns concentrating capital in existing wealth holders while excluding
 *   asset-poor populations). The constraint exhibits active enforcement
 *   through legal property systems, tax policy capture, inheritance laws, and
 *   corporate governance structures that preserve wealth across generations.
 *   Base extractiveness (0.58) reflects moderate-to-high asymmetry: capital
 *   holders enjoy compound returns while wage-dependent workers consume labor
 *   income; suppression (0.65) reflects institutional barriers to capital
 *   accumulation (lack of investable surplus, debt service, predatory
 *   financial inclusion); theater ratio (0.55) indicates that regulatory
 *   frameworks perform constraint functions they increasingly fail to execute
 *   (tax enforcement erodes with complexity, wealth management regulatory
 *   arbitrage, anti-monopoly enforcement decline). The constraint's
 *   tangled_rope classification indicates it solves a genuine problem
 *   (capital allocation through markets) while embedding asymmetric
 *   extraction (wealth concentration across generations). Measurements show
 *   monotonic increase in extractiveness and theater over 40-year interval,
 *   indicating progressive degradation of policy constraints on accumulation.
 *
 * KEY AGENTS:
 *   - Wage-Dependent Workers: Primary victims (powerless/trapped) — income fully consumed on subsistence; no accumulation capacity; bear asymmetric cost of inflation, asset-price appreciation they cannot access
 *   - Asset-Poor Populations: Secondary victims (powerless/constrained) — face predatory inclusion (payday lending, rent extraction, insurance cost premiums); credit markets coordinate wealth extraction upward
 *   - Middle-Class Accumulators: Mixed actors (moderate/constrained) — benefit from asset appreciation (housing, retirement) while bearing mortgage/education debt extraction; some agency but significant constraint from cost of capital access
 *   - Capital-Holding Institutions: Primary beneficiaries (institutional/arbitrage) — pension funds, endowments, corporations with diversified portfolios; full exit options through arbitrage; experience system as pure coordination
 *   - Progressive Policy Coalition: Organized agents (organized/constrained) — unions, advocacy groups, progressive legislatures pushing taxation, inheritance reform, public asset ownership; constrained by regulatory capture and political economy
 *   - Financial Sector: Institutional gatekeeper (institutional/arbitrage) — asset managers, investment banks, fintech extracting fees; capture of policy through regulatory dependency
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (inheritance, property law enforcement, tax policy) as immutable mathematical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wealth_inequality_accumulation, 0.58).
domain_priors:suppression_score(wealth_inequality_accumulation, 0.65).
domain_priors:theater_ratio(wealth_inequality_accumulation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wealth_inequality_accumulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(wealth_inequality_accumulation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(wealth_inequality_accumulation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wealth_inequality_accumulation, tangled_rope).
narrative_ontology:human_readable(wealth_inequality_accumulation, "Wealth Inequality Accumulation Mechanism").
narrative_ontology:topic_domain(wealth_inequality_accumulation, "economic/political").

domain_priors:requires_active_enforcement(wealth_inequality_accumulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wealth_inequality_accumulation, capital_holders).
narrative_ontology:constraint_beneficiary(wealth_inequality_accumulation, institutional_investors).
narrative_ontology:constraint_victim(wealth_inequality_accumulation, wage_dependent_workers).
narrative_ontology:constraint_victim(wealth_inequality_accumulation, asset_poor_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE-DEPENDENT WORKER (SNARE) — Trapped by lack of capital accumulation capacity. Labor income consumed entirely on subsistence leaves no investable surplus. Exit requires capital but accumulation is structurally prevented. Maximum experienced extraction with zero agency.
constraint_indexing:constraint_classification(wealth_inequality_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-CLASS ACCUMULATOR (TANGLED ROPE) — Possesses some capital-accumulation capacity through savings and property ownership, but faces steep barriers: debt service, housing costs, healthcare expenses. Benefits from asset appreciation coordination (housing markets, retirement systems) while bearing asymmetric tax and investment fee extraction. Mixed experience: some agency, significant constraint.
constraint_indexing:constraint_classification(wealth_inequality_accumulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL-HOLDING INSTITUTION (ROPE) — Institutional investors (pension funds, endowments, corporations) experience the constraint as pure coordination: wealth begets access to capital markets, leverage, and diversification. The system solves their problem of capital deployment and returns optimization. Net beneficiary with full exit options (arbitrage).
constraint_indexing:constraint_classification(wealth_inequality_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRESSIVE POLICY COALITION (SCAFFOLD) — Organized agents (unions, advocacy groups, progressive legislatures) see wealth inequality as a temporary institutional failure with policy solutions: progressive taxation, wealth taxes, inheritance reform, public asset ownership. Low experienced extraction because organized agents have agency and see an exit path through political reform.
constraint_indexing:constraint_classification(wealth_inequality_accumulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY THEATER SYSTEM (PITON) — Securities regulation, tax enforcement, and anti-monopoly frameworks persist through institutional inertia despite degraded function. Regulatory bodies maintain oversight rituals while enforcement capacity has eroded relative to financial complexity and capital velocity. Theater ratio (0.55) reflects that much regulatory activity is performative compliance rather than effective constraint of accumulation.
constraint_indexing:constraint_classification(wealth_inequality_accumulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a mathematical perspective, wealth accumulation with compound returns and capital preservation mechanisms is presented as an inevitable consequence of economic law: capital earns returns, returns compound, compounding creates divergence. However, the structural data reveals this as a false summit — the constraint requires active enforcement (progressive taxation suppression, regulatory capture, inheritance systems, corporate governance capture) to maintain the extraction mechanism. No natural law produces inequality without institutional structure.
constraint_indexing:constraint_classification(wealth_inequality_accumulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wealth_inequality_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wealth_inequality_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wealth_inequality_accumulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wealth_inequality_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wealth_inequality_accumulation, TR),
    TR >= 0.70.

:- end_tests(wealth_inequality_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts via multiple mechanisms: (1) wage suppression through capital's bargaining power advantage, (2) asset-price inflation accessible only to existing wealth holders, (3) financial fees and credit spreads that concentrate upward, (4) tax avoidance by capital vs wage garnishment. The value reflects that extraction is substantial but not maximal — some coordination function (capital allocation, risk pooling) creates genuine mutual benefit that prevents pure snare classification. Suppression (0.65): High. Structural barriers to capital accumulation include: (1) inability of wage earners to build surplus for investment, (2) debt service obligations that prevent asset accumulation, (3) predatory financial inclusion that concentrates fees on asset-poor, (4) inherited wealth advantage compounding across generations. Barriers are formidable but not absolute — some social mobility exists, some wage earners do accumulate. Theater ratio (0.55): Moderate. Regulatory frameworks (Securities and Exchange Commission, tax enforcement, anti-monopoly agencies) perform extensive oversight activities that increasingly fail to constrain wealth concentration. Tax code complexity enables avoidance; enforcement resources decline relative to financial velocity; wealth management strategies outpace regulatory adaptation. The theater is not purely performative (some enforcement occurs) but comprises substantial performative activity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between capital holders and wage workers is maximal. Capital holders see coordination (Rope) — capital markets efficiently allocating productive investment. Wage workers see extraction (Snare) — compounding concentration with zero personal leverage. The gap persists because it reflects real structural asymmetry: beneficiaries experience the system as solving their problem (find capital, deploy it, capture returns); targets experience it as preventing their solution (earn wages, save, invest, accumulate). The piton perspective reveals institutional degradation: regulatory frameworks perform extensive activities (SEC filings, tax audits, antitrust investigations) that have declining functional impact on inequality trajectories. The scaffold perspective reveals political contingency: progressive coalitions have actual policy proposals (wealth taxes at 2-3% annually, inheritance taxes at 40-50%, public asset ownership models) with measurable impact in other jurisdictions (Scandinavian models show lower inequality under different policy). The mountain perspective is a false summit — mathematical compound growth is real, but *who captures the returns* is entirely institutional.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from structural position — power level, exit options, and beneficiary/victim status. Capital holders with arbitrage options experience low or negative effective extraction (they benefit from the mechanism). Wage-dependent workers with no capital accumulation option and trapped exit status experience maximum directionality toward the constraint's target role (high d → high chi). Middle-class actors with some asset participation but constrained exit (mortgage debt, healthcare costs) fall between these poles. The constraint's enforcement requirement (property law, tax system, inheritance law, corporate governance) means d values are politically contingent — policy changes (wealth taxation, inheritance reform) directly alter the directionality function by changing exit options and benefit asymmetry. Organized agents see this contingency clearly; powerless agents experience directionality as fixed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint illustrates mandatrophy resolution through perspectival realism. The mandatrophy question is not 'is wealth inequality a mountain or a snare?' but 'which perspective are you measuring from?' For capital holders, it is rope (coordination). For wage workers, it is snare (extraction). For policy reformers, it is scaffold (temporary, fixable). For regulators, it is piton (ritually maintained but degrading). The false summit (mountain perspective) naturalizes what is institutional. The resolution is accepting the presheaf: wealth inequality is all six types simultaneously, viewed from different structural positions. The constraint's classification variance across perspectives is not ambiguity — it is structural. Picking one type and claiming universality reveals the observer's position, not the constraint's nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_vs_labor_coefficient,
    'What is the empirical ratio of capital returns to labor-productivity growth rates, and how much of measured inequality is driven by this ratio vs institutional policy choices?',
    'Historical time-series analysis of real capital returns, real wage growth, and capital share of income across jurisdictions with different tax/inheritance policies. Correlation with policy changes.',
    'If capital returns > labor growth by structural physics: Mountain classification confirmed. If ratio is policy-dependent and reversible: Tangled Rope / Scaffold classifications confirmed; inequality is contingent institutional arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_vs_labor_coefficient, empirical, 'Ratio of capital returns to labor-productivity growth').

omega_variable(
    inheritance_system_counterfactual,
    'Without inheritance, tax avoidance, and dynasty-building mechanisms, what would be the natural stationary distribution of wealth across generations?',
    'Intergenerational mobility analysis; comparison of wealth concentration across countries with different inheritance taxation; computational models of accumulation without institutional preservation mechanisms.',
    'If stationary distribution is highly unequal without policy: natural-law Mountain. If stationary distribution is moderately unequal: institutional suppression of redistribution (Scaffold/Tangled Rope). If stationary distribution is relatively flat: current inequality is enforced extraction (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inheritance_system_counterfactual, empirical, 'Counterfactual wealth distribution without inheritance mechanisms').

omega_variable(
    enforcement_mechanism_dependency,
    'To what extent does current wealth inequality depend on active legal enforcement (property law, contract enforcement, corporate governance structures) vs passive accumulation dynamics?',
    'Historical analysis of periods with weakened enforcement (revolutions, state collapse, currency crises); comparison of inequality in high-enforcement vs low-enforcement jurisdictions; measurement of enforcement costs as fraction of wealth extraction.',
    'If highly dependent on enforcement: Tangled Rope / Snare (requires active structure). If weakly dependent: Mountain (natural property of capital dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_dependency, empirical, 'Dependency of inequality on active legal enforcement mechanisms').

omega_variable(
    escape_velocity_threshold,
    'What is the minimum capital threshold at which an agent can achieve financial independence through asset returns alone, and what fraction of the population reaches this threshold?',
    'Calculation of financial independence threshold per jurisdiction; tracking of social mobility rates; analysis of how many agents ever cross the threshold in their lifetime or via inheritance.',
    'If threshold is reachable by <5% despite full effort: high suppression (Snare). If threshold is reachable by 30-50%: moderate suppression (Tangled Rope). If threshold is reachable by >70%: low suppression (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(escape_velocity_threshold, empirical, 'Minimum capital threshold for financial independence').

omega_variable(
    policy_reversal_feasibility,
    'How easily could progressive wealth taxation, inheritance reform, or public asset ownership reverse accumulated inequality to pre-1980 levels? What are the political economy barriers?',
    'Historical precedent analysis (post-WWII redistribution periods); computational models of tax policy impact on inequality trajectory; political economy analysis of policy coalition dynamics.',
    'If reversal is technically straightforward but politically blocked: Scaffold classification confirmed (sunset exists but faces organized resistance). If reversal is technically difficult: Tangled Rope / Snare confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_reversal_feasibility, conceptual, 'Technical and political feasibility of reversing inequality accumulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wealth_inequality_accumulation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wealth_tr_t0, wealth_inequality_accumulation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(wealth_tr_t20, wealth_inequality_accumulation, theater_ratio, 20, 0.42).
narrative_ontology:measurement(wealth_tr_t40, wealth_inequality_accumulation, theater_ratio, 40, 0.55).
narrative_ontology:measurement(wealth_tr_t10, wealth_inequality_accumulation, theater_ratio, 10, 0.36).

% Extraction over time
narrative_ontology:measurement(wealth_be_t0, wealth_inequality_accumulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wealth_be_t20, wealth_inequality_accumulation, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(wealth_be_t40, wealth_inequality_accumulation, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(wealth_be_t10, wealth_inequality_accumulation, base_extractiveness, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wealth_inequality_accumulation, resource_allocation).
narrative_ontology:affects_constraint(wealth_inequality_accumulation, intergenerational_mobility_trap).
narrative_ontology:affects_constraint(wealth_inequality_accumulation, capital_requirements_barrier).
narrative_ontology:affects_constraint(wealth_inequality_accumulation, tax_avoidance_extraction).

% DUAL FORMULATION NOTE:
% Wealth inequality accumulation decomposes into three downstream constraints: (1) intergenerational_mobility_trap (ε=0.62) capturing inheritance mechanism and dynasty persistence, (2) capital_requirements_barrier (ε=0.51) capturing minimum-capital thresholds for financial participation, (3) tax_avoidance_extraction (ε=0.48) capturing policy capture enabling legal wealth concentration. Each has its own perspectives and measurements. Upstream relationship: wealth inequality is the integrating constraint over these three mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wealth_inequality_accumulation, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
