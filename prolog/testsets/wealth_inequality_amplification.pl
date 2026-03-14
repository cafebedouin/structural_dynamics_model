% ============================================================================
% CONSTRAINT STORY: wealth_inequality_amplification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wealth_inequality_amplification, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: wealth_inequality_amplification
 *   human_readable: Wealth Inequality Amplification Through Institutional Extraction
 *   domain: economic_political
 *
 * SUMMARY:
 *   Wealth inequality amplification is a constraint that operates through
 *   institutional mechanisms that systematically concentrate capital and
 *   returns on capital among existing asset holders while suppressing capital
 *   formation and wealth accumulation among wage and labor-dependent
 *   populations. The constraint exhibits characteristics of both coordination
 *   (capital market function, credit systems) and extraction (asymmetric
 *   returns, policy capture), making it a Tangled Rope at the analytical
 *   level. From different structural positions, the same institutional
 *   framework appears as pure coordination (Rope from asset owner
 *   perspective), pure extraction (Snare from trapped wage-worker
 *   perspective), or as degraded institutional arrangements (Piton from
 *   welfare state perspective). The constraint has amplified over the 40-year
 *   interval: extractiveness has doubled from 0.28 to 0.58, driven by
 *   financialization, capital mobility, policy capture, and weakening of
 *   wage-coordination mechanisms. Theater ratio has risen from 0.38 to 0.55,
 *   reflecting performative welfare states and ideological narratives about
 *   free markets that obscure extractive mechanisms.
 *
 * KEY AGENTS:
 *   - Wage-Dependent Workers: Primary victim (powerless/trapped) — structurally dependent on employment for income; no access to capital accumulation mechanisms; face suppression through debt, housing, healthcare costs
 *   - Asset Owners: Primary beneficiary (institutional/arbitrage) — capital is mobile, returns are reinvestable, leverage creates profitable extraction; experience constraint as coordination mechanism
 *   - Financial Intermediaries: Beneficiary (institutional/arbitrage) — extract rents through spreads, fees, complexity; enable capital allocation but capture surplus
 *   - Unionized Labor: Secondary actor (organized/constrained) — have coordination mechanisms (collective bargaining) but constrained by capital's superior power; some benefits from pension funds but also extraction through wage suppression
 *   - Land-Dependent Communities: Victim (powerless/trapped) — excluded from capital markets, face land extraction, commodity price suppression, intergenerational wealth transfer blocked
 *   - Welfare State Institutions: Degraded actor (institutional/arbitrage) — maintain performative function but real protective capacity has eroded through privatization and capture
 *   - Multinational Corporations: Complex actor (powerful/arbitrage) — benefit from extraction mechanisms but also constrained by competitive dynamics; high exit options through regulatory arbitrage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wealth_inequality_amplification, 0.58).
domain_priors:suppression_score(wealth_inequality_amplification, 0.68).
domain_priors:theater_ratio(wealth_inequality_amplification, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wealth_inequality_amplification, extractiveness, 0.58).
narrative_ontology:constraint_metric(wealth_inequality_amplification, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(wealth_inequality_amplification, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wealth_inequality_amplification, tangled_rope).
narrative_ontology:human_readable(wealth_inequality_amplification, "Wealth Inequality Amplification Through Institutional Extraction").
narrative_ontology:topic_domain(wealth_inequality_amplification, "economic_political").

domain_priors:requires_active_enforcement(wealth_inequality_amplification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wealth_inequality_amplification, asset_owners).
narrative_ontology:constraint_beneficiary(wealth_inequality_amplification, financial_intermediaries).
narrative_ontology:constraint_beneficiary(wealth_inequality_amplification, rent_extraction_sectors).
narrative_ontology:constraint_victim(wealth_inequality_amplification, wage_dependent_workers).
narrative_ontology:constraint_victim(wealth_inequality_amplification, land_and_resource_dependent_communities).
narrative_ontology:constraint_victim(wealth_inequality_amplification, capital_formation_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE-DEPENDENT WORKER (SNARE) — Structurally trapped by dependency on employment for survival. Capital markets, asset ownership, and credit systems all operate at extraction ratios that amplify existing wealth gaps. No meaningful exit options; wage growth systematically lags productivity and asset appreciation. Maximum suppression: credit access conditioned on collateral the worker doesn't have, housing markets driven by investor capital, pension systems indexed to stock markets where workers hold minimal positions.
constraint_indexing:constraint_classification(wealth_inequality_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LAND-DEPENDENT COMMUNITIES (SNARE) — Indigenous peoples, smallholder farmers, and resource-dependent communities face extraction through land acquisition, commodity price suppression, and exclusion from capital markets. Generational analysis shows intergenerational wealth transfer blocked by land loss and debt dependency. Suppression mechanisms include legal frameworks favoring large-scale extraction, credit systems inaccessible at scale needed for land purchase or development.
constraint_indexing:constraint_classification(wealth_inequality_amplification, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: UNIONIZED LABOR AND WORKER COALITIONS (TANGLED ROPE) — Organized workers benefit from collective coordination mechanisms (labor agreements, pension funds, negotiated access to credit) while simultaneously experiencing extraction through capital's superior bargaining power, financialization of wages, and policy capture. Significant agency but constrained by structural power asymmetries. The constraint has genuine coordination function (labor market clearing, collective risk management) alongside asymmetric extraction (capital capture of productivity gains).
constraint_indexing:constraint_classification(wealth_inequality_amplification, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ASSET OWNERS AND FINANCIAL INTERMEDIARIES (ROPE) — Experience the wealth inequality constraint as pure coordination mechanism: capital markets, credit systems, and asset pricing mechanisms allocate resources efficiently (from their perspective). Exit options abundant: capital is mobile, returns are reinvestable, leverage creates profitable arbitrage. Net beneficiary through extraction flow — the institutional framework systematically channels wealth toward asset holders and away from workers and communities dependent on land/labor.
constraint_indexing:constraint_classification(wealth_inequality_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MIDDLE-CLASS PROFESSIONAL WITH ASSET HOLDINGS (TANGLED ROPE) — Dual position: beneficiary from equity and property appreciation (retirement accounts, home ownership) but also victim of housing market extraction, healthcare cost amplification, and education funding inequality. Benefits from wealth inequality mechanisms but also constrained by them. The constraint has coordination function (capital market allocation, credit access) alongside extraction mechanisms (asset price inflation, debt service burdens). Moderate exit options constrained by financial commitment to assets.
constraint_indexing:constraint_classification(wealth_inequality_amplification, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: WELFARE STATE INSTITUTIONS (PITON) — Public pension systems, unemployment insurance, social safety nets maintain performative function (appearing to protect workers) while their real effectiveness has eroded through privatization, austerity, and financialization. Theater ratio high: institutions perform redistribution while actual redistribution mechanisms have been captured or hollowed out. Institutional inertia: systems persist because dismantling them directly is politically costly, but their functional capacity has degraded. Suppression partly structural (policy capture) and partly theatrical (systems appear to function but don't).
constraint_indexing:constraint_classification(wealth_inequality_amplification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: MULTINATIONAL CORPORATIONS AND PRIVATE EQUITY (TANGLED ROPE) — Benefit from capital concentration mechanisms but also constrained by competitive dynamics and regulatory arbitrage requirements. High exit options (capital flight, regulatory shopping, cross-border optimization). Genuine coordination function: capital allocation, supply chain coordination, risk distribution. Simultaneously extraction mechanisms: wage suppression, tax avoidance, resource extraction from lower-income regions. Effective extraction positive but moderated by internal competitive pressure and arbitrage costs.
constraint_indexing:constraint_classification(wealth_inequality_amplification, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / CAPITAL DYNAMICS VIEW (MOUNTAIN) — From civilizational scope, wealth inequality amplification appears as an immutable consequence of capital dynamics: compound returns on capital exceed wage growth by structural necessity (capital → returns → reinvestment → compounding while wages are consumed). This perspective naturalizes contingent institutional arrangements (capital mobility, weak inheritance taxation, financial market design) as inherent to economics. However, this is a false summit — the structural data contradicts mountain classification. Historical periods with different wealth inequality trajectories demonstrate that the 'immutable' dynamics are actually policy-dependent.
constraint_indexing:constraint_classification(wealth_inequality_amplification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wealth_inequality_amplification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wealth_inequality_amplification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wealth_inequality_amplification, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wealth_inequality_amplification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wealth_inequality_amplification, TR),
    TR >= 0.70.

:- end_tests(wealth_inequality_amplification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High, reflecting substantial systematic transfer of returns from labor/land-dependent populations to asset owners. The value has increased over time (from 0.28), indicating that financial system sophistication and capital mobility have amplified extraction mechanisms. Suppression (0.68): High. Multiple barriers to capital formation for low-wealth populations: credit access requires collateral (assets the poor don't have), housing markets driven by investor capital, education funding indexed to local property taxes, pension systems privatized and indexed to stock markets where poor hold minimal positions. Theater ratio (0.55): Moderate-high. Narrative of efficient free markets and meritocratic wealth distribution maintains legitimacy (theater) while actual mechanisms are increasingly extractive. Welfare state institutions perform redistribution (theater) while real redistributive capacity has degraded. This divergence increases over the interval as privatization and austerity hollow out protective institutions while market rhetoric strengthens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival gap. Asset owners see pure coordination (Rope) — efficient capital allocation, legitimate returns on risk-taking, justified by productivity. Wage workers see pure extraction (Snare) — systematic wealth transfer away from labor toward capital with no exit options. Organized workers see mixed coordination-extraction (Tangled Rope) — genuine collective benefits from pension coordination alongside extraction through wage suppression. Welfare state institutions see degradation (Piton) — their functional protective capacity has eroded while their legitimatory role persists. The analytical observer risks naturalizing this as immutable capital dynamics (Mountain) but structural data contradicts: different historical periods and different countries show different inequality trajectories, demonstrating that the 'immutable dynamics' are policy-dependent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to extraction flow. Wage workers trapped with no exit options have d ≈ 0.95 → f(d) ≈ 1.42 → experience maximum chi. Asset owners with arbitrage options have d ≈ 0.05 → f(d) ≈ -0.12 → experience negative or minimal chi, receiving subsidies rather than extraction. Organized workers with constrained exit and mixed beneficiary/victim status have d ≈ 0.55-0.60, producing intermediate chi through the sigmoid. Multinational corporations with powerful status and arbitrage options have d ≈ 0.25-0.35, benefiting from extraction mechanisms. The piton classification derives from welfare state institutions maintaining performative function (theater_ratio 0.55 rising toward 0.70) while real protective capacity has degraded — the rituals persist through institutional inertia, not function.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: Wealth inequality amplification resolves through perspectival analysis. The constraint is NOT a single immutable capital law (false mountain); it is a Tangled Rope where coordination function (capital allocation, credit systems, risk distribution) coexists with extraction mechanisms (rent capture, policy capture, intergenerational wealth transfer). From different structural positions, agents legitimately perceive different types: Rope (beneficiaries), Snare (trapped victims), Tangled Rope (organized or constrained agents), Piton (degraded institutions). The mandatrophy dissolves when the analytical observer recognizes that 'which type is correct?' is the wrong question. The correct question is 'from what structural position is this observed, and what is that position's interests in maintaining or changing this constraint?' The multiplicity of legitimate classifications across perspectives is the analytical finding, not a bug. The constraint's extractiveness has increased (from 0.28 to 0.58) over the interval, indicating that institutional design has shifted toward amplifying extraction — the policy-dependence of the mechanism means the trend can be reversed through institutional redesign.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_versus_wage_growth_mechanism,
    'Is the systematic divergence between capital returns and wage growth a structural feature of capital accumulation or a contingent outcome of specific policy choices (tax treatment, labor law, intellectual property regimes, financial regulation)?',
    'Historical cross-sectional analysis comparing inequality trajectories in periods with different policy regimes; decomposition of aggregate inequality into policy-driven vs. structural-capital-dynamics components',
    'If structural: wealth inequality is partially immutable and constraint classifies toward mountain for analytical perspective. If policy-contingent: constraint is Tangled Rope for all perspectives and can be redistributed through institutional redesign. This distinction separates ''capitalism always produces inequality'' from ''this specific policy configuration amplifies inequality''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_versus_wage_growth_mechanism, empirical, 'Whether capital-wage divergence is structural or policy-driven').

omega_variable(
    intergenerational_wealth_transfer_fluidity,
    'Are wealth inequality gaps persistent across generations because of structural capital dynamics or because wealth transfer mechanisms (inheritance, educational access, network capital) create path dependence that policy could break?',
    'Intergenerational mobility analysis; comparison of mobility rates across different inheritance tax regimes and educational funding models; tracking of wealth persistence vs. educational/skill-based advancement',
    'If structural: constraint appears as mountain from long-term perspectives. If path-dependent: constraint is Tangled Rope/Scaffold with potential sunset through policy intervention (estate taxation, public education, capital access programs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_wealth_transfer_fluidity, empirical, 'Whether intergenerational wealth gaps reflect structure or path dependence').

omega_variable(
    financial_system_allocation_efficiency_vs_extraction,
    'Do capital markets and credit systems allocate resources to highest-productivity uses (efficiency function) or do they primarily extract rents through financial intermediation (extraction function)?',
    'Productivity analysis of capital-funded ventures; comparison of capital allocation patterns to actual output returns; measurement of rent extraction through spreads, fees, and financial complexity; cross-country comparison of financial system efficiency at different regulatory intensities',
    'If predominantly allocative: Rope classification dominates and constraint is viewed as coordination mechanism. If predominantly extractive: Snare classification dominates and constraint is viewed as rent-seeking system. This determines whether financial system reform should preserve the coordination function while reducing extraction or whether the system should be radically restructured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_system_allocation_efficiency_vs_extraction, empirical, 'Whether financial systems primarily allocate or extract').

omega_variable(
    asset_price_inflation_vs_genuine_wealth_creation,
    'Do rising asset prices (real estate, equities, financial instruments) represent genuine wealth creation or redistributive extraction from savers to asset owners through inflation?',
    'Cross-country asset price analysis in inflationary vs. stable-price periods; productivity-weighted valuation analysis; measurement of real returns vs. nominal returns; comparison of wealth concentration changes to measured economic productivity changes',
    'If wealth creation: rising asset values reflect genuine economic growth and distribution reflects market function. If redistribution: asset price inflation is extraction mechanism transferring wealth from non-owners to owners. Changes classification of beneficiary perspective from Rope to Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asset_price_inflation_vs_genuine_wealth_creation, empirical, 'Whether asset price appreciation reflects wealth creation or redistribution').

omega_variable(
    policy_capture_circularity,
    'Does wealth concentration cause policy capture which amplifies wealth inequality, creating a self-reinforcing cycle? If so, is the cycle breakable through policy intervention or has it achieved structural lock-in?',
    'Causal analysis of policy change timing relative to wealth concentration changes; identification of veto points where concentrated wealth can block egalitarian policy; historical cases of policy reversals and inequality reduction to establish whether breakpoints exist',
    'If cycle is breakable: constraint retains Tangled Rope classification and can be interrupted through coordinated policy. If lock-in: constraint becomes Snare and requires exogenous shock or revolutionary change. This distinction shapes whether reform can happen incrementally or requires systemic transformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_capture_circularity, empirical, 'Whether policy capture creates breakable cycles or structural lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wealth_inequality_amplification, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wealth_ineq_tr_t0, wealth_inequality_amplification, theater_ratio, 0, 0.38).
narrative_ontology:measurement(wealth_ineq_tr_t20, wealth_inequality_amplification, theater_ratio, 20, 0.48).
narrative_ontology:measurement(wealth_ineq_tr_t40, wealth_inequality_amplification, theater_ratio, 40, 0.55).
narrative_ontology:measurement(wealth_ineq_tr_t10, wealth_inequality_amplification, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(wealth_ineq_be_t0, wealth_inequality_amplification, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(wealth_ineq_be_t20, wealth_inequality_amplification, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(wealth_ineq_be_t40, wealth_inequality_amplification, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(wealth_ineq_be_t10, wealth_inequality_amplification, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wealth_inequality_amplification, resource_allocation).
narrative_ontology:boltzmann_floor_override(wealth_inequality_amplification, 0.18).
narrative_ontology:affects_constraint(wealth_inequality_amplification, capital_market_structure).
narrative_ontology:affects_constraint(wealth_inequality_amplification, inheritance_taxation_regime).
narrative_ontology:affects_constraint(wealth_inequality_amplification, labor_market_power_asymmetry).
narrative_ontology:affects_constraint(wealth_inequality_amplification, housing_market_extraction).
narrative_ontology:affects_constraint(wealth_inequality_amplification, financial_system_rent_extraction).
narrative_ontology:affects_constraint(wealth_inequality_amplification, educational_access_inequality).

% DUAL FORMULATION NOTE:
% Wealth inequality amplification decomposes into multiple structurally distinct constraints: capital market pricing mechanisms (affects returns to existing wealth), labor market power dynamics (affects wage formation), housing and land extraction (affects primary wealth accumulation avenue for middle class), financial intermediation rents (affects return distribution), and intergenerational transfer mechanisms (affects long-term inequality). Each has different ε values and different policy intervention points. The family of constraints creates a self-reinforcing system where extraction in one domain (housing) reduces capital formation capacity in another (productive assets). This network structure explains why inequality reduction at single intervention points fails — the system redistributes extraction rather than reducing it, unless multiple constraints are simultaneously reformed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wealth_inequality_amplification, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
