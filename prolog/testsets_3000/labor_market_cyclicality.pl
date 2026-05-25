% ============================================================================
% CONSTRAINT STORY: labor_market_cyclicality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_market_cyclicality, []).

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
 *   constraint_id: labor_market_cyclicality
 *   human_readable: Labor Market Cyclicality
 *   domain: economic/labor_policy
 *
 * SUMMARY:
 *   Labor market cyclicality is the recurring pattern of boom-bust employment
 *   dynamics endemic to capitalist economies. Periods of expansion create
 *   labor shortage and rising wages; periods of contraction create mass
 *   unemployment and wage collapse. This constraint exhibits the full range
 *   of DR classification because the cycle simultaneously functions as a
 *   coordination mechanism (allocating labor to productive uses) and an
 *   extraction mechanism (distributing volatility asymmetrically). The cycle
 *   coordinates labor supply with capital demand, enabling efficient resource
 *   allocation during growth phases. But it also extracts through wage
 *   volatility, unemployment risk, and precarity from workers while capital
 *   accumulates wealth through asset purchases during downturns. The theater
 *   ratio reflects that countercyclical policy (central bank rate
 *   adjustments, stimulus packages) is substantially performative: policy
 *   announcements signal intent to control the cycle but the actual
 *   effectiveness in smoothing employment volatility is contested. Over the
 *   measurement interval, extractiveness has increased from 0.38 to 0.58 as
 *   precariat employment has expanded (gig economy, outsourcing, union
 *   decline) while income volatility has grown despite increased policy
 *   intervention.
 *
 * KEY AGENTS:
 *   - Cyclically Displaced Workers: Primary victim (powerless/trapped) — experience mandatory participation in volatile labor markets with no exit; bear full burden of contraction phases through layoffs, reduced hours, and wage pressure
 *   - Permanent Precariat: Secondary victim (moderate/constrained) — internalize employment volatility as normal; experience coordination (the system does allocate labor) alongside extraction (bear disproportionate risk)
 *   - Capital Asset Holders: Primary beneficiary (institutional/arbitrage) — profit from both expansion (via capital gains and wage suppression) and contraction (via asset purchases at reduced prices); mobile exit options
 *   - Labor Union Coalition: Organized agent (organized/mobile) — see cyclicality as addressable through countercyclical policy; advocate for institutional sunset via stronger sectoral bargaining and automatic stabilizers
 *   - Multinational Corporations: Powerful actors (powerful/mobile) — exploit geographic labor arbitrage to externalize contraction risk to lower-wage jurisdictions while maintaining core workforce stability
 *   - Central Banking Institutions: Institutional actors (institutional/arbitrage) — perform countercyclical policy ritual with contested effectiveness; maintain theater of control
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable market laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_market_cyclicality, 0.58).
domain_priors:suppression_score(labor_market_cyclicality, 0.65).
domain_priors:theater_ratio(labor_market_cyclicality, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_market_cyclicality, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_market_cyclicality, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(labor_market_cyclicality, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_market_cyclicality, tangled_rope).
narrative_ontology:human_readable(labor_market_cyclicality, "Labor Market Cyclicality").
narrative_ontology:topic_domain(labor_market_cyclicality, "economic/labor_policy").

domain_priors:requires_active_enforcement(labor_market_cyclicality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_market_cyclicality, capital_asset_holders).
narrative_ontology:constraint_beneficiary(labor_market_cyclicality, labor_arbitrage_firms).
narrative_ontology:constraint_victim(labor_market_cyclicality, cyclically_displaced_workers).
narrative_ontology:constraint_victim(labor_market_cyclicality, precarious_workforce).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CYCLICALLY DISPLACED WORKER (SNARE) — Trapped within boom-bust cycles with no exit. Experiences mandatory participation in labor markets where contraction phases impose mass layoffs. Cannot escape the cycle through individual action; geographic mobility is constrained by housing markets; retraining lacks guarantee of employment. Maximum extraction without meaningful coordination benefit.
constraint_indexing:constraint_classification(labor_market_cyclicality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PERMANENT PRECARIAT (TANGLED ROPE) — Constrained by irregular employment patterns that function as coordination mechanism for labor supply flexibility. The cycle itself coordinates hiring surges with contractions, enabling businesses to adjust workforce size. But precariat workers bear disproportionate costs: income volatility, erosion of skill accumulation, reduced access to benefits. Mixed experience of coordination (the system does allocate labor) and extraction (costs are distributed asymmetrically).
constraint_indexing:constraint_classification(labor_market_cyclicality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL ASSET HOLDER (ROPE) — Experiences labor market cycles as a coordination mechanism for profit maximization. During expansion, wages rise but capital's returns accelerate faster. During contraction, wages collapse but capital purchases distressed assets at reduced prices. The cycle coordinates the periodic transfer of wealth from labor to capital. Net beneficiary with exit options — can withdraw capital, relocate investments, arbitrage across jurisdictions.
constraint_indexing:constraint_classification(labor_market_cyclicality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR UNION COALITION (SCAFFOLD) — Organized agents see cyclicality as a temporary market failure addressable through countercyclical policy: automatic stabilizers, union contracts with cost-of-living adjustments, and sectoral bargaining can smooth the cycle. The sunset mechanism is policy-based: if countercyclical institutions mature (strong sectoral unions, robust unemployment insurance, active labor market policies), the cycle's extraction component weakens. This perspective presumes policy choices can decouple productivity from cyclical volatility.
constraint_indexing:constraint_classification(labor_market_cyclicality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CENTRAL BANKING RITUAL (PITON) — Monetary policy (interest rate adjustments) is performed as a countercyclical mechanism but operates with substantial theater. Central banks present themselves as controllers of the cycle, but the mechanism is indirect and contested: lowering rates during downturns does not guarantee hiring; raising rates during booms does not reliably prevent overheating. The ritual of policy adjustment persists despite mixed effectiveness, maintained through institutional inertia and the need to appear to 'do something.' Theater ratio reflects that policy announcements have significant signaling effects independent of real economic effects.
constraint_indexing:constraint_classification(labor_market_cyclicality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTINATIONAL CORPORATION (TANGLED ROPE) — Powerful actors experience cyclicality as a coordination mechanism they can exploit through geographic labor arbitrage. A firm can maintain core operations in a high-wage stable economy while expanding/contracting contingent workforce in volatile, lower-wage jurisdictions. This creates mixed experience: genuine coordination of global labor supply (the cycle allocates talent to highest-productivity uses) but asymmetric extraction (low-wage workers bear contraction risk while high-wage core remains stable). Mobile exit options give this actor optionality the precariat lacks.
constraint_indexing:constraint_classification(labor_market_cyclicality, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, cyclicality appears as an immutable property of capitalist economies: endogenous business cycles (accelerator-multiplier dynamics, inventory cycles, credit cycles) are structural features of how market-based production accumulates. This perspective naturalizes cyclicality as a law of market economics. However, the structural data contradicts this classification — the engine will flag this as a false summit, revealing that policy choices (inflation targeting, labor market regulation, social insurance design) materially shape cycle amplitude and distributional consequences. The 'immutable' framing conceals contingent institutional arrangements.
constraint_indexing:constraint_classification(labor_market_cyclicality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_market_cyclicality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_market_cyclicality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_market_cyclicality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_market_cyclicality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_market_cyclicality, TR),
    TR >= 0.70.

:- end_tests(labor_market_cyclicality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The cycle creates genuine asymmetry in how contraction costs are distributed. Capital can withdraw or reallocate; precariat workers cannot. Rising extractiveness over the interval (0.38→0.58) reflects empirical growth of gig economy and contingent work, concentrating cycle volatility on workers without buffer. But extractiveness is not maximal (0.70+) because the cycle does coordinate real productivity gains during expansions and workers do benefit from growth. Suppression (0.65): Moderate-high. Significant barriers to escaping volatility include limited geographic mobility (housing market constraints), retraining deficits, path dependency in skill development, and information asymmetries about future employment. The suppression is structural but not absolute — some workers do skill-upgrade and transition across cycles. Theater ratio (0.48): Moderate. Countercyclical policy (monetary stimulus, fiscal packages) has real economic effects but also significant performative content. Central banks present rate adjustments as precise controls despite contested transmission mechanisms; stimulus packages are designed partly for political signaling; unemployment benefits are framed as automatic stabilizers but eligibility and duration are politically contested. The theater has grown as policy complexity has increased without corresponding transparency about effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival gap exists between the cyclically displaced worker (snare: trapped, powerless, biographical horizon) and the capital asset holder (rope: arbitrage, institutional, immediate horizon). Same macroeconomic phenomenon, opposite structural experiences. A secondary gap exists between the labor union coalition (scaffold: organized, generational, sunset through policy) and the analytical observer (mountain: civilizational, naturalized as immutable). The gap reveals whether cyclicality is seen as contingent on institutional design or as a fundamental law of market economies.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the extraction flow. Cyclically displaced workers are victims with trapped exit options: d→1.0, f(d)→1.42, chi→high. Permanent precariat are victims with constrained exit: d→0.65, f(d)→1.00, chi→moderate-high. Capital asset holders are beneficiaries with arbitrage exit: d→0.05, f(d)→-0.12, chi→negative or low. Labor unions are organized victims with some exit (mobility across sectors, union protection): d→0.40, f(d)→0.40, chi→moderate. Multinational corporations are powerful beneficiaries with mobile exit: d→0.30, f(d)→0.10, chi→low. The central banking ritual derives d from institutional position observing the cycle rather than directly experiencing extraction — d→0.72 (observer position), producing chi consistent with piton classification despite the institutional power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that the cycle simultaneously coordinates labor allocation (rope/scaffold functions) and extracts through asymmetric volatility distribution (snare/tangled rope functions). The question 'is cyclicality coordination or extraction?' has no single answer — it is both, and the proportion depends on institutional design. In economies with weak automatic stabilizers and fragmented labor markets, extraction dominates (snare/tangled rope classifications). In economies with sectoral bargaining and strong unemployment insurance, coordination function is larger (scaffold/rope classifications). The mountain perspective is a false summit: the analytical observer's claim that cyclicality is immutable confuses 'observed in all capitalist economies' with 'inherent to capitalism.' Actual cross-national evidence shows cycle amplitude and distributional properties are policy-malleable. The mandatrophy dissolves when we recognize that the constraint's type depends on institutional context, not just on structural properties of labor demand.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cycle_endogeneity_vs_exogeneity,
    'Are labor market cycles endogenous to capitalist dynamics or driven primarily by external shocks (oil prices, financial crises, pandemics)?',
    'Spectral analysis of employment data with and without exogenous shock periods; comparison of cycle regularity across different shock regimes; vector autoregression of labor market indicators against exogenous variables',
    'If endogenous: cyclicality is a structural feature of capitalism and the mountain perspective has validity. If exogenous: cycles are contingent on external factors, and policy/institutional design can substantially reduce amplitude, strengthening scaffold and organized perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cycle_endogeneity_vs_exogeneity, empirical, 'Whether labor cycles are endogenous market dynamics or driven by external shocks').

omega_variable(
    distributional_inevitability,
    'Is the asymmetric distribution of cycle costs (precariat bears more) an inherent feature of flexibility or a policy choice about how to allocate cyclical burden?',
    'Cross-national comparison of cycle amplitude and distributional outcomes; analysis of countries with strong automatic stabilizers, sectoral unions, and generous unemployment insurance vs. those with weak institutions; correlation between institutional strength and volatility of worker income relative to capital returns',
    'If inevitable: snare classification is justified — workers cannot escape asymmetric burden. If policy-driven: scaffold and organized perspectives become more structural, suggesting institutional redesign can substantially reduce extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_inevitability, empirical, 'Whether asymmetric cost distribution is structural or policy-contingent').

omega_variable(
    precariat_identity_lock,
    'Do workers in precarious employment internalize cyclicality as ''the way labor markets work'' such that they become identity-locked to unstable employment, or do they experience it as external structural constraint?',
    'Longitudinal survey data on worker expectations, identity narratives, and perceived control over employment stability; analysis of intergenerational transmission of precarity; comparison of worker narratives in high-volatility vs. low-volatility labor markets',
    'If identity-locked: workers cannot conceptually exit the cycle even when structural barriers are reduced, requiring interventions targeting frames and self-concepts. If constrained: barrier reduction (stronger unions, job guarantees, sectoral bargaining) creates genuine exit opportunities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precariat_identity_lock, empirical, 'Whether precariat workers are identity-locked to cyclical volatility').

omega_variable(
    monetary_policy_efficacy,
    'Does countercyclical monetary policy (central bank rate manipulation) actually smooth labor market cycles, or is it theater with limited real effects?',
    'Time-series econometric analysis of monetary policy shocks and employment response; identification of policy rate changes independent of other factors; lag analysis of policy effects on hiring vs. wage-setting; comparison of policy effectiveness across different labor market institutions',
    'If efficacious: piton classification is incorrect and central banking is genuine coordination mechanism. If theater: piton is correct and the policy ritual persists despite weak effects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monetary_policy_efficacy, empirical, 'Whether central bank monetary policy effectively smooths employment cycles').

omega_variable(
    alternative_coordination_mechanisms,
    'Can sectoral bargaining, job guarantee programs, or automatic stabilizers coordinate labor demand as effectively as price-driven cyclical adjustment?',
    'Natural experiments from countries adopting strong labor market institutions (Scandinavian countries, Germany during crises); comparison of employment volatility, wage volatility, and unemployment duration across different institutional regimes; analysis of recession severity in countries with strong countercyclical policy',
    'If alternative mechanisms work: scaffold sunset is structurally feasible, and the cycle''s extraction is policy-contingent rather than immutable. If they fail: cyclicality persists regardless of institutional design, supporting mountain view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, empirical, 'Efficacy of institutional alternatives to cyclical employment adjustment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_market_cyclicality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lmc_tr_t0, labor_market_cyclicality, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lmc_tr_t5, labor_market_cyclicality, theater_ratio, 5, 0.42).
narrative_ontology:measurement(lmc_tr_t10, labor_market_cyclicality, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(lmc_be_t0, labor_market_cyclicality, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(lmc_be_t5, labor_market_cyclicality, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(lmc_be_t10, labor_market_cyclicality, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_market_cyclicality, resource_allocation).
narrative_ontology:affects_constraint(labor_market_cyclicality, wage_stagnation_in_recovery).
narrative_ontology:affects_constraint(labor_market_cyclicality, unemployment_insurance_inadequacy).
narrative_ontology:affects_constraint(labor_market_cyclicality, precarious_employment_expansion).

% DUAL FORMULATION NOTE:
% Labor market cyclicality is the macro phenomenon encompassing three distinct structural constraints: (1) wage_stagnation_in_recovery (ε≈0.35) — wages lag productivity during expansions, concentrated among lower-wage workers; (2) unemployment_insurance_inadequacy (ε≈0.52) — benefits do not cover income loss during contractions, creating liquidity crises; (3) precarious_employment_expansion (ε≈0.68) — gig/contingent work has grown as firms externalize cyclical volatility to workers. The macro extractiveness (0.58) reflects the combined effect, but the micro-mechanisms have different ε values and different institutional interventions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_market_cyclicality, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
