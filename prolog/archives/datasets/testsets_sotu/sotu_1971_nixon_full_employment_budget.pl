% ============================================================================
% CONSTRAINT STORY: sotu_1971_nixon_full_employment_budget
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1971_nixon_full_employment_budget, []).

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
 *   constraint_id: sotu_1971_nixon_full_employment_budget
 *   human_readable: Full Employment Budget as Fiscal Policy Anchor (1971)
 *   domain: economics/fiscal_policy
 *
 * SUMMARY:
 *   In August 1971, President Nixon announced the full employment budget
 *   framework as the centerpiece of his 'New Economic Policy.' Rather than
 *   constraining federal spending to match tax revenue, the administration
 *   committed to calibrating the budget to stimulate demand assuming the
 *   economy were already at full potential employment. This Keynesian
 *   mechanism operates through countercyclical stimulus: government spends as
 *   if full employment exists, thereby creating the conditions that realize
 *   full employment. The policy framework benefits unemployed workers and
 *   job-seeking firms through labor market tightening and investment
 *   opportunities; it costs inflation-averse savers and fiscal conservatives
 *   through deficit-financed stimulus and the inflation risk that accompanies
 *   sustained full employment budgeting. The constraint requires active
 *   coordination between fiscal authorities (Treasury), monetary authorities
 *   (Federal Reserve), and labor-management actors to prevent wage-price
 *   inflation spirals. The measurements show rising extractiveness and
 *   theater ratio over the interval, reflecting the framework's degradation
 *   as stagflation emerges and wage discipline erodes — by 1979, the full
 *   employment budget justification had become largely performative, used to
 *   rationalize continued deficits despite high inflation.
 *
 * KEY AGENTS:
 *   - Unemployed Workers: Primary beneficiary (moderate/mobile) — direct recipients of job creation and wage recovery through tight labor markets
 *   - Job-Seeking Firms: Primary beneficiary (institutional/arbitrage) — benefit from demand stimulus and reduced hiring frictions in tight labor markets
 *   - Inflation-Averse Savers: Primary victim (powerless/trapped) — fixed-income households and retirees whose purchasing power erodes through inflation redistribution mechanism
 *   - Fiscal Conservatives: Secondary victim (powerful/arbitrage) — prefer balanced budgets; experience constraint as political suppression of preferred policy despite having hedging options
 *   - Federal Reserve: Coordinating institution (institutional/constrained) — required to validate fiscal stimulus through accommodative monetary policy; bears credibility risk if inflation accelerates
 *   - Labor-Management Actors: Coordinating institutions (organized/constrained) — must exercise wage discipline to prevent Phillips Curve inflation spiral; benefit from tight labor markets but constrained by wage guidelines
 *   - New Economics Coalition: Policymakers and economists (organized/constrained) — architects of framework who see sunset logic; become captured by framework rhetoric as stagflation emerges
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating Phillips Curve as natural law rather than institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1971_nixon_full_employment_budget, 0.38).
domain_priors:suppression_score(sotu_1971_nixon_full_employment_budget, 0.42).
domain_priors:theater_ratio(sotu_1971_nixon_full_employment_budget, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1971_nixon_full_employment_budget, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1971_nixon_full_employment_budget, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1971_nixon_full_employment_budget, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1971_nixon_full_employment_budget, tangled_rope).
narrative_ontology:human_readable(sotu_1971_nixon_full_employment_budget, "Full Employment Budget as Fiscal Policy Anchor (1971)").
narrative_ontology:topic_domain(sotu_1971_nixon_full_employment_budget, "economics/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1971_nixon_full_employment_budget).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1971_nixon_full_employment_budget, unemployed_workers).
narrative_ontology:constraint_beneficiary(sotu_1971_nixon_full_employment_budget, job_seeking_firms).
narrative_ontology:constraint_beneficiary(sotu_1971_nixon_full_employment_budget, investment_seeking_capital).
narrative_ontology:constraint_victim(sotu_1971_nixon_full_employment_budget, inflation_averse_savers).
narrative_ontology:constraint_victim(sotu_1971_nixon_full_employment_budget, fixed_income_recipients).
narrative_ontology:constraint_victim(sotu_1971_nixon_full_employment_budget, fiscal_conservatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFLATION-AVERSE SAVER (SNARE) — Fixed-income households, retirees, and savers on fixed wages experience the constraint as pure extraction. Full employment budgeting transfers real purchasing power from savers to employed workers through inflation. No exit mechanism — savings are already deployed. Suppression is structural: the inflation mechanism itself is invisible to macro policy until it accelerates, and voters experience only the benefit narrative (more jobs) not the cost narrative (eroded savings). Biographical horizon: extraction persists across the working lifetime.
constraint_indexing:constraint_classification(sotu_1971_nixon_full_employment_budget, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNEMPLOYED WORKER (ROPE) — Direct beneficiary with modest but real mobility. The full employment budget coordinates employment seeking with job creation — low-friction matching mechanism. Net benefit: immediate income restoration plus dignity of work. Extraction is minimal (fiscal claim on public resources) and benefits the agent. Mobile exit option: workers can relocate, upskill, or enter different labor markets. The coordination function is genuine: the government's countercyclical spending creates conditions that enable voluntary exchange (work) rather than imposing constraints.
constraint_indexing:constraint_classification(sotu_1971_nixon_full_employment_budget, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL RESERVE (TANGLED ROPE) — Coordinating function: monetary policy must validate the fiscal stimulus without igniting runaway inflation. Benefits from the constraint: lower unemployment strengthens the central bank's dual mandate (price stability AND full employment). Costs: active management burden, credibility risk if inflation accelerates, loss of policy independence if political pressure overrides rate-setting autonomy. Constrained exit: cannot refuse participation without triggering unemployment or financial instability. Requires active enforcement: wage-price discipline accords, explicit Fed-Treasury coordination, or political pressure on labor/management to moderate wage claims.
constraint_indexing:constraint_classification(sotu_1971_nixon_full_employment_budget, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR-MANAGEMENT ACTORS (TANGLED ROPE) — Both benefit from lower unemployment (stronger labor market for workers, lower hiring frictions for firms) and bear extraction costs. Wage discipline is the cost: full employment budgets work only if workers do not translate tighter labor markets into wage claims that reignite inflation. Requires active enforcement: explicit wage guidelines, union-management accords, or implicit threat of budget reversal. Constrained exit: cannot exit full employment stimulus without abandoning the unemployment benefit. Perspectival gap: unions see this as extraction (wage suppression), management sees it as coordination (price stability).
constraint_indexing:constraint_classification(sotu_1971_nixon_full_employment_budget, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FISCAL CONSERVATIVE WEALTH HOLDERS (SNARE) — Paradoxically: this agent has arbitrage exit options (reposition assets, hedge inflation, emigrate capital) but the constraint is still experienced as extraction because the hedges are imperfect and the political constraint is real. Deficit spending transfers wealth from creditors to debtors through inflation. Wealthy households with asset mobility experience lower real extraction (they can hedge) but still bear the political-economy cost: the constraint requires suppressing their preferred policy (balanced budgets). The snare classification reflects the suppression gate (suppression ≥ 0.40) and their role as victims in the designed constraint, despite having high structural exit capacity.
constraint_indexing:constraint_classification(sotu_1971_nixon_full_employment_budget, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: NEW ECONOMICS COALITION (SCAFFOLD) — Keynesian economists and policymakers (Walter Heller, Gardner Ackley, Arthur Okun) see this as a temporary coordination framework with a sunset: full employment budgeting is valid during slack (high unemployment, low inflation) but requires sunset mechanisms when inflation accelerates. Theater ratio rises as the framework persists beyond its design window: Keynsian logic works when unemployment > 4-5% and inflation is subdued; it degrades when stagflation emerges. The sunset is structural: either inflation forces a return to balanced budgets (demand destruction), or explicit wage-price controls become necessary (command mechanism). Sunset clause: implicit, triggered by inflation threshold crossing. Low effective extraction for this organized actor because they have agency and foresee the exit path.
constraint_indexing:constraint_classification(sotu_1971_nixon_full_employment_budget, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — The civilizational perspective risks treating the Phillips Curve relationship (inflation-unemployment tradeoff) as a natural law of economics: you cannot have both full employment AND price stability; the tradeoff is structural. From this view, the full employment budget is simply choosing one point on an immutable constraint curve. However, the structural data contradicts this mountain classification: identifiable beneficiaries (workers, job seekers) and victims (savers, fiscal conservatives) reveal that the Phillips Curve relationship is itself a policy construct, not a law of nature. The curve shifts with expectations, wage-setting institutions, and supply shocks — it is not fixed. This perspective instantiates the oracle gap: the analytical frame naturalizes what cross-position analysis reveals as a contingent institutional arrangement.
constraint_indexing:constraint_classification(sotu_1971_nixon_full_employment_budget, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1971_nixon_full_employment_budget_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1971_nixon_full_employment_budget, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1971_nixon_full_employment_budget, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(sotu_1971_nixon_full_employment_budget_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The framework creates genuine benefits for unemployed workers (coordination function) and genuine costs for savers (inflation transfer). The extractiveness is not as high as a pure wealth transfer (which would be 0.65+) because the coordination component is real — tightening labor markets do enable voluntary employment matching. However, extractiveness rises over time from 0.15 (early frame, genuine slack) to 0.52 (late frame, approaching stagflation threshold) as the framework's inflation-generating properties become dominant. Suppression (0.42): Moderate. Mechanisms include: (1) inflation is a hidden mechanism of wealth transfer — savers experience loss without a clear causal attribution to policy; (2) political narrative suppresses fiscal conservative preferences — balanced budget advocates are marginalized by Keynesian consensus; (3) wage-price discipline requires implicit or explicit suppression of union wage claims. Suppression is not total (organized labor retains some voice, fiscal conservatives have media platforms) but significant structural barriers exist. Theater ratio (0.58): Moderate-high. The full employment budget framework requires extensive justification as inflation accelerates — the coordination function becomes less visible and the extraction function more visible. By the late 1970s, the framework is sustained largely through theoretical rhetoric ('the Phillips Curve is flatter than we thought,' 'inflation expectations are anchored') rather than demonstrated functional coordination.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's dual nature. Workers see coordination (Rope) — government spending creates jobs they want. The New Economics Coalition sees temporary coordination with sunset (Scaffold) — full employment budgets work during slack but require sunset when inflation accelerates. The Fed sees coordination with enforcement burden (Tangled Rope) — lower unemployment benefits the dual mandate but requires monetary policy tightening when inflation emerges. Labor-management actors see mixed coordination-extraction (Tangled Rope) — tight labor markets benefit them but wage discipline is extractive. Savers see pure extraction (Snare) — inflation transfers wealth with no offsetting coordination benefit. The analytical observer risks seeing an immutable tradeoff (Mountain) — the Phillips Curve is treated as natural law — but the beneficiary/victim data reveals this as a false summit: the inflation-unemployment tradeoff is institutional (depends on wage-setting institutions, expectations, policy credibility) not natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is derived from the agent's structural relationship to the constraint. Unemployed workers (beneficiary + mobile) derive low d → negative/low χ (they experience subsidy, not extraction). Inflation-averse savers (victim + trapped by already-deployed savings) derive high d → high χ (they experience maximum inflation tax). The Federal Reserve (beneficiary of lower unemployment + constrained by political pressure) derives moderate d → moderate χ. Labor-management actors split: unions see victimization (wage suppression) giving high d; management sees mixed benefit (lower unemployment, constrained wage costs) giving moderate d. Fiscal conservatives face an anomaly: they are nominally powerful and have arbitrage exit options (capital reallocation, emigration) but are trapped by political constraint (cannot exit the national fiscal framework) — the constraint specifies them as victims in the design, and politically powerful actors still experience suppression when their preferred policy (balanced budgets) is overridden. This paradox is resolved through the snare classification: suppression and victim status dominate the calculation despite arbitrage options, because the political suppression is structural and the extraction (through inflation) is real.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint contains genuine coordination (countercyclical stimulus matching employment seeking to job creation) alongside genuine extraction (inflation redistribution from savers to workers). The tangled_rope classification captures both functions. The constraint's legitimacy depends on the empirical resolution of the omega variables: (1) Is the Phillips Curve stable enough that wage discipline prevents runaway inflation? (2) Can the Fed credibly commit to non-accommodative policy if inflation accelerates? (3) Is the labor market actually at slack (unemployment below NAIRU) or structurally tight? If omega 1 and 2 are resolved affirmatively and omega 3 shows genuine slack, the constraint is legitimate coordination (lower extractiveness, more Rope than Snare). If they resolve negatively, the constraint becomes illegitimate extraction hidden behind Keynesian rhetoric (higher extractiveness, more Snare than Rope). The measurement trajectory (rising extractiveness and theater ratio 0.15→0.52 and 0.35→0.68) suggests progressive resolution toward higher extraction and more theater as the 1970s unfold — the empirical record indicates wage discipline failed and inflation accelerated, shifting the constraint toward Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phillips_curve_stability,
    'Is the Phillips Curve (inflation-unemployment tradeoff) a stable structural relationship or an artifact of expectations and wage-setting institutions?',
    'Empirical: compare Phillips Curve slope across different wage-setting regimes (union vs non-union, coordinated vs decentralized) and expectations environments (anchored vs unanchored inflation expectations). Conceptual: model whether the tradeoff is equilibrium-determined or institutional-determined.',
    'If stable structural law: full employment budget is constrained by real tradeoff, mountain classification holds. If institutional: the constraint''s extractiveness depends on ability to manage wage expectations — lower if credible wage discipline, higher if expectations become unanchored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phillips_curve_stability, empirical, 'Stability and structural source of Phillips Curve inflation-unemployment tradeoff').

omega_variable(
    wage_discipline_credibility,
    'Can the Federal Reserve and government credibly commit to wage-price discipline without resorting to command controls or unemployment-inducing policy reversal?',
    'Historical analysis: compare outcomes under explicit wage accords (1960s Guideposts, 1970s Pay Board) vs implicit coordination (1990s ''invisible handshake'') vs monetary credibility alone (Volcker disinflation). Measure: inflation acceleration rate, wage growth relative to productivity, union wage premium stability.',
    'If credible commitment is possible: constraint can sustain full employment without runaway inflation, tangled_rope classification holds. If not: full employment budgets systematically generate inflation, reclassify to snare (for savers) with higher extractiveness and higher suppression (inflation is hidden extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_discipline_credibility, empirical, 'Feasibility of non-coercive wage-price discipline in full employment regimes').

omega_variable(
    labor_market_slack_threshold,
    'Below what unemployment rate does the Phillips Curve become steep enough that full employment budgeting becomes inflationary regardless of wage discipline?',
    'Empirical: estimate the non-accelerating inflation rate of unemployment (NAIRU) for the 1970s economy; compare against actual unemployment during full employment budget era. Test whether NAIRU shifted due to changed supply shocks or expectations.',
    'If NAIRU > observed unemployment: budget-stimulated employment is beyond structural capacity, constraint reclassifies to higher extractiveness and higher suppression (inflation redistribution becomes dominant extraction mechanism). If NAIRU < observed unemployment: slack is genuine, full employment budget is coordination-dominant, tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_slack_threshold, empirical, 'NAIRU threshold determining inflationary impact of full employment budgets').

omega_variable(
    supply_shock_vulnerability,
    'Does the full employment budget framework have built-in mechanisms to absorb supply shocks (oil embargoes, commodity inflation) without collapsing into stagflation?',
    'Historical: analyze 1973-1974 oil shock response. Did full employment budget framework amplify or dampen the shock? Counterfactual: what would unemployment have been under balanced-budget constraint during the same shock?',
    'If vulnerable (no automatic absorbers): constraint''s suppressiveness increases during supply shocks, extractiveness becomes higher. If resilient: framework can coexist with supply shocks, classification remains tangled_rope with conditional sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_shock_vulnerability, empirical, 'Supply shock vulnerability of full employment budget framework').

omega_variable(
    deficit_monetization_loop,
    'Does the full employment budget create a self-reinforcing deficit-monetization dynamic where fiscal stimulus requires continuous monetary accommodation, eroding central bank independence?',
    'Historical: test whether deficit size and monetization rate became coupled during 1971-1979 period. Analyze Federal Reserve minutes for evidence of political pressure to accommodate deficits. Measure: inflation accumulation rate, real interest rate suppression.',
    'If strong coupling: the constraint is more extractive than apparent (hidden extraction through monetary degradation), classification holds but extractiveness may increase. If loose coupling: Fed independence is maintained, constraint remains tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deficit_monetization_loop, empirical, 'Degree of deficit monetization coupling and Fed independence erosion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1971_nixon_full_employment_budget, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu71_tr_t0, sotu_1971_nixon_full_employment_budget, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sotu71_tr_t3, sotu_1971_nixon_full_employment_budget, theater_ratio, 3, 0.45).
narrative_ontology:measurement(sotu71_tr_t6, sotu_1971_nixon_full_employment_budget, theater_ratio, 6, 0.58).
narrative_ontology:measurement(sotu71_tr_t9, sotu_1971_nixon_full_employment_budget, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(sotu71_be_t0, sotu_1971_nixon_full_employment_budget, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sotu71_be_t3, sotu_1971_nixon_full_employment_budget, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(sotu71_be_t6, sotu_1971_nixon_full_employment_budget, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(sotu71_be_t9, sotu_1971_nixon_full_employment_budget, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1971_nixon_full_employment_budget, resource_allocation).
narrative_ontology:affects_constraint(sotu_1971_nixon_full_employment_budget, phillips_curve_inflation_unemployment_tradeoff).
narrative_ontology:affects_constraint(sotu_1971_nixon_full_employment_budget, wage_price_control_framework_1971).
narrative_ontology:affects_constraint(sotu_1971_nixon_full_employment_budget, federal_reserve_monetary_accommodation_doctrine).

% DUAL FORMULATION NOTE:
% The full employment budget is downstream of the Phillips Curve relationship (the underlying economic theory) and upstream of the wage-price control framework (which becomes necessary when full employment budgeting generates inflation). These three constraints form a policy family: the Phillips Curve is the theoretical foundation (ε ≈ 0.08, Mountain-candidate); the full employment budget applies the theory as policy (ε ≈ 0.38, Tangled Rope); wage-price controls emerge when the budget fails to achieve inflation control (ε ≈ 0.55, Snare). The family tracks how policy responds to stagflation by adding coercive mechanisms (controls) rather than reconsidering the underlying Phillips Curve assumption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1971_nixon_full_employment_budget, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
