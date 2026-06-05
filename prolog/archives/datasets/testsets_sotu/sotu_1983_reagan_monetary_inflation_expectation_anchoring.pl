% ============================================================================
% CONSTRAINT STORY: sotu_1983_reagan_monetary_inflation_expectation_anchoring
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1983_reagan_monetary_inflation_expectation_anchoring, []).

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
 *   constraint_id: sotu_1983_reagan_monetary_inflation_expectation_anchoring
 *   human_readable: Inflation Expectation Anchoring via Lender Confidence Restoration (1983)
 *   domain: economic_policy/monetary_transmission
 *
 * SUMMARY:
 *   The Reagan-era disinflation mechanism (1980-1984) represents a structural
 *   constraint on nominal interest rates imposed through expectation
 *   anchoring rather than fiscal stimulus. The Federal Reserve under Volcker
 *   commits credibly to reducing inflation from 13.5% (1980) to 3% (1983),
 *   causing nominal prime rates to fall from 21.5% to 10.5% despite the lack
 *   of new fiscal stimulus. The constraint operates by anchoring inflation
 *   expectations in lender psychology — as households and firms believe the
 *   Fed will maintain price stability, they reduce inflation risk premiums
 *   embedded in nominal lending rates. This enables housing starts to surge
 *   45% and auto sales to recover without direct fiscal support. However, the
 *   constraint's coordination function (restoring sustainable nominal lending
 *   rates) is inseparable from its extractive mechanism: the transition
 *   requires unemployment to spike to 10.8% (November 1982), farm
 *   foreclosures to accelerate, and high-leverage debtors to face sharply
 *   rising real debt service burdens. The constraint is tangled_rope: genuine
 *   coordination (restoring lender confidence in real returns, enabling
 *   capital formation for productive investment) coexists with asymmetric
 *   extraction (unemployment, debt service shock, savers gaining at
 *   borrowers' expense during the transition). The distributional conflict is
 *   not ancillary to the mechanism — it IS the mechanism. Falling
 *   inflationary expectations increase real debt burdens and reduce nominal
 *   demand, which is precisely what drives unemployment and releases
 *   resources for export-oriented sectors.
 *
 * KEY AGENTS:
 *   - Federal Reserve (Volcker institution): Institutional beneficiary (institutional/arbitrage) — captures credibility gains from disinflation commitment; constrained by political pressure and time horizon
 *   - Mortgage borrowers and housing sector: Primary beneficiary (institutional/arbitrage + moderate/constrained) — benefit from falling mortgage rates and sector recovery; housing starts +45%
 *   - Auto industry and export-oriented manufacturing: Primary beneficiary (institutional/arbitrage) — benefit from falling capital costs and real exchange rate anchor enabling competitiveness
 *   - Unemployed workers: Primary victim (powerless/trapped) — bear the full cost of demand destruction during disinflation; unemployment reaches 10.8%
 *   - High-debt-service debtors (farms, leveraged firms): Primary victim (powerless/trapped) — experience rising real debt burdens as expectations fall; farm foreclosures accelerate
 *   - Savers in nominal assets: Secondary beneficiary (moderate/constrained) — gain real returns as inflation expectations fall; but no active agency in the constraint mechanism
 *   - Labor unions and social support system: Organized response (organized/constrained) — provide unemployment insurance and retraining for transition period
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1983_reagan_monetary_inflation_expectation_anchoring, 0.52).
domain_priors:suppression_score(sotu_1983_reagan_monetary_inflation_expectation_anchoring, 0.68).
domain_priors:theater_ratio(sotu_1983_reagan_monetary_inflation_expectation_anchoring, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1983_reagan_monetary_inflation_expectation_anchoring, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1983_reagan_monetary_inflation_expectation_anchoring, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sotu_1983_reagan_monetary_inflation_expectation_anchoring, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1983_reagan_monetary_inflation_expectation_anchoring, tangled_rope).
narrative_ontology:human_readable(sotu_1983_reagan_monetary_inflation_expectation_anchoring, "Inflation Expectation Anchoring via Lender Confidence Restoration (1983)").
narrative_ontology:topic_domain(sotu_1983_reagan_monetary_inflation_expectation_anchoring, "economic_policy/monetary_transmission").

domain_priors:requires_active_enforcement(sotu_1983_reagan_monetary_inflation_expectation_anchoring).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1983_reagan_monetary_inflation_expectation_anchoring, mortgage_borrowers).
narrative_ontology:constraint_beneficiary(sotu_1983_reagan_monetary_inflation_expectation_anchoring, housing_sector).
narrative_ontology:constraint_beneficiary(sotu_1983_reagan_monetary_inflation_expectation_anchoring, auto_industry).
narrative_ontology:constraint_beneficiary(sotu_1983_reagan_monetary_inflation_expectation_anchoring, manufacturing_export_competitiveness).
narrative_ontology:constraint_victim(sotu_1983_reagan_monetary_inflation_expectation_anchoring, high_debt_service_debtors).
narrative_ontology:constraint_victim(sotu_1983_reagan_monetary_inflation_expectation_anchoring, unemployed_workers).
narrative_ontology:constraint_victim(sotu_1983_reagan_monetary_inflation_expectation_anchoring, savers_in_nominal_assets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNEMPLOYED WORKER (SNARE) — Bears the full cost of disinflation without exit option. Trapped in the recession that collapses nominal demand and employment. No mechanism for sharing gains from falling expectations; unemployment peaks at 10.8% (Nov 1982) before recovery. Experiences pure extraction: costs imposed, no coordination benefit.
constraint_indexing:constraint_classification(sotu_1983_reagan_monetary_inflation_expectation_anchoring, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HIGH-LEVERAGE DEBTOR (SNARE) — Trapped by nominal debt burden. When inflationary expectations fall, real debt service rises sharply. Borrower locked into high nominal rates (21.5% starting point) as real rates compress. Cannot refinance; cannot exit the constraint. Farm foreclosures rise dramatically during this period. Pure extraction with suppression of exit options.
constraint_indexing:constraint_classification(sotu_1983_reagan_monetary_inflation_expectation_anchoring, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: HOUSING SECTOR (TANGLED ROPE) — Constrained by capital availability but benefits from falling long rates. As mortgage rates decline from ~16% (1981) toward 12-13% (1983), housing starts surge 45%, permits surge 60%. Genuine coordination function: lower rates enable construction activity that was impossible at high nominal rates. BUT: workers face intermittent employment, unpredictable project flow, and wage compression from labor surplus. Moderate extraction embedded in genuine sector recovery.
constraint_indexing:constraint_classification(sotu_1983_reagan_monetary_inflation_expectation_anchoring, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AUTO INDUSTRY (ROPE) — Primary beneficiary. Falling nominal rates reduce cost of capital for retooling and inventory. Declining inflationary expectations strengthen the dollar (nominal anchor attracts capital), enabling export competitiveness despite higher real rates. Industry experiences the constraint as pure coordination: expectation anchoring enables real sector stimulus without new fiscal stimulus. Low net extraction — benefits outweigh costs.
constraint_indexing:constraint_classification(sotu_1983_reagan_monetary_inflation_expectation_anchoring, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FINANCIAL SECTOR (ROPE) — Benefits from expectation anchoring. Lenders experience falling inflation expectations as a coordination mechanism that restores pricing power and confidence in real returns on long-dated lending. Falling expectations reduce the 'inflation risk premium' embedded in nominal rates, allowing rates to fall without default risk rising. Net beneficiary with genuine coordination function: enabling sustainable lending volumes.
constraint_indexing:constraint_classification(sotu_1983_reagan_monetary_inflation_expectation_anchoring, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LABOR AND SOCIAL SUPPORT (SCAFFOLD) — Organized response (unions, unemployment insurance, retraining programs) to temporary disinflation costs. The constraint is temporary: as expectations anchor, growth recovers (real GDP growth 5.3% in 1984). Exit is visible but painful — 2-3 year transition period before unemployment falls and wage growth resumes. Theater ≤ 0.70 because genuine economic mechanisms (not performance) drive recovery. Sunset clause implicit: disinflation is transitory policy, not permanent constraint.
constraint_indexing:constraint_classification(sotu_1983_reagan_monetary_inflation_expectation_anchoring, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: FEDERAL RESERVE (TANGLED ROPE) — Institutional actor constrained by commitment to disinflation but beneficiary of expectation anchoring credibility. Volcker's institution experiences the constraint as a coordination mechanism (restoring long-run price stability expectations) with embedded extraction (unemployment costs imposed, not by Fed choice but by the transition mechanism). Constrained by political pressure and time horizon; unable to exit the disinflation commitment without credibility collapse.
constraint_indexing:constraint_classification(sotu_1983_reagan_monetary_inflation_expectation_anchoring, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, the inflation-expectation trap appears as an immutable feature of monetary economics: once inflationary expectations de-anchor, restoring them requires nominal pain (unemployment, debt service shock). The constraint appears as a natural law of price formation. However, structural data reveals this as a false summit: the extraction mechanism is institutional (central bank credibility, lender psychology, nominal debt contracts) not physical law. The 'cost' of disinflation is contingent on policy design choices.
constraint_indexing:constraint_classification(sotu_1983_reagan_monetary_inflation_expectation_anchoring, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1983_reagan_monetary_inflation_expectation_anchoring_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1983_reagan_monetary_inflation_expectation_anchoring, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1983_reagan_monetary_inflation_expectation_anchoring, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1983_reagan_monetary_inflation_expectation_anchoring, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1983_reagan_monetary_inflation_expectation_anchoring_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits strong extraction during the transition phase (0-12 months: 0.68) because the unemployment shock is severe and concentrated. Unemployment rises from 7.8% (1981) to 10.8% (Nov 1982), destroying nominal demand and imposing costs on powerless agents with no exit option. However, extractiveness declines over the biographical horizon (12-24 months: 0.35-0.42) as recovery begins — housing starts recover, manufacturing employment rebounds, and the gains from lower nominal rates compound. The measured extractiveness (0.52) is the average across the full transition period, reflecting that the constraint is transitory (not permanent Snare) but acutely extractive in the short term. Suppression (0.68): High. The constraint suppresses alternatives through multiple mechanisms: (a) unemployment makes job exit costlier, (b) nominal debt contracts lock debtors into fixed terms, (c) central bank commitment removes monetary policy escape hatch, (d) fiscal policy is contractionary (Reagan deficits are future phenomenon; 1982 saw revenue collapse and budget stress). But suppression is not maximal (0.68 not 0.85) because some exit paths exist: refinancing opportunities for creditworthy borrowers, relocation options for unemployed workers, Federal Reserve's commitment could theoretically be broken (low probability but not zero). Theater ratio (0.38): Low-moderate. The constraint's mechanism is substantially real: falling inflation expectations genuinely reduce lender risk premiums, enabling lower nominal rates. This is not performative — the rate declines are measurable and immediate. However, some theater exists: the narrative emphasizing 'credible disinflation' and 'expectation anchoring' involves rhetorical work (Reagan messaging, Volcker press conferences) that shapes belief. The theater ratio rises slightly over time (0.25 → 0.41) as the constraint persists and agents begin to understand the mechanism intellectually rather than experiencing it as purely mechanical rate decline.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximal perspectival divergence because agents experience opposite signs of the same mechanism. The Fed and auto industry see Rope (pure coordination enabling capital allocation). The housing sector sees Tangled Rope (mixed coordination and extraction). The unemployed and trapped debtors see Snare (pure extraction with suppression of alternatives). The analytical observer risks seeing Mountain (immutable law of monetary economics: you cannot reduce inflation without unemployment) but this is a false summit — the 'law' is contingent on: (a) nominal debt contracts, (b) central bank independence from fiscal policy, (c) labor market inflexibility, (d) political inability to coordinate wage/price expectations. Different policy architectures (indexed debt, fiscal coordination, wage negotiation) would yield different cost distributions. The gap is not an artifact of observation position — it's the real distributional conflict in the constraint structure itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across perspectives because agents occupy radically different structural positions relative to this constraint. Mortgage borrowers and auto industry are net beneficiaries (d ≈ 0.25-0.35) — they have arbitrage options and capture gains from lower nominal rates. The Federal Reserve is a beneficiary-constrained actor (d ≈ 0.20) — it benefits from credibility gains but is locked into the disinflation commitment. Unemployed workers and trapped debtors are pure targets (d ≈ 0.95) — they bear costs with no exit option. The housing sector is mixed (d ≈ 0.55) — it benefits from lower rates but workers face volatile employment and wage pressure. The directionality derivation from beneficiary/victim status produces: f(d) ranges from -0.12 (institutional beneficiaries with arbitrage) to 1.42 (powerless trapped victims). When scaled by scope (σ(national) = 1.0) and base extractiveness (0.52), effective extraction χ ranges from negative (beneficiaries see the constraint as enabling real gains) to highly extractive (trapped victims see pure cost imposition). The perspectival gap is therefore not about perspective-dependent measurement — it's about real distributional asymmetry embedded in the constraint's mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint is Tangled Rope, not Snare disguised as coordination. The mandatrophy resolves because: (1) genuine coordination function exists: lender confidence restoration enables mortgage lending and capital formation that was impossible at 21.5% rates, (2) asymmetric extraction exists: unemployment and debt service shocks are concentrated on powerless agents, (3) active enforcement required: Federal Reserve maintains disinflation commitment despite political pressure; without enforcement, expectations de-anchor and rates rise. The constraint is NOT pure extraction because the coordination function is not merely rhetorical — housing starts genuinely spike 45%, auto sales recover, the manufacturing sector regains export competitiveness. These are real productive gains, not performance theater. But the extraction is real too — unemployment peaks at 10.8%, farm foreclosures spike, trapped debtors face sharply rising real burdens. The classification as Tangled Rope (not Snare) reflects that the constraint enables genuine real sector activity while asymmetrically imposing transition costs. The distributional conflict is intrinsic: you cannot restore lender confidence and anchor expectations without demand destruction, because falling inflationary expectations mechanically raise real debt burdens and reduce nominal purchasing power. The constraint resolves mandatrophy by showing how tangled_rope and snare can coexist at different observables within the same mechanism: from the lender's perspective (rope), from the unemployed worker's perspective (snare), from the debtor's perspective (snare), from the manufacturing sector's perspective (rope). The engine's multi-perspective classification captures this as perspectival divergence, which is the analytically correct reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expectation_anchoring_mechanism,
    'What actual mechanism enables expectation anchoring to reduce nominal rates without purely extractive shock?',
    'Decompose inflation rate decline (Δπ actual) into: (a) monetary policy transmission (M velocity change), (b) expectation shift in financial pricing, (c) demand destruction via unemployment. Compare relative magnitudes across 1982-1984.',
    'If primarily (b): constraint is information/confidence coordination (Rope dominates). If primarily (c): constraint is extraction mechanism (Snare/Tangled Rope dominates). If mixed: tangled_rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expectation_anchoring_mechanism, empirical, 'Mechanism by which expectation anchoring reduces nominal rates').

omega_variable(
    unemployment_necessity_vs_policy_choice,
    'Was the unemployment spike (peak 10.8% Nov 1982) a necessary cost of disinflation or a policy choice amplified by tighter-than-necessary monetary contraction?',
    'Counterfactual modeling: estimate disinflation path under gradual deceleration (1980-1985) vs. the actual shock path (1980-1982 deep contraction). Compare output loss per inflation-percentage-point reduction.',
    'If necessary: suppression is a structural feature (0.68 confirmed). If amplified by policy: suppression includes institutional choice (0.68 splits into 0.35 structural + 0.33 discretionary). Affects ethical frame for evaluating extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unemployment_necessity_vs_policy_choice, conceptual, 'Whether unemployment was necessary cost or policy choice').

omega_variable(
    debt_service_denominator_shift,
    'In nominal-debt contracts, does falling inflationary expectation increase real debt service burden (transfers wealth from borrowers to savers) or does it trigger refinancing that redistributes gains?',
    'Track mortgage refi volumes and rate changes 1982-1984. Compare average borrower rate change (portfolio refi) vs. prevailing rate change (contract rate at origination). If portfolio tracks prevailing: borrowers capture rate decline gains. If portfolio lags prevailing: borrowers bear wealth transfer.',
    'If refinancing tracks prevailing rates: Snare classification for trapped debtors is incorrect (should be Tangled Rope with partial recovery). If borrowers locked out of refinancing (credit constraints, contract terms): Snare confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_service_denominator_shift, empirical, 'Whether debt contracts enable borrower recovery via refinancing').

omega_variable(
    export_competitiveness_mechanism,
    'Do falling inflationary expectations strengthen export competitiveness through real exchange rate appreciation (nominal anchor attracts capital inflow) or through relative cost deflation?',
    'Decompose trade flow changes 1982-1984 into: (a) real exchange rate appreciation (does DXY strengthen relative to major trading partners?), (b) relative cost compression (do US unit labor costs fall relative to competitors?), (c) global demand recovery (is growth driven by US exports to recovering economies or by domestic demand from falling rates?).',
    'If primarily (a) and (c): constraint enables real sector stimulus without fiscal cost (Rope/tangled_rope coordination dominates). If primarily (b): constraint relies on wage suppression (extraction dominates). Mixed mechanisms confirm tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(export_competitiveness_mechanism, empirical, 'Mechanism by which expectations improve export competitiveness').

omega_variable(
    distributional_permanence,
    'Do gains from housing/auto recovery (45% rise in housing starts, assumed employment rebound in construction/manufacturing) offset losses borne by unemployed and trapped debtors over the biographical time horizon?',
    'Longitudinal tracking: (i) follow unemployed cohort forward 5 years post-1982 (wage paths, employment recovery, welfare impact), (ii) track construction/manufacturing employment recovery and wage effects, (iii) measure net household wealth change by decile. Sum welfare gains and losses.',
    'If gains exceed losses (weighted by duration and intensity): extraction is temporary/transitory (Scaffold/tangled_rope confirmed). If losses persist (some cohorts never recover, permanent wage depression): extraction is durable (Snare dominates).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributional_permanence, empirical, 'Whether recovery gains offset transition losses').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1983_reagan_monetary_inflation_expectation_anchoring, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu83_tr_t0, sotu_1983_reagan_monetary_inflation_expectation_anchoring, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sotu83_tr_t6, sotu_1983_reagan_monetary_inflation_expectation_anchoring, theater_ratio, 6, 0.32).
narrative_ontology:measurement(sotu83_tr_t12, sotu_1983_reagan_monetary_inflation_expectation_anchoring, theater_ratio, 12, 0.38).
narrative_ontology:measurement(sotu83_tr_t24, sotu_1983_reagan_monetary_inflation_expectation_anchoring, theater_ratio, 24, 0.41).

% Extraction over time
narrative_ontology:measurement(sotu83_be_t0, sotu_1983_reagan_monetary_inflation_expectation_anchoring, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(sotu83_be_t6, sotu_1983_reagan_monetary_inflation_expectation_anchoring, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(sotu83_be_t12, sotu_1983_reagan_monetary_inflation_expectation_anchoring, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(sotu83_be_t24, sotu_1983_reagan_monetary_inflation_expectation_anchoring, base_extractiveness, 24, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1983_reagan_monetary_inflation_expectation_anchoring, resource_allocation).
narrative_ontology:affects_constraint(sotu_1983_reagan_monetary_inflation_expectation_anchoring, nominal_debt_burden_real_appreciation).
narrative_ontology:affects_constraint(sotu_1983_reagan_monetary_inflation_expectation_anchoring, unemployment_wage_compression_mechanism).
narrative_ontology:affects_constraint(sotu_1983_reagan_monetary_inflation_expectation_anchoring, export_competitiveness_real_exchange_rate_anchor).

% DUAL FORMULATION NOTE:
% The expectation-anchoring constraint is upstream of three decomposed constraints: (1) nominal debt burden mechanics (how falling expectations increase real burdens via unchanged nominal payment schedules), (2) unemployment transmission (how demand destruction from falling expectations creates involuntary joblessness), (3) export competitiveness (how nominal anchor attracts capital inflow, strengthening real exchange rate). Each has distinct ε: the anchoring mechanism itself (ε=0.52, Tangled Rope) flows downstream to debt service mechanics (ε>0.70, Snare for trapped debtors), unemployment transmission (ε>0.75, Snare), and export benefit (ε≈0.15, Rope for manufacturing). The family structure shows how a single macro-institution creates multiple layered constraints at different levels of decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1983_reagan_monetary_inflation_expectation_anchoring, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
