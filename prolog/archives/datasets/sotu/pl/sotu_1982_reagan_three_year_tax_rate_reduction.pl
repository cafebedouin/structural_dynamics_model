% ============================================================================
% CONSTRAINT STORY: sotu_1982_reagan_three_year_tax_rate_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1982_reagan_three_year_tax_rate_reduction, []).

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
 *   constraint_id: sotu_1982_reagan_three_year_tax_rate_reduction
 *   human_readable: Three-Year Phased Tax Rate Reduction (1981–1984)
 *   domain: economic_policy/fiscal_stimulus
 *
 * SUMMARY:
 *   The three-year phased tax rate reduction implemented by the Reagan
 *   administration (1981–1984) exemplifies a fiscal constraint designed to
 *   break stagflation by increasing disposable income and business investment
 *   capacity. The mechanism is presented as a temporary demand-side stimulus
 *   with a built-in sunset clause — rates will revert after three years if
 *   deficit pressures return to normal levels. However, the constraint
 *   exhibits deep structural ambiguity: it operates simultaneously as a
 *   genuine coordination mechanism (relief for inflation-squeezed workers),
 *   an asymmetrically distributional extraction mechanism (concentrating
 *   gains in upper income brackets), a temporary scaffold (with stated
 *   sunset), a degraded institutional arrangement (progressive tax code
 *   collapse maintained by ideological theater), and a false natural law (the
 *   claim that tax reduction 'naturally' stimulates growth independent of
 *   distributional mechanics). The measured extractiveness increases from
 *   0.28 (year 0, when the cuts appear primarily coordinating) to 0.52 (year
 *   4, as deficit accumulation reveals the extraction component). Theater
 *   ratio climbs from 0.42 to 0.58, indicating growing gap between the stated
 *   supply-side stimulus rationale and actual implementation (capital flows
 *   to financial assets and upper-income consumption rather than productive
 *   investment). The constraint's classification depends entirely on the
 *   observer's structural position: workers see relief (Rope), high earners
 *   see windfall (Rope/Tangled Rope), institutional revenue collectors see
 *   loss (Snare), organized coalitions see temporary crisis management
 *   (Scaffold), the tax code infrastructure sees institutional degradation
 *   (Piton), and the civilizational analyst risks naturalizing policy choice
 *   as macroeconomic law (Mountain).
 *
 * KEY AGENTS:
 *   - High-Income Earners and Business Investment Sector: Primary beneficiaries (powerful/arbitrage) — capture 53% of tax cuts despite representing 10% of population; gain from rate structure collapse
 *   - Inflation-Sensitive Wage Workers: Primary beneficiaries and coordination participants (moderate/mobile) — gain relief from stagflation squeeze; experience constraint as enabling coordination
 *   - Federal Revenue Base: Primary victim (powerless/trapped) — bears full extraction in form of revenue loss, reduced public service capacity, accumulated deficit burden
 *   - Public Services Infrastructure Dependents: Secondary victim (moderate/constrained) — endure service cuts and deficit burden; constrained exit to alternative service provision
 *   - Supply-Side Reform Coalition: Organized beneficiaries (organized/constrained) — political and economic actors committed to cutting progressive taxation; see constraint as temporary crisis measure with clear sunset
 *   - Progressive Tax Code Institution: Institutional actor experiencing degradation (institutional/arbitrage) — structured to redistribute progressively; subject to mechanical rate collapse maintained by ideological theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1982_reagan_three_year_tax_rate_reduction, 0.52).
domain_priors:suppression_score(sotu_1982_reagan_three_year_tax_rate_reduction, 0.35).
domain_priors:theater_ratio(sotu_1982_reagan_three_year_tax_rate_reduction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1982_reagan_three_year_tax_rate_reduction, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1982_reagan_three_year_tax_rate_reduction, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sotu_1982_reagan_three_year_tax_rate_reduction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1982_reagan_three_year_tax_rate_reduction, tangled_rope).
narrative_ontology:human_readable(sotu_1982_reagan_three_year_tax_rate_reduction, "Three-Year Phased Tax Rate Reduction (1981–1984)").
narrative_ontology:topic_domain(sotu_1982_reagan_three_year_tax_rate_reduction, "economic_policy/fiscal_stimulus").

domain_priors:requires_active_enforcement(sotu_1982_reagan_three_year_tax_rate_reduction).
narrative_ontology:has_sunset_clause(sotu_1982_reagan_three_year_tax_rate_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1982_reagan_three_year_tax_rate_reduction, high_income_earners).
narrative_ontology:constraint_beneficiary(sotu_1982_reagan_three_year_tax_rate_reduction, business_investment_sector).
narrative_ontology:constraint_beneficiary(sotu_1982_reagan_three_year_tax_rate_reduction, inflation_sensitive_wage_workers).
narrative_ontology:constraint_victim(sotu_1982_reagan_three_year_tax_rate_reduction, federal_revenue_base).
narrative_ontology:constraint_victim(sotu_1982_reagan_three_year_tax_rate_reduction, public_services_infrastructure).
narrative_ontology:constraint_victim(sotu_1982_reagan_three_year_tax_rate_reduction, future_deficit_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL REVENUE BASE (SNARE) — Cannot exit or opt out of the tax reduction mechanism. Bears the extraction in the form of lost revenue, reduced capacity for public services, and accumulated deficit burden. No alternatives available; the structural constraint (phased reduction) is unchangeable within the biographical horizon. Maximum experienced extraction.
constraint_indexing:constraint_classification(sotu_1982_reagan_three_year_tax_rate_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INFLATION-SENSITIVE WAGE WORKERS (ROPE) — Primary coordination function: increased disposable income from tax cuts provides relief from stagflation squeeze. Experiences the constraint as pure coordination mechanism — the problem is real (stagflation), the solution addresses it directly (more take-home pay), and the mechanism has low coercive overhead. Exit options exist (geographic mobility, sector switching) but the constraint itself is enabling rather than extractive from this position.
constraint_indexing:constraint_classification(sotu_1982_reagan_three_year_tax_rate_reduction, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-INCOME EARNERS AND BUSINESS INVESTMENT SECTOR (TANGLED ROPE) — Genuine coordination function: marginal rate cuts enable business investment and capital formation, solving a real constraint on growth. But asymmetric extraction embedded: the distribution of tax cuts is heavily skewed toward upper income brackets (53% of cuts to top 10%), creating wealth concentration alongside growth stimulus. Active enforcement required to implement and maintain the progressive rate structure collapse. Beneficiary position with arbitrage exit options.
constraint_indexing:constraint_classification(sotu_1982_reagan_three_year_tax_rate_reduction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SUPPLY-SIDE ECONOMIC REFORM COALITION (SCAFFOLD) — Organized political and economic actors (Reagan administration, business councils, free-market think tanks) designed this constraint with explicit sunset: three-year phased reduction with built-in endpoints. The coalition views the tax cut as temporary stimulus addressing a crisis (stagflation), with the sunset date representing transition to sustained growth that reduces deficit pressure. Theater ratio moderate: visible public benefit (take-home pay increases) alongside performative supply-side economics claims. Exit mechanism clear: if growth materializes by 1984, the revenue shortfall is 'paid for' by economic expansion.
constraint_indexing:constraint_classification(sotu_1982_reagan_three_year_tax_rate_reduction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE TAX SYSTEM AS INSTITUTIONAL ARTIFACT (PITON) — The constraint degrades the century-old progressive tax structure (rates rising from 14% baseline to 70% marginal) through mechanical rate reduction and bracket collapse. From the institutional perspective of the tax code itself, the mechanism is largely performative: the stated goal (growth stimulus) is independent of the actual implementation (blanket rate cuts). The institutional arrangement persists because the ideological commitment (supply-side economics) maintains theater around its necessity. By 1986-1990, as deficit accumulation becomes undeniable, the 'structural necessity' narrative loses force, but the rate cuts are not reversed — they persist through inertia and political capture.
constraint_indexing:constraint_classification(sotu_1982_reagan_three_year_tax_rate_reduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PUBLIC SERVICES DEPENDENT POPULATIONS (TANGLED ROPE) — Moderate power agents dependent on public infrastructure (Medicare recipients, public school students, infrastructure workers) experience both coordination and extraction. Coordination function: reduced tax burden theoretically enables broader consumer spending that supports employment. Extraction function: reduced federal revenue flows directly to constraints on public services, creating asymmetric burden distribution. Constrained exit options: these agents cannot arbitrage into alternative service provision systems; they must accept deficit-driven service degradation.
constraint_indexing:constraint_classification(sotu_1982_reagan_three_year_tax_rate_reduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective on macroeconomic dynamics, this constraint might appear as a law of demand-side stimulus: reducing purchasing power leakage (taxes) necessarily increases consumption and investment, which is a structural feature of how capitalist economies respond to demand shocks. However, the structural data contradicts the mountain classification — identifiable beneficiaries (high-income earners capturing disproportionate share), clear victims (future deficit burden), and active institutional enforcement (tax code administration) reveal this as a false summit. The 'natural law of stimulus' naturalizes what is actually a policy choice with distributional consequences.
constraint_indexing:constraint_classification(sotu_1982_reagan_three_year_tax_rate_reduction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1982_reagan_three_year_tax_rate_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1982_reagan_three_year_tax_rate_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1982_reagan_three_year_tax_rate_reduction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1982_reagan_three_year_tax_rate_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1982_reagan_three_year_tax_rate_reduction, TR),
    TR >= 0.70.

:- end_tests(sotu_1982_reagan_three_year_tax_rate_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint embeds both coordination (demand stimulus addressing stagflation) and asymmetric distribution (regressive tax cut benefiting high earners disproportionately). The baseline (year 0) is 0.28 because the immediate coordination benefit is real: tax relief for inflation-squeezed workers addresses a genuine crisis. By year 4, extractiveness rises to 0.52 because deficit accumulation reveals that the revenue loss is not 'paid for' by growth (the sunset clause's premise), creating permanent extraction. Suppression (0.35): Moderate. The mechanism is not coercive — the tax cuts are visible, legal, and politically chosen. But suppression operates indirectly: federal revenue reduction constrains alternative policy paths (public investment, entitlement expansion), and the deficit burden constrains future governments' choices. Theater ratio (0.58): Moderate-high and rising. Initial theater (0.42) reflects that the visible mechanism (cut rates, increase take-home pay) does solve the immediate stagflation problem. By year 4, theater rises to 0.58 as the supply-side claim (that cuts pay for themselves through growth) is empirically contradicted — deficit accumulation accelerates rather than reversing. The mechanism persists not because it delivers on its promises but because the ideological commitment (supply-side economics) maintains the performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The wage worker sees Rope — the constraint solves their immediate problem (stagflation relief). The high-income earner sees Rope or Tangled Rope — they gain from both coordination (growth stimulus) and distribution (rate cuts favor them). The federal revenue base sees Snare — pure extraction with no offsetting benefit and no exit mechanism. The organized coalition sees Scaffold — a temporary measure with built-in sunset, designed to be replaced by sustainable growth. The tax code institution sees Piton — the constraint degrades the structural arrangement through mechanical rate collapse maintained by theater. The analytical observer risks seeing Mountain — treating demand-side stimulus as a natural law of macroeconomics independent of distributional consequences. The perspectival gaps are not measurement errors; they reflect real differences in structural position, exit capacity, and benefit flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural relationships to extraction flow. High-income earners (d≈0.25) benefit from the rate structure collapse — their d value is low, producing negative effective extraction (they experience χ as benefit). Wage workers (d≈0.50) are split: they benefit from immediate demand stimulus (lower d component) but bear indirect cost through deficit burden and service constraints (higher d component). Federal revenue collectors (d≈1.0) bear maximum extraction — they lose resources with no offsetting benefit. Organized coalitions (d≈0.35) experience moderate extraction because while they benefit from policy success, they also bear political risk if the sunset clause triggers revenue crises. Public service dependents (d≈0.60) are tilted toward victim status: they bear deficit costs (service cuts) and constrained exit options (cannot arbitrage into alternative provision). The perspectival gap emerges because powerful beneficiaries experience low d and see Rope, while powerless victims experience high d and see Snare, despite facing the same constraint mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that 'Does tax reduction stimulate growth?' and 'Does tax reduction distribute equitably?' are distinct structural questions with potentially different answers. The constraint can be high-extractiveness (wealth concentration) while simultaneously being high-coordination (demand stimulus). The attempted resolution — 'growth pays for itself, so extraction is temporary' — is the sunset clause mechanism in the Scaffold perspective. This resolution succeeds only if growth is sufficient to reverse deficit accumulation by 1984-1985 AND the political mechanism enforces the sunset (allowing rates to revert). Historical data reveals the critical omega: the sunset clause was never enforced. The tax cuts of 1981 were not reversed in 1984-1985; instead, they were extended and further reduced in the 1986 Tax Reform Act. This failure means the Scaffold perspective is aspirational rather than structural — the three-year horizon was theater, not a real constraint. The constraint reclassifies toward Snare (extractive, no sunset mechanism actually available) from the perspective of those bearing the deficit cost. The mandatrophy resolves: this is a Tangled Rope for beneficiaries and a Snare for victims, with the 'temporary scaffold' claim being ideological maintenance of the arrangement rather than a structural feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_propensity_consumption_assumption,
    'What is the actual marginal propensity to consume by income bracket for the tax-cut recipients, and does it differ from the assumed 0.70-0.80 in supply-side models?',
    'Econometric analysis of post-1981 consumption patterns by income quintile; comparison of actual consumption increase to tax cut magnitude across brackets',
    'If MPC_high_income < 0.50: much of the tax cut flows to savings/investment, reducing immediate demand stimulus and rendering the mechanism closer to pure extraction (higher χ). If MPC_high_income > 0.70: the coordination function is stronger than assessed, reducing ε toward 0.35.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_propensity_consumption_assumption, empirical, 'Actual marginal propensity to consume by income bracket post-tax-cut').

omega_variable(
    deficit_accumulation_threshold,
    'At what cumulative deficit level does the ''growth pays for itself'' sunset clause mechanism fail, requiring either spending cuts or tax increases?',
    'Fiscal trajectory modeling; historical data on deficit accumulation 1981-1990; point at which ''deficit will be eliminated by growth'' claims become empirically falsified',
    'If threshold < 3% of GDP accumulated deficit: sunset clause triggers quickly, constraint transitions to constrained or trapped (reclassifies as Snare). If threshold > 8%: sunset clause is largely decorative, and the scaffold classification is aspirational rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deficit_accumulation_threshold, empirical, 'Deficit accumulation threshold for sunset clause failure').

omega_variable(
    distributional_inequality_amplification,
    'How much of the observed increase in wealth inequality 1981-1990 is directly attributable to the progressivity collapse in the tax cut mechanism, versus other factors (financialization, deindustrialization, globalization)?',
    'Counterfactual analysis: Gini coefficient trajectories with vs. without the tax cut; income share data by percentile; decomposition of inequality change sources',
    'If tax cut causes > 40% of observed inequality increase: the extraction component (χ) is higher than assessed (0.52 → 0.58+), reclassifying perspectives toward Snare. If < 20%: extraction is lower, and the tangled rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_inequality_amplification, empirical, 'Attribution of inequality increase to tax cut mechanism').

omega_variable(
    supply_side_investment_response,
    'Did the tax cut actually increase productive capital investment (factories, machinery, R&D), or did it flow primarily to financial assets, real estate, and consumption?',
    'Capital formation data by sector; comparison of business investment growth 1981-1984 to pre-tax-cut trend; financial asset inflation vs. productive investment',
    'If productive investment increased: the supply-side coordination claim is validated, ε remains ~0.52. If investment flowed to financial assets: the mechanism is closer to pure extraction without coordination benefit, ε → 0.60+.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_side_investment_response, empirical, 'Actual allocation of tax-cut funds to productive vs. financial investment').

omega_variable(
    stagflation_crisis_genuine_urgency,
    'Was the 1981-1982 stagflation crisis sufficiently severe to warrant the broad-based tax cuts, or were alternative mechanisms (monetary policy adjustment, targeted investment, structural reform) available?',
    'Counterfactual analysis: Federal Reserve policy alternatives 1979-1982; comparison to other stagflation episodes (1970s, 2022-2024); historical narrative assessment of policy options available',
    'If alternatives existed: the constraint''s coordination function is overstated, and it reclassifies toward Snare (lower coordination, higher pure extraction). If genuine crisis: coordination function validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stagflation_crisis_genuine_urgency, conceptual, 'Whether the macroeconomic crisis justified the broad-based tax reduction').

omega_variable(
    sunset_clause_enforcement_mechanism,
    'What political mechanism would enforce the sunset clause (allowing tax rates to revert in 1984-1985) if deficit accumulation continued to accelerate?',
    'Historical analysis: What actually happened in 1984-1985? Did Congress enforce the sunset, or did tax cuts persist? Documentation of political pressure and institutional decisions.',
    'If sunset was enforced: the Scaffold classification is validated — the constraint is genuinely temporary. If sunset failed: the Scaffold is aspirational, and the constraint reclassifies as Snare (no exit path actually available). This is the critical test of whether the three-year horizon represents a real structural feature or performative theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_clause_enforcement_mechanism, empirical, 'Whether the three-year sunset clause was actually enforced in law and practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1982_reagan_three_year_tax_rate_reduction, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taxred_tr_t0, sotu_1982_reagan_three_year_tax_rate_reduction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(taxred_tr_t1, sotu_1982_reagan_three_year_tax_rate_reduction, theater_ratio, 1, 0.48).
narrative_ontology:measurement(taxred_tr_t2, sotu_1982_reagan_three_year_tax_rate_reduction, theater_ratio, 2, 0.55).
narrative_ontology:measurement(taxred_tr_t3, sotu_1982_reagan_three_year_tax_rate_reduction, theater_ratio, 3, 0.6).
narrative_ontology:measurement(taxred_tr_t4, sotu_1982_reagan_three_year_tax_rate_reduction, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(taxred_be_t0, sotu_1982_reagan_three_year_tax_rate_reduction, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(taxred_be_t1, sotu_1982_reagan_three_year_tax_rate_reduction, base_extractiveness, 1, 0.35).
narrative_ontology:measurement(taxred_be_t2, sotu_1982_reagan_three_year_tax_rate_reduction, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(taxred_be_t3, sotu_1982_reagan_three_year_tax_rate_reduction, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(taxred_be_t4, sotu_1982_reagan_three_year_tax_rate_reduction, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1982_reagan_three_year_tax_rate_reduction, resource_allocation).
narrative_ontology:affects_constraint(sotu_1982_reagan_three_year_tax_rate_reduction, federal_deficit_accumulation_1981_1990).
narrative_ontology:affects_constraint(sotu_1982_reagan_three_year_tax_rate_reduction, progressive_taxation_institutional_erosion).
narrative_ontology:affects_constraint(sotu_1982_reagan_three_year_tax_rate_reduction, supply_side_economics_ideology_capture).

% DUAL FORMULATION NOTE:
% The three-year tax reduction is upstream of deficit accumulation (the tax cut flows lead to the revenue shortfall) and institutional erosion (rate structure collapse is the mechanism). It is influenced by ideological capture (the supply-side commitment that frames tax cuts as self-funding). These constraints are distinct because they operate at different ε levels: the tax cut mechanism itself is extractive (0.52) but has genuine coordination function; the resulting deficit accumulation is higher-extractiveness snare (no coordination); the institutional erosion is piton (degradation of preexisting structure). All three are linked by causal flow and should be analyzed as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1982_reagan_three_year_tax_rate_reduction, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
