% ============================================================================
% CONSTRAINT STORY: 1963_kennedy_corporate_tax_rate_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1963_kennedy_corporate_tax_rate_reduction, []).

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
 *   constraint_id: 1963_kennedy_corporate_tax_rate_reduction
 *   human_readable: Kennedy Corporate Tax Rate Reduction (1963)
 *   domain: economic/fiscal/labor
 *
 * SUMMARY:
 *   President Kennedy's 1963 corporate tax rate reduction from 52% to 47%
 *   represents a systematic shift from revenue-maximizing taxation toward
 *   supply-side capital incentive policy. The mechanism couples a
 *   5-percentage-point statutory rate cut with investment credits,
 *   accelerated depreciation, and capital gains preferences, reducing
 *   corporate tax liabilities by $2.5 billion initially plus $2 billion
 *   carryover from prior-year reforms. The policy is explicitly justified
 *   through a coordination narrative: lower after-tax returns on investment
 *   discourage capital formation, restricting productive capacity and
 *   constraining job creation. By aligning after-tax returns with expected
 *   profit rates, the tax reduction removes a 'coordination problem' where
 *   private investment incentives diverge from public growth objectives.
 *   However, the mechanism also operates as extraction: by concentrating tax
 *   reduction benefits on capital-intensive sectors, by deferring or
 *   eliminating taxation of capital gains, and by timing the benefits to
 *   capital holders before labor market effects materialize, the policy
 *   redistributes income from the tax base (and thus from public goods and
 *   labor-intensive sectors) toward equity and capital holders. The
 *   constraint exemplifies Tangled Rope classification: it genuinely solves
 *   the investment incentive coordination problem (firms do respond to
 *   after-tax return signals) while simultaneously extracting from labor
 *   share, public revenue capacity, and non-capital-intensive sectors.
 *
 * KEY AGENTS:
 *   - Capital-Intensive Firms: Primary beneficiary (institutional/arbitrage) — machinery, utilities, railroads, chemicals enjoy direct tax reduction and investment credits
 *   - Equity Holders & Investors: Primary beneficiary (institutional/arbitrage) — realize capital gains from dividend policy changes and asset appreciation enabled by tax reduction
 *   - Industrial Workers: Primary victim (powerless/trapped) — labor market participants with no exit; bear suppression from capital substitution and wage pressure during capital-intensive growth phase
 *   - Labor Union Leadership: Secondary actor (institutional/constrained) — institutionally powerful but locked into economy-wide bargaining; can coordinate on general growth but constrained by capital mobility
 *   - Non-Capital-Intensive Small Business: Secondary victim (moderate/constrained) — receive partial benefits from general growth stimulus but excluded from capital-intensive investment credit structure
 *   - Fiscal Administrators: Secondary actor (institutional/arbitrage) — maintain orthodoxy narrative (growth benefits all) while managing revenue loss and distributional consequences
 *   - Development Policymakers: Strategic actor (powerful/mobile) — view tax reduction as temporary supply-side stimulus with sunset logic; can adjust rates downward during slack demand, upward during full employment
 *   - Analytical Observer: Civilizational position (analytical/analytical) — 50+ years of data enable retrospective assessment of actual coordination vs extraction outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1963_kennedy_corporate_tax_rate_reduction, 0.52).
domain_priors:suppression_score(1963_kennedy_corporate_tax_rate_reduction, 0.48).
domain_priors:theater_ratio(1963_kennedy_corporate_tax_rate_reduction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1963_kennedy_corporate_tax_rate_reduction, extractiveness, 0.52).
narrative_ontology:constraint_metric(1963_kennedy_corporate_tax_rate_reduction, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(1963_kennedy_corporate_tax_rate_reduction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1963_kennedy_corporate_tax_rate_reduction, tangled_rope).
narrative_ontology:human_readable(1963_kennedy_corporate_tax_rate_reduction, "Kennedy Corporate Tax Rate Reduction (1963)").
narrative_ontology:topic_domain(1963_kennedy_corporate_tax_rate_reduction, "economic/fiscal/labor").

domain_priors:requires_active_enforcement(1963_kennedy_corporate_tax_rate_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1963_kennedy_corporate_tax_rate_reduction, capital_intensive_firms).
narrative_ontology:constraint_beneficiary(1963_kennedy_corporate_tax_rate_reduction, equity_holders).
narrative_ontology:constraint_beneficiary(1963_kennedy_corporate_tax_rate_reduction, institutional_investors).
narrative_ontology:constraint_victim(1963_kennedy_corporate_tax_rate_reduction, labor_force_purchasing_power).
narrative_ontology:constraint_victim(1963_kennedy_corporate_tax_rate_reduction, public_revenue_base).
narrative_ontology:constraint_victim(1963_kennedy_corporate_tax_rate_reduction, non_capital_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDUSTRIAL WORKER (SNARE) — Trapped in labor market with no exit. Receives promised job creation benefits only if capital investment actually materializes and corporations choose labor-intensive expansion. Tax reduction creates no direct wage improvement; worker bears risk of capital hoarding or automation while capital gains accrue to shareholders. Zero direct benefit, high suppression through unemployment threat.
constraint_indexing:constraint_classification(1963_kennedy_corporate_tax_rate_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (TANGLED ROPE) — Moderate power, constrained exit. Benefits modestly from growth stimulus and lower general tax rates, but investment credit targets capital-intensive firms (manufacturing, utilities, railroads). Constrained by limited access to investment incentives that large firms exploit. Experiences mixed coordination (general growth signal) and extraction (relative disadvantage in incentive structure).
constraint_indexing:constraint_classification(1963_kennedy_corporate_tax_rate_reduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL-INTENSIVE FIRM (ROPE) — Institutional power with arbitrage options (can relocate capacity, adjust investment timing, shift to favorable jurisdictions). Experiences constraint as pure coordination: tax reduction and investment credit solve the legitimate problem of coordinating investor expectations around after-tax returns. Low suppression; high agency. Direct extraction reduces their cost of capital by estimated 1.5-2.5 percentage points.
constraint_indexing:constraint_classification(1963_kennedy_corporate_tax_rate_reduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INDUSTRIAL UNION (TANGLED ROPE) — Institutional power but constrained exit (locked into economy-wide labor market). Nominally benefits from growth stimulus and job creation (coordination function); actually constrained by capital mobility and relative wage decline during growth period. Investment credit favors automation and capital deepening over labor absorption. Real wage suppression during expansion period contradicts promised coordination benefit.
constraint_indexing:constraint_classification(1963_kennedy_corporate_tax_rate_reduction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEVELOPMENT-FOCUSED POLICYMAKER (SCAFFOLD) — Powerful, mobile across policy regimes. Sees tax reduction as temporary growth stimulus with sunset logic: boost capital formation and capacity in the early 1960s, then normalize rates after growth solidifies and unemployment declines. Investment credit is explicitly time-bound (accelerated depreciation has declining effect as stock capital deepens). Theater ratio reflects supply-side narrative covering what is actually a capital-reallocation mechanism.
constraint_indexing:constraint_classification(1963_kennedy_corporate_tax_rate_reduction, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: FISCAL ORTHODOXY (PITON) — Traditional revenue principle 'taxation should be neutral, high and broad' persists despite contradictory supply-side rationale (selective rate reduction + targeted credits). Fiscal orthodoxy maintains ritual argument that 'general prosperity benefits everyone' while the mechanism actually concentrates capital gains. Theater ratio (0.58) reflects the gap between the coordination narrative (growth enables jobs) and the mechanism (capital-selective incentives extract from labor and revenue base).
constraint_indexing:constraint_classification(1963_kennedy_corporate_tax_rate_reduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — 50+ years of economic data enable cross-national comparison. Tax reduction does coordinate capital allocation (genuine coordination function) and does extract from labor share (genuine extraction). Empirical outcome: capital stock increased, labor share declined 1963-1980, wage-productivity divergence began. The constraint's classification as Tangled Rope is confirmed by structural fact — genuine coordination with asymmetric extraction.
constraint_indexing:constraint_classification(1963_kennedy_corporate_tax_rate_reduction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1963_kennedy_corporate_tax_rate_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1963_kennedy_corporate_tax_rate_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1963_kennedy_corporate_tax_rate_reduction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1963_kennedy_corporate_tax_rate_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1963_kennedy_corporate_tax_rate_reduction, TR),
    TR >= 0.70.

:- end_tests(1963_kennedy_corporate_tax_rate_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The tax reduction provides genuine coordination benefit (aligns investment incentives with growth objectives; removes distortion in capital formation decision-making) but simultaneously concentrates extractive benefit on capital holders through: (1) immediate capital gains from dividend policy liberalization, (2) relative shift in incidence from capital income to labor income, (3) timing gap between capital benefits (immediate) and labor benefits (delayed/uncertain). The measurement trajectory (0.38→0.58 over ten years) reflects increasing realization of extraction dynamics: initial period emphasizes coordination narrative and general growth; by end of decade, distributional consequences (labor share decline, wage-productivity divergence) become apparent. Theater ratio (0.52→0.61): Moderate, increasing. The coordination narrative ('tax reduction encourages investment which creates jobs') is empirically partially validated but masks the mechanism: tax reduction directly increases capital income regardless of whether productivity effects materialize. Theater increases over time as the employment coordination benefit becomes less clear while capital income benefits remain stable. Suppression (0.48): Moderate. Workers and non-capital-intensive sectors face real but not insurmountable constraints — labor market remains functional, wage bargaining is constrained but not eliminated by the policy. Suppression is lower than in extractive-only constraints because the growth stimulus has genuine coordination function that partially reallocates capital to productive use (not pure rent extraction).
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap separates beneficiaries (capital-intensive firms, equity holders) from victims (workers, non-capital sectors). Beneficiaries experience the constraint as Rope — a coordination mechanism that solves the investment incentive problem. Their perceived extraction is near-zero (the tax reduction is experienced as removal of distortion, not imposition of cost). Victims experience the constraint as Snare — they receive promised job creation benefits that depend on entrepreneurial discretion and capital allocation decisions outside their control, while bearing direct cost through relative income decline. The gap reflects genuine structural asymmetry: beneficiaries control the capital allocation mechanism and can realize benefits independent of labor market conditions; victims cannot. The institutional union occupies the perspectival middle (Tangled Rope) — they see both the growth stimulus coordination benefit and the capital substitution extraction mechanism. The fiscal administrator sees the constraint as a temporary policy lever (Scaffold perspective) with intentional sunset when unemployment normalizes; this view is contradicted by the measurement trajectory showing persistent extraction beyond the intended temporary period. The policymaker's intended sunset mechanism fails because the political economy of tax reduction (concentrated benefits to capital, diffuse costs to labor and public revenue) creates structural lock-in: reverting to 52% rates becomes politically impossible once firms have restructured around 47% assumptions.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality values (d) derive from the structural position of each agent relative to the tax reduction mechanism. Capital-intensive firms and equity holders are beneficiaries with arbitrage options (can invest elsewhere, relocate, adjust payout policies), yielding low d and negative experienced extractiveness. Industrial workers are trapped victims (no exit from labor market, constrained by capital mobility), yielding high d and high experienced extractiveness. Non-capital-intensive small business occupy intermediate positions (moderate power, constrained exit) with mixed directionality. The labor union has institutional power but constrained exit from the national economy, creating asymmetric directionality: they benefit from general growth (coordination function) but bear extraction through relative wage decline (capital substitution effect). The analytical observer's perspective confirms the tangled_rope classification through historical outcome measurement: both coordination function (capital formation did increase, capacity expanded) and extraction function (labor share declined, wage-productivity divergence emerged) are empirically validated.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION VIA NETWORK DECOMPOSITION: The Kennedy corporate tax reduction appears to mandate a choice between 'coordination mechanism' (Rope) and 'extraction mechanism' (Snare). The mandatrophy resolves by recognizing these as perspectives on the same constraint from different structural positions, unified by the Tangled Rope classification. The constraint genuinely serves coordination function (aligns investment incentives, solves capital formation coordination problem) AND genuinely serves extraction function (concentrates benefits on capital, depresses labor share, redistributes income from public revenue base). Both are real, not one illusory. The mistake is assuming a constraint must be either-or; Tangled Rope classification means it is both. The measurement trajectory (increasing extractiveness from 0.38 to 0.58 over 10 years) reveals that the extraction component becomes more visible as the coordination benefit saturates: initial years emphasize growth and job creation (coordination dominates perceived experience); later years show persistent capital income gains and persistent labor share decline (extraction component becomes salient). The constraint also exhibits the Goodhart degradation pattern: the theater ratio increases from 0.52 to 0.61, reflecting that the coordination narrative ('tax reduction creates jobs') becomes increasingly decoupled from reality as distributional outcomes reveal the extraction mechanism. By the late 1970s, the coordination narrative persists (firms do respond to tax incentives) but the employment creation claim becomes threadbare as wage-productivity divergence emerges. The falsifiability criterion is satisfied: if empirical data had shown net job creation equal to or exceeding coordination narrative predictions, and no labor share decline, the Snare classification would not apply; instead the data confirms both coordination function (capital formation increased) and extraction function (labor share declined), validating the Tangled Rope classification and requiring mandatory enforcement of both beneficiary and victim declarations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_absorption_mechanism,
    'Does tax-reduced capital investment increase absolute employment demand, or is employment displacement (automation) equivalent to or exceeds net new job creation?',
    'Sector-level employment data pre/post 1963-65; decomposition of employment change into new hires vs automation-driven displacement; comparison to counterfactual (employment trajectory under unchanged tax rates)',
    'If net positive employment: constraint includes genuine coordination function for labor (weak tangled_rope). If net zero or negative: extraction function dominates, capital hoarding risk is realized (strong snare for labor). If automation exceeds new hires: false coordination narrative (piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_absorption_mechanism, empirical, 'Whether tax reduction produces net job creation or net labor displacement').

omega_variable(
    investment_credit_targeting,
    'Does investment credit incentive structure favor labor-intensive sectors (food processing, textiles, agriculture) equally with capital-intensive sectors (machinery, utilities, chemicals)?',
    'Cross-sector analysis of investment credit utilization by industry; measurement of credit as percentage of corporate tax liability by sector; correlation between sector capital intensity and credit utilization rates',
    'If uniform: all sectors benefit equally (pure coordination). If capital-biased: labor-intensive sectors extract less value, creating relative disadvantage (tangled_rope confirmed). If highly capital-biased: labor sectors functionally excluded from incentive structure (snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(investment_credit_targeting, empirical, 'Whether investment credit incentives are distributed equally across labor-intensive and capital-intensive sectors').

omega_variable(
    capital_hoarding_propensity,
    'Given the tax reduction, do corporations expand productive capacity or accumulate cash, acquire existing assets, or adjust financial structure without increasing real economic activity?',
    'Decomposition of corporate cash flow post-tax reduction into: tangible capital formation (plant, equipment, R&D) vs financial restructuring (dividends, buybacks, balance-sheet optimization); sector-level comparison of capital formation rates to investment credit utilization',
    'If capital formation dominates: coordination function is real, job creation mechanism activated. If cash accumulation or financial activity dominates: tax reduction is pure extraction (capital gains without employment impact). If mixed: tangled_rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_hoarding_propensity, empirical, 'Whether corporations use tax savings for productive capital formation or financial manipulation').

omega_variable(
    labor_share_causation,
    'Does the post-1963 labor share decline (from ~66% to ~62% by 1980) derive from tax-driven capital substitution or from exogenous forces (globalization, skill-biased tech change, union decline)?',
    'Econometric decomposition of labor share change using factor substitution elasticity estimates; comparison of labor share trajectory across countries with different corporate tax policies; identification of sector-level substitution patterns correlated with investment credit utilization',
    'If tax-driven substitution is primary cause: extraction mechanism is confirmed at scale. If exogenous factors dominate: tax reduction may have minimal causal role in extraction. If mixed: tangled_rope with quantified extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_share_causation, empirical, 'Causal attribution of labor share decline to corporate tax policy vs other structural factors').

omega_variable(
    supply_side_multiplier_magnitude,
    'What is the actual output multiplier for corporate tax reduction — does each dollar of tax relief generate $1.50 of GDP growth (supply-side claim) or $0.50 (Keynesian estimate) or something else?',
    'Econometric estimation of tax reduction impact on subsequent GDP growth, capital formation, and employment; comparison to contemporary fiscal stimulus (government spending multiplier); adjustment for confounding factors (Vietnam War defense spending, monetary policy)',
    'High multiplier (>1.0): supply-side coordination claim validated. Low multiplier (<0.5): extraction mechanism confirmed (capital income transfer without productivity effect). Medium multiplier (0.5-1.0): tangled_rope confirmed with partial coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_side_multiplier_magnitude, empirical, 'Empirical magnitude of output multiplier for corporate tax reduction').

omega_variable(
    distributional_incidence_timing,
    'When are the distributional benefits of tax reduction realized — immediately to shareholders, or only after capital stock expansion translates to wage growth (delayed coordination)?',
    'Time-series analysis of share prices, dividend payouts, and capital gains vs wage growth and employment rates post-tax reduction; measurement of lag between tax policy implementation and labor market benefits',
    'If benefits to capital are immediate and wages are delayed >3 years: extractive dynamics are confirmed (temporal extraction). If benefits align temporally: coordination function more plausible. If wage benefits never materialize: extraction is permanent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_incidence_timing, empirical, 'Temporal distribution of tax reduction benefits between capital and labor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1963_kennedy_corporate_tax_rate_reduction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ktax_tr_t0, 1963_kennedy_corporate_tax_rate_reduction, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ktax_tr_t3, 1963_kennedy_corporate_tax_rate_reduction, theater_ratio, 3, 0.55).
narrative_ontology:measurement(ktax_tr_t6, 1963_kennedy_corporate_tax_rate_reduction, theater_ratio, 6, 0.58).
narrative_ontology:measurement(ktax_tr_t10, 1963_kennedy_corporate_tax_rate_reduction, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(ktax_be_t0, 1963_kennedy_corporate_tax_rate_reduction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ktax_be_t3, 1963_kennedy_corporate_tax_rate_reduction, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ktax_be_t6, 1963_kennedy_corporate_tax_rate_reduction, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(ktax_be_t10, 1963_kennedy_corporate_tax_rate_reduction, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1963_kennedy_corporate_tax_rate_reduction, resource_allocation).
narrative_ontology:affects_constraint(1963_kennedy_corporate_tax_rate_reduction, postwar_capital_labor_compromise).
narrative_ontology:affects_constraint(1963_kennedy_corporate_tax_rate_reduction, wage_productivity_divergence_onset).
narrative_ontology:affects_constraint(1963_kennedy_corporate_tax_rate_reduction, multinational_capital_mobility_expansion).

% DUAL FORMULATION NOTE:
% The tax rate reduction is the primary mechanism in this constraint; downstream constraints (wage-productivity divergence) are causal consequences of the tax policy's distributional effects. The constraint family models: (1) the tax policy as a resource allocation coordination mechanism, (2) the distributional outcome as capital income extraction, (3) the narrative decoupling as theater ratio increase, and (4) the political lock-in preventing policy reversal as inertial piton dynamics emerging from the tangled rope structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1963_kennedy_corporate_tax_rate_reduction, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
