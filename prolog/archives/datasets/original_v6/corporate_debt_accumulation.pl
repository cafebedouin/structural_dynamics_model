% ============================================================================
% CONSTRAINT STORY: corporate_debt_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_corporate_debt_accumulation, []).

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
 *   constraint_id: corporate_debt_accumulation
 *   human_readable: Corporate Debt Accumulation and Shareholder-Creditor Extraction
 *   domain: financial_economics/corporate_governance
 *
 * SUMMARY:
 *   Corporate debt accumulation represents a structural constraint that
 *   operates at multiple levels: individual firm capital structure, sectoral
 *   financial risk, and macroeconomic fragility. The constraint exhibits the
 *   core mandatrophy problem in financial economics: debt is simultaneously a
 *   coordination mechanism (enabling capital allocation and temporal
 *   smoothing) and an extraction mechanism (concentrating returns on equity
 *   while distributing risk to creditors, employees, and taxpayers). The
 *   indexical framework resolves the ambiguity by showing that different
 *   agents genuinely experience different constraint types: equity holders
 *   and managers see coordination (Rope); creditors and taxpayers see pure
 *   extraction (Snare); employees see mixed coordination-extraction (Tangled
 *   Rope); and the analytical macroeconomic view sees both functions
 *   operating simultaneously (Tangled Rope). The constraint's evolution is
 *   measured through increasing extractiveness (0.35 → 0.58 over 20 periods)
 *   and moderately increasing theater ratio (0.32 → 0.45), suggesting that
 *   the coordination function is eroding and the extraction mechanism is
 *   becoming more prominent over time.
 *
 * KEY AGENTS:
 *   - Equity Holders / Management: Primary beneficiaries (institutional/arbitrage) — capture leverage-driven returns, tax subsidies, and financial engineering gains; have costless exit via share sales or role changes
 *   - Debt Holders / Creditors: Primary victims (powerless/trapped) — face covenant erosion, seniority dilution, refinancing risk, and adverse selection in debt markets; locked into positions once capital deployed
 *   - Taxpayers / Implicit Guarantors: Secondary victims (powerless/trapped) — provide implicit insurance for systemic failures; absorb losses in crisis but receive no compensation; cannot exit membership in guarantee set
 *   - Incumbent Employees: Mixed victims (moderate/constrained) — benefit from firm stability but experience wage suppression, reduced capex, and layoff risk; have labor market exit but at substantial cost
 *   - Financial Intermediaries: Secondary beneficiaries (institutional/arbitrage) — profit from debt origination, underwriting, and refinancing; have full market exit via hedging and mark-to-market
 *   - Analytical Observer: System-level view (analytical/analytical) — recognizes both coordination (liquidity provision, capital allocation) and extraction (fragility, moral hazard, procyclicality) functions operating simultaneously
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(corporate_debt_accumulation, 0.58).
domain_priors:suppression_score(corporate_debt_accumulation, 0.68).
domain_priors:theater_ratio(corporate_debt_accumulation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(corporate_debt_accumulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(corporate_debt_accumulation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(corporate_debt_accumulation, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(corporate_debt_accumulation, tangled_rope).
narrative_ontology:human_readable(corporate_debt_accumulation, "Corporate Debt Accumulation and Shareholder-Creditor Extraction").
narrative_ontology:topic_domain(corporate_debt_accumulation, "financial_economics/corporate_governance").

domain_priors:requires_active_enforcement(corporate_debt_accumulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(corporate_debt_accumulation, equity_holders).
narrative_ontology:constraint_beneficiary(corporate_debt_accumulation, corporate_management).
narrative_ontology:constraint_beneficiary(corporate_debt_accumulation, financial_intermediaries).
narrative_ontology:constraint_victim(corporate_debt_accumulation, creditors).
narrative_ontology:constraint_victim(corporate_debt_accumulation, employees).
narrative_ontology:constraint_victim(corporate_debt_accumulation, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED CREDITOR (SNARE) — Bondholders and lenders face seniority dilution, covenant erosion, and refinancing risk with minimal recourse. Once capital is deployed, exit is costly and illiquid. Asymmetric information favors management; debt terms are set ex-ante and cannot adjust as leverage increases. The constraint appears as pure extraction: management captures surplus through leverage arbitrage while creditors absorb tail risk.
constraint_indexing:constraint_classification(corporate_debt_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IMPLICIT GUARANTOR / TAXPAYERS (SNARE) — Creditors are often protected by implicit government guarantees ('too big to fail'). When leverage becomes unsustainable, taxpayers absorb losses through bailouts or defaults that cascade through the financial system. Taxpayers cannot exit this constraint — they are trapped by their status as citizens. Their extraction is structurally involuntary and uncompensated.
constraint_indexing:constraint_classification(corporate_debt_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT EMPLOYEE (TANGLED ROPE) — Workers benefit from firm coordination (stable employment, wage income, skill development) but also bear wage suppression, reduced investment in productivity growth, and layoff risk as debt service claims resources. They have some exit optionality (labor market mobility) but face real costs: relocation, retraining, opportunity loss. The constraint is mixed: genuine coordination (the firm exists) combined with extraction (leverage-driven austerity).
constraint_indexing:constraint_classification(corporate_debt_accumulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EQUITY HOLDER / MANAGEMENT (ROPE) — Shareholders and managers benefit from leverage: increased returns on equity through financial engineering, tax deductibility of interest, ability to sustain distributions while underlying business deteriorates. Exit is costless (sell shares or change roles). The constraint appears as pure coordination from this perspective: debt enables capital allocation and risk management. Extraction runs toward this agent, not away.
constraint_indexing:constraint_classification(corporate_debt_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FINANCIAL INTERMEDIARY (ROPE) — Banks, bond underwriters, and private equity firms profit from originating debt, arranging refinancing, and capturing spreads. They have exit via mark-to-market and portfolio hedging. The constraint is coordination: debt markets enable capital reallocation and liquidity provision. From their view, accumulated leverage is functional complexity, not extraction.
constraint_indexing:constraint_classification(corporate_debt_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — At the system level, debt accumulation has genuine coordination functions: it smooths consumption over time, finances productive capital, and allocates risk to agents with highest tolerance. But accumulated leverage also creates fragility, financial contagion risk, and endogenous procyclicality. The constraint is mixed because both functions coexist: coordination infrastructure + extraction mechanism. Whether the system is fragile or resilient depends on debt composition, rollover rates, and macro conditions.
constraint_indexing:constraint_classification(corporate_debt_accumulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(corporate_debt_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(corporate_debt_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(corporate_debt_accumulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(corporate_debt_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(corporate_debt_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint exhibits significant extraction mechanisms — creditors absorb tail risk and face seniority dilution; taxpayers provide implicit insurance with no compensation; employees experience wage suppression and underinvestment. However, extractiveness is not extreme (0.70+) because some leverage is genuinely productive (financing capital investment) and market discipline does constrain excessive leverage in normal times. The trajectory (0.35 → 0.58) reflects increasing leverage ratios and tightening credit conditions that make refinancing risk endogenous. Suppression (0.68): High. Multiple barriers prevent agents from exiting: creditors face illiquidity and seniority erosion; employees face labor market friction; taxpayers face collective action problems and implicit guarantees that prevent contractual exit; governments face systemic consequences of allowing failures. The suppression has increased as debt ratios have risen and refinancing windows have narrowed. Theater ratio (0.45): Moderate. The constraint has significant performative elements (covenant engineering, ratings inflation, 'covenant lite' bonds, management guidance) but is not primarily theatrical — underlying financial mathematics and default risk are real. The theater has increased as sophisticated debt instruments have proliferated, offsetting the tightening credit conditions.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal between institutional beneficiaries (Rope) and powerless victims (Snare). This gap is diagnostic: it shows that the constraint has asymmetric effects that cannot be reconciled by a single classification. The Tangled Rope claim at the analytical level reflects that both genuine coordination and genuine extraction coexist — the gap is real, not an artifact of measurement. The moderate agent (Employee) perspective shows mixed classification (Tangled Rope), which is structurally appropriate: they are neither pure extractors nor pure victims, but participants in both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position in the extraction flow. Equity holders occupy the beneficiary position: low base extraction ε, beneficiary status, arbitrage exit → low d value (0.15 range) → negative or low f(d) → negative or low effective extraction χ. They experience the constraint as favorable. Creditors occupy the victim position: high base extraction ε, victim status, trapped exit → high d value (0.95 range) → high f(d) ≈ 1.42 → high effective extraction χ. They experience extraction acutely. Employees occupy the mixed position: moderate extraction, mixed beneficiary/victim (benefit from firm but absorb wage suppression), constrained exit → moderate d (0.65 range) → moderate f(d) ≈ 1.0 → proportional χ. Taxpayers occupy the pure victim position with no contractual relationship: trapped exit, implicit liability, no benefit → high d → high χ. The directionality computation automatically captures that the same constraint produces opposite experienced extraction for different agents.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY: The core ambiguity is whether accumulated corporate debt is primarily a coordination mechanism (enabling capital allocation and risk distribution — Rope logic) or an extraction mechanism (concentrating returns on equity while distributing risk to creditors and taxpayers — Snare logic). The resolution emerges from examining the temporal trajectory and sectoral variation. When leverage finances productive capital investment (capex, R&D, working capital), the constraint is closer to Tangled Rope with coordination function dominant. When leverage finances share buybacks, dividend recaps, and financial engineering without corresponding productivity gains, the constraint is closer to Snare with extraction function dominant. The increasing extractiveness trajectory (0.35 → 0.58) suggests a shift toward extraction dominance — as leverage ratios increase and refinancing windows narrow, the productivity-to-extraction ratio is declining. The moderate theater ratio (0.45) indicates that some performance metrics (covenants, credit ratings) are becoming theater, but the underlying financial mathematics remains real. The mandatrophy resolves at the firm and sector level: productive sectors with genuine capex will classify closer to Tangled Rope; financial engineering and LBO-heavy sectors will classify closer to Snare. A system-level story requires showing both — some corporate debt is coordination (productive), some is extraction (financial engineering). This story reflects the constraint at the aggregated level where both coexist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_vs_extraction_distinction,
    'Does debt finance productive capital investment (coordination) or does it primarily enable distribution extraction and financial engineering (extraction)?',
    'Decompose debt by use: capex financing vs share buybacks vs dividend distributions vs refinancing prior debt. Calculate IRR on debt-financed capex vs distribution flows.',
    'If productive: more leverage is functional and constraint is closer to Rope. If extractive: leverage primarily extracts from creditors and employees, constraint is closer to Snare. Ambiguity is the core mandatrophy issue.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(productivity_vs_extraction_distinction, empirical, 'Whether accumulated debt finances productive investment or enables distribution extraction').

omega_variable(
    rollover_and_fragility_threshold,
    'At what leverage threshold does rollover risk become endogenous? When does debt composition matter more than debt level for systemic stability?',
    'Historical analysis of corporate default cascades; model debt maturity structure and refinancing risk; identify leverage levels at which credit spreads become nonlinear.',
    'If threshold is high: corporations have significant borrowing capacity before fragility emerges; extraction is suppressed by market discipline. If threshold is low: current leverage levels are already creating hidden fragility; extraction is masked by low current rates but will materialize in downturns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rollover_and_fragility_threshold, empirical, 'Leverage threshold at which rollover risk becomes endogenous').

omega_variable(
    tax_subsidy_magnitude,
    'Does the tax deductibility of interest constitute a subsidy to leverage, and if so, at what magnitude? Is this subsidy rational tax policy or does it distort capital structure?',
    'Calculate effective marginal tax rate on debt vs equity financing; model comparative capital structure if debt were not tax-advantaged; assess whether tax treatment reflects risk-adjusted policy or path-dependent artifact.',
    'If subsidies are significant: debt accumulation is partially policy-driven extraction from taxpayers; constraint is partially manufactured. If subsidies are incidental: debt is market-driven; constraint reflects real incentive misalignment rather than fiscal distortion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tax_subsidy_magnitude, preference, 'Whether tax deductibility of interest constitutes distortionary subsidy to leverage').

omega_variable(
    implicit_guarantee_scope,
    'For which corporations does the ''too big to fail'' implicit guarantee apply? Is it a formal covenant or an emergent market expectation?',
    'Compare credit spreads and borrowing costs for large vs small corporations; model expected bailout probability using market pricing and historical intervention patterns; assess regulatory statements on systemicity thresholds.',
    'If guarantee is broad: leverage is subsidized and moral hazard is severe; extraction from taxpayers is structural and large. If guarantee is narrow: market discipline constrains leverage; extraction is limited to creditors and employees.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_guarantee_scope, empirical, 'Scope and magnitude of implicit government guarantee for corporate debt').

omega_variable(
    covenant_degradation_mechanism,
    'Do existing debt covenants actually constrain additional leverage, or have they been systematized away (cov-lite bonds, cash sweep removal, leverage ratchets)?',
    'Empirical analysis of covenant presence and tightness over time; study cases where leveraged buyouts and dividend recaps violated covenant spirit but not letter; assess whether creditor sophistication has been offset by covenant erosion.',
    'If covenants are effective: creditor protection is structural; extraction is limited. If covenants have been systematized away: creditor protections are theatrical; extraction is high and ongoing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(covenant_degradation_mechanism, empirical, 'Whether debt covenants actually constrain additional leverage').

omega_variable(
    employee_voice_and_wage_suppression,
    'Do corporations deliberately suppress wages or defer investment to service debt, or is austerity an inevitable macro constraint?',
    'Comparative analysis of wage and capex trajectories in firms with different leverage levels, holding sector and size constant; interview evidence on management debt-service priorities; cross-country comparison of leverage norms and wage dynamics.',
    'If deliberate: employees are intentional victims; extraction is structural. If macro constraint: employees suffer incidentally; extraction is unintentional side effect of financial sustainability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employee_voice_and_wage_suppression, empirical, 'Whether corporations deliberately suppress wages to service debt').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(corporate_debt_accumulation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corpdebt_tr_t0, corporate_debt_accumulation, theater_ratio, 0, 0.32).
narrative_ontology:measurement(corpdebt_tr_t10, corporate_debt_accumulation, theater_ratio, 10, 0.4).
narrative_ontology:measurement(corpdebt_tr_t20, corporate_debt_accumulation, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(corpdebt_be_t0, corporate_debt_accumulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(corpdebt_be_t10, corporate_debt_accumulation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(corpdebt_be_t20, corporate_debt_accumulation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(corporate_debt_accumulation, resource_allocation).
narrative_ontology:affects_constraint(corporate_debt_accumulation, financial_fragility_and_procyclicality).
narrative_ontology:affects_constraint(corporate_debt_accumulation, private_equity_leverage_extraction).
narrative_ontology:affects_constraint(corporate_debt_accumulation, wage_suppression_through_financial_engineering).

% DUAL FORMULATION NOTE:
% Corporate debt accumulation at the firm level (capital structure optimization) should be decomposed from debt accumulation at the sectoral level (competitive leverage race) and macroeconomic level (systemic fragility). Each has different ε values and different victim/beneficiary structures. This story represents the multi-level aggregate constraint; downstream stories address specific sectoral manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(corporate_debt_accumulation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
