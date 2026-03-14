% ============================================================================
% CONSTRAINT STORY: financial_leverage_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_financial_leverage_accumulation, []).

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
 *   constraint_id: financial_leverage_accumulation
 *   human_readable: Financial Leverage Accumulation and Debt Trap Dynamics
 *   domain: financial_economics/systemic_risk
 *
 * SUMMARY:
 *   Financial leverage accumulation operates as a structural trap where
 *   institutional actors benefit from debt expansion while powerless
 *   borrowers face compounding obligations with no exit. The constraint
 *   exhibits all classic snare characteristics: high extractiveness (0.68),
 *   high suppression (0.72) through legal obligation and credit system
 *   dependence, and performative regulation (theater ratio 0.58). The
 *   mechanism functions through asymmetric information (lenders understand
 *   systemic risk better than individual borrowers), moral hazard
 *   (institutions expect bailouts), and procyclical dynamics (leverage
 *   expands during booms, contracts catastrophically during busts).
 *   Measurement data shows extractiveness rising from 0.35 to 0.68 over a
 *   20-year period, with theater ratio increasing from 0.35 to 0.58,
 *   indicating both increasing raw extraction and increasing regulatory
 *   theater masking the extraction. The beneficiaries are leveraged financial
 *   institutions and wealth concentrators who capture gains during expansion
 *   periods; victims are over-leveraged borrowers and the systemic financial
 *   stability collective that bears costs of crises. From an institutional
 *   policy perspective, the constraint exhibits tangled rope characteristics
 *   (genuine coordination function mixed with extraction), but from the
 *   powerless borrower's perspective, it is pure snare.
 *
 * KEY AGENTS:
 *   - Over-Leveraged Borrowers: Primary victim (powerless/trapped) — individuals, households, SMEs locked into debt obligations with no exit pathway
 *   - Leveraged Financial Institutions: Primary beneficiary (institutional/arbitrage) — banks, shadow banks, hedge funds capturing spread and managing their own leverage through risk transfer mechanisms
 *   - Central Banks / Macroeconomic Policy Authorities: Organized institutional actor (organized/constrained) — coordinate financial system stability while managing leverage cycles and implicit bailout expectations
 *   - Systemic Financial Stability Collective: Secondary victim (moderate/constrained) — abstract system that absorbs shocks from leverage cycles; includes depositors, uninsured creditors, workers facing unemployment from crises
 *   - Financial Regulators: Institutional observer (institutional/arbitrage) — maintain performative oversight through stress tests and capital requirements; face capture pressures from regulated institutions
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing leverage accumulation as inherent to financial systems rather than recognizing it as a contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(financial_leverage_accumulation, 0.68).
domain_priors:suppression_score(financial_leverage_accumulation, 0.72).
domain_priors:theater_ratio(financial_leverage_accumulation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(financial_leverage_accumulation, extractiveness, 0.68).
narrative_ontology:constraint_metric(financial_leverage_accumulation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(financial_leverage_accumulation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(financial_leverage_accumulation, snare).
narrative_ontology:human_readable(financial_leverage_accumulation, "Financial Leverage Accumulation and Debt Trap Dynamics").
narrative_ontology:topic_domain(financial_leverage_accumulation, "financial_economics/systemic_risk").

domain_priors:requires_active_enforcement(financial_leverage_accumulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(financial_leverage_accumulation, leveraged_financial_institutions).
narrative_ontology:constraint_beneficiary(financial_leverage_accumulation, wealth_concentrators).
narrative_ontology:constraint_victim(financial_leverage_accumulation, over_leveraged_borrowers).
narrative_ontology:constraint_victim(financial_leverage_accumulation, systemic_financial_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OVER-LEVERAGED BORROWER (SNARE) — Structurally trapped by accumulated debt obligations. Cannot exit the leverage without defaulting (catastrophic cost). Faces compounding interest, margin calls, and forced asset sales. Maximum suppression through legal obligation and credit system dependence. No genuine coordination benefit — pure extraction mechanism.
constraint_indexing:constraint_classification(financial_leverage_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FINANCIAL INSTITUTION (ROPE) — Institutional actor with arbitrage options. Extracts spread between lending rate and cost of capital. Experiences the constraint as profitable coordination: allocating capital efficiently generates returns. Net beneficiary with easy exit — can stop lending, shift portfolios, or transfer risk. Effective extraction toward this agent.
constraint_indexing:constraint_classification(financial_leverage_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CENTRAL BANK / POLICY AUTHORITY (TANGLED ROPE) — Organized institutional actors whose structural position is mixed. Must coordinate financial system stability (genuine function) while also managing leverage cycles they partly enabled. Constrained by political economy and prior policy commitments. Benefits from financial system growth but bears costs of systemic crisis management. Active enforcement required (interest rates, reserve requirements, stress tests).
constraint_indexing:constraint_classification(financial_leverage_accumulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: SYSTEMIC FINANCIAL STABILITY (SNARE) — Abstract collective that cannot organize or exit. Bears full cost of systemic leverage accumulation: credit freezes, asset price collapses, contagion. Suppression is structural (embedded in financial architecture). Experiences high extraction through forced capital transfers during crises (bailouts, debt restructuring, inflation). No coordination benefit — pure negative externality.
constraint_indexing:constraint_classification(financial_leverage_accumulation, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY OVERSIGHT SYSTEM (PITON) — Stress tests, capital requirements, loan-to-value ratios, and macroprudential tools are largely performative. The underlying leverage accumulation mechanism persists because regulation creates the appearance of control without addressing the fundamental incentive structure (institutional mandates to maximize returns, implicit bailout expectations, pro-cyclical leverage dynamics). Theater ratio reflects that regulatory compliance has become decoupled from systemic stability outcomes.
constraint_indexing:constraint_classification(financial_leverage_accumulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Risks treating leverage accumulation as an immutable law of financial markets ('leverage is how capital works,' 'debt is inevitable'). This naturalizes what is actually a contingent institutional arrangement with specific incentive structures. The engine's false summit detection should flag this — the structural data contradicts mountain classification, revealing that 'inherent to finance' framing obscures policy choices and institutional design.
constraint_indexing:constraint_classification(financial_leverage_accumulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(financial_leverage_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(financial_leverage_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(financial_leverage_accumulation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(financial_leverage_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(financial_leverage_accumulation, TR),
    TR >= 0.70.

:- end_tests(financial_leverage_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Measurement progression from 0.35→0.52→0.68 indicates sustained accumulation of extraction mechanisms. The constraint extracts through multiple channels: (1) interest spread between lending rate and cost of capital captured by lenders, (2) credit rationing and collateral extraction from borrowers, (3) pro-cyclical leverage dynamics that generate asset price volatility benefiting leveraged traders, (4) systemic risk externalities borne by non-leveraged actors. The rising trajectory reflects regulatory environment post-2008 that maintained low rates and leverage accommodation rather than constraint. Suppression (0.72): Very high. Multiple barriers prevent exit: (A) Legal obligation — contracts bind borrowers to debt service with severe penalties for default; (B) Credit system dependence — refusal to service debt triggers cascade of penalties (credit report damage, asset seizure, bankruptcy); (C) Information asymmetry — borrowers often do not understand full implications of leverage commitments at origination; (D) Procyclical dynamics — leverage forced by competitive pressure (cannot remain unleveraged while competitors leverage; forces you to leverage to compete). Theater ratio (0.58): Moderate-high. Stress tests, capital requirements (Basel III), loan-to-value ratios, and macroprudential tools create appearance of control while underlying leverage accumulation persists. Regulatory theater increased over measurement period as complexity of financial instruments made real oversight impossible — regulators shifted to metric-based (theater) compliance rather than outcome-based stability metrics.
 *
 * PERSPECTIVAL GAP:
 *   The over-leveraged borrower sees pure extraction (snare): rising obligations, constrained exit, no coordination benefit. The financial institution sees coordination with profit (rope): allocating capital efficiently, managing risk, earning returns. The central bank sees mixed problem-solving (tangled rope): stabilizing financial system (genuine function) while managing moral hazard and extractive leverage dynamics. The regulatory system sees its own ritual (piton): stress tests and capital requirements persist through institutional inertia despite weak predictive power for actual crises. The systemic stability collective sees pure negative externality (snare): absorbs shocks with no agency or compensation. The analytical observer risks the false natural law (mountain): 'leverage is how markets work' naturalizes what is a specific institutional design with alternatives. This full perspectival range within a single constraint demonstrates DR's power for detecting institutional capture and naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values map directly to structural position. Powerless trapped borrowers: d≈0.95 (victims at maximum exit cost). Institutional lenders with arbitrage: d≈0.10 (beneficiaries with options). Central bank policy authority: d≈0.55 (mixed position — stabilizer but also enabler of leverage). Systemic stability abstract collective: d≈0.92 (powerless victim of aggregate outcomes). Each agent's experienced extractiveness (chi) is computed from base extractiveness × f(d) × scope modifier. Trapped borrowers experience maximum chi; beneficiary institutions experience minimal or negative chi (the constraint subsidizes them). The perspectival gap is fundamental: same structural mechanism appears as profit opportunity to one agent and trap to another.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE VS TANGLED ROPE DISTINCTION: The constraint classifies as pure snare from the borrower perspective because borrowers experience only extraction with no genuine coordination benefit — the lending relationships that exist are extractive, not mutually beneficial. However, from the policy perspective, there is a genuine coordination function (capital allocation, liquidity provision, macroeconomic stabilization) mixed with extraction. The resolution: this is a real snare at the individual/borrower level, but appears as tangled rope from the institutional/policy level. The mandatrophy resolves by recognizing that beneficiary/victim declarations are STRUCTURAL (who actually benefits, who bears costs) rather than perspectival. Borrowers bear costs; institutions benefit. This structural asymmetry is the hallmark of snare or tangled rope depending on whether coordination functions exist. The coordination functions DO exist (capital allocation, liquidity provision), but they are ASYMMETRICALLY DISTRIBUTED — borrowers do not capture the coordination benefits they enable. This is the defining feature of tangled rope at the policy level, but remains a snare at the individual level. The Snare classification is the correct one for the constraint_claim (analytical perspective) because it correctly identifies that the coordinate benefits are not shared equitably with cost-bearers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_bailout_expectations,
    'To what extent do financial institutions'' leverage accumulation choices depend on implicit expectations of government bailout during crises?',
    'Counterfactual analysis: Compare leverage ratios before/after major bailout events. Examine stated risk models against realized crisis outcomes. Analyze market pricing of systemic institutions (implicit insurance premium). Conduct surveys of institutional risk officers.',
    'If bailout expectations are HIGH: leveraged institutions are rationally capturing taxpayer subsidy; the snare classification is correct (institutions extract from systemic stability via moral hazard). If bailout expectations are LOW: leverage accumulation reflects genuine capital efficiency choices; extractiveness should be downgraded to 0.45-0.55 (Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_bailout_expectations, empirical, 'Extent of implicit bailout expectations in leverage decisions').

omega_variable(
    leverage_accumulation_mechanism,
    'Is leverage accumulation driven primarily by (A) rational profit-maximization under existing incentives, (B) agency problems and principal-agent misalignment, or (C) deliberate extraction by sophisticated actors with information asymmetry advantage?',
    'Cross-institutional comparison of leverage ratios and compensation structure alignment. Analysis of information asymmetry in credit underwriting (lenders'' access to borrower data vs borrower self-knowledge). Examination of loan origination vs loan retention (moral hazard indicator). Historical comparison across regulatory regimes.',
    'If (A): institutional rationality operates within the constraint structure; constraint remains snare but mechanism is coordinated poor equilibrium. If (B): agency costs suggest leverage accumulation is not optimally controlled extraction; snare classification persists. If (C): deliberate extraction with information advantage; snare classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leverage_accumulation_mechanism, empirical, 'Primary driver of leverage accumulation behavior').

omega_variable(
    crisis_frequency_threshold,
    'What frequency of leveraged crises (per decade) constitutes systemic instability vs normal market volatility?',
    'Historical time-series analysis of credit cycles, asset bubble frequency, and major financial crises. Comparison of crisis frequency across different financial system architectures (regulated vs deregulated periods, different countries). Correlation with leverage accumulation metrics.',
    'If crises >1 per decade are systemic: leverage accumulation is a snare. If crises <0.5 per decade: leverage accumulation may be rope (coordinated risk-taking). Current data suggests >1 per decade in globalized system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_frequency_threshold, empirical, 'Crisis frequency threshold indicating systemic instability').

omega_variable(
    regulatory_capture_depth,
    'To what degree are financial regulators captured by the institutions they supervise, and does this capture systematically bias leverage thresholds and enforcement?',
    'Study of revolving door between regulatory agencies and financial institutions. Analysis of regulatory enforcement patterns (probability of penalties vs violation severity). Comparison of stress test scenarios against actual crisis outcomes. Examination of regulatory impact on market structure and competitive dynamics.',
    'If capture is deep: regulatory theater is performative (piton classification accurate), and regulatory action will not reduce leverage extraction. If capture is shallow: regulators have agency and can reduce extractiveness to tangled_rope or rope levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Degree of financial regulator capture by regulated institutions').

omega_variable(
    alternative_financial_architecture,
    'Are there empirical examples of financial systems with substantially lower leverage accumulation that maintain comparable economic efficiency?',
    'Historical and cross-national comparison: narrow banking regimes, Islamic finance models, cooperative credit systems, state-directed finance. Measurement of real economic outcomes (capital allocation efficiency, productive investment, innovation) vs leverage levels. Analysis of stability metrics.',
    'If alternatives exist with lower extractiveness: snare classification is confirmed as contingent on current architecture. Suggests extractiveness could be reduced via redesign. If no alternatives: leverage accumulation may reflect genuine coordination necessity (downgrade to tangled_rope or rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_financial_architecture, empirical, 'Existence and performance of alternative financial architectures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(financial_leverage_accumulation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flev_tr_t0, financial_leverage_accumulation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(flev_tr_t10, financial_leverage_accumulation, theater_ratio, 10, 0.48).
narrative_ontology:measurement(flev_tr_t20, financial_leverage_accumulation, theater_ratio, 20, 0.58).
narrative_ontology:measurement(flev_tr_t5, financial_leverage_accumulation, theater_ratio, 5, 0.4).

% Extraction over time
narrative_ontology:measurement(flev_be_t0, financial_leverage_accumulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(flev_be_t10, financial_leverage_accumulation, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(flev_be_t20, financial_leverage_accumulation, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(flev_be_t5, financial_leverage_accumulation, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(financial_leverage_accumulation, resource_allocation).
narrative_ontology:boltzmann_floor_override(financial_leverage_accumulation, 0.18).
narrative_ontology:affects_constraint(financial_leverage_accumulation, systemic_financial_crisis_propagation).
narrative_ontology:affects_constraint(financial_leverage_accumulation, wealth_concentration_dynamics).
narrative_ontology:affects_constraint(financial_leverage_accumulation, monetary_policy_transmission_lag).

% DUAL FORMULATION NOTE:
% Financial leverage accumulation decomposes into separate constraint stories: (1) individual_debt_trap (ε=0.72, Snare) — personal borrower perspective, (2) institutional_leverage_spread (ε=0.45, Tangled Rope) — financial institution coordination with extraction, (3) macroeconomic_procyclicality (ε=0.55, Tangled Rope) — policy-level coordination of stability with leverage cycle dynamics. This story focuses on the integrated system view; downstream constraints address specific mechanisms and institutional manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(financial_leverage_accumulation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
