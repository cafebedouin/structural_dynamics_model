% ============================================================================
% CONSTRAINT STORY: sotu_1999_clinton_usa_accounts_universal_savings
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1999_clinton_usa_accounts_universal_savings, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1999_clinton_usa_accounts_universal_savings
 *   human_readable: Universal Savings Accounts (USA) with Government Matching Contributions
 *   domain: economics/retirement_policy
 *
 * SUMMARY:
 *   President Clinton's 1999 State of the Union proposal to allocate 11% of
 *   the federal budget surplus to Universal Savings Accounts (USA) represents
 *   a structural choice about retirement security architecture: shifting from
 *   collective pooled-risk guarantee (Social Security/Medicare) toward
 *   individualized account ownership with government matching incentives. The
 *   constraint embeds a coordination mechanism (matching funds incentivize
 *   savings) within an extractive restructuring (risk transfer from pooled
 *   insurance to individual investment volatility). Low-income workers who
 *   lack discretionary savings capacity are mechanically excluded from
 *   matching benefits, while capital markets and financial services gain a
 *   new fee-revenue stream. Middle-income savers experience genuine benefit
 *   (matching is free money) alongside extraction (investment volatility
 *   transferred to individual). The Social Security system transitions from
 *   primary retirement security mechanism to supplemental guarantee. The
 *   constraint's theater ratio (0.35) reflects that matching-fund
 *   coordination is relatively transparent — the mechanism is straightforward
 *   and not highly performative — unlike regulatory theater or institutional
 *   ritual. However, theater increases over time as USA accounts proliferate
 *   and market volatility increasingly affects retirement security outcomes,
 *   creating higher performative content in communication about 'ownership'
 *   and 'wealth-building' versus actual income adequacy.
 *
 * KEY AGENTS:
 *   - Low-Income Workers: Primary victims (powerless/trapped) — lack savings capacity to participate; face exclusion from matching; bear concentrated risk from Social Security degradation
 *   - Capital Markets and Financial Services Industry: Primary beneficiaries (institutional/arbitrage) — receive channeled savings flows; gain fee revenue; subsidized by government matching funds
 *   - Middle-Income Savers: Secondary beneficiary/victim hybrid (moderate/constrained) — access matching funds but bear investment risk; opportunity cost of foregone Social Security expansion
 *   - Social Security Administration: Institutional actor (institutional/constrained) — primary function displaced; maintains administrative role with degraded political primacy; cannot exit system restructuring
 *   - Labor Coalition and Progressive Organizations: Organized opposition (organized/constrained) — constrained by political economy; see potential for reversal if political coalitions shift back toward income security emphasis
 *   - Analytical Economic Observer: Civilizational perspective (analytical/analytical) — risks naturalizing political choice as market efficiency; blind to beneficiary presence indicating false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1999_clinton_usa_accounts_universal_savings, 0.52).
domain_priors:suppression_score(sotu_1999_clinton_usa_accounts_universal_savings, 0.48).
domain_priors:theater_ratio(sotu_1999_clinton_usa_accounts_universal_savings, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1999_clinton_usa_accounts_universal_savings, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1999_clinton_usa_accounts_universal_savings, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1999_clinton_usa_accounts_universal_savings, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1999_clinton_usa_accounts_universal_savings, tangled_rope).
narrative_ontology:human_readable(sotu_1999_clinton_usa_accounts_universal_savings, "Universal Savings Accounts (USA) with Government Matching Contributions").
narrative_ontology:topic_domain(sotu_1999_clinton_usa_accounts_universal_savings, "economics/retirement_policy").

domain_priors:requires_active_enforcement(sotu_1999_clinton_usa_accounts_universal_savings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1999_clinton_usa_accounts_universal_savings, capital_markets).
narrative_ontology:constraint_beneficiary(sotu_1999_clinton_usa_accounts_universal_savings, higher_income_savers).
narrative_ontology:constraint_beneficiary(sotu_1999_clinton_usa_accounts_universal_savings, financial_services_industry).
narrative_ontology:constraint_victim(sotu_1999_clinton_usa_accounts_universal_savings, low_income_workers).
narrative_ontology:constraint_victim(sotu_1999_clinton_usa_accounts_universal_savings, systemic_retirement_risk_pooling).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME WORKER (SNARE) — Trapped in immediate economic necessity. Government matching is illusory: requires discretionary savings capacity that does not exist. The constraint transfers retirement security burden from collective guarantee (Social Security) to individual accumulation, extracting from those who cannot participate. Suppression is complete: no alternative between participation-incompatibility and retirement insecurity.
constraint_indexing:constraint_classification(sotu_1999_clinton_usa_accounts_universal_savings, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME SAVER (TANGLED ROPE) — Constrained but capable. Government matching provides genuine coordination benefit (incentivizes retirement savings) alongside extraction (risk shift from pooled system to individual volatility). Can participate but faces opportunity cost (matching funds could have expanded Social Security) and investment risk. Mixed experience: benefit from match and tax advantages; extraction from privatization of insurance function.
constraint_indexing:constraint_classification(sotu_1999_clinton_usa_accounts_universal_savings, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL MARKETS / FINANCIAL SERVICES (ROPE) — Net beneficiary with arbitrage capacity. USA accounts channel savings flows into capital markets and generate fee revenue for financial intermediaries. Government matching funds are pure subsidy to capital formation. Exit capacity is maximal: can redirect capital flows or reduce intermediation if regulatory environment changes. Sees constraint as pure coordination: mobilizing savings into productive investment.
constraint_indexing:constraint_classification(sotu_1999_clinton_usa_accounts_universal_savings, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR/PROGRESSIVE COALITION (SCAFFOLD) — Organized agents see USA as temporary risk-shift with sunset potential. If future political coalitions restore emphasis on Social Security expansion or create mandatory retirement accounts with higher contribution rates and pooled risk, USA becomes scaffolding for transition (short-term partial privatization before systemic re-collectivization). Constraint has bounded duration if political will shifts toward income security as primary frame rather than wealth-building.
constraint_indexing:constraint_classification(sotu_1999_clinton_usa_accounts_universal_savings, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SOCIAL SECURITY SYSTEM (PITON) — Once-functional collective insurance mechanism degrading into performative role. USA accounts fragment the retirement security function; Social Security remains but with reduced primacy and political protection. The institutional identity of Social Security (universal risk pooling, income replacement guarantee) atrophies as USA accounts become the aspirational path. Theater increases: SSA continues administering a shrinking share of retirement security while market-based accounts (theoretically) handle growth. Institutional inertia maintains SSA even as its primary function is displaced.
constraint_indexing:constraint_classification(sotu_1999_clinton_usa_accounts_universal_savings, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MARKET EFFICIENCY VIEW (MOUNTAIN) — From civilizational scale, individual accounts + competitive capital markets = natural law of efficient capital allocation and optimal retirement security. Market pricing of risk is invariant; savers must bear investment volatility; government subsidy through matching is coordination tax. This perspective risks naturalizing a contingent institutional choice (private wealth accumulation as retirement pathway) as inevitable economic truth. Engine false-summit detection will flag this: beneficiary presence (capital markets) contradicts natural-law framing.
constraint_indexing:constraint_classification(sotu_1999_clinton_usa_accounts_universal_savings, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1999_clinton_usa_accounts_universal_savings_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1999_clinton_usa_accounts_universal_savings, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1999_clinton_usa_accounts_universal_savings, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1999_clinton_usa_accounts_universal_savings, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1999_clinton_usa_accounts_universal_savings, TR),
    TR >= 0.70.

:- end_tests(sotu_1999_clinton_usa_accounts_universal_savings_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from low-income workers who cannot participate in matching (shifted retirement risk without compensation) and from the collective insurance function (Social Security) that is displaced. But it is not maximal extraction because middle-income and high-income savers do benefit from genuine matching subsidies and middle-income savers do experience expanded retirement options. The extraction is built into the distribution of matching benefits (regressive: higher earners can save more and access larger absolute matches) and into the risk shift (individual bears market volatility formerly pooled). Suppression (0.48): Moderate. Significant barriers to participation for low-income workers (lack of discretionary savings, financial literacy requirements, fee burden) but not total suppression — some low-income savers can and do participate. Organized opposition exists (labor, progressive groups) with political voice, indicating suppression is not absolute. Theater ratio (0.35): Low-moderate. The matching-fund coordination mechanism is relatively transparent and straightforward. Theater increases over time (0.25→0.35) as market volatility accumulates and USA accounts accumulate, because communication about 'wealth-building' and 'ownership' becomes increasingly theatrical cover for volatility exposure and investment risk.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Low-income workers perceive snare (extraction without recourse; matching is illusory). Capital markets perceive rope (coordination of capital flows with net benefit). Middle-income savers perceive tangled rope (genuine coordination benefit alongside risk extraction). Social Security perceives piton (degraded institutional role maintained through inertia). Labor coalition perceives scaffold (temporary privatization before potential systemic reversal). Analytical observer risks perceiving mountain (naturalized market efficiency). These are not measurement errors or perspective distortions — they are structural readings of the constraint's actual form. The gap reveals that USA is doing multiple incompatible things simultaneously: genuine coordination for some agents, genuine extraction for others, genuine institutional degradation for third parties. No single classification captures the full constraint; the presheaf of perspectives IS the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure is asymmetric: beneficiaries (capital markets, higher-income savers) have mobile or arbitrage exit options — they can redirect capital flows, reduce intermediation, or adjust portfolio allocation. Victims (low-income workers) have trapped or severely constrained exit options — they face choice between non-participation (retirement insecurity) or participation-with-risk (market volatility exposure). Social Security (institutional actor) has constrained exit — the system cannot exit the restructuring without legislative action. Middle-income savers have constrained exit with partial benefit — they can choose participation level but cannot fully avoid the risk-shift policy choice. The automatic derivation of d (directionality value) from beneficiary/victim + exit options produces: capital_markets d≈0.10 (beneficiary + arbitrage), low_income_workers d≈0.92 (victim + trapped), middle_income d≈0.58 (partial victim/beneficiary + constrained). No directionality overrides required — the structural data automatically produces the perspectival gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the coordination-vs-extraction mandatrophy through perspectival multiplicity. USA accounts ARE genuine coordination (matching funds do incentivize and enable retirement savings for many). USA accounts ARE genuine extraction (risk transfer from pooled guarantee to individual volatility disproportionately harms low-income workers who cannot absorb volatility). Both are simultaneously true. The mandatrophy is resolved not by choosing one classification but by recognizing that the constraint's function is different for different structural positions: for capital markets and higher-income savers, it is coordination; for low-income workers excluded from matching and exposed to volatility, it is extraction; for Social Security and institutional risk-pooling, it is degradation. The tangled_rope classification (claimed_type) captures this hybrid at the moderate/biographical level — genuine coordination function (matching) + asymmetric extraction (risk shift, regressive benefit distribution) + active enforcement (government matching bureaucracy). The perspectival spread (snare→rope→piton→mountain) reveals the full mandate structure without collapsing it to false unity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    matching_fund_participation_rate,
    'What percentage of low-income workers actually achieve sufficient discretionary savings to access government matching funds?',
    'Longitudinal tracking of USA account holders by income quintile; measurement of participation rate vs. eligibility rate; analysis of matching fund utilization by income decile',
    'If participation < 25% among target-income workers: matching is concentrated among higher earners who would have saved anyway (pure regressive redistribution). If participation > 60%: program genuinely expands retirement savings capacity for lower earners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(matching_fund_participation_rate, empirical, 'Actual participation rates in matching funds across income groups').

omega_variable(
    market_volatility_distributional_impact,
    'How do market downturns disproportionately affect lower-income savers who cannot absorb portfolio losses and must access accounts early?',
    'Comparative analysis of withdrawal rates during market downturns by income quintile; tracking of early withdrawal penalties and tax consequences for low-income account holders; intergenerational wealth impact of volatility exposure',
    'If volatility extracts disproportionately from low-income savers: snare classification strengthens. If risk is equally distributed or low-income savers have better downside protection: tangled-rope coordination function is genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_volatility_distributional_impact, empirical, 'Distribution of market volatility risk burden across income classes').

omega_variable(
    social_security_crowding_out,
    'Does USA account expansion politically crowd out Social Security enhancement and create path dependency toward privatization?',
    'Political economy analysis: tracking of legislative proposals 1999-2030; correlation between USA account popularity and political support for Social Security benefit expansion; analysis of replacement effect vs. additive effect',
    'If crowding-out is strong: USA represents extraction (risk shift without compensation) masked as coordination. If additive: genuine expansion of retirement security options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_security_crowding_out, empirical, 'Whether USA expansion crowds out Social Security enhancement').

omega_variable(
    financial_literacy_capacity_gap,
    'Can low-income savers realistically manage investment decisions for long-term retirement accounts given cognitive burden and information asymmetry with financial services industry?',
    'Behavioral analysis of investment choices among low-income USA account holders; tracking of fee burden and underperformance vs. institutional benchmarks; measurement of decision paralysis and default-option effects',
    'If cognitive/information gap is severe: suppression is structural (agents cannot exercise exit even with participation). If manageable with defaults and education: gap is reducible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(financial_literacy_capacity_gap, empirical, 'Financial literacy and cognitive capacity for retirement investment decisions').

omega_variable(
    natural_law_vs_contingent_institution,
    'Is the shift from collective (Social Security) to individual (USA accounts) retirement security a natural law of efficient capital allocation, or a contingent political choice reflecting 1990s ideology?',
    'Comparative policy analysis: retirement security models across developed economies; historical analysis of Social Security effectiveness and market-based alternatives; institutional economics analysis of political coalitions driving USA adoption',
    'If natural law: mountain classification holds. If contingent institution: false summit signature fires, reclassifying to tangled_rope. Shapes whether constraint appears inevitable or change-able.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_institution, conceptual, 'Whether risk shift to individuals is natural law or contingent political choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1999_clinton_usa_accounts_universal_savings, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1999_clinton_usa_accounts_universal_savings, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sotu_tr_t3, sotu_1999_clinton_usa_accounts_universal_savings, theater_ratio, 3, 0.3).
narrative_ontology:measurement(sotu_tr_t6, sotu_1999_clinton_usa_accounts_universal_savings, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1999_clinton_usa_accounts_universal_savings, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_be_t3, sotu_1999_clinton_usa_accounts_universal_savings, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(sotu_be_t6, sotu_1999_clinton_usa_accounts_universal_savings, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1999_clinton_usa_accounts_universal_savings, resource_allocation).
narrative_ontology:affects_constraint(sotu_1999_clinton_usa_accounts_universal_savings, social_security_solvency_timeline).
narrative_ontology:affects_constraint(sotu_1999_clinton_usa_accounts_universal_savings, retirement_security_risk_distribution).
narrative_ontology:affects_constraint(sotu_1999_clinton_usa_accounts_universal_savings, capital_market_concentration_fed_subsidy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
