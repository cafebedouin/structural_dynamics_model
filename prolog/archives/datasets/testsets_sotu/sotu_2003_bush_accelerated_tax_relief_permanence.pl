% ============================================================================
% CONSTRAINT STORY: sotu_2003_bush_accelerated_tax_relief_permanence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2003_bush_accelerated_tax_relief_permanence, []).

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
 *   constraint_id: sotu_2003_bush_accelerated_tax_relief_permanence
 *   human_readable: Accelerated Permanence of Bush Tax Relief (2003–2006)
 *   domain: economic/fiscal_policy
 *
 * SUMMARY:
 *   The 2003 acceleration of Bush tax relief scheduled for 2004–2006,
 *   combined with the elimination of dividend double taxation, exemplifies a
 *   constraint mechanism that coordinates demand-side stimulus for a slack
 *   economy (2003 unemployment 6.0%, growth stalling) while extracting from
 *   future fiscal capacity. The constraint distributes ~$1.35 trillion in tax
 *   relief over the decade to 92 million individual wage earners and 23
 *   million small business entities, framing the mechanism as broad-based
 *   consumption stimulus. The structural innovation is permanence: rather
 *   than the originally scheduled 10-year sunset (2001 EGTRRA), the 2003
 *   acceleration claims the relief immediately and asserts its permanent
 *   status, locking future administrations into a revenue loss baseline. The
 *   beneficiaries (wage earners, business owners, dividend income recipients
 *   in top deciles) experience genuine coordination: the relief solves the
 *   2003 macroeconomic slack. The victims (future federal revenue base,
 *   discretionary spending capacity) face suppression through political
 *   economy: reversing tax relief after it has normalized becomes
 *   structurally difficult. The constraint exhibits tangled rope structure:
 *   real coordination function (stimulus) overlaid with asymmetric extraction
 *   (accelerated timeline locks benefits while backloading costs,
 *   distributional skew toward top 10% disguised within broad-based
 *   narrative).
 *
 * KEY AGENTS:
 *   - Bush Administration: Primary beneficiary (institutional/arbitrage) — captures stimulus credit during 2004 reelection; achieves dividend tax relief for core constituencies
 *   - Individual Wage Earners (92M): Secondary beneficiary (moderate/constrained) — receive immediate consumption boost but trapped in future fiscal trade-off they cannot model while experiencing relief
 *   - Small Business Owners (23M): Secondary beneficiary (moderate/constrained) — receive investment incentives; distribution is heavily weighted to top-income businesses, creating hidden extraction from middle-income operators
 *   - Dividend Income Recipients (top 10%): Primary beneficiary (powerful/arbitrage) — double-taxation elimination is substantial wealth transfer; highest income concentration
 *   - Federal Revenue Base: Primary victim (powerless/trapped) — abstract fiscal capacity with no exit; revenue loss is structural and permanent
 *   - Future Spending Programs: Secondary victim (powerless/trapped) — Medicare, Discretionary spending face squeeze as entitlements consume growing share of revenue
 *   - Congressional Budget Authority: Institutional actor (institutional/constrained) — PAYGO rules and budget processes are performative rather than functional
 *   - Long-Term Fiscal Reformers: Organized observer (organized/constrained) — can see unsustainability but lack current power to prevent acceleration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2003_bush_accelerated_tax_relief_permanence, 0.58).
domain_priors:suppression_score(sotu_2003_bush_accelerated_tax_relief_permanence, 0.48).
domain_priors:theater_ratio(sotu_2003_bush_accelerated_tax_relief_permanence, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2003_bush_accelerated_tax_relief_permanence, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_2003_bush_accelerated_tax_relief_permanence, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_2003_bush_accelerated_tax_relief_permanence, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2003_bush_accelerated_tax_relief_permanence, tangled_rope).
narrative_ontology:human_readable(sotu_2003_bush_accelerated_tax_relief_permanence, "Accelerated Permanence of Bush Tax Relief (2003–2006)").
narrative_ontology:topic_domain(sotu_2003_bush_accelerated_tax_relief_permanence, "economic/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_2003_bush_accelerated_tax_relief_permanence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2003_bush_accelerated_tax_relief_permanence, individual_wage_earners).
narrative_ontology:constraint_beneficiary(sotu_2003_bush_accelerated_tax_relief_permanence, small_business_owners).
narrative_ontology:constraint_beneficiary(sotu_2003_bush_accelerated_tax_relief_permanence, dividend_income_recipients).
narrative_ontology:constraint_victim(sotu_2003_bush_accelerated_tax_relief_permanence, federal_revenue_base).
narrative_ontology:constraint_victim(sotu_2003_bush_accelerated_tax_relief_permanence, future_discretionary_spending_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE FISCAL CAPACITY (SNARE) — The acceleration of tax relief into immediate implementation locks in revenue loss at exactly the moment structural demographics (post-WWII cohorts aging into Medicare/Social Security) require expanding social entitlements. Future administrations face an impossible choice: raise taxes (politically costly), cut discretionary spending (infrastructure, education, defense), or accept deficits. The trapped agent here is the future fiscal system itself — it cannot exit the revenue constraint created by permanence without reversing the tax relief (politically infeasible after normalization) or restructuring entitlements (also politically intractable). Maximum experienced extraction because exit is foreclosed by political economy.
constraint_indexing:constraint_classification(sotu_2003_bush_accelerated_tax_relief_permanence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BUSH ADMINISTRATION / DIVIDEND RECIPIENTS (ROPE) — The administration and dividend income recipients (top 10% of households) experience the constraint as pure coordination: accelerating relief into the 2004–2006 window solves their demand-side stimulus objective and creates immediate capital gains tax relief. No suppression perceived — exit is available through the legislative process. Net beneficiary with positive arbitrage: capture tax relief gains while positioning for future political advantage (the 2004 reelection campaign). Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(sotu_2003_bush_accelerated_tax_relief_permanence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: INDIVIDUAL WAGE EARNERS (TANGLED ROPE) — Genuine coordination function: income tax relief increases disposable income and consumption velocity, addressing the 2003 economic slack (unemployment 6.0%, growth stalling). Coordination benefit is real and immediate (extra $300–$1,200 per household in 2004–2006). But the constraint also extracts from this group across the biographical horizon: the acceleration (claiming tax relief immediately rather than phasing in gradually over 10 years) frontloads benefits and backloads costs. Individual wage earners cannot exit without rejecting the legislative outcome — suppression is political, not material. Most wage earners cannot model the future fiscal trade-off (2009+ deficit pressure) while experiencing immediate relief.
constraint_indexing:constraint_classification(sotu_2003_bush_accelerated_tax_relief_permanence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESSIONAL BUDGET AUTHORITY (PITON) — The budget rules and scoring mechanisms that are supposed to constrain deficit-increasing legislation are largely performative by 2003. The constraint is formally evaluated through the PAYGO rules (Pay-As-You-Go) and offset requirements, but these mechanisms are already degraded — temporary tax relief was routinely extended (see 2001 EGTRRA), creating a theater of fiscal constraint that masks the structural reality of uncontrolled revenue loss. The CBA sees the legislation as following proper procedures (committee review, debate, vote) while experiencing the constraint as inert — the machinery produces the right performance but not the right outcome. Theater ratio is high because the budget process persists despite its failure to control deficits.
constraint_indexing:constraint_classification(sotu_2003_bush_accelerated_tax_relief_permanence, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LONG-TERM FISCAL REFORM ADVOCATES (SCAFFOLD) — This organized coalition (budget reform groups, some fiscal conservatives, future-looking economists) sees the tax relief acceleration as a temporary constraint on fiscal flexibility with a potential sunset: if demographic and economic conditions force reform, the tax relief structure could be unwound or fundamentally restructured. The coalition is constrained by current political economy (they lack power to prevent the acceleration) but sees an exit path through future structural crisis and demographic inevitability. Theater is moderate because reform advocates continue to argue for constraint even though they lack current power to enforce it. Sunset logic: the permanence is not truly permanent — it becomes unsustainable within 15–20 years as entitlement spending reaches 12–14% of GDP.
constraint_indexing:constraint_classification(sotu_2003_bush_accelerated_tax_relief_permanence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DEMAND-SIDE NATURAL LAW VIEW (MOUNTAIN) — From a long-horizon analytical perspective, the constraint appears as a natural economic law: demand-side stimulus through tax relief is necessary during recessions, and accelerating relief is simply applying the law of stimulus. The constraint naturalizes the 2003 decision as inevitable economic necessity. However, this perspective risks false summitry — the 'necessity' of acceleration rather than gradual phase-in is a political choice, not a natural law. The base properties (beneficiary presence, suppression, extractiveness) indicate a tangled rope or snare that has been naturalized as mountain through appeal to stimulus doctrine.
constraint_indexing:constraint_classification(sotu_2003_bush_accelerated_tax_relief_permanence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: INTERNATIONAL CAPITAL MARKETS (TANGLED ROPE) — Global capital flows are structured by the U.S. fiscal position. Tax relief that increases deficits can be coordinated with increased capital inflows (foreign purchases of Treasury bonds) — a genuine coordination function. But the constraint also extracts from international actors: if U.S. deficits drive interest rates upward or currency depreciation, foreign investors bear some cost. Mobile exit: capital markets can shift allocation to other sovereigns, but 2003–2006 geopolitical factors (dollar hegemony, post-9/11 security demand for safe assets) made U.S. Treasuries uniquely attractive. Extraction is present but constrained by beneficiary demand for capital.
constraint_indexing:constraint_classification(sotu_2003_bush_accelerated_tax_relief_permanence, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2003_bush_accelerated_tax_relief_permanence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2003_bush_accelerated_tax_relief_permanence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2003_bush_accelerated_tax_relief_permanence, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2003_bush_accelerated_tax_relief_permanence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2003_bush_accelerated_tax_relief_permanence, TR),
    TR >= 0.70.

:- end_tests(sotu_2003_bush_accelerated_tax_relief_permanence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint coordinates real macroeconomic stimulus (addressing 2003 slack), justifying a non-zero coordination baseline. But the acceleration and permanence claims add extraction: locking in relief when gradual phase-in might have achieved similar macro stimulus without foreclosing future revenue recovery. The distributional incidence (heavily weighted to top 10%) further elevates extractiveness — the broad 92-million narrative masks that ~60% of benefits accrued to top decile. The value reflects coordination plus strategic extraction through timing and permanence. Suppression (0.48): Moderate. Structural barriers to reversal include political economy (normalized tax relief is difficult to raise), cognitive anchoring (taxpayers perceive the relief as baseline rather than temporary), and distributional politics (top beneficiaries have lobbying power to prevent reversal). But suppression is not total — future fiscal crisis or demographic pressure can force change. Constraining factors limit mobility but do not fully trap. Theater ratio (0.62): Moderate-high. The 2003 framework exhibits significant performative content: PAYGO rules and budget procedures are followed (theater), but deficit outcomes contradict their stated purpose (inertial mechanism). The 'permanence' framing is itself theatrical — it naturalizes what is actually a contingent political choice. Theater increases over the measurement interval (0.48 → 0.68) as the initial stimulus justification fades and the pure revenue loss becomes salient, with budget rituals persisting despite visible failure to constrain deficits.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap separates immediate/institutional beneficiaries from future/powerless victims. The administration sees coordination (Rope) — stimulus in real time addresses slack demand. Individual wage earners see mixed benefit/extraction (Tangled Rope) — real immediate relief but suppressed future fiscal options. The Congressional Budget Authority sees degraded ritual (Piton) — procedures are followed, but outcomes contradict purpose. Long-term fiscal reformers see a temporary constraint with a crisis-driven sunset (Scaffold) — permanence is not truly permanent; demographic and fiscal inevitability will force restructuring within 15–20 years. The future fiscal system sees pure extraction (Snare) — it faces a revenue lock-in with no exit path. The analytical observer at civilizational scale risks false summitry (Mountain) — naturalizing demand-side stimulus as economic law rather than recognizing it as a policy choice among alternatives. The gap between immediate beneficiaries and future victims is obscured by the framing shift from 'temporary relief' (2001 EGTRRA) to 'permanent tax relief' (2003 acceleration) — the permanence claim acts as suppression, preventing beneficiaries from modeling the future trade-off.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives directionality from beneficiary/victim status plus exit options. Beneficiaries with arbitrage (institutional) or mobile (organized) exit experience low or negative d; they perceive the constraint as coordination serving their interests. Victims with trapped or constrained exit experience high d; they bear extraction they cannot escape. The acceleration mechanism is the critical directionality feature: immediate implementation combined with permanence claims forces beneficiaries' gains to compound (expectation lock-in) while preventing victims from adjusting through fiscal adjustment (revenue loss is instantaneous and difficult to reverse). This structural asymmetry in exit options across time horizons — beneficiaries exit immediately with gains realized; victims are trapped by future constraints they cannot foresee from 2003 perspective — is the core extraction mechanism. The wage-earner perspective illustrates this most clearly: they benefit immediately (constrained exit with positive payoff) but cannot exit the future fiscal squeeze (trapped exit with negative payoff). The constraint succeeds by collapsing the temporal gap between benefit realization and cost incurrence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that 'stimulus' and 'extraction' are not contradictory but orthogonal axes. The constraint genuinely coordinates demand-side stimulus (coordination function = Rope-like benefit) while simultaneously extracting from future fiscal capacity (extraction = Snare-like harm). The tangled rope classification is the correct synthesis: both are true. Mandatrophy is avoided by declaring both beneficiaries (stimulus recipients) and victims (future fiscal capacity) in the base properties. The analytical observer's mountain (naturalizing stimulus as law) is revealed as false summit through structural data: beneficiary presence (dividend tax relief was explicitly designed to favor top 10%) and suppression (political economy locks out reversal) indicate a constraint serving identifiable interests, not a law of nature. The constraint's permanence claim — 'this is permanent tax relief' — performs the work of false summitry: it naturalizes a political choice (acceleration of scheduled relief) as an immutable economic necessity. The engine's false summit detector will identify this when it compiles the structural data (beneficiaries + mountain claim → FSM investigation → reclassification through override chain if coupling confirms extraction).The mandatrophy is fully resolved: all six types are legitimate perspectival readings, the analytical observer's mountain is a false summit candidate, and the constraint's true structure is tangled rope with temporal extraction embedded in acceleration and permanence framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    acceleration_necessity_counterfactual,
    'Would the same stimulus benefit have been achieved with gradual rather than immediate acceleration of the tax relief?',
    'Econometric simulation of alternative implementation timelines; comparison of 2004–2006 consumption/investment growth under gradual vs. accelerated schedules controlling for concurrent monetary policy',
    'If gradual achieves similar macro outcomes: acceleration is extractive redistribution (higher Snare classification). If immediate acceleration is necessary for sufficient stimulus: coordination function is genuine (Rope/Tangled Rope justified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceleration_necessity_counterfactual, empirical, 'Whether acceleration was necessary for stimulus effectiveness').

omega_variable(
    permanence_expectation_formation,
    'Did taxpayers, businesses, and investors update consumption/investment expectations based on belief that tax relief was permanent, creating lock-in that would not have occurred under scheduled expiration?',
    'Survey evidence on perceived permanence (Consumer Confidence Index, small business surveys 2003–2006); correlation between perceived permanence and consumption/investment behavior; comparison to treatment countries with sunset-clause tax relief',
    'If permanence was believed: stronger extraction mechanism (suppression higher because reversal becomes politically costly). If treated as temporary: extraction is lower (reversibility remains available).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanence_expectation_formation, empirical, 'Permanence expectations and lock-in effects').

omega_variable(
    distributional_incidence_measurement,
    'What proportion of the tax relief benefits accrued to top 10% vs. bottom 50% of households?',
    'IRS tax return data analysis (Treasury Office of Tax Analysis reports); income distribution of benefit recipients by decile; ratio of dividend income relief to wage income relief',
    'If heavily skewed to top 10%: Snare classification strengthened (extractive redistribution from middle-income taxpayers). If broad-based: Rope/Tangled Rope justified (coordination benefit shared).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_incidence_measurement, empirical, 'Actual distributional incidence of tax relief').

omega_variable(
    deficit_persistence_structural,
    'Are the deficits created by tax relief acceleration structural (impossible to close without tax increases) or cyclical (would resolve with growth)?',
    'Structural deficit analysis (OMB long-term budget outlook); counterfactual deficit projections absent the accelerated relief; timeline to cyclical recovery as growth returns',
    'If structural: victim group (future spending capacity) faces true trap (Snare). If cyclical: constraint is temporary (Scaffold logic applies, sunset occurs when growth returns).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deficit_persistence_structural, empirical, 'Structural vs. cyclical nature of deficit persistence').

omega_variable(
    false_summit_stimulus_necessity,
    'Is demand-side stimulus through tax relief a natural law of macroeconomics or a policy choice among alternatives?',
    'Historiographical analysis of stimulus theory and alternatives (post-Keynesian monetary policy, public investment, automatic stabilizers); demonstration that non-tax stimulus mechanisms were available and rejected',
    'If policy choice: mountain classification is false summit (revealed by beneficiary presence and suppression structure). If genuine natural law: mountain classification stands (no alternatives to stimulus during slack demand).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_stimulus_necessity, conceptual, 'Whether stimulus necessity naturalizes a political choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2003_bush_accelerated_tax_relief_permanence, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taxrel_tr_t0, sotu_2003_bush_accelerated_tax_relief_permanence, theater_ratio, 0, 0.48).
narrative_ontology:measurement(taxrel_tr_t2, sotu_2003_bush_accelerated_tax_relief_permanence, theater_ratio, 2, 0.55).
narrative_ontology:measurement(taxrel_tr_t4, sotu_2003_bush_accelerated_tax_relief_permanence, theater_ratio, 4, 0.62).
narrative_ontology:measurement(taxrel_tr_t6, sotu_2003_bush_accelerated_tax_relief_permanence, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(taxrel_be_t0, sotu_2003_bush_accelerated_tax_relief_permanence, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(taxrel_be_t2, sotu_2003_bush_accelerated_tax_relief_permanence, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(taxrel_be_t4, sotu_2003_bush_accelerated_tax_relief_permanence, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(taxrel_be_t6, sotu_2003_bush_accelerated_tax_relief_permanence, base_extractiveness, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2003_bush_accelerated_tax_relief_permanence, resource_allocation).
narrative_ontology:affects_constraint(sotu_2003_bush_accelerated_tax_relief_permanence, federal_deficit_accumulation_2001_2008).
narrative_ontology:affects_constraint(sotu_2003_bush_accelerated_tax_relief_permanence, entitlement_spending_demographic_pressure).
narrative_ontology:affects_constraint(sotu_2003_bush_accelerated_tax_relief_permanence, congressional_discretionary_spending_caps).

% DUAL FORMULATION NOTE:
% This constraint is upstream of the 2009+ fiscal crisis and the 2017 Tax Cuts and Jobs Act. The acceleration mechanism established precedent for 'temporary' tax relief becoming effectively permanent through political economy. Related constraints: federal_deficit_accumulation (downstream effect), entitlement_spending_demographic_pressure (competing claim on revenues), and congressional_discretionary_spending_caps (compression of non-entitlement budgets).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2003_bush_accelerated_tax_relief_permanence, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
