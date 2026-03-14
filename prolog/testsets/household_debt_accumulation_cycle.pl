% ============================================================================
% CONSTRAINT STORY: household_debt_accumulation_cycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_household_debt_accumulation_cycle, []).

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
 *   constraint_id: household_debt_accumulation_cycle
 *   human_readable: Household Debt Accumulation Cycle
 *   domain: economic/financial
 *
 * SUMMARY:
 *   The household debt accumulation cycle represents a structural constraint
 *   where financial institutions extract wealth from households through
 *   credit mechanisms while presenting the constraint as a coordination
 *   solution (credit enabling consumption and investment). The cycle operates
 *   through multiple reinforcing mechanisms: wage stagnation forces
 *   households to borrow for essential expenses; rising interest rates and
 *   fees increase debt service burden; debt service reduces ability to save,
 *   forcing continued borrowing; asset values and collateral stakes bind
 *   households to the creditor relationship; regulatory theater (consumer
 *   protection laws) persists without effectively limiting extraction. The
 *   constraint demonstrates how a genuine coordination function (credit
 *   systems solving liquidity problems) becomes degraded into a pure
 *   extraction mechanism (debt traps), visible as the transition from Tangled
 *   Rope to Snare across different agent perspectives. The extractiveness
 *   trajectory (0.35 → 0.58) reflects decades of structural change:
 *   financialization of household expenditure, declining real wages,
 *   healthcare and education cost inflation, and removal of traditional exit
 *   paths (bankruptcy reform making discharge harder, predatory lending
 *   practices disguising extraction). The theater ratio trajectory (0.42 →
 *   0.58) captures the increasing performative content of regulation relative
 *   to actual consumer protection.
 *
 * KEY AGENTS:
 *   - Indebted Households: Primary victims (powerless/trapped) — structurally unable to exit debt service; bear extraction through interest, fees, and life opportunity costs
 *   - Financial Institutions: Primary beneficiaries (institutional/arbitrage) — capture interest income, fees, collateral, and behavioral extraction through debt trap design
 *   - Wage Earners Across Generations: Secondary victims (moderate/constrained) — experience coordination benefit but also extraction; generational transmission of debt stress
 *   - Low-Income and Precarious Workers: Organized victims (organized/constrained) — collective perception reveals extraction mechanism despite worker organization
 *   - Regulatory and Government Systems: Institutional actor (institutional/constrained) — maintains performative consumer protection apparatus while extraction mechanisms persist
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (usury rates, predatory design, wage suppression) as immutable economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(household_debt_accumulation_cycle, 0.58).
domain_priors:suppression_score(household_debt_accumulation_cycle, 0.65).
domain_priors:theater_ratio(household_debt_accumulation_cycle, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(household_debt_accumulation_cycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(household_debt_accumulation_cycle, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(household_debt_accumulation_cycle, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(household_debt_accumulation_cycle, snare).
narrative_ontology:human_readable(household_debt_accumulation_cycle, "Household Debt Accumulation Cycle").
narrative_ontology:topic_domain(household_debt_accumulation_cycle, "economic/financial").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(household_debt_accumulation_cycle, financial_institutions).
narrative_ontology:constraint_beneficiary(household_debt_accumulation_cycle, creditors).
narrative_ontology:constraint_victim(household_debt_accumulation_cycle, indebted_households).
narrative_ontology:constraint_victim(household_debt_accumulation_cycle, wage_earners_trapped_in_debt_service).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEBTED HOUSEHOLD (SNARE) — Trapped in debt service by material barriers: credit dependency, wage stagnation, housing costs, medical emergencies, and childcare expenses create structural inability to exit. Extraction is maximized because exit options are materially blocked. The household perceives the constraint as unchangeable within their biographical horizon.
constraint_indexing:constraint_classification(household_debt_accumulation_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WAGE EARNERS ACROSS GENERATIONS (TANGLED ROPE) — At generational horizon, wage earners experience both coordination (credit enables household stability, education, housing) and extraction (debt service reduces disposable income, constrains life choices, transmits financial stress across family lines). High suppression but not total — some households break the cycle through income mobility, inheritance, or debt forgiveness.
constraint_indexing:constraint_classification(household_debt_accumulation_cycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL INSTITUTIONS (ROPE) — Experiences the constraint as coordination: credit systems solve household liquidity problems, enabling consumption smoothing and investment. Institutions benefit from interest income, fee collection, and collateral acquisition. They perceive low suppression and high agency — they can exit individual relationships, modify terms, or securitize portfolios. Net beneficiary with minimal extraction cost.
constraint_indexing:constraint_classification(household_debt_accumulation_cycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AND GOVERNMENT SYSTEMS (PITON) — Regulation of consumer lending persists through institutional inertia despite limited enforcement effectiveness. Truth in Lending Act, Fair Debt Collection Practices Act, and bankruptcy law create performative consumer protections while debt accumulation accelerates. Theater ratio high: regulatory theater maintains appearance of consumer protection while structural extraction mechanisms remain unchanged. Sunset clause absent — no planned termination of the regulatory apparatus.
constraint_indexing:constraint_classification(household_debt_accumulation_cycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LOW-INCOME AND PRECARIOUS WORKERS (SNARE) — Organized collective perception (union membership, worker advocacy groups) reveals the debt cycle as pure extraction with minimal coordination benefit. High suppression: wage stagnation, gig economy, employment instability, and lack of healthcare access force continued credit dependence. Organization provides some agency but exit paths remain materially constrained.
constraint_indexing:constraint_classification(household_debt_accumulation_cycle, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some household debt is inherent to modern economies: time-preference mismatches between consumption and income, uncertainty, and large capital needs (education, housing) create structural necessity for credit systems. This perspective risks naturalizing contingent institutional arrangements (usury rates, predatory lending, debt trap design) as immutable economic laws. The false summit detector will flag this classification as a naturalization of what is actually an extractive institutional arrangement with historical contingency.
constraint_indexing:constraint_classification(household_debt_accumulation_cycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(household_debt_accumulation_cycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(household_debt_accumulation_cycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(household_debt_accumulation_cycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(household_debt_accumulation_cycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(household_debt_accumulation_cycle, TR),
    TR >= 0.70.

:- end_tests(household_debt_accumulation_cycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting the constraint's function as a revenue mechanism for creditors through interest, fees, and debt trap design, offset partially by genuine coordination function (credit enabling household stability). The value reflects that extraction is significant but not total — some households maintain manageable debt levels, and credit does solve genuine liquidity problems. Suppression (0.65): High. Barriers to exit include: structural wage stagnation (external constraint), credit dependency for basic needs (external constraint), credit score system linking debt status to economic opportunity (institutional constraint), bankruptcy reform making discharge harder (policy constraint), lack of household savings (resource constraint), and predatory lending practices disguising trap mechanisms (informational constraint). Theater ratio (0.58): Moderate-high. Regulatory apparatus (Truth in Lending Act, Fair Debt Collection Practices Act, bankruptcy law) creates perception of consumer protection while extraction mechanisms persist largely unchanged. Regulatory enforcement is sporadic; penalties are often lower than extracted rents. Consumer education campaigns frame debt as personal responsibility rather than structural mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The original research group sees coordination (Rope) — they experience credit as enabling household stability and consumption smoothing. Financial institutions genuinely experience the constraint as coordination: credit systems solve their liquidity problems too, and interest income solves their revenue problems. But trapped households perceive pure extraction (Snare) because their structural position offers no exit: they cannot avoid borrowing, cannot negotiate rates, cannot benefit from alternative financial products. The moderate wage-earner perspective sees mixed coordination and extraction (Tangled Rope) — they benefit from credit access but also bear extraction cost through debt service. The organized worker perspective sees extraction (Snare) because collective analysis reveals how debt mechanisms are structurally designed to prevent exit. The regulator perspective sees theater (Piton) — they maintain a performative protection apparatus while the extraction mechanism persists. The false summit (Mountain) risks naturalizing the arrangement as an immutable property of economies with time-preference mismatches, obscuring the contingent institutional choices that converted coordination into extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural relationship to extraction flow. Indebted households occupy maximum target position (d ≈ 0.95): trapped by material barriers, gaining minimal coordination benefit relative to extraction cost. Financial institutions occupy beneficiary position (d ≈ 0.10): benefit from interest and fees, face no material barriers to exit relationships or modify terms. Low-income workers occupy constrained target position (d ≈ 0.80): materially trapped but organized, experiencing extraction but with some collective agency. Regulatory systems occupy ambiguous position (d ≈ 0.60): constrained by political economy (financial industry influence, campaign finance), partly captured by beneficiaries while ostensibly protecting victims. The perspectival gaps reflect these structural differences: beneficiaries see coordination; victims see extraction; regulators see theater.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint demonstrates mandatrophy resolution through perspectival multiplicity. The question 'Is household debt a Rope or a Snare?' admits no single answer — both classifications are structurally valid from different agent positions. The debt arrangement functions as Rope (coordination) for those who exit it or control it (financial institutions, mobile professionals). It functions as Snare (extraction) for those trapped in it (low-income households, precarious workers). The Tangled Rope classification represents the modal case: households experience both coordination benefit (credit access) and extraction (debt service burden, interest, fees). The mandatrophy is resolved by recognizing that a single institutional arrangement can deliver coordination to beneficiaries and extraction to victims simultaneously. The constraint's true type is not 'Rope XOR Snare' but 'structured to transfer coordination benefit to beneficiaries while imposing extraction cost on victims.' The theater ratio trend (increasing over the interval) indicates that regulatory responses are primarily performative — new rules maintain appearance of protection without reducing extraction mechanisms. This pattern is diagnostic of a constraint that has evolved from genuine Rope (early credit systems with lower rates and simpler mechanics) toward Snare (modern debt trap mechanisms with sophisticated extraction design, predatory practices, and regulatory capture).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_necessity_threshold,
    'What portion of household debt represents genuine coordination (consumption smoothing, education investment, housing access) versus extractive accumulation driven by wage stagnation and cost-of-living inflation?',
    'Decompose household debt by origination purpose (mortgage, education, emergency medical, discretionary consumption); correlate with household income trajectory; track repayment vs rollover cycles',
    'If coordination dominates: constraint reclassifies toward Tangled Rope from more perspectives. If extraction dominates: confirms Snare classification across more contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_necessity_threshold, empirical, 'Coordination vs extraction decomposition of household debt').

omega_variable(
    interest_rate_extraction_mechanism,
    'How much of the extraction mechanism is driven by interest rates above minimum necessary (reflecting monopolistic rents and predatory pricing) versus rates that reflect genuine default risk and capital costs?',
    'Compare household debt interest rates to institutional cost of capital; analyze dispersion across credit tiers; identify markup components attributable to adverse selection vs monopoly pricing',
    'High extraction through usurious rates: Snare classification strengthened. Rates aligned with risk: extraction appears more as legitimate coordination cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interest_rate_extraction_mechanism, empirical, 'Interest rate composition: risk-adjusted cost vs monopolistic extraction').

omega_variable(
    exit_path_materiality,
    'Are reported exit paths (debt consolidation, refinancing, bankruptcy) genuinely available to trapped households or are they theater masking continued extraction?',
    'Empirical tracking: percentage of households attempting each exit path; success rates; post-exit financial trajectories; costs (credit score damage, bankruptcy stigma, legal fees)',
    'If exit paths are materially available: reclassify to constrained rather than trapped (Tangled Rope from more perspectives). If theater: trapped classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_path_materiality, empirical, 'Whether debt exit paths are materially available or theater').

omega_variable(
    wage_stagnation_constraint_entanglement,
    'Is the household debt accumulation cycle primarily a mechanism of direct extraction by creditors, or is it a secondary effect of wage stagnation driven by separate labor market constraints?',
    'Compare household debt accumulation trajectories across countries with different wage dynamics; isolate creditor behavior from labor market shocks; model counterfactual with stable real wages',
    'If primary extraction mechanism: debt_accumulation_cycle is autonomous Snare. If secondary to wage stagnation: the primary constraint is labor market extraction, and debt_accumulation_cycle is an amplification mechanism (network dependency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_stagnation_constraint_entanglement, empirical, 'Whether debt accumulation is primary extraction or secondary effect of wage stagnation').

omega_variable(
    cyclical_dynamics_regime,
    'Does the household debt cycle exhibit endogenous oscillation (credit expansion → debt accumulation → defaults → credit contraction → recovery) or secular trend (monotonic accumulation)?',
    'Time-series analysis of aggregate household debt, credit availability, default rates, and interest rates; identify cycle frequency and shock response patterns',
    'Endogenous cycles: measurements should show oscillation pattern; extraction mechanism includes credit availability control. Secular trend: monotonic measurements indicate accumulation; extraction mechanism is more stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cyclical_dynamics_regime, empirical, 'Cyclical vs secular dynamics of household debt').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(household_debt_accumulation_cycle, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hdac_tr_t0, household_debt_accumulation_cycle, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hdac_tr_t10, household_debt_accumulation_cycle, theater_ratio, 10, 0.52).
narrative_ontology:measurement(hdac_tr_t20, household_debt_accumulation_cycle, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(hdac_be_t0, household_debt_accumulation_cycle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hdac_be_t10, household_debt_accumulation_cycle, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hdac_be_t20, household_debt_accumulation_cycle, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(household_debt_accumulation_cycle, resource_allocation).
narrative_ontology:affects_constraint(household_debt_accumulation_cycle, wage_stagnation_mechanism).
narrative_ontology:affects_constraint(household_debt_accumulation_cycle, financial_institution_monopoly_pricing).
narrative_ontology:affects_constraint(household_debt_accumulation_cycle, bankruptcy_reform_reform_cycle).
narrative_ontology:affects_constraint(household_debt_accumulation_cycle, consumer_credit_predatory_design).

% DUAL FORMULATION NOTE:
% Household debt accumulation is upstream of multiple downstream constraints: wage stagnation removes exit paths; predatory lending design converts coordination into extraction; bankruptcy reform removes safety valves; regulatory capture prevents policy intervention. The constraint family decomposes into structural and behavioral components. Structural (resource allocation coordination function, real lending rates) has lower ε; behavioral (predatory design, debt trap mechanisms, regulatory theater) has higher ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(household_debt_accumulation_cycle, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
