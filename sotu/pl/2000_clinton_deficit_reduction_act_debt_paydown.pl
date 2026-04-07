% ============================================================================
% CONSTRAINT STORY: 2000_clinton_deficit_reduction_act_debt_paydown
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_2000_clinton_deficit_reduction_act_debt_paydown, []).

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
 *   constraint_id: 2000_clinton_deficit_reduction_act_debt_paydown
 *   human_readable: Fiscal Discipline Mechanism: Deficit Reduction Act Debt Paydown Constraint
 *   domain: economic/fiscal_policy
 *
 * SUMMARY:
 *   The Deficit Reduction Act of 1993 and subsequent fiscal discipline
 *   mechanisms establish a structural constraint that transforms budget
 *   surpluses from flexible policy instruments into dedicated debt-reduction
 *   flows. The constraint operates through formal rules (pay-as-you-go
 *   budgeting, budget enforcement mechanisms, debt ceiling procedures) and
 *   informal institutional norms (fiscal orthodoxy in economic policy
 *   discourse, credit rating agency discipline, Federal Reserve policy
 *   alignment). The mechanism creates a fundamental allocation conflict:
 *   revenues generated in surplus periods can either fund immediate public
 *   goods (infrastructure, healthcare, education) or reduce outstanding debt.
 *   The constraint mandates debt reduction as the dominant allocation
 *   principle. This creates asymmetric distributional consequences: future
 *   generations benefit through reduced interest costs and restored fiscal
 *   flexibility; immediate constituencies demanding public investment bear
 *   the cost of deferred spending. The constraint is presented and defended
 *   as a natural law of economics (deficits are unsustainable, debt is a
 *   burden), but the structural data reveals it as a contingent institutional
 *   choice with identifiable beneficiaries and victims. The theater_ratio
 *   rise from 0.35 to 0.58 over 15 years indicates increasing performative
 *   content: debt ceiling crises, sequestration suspensions, and rule-waiving
 *   during recessions suggest that the formal apparatus persists more through
 *   political ritual than binding institutional force.
 *
 * KEY AGENTS:
 *   - Deficit Reduction Act proponents and fiscal orthodoxy institutions: Primary beneficiaries (institutional/arbitrage) — central banks, IMF, rating agencies, bond markets benefit from credible debt commitment signaling
 *   - Immediate social program constituencies: Primary victims (powerless/trapped) — constituencies demanding infrastructure investment, healthcare expansion, education funding face structural veto; trapped by fiscal discipline doctrine
 *   - Working population and current taxpayers: Secondary victims (moderate/constrained) — bear extraction through deferred public investment but benefit from averted fiscal crises; constrained by genuine macroeconomic necessity
 *   - Future generations: Nominal beneficiaries (analytical/analytical) — presented as beneficiaries through reduced interest costs but inherit altered fiscal and economic environment; benefit may be negated by foregone current investment in human and physical capital
 *   - Fiscal reform coalition: Organized agents (organized/constrained) — advocates for structural tax and spending reform who see the constraint as temporary forcing mechanism with a sunset once structural imbalance is addressed
 *   - High-income asset holders: Asymmetric beneficiaries (powerful/mobile) — benefit from lower public debt-driven interest rates and stable currency while having mobile exit options; extract through constrained spending preventing tax increases
 *   - Debt ceiling and fiscal rules apparatus: Institutional theater (institutional/arbitrage) — bureaucratic and procedural mechanisms that maintain appearance of fiscal discipline despite frequent violations and suspensions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(2000_clinton_deficit_reduction_act_debt_paydown, 0.52).
domain_priors:suppression_score(2000_clinton_deficit_reduction_act_debt_paydown, 0.68).
domain_priors:theater_ratio(2000_clinton_deficit_reduction_act_debt_paydown, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(2000_clinton_deficit_reduction_act_debt_paydown, extractiveness, 0.52).
narrative_ontology:constraint_metric(2000_clinton_deficit_reduction_act_debt_paydown, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(2000_clinton_deficit_reduction_act_debt_paydown, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(2000_clinton_deficit_reduction_act_debt_paydown, tangled_rope).
narrative_ontology:human_readable(2000_clinton_deficit_reduction_act_debt_paydown, "Fiscal Discipline Mechanism: Deficit Reduction Act Debt Paydown Constraint").
narrative_ontology:topic_domain(2000_clinton_deficit_reduction_act_debt_paydown, "economic/fiscal_policy").

domain_priors:requires_active_enforcement(2000_clinton_deficit_reduction_act_debt_paydown).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(2000_clinton_deficit_reduction_act_debt_paydown, future_generations).
narrative_ontology:constraint_beneficiary(2000_clinton_deficit_reduction_act_debt_paydown, bond_markets).
narrative_ontology:constraint_beneficiary(2000_clinton_deficit_reduction_act_debt_paydown, fiscal_orthodoxy_institutions).
narrative_ontology:constraint_victim(2000_clinton_deficit_reduction_act_debt_paydown, immediate_social_program_constituencies).
narrative_ontology:constraint_victim(2000_clinton_deficit_reduction_act_debt_paydown, discretionary_spending_advocates).
narrative_ontology:constraint_victim(2000_clinton_deficit_reduction_act_debt_paydown, current_generation_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMMEDIATE SOCIAL PROGRAM CONSTITUENCY (SNARE) — Constituencies demanding infrastructure, healthcare, education, and social services face a structural veto: surplus revenues are constitutionally and institutionally directed toward debt repayment regardless of current urgent needs. No exit option exists; political mobilization to redirect surpluses faces the doctrine of fiscal responsibility as an immutable constraint. Maximum experienced extraction — immediate needs are deferred indefinitely while debt service consumes political-economic flexibility.
constraint_indexing:constraint_classification(2000_clinton_deficit_reduction_act_debt_paydown, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING POPULATION AND CURRENT TAXPAYERS (TANGLED ROPE) — Constrained by fiscal necessity (deficits genuinely require future correction) but also coordinates with the constraint's coordination function: reducing debt averts future fiscal crises that would impact employment and economic stability. High suppression (cannot redirect surpluses to immediate programs) paired with genuine coordination benefit (averted future crises). Significant extraction with embedded coordination.
constraint_indexing:constraint_classification(2000_clinton_deficit_reduction_act_debt_paydown, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FISCAL ORTHODOXY INSTITUTIONS (ROPE) — Central Bank, IMF, rating agencies, bond markets experience the constraint as coordination: predictable debt reduction signals fiscal seriousness, stabilizes currency, lowers borrowing costs, and prevents future crises. These institutions benefit from the constraint and perceive it as solving a genuine coordination problem (credibility signaling). Net beneficiary with arbitrage options (can exit fiscal discipline and accept higher borrowing costs, but choose not to).
constraint_indexing:constraint_classification(2000_clinton_deficit_reduction_act_debt_paydown, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FISCAL REFORM AND SUNSET COALITION (SCAFFOLD) — Organized advocates for pay-as-you-go budgeting, tax reform, and entitlement restructuring perceive the debt constraint as a temporary forcing mechanism that should sunset once structural fiscal reform (tax base expansion, spending efficiency, demographic adjustment) solves the underlying imbalance. Extraction is tolerated only because the coalition sees a real exit path and timeline. Sunset rationale: Once revenue growth stabilizes and spending growth aligns with economic growth, the artificial constraint on discretionary spending becomes unnecessary.
constraint_indexing:constraint_classification(2000_clinton_deficit_reduction_act_debt_paydown, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HIGH-INCOME AND ASSET-HOLDING BENEFICIARIES (TANGLED ROPE) — Actors with significant financial assets benefit from lower interest rates (driven by lower public debt demand) and stable currency valuations. They also benefit from constrained public spending that prevents tax increases necessary to fund immediate social programs. This group experiences genuine coordination (stable macroeconomic environment) alongside asymmetric extraction: they have mobile options (invest elsewhere, lobby for tax cuts, relocate) while constrained groups do not. They coordinate with the constraint's defenders while extracting from those without exit options.
constraint_indexing:constraint_classification(2000_clinton_deficit_reduction_act_debt_paydown, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: THE DEBT CEILING AND FISCAL RULES THEATER (PITON) — The formal procedural apparatus (debt ceiling votes, budget reconciliation, sequestration triggers) performs fiscal discipline but has become substantially theatrical. The debt ceiling is routinely raised, sequestration is frequently suspended, and fiscal rules are waived during crises. The ritual persists (periodic cliff negotiations, shutdown threats) despite minimal functional constraint on actual borrowing. Piton classification derives from theater_ratio (0.58) — significant performative content, degraded functional constraint.
constraint_indexing:constraint_classification(2000_clinton_deficit_reduction_act_debt_paydown, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, fiscal constraints are presented as immutable laws of economics: deficits produce inflation, debt produces crises, living beyond means produces inevitable correction. This perspective naturalizes fiscal discipline as inherent to economic reality rather than as a contingent institutional choice. However, the structural data contradicts this — identifiable beneficiaries, measurable suppression, and organized opposition reveal this as a false summit: fiscal discipline is a constructed policy framework, not a natural law.
constraint_indexing:constraint_classification(2000_clinton_deficit_reduction_act_debt_paydown, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(2000_clinton_deficit_reduction_act_debt_paydown_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(2000_clinton_deficit_reduction_act_debt_paydown, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(2000_clinton_deficit_reduction_act_debt_paydown, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(2000_clinton_deficit_reduction_act_debt_paydown, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(2000_clinton_deficit_reduction_act_debt_paydown, TR),
    TR >= 0.70.

:- end_tests(2000_clinton_deficit_reduction_act_debt_paydown_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint directs resources away from constituencies with immediate needs toward a future state (low debt burden). The extraction is real — discretionary spending is suppressed, constituencies with demand for public goods are denied resources — but moderate rather than severe because some coordination benefit exists (genuine macroeconomic stability concerns justify some deficit reduction). The rise from 0.28 (1993, genuine crisis response) to 0.52 (2003-2008, as fiscal orthodoxy becomes entrenched) reflects accumulated layers of institutional enforcement. Suppression (0.68): High. Multiple mechanisms suppress alternatives to the constraint: fiscal rules with enforcement teeth (sequestration), political norm against deficit spending, international pressure (IMF, credit rating agencies), and epistemological lock-in (fiscal discipline naturalized as economic law). Constituencies cannot easily mobilize alternative fiscal arrangements. Theater ratio (0.58): Moderate-high. The formal apparatus includes genuine functional elements (budget reconciliation process, CBO analysis) but also substantial theater (debt ceiling crises are procedurally unnecessary, sequestration is frequently suspended, fiscal rules are waived during crises). The rise from 0.35 to 0.58 indicates increasing ratio of procedural drama to actual binding constraint as the political economy of the mechanism becomes more contested.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates maximum perspectival distance. The immediate constituency's Snare classification derives from powerless/trapped positioning: they have no veto, no exit, and no compensation. The fiscal institution's Rope classification derives from institutional/arbitrage positioning: they solve genuine coordination problems and can exit without catastrophic cost (markets would adjust, borrowing costs would rise, but the state could continue operating). The analytical observer's temptation toward Mountain classification reveals how fiscal orthodoxy naturalizes a contingent institutional arrangement — the 'law' that deficits are unsustainable is contingent on specific institutional choices about who bears the burden of adjustment. If the burden were shifted to high-income tax increases rather than discretionary spending cuts, the fiscal arithmetic would be identical but the institutional constraint would not exist.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain reveals why this constraint extracts from some agents while benefiting others. Immediate social program constituencies are declared victims — they bear extraction through deferred spending. Future generations are declared beneficiaries — they benefit through reduced interest costs. This beneficiary/victim split alone generates directionality asymmetry. Add exit options: immediate constituencies are trapped (no legal authority can override fiscal discipline, no coalition can mobilize sufficient power); future generations are analytical (they are not yet present). This trapped + victim combination produces maximum d (≈0.95) and maximum f(d) (≈1.42), yielding high experienced chi. Fiscal institutions are declared beneficiaries and hold arbitrage exit options (can choose to enforce discipline or tolerate deficits), producing low d (≈0.15) and negative f(d) (≈-0.01), yielding negative or minimal chi — the constraint subsidizes them. High-income asset holders are declared beneficiaries with mobile exit options, producing low d (≈0.20) and low f(d) (≈0.02). The perspectival gaps in chi output reveal that the constraint systematically transfers resources from powerless/trapped agents to institutional/powerful agents.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that all seven types emerge from legitimate but distinct indexical positions. The mandatrophy question is not 'which type is correct?' but 'which extractive mechanism do you see from where?' The immediate constituency sees pure extraction (Snare) because they are maximally constrained and receive no coordination benefit. Fiscal institutions see pure coordination (Rope) because they solve genuine credibility problems. The working population sees hybrid coordination-extraction (Tangled Rope) because they both benefit from stability and lose service access. The procedural apparatus sees degraded ritual (Piton) because the formal rules are frequently violated. The reform coalition sees temporary coordination with a sunset (Scaffold) because they perceive structural solutions that would obsolete the temporary constraint. The analytical observer risks seeing a natural law (Mountain) because fiscal discipline is narratively naturalized. The ambiguity is not about classification methodology — it is about whether the underlying constraint is genuine (coordination solving a real fiscal sustainability problem) or constructed (institutional choice to protect high-income beneficiaries from taxation). The omega variables identify the empirical tests that would resolve this: Does future generation benefit outweigh foregone current investment? Is suppression structural or contingent on revenue policy choices? How much of the constraint's force is credible versus theatrical?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_debt_to_gdp_ratio,
    'What debt-to-GDP ratio level constitutes genuine fiscal insolvency versus politically constructed constraint?',
    'Comparative analysis of sustainable debt levels across developed economies; empirical correlation between debt ratios and economic performance; modeling of actual debt service paths under various spending scenarios',
    'If optimal ratio exists at current levels: constraint is justified coordination problem. If optimal ratio is substantially higher: constraint is extractive artificial scarcity that could be relaxed without future cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_debt_to_gdp_ratio, empirical, 'Optimal debt-to-GDP threshold for fiscal health').

omega_variable(
    future_generations_actual_benefit,
    'Do future generations actually benefit from current debt reduction, or do they inherit a different fiscal environment that nullifies the benefit?',
    'Intergenerational accounting: net present value of avoided interest costs versus foregone public investment in education, infrastructure, and human capital. Long-run economic growth comparisons between debt-reduction vs. investment-focused fiscal strategies.',
    'If debt reduction net-benefits future generations: constraint is justified (rope or coordination). If foregone investment causes greater long-run damage: constraint is extractive transfer from present to future (snare reframing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_generations_actual_benefit, empirical, 'Whether future generations net-benefit from current debt reduction').

omega_variable(
    suppression_mechanism_elasticity,
    'Is the suppression of discretionary spending a structural feature of fiscal constraints or a contingent political choice to avoid revenue increases?',
    'Historical analysis of deficit reduction in periods with various revenue policies; comparison of countries using spending cuts vs. revenue increases for fiscal consolidation; modeling of alternative constraint designs that split burden differently',
    'If structural: suppression is inherent and constraint remains snare/tangled rope. If contingent: burden allocation reveals that constraint is a choice to extract from discretionary spending constituencies while protecting high-income beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_elasticity, empirical, 'Whether suppression of discretionary spending is structural or political choice').

omega_variable(
    fiscal_rule_credibility_decay,
    'How much of the constraint''s extractive force derives from credible enforcement versus performative ritual that persists despite repeated violations?',
    'Time-series analysis of debt ceiling votes, sequestration triggers, and suspension frequency; measurement of market response to rule violations; correlation between rule severity and actual budget outcomes',
    'If highly credible: constraint extracts through genuine discipline. If substantially theatrical: much of the extraction occurs through psychological lock-in and political norm rather than binding institutional force — suggests piton degradation is advanced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_rule_credibility_decay, empirical, 'Credibility vs. performative content of fiscal rules').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(2000_clinton_deficit_reduction_act_debt_paydown, 1993, 2008).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defrd_theater_1993, 2000_clinton_deficit_reduction_act_debt_paydown, theater_ratio, 0, 0.35).
narrative_ontology:measurement(defrd_theater_1998, 2000_clinton_deficit_reduction_act_debt_paydown, theater_ratio, 5, 0.42).
narrative_ontology:measurement(defrd_theater_2003, 2000_clinton_deficit_reduction_act_debt_paydown, theater_ratio, 10, 0.52).
narrative_ontology:measurement(defrd_theater_2008, 2000_clinton_deficit_reduction_act_debt_paydown, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(defrd_extractiveness_1993, 2000_clinton_deficit_reduction_act_debt_paydown, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(defrd_extractiveness_1998, 2000_clinton_deficit_reduction_act_debt_paydown, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(defrd_extractiveness_2003, 2000_clinton_deficit_reduction_act_debt_paydown, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(defrd_extractiveness_2008, 2000_clinton_deficit_reduction_act_debt_paydown, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(2000_clinton_deficit_reduction_act_debt_paydown, resource_allocation).
narrative_ontology:affects_constraint(2000_clinton_deficit_reduction_act_debt_paydown, entitlement_spending_limitations).
narrative_ontology:affects_constraint(2000_clinton_deficit_reduction_act_debt_paydown, tax_policy_floor_constraints).
narrative_ontology:affects_constraint(2000_clinton_deficit_reduction_act_debt_paydown, government_shutdown_dynamics).

% DUAL FORMULATION NOTE:
% The fiscal discipline constraint can be decomposed into multiple structurally distinct constraints: (1) debt service as a percentage of budget (mechanical arithmetic), (2) political commitment to debt reduction as a policy priority (institutional choice), and (3) formal fiscal rules and procedures (procedural architecture). Each has different epsilon values reflecting different empirical status. This story focuses on the institutional commitment (ε=0.52) as the binding constraint; the procedural apparatus (ε=0.35 theatrical content) is a separate story (piton family member).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(2000_clinton_deficit_reduction_act_debt_paydown, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
