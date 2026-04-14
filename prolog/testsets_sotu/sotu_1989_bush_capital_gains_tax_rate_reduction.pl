% ============================================================================
% CONSTRAINT STORY: sotu_1989_bush_capital_gains_tax_rate_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1989_bush_capital_gains_tax_rate_reduction, []).

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
 *   constraint_id: sotu_1989_bush_capital_gains_tax_rate_reduction
 *   human_readable: Capital Gains Tax Rate Reduction as Investment Stimulus (1989)
 *   domain: fiscal_policy/taxation/inequality
 *
 * SUMMARY:
 *   The 1989 reduction of maximum capital gains tax rate from 28% to 15%
 *   exemplifies a constraint that claims coordination (stimulating productive
 *   investment) while embedding significant extraction (shifting tax
 *   incidence toward wage earners and reducing progressivity). The policy
 *   mechanism uses tax structure to create differential returns on capital
 *   vs. labor income, intended to increase capital formation under the
 *   presumption that lower capital taxation generates offsetting revenue
 *   through economic growth (Laffer dynamics). The constraint exhibits all
 *   six classification types from different structural positions. Capital
 *   holders and institutional investors see pure coordination (Rope) — the
 *   policy directly incentivizes their behavior. Wage-earners trapped within
 *   labor income see pure extraction (Snare) — they bear the tax incidence
 *   without benefit. High-income earners with diversified portfolios
 *   experience mixed coordination-extraction (Tangled Rope) — they benefit
 *   from the rate reduction but see peers with labor-only income
 *   disadvantaged. Organized labor agents see a constraint they can partially
 *   resist (Tangled Rope) — they have agency to mobilize political
 *   countervailing responses. The analytical observer at civilizational scope
 *   risks seeing immutable incentive logic (Mountain) — capital must be taxed
 *   less than labor — but this naturalizes a policy choice. The theater ratio
 *   of 0.48 reflects the gap between the claimed growth-coordination function
 *   and the measured outcomes. The dynamic feedback hypothesis (lower rates →
 *   higher capital formation → higher revenues) is plausible but unproven;
 *   much of the policy case relies on this theoretical claim rather than
 *   demonstrated evidence.
 *
 * KEY AGENTS:
 *   - Capital Holders: Primary beneficiary (institutional/arbitrage) — direct rate reduction increases after-tax returns; international capital mobility provides exit option and strengthens bargaining position
 *   - Wage Earners: Primary victim (powerless/trapped) — no benefit from rate reduction; structurally trapped within labor income taxation; bear effective tax incidence shift
 *   - High-Income Wage Earners: Secondary victim (moderate/constrained) — benefit from rate reduction on portfolio income but see wage-income progressivity reduced; constrained exit through income restructuring
 *   - Institutional Investors: Beneficiary (institutional/arbitrage) — large asset bases mean substantial absolute tax benefit; capital mobility provides exit option to lower-tax jurisdictions
 *   - Labor Unions: Organized victim (organized/constrained) — can mobilize political responses; constrained by inability to exit national jurisdiction
 *   - Federal Treasury (Revenue Perspective): Nominal victim (analytical/analytical) — direct revenue loss from rate reduction; dependent on dynamic feedback to offset incidence
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing policy choice as immutable law; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1989_bush_capital_gains_tax_rate_reduction, 0.58).
domain_priors:suppression_score(sotu_1989_bush_capital_gains_tax_rate_reduction, 0.65).
domain_priors:theater_ratio(sotu_1989_bush_capital_gains_tax_rate_reduction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1989_bush_capital_gains_tax_rate_reduction, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1989_bush_capital_gains_tax_rate_reduction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1989_bush_capital_gains_tax_rate_reduction, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1989_bush_capital_gains_tax_rate_reduction, tangled_rope).
narrative_ontology:human_readable(sotu_1989_bush_capital_gains_tax_rate_reduction, "Capital Gains Tax Rate Reduction as Investment Stimulus (1989)").
narrative_ontology:topic_domain(sotu_1989_bush_capital_gains_tax_rate_reduction, "fiscal_policy/taxation/inequality").

domain_priors:requires_active_enforcement(sotu_1989_bush_capital_gains_tax_rate_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1989_bush_capital_gains_tax_rate_reduction, capital_holders).
narrative_ontology:constraint_beneficiary(sotu_1989_bush_capital_gains_tax_rate_reduction, institutional_investors).
narrative_ontology:constraint_beneficiary(sotu_1989_bush_capital_gains_tax_rate_reduction, high_income_earners).
narrative_ontology:constraint_victim(sotu_1989_bush_capital_gains_tax_rate_reduction, wage_earners).
narrative_ontology:constraint_victim(sotu_1989_bush_capital_gains_tax_rate_reduction, fiscal_progressivity).
narrative_ontology:constraint_victim(sotu_1989_bush_capital_gains_tax_rate_reduction, revenue_distribution_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE-DEPENDENT HOUSEHOLDS (SNARE) — No capital holdings to benefit from rate reduction. Trapped within labor income taxation. As policy shifts tax incidence toward wages through effective rate changes, these households experience pure extraction with no coordination benefit. Career mobility cannot escape the structural tax asymmetry.
constraint_indexing:constraint_classification(sotu_1989_bush_capital_gains_tax_rate_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER SAVERS WITHOUT CAPITAL WEALTH (SNARE) — Can exit through savings and investment accumulation, but at high cost and with multi-year horizon. Meanwhile, policy creates differential return on capital vs. labor income. Experienced extraction is moderate-to-high: policy benefits those who already have capital, constrains paths for those accumulating capital through wages.
constraint_indexing:constraint_classification(sotu_1989_bush_capital_gains_tax_rate_reduction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL HOLDERS & INSTITUTIONAL INVESTORS (ROPE) — Immediate beneficiaries. Rate reduction directly subsidizes investment returns. Pure coordination function: the policy exists to solve the presumed coordination problem of insufficient capital formation. These agents experience it as coordination mechanism—their investment behavior is the targeted outcome. Arbitrage exit: capital is internationally mobile, and lower rates reduce the exit cost of maintaining investment in domestic markets.
constraint_indexing:constraint_classification(sotu_1989_bush_capital_gains_tax_rate_reduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HIGH-INCOME EARNERS WITH DIVERSIFIED PORTFOLIOS (TANGLED ROPE) — Experience both coordination and extraction. The policy coordinates capital investment (benefits them), but also extracts from fellow high-income earners who derive income primarily from wages. Asymmetric extraction: portfolio-income earners benefit; wage-income earners in the same bracket do not. Exit is mobile but costly (requires portfolio restructuring or relocation). Classification reflects mixed benefit and coordination function.
constraint_indexing:constraint_classification(sotu_1989_bush_capital_gains_tax_rate_reduction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR UNIONS & WAGE-EARNER ADVOCATES (TANGLED ROPE) — Organized agents that experience extraction but retain some agency. The policy creates identifiable extraction (shifting tax incidence away from capital), but unions can coordinate countervailing policy responses, wage negotiations, and political mobilization. Constrained exit: exit the national tax jurisdiction is prohibitive; exit through political organizing is available at coordination cost.
constraint_indexing:constraint_classification(sotu_1989_bush_capital_gains_tax_rate_reduction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE REVENUE-NEUTRALITY THEATER (PITON) — The policy's stated justification is that lower capital gains rates will increase tax revenue through economic growth (Laffer curve hypothesis). Empirically, this claim is substantially unproven and theater-heavy: theoretical argument for dynamic feedback is plausible but contested. The institutional commitment to revenue-neutrality persists despite limited evidence, maintained through narrative rather than demonstrated function. Theater ratio reflects the gap between claimed coordination benefit (growth that offsets lower rates) and measured outcomes.
constraint_indexing:constraint_classification(sotu_1989_bush_capital_gains_tax_rate_reduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, some tax-rate differential between capital and labor income is structurally necessary: capital taxation distorts investment decisions; labor taxation distorts labor supply. Lower capital-gains taxation follows from first principles of incentive design. However, this perspective naturalizes a policy choice (how much differential is optimal) as immutable law (that capital should be taxed less than labor). The false summit detection identifies this: identifiable beneficiaries (capital holders) benefit from the naturalization.
constraint_indexing:constraint_classification(sotu_1989_bush_capital_gains_tax_rate_reduction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1989_bush_capital_gains_tax_rate_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1989_bush_capital_gains_tax_rate_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1989_bush_capital_gains_tax_rate_reduction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1989_bush_capital_gains_tax_rate_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1989_bush_capital_gains_tax_rate_reduction, TR),
    TR >= 0.70.

:- end_tests(sotu_1989_bush_capital_gains_tax_rate_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high, reflecting substantial asymmetry between capital and labor income treatment and clear distributional consequences. The extraction is not maximal (0.60+) because the policy has a genuine coordination logic — capital formation does require some tax incentive, and lower rates can increase investment. However, the magnitude of the rate differential and the incidence shift justify a score above 0.50. The measurement trajectory shows extractiveness rising from 0.42 (pre-1989) to 0.60 (1999 decade view) as inequality effects accumulate and revenue-neutrality claims prove increasingly difficult to defend. Suppression (0.65): High. The policy constrains alternatives through several mechanisms: (1) wage earners cannot restructure their income to avoid the unfavorable tax treatment without career-path disruption; (2) the political opportunity cost is high — capital gains rate reduction consumed tax-reform bandwidth that could have been directed to other priorities; (3) the revenue-neutrality claim suppresses political space for subsequent base broadening (broadening capital gains taxation would seem to violate the growth-promoting intent). Theater ratio (0.48): Moderate. The policy's coordination function is genuine (capital formation is a real coordination problem) but the growth-revenue offset claim is substantially theater — the Laffer dynamics are theoretically plausible but empirically ambiguous. The theater increases over the interval as actual revenue outcomes become visible and diverge from projections.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits substantial perspectival divergence. Capital holders see Rope (pure coordination) — the policy solves the investment-incentive problem and produces the intended behavioral response. Wage earners see Snare (pure extraction) — they are trapped in labor income taxation with no benefit and cannot escape the incidence shift. High-income earners with portfolios see Tangled Rope (mixed benefit and extraction) — they benefit from lower rates on capital income but see fellow high-earners (those with primarily wage income) disadvantaged. Labor organizations see constrained Tangled Rope — they experience extraction but retain agency to mobilize political responses. The analytical observer risks seeing Mountain (immutable incentive structure) — capital must be taxed less than labor — but the engine's false summit detection identifies this as naturalization of a contingent policy choice. The perspectival gap is diagnostic: if all agents saw the same classification, it would suggest either weak extraction (everyone benefits) or weak coordination (everyone is harmed). The fact that beneficiaries see coordination and victims see extraction confirms the constraint's true hybrid character.
 *
 * DIRECTIONALITY LOGIC:
 *   The chi calculation derives from the structural relationship of each agent to the tax rate differential. Capital holders (d ≈ 0.10, beneficiary + arbitrage) experience negative effective extraction — the policy subsidizes them. Wage earners (d ≈ 0.90, victim + trapped) experience high effective extraction — they bear incidence. High-income earners with mixed income (d ≈ 0.55) experience moderate extraction on the wage-income portion and negative extraction on the capital-income portion. The aggregate chi for the national context is χ ≈ 0.58 (from ε × f(d) × σ(S), where ε=0.58, average f(d) across the distribution ≈ 0.75, σ(national)=1.0). This produces effective extraction of approximately 0.44 at the mean, but distributed highly unequally — zero or negative extraction for capital holders, high extraction (0.85+) for wage-dependent powerless agents.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine coordination (capital formation) from distributional extraction (incidence shift). The policy has a real coordination function: capital formation is indeed a collective action problem, and tax rates do affect investment decisions. The coordination is the reason the classification is Tangled Rope rather than pure Snare — the policy is not purely extractive. However, the policy also embeds extraction: it shifts tax incidence toward those with less bargaining power (wage earners) and increases inequality. The mandatrophy is resolved by measuring whether the coordination benefit (additional capital formation) exceeds the extraction cost (inequality increase, incidence shift). Empirically, this depends on the dynamic feedback elasticity (omega variable 1) and the incidence distribution (omega variable 3). If elasticity is high and incidence is efficiently distributed, the policy is a plausible Tangled Rope with net coordination benefit. If elasticity is low and incidence is concentrated on powerless agents, the policy is a Tangled Rope with net extraction cost. The engine does not adjudicate this tradeoff — it flags the constraint as requiring omega resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dynamic_feedback_magnitude,
    'What is the actual elasticity of capital formation and tax revenue to capital gains rate reductions? Does the Laffer curve dynamics offset the direct revenue loss?',
    'Econometric estimation of capital formation elasticity post-1989; comparison of projected vs. actual federal revenue from capital gains taxation over 5-year and 10-year horizons; decomposition of revenue changes by behavioral response vs. rate effect',
    'If elasticity is high (behavioral response > 50% revenue offset): policy coordinates capital formation efficiently; extraction component is modest; Tangled Rope classification holds with rope-leaning bias. If elasticity is low (behavioral response < 20% revenue offset): policy is primarily redistributive extraction; classification leans toward Snare from wage-earner perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynamic_feedback_magnitude, empirical, 'Elasticity of capital formation and federal revenue to rate reduction').

omega_variable(
    alternative_investment_coordination,
    'Would capital formation occur at similar levels without the rate reduction if coordinated through other mechanisms (public investment, regulatory reform, credit availability)?',
    'Comparison with peer economies using different capital coordination mechanisms; counterfactual analysis of capital formation under alternative policy regimes; identification of which aspects of capital formation are actually rate-sensitive vs. driven by other factors (regulatory environment, trade policy, credit conditions)',
    'If alternative coordination is possible: rate reduction is not the only way to solve capital formation coordination problem; policy appears more as targeted extraction (benefiting capital holders specifically) than as pure coordination. If alternative coordination is weak: rate reduction is the dominant mechanism; policy appears more as essential coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_investment_coordination, conceptual, 'Whether capital formation can be coordinated through non-tax mechanisms').

omega_variable(
    incidence_distribution_certainty,
    'Who actually bears the tax incidence of shifts toward wage taxation? Is the incidence borne by current wage earners, or distributed across generations through debt or inflation?',
    'Intertemporal incidence analysis; decomposition of tax-burden changes by income source (wages vs. capital) across income distribution over 20-year horizon; measurement of effective tax rate changes by decile and wealth quintile',
    'If incidence is on current wage earners: extraction is concentrated and immediate; Snare classification from powerless perspective is accurate. If incidence is distributed across generations via fiscal deficits: extraction is diffused and delayed; classification shifts toward Rope (coordination across time periods) from some perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incidence_distribution_certainty, empirical, 'Distribution of tax incidence across groups and time periods').

omega_variable(
    inequality_growth_tradeoff,
    'Does capital gains taxation reduction increase long-term growth enough to justify or offset increased inequality?',
    'Long-term growth accounting analysis post-1989; comparison of growth-per-unit-inequality produced by capital tax reduction vs. alternative growth policies (human capital investment, infrastructure, R&D subsidies); measurement of whether median real wage growth keeps pace with capital income growth',
    'If growth benefit is large relative to inequality cost: policy is defensible as beneficial coordination mechanism. If growth benefit is small relative to inequality increase: policy is primarily redistributive extraction masked by growth rhetoric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inequality_growth_tradeoff, preference, 'Tradeoff between growth and inequality outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1989_bush_capital_gains_tax_rate_reduction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1989_bush_capital_gains_tax_rate_reduction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sotu_tr_t5, sotu_1989_bush_capital_gains_tax_rate_reduction, theater_ratio, 5, 0.48).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1989_bush_capital_gains_tax_rate_reduction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sotu_be_t2, sotu_1989_bush_capital_gains_tax_rate_reduction, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(sotu_be_t5, sotu_1989_bush_capital_gains_tax_rate_reduction, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(sotu_be_t10, sotu_1989_bush_capital_gains_tax_rate_reduction, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1989_bush_capital_gains_tax_rate_reduction, resource_allocation).
narrative_ontology:affects_constraint(sotu_1989_bush_capital_gains_tax_rate_reduction, wealth_accumulation_inequality).
narrative_ontology:affects_constraint(sotu_1989_bush_capital_gains_tax_rate_reduction, fiscal_progressivity_erosion).
narrative_ontology:affects_constraint(sotu_1989_bush_capital_gains_tax_rate_reduction, capital_flight_arbitrage).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the assumption that capital formation is undersupplied (requires tax incentive) and upstream of wealth concentration dynamics. The capital gains rate reduction is a specific mechanism for solving the capital formation coordination problem; alternative mechanisms (public investment, regulatory reform, credit availability) would produce different constraint stories with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1989_bush_capital_gains_tax_rate_reduction, institutional, 0.08).
constraint_indexing:directionality_override(sotu_1989_bush_capital_gains_tax_rate_reduction, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
