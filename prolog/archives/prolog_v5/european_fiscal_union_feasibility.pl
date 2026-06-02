% ============================================================================
% CONSTRAINT STORY: european_fiscal_union_feasibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_european_fiscal_union_feasibility, []).

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
 *   constraint_id: european_fiscal_union_feasibility
 *   human_readable: European Fiscal Union Feasibility Constraint
 *   domain: political_economy/monetary_union
 *
 * SUMMARY:
 *   The European monetary union constraint operates as a hybrid
 *   coordination-extraction mechanism that exhibits dramatically different
 *   structural effects depending on the observer's position within the
 *   eurozone hierarchy. The constraint coordinates cross-border capital
 *   flows, trade integration, and monetary stability while simultaneously
 *   extracting fiscal autonomy, labor market flexibility, and distributional
 *   power from peripheral member states. The constraint's extractiveness has
 *   followed a crisis-reform-persistence pattern: moderate extraction during
 *   the stability era (1999–2007), maximal extraction during the sovereign
 *   debt crisis (2010–2015), and partial stabilization post-ECB intervention
 *   (2015–2024). The theater ratio has risen throughout, reflecting
 *   increasing gap between nominal rules (Stability and Growth Pact) and
 *   actual enforcement (selective application, rule suspension). This
 *   constraint exemplifies how monetary union without fiscal union creates
 *   structural misalignment: creditor states benefit from cheap borrowing
 *   access to peripheral markets and political leverage over fiscal policy,
 *   while peripheral states face conditionality, austerity mandates, and
 *   capital flight that creditor states do not. The constraint is embedded in
 *   EU institutional design — active enforcement through troika
 *   conditionality, ECB collateral policy, and Commission fiscal surveillance
 *   — making it a tangled rope rather than either pure coordination or pure
 *   extraction.
 *
 * KEY AGENTS:
 *   - Creditor Northern States (Germany, Netherlands, Finland): Institutional/arbitrage — benefit from export markets, financial deepening, political influence over fiscal policy; experience constraint as coordination mechanism (Rope)
 *   - Debtor Peripheral States (Greece, Spain, Portugal, Italy): Powerless/trapped — face austerity conditionality, labor market deregulation, pension cuts, and capital flight; maximum extraction (Snare)
 *   - Mid-Tier Peripheral Economies (Poland, Romania, Czech Republic): Moderate/constrained — receive structural funds (coordination benefit) but face regulatory harmonization costs and labor migration pressure (Tangled Rope)
 *   - EU Institutional Complex (Commission, ECB, Council): Institutional/constrained — genuinely coordinate cross-border spillovers but also extract through expanded supranational mandate (Tangled Rope)
 *   - Financial Markets & Capital: Organized/arbitrage — extract through bond spreads, currency speculation, and collateral-triggered fire sales; benefit from eurozone fragmentation (implicit beneficiary)
 *   - Analytical Observer: Analytical/analytical — at risk of naturalizing contingent institutional design as immutable macroeconomic law (false Mountain)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(european_fiscal_union_feasibility, 0.58).
domain_priors:suppression_score(european_fiscal_union_feasibility, 0.68).
domain_priors:theater_ratio(european_fiscal_union_feasibility, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(european_fiscal_union_feasibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(european_fiscal_union_feasibility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(european_fiscal_union_feasibility, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(european_fiscal_union_feasibility, tangled_rope).
narrative_ontology:human_readable(european_fiscal_union_feasibility, "European Fiscal Union Feasibility Constraint").
narrative_ontology:topic_domain(european_fiscal_union_feasibility, "political_economy/monetary_union").

domain_priors:requires_active_enforcement(european_fiscal_union_feasibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(european_fiscal_union_feasibility, creditor_northern_states).
narrative_ontology:constraint_beneficiary(european_fiscal_union_feasibility, structural_fund_recipients).
narrative_ontology:constraint_victim(european_fiscal_union_feasibility, debtor_southern_states).
narrative_ontology:constraint_victim(european_fiscal_union_feasibility, peripheral_economies).
narrative_ontology:constraint_victim(european_fiscal_union_feasibility, eurozone_political_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL DEBTOR STATE (SNARE) — Nations like Greece, Portugal, or Spain during crisis periods face structural entrapment within eurozone constraints. Exit options (euro exit, fiscal default) carry catastrophic costs (capital flight, banking collapse, currency collapse). Suppression is maximal through legal eurozone membership requirements, ECB conditionality, and IMF structural adjustment programs. The constraint extracts through austerity mandates, pension cuts, labor flexibility requirements, and asset sales — all imposed as conditions of continued financing. No meaningful exit; maximum experienced extraction.
constraint_indexing:constraint_classification(european_fiscal_union_feasibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MID-TIER PERIPHERAL ECONOMY (TANGLED ROPE) — Nations like Poland, Romania, or Czech Republic occupy a hybrid position. They receive substantial structural funds (coordination benefit) but face labor migration pressure, capital flight to richer EU members, and regulatory harmonization costs (extraction). Exit is theoretically possible (EU withdrawal) but costly — loss of structural funds, trade advantages, institutional access. The constraint coordinates EU infrastructure development alongside extracting regulatory compliance and institutional sovereignty. Genuine mixed experience.
constraint_indexing:constraint_classification(european_fiscal_union_feasibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDITOR NORTHERN STATE (ROPE) — Germany, Netherlands, or Finland see the eurozone constraint as primarily coordination: currency stability, export markets, financial deepening, and political influence over peripheral fiscal policy. Suppression is minimal — no conditionality applied to northern states. The constraint benefits creditors through cheap financing (peripheral borrowing in euros at convergence rates), political leverage over debtors, and export-led growth. These states experience the constraint as coordinating a favorable monetary union structure, with minimal coercion applied to them.
constraint_indexing:constraint_classification(european_fiscal_union_feasibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: EU INSTITUTIONAL COMPLEX (TANGLED ROPE) — The Commission, ECB, and Council face genuine coordination problems (cross-border fiscal spillovers, monetary transmission asymmetries) requiring some shared rules. But these institutions also extract through supranational authority, fiscal conditionality, and sovereignty constraints imposed on member states. The institutions benefit from expanded mandate (crisis management powers), while member states bear costs through constrained fiscal autonomy. Active enforcement required — the Stability and Growth Pact, fiscal compact, and troika conditionality are sustained through institutional will, not market forces.
constraint_indexing:constraint_classification(european_fiscal_union_feasibility, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: STABILITY AND GROWTH PACT MECHANISM (PITON) — The Pact's 3% deficit rule and 60% debt ceiling are substantially performative. Large economies (France, Germany, Italy) have repeatedly violated rules without enforcement. Small economies face strict enforcement. The rule mechanism persists through institutional inertia and political theater (compliance narratives) despite low functional enforcement. Theater ratio (0.65) reflects that most EU fiscal governance discussions involve recalibrating rules rather than enforcing them against structural violators. The mechanism is degraded — maintained for symbolic commitment to fiscal discipline rather than actual constraint on deficits.
constraint_indexing:constraint_classification(european_fiscal_union_feasibility, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: FISCAL UNION REFORMERS (SCAFFOLD) — Organized actors (progressive EU coalitions, some southern governments) view current arrangements as a temporary coordination failure with a structural exit: deeper fiscal union (eurobonds, tax harmonization, shared unemployment insurance, fiscal transfers) would replace the extraction mechanism with genuine coordination. The sunset logic: if fiscal union reforms succeed (high suppression → low extraction), the current constraint dissolves; if reforms fail, the constraint persists (high suppression → high extraction). Organized actors experience the constraint as a solvable coordination problem with a clear sunset condition — though the probability of reform is contested.
constraint_indexing:constraint_classification(european_fiscal_union_feasibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — MACROECONOMIC LIMITS VIEW (MOUNTAIN) — From a civilizational perspective, the eurozone constraint appears to reflect immutable macroeconomic realities: monetary union without fiscal union creates inevitable imbalances (trade deficits, current account divergence) that no institution can fully resolve without causing economic damage. The constraint is naturalized as a structural limit on what monetary integration can achieve without fiscal union. However, this perspective risks false summitry — what appears as natural economic law is actually a political choice (unified currency without fiscal transfer mechanisms). The structural data contradicts the mountain classification.
constraint_indexing:constraint_classification(european_fiscal_union_feasibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(european_fiscal_union_feasibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(european_fiscal_union_feasibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(european_fiscal_union_feasibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(european_fiscal_union_feasibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(european_fiscal_union_feasibility, TR),
    TR >= 0.70.

:- end_tests(european_fiscal_union_feasibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts through multiple mechanisms: (1) austerity mandates imposed as troika conditionality, (2) sovereign spread volatility that increases peripheral borrowing costs, (3) ECB collateral policy that discriminates against peripheral securities, (4) labor market deregulation requirements embedded in fiscal adjustment programs, (5) capital flight during crisis periods that forces fire sales of assets at depressed prices. The extractiveness increased sharply during 2010–2015 (peak 0.72) as conditionality became more severe. Post-2015 extractiveness declined slightly (0.58 in 2024) due to ECB asset purchases lowering spreads and ECB willingness to relax collateral rules during crisis. However, extractiveness remains elevated because underlying structural imbalances (divergent productivity, savings preferences, demographic trajectories) persist. Suppression (0.68): High. Peripheral states face substantial barriers to exit: (1) legal eurozone membership constraints, (2) ECB financing dependence for banks and governments, (3) catastrophic costs of euro exit (capital flight, banking collapse, currency depreciation), (4) capital account integration making independent monetary policy impossible even with withdrawal, (5) political pressure from creditor states and EU institutions, (6) fear of market contagion effects of one country's exit. Suppression decreased slightly post-2015 as ECB backstop reduced financing panic, but structural barriers remain. Theater ratio (0.65): Moderate-high and increasing. The Stability and Growth Pact's 3% deficit ceiling and 60% debt ceiling are enforced selectively: France, Germany, Italy have violated rules repeatedly without enforcement; smaller peripheral states face strict enforcement. This selective enforcement reveals the mechanism as substantially performative — the rules serve as commitment devices and negotiating frameworks rather than binding constraints. The theater ratio increased from 0.45 (1999) to 0.71 (2012) as gap widened between nominal rules and actual enforcement, then declined to 0.65 by 2024 as some enforcement consistency improved under revised fiscal compact. However, persistent gap between stated rules and actual application indicates ongoing theater.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between creditor and debtor states represents maximal DR diagnostic value. The constraint generates opposing classifications from structurally opposed positions: creditor states see Rope (pure coordination), peripheral states see Snare (pure extraction). This gap is not a measurement error or observer bias — it reflects the true asymmetric distribution of extraction within the constraint. The gap reveals that the constraint has not converged to an equilibrium where all parties perceive mutual benefit (Rope from all perspectives). Instead, the constraint persists through asymmetric power: creditor states benefit enough to maintain it, peripheral states are trapped enough that exit is worse than compliance. The scaffold perspective (fiscal union reformers) represents an escape path from this gap: if fiscal union were implemented (eurobonds, progressive redistribution, shared unemployment insurance), the constraint would shift from snare-to-periphery / rope-to-core toward genuine rope (coordination with mutual benefit) from all perspectives. The fact that this reform path exists but remains blocked reveals the true nature of the constraint: it is a tangled rope maintained by creditor power precisely because dissolution into pure fiscal union would eliminate the asymmetric extraction. The creditor states benefit from the hybrid (they get coordination + extraction), so they have little incentive to reform toward pure coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint flows from power asymmetries in the eurozone hierarchy. Creditor states occupy low-d positions (high benefit, low cost, high exit optionality) — they can arbitrage between eurozone participation and alternatives, and the constraint distributes resources toward them. Peripheral states during crises occupy high-d positions (low benefit, high cost, low exit optionality) — they are trapped within the constraint and extraction flows from them toward creditors. The sigmoid function f(d) amplifies this asymmetry: as d increases (victim status), experienced extractiveness χ increases through the sigmoid multiplier. A peripheral state with d=0.90 (near-total victim status, trapped exit) experiences χ = 0.58 × f(0.90) × σ(regional) ≈ 0.58 × 1.36 × 0.9 ≈ 0.71 — very high extraction experienced. A creditor state with d=0.10 (beneficiary status, arbitrage exit) experiences χ = 0.58 × f(0.10) × σ(continental) ≈ 0.58 × 0.05 × 1.0 ≈ 0.03 — minimal extraction experienced, essentially a coordination mechanism from their perspective. The directionality derivation captures the structural reality: the same constraint feels like pure extraction to the trapped peripheral state and like pure coordination to the creditor state with exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The constraint does not collapse into false natural law (mountain) because the structural data contradicts naturalization. The claim that 'monetary union without fiscal union is inherently unstable' is falsified by counterfactuals: the eurozone could be stabilized through fiscal union (eurobonds, progressive transfers), or it could be destabilized by deliberate institutional design choices (austerity mandates, collateral discrimination). The constraint is not a mountain because its existence depends on active enforcement (troika conditionality, ECB collateral policy, Commission fiscal surveillance) — if enforcement were withdrawn, peripheral states would default and exit. The constraint does collapse into extractive snare for peripheral states (χ ≥ 0.66 from their perspective) because suppression exceeds 0.60 and active enforcement maintains the mechanism. However, it does NOT collapse into pure snare from the analytical observer perspective because: (1) genuine coordination functions exist (cross-border capital flows, monetary stability, political integration), (2) organized reform pathways exist (fiscal union with sunset logic for current constraints), (3) the institutional structure is contingent (could be redesigned, has been reformed multiple times). Therefore, the constraint is analytically best characterized as Tangled Rope at the system level, with Snare as the experienced classification from trapped peripheral perspectives and Rope as the experienced classification from beneficiary creditor perspectives. The mandatrophy is resolved by recognizing that the 'true' type is the ensemble of perspectival types, not a single privileged observation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_union_political_feasibility,
    'Is deeper fiscal union politically achievable within the eurozone given heterogeneous preferences over redistribution, or is the current constraint structurally permanent?',
    'Longitudinal tracking of fiscal union proposals (eurobonds, progressive tax harmonization, shared unemployment insurance); electoral outcomes in peripheral vs creditor states; crisis dynamics that either enable or block reform windows',
    'If fiscal union is achievable: scaffold classification is correct, constraint has genuine sunset. If politically infeasible: constraint is effectively snare for periphery, rope for core, with indefinite duration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_union_political_feasibility, preference, 'Political feasibility of fiscal union reforms that would dissolve the current extraction mechanism').

omega_variable(
    monetary_transmission_asymmetry_source,
    'Do observed macroeconomic imbalances in the eurozone reflect structural real differences (productivity, savings preferences, demographic divergence) or monetary transmission asymmetries created by the institutional design?',
    'Counterfactual modeling: hypothetical eurozone with fiscal union vs. current design; cross-national regression analysis of macro outcomes controlling for structural vs institutional variables; comparison with other monetary unions (US states, Australian states)',
    'If structural: imbalances are inevitable (mountain-like constraint). If institutional: imbalances are contingent policy choices, revealing the constraint as tangled rope rather than natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_transmission_asymmetry_source, empirical, 'Whether eurozone imbalances reflect structural or institutional causes').

omega_variable(
    conditionality_enforcement_selectivity,
    'Why is the Stability and Growth Pact selectively enforced against small/peripheral economies but not against large/core economies? Is enforcement driven by technical fiscal considerations or by power asymmetries embedded in the constraint?',
    'Historical analysis of Pact violations and enforcement actions across member states; regression analysis of enforcement probability on state size, creditor status, and veto power in EU institutions; interview data from Commission enforcement officials',
    'If technical: enforcement reflects actual fiscal risk differences (exogenous). If power-driven: the piton classification is correct — the Pact is theater maintained by core power, not by coordination logic. This determines whether the constraint is extractive (snare/tangled rope) or merely performative (piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditionality_enforcement_selectivity, empirical, 'Sources of selective enforcement in Stability and Growth Pact').

omega_variable(
    ecb_monetary_transmission_design,
    'Does ECB monetary policy transmission asymmetrically favor creditor states through lower borrowing costs and capital inflows while penalizing peripheral states through collateral requirements and sovereign spread volatility?',
    'Time-series analysis of bond spreads, asset purchase program composition, collateral haircuts, and lending facility usage across member states; causal analysis of ECB policy shocks on periphery vs core economies; comparison with Federal Reserve transmission during US regional crises',
    'If asymmetric transmission is institutional design: ECB policy is part of extraction mechanism. If symmetric in intent but asymmetric in effect: the constraint reflects real economic divergence that ECB cannot overcome. This affects classification between snare (institutional design) and mountain (economic law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecb_monetary_transmission_design, empirical, 'Asymmetry in ECB monetary transmission across core and peripheral member states').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(european_fiscal_union_feasibility, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eufu_theater_1999, european_fiscal_union_feasibility, theater_ratio, 0, 0.45).
narrative_ontology:measurement(eufu_theater_2007, european_fiscal_union_feasibility, theater_ratio, 8, 0.52).
narrative_ontology:measurement(eufu_theater_2012, european_fiscal_union_feasibility, theater_ratio, 13, 0.71).
narrative_ontology:measurement(eufu_theater_2024, european_fiscal_union_feasibility, theater_ratio, 25, 0.65).

% Extraction over time
narrative_ontology:measurement(eufu_extractiveness_1999, european_fiscal_union_feasibility, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(eufu_extractiveness_2007, european_fiscal_union_feasibility, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(eufu_extractiveness_2012, european_fiscal_union_feasibility, base_extractiveness, 13, 0.72).
narrative_ontology:measurement(eufu_extractiveness_2024, european_fiscal_union_feasibility, base_extractiveness, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(european_fiscal_union_feasibility, resource_allocation).
narrative_ontology:affects_constraint(european_fiscal_union_feasibility, sovereign_debt_asymmetry).
narrative_ontology:affects_constraint(european_fiscal_union_feasibility, monetary_policy_transmission_eurozone).
narrative_ontology:affects_constraint(european_fiscal_union_feasibility, labor_market_flexibility_mandates).

% DUAL FORMULATION NOTE:
% The European fiscal union constraint overlaps with but is structurally distinct from: (1) sovereign debt dynamics (the contractual relationships between states and creditors), (2) monetary policy transmission (the ECB's distributional effects across member states), and (3) labor market deregulation (the conditionality requirements for peripheral states). Each of these has its own extractiveness value and classification. The fiscal union constraint is the overarching coordination-extraction mechanism linking them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(european_fiscal_union_feasibility, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
