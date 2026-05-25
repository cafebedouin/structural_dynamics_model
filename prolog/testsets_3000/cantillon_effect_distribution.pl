% ============================================================================
% CONSTRAINT STORY: cantillon_effect_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cantillon_effect_distribution, []).

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
 *   constraint_id: cantillon_effect_distribution
 *   human_readable: Cantillon Effect Distribution: Asymmetric Access to New Money Creation
 *   domain: monetary_economics/financial_systems
 *
 * SUMMARY:
 *   The Cantillon Effect describes the asymmetric distributional consequence
 *   of new money creation: agents positioned closest to the money creation
 *   point (central banks, primary dealers, large financial institutions,
 *   governments) receive newly created money before inflation cascades
 *   outward and erodes purchasing power for everyone else. This creates a
 *   systematic wealth transfer from savers and wage earners to asset holders
 *   and financial institutions, independent of productivity or merit. The
 *   constraint exhibits the full Deferential Realism spectrum because it
 *   genuinely coordinates monetary function (preventing deflationary
 *   collapse, enabling investment) while structurally embedding asymmetric
 *   extraction (first-mover advantage in newly created purchasing power). The
 *   classification as tangled rope reflects this dual character: the
 *   coordination function is real and necessary; the extraction is also real
 *   and unnecessary but deeply embedded in how modern central banking
 *   operates.
 *
 * KEY AGENTS:
 *   - Wage Earners and Fixed-Income Populations: Primary victims (powerless/trapped) — last to receive new money, first to experience inflation; locked into system by employment and currency denomination
 *   - Savers with Fixed Nominal Returns: Primary victims (powerless/trapped) — systematic purchasing power erosion through currency depreciation; no exit short of liquidation
 *   - Central Banks and Primary Dealers: Primary beneficiaries (institutional/arbitrage) — receive new money at creation point; can arbitrage across systems; perceive constraint as pure coordination
 *   - Financial System: Institutional beneficiary (institutional/arbitrage) — cascades money creation through asset markets before reaching wage economy; captures spread between creation and distribution
 *   - Asset Holders: Secondary beneficiaries (moderate to powerful/mobile) — benefit from asset price inflation driven by new liquidity; can exit into alternative assets
 *   - Small Business: Mixed role (moderate/constrained) — benefits from demand stimulus but faces input inflation and wage rigidity
 *   - Regulatory Establishment: Institutional actor (institutional/arbitrage) — maintains theoretical framework that obscures distributional effects; benefits from Cantillon extraction while denying its existence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cantillon_effect_distribution, 0.58).
domain_priors:suppression_score(cantillon_effect_distribution, 0.65).
domain_priors:theater_ratio(cantillon_effect_distribution, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cantillon_effect_distribution, extractiveness, 0.58).
narrative_ontology:constraint_metric(cantillon_effect_distribution, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cantillon_effect_distribution, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cantillon_effect_distribution, tangled_rope).
narrative_ontology:human_readable(cantillon_effect_distribution, "Cantillon Effect Distribution: Asymmetric Access to New Money Creation").
narrative_ontology:topic_domain(cantillon_effect_distribution, "monetary_economics/financial_systems").

domain_priors:requires_active_enforcement(cantillon_effect_distribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cantillon_effect_distribution, primary_money_recipients).
narrative_ontology:constraint_beneficiary(cantillon_effect_distribution, financial_institutions).
narrative_ontology:constraint_beneficiary(cantillon_effect_distribution, asset_holders).
narrative_ontology:constraint_victim(cantillon_effect_distribution, wage_earners).
narrative_ontology:constraint_victim(cantillon_effect_distribution, savers_with_fixed_income).
narrative_ontology:constraint_victim(cantillon_effect_distribution, price_adjusting_lag_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIXED-INCOME WAGE EARNER (SNARE) — Structurally trapped within the monetary system. Receives newly created money only after it has cascaded through financial institutions and asset markets, experiencing maximum price inflation impact before income adjustment. No meaningful exit from this exposure — employment requires accepting wages denominated in the depreciating medium. Full extraction without coordination benefit.
constraint_indexing:constraint_classification(cantillon_effect_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAVERS WITH FIXED NOMINAL RETURNS (SNARE) — Structurally trapped by savings contracts denominated in depreciating currency. New money creation systematically transfers their accumulated purchasing power to asset holders without mechanism for recourse. No exit option except liquidating savings into inflation-exposed consumption. Pure extraction with no coordination function.
constraint_indexing:constraint_classification(cantillon_effect_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANK AND PRIMARY RECIPIENTS (ROPE) — Perceives new money creation as coordination mechanism: directing capital to investment, stabilizing financial system, enabling economic function. Primary recipients (banks, government, large corporations) receive fresh liquidity at creation moment before inflation cascades outward. Experiences constraint as beneficial coordination with arbitrage access to other monetary systems if needed.
constraint_indexing:constraint_classification(cantillon_effect_distribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SMALL BUSINESS AND PRODUCTIVE WAGE EARNERS (TANGLED ROPE) — Receive new money after financial sector but before price stabilization; experience mixed coordination and extraction. Benefit from increased nominal demand for goods and services (coordination element) but face inflation in inputs and wage rigidity (extraction element). Constrained by employment lock and business debt, but not trapped — can partially hedge through pricing or relocation if sufficiently organized.
constraint_indexing:constraint_classification(cantillon_effect_distribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY ESTABLISHMENT (PITON) — Officially maintains that monetary policy operates through symmetric aggregate effects; publishes models treating all agents as identical. Reality shows cascading distributional effects and first-mover advantages. Regulatory theater persists through institutional inertia despite extensive academic documentation of Cantillon effects. Maintains equipoise with published data while structurally benefiting from asymmetric distribution. Theater ratio high due to theoretical framework that obscures actual mechanism.
constraint_indexing:constraint_classification(cantillon_effect_distribution, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, new money creation serves genuine coordination function (enabling exchange, facilitating investment, preventing deflationary collapse) but structurally embeds extractive distribution determined by proximity to money creation point. The constraint is not eliminable without redesigning monetary plumbing, yet current architecture is contingent (chosen, not inherent). Classification as tangled rope reflects genuine coordination need alongside unavoidable extraction asymmetry.
constraint_indexing:constraint_classification(cantillon_effect_distribution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cantillon_effect_distribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cantillon_effect_distribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cantillon_effect_distribution, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cantillon_effect_distribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cantillon_effect_distribution, TR),
    TR >= 0.70.

:- end_tests(cantillon_effect_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint systematically transfers purchasing power from wage earners and savers to asset holders and financial institutions. The magnitude is neither maximal (like predatory lending at 0.72) nor minimal (like coordination overhead at 0.15) because the transfer is real but partially offset by genuine monetary coordination benefits and the heterogeneity of agents experiencing different phases of the distribution cascade. The measurement trajectory (0.35 → 0.48 → 0.58) shows accumulation over the interval, reflecting increasing recognition and documentation of Cantillon effects in economic literature. Suppression (0.65): High. The suppression mechanisms include: (1) Theoretical suppression — standard macroeconomic models treat monetary expansion as having symmetric aggregate effects, obscuring distributional analysis; (2) Institutional suppression — central banks and finance ministries publish data on aggregate inflation but rarely break out Cantillon-style distributional timelines; (3) Cognitive suppression — the cascade is distributed across years and sectors, making causal chains psychologically illegible to most wage earners; (4) Exit suppression — no meaningful alternative to holding currency or employment; (5) Narrative suppression — 'inflation is everyone's problem' obscures that timing and asset composition determine winners and losers. Theater ratio (0.48): Moderate. Central bank communications emphasize aggregate economic management and price stability targets while remaining silent on distribution mechanisms. However, the actual coordination work of monetary policy (preventing deflationary spirals, enabling credit) is substantially real, not purely theatrical — hence theater ratio is moderate rather than high. Academic economics increasingly documents Cantillon effects, reducing the theater component.
 *
 * PERSPECTIVAL GAP:
 *   The Cantillon Effect reveals a structural tension between aggregate monetary coordination and distributional extraction. From the perspective of central banks and primary recipients, new money creation is pure coordination — it prevents deflationary collapse and enables investment. From the perspective of wage earners and savers, the same process is pure extraction — they bear the purchasing power cost without receiving the creation benefit. This gap is not resolvable through a single 'true' classification because both perspectives are accurate within their structural contexts. The tangled rope classification reflects this: the constraint simultaneously solves a genuine coordination problem (preventing money shortage) and embeds a systematic extraction mechanism (cascading distribution favoring proximity to creation). The analytical observer's tangled rope classification emphasizes that the extraction is structurally avoidable — equal-distribution alternatives exist in principle, making this a policy design problem, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position in the money creation cascade and their substitution options. Central banks and primary money recipients occupy d ≈ 0.0-0.15 positions: they are full beneficiaries (receive money at creation) with arbitrage options (can operate across jurisdictions). Wage earners in non-financial sectors occupy d ≈ 0.85-0.95: they are full targets (receive money only after inflation cascades) with trapped exit options. Savers with fixed returns occupy d ≈ 0.90-1.00: pure targets with systematically eroding purchasing power and no realistic exit. Small business and productive sectors occupy d ≈ 0.55-0.70: they receive new money after financial sector but before complete price adjustment, receiving some benefit before the full extraction hits. Regulatory institutions benefit from the arrangement (low d through arbitrage options on policy knowledge) but officially deny the mechanism exists. The directional asymmetry is the quantitative expression of the Cantillon insight: distance from money creation point determines the direction and magnitude of the wealth transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint performs two functions simultaneously: (1) Genuine coordination — new money creation prevents deflationary collapse and enables efficient capital allocation, a necessary function; (2) Systematic extraction — the mechanics of how new money enters the system create first-mover advantages that systematically favor asset holders over wage earners. These are not conflicting classifications; they are dual aspects of the same structural arrangement. The tangled rope type captures this duality: the constraint cannot be classified as pure rope (coordination only) because the extraction is real and systematic, not noise. It cannot be classified as pure snare (extraction only) because the monetary coordination function is genuine, not theatrical. The mandate is resolved by showing that both aspects are accurate from their respective structural positions, and the disagreement reflects real asymmetries in the system, not observational error. The constraint would become pure rope only if new money were distributed equally to all agents (helicopter money) — the coordination function would remain, but the extraction would disappear. This shows that extraction is contingent on the distribution mechanism, not necessary for monetary function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_measurement_basis,
    'What constitutes ''inflation'' for assessing when different agents experience price impact? CPI, asset-price inflation, or sector-specific deflation?',
    'Empirical tracking of price changes across consumption baskets (wage earner vs asset holder) and timeline analysis of when price adjustments occur in different sectors following monetary expansion',
    'If CPI properly captures all agents'' experience: extraction may be overstated. If asset-price inflation is excluded from standard measures: extraction is understated. Determines whether measured extractiveness of 0.58 should shift toward 0.42 or 0.68.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_measurement_basis, empirical, 'Measurement methodology for inflation experience across agent types').

omega_variable(
    wage_adjustment_lag_mechanism,
    'Is the lag between money creation and wage adjustment a structural feature of labor markets or an extractive lock-in mechanism maintained by institutional suppression?',
    'Comparative analysis of labor negotiation power and wage-setting mechanisms across periods of high vs low monetary expansion; study of jurisdictions with more frequent wage indexation vs implicit annual review',
    'If structural: wages eventually adjust and extraction is temporary. If institutional suppression: adjustment lag is deliberately extended and extraction is systematic. Determines whether classification shifts from tangled rope to pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_adjustment_lag_mechanism, empirical, 'Nature of wage adjustment lag in response to monetary expansion').

omega_variable(
    substitution_capacity_for_savers,
    'Can savers meaningfully shift to alternative stores of value (commodities, foreign currency, crypto-assets) sufficient to constitute an exit option from currency depreciation?',
    'Empirical analysis of substitution barriers (transaction costs, regulatory restrictions, counterparty risk, volatility), measurement of actual substitution rates during high-inflation periods, accessibility by income level',
    'If substitution is available and low-cost: exit options shift from trapped to constrained or mobile, reducing effective extraction. If substitution is blocked or unaffordable: trapped classification confirmed and extraction remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_capacity_for_savers, empirical, 'Availability of meaningful exit options for savers through alternative value stores').

omega_variable(
    distribution_intentionality,
    'Is the Cantillon asymmetry a necessary technical feature of modern monetary systems or a policy choice that could be restructured?',
    'Analysis of alternative monetary architectures (equal-distribution helicopter money, universal basic income with reserve-backed currency, distributed ledger alternatives), empirical testing of whether asymmetric distribution is inevitable or contingent',
    'If necessary: constraint is closer to mountain (structural limit of any money creation system). If contingent: constraint is squarely tangled rope — genuine coordination with structurally avoidable extraction. Determines whether reform is realistic or futile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distribution_intentionality, conceptual, 'Whether asymmetric distribution is inherent to monetary systems or contingently designed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cantillon_effect_distribution, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cantillon_tr_t0, cantillon_effect_distribution, theater_ratio, 0, 0.32).
narrative_ontology:measurement(cantillon_tr_t3, cantillon_effect_distribution, theater_ratio, 3, 0.4).
narrative_ontology:measurement(cantillon_tr_t6, cantillon_effect_distribution, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(cantillon_be_t0, cantillon_effect_distribution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cantillon_be_t3, cantillon_effect_distribution, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(cantillon_be_t6, cantillon_effect_distribution, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cantillon_effect_distribution, resource_allocation).
narrative_ontology:affects_constraint(cantillon_effect_distribution, financial_asset_inflation).
narrative_ontology:affects_constraint(cantillon_effect_distribution, wage_lag_persistence).
narrative_ontology:affects_constraint(cantillon_effect_distribution, wealth_inequality_dynamics).

% DUAL FORMULATION NOTE:
% The Cantillon Effect decomposes into three distinct constraints with different ε values: (1) monetary_creation_coordination (ε ≈ 0.15, Rope) — the actual coordination function of new money creation; (2) asymmetric_distribution_mechanism (ε ≈ 0.68, Snare) — the specific institutional choice to cascade money through finance sector first; (3) cantillon_effect_distribution (ε ≈ 0.58, Tangled Rope) — the combined system as currently implemented. This story addresses the combined system. Upstream: monetary_creation_coordination. Downstream: wage_lag_persistence, financial_asset_inflation, wealth_inequality_dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cantillon_effect_distribution, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
