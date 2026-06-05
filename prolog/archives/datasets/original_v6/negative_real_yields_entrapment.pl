% ============================================================================
% CONSTRAINT STORY: negative_real_yields_entrapment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_negative_real_yields_entrapment, []).

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
 *   constraint_id: negative_real_yields_entrapment
 *   human_readable: Negative Real Yields Entrapment in Monetary Policy Regime
 *   domain: macroeconomics/monetary_policy
 *
 * SUMMARY:
 *   Negative real yields occur when nominal interest rates fall below the
 *   inflation rate, eroding the purchasing power of savings in real terms.
 *   This constraint is fundamental to monetary policy regimes that use
 *   inflation to reduce sovereign debt burdens and subsidize leveraged
 *   borrowers. The constraint exhibits a complex perspectival structure:
 *   beneficiaries (leveraged borrowers, fiscal authorities, financial sector)
 *   experience coordination benefits alongside extraction mechanisms; victims
 *   (savers, fixed-income recipients, currency holders) experience pure
 *   extraction with suppression through capital controls and inflation
 *   expectations management. The negative real yields regime requires
 *   continuous enforcement through central bank policy, inflation targeting,
 *   and financial repression mechanisms. It creates a tangled
 *   coordination-extraction hybrid where the coordination function
 *   (sustainable sovereign finance, financial system stability) coexists with
 *   asymmetric distribution of costs. The measurement trajectory shows
 *   extractiveness rising from 0.32 to 0.58 over the interval, reflecting
 *   accumulating wealth transfer and deepening entrenchment. Theater ratio
 *   remains moderate (0.48) because while monetary policy communication
 *   contains performative elements, the real mechanism (inflation and
 *   financial repression) is structurally straightforward — unlike the
 *   verification bottleneck, which is highly theatrical, the negative real
 *   yields regime operates through transparent economic forces that are
 *   theoretically well-understood.
 *
 * KEY AGENTS:
 *   - Savers (Powerless/Trapped): Primary victims bearing extraction through real wealth erosion with no exit option
 *   - Fixed-Income Recipients (Moderate/Constrained): Pensioners and annuity holders experiencing real value erosion with high exit barriers
 *   - Leveraged Borrowers (Institutional/Arbitrage): Primary beneficiaries capturing debt relief and asset appreciation
 *   - Financial Sector (Organized/Arbitrage): Secondary beneficiary amplifying extraction through credit expansion and asset market activity
 *   - Fiscal Authorities (Institutional/Constrained): Beneficiary with coordination function (sustainable debt service) requiring suppression enforcement
 *   - Currency Community (Powerless/Trapped): Holders of monetary base experiencing systematic depreciation relative to real resources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(negative_real_yields_entrapment, 0.58).
domain_priors:suppression_score(negative_real_yields_entrapment, 0.62).
domain_priors:theater_ratio(negative_real_yields_entrapment, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(negative_real_yields_entrapment, extractiveness, 0.58).
narrative_ontology:constraint_metric(negative_real_yields_entrapment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(negative_real_yields_entrapment, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(negative_real_yields_entrapment, tangled_rope).
narrative_ontology:human_readable(negative_real_yields_entrapment, "Negative Real Yields Entrapment in Monetary Policy Regime").
narrative_ontology:topic_domain(negative_real_yields_entrapment, "macroeconomics/monetary_policy").

domain_priors:requires_active_enforcement(negative_real_yields_entrapment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(negative_real_yields_entrapment, leveraged_borrowers).
narrative_ontology:constraint_beneficiary(negative_real_yields_entrapment, fiscal_authorities).
narrative_ontology:constraint_beneficiary(negative_real_yields_entrapment, financial_sector).
narrative_ontology:constraint_victim(negative_real_yields_entrapment, savers).
narrative_ontology:constraint_victim(negative_real_yields_entrapment, fixed_income_recipients).
narrative_ontology:constraint_victim(negative_real_yields_entrapment, currency_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SAVER UNDER MONETARY REPRESSION (SNARE) — Trapped by denominated savings in currency bearing negative real yields. Cannot exit without incurring transaction costs, currency risk, or opportunity loss. Bears full cost of wealth erosion through inflation. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(negative_real_yields_entrapment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIXED-INCOME PENSIONER (TANGLED ROPE) — Receives coordination benefit from stable payment streams (pension indexation, annuity guarantees) but experiences extraction through real value erosion. Constrained by limited alternative income sources and high cost of changing pension arrangements. Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(negative_real_yields_entrapment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEVERAGED BORROWER (ROPE) — Experiences negative real yields as coordination benefit: debt repayment in currency worth less than borrowed. Benefits from financial deepening and asset appreciation. Can arbitrage across markets. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(negative_real_yields_entrapment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL SECTOR (ROPE) — Coordinates credit flows and asset allocation under regime. Benefits from negative real rates through asset price inflation, increased leverage capacity, and spreads. Organized with arbitrage options across jurisdictions. Low net extraction.
constraint_indexing:constraint_classification(negative_real_yields_entrapment, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FISCAL AUTHORITY (TANGLED ROPE) — Benefits from negative real rates reducing debt service burden (coordination function: sustainable sovereign finance). But requires continuous enforcement of low-rate regime through central bank intervention and capital controls (suppression). Constrained by inflation expectations and political economy constraints on fiscal consolidation.
constraint_indexing:constraint_classification(negative_real_yields_entrapment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: QUANTITY THEORY FRAMEWORK (PITON) — Conventional monetary framework (MV=PQ) historically explained price dynamics. Under negative real yields and zero lower bound, the framework became theater: money aggregates exploded while velocity collapsed, and price effects fragmented across asset classes and geographies. The theoretical framework persists in central bank communications despite degraded explanatory power. Theater ratio reflects that the framework is maintained for institutional legitimacy, not empirical coherence.
constraint_indexing:constraint_classification(negative_real_yields_entrapment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL CONSTRAINTS VIEW (MOUNTAIN) — From a civilizational perspective, negative real yields in a fiat currency system reflect a fundamental constraint: real resources are scarce, and no monetary regime can permanently repudiate claims on those resources. The negative real yield regime merely defers the adjustment — pushing it forward in time or across populations. This perspective sees the constraint as an immutable feature of monetary scarcity that cannot be escaped through policy, only postponed. However, the structural data reveals this as naturalization: the negative real yield regime requires continuous enforcement, benefits specific agents, and reflects institutional choices, not physical law.
constraint_indexing:constraint_classification(negative_real_yields_entrapment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(negative_real_yields_entrapment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(negative_real_yields_entrapment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(negative_real_yields_entrapment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(negative_real_yields_entrapment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(negative_real_yields_entrapment, TR),
    TR >= 0.70.

:- end_tests(negative_real_yields_entrapment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial but not maximal. The regime extracts real wealth from savers to borrowers through inflation-driven wealth transfer. The value reflects the magnitude of real yield gaps (typically -2% to -4% annually in developed markets) aggregated across the saver population. This is not as severe as maximum snare extraction (0.85+) because the mechanism is transparent and some agents can partially exit through asset substitution. Suppression (0.62): Moderate-high. The regime requires active suppression through capital controls (restricting foreign currency access), inflation expectations management (forward guidance, credibility maintenance), financial regulation (restricting alternative store-of-value access), and occasionally direct capital controls. Suppression is not total because wealthy agents retain arbitrage options and partial exit capacity through real assets. Theater ratio (0.48): Below median. The negative real yields mechanism is structurally transparent: inflation reduces real value, financial repression restricts alternatives, capital controls enforce denominated savings. The theater element is primarily in central bank communication (claiming inflation is 'transitory,' 'temporary,' or 'manageable') rather than in the mechanism itself. Unlike peer review theater, which is entirely performative, the negative real yields constraint has both real mechanism and communicative theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates complete perspectival disagreement. Leveraged borrowers see a rope (coordination benefit: debt relief, asset appreciation, financial stability). Fiscal authorities see a tangled_rope (coordination function of sustainable debt service plus required enforcement). Savers see a snare (pure extraction with trapped exit options). Fixed-income recipients see tangled_rope (coordination of income stability with extraction of real value). Financial sector sees rope (coordination of credit allocation with beneficiary status). The piton perspective reveals that monetary policy frameworks (quantity theory, IS-LM) have degraded explanatory power under zero lower bound and negative real yields — the theater is the persistence of frameworks that no longer coherently predict price dynamics. The mountain perspective naturalizes the constraint as inherent monetary scarcity, but the structural data reveals this as a false summit: the regime requires continuous enforcement (central bank policy, capital controls, inflation management) and benefits specific agents, indicating it is contingent institutional arrangement rather than physical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values differ dramatically across agent types. Leveraged borrowers have low d (0.10-0.20) — they are net beneficiaries with arbitrage options — producing negative or near-zero f(d) values and minimal effective extraction from their perspective. Savers have high d (0.88-0.95) — they are trapped victims — producing maximum f(d) values (1.35+) and maximum experienced extraction. Fixed-income recipients have intermediate d (0.65-0.75) reflecting constrained exit and victim status. Fiscal authorities have low-to-moderate d (0.25-0.35) reflecting beneficiary status with constraints. The perspectival gaps in classification emerge from these divergent directionality values applied to the same base extractiveness metric. The engine computes chi = 0.58 × f(d) × σ(national=1.0), producing: savers experience χ ≈ 0.78 (snare range); borrowers experience χ ≈ -0.07 (rope range); fiscal authorities experience χ ≈ 0.38 (tangled_rope range). The same structural constraint produces divergent classifications because d is constraint-relative, not observer-absolute.
 *
 * MANDATROPHY ANALYSIS:
 *   The negative real yields constraint resolves mandatrophy through recognition that the classification type depends entirely on the agent's structural position (d-value) relative to the inflation mechanism. The question 'Is negative real yields a coordination mechanism or extraction?' has no single answer — it is both simultaneously. For borrowers, it is pure coordination (debt relief enables solvent spending). For savers, it is pure extraction (wealth erosion). For fiscal authorities, it is tangled_rope (coordination of debt sustainability with suppression enforcement). For the analytical observer, the temptation is to naturalize it as mountain (immutable monetary scarcity), but the structural requirement for continuous enforcement and selective agent benefit reveals it as contingent institutional arrangement. The constraint is not mislabeled at multiple types — it legitimately IS multiple types from different structural perspectives. The engine's job is not to resolve 'which type is true' but to compute the perspectival presheaf and reveal why single-position analysis fails. The mandatrophy is resolved by accepting that all six readings are structurally accurate observations from their respective positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_expectations_collapse,
    'At what inflation rate do expectations deanchor and produce discontinuous adjustment (stagflation or currency crisis)?',
    'Cross-country empirical analysis of inflation-expectation relationships; historical identification of threshold events; forward guidance effectiveness studies',
    'If threshold is near current inflation: regime is fragile and may transition abruptly to snare-dominant classification. If threshold is far: regime can persist longer, maintaining tangled_rope classification for fiscal authorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_expectations_collapse, empirical, 'Inflation expectations deanchor threshold in negative real yields regime').

omega_variable(
    capital_flight_boundary,
    'What level of negative real yields triggers mass capital flight and currency exit, and can the boundary be maintained through capital controls?',
    'Comparative analysis of cross-border capital flows during different yield regimes; effectiveness of capital controls in preventing exit; substitution between currency holdings and alternative stores of value',
    'If boundary is high (can sustain deep negative real yields): suppression metric may be overstated. If boundary is low: regime is more fragile and resembles snare more closely for trapped savers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_flight_boundary, empirical, 'Boundary conditions for capital flight and currency exit under negative real yields').

omega_variable(
    asset_price_sustainability,
    'How much of the asset price inflation in the negative real yields regime is coordination (lower discount rates, sustainable valuation) vs. bubble formation (unsustainable multiple expansion)?',
    'Cross-sectional analysis of asset valuation metrics relative to long-term earnings/dividends; forward return expectations under different rate regimes; historical comparison to prior asset bubbles',
    'If mostly coordination: beneficiary perspectives (borrowers, financial sector) are accurate. If mostly bubble: regime is extractive disguised as coordination, and actual suppression is higher (agents are trapped in inflating asset markets).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asset_price_sustainability, empirical, 'Asset price inflation sustainability in negative real yields regime').

omega_variable(
    exit_option_availability,
    'How accessible are real alternative stores of value (commodities, real estate, foreign currency, crypto) for different wealth levels?',
    'Survey data on portfolio diversification by wealth level; transaction cost analysis; regulatory barrier analysis for different asset classes and jurisdictions',
    'If alternatives are accessible: ''trapped'' classification for savers may be too severe (should be ''constrained''). If alternatives are restricted: suppression is even higher and regime is closer to pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_availability, empirical, 'Accessibility of real alternative stores of value for savers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(negative_real_yields_entrapment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nrye_tr_t0, negative_real_yields_entrapment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nrye_tr_t5, negative_real_yields_entrapment, theater_ratio, 5, 0.42).
narrative_ontology:measurement(nrye_tr_t10, negative_real_yields_entrapment, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(nrye_be_t0, negative_real_yields_entrapment, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(nrye_be_t5, negative_real_yields_entrapment, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(nrye_be_t10, negative_real_yields_entrapment, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(negative_real_yields_entrapment, resource_allocation).
narrative_ontology:affects_constraint(negative_real_yields_entrapment, sovereign_debt_sustainability).
narrative_ontology:affects_constraint(negative_real_yields_entrapment, pension_system_solvency).
narrative_ontology:affects_constraint(negative_real_yields_entrapment, currency_substitution_dynamics).
narrative_ontology:affects_constraint(negative_real_yields_entrapment, asset_bubble_formation).

% DUAL FORMULATION NOTE:
% Negative real yields entrapment is a high-level constraint affecting multiple downstream policy mechanisms. Sovereign debt sustainability (ε≈0.35, Rope) and pension solvency (ε≈0.52, Tangled Rope) are decomposed downstream constraints that inherit the suppression and beneficiary structure. Currency substitution (ε≈0.64, Snare) represents the partial-exit mechanism for savers attempting to escape denomination constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(negative_real_yields_entrapment, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
