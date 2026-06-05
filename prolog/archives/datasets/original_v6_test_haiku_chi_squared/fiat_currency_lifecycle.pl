% ============================================================================
% CONSTRAINT STORY: fiat_currency_lifecycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fiat_currency_lifecycle, []).

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
 *   constraint_id: fiat_currency_lifecycle
 *   human_readable: The Lifecycle of a Fiat Currency
 *   domain: economic/political
 *
 * SUMMARY:
 *   The lifecycle of a fiat currency exhibits a characteristic temporal
 *   progression from effective coordination mechanism (early stage: stable
 *   purchasing power, enabled government spending, facilitated trade) to
 *   increasingly extractive snare (late stage: predictable depreciation,
 *   financial repression of savers and creditors, theater-heavy central bank
 *   ritual). A fiat system begins with credible commitment: the monetary
 *   authority promises price stability and establishes expectations. Savers
 *   willingly hold currency; creditors lend at low real rates; the system
 *   coordinates economic activity. Over decades, fiscal pressures, political
 *   constraints, and demographic shifts accumulate. The monetary authority
 *   faces a choice: either impose real losses (austerity, default, currency
 *   appreciation) or gradually inflate. Most fiat systems choose inflation,
 *   which redistributes wealth from savers, fixed-income earners, and
 *   late-stage creditors to the monetary authority and early beneficiaries
 *   who can anticipate and arbitrage the depreciation. The theater increases
 *   as central banks develop increasingly sophisticated communication
 *   strategies (forward guidance, credibility signals, inflation targeting
 *   frameworks) to maintain expectations despite policy choices that
 *   undermine them. The constraint exemplifies how institutions degrade from
 *   genuine coordination to performative extraction, classified from
 *   different perspectives as rope, snare, piton, scaffold, and false-summit
 *   mountain depending on the observer's structural relationship and time
 *   horizon.
 *
 * KEY AGENTS:
 *   - Monetary Authority: Primary beneficiary (institutional/arbitrage) — captures seigniorage; controls currency creation; early perspective sees coordination (Rope)
 *   - Currency Savers: Primary victim (powerless/trapped) — cannot exit; bears full extraction cost as purchasing power erodes
 *   - Fixed-Income Earners: Victim (moderate/constrained) — wages/pensions in nominal terms; purchasing power declines predictably
 *   - Banking System: Mixed (institutional/constrained) — benefits from seigniorage distribution and liquidity access; also extracts through intermediation
 *   - Late-Stage Creditors: Victim (powerful/constrained) — receive repayment in depreciated units; subject to financial repression
 *   - Alternative Currency Coalition: Organized agents (organized/mobile) — cryptocurrency, commodity-backed systems, multi-currency unions building exit pathways
 *   - Central Bank Ritual System: Institutional actor (institutional/arbitrage) — maintains performative communication and inflation-targeting theater; sees own process as degraded (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fiat_currency_lifecycle, 0.58).
domain_priors:suppression_score(fiat_currency_lifecycle, 0.65).
domain_priors:theater_ratio(fiat_currency_lifecycle, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiat_currency_lifecycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(fiat_currency_lifecycle, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fiat_currency_lifecycle, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fiat_currency_lifecycle, snare).
narrative_ontology:human_readable(fiat_currency_lifecycle, "The Lifecycle of a Fiat Currency").
narrative_ontology:topic_domain(fiat_currency_lifecycle, "economic/political").

domain_priors:requires_active_enforcement(fiat_currency_lifecycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fiat_currency_lifecycle, monetary_authority).
narrative_ontology:constraint_beneficiary(fiat_currency_lifecycle, early_adopters).
narrative_ontology:constraint_beneficiary(fiat_currency_lifecycle, incumbent_financial_institutions).
narrative_ontology:constraint_victim(fiat_currency_lifecycle, currency_savers).
narrative_ontology:constraint_victim(fiat_currency_lifecycle, fixed_income_earners).
narrative_ontology:constraint_victim(fiat_currency_lifecycle, late_stage_creditors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CURRENCY SAVER (SNARE) — Trapped in the fiat system; cannot exit without abandoning accumulated savings or incurring transaction costs to convert to alternatives. Bears full extraction cost as currency depreciates. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIXED-INCOME EARNER (SNARE) — Receiving wages or pensions in nominal terms; cannot negotiate real (inflation-adjusted) compensation without collective action. Purchasing power erodes predictably. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LATE-STAGE CREDITOR (SNARE) — Creditors who extended loans late in the currency lifecycle receive repayment in depreciated nominal units. Extraction via financial repression. d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.64.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MONETARY AUTHORITY (ROPE) — Primary beneficiary. Controls currency creation, captures seigniorage. Experiences constraint as coordination mechanism: enabling government spending and stabilizing expectations (when credible). d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07. Negative extraction = net benefit.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: BANKING SYSTEM (TANGLED ROPE) — Benefits from access to central bank liquidity and seigniorage distribution (coordination function). Also extracts through financial intermediation and inflation pass-through advantages. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.28.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FIAT CURRENCY RITUAL (PITON) — From civilizational view, the constraint persists through institutional inertia and ceremonial maintenance (central bank communications, GDP reporting, inflation targeting rhetoric). The functional purpose (enabling government spending, coordinating economic activity) has atrophied; the ritual persists because no better alternative has fully displaced it globally. theater_ratio=0.68 satisfies piton gate. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.10.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ALTERNATIVE CURRENCY COALITION (SCAFFOLD) — Cryptocurrency, commodity-backed currencies, and multi-national currency unions represent organized agents building exit pathways (Bitcoin, gold, EUR/SDR). The fiat constraint becomes a temporary coordination failure with a sunset: as alternatives mature and network effects shift, the fiat system's extraction mechanism loses force. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.25. Lower extraction because coalition perceives and is building an exit path.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, risks seeing fiat currency lifecycle as immutable law of economics: all currencies eventually debase, all governments face fiscal pressures, all monetary systems are extraction mechanisms. However, the structural data (ε=0.58, suppression=0.65, theater=0.68) contradicts mountain classification. The engine detects this as a false summit: the biological aging of fiat systems is not a natural law but a contingent institutional arrangement subject to policy choice.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiat_currency_lifecycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fiat_currency_lifecycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fiat_currency_lifecycle, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fiat_currency_lifecycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fiat_currency_lifecycle, TR),
    TR >= 0.70.

:- end_tests(fiat_currency_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts wealth from savers and creditors via predictable currency depreciation. However, it is not maximal extraction (>0.66) because fiat systems also provide real coordination benefits (enabling government spending, facilitating trade, stabilizing expectations) that are difficult to fully decompose from the extractive mechanism. The 0.58 value reflects that the system is primarily extraction-oriented but retains residual coordination functionality. Suppression (0.65): High. Significant barriers prevent exit: legal tender laws, deposit insurance tied to domestic currency, currency-denominated debt, capital controls, and social convention all constrain alternatives. Transaction costs and regulatory barriers suppress the emergence of competing currencies. Theater ratio (0.68): High. Central bank communications (inflation targeting, forward guidance, credibility signals) are increasingly theatrical: the rhetoric of price stability persists despite policies that prioritize output and employment, suggesting the theater has increased as functional credibility decayed. The ratio reflects that the central bank maintains legitimacy through communication rather than through achieving the stated objectives of stable prices.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates heterogeneous classification from a single structural data set. The monetary authority sees rope (coordination) because they are the primary beneficiary; the constraint enables government spending and stabilizes broad expectations. Currency savers see snare (pure extraction); they are powerless to exit and absorb all depreciation cost. Fixed-income earners see snare with lower severity (tangled rope transition); their real wages decline but labor income provides some inflation hedge. The banking system sees mixed coordination-extraction (tangled rope); they benefit from seigniorage distribution but also capture some of the extraction premium through intermediation margins. Late-stage creditors see snare (financial repression). The alternative currency coalition sees a temporary problem with a sunset (scaffold); Bitcoin, stablecoins, and decentralized finance represent real exit pathways that are maturing. The civilizational observer risks seeing natural law (mountain) — 'all currencies eventually debase' — but this naturalizes a contingent policy choice (inflation prioritization over price stability) that could be otherwise constrained by rules.
 *
 * DIRECTIONALITY LOGIC:
 *   Currency savers: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Fixed-income earners: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction; some wage-indexation escape routes. Late-stage creditors: Victim + constrained (powerful) → d≈0.75, f(d)≈1.10. Significant extraction via financial repression; but powerful agents may have inflation-hedging alternatives. Monetary authority: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary. Banking system: Mixed beneficiary/victim + constrained → d≈0.45, f(d)≈0.48. Moderate extraction; access to seigniorage distribution but also exposure to real rates and credit quality. Alternative currency coalition: Organized + mobile → d≈0.35, f(d)≈0.32. Low effective extraction because coalition has agency and genuine exit pathways emerging. Central bank ritual system (piton): Institutional + arbitrage → d≈0.05, f(d)≈-0.12. No extraction at the level of theater maintenance itself; piton classification comes from theater gate, not from directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The fiat currency lifecycle avoids false mandatrophy by distinguishing institutional phases. Early-stage fiat (first 10-20 years): Genuine rope from analytical perspective — credible commitment, stable expectations, real coordination benefits. Mid-stage (20-50 years): Transition to tangled rope — some coordination preserved but extraction mechanism activating. Late-stage (50+ years): Snare from most perspectives except the beneficiary (monetary authority) — extraction mechanism dominant, coordination function atrophied, theater elevated. The constraint's claim is Snare because the prompt frame is 'lifecycle' — modeling the full trajectory from inception to mature depreciation. If the constraint were restricted to early-stage fiat only, classification would be rope. If restricted to terminal inflation spirals, it would be pure snare. The Snare classification for the full lifecycle correctly captures that the extractive mechanism is the defining feature that endures: the constraint persists because depreciation is inevitable given institutional incentives, not because coordination is necessary. Alternative currencies (scaffold) represent organized escape routes, confirming that fiat extraction is not immutable but contingent on institutional choice. False summit risk: Naturalizing the lifecycle as 'inherent to economics' misses that alternative monetary regimes (gold standard, currency boards, multi-currency baskets) operate differently. The lifecycle is the lifecycle of a specific institutional choice, not of money itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_v_monetary_expansion,
    'Is currency depreciation primarily driven by monetary expansion policy or by real economic forces (supply shocks, demographic transitions, productivity growth)?',
    'Cross-national regression analysis of monetary base growth vs inflation; counterfactual simulation of alternative policy paths; comparative analysis of tight-monetary-policy regimes (Hong Kong peg, ECB) vs expansionary regimes',
    'If monetary expansion dominates: snare classification confirmed — extraction is deliberate policy choice. If real forces dominate: constraint is coordination problem (rope) — monetary expansion is endogenous response to structural conditions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_v_monetary_expansion, empirical, 'Whether inflation is driven by monetary policy choice or economic structure').

omega_variable(
    credibility_erosion_mechanism,
    'Does currency depreciation result from loss of credibility in the monetary authority''s commitment to price stability, or from fundamental fiscal unsustainability (debt-to-GDP spiral)?',
    'Central bank independence index analysis; historical episodes of successful disinflation (Volcker 1980s, ECB credibility); comparison with failed stabilization attempts; market expectation surveys',
    'If credibility-driven: snare can be escaped through credible commitment rules (constrained exit becomes mobile). If fiscal-driven: creditors have no exit regardless of credibility — snare is irreducible at the fiscal level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_erosion_mechanism, empirical, 'Whether currency erosion is credibility-driven or fiscal-driven').

omega_variable(
    alternative_currency_adoption_feasibility,
    'Can decentralized alternatives (cryptocurrency, commodity baskets, synthetic units) achieve sufficient network effects and institutional acceptance to constitute a genuine exit option for currency savers?',
    'Adoption curves of Bitcoin, stablecoins, and cross-border payment systems; regulatory capture patterns; transaction cost analysis vs fiat; volatility and store-of-value function comparison over 10+ year horizons',
    'If feasible: scaffold sunset is real — fiat extraction mechanism weakens as alternatives mature (mobile exit emerges). If infeasible: powerless savers remain trapped — snare is structural and long-term.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_currency_adoption_feasibility, empirical, 'Whether alternatives provide viable exit from fiat currency dependence').

omega_variable(
    seigniorage_distribution_equity,
    'Does seigniorage (the benefit of currency creation) actually flow to savers/taxpayers through lower debt burdens and public goods, or is it captured by early receivers and financial institutions through inflation pass-through asymmetries?',
    'Distributional analysis of inflation incidence by income quintile; timing of wage/price adjustment; asset ownership correlation with inflation hedging; government spending allocation tracking',
    'If equitably distributed: fiat currency is rope (coordination with benefits broadly shared). If captured: it is snare (extraction concentrated among beneficiaries). This is a preference/empirical hybrid — depends on measurement methodology and policy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_distribution_equity, empirical, 'Whether seigniorage benefits are equitably distributed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fiat_currency_lifecycle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fiat_tr_t0, fiat_currency_lifecycle, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fiat_tr_t5, fiat_currency_lifecycle, theater_ratio, 5, 0.52).
narrative_ontology:measurement(fiat_tr_t10, fiat_currency_lifecycle, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(fiat_be_t0, fiat_currency_lifecycle, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fiat_be_t5, fiat_currency_lifecycle, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(fiat_be_t10, fiat_currency_lifecycle, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fiat_currency_lifecycle, resource_allocation).
narrative_ontology:affects_constraint(fiat_currency_lifecycle, monetary_policy_transmission).
narrative_ontology:affects_constraint(fiat_currency_lifecycle, financial_repression_mechanism).
narrative_ontology:affects_constraint(fiat_currency_lifecycle, seigniorage_distribution).
narrative_ontology:affects_constraint(fiat_currency_lifecycle, inflation_expectations_anchoring).

% DUAL FORMULATION NOTE:
% The fiat currency lifecycle decomposes into three structurally distinct constraints with different ε values: (1) monetary_policy_transmission (ε≈0.25, rope) — the mechanism by which central bank tools affect real economy; (2) financial_repression_mechanism (ε≈0.72, snare) — the systematic extraction from creditors via negative real rates; (3) seigniorage_distribution (ε≈0.55, tangled_rope) — the coordination and extraction around who captures currency creation benefits. These three constraints are causally linked but have different empirical status and stability. This story models the aggregate lifecycle; the linked stories model specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fiat_currency_lifecycle, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
