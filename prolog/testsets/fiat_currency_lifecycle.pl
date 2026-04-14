% ============================================================================
% CONSTRAINT STORY: fiat_currency_lifecycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The lifecycle of a fiat currency exhibits the full spectrum of constraint
 *   classifications depending on the observer's structural position and time
 *   horizon. Initially, the fiat currency functions as pure coordination: a
 *   medium of exchange and unit of account enabling complex economic
 *   activity. The government and central bank benefit from monetary policy
 *   flexibility. Ordinary currency holders experience a rope-like
 *   coordination mechanism — they can transact, borrow, and save in the
 *   currency. Over the lifecycle, extraction mechanisms accumulate: inflation
 *   taxes the accumulation of savings; financial repression keeps real
 *   interest rates negative; regulatory barriers prevent substitution to
 *   alternative currencies. By mature stages, the constraint exhibits high
 *   extractiveness (0.58) and suppression (0.68), with the central bank and
 *   fiscal authority capturing benefits while currency holders and savers
 *   bear costs. The theater ratio (0.65) reflects the performative nature of
 *   central bank independence and inflation targeting — the institutions
 *   maintain public commitment to low inflation while institutional
 *   structures (fractional reserve banking, fiscal dominance, regulatory
 *   capture by financial sector) make high inflation incentives structural.
 *   The constraint decomposed across time: coordination at inception, tangled
 *   rope in the growth phase, snare for trapped savers, scaffold for those
 *   building alternatives, piton for degraded gold standard mythology, and
 *   mountain for those naturalizing monetary extraction as inherent to
 *   economic systems.
 *
 * KEY AGENTS:
 *   - Currency Holders: Primary victims (powerless/trapped) — savings eroded by inflation; no exit option; experience maximum extraction
 *   - Savers on Fixed Income: Primary victims (powerless/trapped) — pensioners and fixed-wage earners bear inflation tax directly; cannot renegotiate contracts
 *   - Central Bank: Primary beneficiary (institutional/arbitrage) — controls monetary policy parameters; captures seigniorage; low extraction experienced
 *   - Government Fiscal Authority: Primary beneficiary (institutional/arbitrage) — ability to deficit-spend; inflation erodes real debt; net beneficiary
 *   - Small Business Owners: Secondary actors (moderate/constrained) — benefit from cheap credit (coordination) but constrained by currency volatility and input cost inflation
 *   - Financial Sector: Secondary actor (organized/constrained) — benefits from credit expansion and regulatory capture; constrained by systemic risk and volatility
 *   - Cryptocurrency Coalition: Organized alternative builders (organized/mobile) — constructing exit pathways with sunset logic; declining suppression as adoption grows
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent to monetary systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fiat_currency_lifecycle, 0.58).
domain_priors:suppression_score(fiat_currency_lifecycle, 0.68).
domain_priors:theater_ratio(fiat_currency_lifecycle, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiat_currency_lifecycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(fiat_currency_lifecycle, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fiat_currency_lifecycle, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fiat_currency_lifecycle, tangled_rope).
narrative_ontology:human_readable(fiat_currency_lifecycle, "The Lifecycle of a Fiat Currency").
narrative_ontology:topic_domain(fiat_currency_lifecycle, "economic/political").

domain_priors:requires_active_enforcement(fiat_currency_lifecycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fiat_currency_lifecycle, central_bank).
narrative_ontology:constraint_beneficiary(fiat_currency_lifecycle, government_fiscal_authority).
narrative_ontology:constraint_beneficiary(fiat_currency_lifecycle, incumbent_financial_institutions).
narrative_ontology:constraint_victim(fiat_currency_lifecycle, currency_holders).
narrative_ontology:constraint_victim(fiat_currency_lifecycle, savers_on_fixed_income).
narrative_ontology:constraint_victim(fiat_currency_lifecycle, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CURRENCY HOLDER (SNARE) — Individual with savings denominated in the currency has no exit: cannot easily convert to alternative stores of value without regulatory obstacles, taxation, or capital controls. Experiences full extraction as currency depreciates. Maximum suppression: no alternatives offered, psychological anchoring to local currency, few knowledge of hedging options.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAVER ON FIXED INCOME (SNARE) — Pensioner or wage-earner with income fixed in nominal terms experiences erosion of purchasing power. Cannot renegotiate pension or wage easily. Bears extraction directly through inflation tax. Trapped by age/contractual constraints.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL BUSINESS OWNER (TANGLED ROPE) — Benefits from access to credit denominated in cheap currency (coordination function: currency enables borrowing). But constrained by currency volatility, inability to hedge easily, and input cost inflation. Mixed extraction and benefit depending on whether business is net debtor or net creditor. Constrained exit: cannot easily switch to another currency or jurisdiction.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CENTRAL BANK (ROPE) — Experiences the fiat currency system as a coordination mechanism for monetary policy implementation. Can arbitrage between currency creation, interest rates, and inflation targets. Low extraction experienced because the institution controls the constraint's parameters. Net beneficiary of seigniorage and policy flexibility.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GOVERNMENT FISCAL AUTHORITY (ROPE) — Benefits from ability to deficit-spend and inflate away real debt. Experiences fiat currency as pure coordination enabling fiscal transfers and counter-cyclical spending. Arbitrage exit: can change monetary policy rules. Net beneficiary of the system.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FINANCIAL SECTOR (TANGLED ROPE) — Benefits from currency as medium of exchange and credit expansion (coordination). But faces constraints from regulatory oversight and systemic risk exposure. Can arbitrage between different currencies and assets (partial exit). Mixed: extraction runs toward this agent through regulatory capture but away through volatility risk.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: GOLD STANDARD LEGACY (PITON) — The fiat currency system retains performative commitment to backing (central banks hold reserves, international trade settlement occurs through official channels) but the backing is theatrical: fiat money is not convertible to gold or other scarce commodity. The ritual of reserve management persists despite zero functional constraint. Theater ratio very high — the constraint is maintained through institutional inertia and narrative authority ('Fed independence', 'credible commitment'), not through real backing.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: CRYPTOCURRENCY COALITION (SCAFFOLD) — Organized agents (Bitcoin developers, stablecoin designers, DeFi protocols) building alternative monetary systems with built-in sunset to fiat dependence. Low effective extraction because this coalition has genuine exit options and is actively constructing alternatives. Suppression declining as barriers to crypto adoption fall. Sunset clause: as alternative systems mature, fiat currency's monopoly on settlement erodes.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some monetary system is necessary for large-scale coordination. Any medium of exchange will experience the same lifecycle: trust creation, adoption, inevitable inflation pressures, eventual instability or reform. The extraction appears to be inherent to monetary systems themselves. However, structural data contradicts true mountain classification — the specific institutional arrangements (central bank independence, fractional reserve leverage, regulatory capture) are contingent, not laws of nature. Engine will detect as false summit.
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
    constraint_indexing:constraint_classification(fiat_currency_lifecycle, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Base Extractiveness (0.58): High-moderate, reflecting that the fiat currency system enables both genuine coordination (medium of exchange, credit creation) and sustained extraction (inflation tax, financial repression). The extractiveness value is not at maximum because the system provides real coordination benefits — without it, complex modern economies cannot function. The 0.58 reflects the lifecycle: started near 0.15 (pure coordination), accumulated to 0.58 over 50 time units as rent-seeking and extraction mechanisms layered atop coordination. Suppression (0.68): High, reflecting regulatory barriers to currency substitution, capital controls, inflation-driven opportunity costs of holding cash, and lack of transparently available alternatives for ordinary savers. Suppression includes both coercive barriers (legal prohibition on foreign currencies) and opportunity costs (financial repression on denominated assets). Theater Ratio (0.65): Moderate-high. Central bank independence, inflation targeting frameworks, and reserve management are partially performative. The actual inflation rate often exceeds official targets; the independence is constrained by political pressure; the backing (reserves) does not create scarcity. The performance maintains institutional legitimacy even as real policies extract value from savers. The constraint requires active institutional enforcement of the monetary regime through regulatory authority, legal tender laws, and control of settlement systems.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects lifecycle and structural position. Early in the lifecycle (Perspective 4, Central Bank), the constraint appears as pure coordination (Rope) — the system solves the double-coincidence-of-wants problem and enables monetary policy. Later in the lifecycle (Perspective 1, Currency Holder), the same system appears as extraction (Snare) — savings eroded, no alternatives. The government (Perspective 5) sees benefits (Rope); the pensioner (Perspective 2) sees costs (Snare). Crucially, both perspectives describe the same constraint — same institutional structure, same monetary mechanisms. The perspectival gap reveals that fiat currency is NOT a single classification but a presheaf: the classification depends on observer position and time horizon. The analytical observer (Perspective 9) risks seeing this as a mountain — monetary inflation is 'inherent to fiat systems' — but the structural data contradicts this: the inflation rate is a policy choice, not a natural law. The cryptocurrency coalition (Perspective 8) demonstrates that alternatives exist with lower extraction and sunset logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extraction (chi) depends on their structural position — power level, time horizon, and exit options — derived through the sigmoid function from their directionality value (d). Currency holders with no exit (trapped) and no power (powerless) derive high d approaching 1.0, producing high f(d) and experiencing chi at maximum. Government and central bank with institutional power and arbitrage exit (can change monetary policy rules) derive low d near 0.0, producing negative f(d) and experiencing negative chi (benefit). Small business owners with moderate power and constrained exit (cannot easily switch currencies but can partially hedge) derive d around 0.55-0.60, experiencing moderate extraction. The financial sector, though organized and powerful globally, faces constrained exit within their home currency (must clear through national settlement systems), producing mid-range d and mixed extraction. Cryptocurrency builders have mobile exit (building alternatives) and organized power, producing low d and low experienced extraction despite the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that tangled rope is the stable classification: genuine coordination function (medium of exchange, credit creation) coexists with asymmetric extraction (inflation tax, financial repression, regulatory barriers to substitution). The classification prevents mislabeling: not a pure rope (extraction is significant and asymmetric), not a snare (coordination benefits are real and necessary), but genuinely hybrid. The piton perspective (Perspective 7) reveals that mythology (gold standard backing, central bank independence) maintains the system's institutional legitimacy despite degradation — this is classic inertial constraint behavior. The scaffold perspective (Perspective 8) is crucial: it shows that the lifecycle is transitional, not eternal. As alternative monetary systems mature (cryptocurrency, stablecoins, multi-currency settlement), the monopoly on fiat currency erodes, the suppression declines, and agents gain exit options. The constraint transforms from tangled rope toward scaffold, with sunset logic on the fiat-only regime. The false mountain claim (inherent inflation) is exposed by the existence of alternatives and the policy choice nature of inflation rates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    velocity_of_money_threshold,
    'At what inflation rate does currency velocity spike irreversibly, transitioning from coordination mechanism to extraction device?',
    'Historical analysis of hyperinflationary episodes; correlation between inflation rate, velocity changes, and institutional switching costs for alternative currencies',
    'If threshold < 5% annual inflation: many nominal stable currencies misclassified as snares. If threshold > 20%: extraction persists undetected until acute crisis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(velocity_of_money_threshold, empirical, 'Inflation threshold triggering currency abandonment').

omega_variable(
    substitution_mechanism_viability,
    'Can alternative monetary systems (cryptocurrency, commodity-backed, multi-currency baskets) actually provide equivalent coordination benefits with lower extraction costs?',
    'Comparative analysis of settlement systems, transaction costs, volatility, and institutional stability across multiple monetary regimes; empirical testing of alternative systems at scale',
    'If viable: scaffold perspective confirmed, fiat currency lifecycle is transitional. If unviable: alternatives remain niche, fiat monopoly persists despite extraction costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_mechanism_viability, empirical, 'Whether alternative monetary systems can viably substitute for fiat').

omega_variable(
    inflation_expectation_anchoring,
    'Is central bank credibility in low-inflation commitment an emergent consensus or performative theater maintained by regulatory authority?',
    'Analysis of inflation expectations markets; correlation between central bank messaging and actual inflation outcomes across multiple policy regimes; study of what happens when central bank credibility breaks',
    'If consensus: inflation remains bounded and extraction contained. If theater: coordination mechanism is fragile and extractive collapse (hyperinflation) is tail risk event.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_expectation_anchoring, conceptual, 'Whether central bank inflation credibility is structural or performative').

omega_variable(
    financial_repression_necessity,
    'Do government debt/GDP ratios require persistent financial repression (negative real interest rates on savings) to remain manageable, making extraction inherent to modern fiscal systems?',
    'Time-series analysis of real interest rates vs government debt levels across developed economies; modeling of sustainable debt trajectories with and without repression; comparison to pre-fiat commodity-based systems',
    'If necessary: extraction is hidden fiscal restructuring, not coordination failure. If optional: financial repression represents regulatory choice, not constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(financial_repression_necessity, empirical, 'Whether financial repression is required to manage modern government debt').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fiat_currency_lifecycle, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fiat_tr_t0, fiat_currency_lifecycle, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fiat_tr_t25, fiat_currency_lifecycle, theater_ratio, 25, 0.52).
narrative_ontology:measurement(fiat_tr_t50, fiat_currency_lifecycle, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(fiat_be_t0, fiat_currency_lifecycle, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fiat_be_t25, fiat_currency_lifecycle, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(fiat_be_t50, fiat_currency_lifecycle, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fiat_currency_lifecycle, resource_allocation).
narrative_ontology:affects_constraint(fiat_currency_lifecycle, financial_repression_mechanism).
narrative_ontology:affects_constraint(fiat_currency_lifecycle, central_bank_independence_capture).
narrative_ontology:affects_constraint(fiat_currency_lifecycle, cryptocurrency_settlement_competition).

% DUAL FORMULATION NOTE:
% The fiat currency lifecycle constraint has two analytically distinct sub-constraints: (1) the coordination mechanism (medium of exchange, credit creation) which is genuine and necessary, and (2) the extraction mechanism (inflation tax, financial repression) which emerges over the lifecycle. These have different epsilon values — the coordination is near 0.10 (rope), the extraction is near 0.68 (snare). The story models them as one tangled rope rather than decomposing because they are inseparable institutionally: you cannot have fiat currency credit without accepting inflation risk. However, the network edges to downstream constraints (financial repression, regulatory capture) model the specific mechanisms through which extraction compounds over the lifecycle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fiat_currency_lifecycle, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
