% ============================================================================
% CONSTRAINT STORY: financial_repression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_financial_repression, []).

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
 *   constraint_id: financial_repression
 *   human_readable: Financial Repression: Coordinated Suppression of Real Returns on Savings
 *   domain: economic_policy/monetary_governance
 *
 * SUMMARY:
 *   Financial repression describes the policy framework in which real
 *   interest rates (nominal rates minus inflation) are held substantially
 *   below equilibrium levels through coordinated central bank and government
 *   action. The mechanism includes: inflation targeting above true zero-bound
 *   inflation, forward guidance maintaining expectations of low real rates,
 *   regulatory mandates forcing institutional capital into government debt,
 *   and exchange controls restricting capital outflows. This constraint
 *   exhibits genuine ambiguity between coordination and extraction. The
 *   coordination function is real: sustained financial repression enables
 *   debt stabilization without explicit default or austerity, reduces banking
 *   system solvency risk, and maintains currency stability. But the
 *   extraction mechanism is equally real: savers bear the cost through
 *   systematically negative real returns, and the burden falls heaviest on
 *   those without arbitrage access (retirees, retail savers in emerging
 *   markets). The constraint is tangled rope from the analytical perspective
 *   because the two functions are inseparable — removing the extraction
 *   mechanism (normalizing real rates) breaks the coordination function (debt
 *   sustainability). The perspectival gap is sharp: central banks and fiscal
 *   authorities see coordination; powerless savers see extraction; global
 *   institutional investors with arbitrage access see a manageable hybrid.
 *
 * KEY AGENTS:
 *   - Central Bank Authority: Primary beneficiary (institutional/arbitrage) — designer and enforcer of repression; solves debt sustainability without explicit default
 *   - Fiscal Authority: Primary beneficiary (organized/constrained) — benefits from reduced debt service burdens and deferred adjustment; retains political viability during crisis
 *   - Banking System: Secondary beneficiary (organized/mobile) — protected net interest margins through deposit rate suppression and government lending spreads
 *   - Fixed-Income Retirees: Primary victim (powerless/trapped) — nominally fixed income loses purchasing power; no exit available; maximum extraction
 *   - Retail Savers: Primary victim (powerless/constrained) — real returns persistently negative; high barriers to currency diversification or inflation hedges
 *   - Global Institutional Investors: Mixed position (powerful/arbitrage) — bear extraction through forced low-return positions but can exit through geographic diversification; genuine coordination benefit from stability
 *   - Analytical Observer: Evaluates system-wide structure (analytical/analytical) — recognizes inseparability of coordination and extraction; cannot simplify to single classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(financial_repression, 0.58).
domain_priors:suppression_score(financial_repression, 0.65).
domain_priors:theater_ratio(financial_repression, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(financial_repression, extractiveness, 0.58).
narrative_ontology:constraint_metric(financial_repression, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(financial_repression, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(financial_repression, tangled_rope).
narrative_ontology:human_readable(financial_repression, "Financial Repression: Coordinated Suppression of Real Returns on Savings").
narrative_ontology:topic_domain(financial_repression, "economic_policy/monetary_governance").

domain_priors:requires_active_enforcement(financial_repression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(financial_repression, sovereign_borrowers).
narrative_ontology:constraint_beneficiary(financial_repression, financial_system_stabilizers).
narrative_ontology:constraint_victim(financial_repression, retail_savers).
narrative_ontology:constraint_victim(financial_repression, fixed_income_retirees).
narrative_ontology:constraint_victim(financial_repression, currency_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIXED-INCOME RETIREE (SNARE) — Trapped by dependency on nominally-fixed income streams. Real purchasing power erodes through sustained negative real interest rates, inflation targeting, and capital controls. No arbitrage exit: cannot move savings to higher-return jurisdictions (exchange controls), cannot access inflation-hedging assets (transaction costs, regulatory barriers), cannot exit currency (legal tender laws). Maximum experienced extraction without coordination benefit.
constraint_indexing:constraint_classification(financial_repression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RETAIL SAVER IN EMERGING MARKETS (SNARE) — Constrained by high transaction costs and regulatory barriers to currency diversification. Domestic interest rates held below inflation by policy; alternative stores of value (hard currency, crypto, real estate) face legal restrictions or speculative volatility. Real wealth erosion systematic and difficult to escape through legal channels. High suppression with minimal coordination function.
constraint_indexing:constraint_classification(financial_repression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GLOBAL INSTITUTIONAL INVESTOR (TANGLED ROPE) — Benefits from access to multiple jurisdictions and currency markets (arbitrage exit). Genuine coordination function: financial repression stabilizes sovereign debt loads and prevents systemic financial crises that would harm institutional balance sheets. But also bears extraction: negative real returns on some positions, forced participation in domestic credit markets to maintain jurisdiction access, regulatory pressure to hold government debt. Asymmetric extraction overlaid on genuine coordination.
constraint_indexing:constraint_classification(financial_repression, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CENTRAL BANK AUTHORITY (ROPE) — Primary beneficiary. Experiences financial repression as pure coordination: reducing real interest rates via inflation targeting, forward guidance, and regulatory constraints enables debt sustainability and financial stability without explicit default. Solves the collective action problem of excess sovereign leverage. No extraction experienced by this agent — they are the constraint designer and primary beneficiary.
constraint_indexing:constraint_classification(financial_repression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: BANKING SYSTEM (ROPE) — Benefits from financial repression through protected net interest margins: depositors earn near-zero on savings while banks borrow short at regulated rates, lending to government at controlled yields. Genuine coordination function: repressed rates reduce bank solvency risk and enable recapitalization through spreads. Mobile exit (can relocate some operations) but organizational structure anchored to domestic regulatory framework. Primary beneficiary alongside central bank.
constraint_indexing:constraint_classification(financial_repression, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: FISCAL AUTHORITY (SCAFFOLD) — Benefits from financial repression as a temporary debt-reduction mechanism during deleveraging phase. Genuine coordination function: repressed rates allow debt-to-GDP stabilization without explicit taxation or austerity. Sunset clause: repression is explicitly presented as a temporary transition mechanism — once debt levels normalize and structural reforms are complete, real interest rates should normalize. Constrained by political economy constraints on true deleveraging. Theater moderately high as repression is often presented as price stability rather than debt management.
constraint_indexing:constraint_classification(financial_repression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: INTERNATIONAL MONETARY REGIME (PITON) — At civilizational scale, financial repression represents a degraded version of the Bretton Woods coordination system. The original regime (pegged exchange rates, capital controls, domestic credit allocation) served genuine coordination functions; modern financial repression maintains the control framework while the coordination logic has atrophied. Theater ratio high: repression is sustained through narrative (temporary crisis measures, price stability objectives) rather than functional necessity. The regime sees its own constraints as inertial.
constraint_indexing:constraint_classification(financial_repression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, financial repression solves genuine coordination problems (debt sustainability, financial stability, currency stability) while systematically extracting from savers. The extraction is not ancillary — it is the primary mechanism through which the coordination function operates. The constraint must be classified as tangled rope because removing the extraction mechanism breaks the coordination function. This is genuine policy mandatrophy: stabilizing debt levels through redistribution from savers to borrowers is coordination overlaid on extraction. Cannot be simplified to either pure type.
constraint_indexing:constraint_classification(financial_repression, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(financial_repression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(financial_repression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(financial_repression, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(financial_repression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(financial_repression, TR),
    TR >= 0.70.

:- end_tests(financial_repression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting sustained negative real returns on savings and systematic wealth transfer from savers to borrowers. The measurement has risen from 0.35 to 0.58 over the interval, indicating intensification as debt stabilization demands persisted longer than initially anticipated and central banks extended forward guidance repeatedly. This is not maximal extraction (0.70+) because some savers retain access to alternative assets and some beneficiaries (institutional investors with arbitrage) partially escape extraction. Suppression (0.65): High. Savers face multiple barriers to exit: legal tender laws (must use repressing currency for obligations), capital controls (explicit or implicit through regulatory burden), information asymmetry about true inflation (official statistics may understate), and psychological sunk-cost effects (long-term savings already committed). Emerging market savers face even higher suppression through explicit exchange controls and restricted access to foreign-denominated assets. Theater ratio (0.58): Moderate-high, and rising. Financial repression is maintained through narrative framing: presented as temporary crisis measures or price stability objectives rather than explicit debt reduction. The theater increases when debt stabilization proceeds faster than policy narrative adjusts, creating lag between functional necessity and stated rationale. Theater rises from 0.42 to 0.58 because the original emergency framing (2008 crisis, 2020 pandemic) persists despite structural normalization.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The central bank sees coordination (rope) — repression solves debt sustainability. The powerless saver sees extraction (snare) — real returns are negative with no exit. The global institutional investor sees manageable hybrid (tangled rope) — both extraction and coordination are real but the investor can arbitrage. The fiscal authority sees temporary solution (scaffold) — sunset clause is explicit, though deferred. The banking system sees stable rent extraction (rope) — protected spreads provide coordination benefit through financial stability. The international monetary regime sees inertial degradation (piton) — the control framework persists though original coordination logic has weakened. The analytical observer sees irreducible ambiguity (tangled rope) — coordination and extraction are inseparable at this scale. No two perspectives produce the same classification. The gap is not resolvable by choosing 'the right perspective' — it is structural, reflecting genuine divergence in costs, benefits, and available exits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Central bank/fiscal authority (beneficiaries, arbitrage exits) have d ≈ 0.05-0.15 → f(d) ≈ -0.05 to 0.05 → negative or near-zero experienced extraction. Retail savers (victims, trapped or constrained) have d ≈ 0.85-0.95 → f(d) ≈ 1.15-1.42 → high experienced extraction chi. Global institutional investors (mixed: some beneficiary status through stability, some victim status through forced positions, arbitrage exits) have d ≈ 0.40-0.55 → f(d) ≈ 0.40-0.75 → moderate extraction. The spatial scope modifier σ(S) amplifies for global scope (σ=1.2): verification of repression as policy is easier at larger scope (not hidden as local choice) so effective extraction is higher. The perspectival gap emerges because the same base extractiveness (0.58) is experienced very differently depending on the observer's power and exit capacity: to the powerful with arbitrage it may feel like 0.25-0.35 effective extraction; to the powerless trapped it feels like 0.80+.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH TANGLED ROPE CLASSIFICATION: Financial repression cannot be simplified to either pure coordination (rope) or pure extraction (snare) because the extraction mechanism IS the coordination mechanism. Removing negative real returns (the extraction) breaks debt stabilization (the coordination). This is a genuine mandatrophy case: the constraint solves a real collective action problem while systematically transferring wealth. The mandate is to stabilize debt and prevent financial crisis; the mandate is achieved through extraction from savers. The trap is that alternative debt adjustment mechanisms (progressive taxation, fiscal reforms, explicit restructuring) are politically infeasible, so the only available mandatrophic solution is the extraction mechanism itself. Classification as tangled rope makes this explicit: genuine coordination overlaid on asymmetric extraction. The constraint cannot be dismissed as 'just extraction' (snare) because financial stability and debt sustainability are real public goods; it also cannot be celebrated as 'just coordination' (rope) because the distribution of costs is radically asymmetric and concentrated on those with no exit. The classification forces the ambiguity into visibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_measurement_ambiguity,
    'Is measured inflation capturing true erosion of purchasing power, or is official inflation deliberately underestimating real cost of living?',
    'Comparison of official inflation indices with hedonic adjustments, basket reweighting, and real consumption patterns; analysis of historical inflation measurement changes; cross-country comparison of inflation vs wage growth trajectories',
    'If official inflation understates true cost of living: repression is more severe than measured. If official inflation is accurate: repression is moderate but still negative for savers. Either case: measurement manipulation is itself an extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_measurement_ambiguity, empirical, 'Whether official inflation accurately measures purchasing power erosion').

omega_variable(
    genuine_vs_manufactured_debt_crisis,
    'Is financial repression a necessary response to a genuine debt sustainability crisis, or is the crisis itself partly manufactured through prior policy choices and maintained through narrative?',
    'Historical analysis of debt accumulation mechanisms; comparison of counterfactual trajectories under alternative policy regimes; analysis of primary balance trends and structural revenue capacity',
    'If genuine crisis: repression is justified coordination mechanism (classification remains tangled rope but with lower perceived extraction). If manufactured: repression is primarily extraction disguised as coordination necessity (classification shifts toward snare/piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_manufactured_debt_crisis, empirical, 'Whether debt crisis necessitating repression is genuine or manufactured').

omega_variable(
    exit_options_availability,
    'What percentage of savers in a given jurisdiction actually have access to arbitrage exits (currency diversification, offshore accounts, inflation-hedging assets)?',
    'Cross-country surveys of retail investor access to alternative assets; analysis of capital controls effectiveness; measurement of wealth distribution correlation with exit capacity',
    'If exit options are widely available: constraint is less severe; powerless agents should reclassify as constrained. If exits are effectively closed for <50% of population: repression is more severe snare than tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_options_availability, empirical, 'Actual exit availability for savers across wealth distribution').

omega_variable(
    real_rate_normalization_timeline,
    'What is the actual time horizon for real interest rate normalization after debt stabilization — is the sunset clause genuine or aspirational?',
    'Historical analysis of post-crisis monetary policy normalization timelines; comparison of policy guidance promises vs actual outcomes; structural analysis of whether debt normalization creates political/economic conditions enabling rate normalization',
    'If normalization occurs within 5-10 years: scaffold perspective is valid and constraint has genuine sunset. If normalization is indefinitely deferred: scaffold is illusory and constraint is permanent snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_rate_normalization_timeline, empirical, 'Whether real rate normalization follows debt stabilization or is indefinitely deferred').

omega_variable(
    alternative_coordination_mechanisms,
    'Could debt stabilization and financial system stability be achieved through mechanisms that do not require negative real returns on savings (e.g., progressive taxation, explicit restructuring, fiscal reforms)?',
    'Comparative analysis of countries using explicit fiscal adjustment vs financial repression; modeling of debt paths under alternative policy mixes; analysis of political feasibility of alternatives',
    'If alternatives exist but are politically rejected: repression is extraction disguised as necessity, and should be reclassified as snare. If alternatives are genuinely infeasible: repression is unique coordination mechanism and tangled rope classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, conceptual, 'Whether financial repression is unique or merely politically preferred debt adjustment mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(financial_repression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(finrep_tr_t0, financial_repression, theater_ratio, 0, 0.42).
narrative_ontology:measurement(finrep_tr_t5, financial_repression, theater_ratio, 5, 0.52).
narrative_ontology:measurement(finrep_tr_t10, financial_repression, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(finrep_be_t0, financial_repression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(finrep_be_t5, financial_repression, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(finrep_be_t10, financial_repression, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(financial_repression, resource_allocation).
narrative_ontology:affects_constraint(financial_repression, sovereign_debt_sustainability).
narrative_ontology:affects_constraint(financial_repression, currency_stability_regimes).
narrative_ontology:affects_constraint(financial_repression, banking_system_solvency).

% DUAL FORMULATION NOTE:
% Financial repression is a coordination mechanism for multiple downstream constraints: it stabilizes sovereign debt loads (affects sovereign_debt_sustainability), supports currency pegs or stability targets (affects currency_stability_regimes), and reduces banking sector insolvency risk (affects banking_system_solvency). Each downstream constraint has its own extractiveness value reflecting specific causal pathways; financial repression represents the common coordination infrastructure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(financial_repression, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
