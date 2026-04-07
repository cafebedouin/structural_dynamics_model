% ============================================================================
% CONSTRAINT STORY: currency_regime_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currency_regime_lock_in, []).

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
 *   constraint_id: currency_regime_lock_in
 *   human_readable: Currency Regime Lock-In
 *   domain: political_economy/monetary_systems
 *
 * SUMMARY:
 *   Currency regime lock-in is a structural constraint that couples legal
 *   authority (legal tender laws, tax obligations), network effects (payment
 *   system coordination), and institutional inertia (central bank monetary
 *   policy). The constraint exhibits a perspectival spread across all six DR
 *   types, revealing tensions between treating currency as an immutable
 *   coordinating mechanism versus treating it as a contingent institutional
 *   arrangement subject to exit and disruption. From the powerless
 *   individual's perspective, legal tender laws and tax obligations create
 *   absolute exit barriers — the regime currency is trapped. From the
 *   financial incumbent's perspective, the constraint is pure coordination —
 *   currency regime solves unit-of-account standardization. From the
 *   cryptocurrency ecosystem's perspective, the constraint is temporary —
 *   blockchain scalability and stablecoin maturity represent a sunset
 *   pathway. The extractiveness metric has increased over the 30-year
 *   interval (0.35 to 0.62) reflecting accumulation of regulatory suppression
 *   of alternatives, while the theater ratio has also increased (0.32 to
 *   0.55) reflecting that enforcement relies increasingly on performative
 *   authority (declaring legal tender, criminalizing alternatives) rather
 *   than functional necessity. The constraint is empirically a tangled_rope:
 *   genuine coordination function (unit of account, settlement
 *   infrastructure) coupled with asymmetric extraction (seigniorage,
 *   regulatory moat protecting incumbents, suppression of alternatives).
 *   However, the analytical observer risks misclassifying this as mountain
 *   (treating fiat currency as an inherent feature of modern economy) when
 *   the structural data reveals it as contingent institutional arrangement
 *   subject to decomposition and disruption.
 *
 * KEY AGENTS:
 *   - Individual Currency User: Primary victim (powerless/trapped) — faces legal tender laws, tax obligations, wage contracts denominated in regime currency; exit blocked by law and economic dependency
 *   - Alternative Payment Developer: Secondary victim (moderate/constrained) — faces regulatory barriers, network disadvantage, capital requirements; also benefits from coordination function of regime currency for settlement
 *   - Currency Issuer (Central Bank): Primary beneficiary (institutional/arbitrage) — captures seigniorage, macroeconomic control; experiences constraint as pure coordination
 *   - Financial Incumbent (Banking System): Primary beneficiary (powerful/arbitrage) — benefits from deposit networks, settlement advantages, regulatory protection; has exit options but chooses regime currency
 *   - Cryptocurrency Ecosystem: Organized agent (organized/mobile) — building alternative infrastructure with sunset timeline; high agency and clear exit pathway
 *   - Monetary Authority: Secondary institutional (institutional/constrained) — maintains formal control through legal tender enforcement; experiences degraded function (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currency_regime_lock_in, 0.58).
domain_priors:suppression_score(currency_regime_lock_in, 0.65).
domain_priors:theater_ratio(currency_regime_lock_in, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currency_regime_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(currency_regime_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(currency_regime_lock_in, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currency_regime_lock_in, tangled_rope).
narrative_ontology:human_readable(currency_regime_lock_in, "Currency Regime Lock-In").
narrative_ontology:topic_domain(currency_regime_lock_in, "political_economy/monetary_systems").

domain_priors:requires_active_enforcement(currency_regime_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(currency_regime_lock_in, currency_issuer).
narrative_ontology:constraint_beneficiary(currency_regime_lock_in, financial_incumbents).
narrative_ontology:constraint_beneficiary(currency_regime_lock_in, reserve_currency_holders).
narrative_ontology:constraint_victim(currency_regime_lock_in, currency_users).
narrative_ontology:constraint_victim(currency_regime_lock_in, alternative_payment_system_developers).
narrative_ontology:constraint_victim(currency_regime_lock_in, currency_exit_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CURRENCY USER (SNARE) — Trapped by legal tender laws, tax obligations denominated in the regime currency, wage contracts, and debt obligations. Exit options are severely constrained: all major transactions require the regime currency; alternatives are suppressed or prohibited; the cost of emigration to escape currency regime is prohibitive. Maximum suppression (0.65) reflects that users cannot opt out of taxation, wage contracts, or debt service in the regime currency. Experiences pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(currency_regime_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ALTERNATIVE PAYMENT NETWORK DEVELOPER (TANGLED ROPE) — Constrained by regulatory barriers, network effects favoring the regime currency, and capital requirements to build competing infrastructure. However, they also benefit from the regime currency's coordination function: they can price in a stable unit of account, leverage existing payment rails for settlement, and access regime-currency markets. The constraint exhibits both genuine coordination (unit of account, settlement infrastructure) and asymmetric extraction (regulatory suppression, network lock-in advantages favoring incumbents). Active enforcement required to maintain suppression of alternatives.
constraint_indexing:constraint_classification(currency_regime_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CURRENCY ISSUER (ROPE) — Benefits from seigniorage (printing), reserve currency status (if applicable), and macroeconomic control. Experiences the constraint as pure coordination: currency regime solves the coordination problem of unit-of-account standardization. Exit options are high (can change monetary policy, introduce new currencies) but the issuer benefits from perpetuating the regime, so arbitrage is available but unmotivating. Net beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(currency_regime_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL INCUMBENT (ROPE) — Banks benefit from regime currency through deposit networks, loan contracts, and settlement advantages. Experiences constraint as coordination mechanism: currency regime enables their function as payment intermediaries. They have arbitrage options (can exit to alternative currencies, create stablecoins, build competing payment systems) but choosing not to because regime lock-in benefits them. Coordination function is genuine; extraction is moderate because beneficiaries have mobility.
constraint_indexing:constraint_classification(currency_regime_lock_in, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CRYPTOCURRENCY ECOSYSTEM (SCAFFOLD) — Organized movement (Bitcoin developers, DeFi protocols, stablecoin issuers) sees currency regime lock-in as a temporary coordination failure. They are building alternative payment and store-of-value infrastructure with sunset logic: as blockchain scalability improves, as custody solutions mature, as merchant acceptance grows, the regime currency's monopoly on unit-of-account and settlement functions declines. This perspective has high agency (can build, deploy, adopt) and sees a concrete exit path. Theater ratio low for this perspective — actual working code, not performative claims.
constraint_indexing:constraint_classification(currency_regime_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CENTRAL BANK MONETARY AUTHORITY (PITON) — Maintains formal control over currency regime through reserve requirements, interest rate policy, and legal tender law. But the functional authority is degraded: central banks cannot effectively control velocity or ultimate demand for the currency as alternatives proliferate; they maintain control through theatrical enforcement of legal tender status rather than genuine functional necessity. Theater ratio (0.48) reflects mixed: some genuine macroeconomic coordination (setting base interest rate), much performative enforcement (legal tender declarations, capital controls). Regime persists through institutional inertia — maintained because alternatives haven't fully replaced it, not because monetary policy is deeply necessary.
constraint_indexing:constraint_classification(currency_regime_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER - FALSE SUMMIT (MOUNTAIN) — From a naturalized perspective, currency regimes appear immutable: money is a natural law of economics, barter is inefficient, fiat currency is necessary for scale, the nation-state monopoly on currency is inherent to sovereignty. This perspective risks treating the contingent institutional arrangement (legal tender laws, central bank monopoly) as inherent to economic coordination itself. The engine's false summit detector should flag this: currency regimes are contingent institutional structures, not natural laws. Alternative payment systems (commodity-backed, decentralized, algorithmic) demonstrate that currency functions can be coordinated without state monopoly.
constraint_indexing:constraint_classification(currency_regime_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currency_regime_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(currency_regime_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currency_regime_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(currency_regime_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(currency_regime_lock_in, TR),
    TR >= 0.70.

:- end_tests(currency_regime_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regime currency captures seigniorage (currency issuer benefit), imposes transaction costs on alternatives (financial incumbent benefit), and restricts user choice through legal tender laws. The value reflects that extraction is significant but not maximal — regime currency does provide genuine coordination function (unit of account, settlement infrastructure), so some of the asymmetry is justified coordination incentive. Suppression (0.65): High. Legal tender laws, capital controls, tax obligations denominated in regime currency, criminalization of alternative payment systems, and network effects create substantial barriers to exit. Users cannot easily opt out of taxation, wage contracts, or debt service in regime currency. Alternative payment systems face regulatory suppression. Theater ratio (0.48): Moderate. Central bank policy contains both genuine coordination components (setting base interest rate to affect macroeconomic equilibrium) and performative elements (declaring legal tender status, enforcement theater around alternative currencies, maintaining control appearances as actual velocity control diminishes). The trajectory shows increasing theater (0.32 to 0.55) as monetary authorities rely more on enforcement declarations than functional necessity — suggesting degradation toward piton classification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. The individual currency user sees immutable law (trapped by legal tender laws, tax obligations, network lock-in) — would classify as mountain if not for the contradiction with the beneficiary perspective. The currency issuer and financial incumbents see pure coordination (rope) — the constraint solves their unit-of-account and settlement problems without friction. The cryptocurrency ecosystem sees temporary coordination failure with known sunset (scaffold) — blockchain scalability and merchant adoption are measurable, achievable, timed. The monetary authority sees degraded institutional ritual (piton) — legal tender enforcement and policy theater persist despite declining functional necessity. The alternative payment developer sees mixed constraint (tangled_rope) — blocked by regulation but also benefits from regime currency's coordination for settlement. The civilizational analytical observer risks seeing immutable law (mountain) — treating fiat currency regime as inherent to modern economy rather than contingent institutional choice. The perspectival spread reveals that the constraint's stability depends on powerless agents' inability to coordinate exits and on institutional actors' continued enforcement despite declining functional necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives, reflecting power asymmetry in the constraint. The powerless individual faces trapped exit (d ≈ 0.95) — high legal barriers, economic dependency, no alternatives. The financial incumbent faces arbitrage exit (d ≈ 0.05) — could deploy alternatives but benefits from regime, so unmotivated to exit. The central bank faces high arbitrage (d ≈ 0.10) — benefits from seigniorage, can change policy, but constraint serves its interests. The cryptocurrency ecosystem faces mobile exit (d ≈ 0.55) — can build alternatives, scaling constraints are surmountable, not externally trapped. The alternative payment developer faces constrained exit (d ≈ 0.70) — faces regulatory barriers and network disadvantage but not legal prohibition. The monetary authority faces constrained exit (d ≈ 0.60) — can change policy in principle but institutional inertia and vested interests constrain actual freedom. The perspectival gap reveals that the constraint's apparent immutability to powerless agents masks genuine mobility for institutional agents — classic asymmetric extraction structure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint's true classification is tangled_rope, not mountain. The false summit (analytical mountain perspective) naturalizes what is actually a contingent institutional arrangement. Evidence: (1) Currency regimes have transitioned historically (euro adoption, dollarization, commodity standards) — not physically immutable. (2) Alternatives are functionally feasible (commodity-backed money, decentralized stablecoins, blockchain settlement) — not logically impossible. (3) Enforcement costs are rising (regulatory capture of crypto, prosecuting alternative systems, maintaining legal tender declarations) — indicating that suppression is becoming more performative, not more natural. (4) The extractiveness metric is increasing over time (0.35 to 0.62) — suggesting that suppression must be actively strengthened, not that the constraint is becoming more natural. The mandatrophy resolution is to recognize that currency regime lock-in is a tangled_rope: it provides genuine coordination function (unit of account, settlement) coupled with asymmetric extraction (seigniorage, regulatory moat, suppression of alternatives). The constraint is not immutable — it is merely that the beneficiaries (currency issuer, financial incumbents) maintain enforcement while the victims (individual users, alternative developers) are suppressed and lack coordination. The sunset pathway exists (cryptocurrency ecosystem perspective) — whether it will actualize depends on whether the cascade of successful alternatives reaches critical mass before enforcement costs force regime accommodation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_reversibility,
    'Are network effects favoring regime currency reversible or do they represent irreversible path dependence?',
    'Historical analysis of currency regime transitions (euro adoption, dollarization, cryptocurrency adoption curves); measurement of tipping-point thresholds where alternative currencies achieve critical mass',
    'If reversible: constraint is tangled_rope with sunset potential (scaffold perspective). If irreversible: constraint approaches snare (powerless agents remain trapped indefinitely).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_reversibility, empirical, 'Whether network effects favoring regime currency are reversible').

omega_variable(
    legal_tender_enforceability,
    'Does legal tender law actually suppress alternative currencies or merely create friction that determined users can overcome?',
    'Comparison of suppression costs: transaction costs for black-market alternatives vs regime currency vs hybrid payment systems; measurement of underground economy size and alternative currency adoption in high-inflation regimes',
    'If high friction only: suppression metric should be 0.35-0.45 (constrained exit), not 0.65 (trapped exit). If genuine prohibition: suppression metric justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_tender_enforceability, empirical, 'Enforceability of legal tender law against alternatives').

omega_variable(
    seigniorage_sustainability,
    'Can currency issuers sustain seigniorage revenue as monetary alternatives proliferate, or does the lock-in mechanism depend on active suppression becoming increasingly costly?',
    'Fiscal analysis of central bank seigniorage revenue trends in countries with stable vs volatile currency demand; measurement of costs of enforcement (policing, litigation) vs seigniorage gains',
    'If sustainable: issuer''s rope perspective is robust. If unsustainable: issuer will eventually face choice between accommodation (scaffold exit) or escalation (enforcement costs rise).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_sustainability, empirical, 'Sustainability of seigniorage as alternatives proliferate').

omega_variable(
    merchant_acceptance_dynamics,
    'What merchant acceptance threshold triggers transition from alternative currency to dominant currency, and is that threshold achievable without central bank suppression?',
    'Measurement of merchant acceptance curves for alternative currencies (BTC, stablecoins) in different contexts (El Salvador, online commerce, remittance corridors); identification of critical mass and velocity thresholds',
    'If threshold achievable: scaffold sunset is real and timing is measurable. If merchant acceptance hits ceiling despite growth: lock-in is stronger than network effect analysis suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merchant_acceptance_dynamics, empirical, 'Merchant acceptance threshold for alternative currency adoption').

omega_variable(
    tax_compliance_coupling,
    'Is tax compliance obligation to regime currency a separable constraint or fundamentally coupled to currency lock-in?',
    'Policy analysis of tax systems that permit alternative-currency settlement (crypto taxes, local currency tax provisions); comparison of compliance rates and revenue collection',
    'If separable: suppression metric can be disaggregated; some victims experience constrained exit (can opt out of currency) while others face trapped exit (tax obligations remain). If coupled: decoupling requires macroeconomic policy change, making exit for tax purposes only a constrained-not-mobile option.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tax_compliance_coupling, conceptual, 'Whether tax compliance obligation is separable from currency regime lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currency_regime_lock_in, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(curlock_tr_t0, currency_regime_lock_in, theater_ratio, 0, 0.32).
narrative_ontology:measurement(curlock_tr_t10, currency_regime_lock_in, theater_ratio, 10, 0.4).
narrative_ontology:measurement(curlock_tr_t20, currency_regime_lock_in, theater_ratio, 20, 0.48).
narrative_ontology:measurement(curlock_tr_t30, currency_regime_lock_in, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(curlock_be_t0, currency_regime_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(curlock_be_t10, currency_regime_lock_in, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(curlock_be_t20, currency_regime_lock_in, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(curlock_be_t30, currency_regime_lock_in, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(currency_regime_lock_in, information_standard).
narrative_ontology:affects_constraint(currency_regime_lock_in, seigniorage_extraction).
narrative_ontology:affects_constraint(currency_regime_lock_in, monetary_policy_authority).
narrative_ontology:affects_constraint(currency_regime_lock_in, capital_control_regimes).
narrative_ontology:affects_constraint(currency_regime_lock_in, tax_compliance_obligation).

% DUAL FORMULATION NOTE:
% Currency regime lock-in is a high-level constraint decomposable into structurally distinct lower-level constraints: seigniorage extraction (ε ≈ 0.10, mountain), monetary policy authority (ε ≈ 0.35, rope), capital controls (ε ≈ 0.75, snare), tax compliance obligation (ε ≈ 0.68, snare). The integrated constraint (ε = 0.58, tangled_rope) represents the combined effect across all four mechanisms. Decomposition recommended for detailed empirical analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(currency_regime_lock_in, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
