% ============================================================================
% CONSTRAINT STORY: us_sanctions_moex_2024
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_sanctions_moex_2024, []).

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
 *   constraint_id: us_sanctions_moex_2024
 *   human_readable: U.S. Sanctions on Moscow Exchange (MOEX) 2024
 *   domain: geopolitical/financial_sanctions
 *
 * SUMMARY:
 *   The U.S. sanctions on the Moscow Exchange (MOEX) implemented in 2024
 *   represent a structural constraint on Russian financial market access and
 *   capital allocation. The sanction regime creates extraction from
 *   sanctioned Russian traders and institutional participants through
 *   coercive barriers to market access, while benefiting U.S. Treasury policy
 *   leverage and alternative trading infrastructure operators. The constraint
 *   exhibits high suppression (75%) — no exit option exists within the legal
 *   framework — and significant extractiveness (68%) through capital flight
 *   costs, liquidity degradation, and forced migration to inferior venues.
 *   Theater ratio (58%) reflects partial performative content: MOEX continues
 *   operations nominally as a functioning exchange, but its liquidity and
 *   scope are drastically reduced by sanction-driven migration of trading
 *   activity. The six perspectives reveal the constraint as primarily a snare
 *   for sanctioned Russian actors (trapped with no exit), coordinative for
 *   U.S. policymakers (achieving geopolitical objectives), hybrid for
 *   alternative trading infrastructure (benefiting from displacement but
 *   constrained by secondary sanctions risk), temporary for de-dollarization
 *   coalitions (seeing a sunset as BRICS+ alternatives mature), inert for
 *   international settlement standards (piton theater masking geopolitical
 *   function), and structurally real from the analytical observer's
 *   civilizational view (confirming political gating of financial access as a
 *   persistent constraint).
 *
 * KEY AGENTS:
 *   - Russian Retail Traders: Primary victims (powerless/trapped) — lose direct MOEX access; forced to offshore brokers with degraded execution
 *   - Russian Fund Managers: Secondary victims (moderate/constrained) — can access alternatives (SPYF, Singapore) but face liquidity collapse and settlement delays
 *   - U.S. Treasury / OFAC: Primary beneficiary (institutional/arbitrage) — gains policy leverage and capital control mechanisms; can adjust designations and scopes
 *   - Alternative Trading Venues (SPYF, Shanghai, Singapore Exchanges): Secondary beneficiary (organized/constrained) — capture liquidity migration but operate under secondary sanctions threat
 *   - De-Dollarization Coalition (China, Russia, India, Brazil): Organized actor (powerful/mobile) — benefits from sanction pressure to accelerate CIPS, rupee settlement alternatives with explicit sunset to sanctions mechanism
 *   - International Settlement Standards (SWIFT, CHIPS, Correspondent Banking): Institutional theater (institutional/arbitrage) — nominally neutral technical infrastructure but de facto geopolitical enforcement mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees political gating of financial access as permanent structural feature, not law of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sanctions_moex_2024, 0.68).
domain_priors:suppression_score(us_sanctions_moex_2024, 0.75).
domain_priors:theater_ratio(us_sanctions_moex_2024, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sanctions_moex_2024, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_sanctions_moex_2024, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_sanctions_moex_2024, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_sanctions_moex_2024, snare).
narrative_ontology:human_readable(us_sanctions_moex_2024, "U.S. Sanctions on Moscow Exchange (MOEX) 2024").
narrative_ontology:topic_domain(us_sanctions_moex_2024, "geopolitical/financial_sanctions").

domain_priors:requires_active_enforcement(us_sanctions_moex_2024).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_sanctions_moex_2024, us_treasury_dept).
narrative_ontology:constraint_beneficiary(us_sanctions_moex_2024, alternative_trading_venues).
narrative_ontology:constraint_victim(us_sanctions_moex_2024, russian_equity_traders).
narrative_ontology:constraint_victim(us_sanctions_moex_2024, moex_institutional_participants).
narrative_ontology:constraint_victim(us_sanctions_moex_2024, settlement_infrastructure_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SANCTIONED RUSSIAN RETAIL TRADER (SNARE) — Complete loss of access to MOEX and U.S.-linked settlement systems. No exit option except offshore brokers with severe degradation in liquidity, clearing speed, and regulatory protection. Trapped by geography and asset location. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.75. High extractiveness because coercive barriers replace all exit options.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RUSSIAN FUND MANAGER (SNARE) — Can theoretically move to SPYF (sanctions-compliant MOEX proxy) or Singapore exchanges, but liquidity is catastrophically reduced (~10% of pre-sanction volumes). Constrained exit — nominally available but economically crippling. Experiences extraction because capital flight costs dwarf any benefit. d≈0.78, f(d)≈1.08, σ=1.2 → χ≈0.55.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: U.S. TREASURY / OFAC (ROPE) — Sanctions are experienced as coordination mechanism: enforces capital controls, prevents capital flight, reduces Russian Central Bank access to hard currency. Treasury benefits from arbitrage authority — can adjust sanction scope, license exceptions, target designations. Sees sanction architecture as enabling other policy objectives (defense aid, sanctions evasion detection). d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.04. Negative extraction because benefits (policy leverage) exceed costs (enforcement overhead).
constraint_indexing:constraint_classification(us_sanctions_moex_2024, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE TRADING INFRASTRUCTURE (TANGLED ROPE) — Benefits from sanction-driven migration of Russian equity flows (liquidity capture, market share growth in SPYF and Singapore venues). Also constrained by need to avoid secondary U.S. sanctions for 'facilitating Russian circumvention' — must enforce compliance screening, maintain separation from MOEX settlement. d≈0.35, f(d)≈0.36, σ=1.1 → χ≈0.27. Hybrid: benefits from displaced flows (coordination) but operates under active suppression (secondary sanctions risk and compliance burden).
constraint_indexing:constraint_classification(us_sanctions_moex_2024, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTILATERAL DE-DOLLARIZATION COALITION (SCAFFOLD) — China, Russia, India, Brazil invest in alternatives (CIPS, Rupee settlements, direct bilateral trade) to reduce U.S. sanction leverage. Coalition has exit option (transact outside dollar system). Sees sanctions as temporary coordination problem with explicit sunset: as BRICS+ settlement infrastructure matures (estimated 5-10 year horizon), dollar-based sanction enforcement loses mechanism. d≈0.45, f(d)≈0.50, σ=1.1 → χ≈0.27. Moderate extraction now; declining as infrastructure sunset approaches.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL SETTLEMENT STANDARDS (PITON) — Sanctions enforcement depends on SWIFT messaging, CHIPS clearing participation, and correspondent banking networks. These systems nominally operate on technical/commercial logic (settlement efficiency, liquidity provision). But de facto they have become geopolitical enforcement tools — the technical standards are now substantially performative. SWIFT's 'neutral messaging' while enabling sanctions targeting; correspondent banking's 'commercial relationships' while enforcing political exclusion. theater_ratio=0.58 suggests modest performative content; but civilizational view reveals SWIFT-as-impartial-technical-standard is substantially inertial theater masking political function. d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.03. Piton classification comes from inertial framing of technical systems as value-neutral.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GLOBAL FINANCIAL INTEROPERABILITY (SNARE) — From a universal perspective, MOEX sanctions represent a fundamental constraint on the global financial system's theoretical property of 'open access to capital markets.' The constraint is that access is always gated by political authority (U.S. Treasury, sanctions lists, OFAC designations). No observer can exit this political gating. The analytical observer sees the snare structure as civilizationally real: financial 'openness' exists only within permitted political boundaries. d≈0.70, f(d)≈1.05, σ=1.2 → χ≈0.57. This perspective confirms rather than naturalizes the snare — it is not a law of nature but a persistent structural fact of geopolitical organization.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sanctions_moex_2024_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_sanctions_moex_2024, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_sanctions_moex_2024, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_sanctions_moex_2024, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_sanctions_moex_2024, TR),
    TR >= 0.70.

:- end_tests(us_sanctions_moex_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The sanction regime creates significant extraction through multiple mechanisms: (1) capital flight costs (liquidity migration to inferior venues with 50-80% execution degradation), (2) asset freezing (trapped equity positions), (3) settlement delays (circumventing SWIFT creates 2-3 day clearing lags), (4) regulatory uncertainty (sanctions scope expands unpredictably, creating risk premium on Russian assets). The measure reflects that extraction is both intentional (policy objective) and structural (system design). Suppression (0.75): High. Exit options are almost entirely eliminated: direct MOEX access is sanctioned; U.S. market access is blocked; European clearing is unavailable; SWIFT alternatives are nascent and monitored. The only remaining exit (offshore brokers with alternative settlement) involves massive execution degradation, making it economically crippling rather than genuine exit. Suppression is codified in law (Executive Order) and enforced through correspondent banking blockade. Theater ratio (0.58): Moderate. MOEX continues to publish trading data, operate clearing houses, and maintain regulatory facade of a functioning exchange. But the theater is less than complete — actual trading volumes are public and demonstrably collapsed, making the performance of 'normalcy' partially transparent. The 58% value reflects that MOEX's operational facade persists while its functional substance (liquidity, capital allocation) has migrated elsewhere.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a stark perspectival gap between victim and beneficiary perspectives. Russian traders (powerless/trapped) experience the constraint as a snare with no exit: d≈0.92 drives χ=0.75. U.S. Treasury (institutional/arbitrage) experiences it as coordination enabling policy leverage: d≈0.08 drives χ≈-0.04. The beneficiary sees coordination; the victim sees extraction. Alternative trading infrastructure (organized/constrained) experiences a tangled rope: d≈0.35 drives χ≈0.27 — they gain liquidity but operate under secondary sanctions suppression. De-dollarization coalitions (powerful/mobile) experience a scaffold: d≈0.45 drives χ≈0.27, declining as BRICS+ alternatives mature. The piton perspective (SWIFT as neutral technical standard) naturalizes what is actually a geopolitical enforcement mechanism — theater_ratio=0.58 masks the reality that SWIFT's 'commercial neutrality' is performative. The analytical observer's snare perspective confirms that political gating of financial access is the actual structural constraint, not laws of economics or nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Russian retail traders: Victims + trapped → d≈0.92, f(d)≈1.40. Maximum extraction coefficient because no exit option exists within legal framework. Capital flight to offshore alternatives represents economic devastation, not genuine exit. Russian fund managers: Victims + constrained → d≈0.78, f(d)≈1.08. High extraction because alternatives (SPYF, Singapore) provide only 10-20% of pre-sanction liquidity — constrained exit is barely superior to trapped exit. U.S. Treasury: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Negative extraction coefficient because Treasury benefits from policy authority to designate, adjust, and enforce. Sanctions are a net gain to institutional power. Alternative infrastructure: Beneficiary + constrained → d≈0.35, f(d)≈0.36. Moderate extraction because secondary sanctions threat constrains how much they can benefit from liquidity migration. De-dollarization coalition: Beneficiary/victim mix with mobile/constrained → d≈0.45, f(d)≈0.50. Benefits from geopolitical pressure to build alternatives; constrained by current SWIFT/dollar dependence. SWIFT/Settlement: Institutional + arbitrage → d≈0.10, f(d)≈-0.05. Beneficiary in appearance (conducts transactions) but see piton perspective — operates under inertial theater. Analytical observer: analytical → d≈0.70, f(d)≈1.05. Confirms snare structure at civilizational level.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolved: The constraint avoids collapsing into mislabeled 'coordination' by clearly identifying extraction (capital flight costs, settlement delays, regulatory uncertainty) as distinct from coordination (policy objectives, capital controls). The snare classification is primary (χ=0.75 for trapped traders, suppression=0.75) and is NOT mislabeled as rope. The beneficiary perspective (U.S. Treasury) legitimately experiences coordination (d≈0.08, χ≈-0.04) because the sanction mechanism DOES solve a policy coordination problem for Treasury — it aggregates enforcement power and creates deterrence. But this coordination is asymmetric: it coordinates extractors, not coordinators-and-extractees together. The tangled rope perspective (alternative infrastructure) correctly identifies both coordination (liquidity capture) and asymmetric extraction (secondary sanctions suppression). The piton perspective identifies the SWIFT inertial theater without mislabeling it as coordination. The analytical observer's snare perspective confirms that the constraint's fundamental structure is extraction (political gating of financial access), not a misunderstood coordination mechanism. Mandatrophy is resolved because every victim perspective correctly identifies extraction, every beneficiary perspective correctly identifies asymmetric gains, and the hybrid perspectives correctly decompose into coordination + suppression components.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secondary_sanctions_enforcement_threshold,
    'How aggressively will the U.S. pursue secondary sanctions against third-country intermediaries (SPYF, Singapore venues, CIPS settlement partners) that facilitate Russian equity trading?',
    'Analysis of OFAC enforcement patterns; tracking of Treasury warnings; measuring effective scope of ''facilitating Russian evasion'' designations',
    'If aggressive (secondary sanctions on Asian venues): alternative trading infrastructure loses benefit, collapses back to snare for organizers. If permissive (OFAC tolerates SPYF): scaffold perspective confirmed — alternatives gain sustained institutional legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_sanctions_enforcement_threshold, preference, 'U.S. enforcement intensity on third-country facilitators of Russian equity trading').

omega_variable(
    dollar_system_structural_resilience,
    'How quickly can BRICS+ de-dollarization infrastructure (CIPS, rupee settlements, direct clearing networks) achieve liquidity and settlement reliability sufficient to reduce U.S. sanction mechanism dependence?',
    'Empirical tracking of settlement volumes through CIPS vs SWIFT; measurement of liquidity spreads and clearing times in alternative systems; correlation of geopolitical tension with capital flight to alternatives',
    'If rapid (2-3 years): scaffold sunset accelerates; alternative infrastructure gains irreversible institutional mass. If slow (10+ years): MOEX sanctions remain structurally binding; snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dollar_system_structural_resilience, empirical, 'Speed of de-dollarization infrastructure maturation').

omega_variable(
    russian_equity_market_relocation,
    'Does sustained MOEX sanction pressure cause structural relocation of Russian equity trading to non-U.S.-linked venues (SPYF, Shanghai, Singapore) with permanent migration of listed companies, or temporary avoidance with expectation of eventual U.S. market access restoration?',
    'Tracking of secondary listings, delistings from MOEX alternatives, and company statements about trading venue permanence; measurement of capital allocation decisions in light of sanction continuity expectations',
    'If permanent: Russian financial system decouples from U.S. market discipline; snare transitions to piton (theater of MOEX as primary Russian venue persists while real trading migrates). If temporary: snare persists as agents expect eventual restoration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(russian_equity_market_relocation, empirical, 'Whether MOEX sanction effects are structural or temporary').

omega_variable(
    moex_as_regime_legitimacy_theater,
    'Does MOEX continue functioning as a showcase of ''normal market operations'' despite sanctions-driven liquidity collapse, maintaining theater of institutional legitimacy?',
    'Measurement of MOEX trading volumes relative to pre-sanction baselines; analysis of regulatory rhetoric vs. actual market microstructure; tracking of blue-chip company primary issuances vs. secondary trading ratios',
    'If high theater: piton classification is correct — MOEX becomes inert symbol. If low theater: MOEX sanctions represent genuine market seizure, confirming snare for all victim classes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moex_as_regime_legitimacy_theater, empirical, 'Degree to which MOEX functions as performative legitimacy vs. operational market').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_sanctions_moex_2024, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moex_sanc_tr_t0, us_sanctions_moex_2024, theater_ratio, 0, 0.42).
narrative_ontology:measurement(moex_sanc_tr_t6, us_sanctions_moex_2024, theater_ratio, 6, 0.5).
narrative_ontology:measurement(moex_sanc_tr_t12, us_sanctions_moex_2024, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(moex_sanc_be_t0, us_sanctions_moex_2024, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(moex_sanc_be_t6, us_sanctions_moex_2024, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(moex_sanc_be_t12, us_sanctions_moex_2024, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_sanctions_moex_2024, enforcement_mechanism).
narrative_ontology:affects_constraint(us_sanctions_moex_2024, cips_ruble_settlement_infrastructure).
narrative_ontology:affects_constraint(us_sanctions_moex_2024, secondary_sanctions_surveillance).
narrative_ontology:affects_constraint(us_sanctions_moex_2024, brics_currency_swap_agreements).

% DUAL FORMULATION NOTE:
% MOEX sanctions decompose into structural constraint (political gating of financial access) and temporal constraint (sunset via de-dollarization infrastructure maturation). The network upstream includes geopolitical coordination mechanisms (BRICS+ de-dollarization); the network downstream includes settlement infrastructure alternatives (CIPS, rupee clearing) that create exit pathways. Do not conflate MOEX sanctions with broader U.S. sanctions regime — this story focuses on financial market access constraint, not asset freezing or export control constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_sanctions_moex_2024, analytical, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
