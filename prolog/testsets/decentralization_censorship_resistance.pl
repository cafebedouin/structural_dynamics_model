% ============================================================================
% CONSTRAINT STORY: decentralization_censorship_resistance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decentralization_censorship_resistance, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: decentralization_censorship_resistance
 *   human_readable: Decentralization Censorship-Resistance as Contested Monetary Commitment
 *   domain: monetary_economics/technology_governance/political_economy
 *
 * SUMMARY:
 *   Cryptocurrency systems, particularly Bitcoin, instantiate a structural
 *   tension between technical decentralization (distributed ledger,
 *   peer-to-peer settlement) and extractive concentration (wealth
 *   distribution skewed toward early adopters, custody consolidation by
 *   institutional finance, volatility that transfers risk to retail
 *   participants). The constraint analysis reveals that what appears as a
 *   single decentralization commitment actually represents three distinct
 *   readings of contested kernels: (1) sound-money doctrine grounded in
 *   Austrian economics (inflation hedge, store of value independent of
 *   state), (2) speculative-asset framework (tradable commodity with price
 *   driven by sentiment, not fundamentals), and (3) decentralization-ideology
 *   (anti-state coordination substrate enabling financial access). These
 *   readings have different beneficiaries, different causation mechanisms,
 *   and different temporal horizons. From different structural positions —
 *   early adopter, retail trader, unbanked aspirant, institutional finance,
 *   regulatory apparatus, analytical observer — the same technical constraint
 *   (immutable ledger, peer-to-peer settlement, no trusted intermediary)
 *   appears as pure coordination (rope), mixed coordination-extraction
 *   (tangled rope), pure extraction (snare), degraded ritual (piton), or a
 *   contestation of kernels itself. The theater ratio (0.68) reflects that
 *   regulatory compliance narratives (KYC/AML) have created an appearance of
 *   controlled-yet-decentralized finance while actually channeling
 *   transactions back through intermediaries.
 *
 * KEY AGENTS:
 *   - Early Adopters & Protocol Developers: Primary beneficiaries (institutional/arbitrage) — capture disproportionate value through pre-mine allocation, network effects, and development rewards. Low experienced extraction because they control the coordination mechanism.
 *   - Retail Traders: Secondary participants (moderate/constrained) — benefit from peer-to-peer settlement and speculative upside; bear volatility exposure and asymmetric information disadvantage. Face high transaction costs and liquidity barriers.
 *   - Unbanked Aspirants: Primary victims (powerless/trapped) — promised financial inclusion but face technical literacy barriers, irreversible transaction risk, and dependency on infrastructure they cannot control. Extraction is maximal because the mechanism is financial fragility.
 *   - Institutional Finance Sector: Organized beneficiaries (organized/constrained) — initially resisted but now derive coordination benefit from custody, derivatives, and portfolio diversification. Also derive extraction through recreating intermediary positions.
 *   - State Regulatory Apparatus: Institutional enforcer (institutional/arbitrage) — nominally opposed but performs coordination function through KYC/AML, creating regulatory theater that channels transactions through licensed intermediaries and recreates censorship points.
 *   - Analytical Observer: Observes perspectival gap (analytical/analytical) — can see that the constraint instantiates both genuine coordination (ledger immutability, peer settlement) and asymmetric extraction (wealth concentration, regulatory capture) simultaneously.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decentralization_censorship_resistance, 0.58).
domain_priors:suppression_score(decentralization_censorship_resistance, 0.62).
domain_priors:theater_ratio(decentralization_censorship_resistance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decentralization_censorship_resistance, extractiveness, 0.58).
narrative_ontology:constraint_metric(decentralization_censorship_resistance, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(decentralization_censorship_resistance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decentralization_censorship_resistance, tangled_rope).
narrative_ontology:human_readable(decentralization_censorship_resistance, "Decentralization Censorship-Resistance as Contested Monetary Commitment").
narrative_ontology:topic_domain(decentralization_censorship_resistance, "monetary_economics/technology_governance/political_economy").

domain_priors:requires_active_enforcement(decentralization_censorship_resistance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decentralization_censorship_resistance, '55749699-9fa4-4483-bb6d-1a6af53b1604').
narrative_ontology:cs_kernel_codification('55749699-9fa4-4483-bb6d-1a6af53b1604', distributed).
narrative_ontology:cs_authority_grounding('55749699-9fa4-4483-bb6d-1a6af53b1604', distributed).
narrative_ontology:cs_reading_relation('55749699-9fa4-4483-bb6d-1a6af53b1604', sound_money_reading, influences).
narrative_ontology:cs_reading_relation('55749699-9fa4-4483-bb6d-1a6af53b1604', speculative_asset_reading, influences).
narrative_ontology:cs_axiom('55749699-9fa4-4483-bb6d-1a6af53b1604', foundational, ledger_immutability_prevents_censorship).
narrative_ontology:cs_axiom_status(ledger_immutability_prevents_censorship, holdable).
narrative_ontology:cs_axiom_grounding('55749699-9fa4-4483-bb6d-1a6af53b1604', ledger_immutability_prevents_censorship, empirically_contingent).
narrative_ontology:cs_axiom('55749699-9fa4-4483-bb6d-1a6af53b1604', foundational, peer_settlement_eliminates_intermediaries).
narrative_ontology:cs_axiom_status(peer_settlement_eliminates_intermediaries, holdable).
narrative_ontology:cs_axiom_grounding('55749699-9fa4-4483-bb6d-1a6af53b1604', peer_settlement_eliminates_intermediaries, instrumental).
narrative_ontology:cs_axiom('55749699-9fa4-4483-bb6d-1a6af53b1604', secondary, distributed_validation_ensures_network_resilience).
narrative_ontology:cs_axiom_status(distributed_validation_ensures_network_resilience, overridden).
narrative_ontology:cs_axiom_grounding('55749699-9fa4-4483-bb6d-1a6af53b1604', distributed_validation_ensures_network_resilience, empirically_contingent).
narrative_ontology:cs_reference_frame('55749699-9fa4-4483-bb6d-1a6af53b1604', truly_decentralized_peer_to_peer_settlement).
narrative_ontology:cs_drift_state('55749699-9fa4-4483-bb6d-1a6af53b1604', contemporary_institutional_finance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('55749699-9fa4-4483-bb6d-1a6af53b1604', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decentralization_censorship_resistance, early_adopters).
narrative_ontology:constraint_beneficiary(decentralization_censorship_resistance, protocol_developers).
narrative_ontology:constraint_beneficiary(decentralization_censorship_resistance, mining_operators).
narrative_ontology:constraint_victim(decentralization_censorship_resistance, financial_inclusion_aspirants).
narrative_ontology:constraint_victim(decentralization_censorship_resistance, institutional_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNBANKED ASPIRANTS (SNARE) — Trapped in the narrative that cryptocurrency provides censorship-resistant financial access, but actually face high entry costs (technical literacy, volatility exposure, liquidity barriers), irreversible transaction risk, and dependency on technical infrastructure they cannot control. Extraction is maximal: the promise of inclusion is the bait; the mechanism is financial fragility.
constraint_indexing:constraint_classification(decentralization_censorship_resistance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RETAIL TRADER (TANGLED ROPE) — Faces coordination benefit (peer-to-peer settlement without intermediary, transparent ledger) alongside asymmetric extraction (extreme volatility, network effects that lock wealth at the top, susceptibility to manipulation and exit scams). Constrained by technology barriers and social capital requirements to switch; also benefits from speculative upside during bull phases.
constraint_indexing:constraint_classification(decentralization_censorship_resistance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROTOCOL DEVELOPERS & EARLY ADOPTERS (ROPE) — Primary beneficiaries with arbitrage options. Capture disproportionate value through pre-mine allocation, developer rewards, and network effects that reward early positioning. Experience the constraint as pure coordination — the censorship-resistance narrative is the coordination function that attracts users and capital. Exit options remain open (they can liquidate holdings, pivot to other projects). Low experienced extraction because they control the mechanism.
constraint_indexing:constraint_classification(decentralization_censorship_resistance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL FINANCE (TANGLED ROPE) — Organized agents (banks, hedge funds, exchanges) initially resisted but now derive coordination benefit from crypto custody, derivatives markets, and portfolio diversification mechanisms. However, systemic extraction is embedded: stablecoin collapse threatens banking system integrity, custody consolidation recreates the intermediaries censorship-resistance was supposed to eliminate, and regulatory arbitrage transfers risk to less-protected populations. Constrained by regulatory environment and reputational risk; also benefiting from speculative volatility and custody fees.
constraint_indexing:constraint_classification(decentralization_censorship_resistance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE REGULATORY APPARATUS (PITON) — Nominally opposed to decentralized financial systems but increasingly performing coordination function through KYC/AML requirements, exchange regulation, and stablecoin licensing. The enforcement theater is high: regulatory agencies claim to be controlling financial crime while actually channeling transactions through licensed intermediaries (recreating the censorship points they claim to prevent). Theater ratio is elevated because the regulatory performativity (compliance theater) has partially replaced the decentralization function; the censorship resistance itself has become a negotiated ritual.
constraint_indexing:constraint_classification(decentralization_censorship_resistance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MACRO STABILITY (TANGLED ROPE) — From a civilizational/global perspective, cryptocurrency systems coordinate peer-to-peer settlement (genuine function) while extracting from financial stability, generating concentrated wealth, and enabling regulatory arbitrage that destabilizes smaller economies. The analytical view requires both the coordination benefit and the asymmetric extraction to be simultaneously true — the system cannot fail to coordinate (hence not a snare) but cannot deliver the distributed outcome the narrative promises (hence not a rope). The perspectival gap reveals the constraint as a hybrid.
constraint_indexing:constraint_classification(decentralization_censorship_resistance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decentralization_censorship_resistance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decentralization_censorship_resistance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decentralization_censorship_resistance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decentralization_censorship_resistance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decentralization_censorship_resistance, TR),
    TR >= 0.70.

:- end_tests(decentralization_censorship_resistance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The base extractiveness measures how much the constraint extracts from those governed by it. Initial value (0.15) reflects genuine peer-to-peer coordination in early adoption phase. Current value (0.58) reflects wealth concentration (Gini coefficient of Bitcoin distribution ~95%, highest inequality of major assets), custody consolidation (top 5 exchanges control ~70% of traded volume), and regulatory gatekeeping (sanctions compliance, AML requirements). The rise over the 10-year interval reflects institutional capture of the infrastructure: as technical decentralization matured, financial and regulatory centralization increased. Suppression (0.62): High and rising. Multiple barriers prevent exit from the narrative and from financial exposure: (1) technical barriers — users require understanding of keys, wallets, addresses; (2) social pressure — "diamond hands" ideology stigmatizes exit; (3) financial barriers — once trapped in high-volatility holdings, opportunity cost of exit during bull phases is severe; (4) regulatory barriers — sanctions regimes create implicit pressure to hold rather than transact; (5) institutional barriers — stablecoin dependence recreates the intermediaries the system was supposed to eliminate. Theater ratio (0.68): High and rising. Initial theater (0.35) reflects genuine technical novelty and low regulatory interference. Current theater (0.68) reflects that most activity occurs in regulated exchanges (compliance theater), stablecoin collateral is opaque (Fed deposits? corporate IOUs?), and the decentralization narrative persists even as technical infrastructure concentrates (Nakamoto coefficient rising, custody consolidation). The theater has become the constraint itself: the appearance of censorship-resistance maintained through regulatory coordination and narrative performance rather than technical mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the clearest perspectival divergence in the corpus. Early adopters see rope (pure coordination, disproportionate reward is just first-mover advantage). Retail traders see tangled rope (real settlement coordination mixed with extreme volatility extraction). Unbanked aspirants see snare (promised inclusion, actual financial fragility). Institutional finance sees rope (now benefits from custody and derivatives). State apparatus sees piton (maintains regulatory theater, performs opposition while channeling transactions through intermediaries). Analytical observer sees tangled rope with rising extraction (genuine coordination substrate captured by institutional finance and regulatory systems). The perspectival gap reveals that the constraint cannot be accurately classified from a single position — the classification requires seeing how different agents experience the same technical mechanism (immutable ledger) as serving opposed structural functions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options. Early adopters are beneficiaries (d ≈ 0.05) with arbitrage options, experiencing negative effective extraction (the constraint subsidizes them). Unbanked aspirants are victims (d ≈ 0.95) with trapped exit, experiencing maximum extraction (f(d) ≈ 1.42). Retail traders are victims (d ≈ 0.75) with constrained exit (technical barriers, switching costs), experiencing high extraction (f(d) ≈ 1.15). Institutional finance began as victim (d ≈ 0.85) but shifted to beneficiary (d ≈ 0.20) as custody infrastructure matured — this shift explains why the institutional perspective moved from snare-like (2015) to rope-like (2023). State apparatus remains beneficiary (d ≈ 0.10) with arbitrage options — they control the regulatory gates that define what is legal, hence low experienced extraction. The spread in d values (0.05 to 0.95) produces the perspectival divergence: some agents experience nearly-zero effective extraction while others experience maximum. The chi formula χ = ε × f(d) × σ(S) amplifies this divergence: global scope (σ = 1.2) scales χ differently for each agent, meaning the same 0.58 base extractiveness becomes wildly different effective extractiveness depending on position.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through kernel-reading decomposition. The constraint cannot be classified as a single type because the base kernel is contested: (1) If sound-money reading is correct (Bitcoin as inflation hedge): constraint should be Rope or Mountain (coordination without extraction, or natural law). Current extractiveness 0.58 contradicts this. (2) If speculative-asset reading is correct: constraint should be Snare (pure extraction through sentiment cycles, no coordination function). But coordination function (peer settlement, ledger immutability) is demonstrably real. (3) If decentralization-ideology reading is correct: constraint should be Scaffold (temporary, sunset when state adopts blockchain or regulation stabilizes) or Rope (pure coordination). But wealth concentration and custody consolidation are rising, not falling. None of the three readings fully contain the observed structural data. The resolution is that the constraint IS a reading-dependent classification, and the mandate is to show that from different positions, the same technical mechanism produces different perceived types. The engine's perspectival-gap analysis proves that no single type is mandatorily correct — the type depends on the observer's structural position. This is not mandatrophy failure (the system made a mistake) — it is mandatrophy resolution via perspectival pluralism (the system correctly shows that classification is position-dependent).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sound_money_vs_speculative_asset,
    'Is the cryptocurrency constraint a reading of sound-money doctrine (inflation hedge, store of value independent of state) or a distinct speculative-asset kernel with no fundamental anchor?',
    'Historical correlation analysis between crypto prices and macroeconomic variables (inflation, real interest rates, broad money aggregates); assessment of whether price movements are consistent with sound-money thesis or pure speculative cycles',
    'If sound-money reading: constraint is temporary (fiat currency failure leads to adoption). If speculative-asset reading: constraint exhibits boom-bust oscillation with no equilibrium. If both coexist: the kernel itself is contested and the constraint is reading-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sound_money_vs_speculative_asset, empirical, 'Whether cryptocurrency instantiates sound-money or speculative-asset thesis').

omega_variable(
    decentralization_narrative_capture,
    'Has the decentralization narrative become purely extractive cover story while institutional finance has captured the technical infrastructure (exchange consolidation, stablecoin collateral, custody oligopoly)?',
    'Network analysis of transaction flow: measure percentage of transactions routed through regulated intermediaries (exchanges, custodians); compare Nakamoto coefficient (minimum number of entities required to halt network) over time; assess whether custody consolidation reproduces banking system centralization',
    'If narrative capture: constraint reclassifies as snare (extraction hidden by decentralization ideology). If genuine decentralization persists: constraint remains tangled rope (real coordination with asymmetric distribution). Evidence of custody consolidation and exchange gatekeeping strengthens the capture interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_narrative_capture, empirical, 'Whether decentralization narrative masks institutional capture').

omega_variable(
    financial_inclusion_actualization,
    'Does censorship-resistant cryptocurrency actually improve financial access for unbanked populations, or does it concentrate risk on the least protected agents while creating the appearance of inclusion?',
    'Longitudinal studies of cryptocurrency adoption in remittance-dependent economies; measurement of transaction costs (fees + volatility exposure) vs traditional banking; assessment of exit pathways (ability to convert back to fiat) and technical support infrastructure',
    'If actualized: the unbanked perspective is constrained but improving (tangled rope). If appearance only: the unbanked perspective is trapped extraction (snare) and the constraint naturalizes financial fragility as liberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_inclusion_actualization, empirical, 'Whether cryptocurrency improves financial inclusion or concentrates risk on unbanked agents').

omega_variable(
    censorship_resistance_definition_ambiguity,
    'What constitutes censorship-resistant financial infrastructure: immunity to transaction blocking (protocol-level), or accessibility to users regardless of state surveillance (practical-level)?',
    'Comparative case analysis: assess whether high-frequency transaction censoring (state-mandated exchange blocking, sanctions compliance) constitutes constraint success or failure under each definition',
    'If protocol-level: constraint succeeds (Bitcoin ledger is immutable). If practical-level: constraint fails (users require intermediaries to access value, creating new censorship points). The gap between these definitions is where the narratives diverge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(censorship_resistance_definition_ambiguity, conceptual, 'Ambiguity in what censorship-resistance means operationally').

omega_variable(
    kernel_plurality_hypothesis,
    'Are sound-money, speculative-asset, and decentralization-ideology three readings of one shared kernel, or three distinct kernels that share only a name (Bitcoin/cryptocurrency)?',
    'Examination of canonical texts (Nakamoto whitepaper, Austrian school literature, tech decentralization literature); identification of whether adherents claim shared foundational commitment or merely contingent alliance',
    'If shared kernel: the constraint is reading-dependent but unified (three perspectival readings of one commitment system). If distinct kernels: there is no single decentralization_censorship_resistance constraint — there are three separate constraints misidentified as one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_plurality_hypothesis, conceptual, 'Whether constraint is unified kernel with multiple readings or three distinct kernels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decentralization_censorship_resistance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcr_tr_t0, decentralization_censorship_resistance, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dcr_tr_t3, decentralization_censorship_resistance, theater_ratio, 3, 0.52).
narrative_ontology:measurement(dcr_tr_t6, decentralization_censorship_resistance, theater_ratio, 6, 0.63).
narrative_ontology:measurement(dcr_tr_t10, decentralization_censorship_resistance, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(dcr_be_t0, decentralization_censorship_resistance, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(dcr_be_t3, decentralization_censorship_resistance, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(dcr_be_t6, decentralization_censorship_resistance, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(dcr_be_t10, decentralization_censorship_resistance, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dcr_su_t0, decentralization_censorship_resistance, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dcr_su_t5, decentralization_censorship_resistance, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(dcr_su_t10, decentralization_censorship_resistance, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decentralization_censorship_resistance, resource_allocation).
narrative_ontology:affects_constraint(decentralization_censorship_resistance, stablecoin_collateral_stability).
narrative_ontology:affects_constraint(decentralization_censorship_resistance, regulatory_arbitrage_vulnerability).
narrative_ontology:affects_constraint(decentralization_censorship_resistance, wealth_concentration_amplification).

% DUAL FORMULATION NOTE:
% The decentralization_censorship_resistance constraint is upstream of three distinct infrastructure constraints: stablecoin collateral (depends on crypto custody and asset backing, inherits extractiveness from custody consolidation), regulatory arbitrage (created by the gap between jurisdiction rules, enables extraction at border points), and wealth concentration (amplified by Nakamoto coefficient dynamics). Each downstream constraint inherits the base extractiveness while adding its own mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decentralization_censorship_resistance, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
