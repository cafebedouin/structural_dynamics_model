% ============================================================================
% CONSTRAINT STORY: cryptocurrency_velocity_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cryptocurrency_velocity_dynamics, []).

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
 *   constraint_id: cryptocurrency_velocity_dynamics
 *   human_readable: Cryptocurrency Velocity Dynamics Constraint
 *   domain: monetary_economics/blockchain
 *
 * SUMMARY:
 *   Cryptocurrency velocity dynamics create a structural tension between two
 *   incompatible roles: cryptocurrency-as-store-of-value (requiring price
 *   stability and low velocity) and cryptocurrency-as-medium-of-exchange
 *   (requiring high velocity and transaction throughput). This tension
 *   generates extraction mechanisms where high-frequency traders and exchange
 *   operators benefit from volatility while long-term holders and merchants
 *   bear costs. The constraint exhibits classical Tangled Rope structure:
 *   genuine coordination function (enabling peer-to-peer transactions without
 *   intermediaries), genuine beneficiary class (traders capturing volatility
 *   spreads), genuine victim class (holders experiencing price erosion), and
 *   active enforcement (exchange market structures, fee incentives,
 *   transaction ordering). However, layer-2 solutions and stablecoin adoption
 *   represent genuine alternatives with sunset logic, making this a Scaffold
 *   from the perspective of organized developers. The increasing theater
 *   ratio (0.35 → 0.58) reflects the growth of 'HODLing' narrative (identity
 *   maintenance for holders experiencing extraction) rather than functional
 *   utility increase.
 *
 * KEY AGENTS:
 *   - Long-Term Holders: Primary victims (powerless/trapped) — experience extractive volatility; cannot exit without losses
 *   - High-Frequency Traders: Primary beneficiaries (institutional/arbitrage) — extract value from velocity spreads
 *   - Exchange Operators: Secondary beneficiaries (institutional/arbitrage) — capture transaction fees, market-making spreads
 *   - Liquidity Providers: Secondary beneficiaries (institutional/arbitrage) — earn velocity-driven trading spreads
 *   - Merchants: Mixed position (moderate/constrained) — gain coordination benefits but suffer extraction from volatility
 *   - Payment Protocol Developers: Organized exit builders (organized/constrained) — constructing layer-2 and L3 alternatives
 *   - HODL Community: Institutional performers (institutional/arbitrage) — maintain narrative theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cryptocurrency_velocity_dynamics, 0.52).
domain_priors:suppression_score(cryptocurrency_velocity_dynamics, 0.48).
domain_priors:theater_ratio(cryptocurrency_velocity_dynamics, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cryptocurrency_velocity_dynamics, extractiveness, 0.52).
narrative_ontology:constraint_metric(cryptocurrency_velocity_dynamics, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(cryptocurrency_velocity_dynamics, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cryptocurrency_velocity_dynamics, tangled_rope).
narrative_ontology:human_readable(cryptocurrency_velocity_dynamics, "Cryptocurrency Velocity Dynamics Constraint").
narrative_ontology:topic_domain(cryptocurrency_velocity_dynamics, "monetary_economics/blockchain").

domain_priors:requires_active_enforcement(cryptocurrency_velocity_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cryptocurrency_velocity_dynamics, high_frequency_traders).
narrative_ontology:constraint_beneficiary(cryptocurrency_velocity_dynamics, exchange_operators).
narrative_ontology:constraint_beneficiary(cryptocurrency_velocity_dynamics, liquidity_providers).
narrative_ontology:constraint_victim(cryptocurrency_velocity_dynamics, long_term_holders).
narrative_ontology:constraint_victim(cryptocurrency_velocity_dynamics, price_stability).
narrative_ontology:constraint_victim(cryptocurrency_velocity_dynamics, network_utility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Long-term cryptocurrency holders experience the velocity constraint as extractive entrapment. High transaction velocity (speculation, arbitrage, flash trading) creates price volatility that directly erodes purchasing power for holders. They cannot exit without realizing losses; they are trapped in a system where their stored value depreciates due to mechanisms they cannot control or avoid.
constraint_indexing:constraint_classification(cryptocurrency_velocity_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Merchants accepting cryptocurrency as payment face genuine coordination benefits (borderless, low-friction settlement) alongside significant extraction costs (price volatility between acceptance and conversion, double-spend risk management, compliance burden). They are constrained by infrastructure dependencies and regulatory uncertainty, not purely trapped. The coordination function is real; the asymmetric extraction is also real.
constraint_indexing:constraint_classification(cryptocurrency_velocity_dynamics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Exchange operators benefit from velocity dynamics through transaction fees, market-making spreads, and order flow. They experience the constraint as pure coordination: velocity equals transaction volume equals revenue. They have arbitrage optionality (can exit, pivot to other assets, diversify). Net experience is beneficial coordination without extraction bearing down on them.
constraint_indexing:constraint_classification(cryptocurrency_velocity_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Payment protocol developers (Lightning Network, Solana, Roll-ups, stablecoins) see velocity constraints as a temporary coordination problem with architectural sunset. Layer-2 solutions reduce velocity pressure by enabling faster settlement without on-chain transaction costs. As these mature, the velocity bottleneck's extraction force diminishes. Organized agents can implement exits; high suppression is tolerated because the sunset is real.
constraint_indexing:constraint_classification(cryptocurrency_velocity_dynamics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The 'HODL' (hold) ethos persists as theatrical performance despite diminishing functional utility. It originated as coordination for network effects but has become identity-protective narrative for holders experiencing extraction. The narrative maintains itself through community reinforcement and institutional inertia, not because it solves the velocity problem. Theater ratio reflects this performative maintenance of meaning.
constraint_indexing:constraint_classification(cryptocurrency_velocity_dynamics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational analytical perspective, velocity dynamics appear as immutable monetary economics: high velocity reduces price stability; low velocity reduces utility as medium of exchange. The constraint appears as an irreducible trade-off inherent to commodity-based money. However, structural data reveals this as false summitry — the velocity dynamics are contingent on institutional architecture (exchange design, layer-1 throughput, stablecoin supply), not natural law.
constraint_indexing:constraint_classification(cryptocurrency_velocity_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cryptocurrency_velocity_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cryptocurrency_velocity_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cryptocurrency_velocity_dynamics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cryptocurrency_velocity_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cryptocurrency_velocity_dynamics, TR),
    TR >= 0.70.

:- end_tests(cryptocurrency_velocity_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, trending upward. The constraint increases as HFT activity and professional trading dominate transaction volume relative to retail payments. Initial extractiveness (0.28) reflects early adoption period where speculation and network effects were balanced. Current value (0.52) reflects professional extraction from holders during maturation phase. Suppression (0.48): Moderate. Barriers to exit include opportunity cost of missing appreciation, sunk investments in mining/staking infrastructure, psychological commitment to early adoption, and network effects (moving to competing systems incurs friction). But suppression is not total — holders can partially exit, hedge, or diversify into stablecoins. Theater ratio (0.58): Moderately high and rising. The HODL narrative persists despite declining functional utility as a payment mechanism; it performs identity work for holders experiencing extraction rather than solving the velocity problem.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The long-term holder sees pure extraction (Snare) — velocity dynamics directly erode their purchasing power. The trader sees coordination (Rope) — they are solving the price discovery problem and providing liquidity. The merchant sees mixed (Tangled Rope) — they gain payment infrastructure but suffer volatility risk. The protocol developer sees a solvable problem with sunset (Scaffold) — layer-2 solutions are building alternatives. The HODL narrative sees its own degradation (Piton) — identity performance replacing functionality. The analytical observer risks false summitry (Mountain) — seeing velocity-stability trade-offs as inherent to monetary economics rather than contingent on current layer-1 architecture. The perspectival divergence precisely tracks the directionality derivation: beneficiaries with exit options diverge maximally from trapped victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are computed from beneficiary/victim status and exit options. Traders with arbitrage exit (d ≈ 0.15) experience low or negative effective extraction. Long-term holders trapped by opportunity cost and sunk psychology (d ≈ 0.90) experience maximum extraction. Merchants with some exit optionality but transaction risk constraints (d ≈ 0.65) experience moderate extraction. Exchange operators with full arbitrage (d ≈ 0.05) experience negative extraction (they benefit). Protocol developers with exit pathways (d ≈ 0.45) experience moderate extraction but see the Scaffold sunset. The perspectival gap between powerless holders (snare) and institutional traders (rope) directly reflects the directionality range: identical constraint, opposite classification, due to structural position in the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing genuine coordination function (price discovery, liquidity provision, settlement) coexisting with genuine extraction (holder wealth flowing to traders through volatility capture). Tangled Rope classification is robust: chi ≈ 0.70 (extractiveness 0.52 × f(d) ≈ 1.35 × scope modifier 1.2), meeting the 0.66 threshold. The classification is not a false compromise — it accurately captures that the system is both coordination and extraction. The Scaffold perspective adds critical insight: payment layer alternatives (Lightning, Solana, roll-ups) represent genuine sunset pathways where organized actors are constructing ways around the velocity extraction. The Piton perspective (HODL narrative) identifies that maintaining the current holder-as-victim structure increasingly depends on performative community identity work rather than functional role. The false mountain (analytical view) naturalizes contingent architecture — velocity-stability trade-offs are real only for monolithic layer-1 systems; they are architectural choices, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    velocity_measurement_definition,
    'What constitutes ''velocity'' in a system where multiple tokens, layers, and timeframes exist? Is on-chain velocity different from economic velocity?',
    'Operationalize velocity across different layers (L1/L2), token types (native/wrapped/stablecoin), and timeframes. Compare on-chain transaction frequency to economic transaction frequency using merchant data.',
    'If velocity is L1-only: constraint may be overstated for systems with robust L2 adoption. If economic velocity includes L2: current extractiveness (0.52) may understate the constraint''s actual functionality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(velocity_measurement_definition, empirical, 'Definition and measurement scope for velocity in multi-layer systems').

omega_variable(
    stablecoin_substitution,
    'Do stablecoins resolve the velocity-stability trade-off by decoupling velocity from volatility, or do they externalize instability to the underlying layer and collateral?',
    'Analyze stablecoin depegging events, collateral concentration, and velocity patterns in stablecoin vs native token markets. Track whether velocity externalities propagate to collateral assets.',
    'If stablecoins resolve: velocity constraint for native tokens becomes a choice (Scaffold perspective strengthened, partial sunset). If externalized: the constraint shifts rather than resolves (Piton perspective confirmed — problem persists via institutional arrangement change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stablecoin_substitution, empirical, 'Whether stablecoins resolve or externalize the velocity-stability constraint').

omega_variable(
    extraction_mechanism_attribution,
    'Is high velocity extraction driven by speculator activity extracting from long-term holders, or by legitimate network effects and price discovery?',
    'Separate transaction types: legitimate retail payments vs high-frequency trading activity vs arbitrage. Measure wealth redistribution per transaction type. Analyze price impact of HFT on merchant adoption rates.',
    'If primarily speculation: extraction is parasitic (Snare from holder perspective, Rope from trader perspective confirmed). If primarily price discovery: extraction is a coordination cost (Tangled Rope perspective shifts to higher coordination component).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_attribution, empirical, 'Attribution of extraction to speculation vs legitimate price discovery').

omega_variable(
    layer2_adoption_sufficiency,
    'Do layer-2 solutions actually reduce velocity-driven extraction, or do they fragment liquidity and create new coordination problems?',
    'Track merchant adoption, settlement finality, and cross-layer composability. Measure whether velocity pressure migrates to L2 or genuinely dissipates.',
    'If L2 solutions are sufficient: Scaffold sunset is real, extractiveness will decline as L2 adoption matures (Piton perspective confirms temporary nature). If L2 fragments: new Tangled Rope constraints emerge between layers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer2_adoption_sufficiency, empirical, 'Sufficiency of layer-2 solutions for reducing velocity extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cryptocurrency_velocity_dynamics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crypto_vel_tr_t0, cryptocurrency_velocity_dynamics, theater_ratio, 0, 0.35).
narrative_ontology:measurement(crypto_vel_tr_t3, cryptocurrency_velocity_dynamics, theater_ratio, 3, 0.42).
narrative_ontology:measurement(crypto_vel_tr_t6, cryptocurrency_velocity_dynamics, theater_ratio, 6, 0.55).
narrative_ontology:measurement(crypto_vel_tr_t10, cryptocurrency_velocity_dynamics, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(crypto_vel_be_t0, cryptocurrency_velocity_dynamics, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(crypto_vel_be_t3, cryptocurrency_velocity_dynamics, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(crypto_vel_be_t6, cryptocurrency_velocity_dynamics, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(crypto_vel_be_t10, cryptocurrency_velocity_dynamics, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cryptocurrency_velocity_dynamics, resource_allocation).
narrative_ontology:affects_constraint(cryptocurrency_velocity_dynamics, stablecoin_collateral_dependency).
narrative_ontology:affects_constraint(cryptocurrency_velocity_dynamics, exchange_market_structure).
narrative_ontology:affects_constraint(cryptocurrency_velocity_dynamics, network_effect_lock_in).

% DUAL FORMULATION NOTE:
% Cryptocurrency velocity dynamics decompose into three linked constraints: (1) on-chain velocity-stability trade-off (this story, ε=0.52, Tangled Rope), (2) stablecoin collateral fragility (downstream, ε=0.68, Snare), (3) layer-2 liquidity fragmentation (parallel, ε=0.38, Scaffold). Each has distinct beneficiary/victim structure and exit options. Stories are linked via network edges because stablecoin depegging cascades to layer-1 velocity extraction, and L2 adoption reduces L1 transaction volume, changing the constraint's extractiveness over the interval.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cryptocurrency_velocity_dynamics, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
