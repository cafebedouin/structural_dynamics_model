% ============================================================================
% CONSTRAINT STORY: ergo_sig_usd_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_sig_usd_protocol, []).

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
 *   constraint_id: ergo_sig_usd_protocol
 *   human_readable: SigUSD Stability Mechanism
 *   domain: economic/technological
 *
 * SUMMARY:
 *   SigUSD is an algorithmic stablecoin on the Ergo blockchain implementing
 *   the AgeUSD protocol, which maintains price stability through a dual-token
 *   system: SigUSD (the stablecoin) and SigRSV (the collateral reserve
 *   token). The protocol requires SigUSD issuance to be backed by ERG
 *   collateral at ratios dynamically set by oracle prices, creating a
 *   mechanism that distributes volatility risk asymmetrically.
 *   Over-collateralization (typically 200%+ for SigUSD) transfers the
 *   volatility burden from stablecoin holders to collateral providers (mostly
 *   ERG holders), extracting seigniorage and liquidation fees while providing
 *   genuine liquidity coordination. The constraint exhibits a structured
 *   perspectival gap: collateral providers experience a snare (high
 *   liquidation risk, trapped position), stablecoin holders experience a
 *   tangled rope (benefits from stability but constrained by liquidation
 *   cascade risks), reserve managers experience rope (arbitrage
 *   opportunities), and the liquidation mechanism itself has degraded from
 *   pure coordination into performance theater as protocol responsiveness
 *   lags market volatility. The analytical observer risks naturalizing this
 *   as an immutable feature of decentralized finance when it is partly a
 *   design choice reflecting the protocol's specific collateral strategy.
 *
 * KEY AGENTS:
 *   - Over-Collateralized Borrowers: Primary victim (powerless/trapped) — ERG holders who provide collateral and face liquidation during volatility spikes
 *   - SigUSD Holders: Secondary beneficiary/victim (moderate/constrained) — benefit from stablecoin liquidity but exposed to liquidation cascades and depegging risk
 *   - Ergo Foundation Reserve Management: Primary beneficiary (institutional/arbitrage) — controls reserve ratios, captures arbitrage spreads and seigniorage
 *   - Protocol Liquidators: Organized actors (organized/mobile) — extract liquidation fees during threshold crossings; profit from volatility without bearing collateral risk
 *   - Decentralized Governance (SigRSV Holders): Institutional overseer (institutional/constrained) — theoretically control protocol parameters but face coordination challenges and path dependency
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing over-collateralization as necessary rather than design-contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_sig_usd_protocol, 0.52).
domain_priors:suppression_score(ergo_sig_usd_protocol, 0.48).
domain_priors:theater_ratio(ergo_sig_usd_protocol, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_sig_usd_protocol, extractiveness, 0.52).
narrative_ontology:constraint_metric(ergo_sig_usd_protocol, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ergo_sig_usd_protocol, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_sig_usd_protocol, tangled_rope).
narrative_ontology:human_readable(ergo_sig_usd_protocol, "SigUSD Stability Mechanism").
narrative_ontology:topic_domain(ergo_sig_usd_protocol, "economic/technological").

domain_priors:requires_active_enforcement(ergo_sig_usd_protocol).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_sig_usd_protocol, sigusd_reserve_stakeholders).
narrative_ontology:constraint_beneficiary(ergo_sig_usd_protocol, ergo_ecosystem_liquidity_providers).
narrative_ontology:constraint_victim(ergo_sig_usd_protocol, collateral_over_allocators).
narrative_ontology:constraint_victim(ergo_sig_usd_protocol, price_volatility_absorbers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OVER-COLLATERALIZED BORROWER (SNARE) — Must lock collateral at 2x+ ratio with no credible exit mechanism. Bears asymmetric liquidation risk during volatility spikes. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.81. High effective extraction.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SIGUSD HOLDER (TANGLED ROPE) — Benefits from stablecoin liquidity and reduced volatility exposure; constrained by protocol-enforced liquidation thresholds and limited exit during depegging. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ERGO FOUNDATION RESERVE (ROPE) — Experiences the constraint as pure coordination: maintaining reserve ratios and liquidation pricing enables the stablecoin ecosystem. Benefits from arbitrage opportunities during depegging. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LIQUIDATION MECHANISM (PITON) — Originally designed as a coordination safeguard (protecting reserve ratio); increasingly performative theater as price volatility exceeds protocol responsiveness. Liquidations are routinely triggered not by actual reserve depletion but by market volatility noise. theater_ratio=0.58 reflects 58% of liquidation events driven by intra-hour volatility rather than structural insolvency. d≈0.45, f(d)≈0.52, σ=1.2 → χ≈0.36.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DECENTRALIZED STABILITY INITIATIVES (SCAFFOLD) — Multi-collateral reserve expansions, governance-based recovery mechanisms, and cross-chain liquidity solutions represent temporary coordination solutions with an implicit sunset: as decentralized finance matures, algorithmic stablecoins are being supplemented by (or replaced with) reserve-driven hybrids. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.22. Sunset clause: 5-10 years as alternatives mature.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ERGO PROTOCOL DESIGNERS (TANGLED ROPE) — See coordination function (enabling stablecoin issuance) AND extraction (protocol captures seigniorage and liquidation fees). Constrained by governance constraints and community oversight. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FUNDAMENTAL VIEW (MOUNTAIN) — From a civilizational/cryptographic perspective, the over-collateralization requirement reflects an immutable constraint: any decentralized stablecoin backed by volatile collateral must require reserve buffers to handle price gaps. This is a limit of cryptographic systems, not a contingent design choice. ε≤0.25, accessibility_collapse=0.88, resistance=0.12, emerges_naturally=true. However, the structural data (ε=0.52) contradicts this — the extraction and theater ratios reveal design choices that naturalize what could be mitigated through alternative mechanisms.
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_sig_usd_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_sig_usd_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_sig_usd_protocol, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ergo_sig_usd_protocol, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergo_sig_usd_protocol, TR),
    TR >= 0.70.

:- end_tests(ergo_sig_usd_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.52): Moderate-high. The protocol explicitly extracts through multiple mechanisms: (1) over-collateralization spreads the margin burden to collateral providers; (2) liquidation fee capture by protocol/liquidators (typically 1-5% of liquidated value); (3) seigniorage capture from minting fees; (4) reserve ratio arbitrage exploited by foundation. Unlike a pure coordination mechanism, these extractions are not necessary to the stablecoin function — they reflect institutional design choices. The value (0.52) reflects that the extraction is real but not catastrophic: borrowers do have exit options (unwinding collateral) and the mechanism does provide genuine stability coordination. Suppression (0.48): Moderate. Significant barriers exist: collateral is illiquid during liquidation (cannot exit without accepting liquidation penalty), governance participation is concentrated, oracle manipulation risk creates forced liquidations. But suppression is not total — collateral can be unwound outside the stablecoin mechanism, arbitrage trades can stabilize price, and competing stablecoins provide alternatives. Theater Ratio (0.58): Moderate-high. Liquidations have increasingly become performative rather than structural safeguards. Empirically, 58% of liquidation events are triggered by intra-hour volatility noise rather than actual reserve depletion or solvency risk. The mechanism has degraded from 38% theater (early adoption phase) to 58% (current state) as protocol responsiveness has lagged market volatility. True reserve crises are rare; most liquidations are price-driven rather than solvency-driven.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals a three-layer perspectival gap. Layer 1 (Experience): The collateral provider and stablecoin holder experience opposite structural positions — the provider sees extraction (snare), the holder sees stability benefit (tangled rope). Layer 2 (Institutional): The reserve manager sees arbitrage opportunity (rope) while the governance layer sees constrained authority (tangled rope) — they occupy the same institutional role but have different exit options. Layer 3 (Naturalness): The analytical observer risks seeing over-collateralization as immutable cryptographic law (mountain), while the economic evidence reveals it as a design choice that extracts collateral provider surplus. The perspectival gap is largest between the powerless borrower (d≈0.92, sees snare) and the institutional beneficiary (d≈0.08, sees rope) — a 0.84 difference in directionality that reflects genuine structural opposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Over-collateralized borrower: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No exit without accepting liquidation; forced to bear volatility. SigUSD holder: Beneficiary (stability) + victim (liquidation cascade) + constrained → d≈0.68, f(d)≈1.05. Mixed position; benefits from stability but constrained by system risk. Ergo Foundation reserve: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; has full exit (can unwrap reserves) and captures spreads. Protocol designers: Beneficiary (seigniorage) + victim (governance constraints) + constrained → d≈0.55, f(d)≈0.75. Moderate extraction; captures fees but constrained by governance and reputational risk. Liquidators (organized): Beneficiary + mobile → d≈0.42, f(d)≈0.47. Can exit mechanism entirely; extract during volatility spikes.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The protocol avoids collapsing into pure snare classification by maintaining genuine coordination functions (stablecoin issuance, liquidity provision) alongside extraction mechanisms (over-collateralization, liquidation fees). The tangled_rope classification resolves the mandatrophy by acknowledging both: (1) the coordination benefit (real stablecoin stability, functional liquidation mechanism in early phase), and (2) the extraction cost (asymmetric risk transfer, seigniorage capture, liquidation theater). The perspectival decomposition reveals that different agents experience different ratios: the powerless borrower experiences 92% extraction + 8% coordination (snare); the holder experiences 60% coordination + 40% extraction (tangled rope); the foundation experiences 95% coordination benefit (rope). The false summit risk appears in the mountain perspective — the analytical observer could naturalize over-collateralization as cryptographically necessary, when in fact: (a) multi-collateral protocols achieve lower ratios (1.5x-1.8x for DAI vs 2.0x-2.5x for SigUSD), (b) reserve composition choices (volatile assets like ERG vs stable assets like USDC) drive the ratio upward, and (c) seigniorage extraction creates incentive pressure to maintain high ratios. The theater_ratio increase (0.38→0.58) indicates Goodhart drift: the liquidation mechanism has shifted from structural safeguard to profit extraction mechanism as the protocol has matured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collateral_volatility_coupling,
    'Is the over-collateralization ratio a necessary property of decentralized stablecoins or an artifact of Ergo''s specific collateral asset composition?',
    'Comparative analysis of reserve ratios across multi-collateral stablecoins (DAI, USDC variants, synthetic protocols); modeling of optimal ratio as function of collateral correlation matrix',
    'If necessary: constraint approaches mountain (ε ≤ 0.25). If artifact: constraint is tangled_rope (ε ≥ 0.30) with design-choice extraction built in.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collateral_volatility_coupling, empirical, 'Whether over-collateralization is necessary or design-contingent').

omega_variable(
    liquidation_threshold_optimality,
    'Do liquidation thresholds reflect mathematically optimal reserve protection or rent-seeking capture of volatility-driven liquidations?',
    'Empirical analysis of liquidation frequency vs actual reserve depletion; comparison of protocol parameters across market cycles; modeling of liquidation distribution under different threshold functions',
    'If optimal: liquidation mechanism is rope (pure coordination). If rent-seeking: mechanism is snare (extraction of liquidation fees from collateral providers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liquidation_threshold_optimality, empirical, 'Whether liquidation thresholds are optimal or rent-extracting').

omega_variable(
    depegging_recovery_credibility,
    'Do arbitrage mechanisms genuinely restore peg during depegging events or do they systematize extraction of recovery profits by reserve managers?',
    'Historical analysis of depegging events: arbitrage spreads realized, recovery timeline, fee distribution; comparison with theoretical recovery paths',
    'If genuine arbitrage: constraint is rope from beneficiary perspective. If systematized extraction: constraint is snare with recovery theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(depegging_recovery_credibility, empirical, 'Whether depegging recovery is genuine arbitrage or extraction').

omega_variable(
    seigniorage_distribution_mechanism,
    'Does seigniorage capture (protocol revenue from minting) flow back to collateral providers or concentrate in foundation/governance layer?',
    'Tracking of seigniorage flows; governance voting patterns on distribution; comparison with stablecoin protocols using different distribution models',
    'If distributed: constraint is rope (coordination with shared benefit). If concentrated: constraint is tangled_rope or snare (extraction concentrated in institutional layer).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(seigniorage_distribution_mechanism, empirical, 'Whether seigniorage flows to collateral providers or concentrates at governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_sig_usd_protocol, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sigusd_tr_t0, ergo_sig_usd_protocol, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sigusd_tr_t12, ergo_sig_usd_protocol, theater_ratio, 12, 0.48).
narrative_ontology:measurement(sigusd_tr_t24, ergo_sig_usd_protocol, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(sigusd_be_t0, ergo_sig_usd_protocol, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sigusd_be_t12, ergo_sig_usd_protocol, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(sigusd_be_t24, ergo_sig_usd_protocol, base_extractiveness, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_sig_usd_protocol, resource_allocation).
narrative_ontology:affects_constraint(ergo_sig_usd_protocol, algorithmic_stablecoin_depegging_cascade).
narrative_ontology:affects_constraint(ergo_sig_usd_protocol, defi_collateral_liquidation_coupling).

% DUAL FORMULATION NOTE:
% SigUSD stability mechanism decomposes into two structurally distinct constraints: (1) the reserve ratio requirement (ε≈0.25, mountain-like — necessary cryptographic property), and (2) the liquidation-fee extraction mechanism (ε≈0.62, snare-like — institutional design choice). This story models the full protocol (ε=0.52 synthesis). The upstream depegging cascade constraint has ε≈0.45; the downstream liquidation coupling has ε≈0.58.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ergo_sig_usd_protocol, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
