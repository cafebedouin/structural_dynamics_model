% ============================================================================
% CONSTRAINT STORY: speculation_volatility_engine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speculation_volatility_engine, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speculation_volatility_engine
 *   human_readable: Cryptocurrency Speculation-Volatility Coordination Constraint
 *   domain: political_economy/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   Cryptocurrency's structural constraint system exhibits a deep
 *   coherence-boundary problem: the same technical system (distributed
 *   ledger, open-source code, cryptographic proof of work) is simultaneously
 *   narrated as three incompatible commitments — sound money (Austrian
 *   economics, scarcity-based value), speculative asset (institutional
 *   trading, volatility capture), and decentralization ideology (cypherpunk
 *   disintermediation, censorship resistance). The research flags this as a
 *   potential contested kernel: one stabilized commitment interpreted through
 *   three competing readings, each with its own authority structure and
 *   legitimacy claim. The speculation-volatility-engine constraint captures
 *   the mechanism that enables all three narratives to coexist: volatility
 *   itself is the unifying principle. It enables speculation (traders profit
 *   from price variance), it incentivizes early adoption (risk premium
 *   attracts believers), it funds development (transaction fees + block
 *   rewards), and it obscures concentration (whale addresses hide in
 *   volatility noise). The constraint exhibits all six DR types from
 *   different observer positions, making it a diagnostic exemplar for
 *   reading-dependent classification. The expanding theater ratio (from 0.28
 *   to 0.71 across the interval) reveals the degradation pattern: as the
 *   system matures, the decentralization narrative persists with increasing
 *   effort despite contradictory structural reality (exchange gatekeeping,
 *   whale concentration, institutional dominance). The rising base
 *   extractiveness (0.12 to 0.62) reflects the accumulation of extraction
 *   mechanisms: initial price discovery (0.12) → retail entry volatility
 *   (0.31) → institutional financialization (0.48) → leverage cycles and
 *   liquidation cascades (0.58+). The constraint's claimed tangled_rope type
 *   reflects that genuine coordination (distributed settlement, uncensorable
 *   transactions, programmable payments) coexists with asymmetric extraction
 *   (whale advantage, institutional trading edge, retail liquidations). This
 *   is structurally the constraint is a hybrid: coordination function is
 *   real, extraction is real, and neither would exist without the other.
 *
 * KEY AGENTS:
 *   - Early adopters (2010-2013): Primary beneficiaries (institutional/arbitrage) — captured wealth concentration before institutional entry; narrative authority over 'cypherpunk mission'
 *   - Retail speculators (ongoing): Primary victims (powerless/trapped) — entry via FOMO narrative, exit blocked by losses and psychological lock-in; volatility extraction mechanism target
 *   - Institutional traders: Secondary beneficiaries (institutional/arbitrage) — exploit volatility through derivatives, leverage, and information asymmetry; low-cost exit to other markets
 *   - Long-term hodlers / believers: Mixed (moderate/identity_locked, constrained) — structurally mobile (could sell) but identity-fused with decentralization mission; capital at risk; participate in coordination benefits but bear extraction costs
 *   - Protocol developers: Secondary beneficiaries (organized/mobile) — benefit from transaction fees, block rewards, governance token appreciation; control protocol evolution; can exit to other projects
 *   - Exchange operators: Institutional beneficiaries (institutional/arbitrage) — extract through custody, trading fees, data monopoly; gatekeepers on both on-ramps and volatility access
 *   - Regulatory coalitions: Organized actors (organized/constrained) — attempting to build sunset architecture through stablecoins and custody standards; see constraint as temporary coordination failure to be engineered away
 *   - Decentralization narrative custodians: Institutional (institutional/arbitrage) — maintain theater of decentralization through rhetoric despite concentration; benefit from narrative authority
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (early-adopter dominance, exchange power) as inherent mathematical limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speculation_volatility_engine, 0.58).
domain_priors:suppression_score(speculation_volatility_engine, 0.52).
domain_priors:theater_ratio(speculation_volatility_engine, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speculation_volatility_engine, extractiveness, 0.58).
narrative_ontology:constraint_metric(speculation_volatility_engine, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(speculation_volatility_engine, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speculation_volatility_engine, tangled_rope).
narrative_ontology:human_readable(speculation_volatility_engine, "Cryptocurrency Speculation-Volatility Coordination Constraint").
narrative_ontology:topic_domain(speculation_volatility_engine, "political_economy/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(speculation_volatility_engine).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(speculation_volatility_engine, distributed).
narrative_ontology:cs_authority_grounding(speculation_volatility_engine, distributed).
narrative_ontology:cs_reading_relation(speculation_volatility_engine, sound_money_reading, coexists_with).
narrative_ontology:cs_reading_relation(speculation_volatility_engine, decentralization_governance_reading, coexists_with).
narrative_ontology:cs_reading_relation(speculation_volatility_engine, speculation_asset_reading, coexists_with).
narrative_ontology:cs_axiom(speculation_volatility_engine, foundational, distributed_ledger_enables_value_transfer).
narrative_ontology:cs_axiom_status(distributed_ledger_enables_value_transfer, holdable).
narrative_ontology:cs_axiom(speculation_volatility_engine, secondary, volatility_funds_network_development).
narrative_ontology:cs_axiom_status(volatility_funds_network_development, holdable).
narrative_ontology:cs_axiom(speculation_volatility_engine, secondary, open_source_code_ensures_decentralization).
narrative_ontology:cs_axiom_status(open_source_code_ensures_decentralization, overridden).
narrative_ontology:cs_reference_frame(speculation_volatility_engine, cypherpunk_monetary_autonomy).
narrative_ontology:cs_drift_state(speculation_volatility_engine, institutional_capture_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speculation_volatility_engine, early_adopters).
narrative_ontology:constraint_beneficiary(speculation_volatility_engine, institutional_traders).
narrative_ontology:constraint_beneficiary(speculation_volatility_engine, protocol_developers).
narrative_ontology:constraint_victim(speculation_volatility_engine, retail_speculators).
narrative_ontology:constraint_victim(speculation_volatility_engine, long_term_hodlers).
narrative_ontology:constraint_victim(speculation_volatility_engine, monetary_stability_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL SPECULATOR (SNARE) — Trapped by the volatility extraction mechanism itself. Entry point is narrative allure (wealth-building story); exit is blocked by sunk losses and psychological lock-in. The constraint suppresses alternatives (traditional savings, diversified assets) through volatility spikes that generate FOMO and panic selling. No effective coordination benefit — the speculator is mining value extraction, not coordinating resource allocation. Maximum extraction relative to power.
constraint_indexing:constraint_classification(speculation_volatility_engine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LONG-TERM HODLER / CYPHERPUNK BELIEVER (TANGLED ROPE) — Structurally constrained by capital lock-in and career identity fusion with the protocol, but also participates in the genuine coordination benefit: distributed ledger infrastructure, censorship resistance, programmable settlement. The constraint is both: (a) extraction through volatility volatility enables institutional entry at high valuations, then extraction downward, and (b) coordination through protocol development and decentralized network effects. Identity-locked exit: the agent's self-concept is fused with the decentralization mission. Constrained exit: significant capital at risk and reputational cost if abandoning the movement.
constraint_indexing:constraint_classification(speculation_volatility_engine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL TRADER / MARKET MAKER (ROPE) — Experiences the constraint as pure coordination: volatility IS the signal. The trading infrastructure (exchanges, derivatives, lending pools) solves the price discovery problem at scale. Extraction flows toward this agent but is framed as fair compensation for liquidity provision. High arbitrage options (can move between crypto, forex, commodities markets). Net beneficiary — the volatility is their operating environment and profit source, but they do provide genuine coordination service (market depth, settlement efficiency).
constraint_indexing:constraint_classification(speculation_volatility_engine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM COALITION (SCAFFOLD) — Organized agents (SEC, CFTC, global regulators, stablecoin developers) see the volatility engine as a temporary coordination failure that can be solved through institutional embedding. Stablecoin frameworks, custody standards, and derivative clearing houses are building a sunset architecture: the volatility extraction mechanism loses force as on-ramps become regulated and spot markets are backstopped by institutional confidence. Low experienced extraction because organized actors see an exit path and control the institutional engineering of the sunset. Theater ratio lower than raw constraint — enforcement is structured toward function, not ritual.
constraint_indexing:constraint_classification(speculation_volatility_engine, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DECENTRALIZATION NARRATIVE / ORIGINAL PROTOCOL VISION (PITON) — The original cypherpunk mission (censorship resistance, monetary sovereignty, disintermediation) persists as institutional narrative long after the structural reality has shifted toward institutional concentration. Exchanges are centralized gatekeepers. Mining pools are oligopolies. Whale addresses control >50% of supply on many chains. The decentralization theater remains high (code is open-source, nodes can run locally) but the functional governance has atrophied into protocol developer dominance and exchange power. The piton classification derives from the high theater_ratio (0.68): the system performs decentralization without delivering it. Volatility extraction persists because the narrative covers the mechanics.
constraint_indexing:constraint_classification(speculation_volatility_engine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MARKET EFFICIENCY VIEW (MOUNTAIN) — From a civilizational perspective, this constraint could appear as an immutable natural law: volatility in an unregulated, globally-distributed asset class is inherent to price discovery without institutional anchors. The constraint emerges naturally from the interaction of no central bank backstop + 24/7 trading + thin order books in early years + information asymmetries. Accessibility collapse > 0.85 from this view (no one can escape price discovery dynamics) and resistance ≤ 0.15 (volatility is built into the system). However, the structural data contradicts mountain classification — identifiable beneficiaries (early adopters, traders, developers) extract disproportionate value, and suppression is enforced through narrative (mining difficulty, token scarcity) rather than physics. Engine will flag as false summit.
constraint_indexing:constraint_classification(speculation_volatility_engine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speculation_volatility_engine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speculation_volatility_engine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speculation_volatility_engine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(speculation_volatility_engine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(speculation_volatility_engine, TR),
    TR >= 0.70.

:- end_tests(speculation_volatility_engine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The constraint extracts value from retail speculators through volatility, but this is a hybrid mechanism — some volatility is functional price discovery, some is exploitable variation. The measurement trajectory from 0.12 to 0.62 shows accumulation of extraction layers: initial speculation was genuine risk premium discovery (buyers and sellers learning prices), but as institutional traders entered (2016+), the mechanism became optimized for extraction through derivatives, leverage, and wash trading (before exchange regulation). Current value (0.58) represents the equilibrium post-institutional-entry where volatility is no longer price discovery but exploitation. Suppression (0.52): Moderate-high. Multiple barriers prevent victims from exiting: psychological (sunk-cost fallacy, narrative lock-in), economic (losses prevent exit without loss realization), and institutional (limited off-ramps in early years, then exchange gatekeeping). Suppression is enforced through narrative (mining difficulty, scarcity mythology) and design (block reward halvings create false scarcity). Theater ratio (0.68): High and rising. The decentralization narrative (0.28 → 0.71 across interval) increases as concentration increases, indicating performative compensation. Early years (0.28) had lower theater because the system WAS genuinely decentralized (few miners, simple nodes). Later years (0.71) maintain the narrative despite whale concentration, exchange dominance, and mining pool oligopoly. The theater is not deception in intent but structural: the code remains open-source and nodes can run locally, but governance has migrated to institutional actors and network effects have created unavoidable centralization.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies the coherence-boundary problem. The three readings (sound money, speculation asset, decentralization) are held simultaneously by overlapping populations, and the reading is partially determined by the agent's structural position. Early adopters read primarily sound-money (Austrian scarcity narrative, wealth-preservation ideology). Institutional traders read primarily speculation-asset (volatility as signal, market microstructure logic). Protocol developers and long-term believers read primarily decentralization (cypherpunk disintermediation, censorship resistance). But the underlying constraint is unified: volatility enables all three readings to coexist. Remove volatility (stabilize the price) and the constraint collapses — sound money loses its risk premium, speculation loses its trading opportunity, and decentralization loses its development incentive. This unified mechanism creates a perspectival gap not just across power levels but across interpretive frameworks. The analytical observer who tries to adjudicate which reading is 'correct' will fail: all three are coherent within their respective authority structures (Austrian economic theory, market microstructure theory, cryptographic autonomy theory). The engine's correct response is to flag this as a contested kernel: one technical system, three incompatible legitimacy grounds.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural relationship to the extraction flow. Early adopters and institutional traders are beneficiaries with arbitrage options (low d, negative χ contribution). Retail speculators are victims with trapped exit (high d, high f(d), high χ). Long-term believers are identity-locked victims with constrained exit (high d due to victim status, but f(d) is moderate because identity-locked at biographical time produces rope from the identity frame, not mountain immutability). Developers occupy a mixed position (beneficiaries but with organized rather than institutional power, giving them higher agency than beneficiaries with institutional-only power). Regulatory coalitions are organized but constrained (not beneficiaries, not full victims, but actors attempting to reshape the constraint). The perspectival gap emerges because beneficiaries (low d, negative χ) experience rope or coordination, while victims (high d, high χ) experience snare or extraction. The identity-locked perspective (2nd perspective: long-term hodler) is crucial: it shows that an agent with constrained exit to the volatility system can perceive it as rope (coordination through protocol development, network effects) if their identity frame centers the decentralization mission rather than the wealth extraction mechanism. This is not self-deception but a genuine perceptual reframing enabled by identity fusion.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is UNRESOLVED in the traditional sense: the constraint does not decompose cleanly into 'this is coordination' or 'this is extraction.' The tangled_rope classification captures this: volatility is both the coordination mechanism (enables price discovery and development incentives) and the extraction mechanism (enables institutional profit-taking and retail liquidation). The mandatrophy resolution lies in identifying that the threat is not internal logical contradiction but external structural change. The scaffold perspective offers a genuine sunset path: if regulated on-ramps and stablecoins mature, the volatility extraction mechanism loses force because entry/exit is no longer gated by whale-driven price spikes. This would compress the spectrum: sound-money reading would remain (immutable scarcity), speculation reading would collapse (stable prices remove trading opportunity), decentralization reading would either sharpen (if coordination functions without volatility) or degrade (if development incentives were volatility-dependent). The current constraint persists because all three readings feed the volatility engine. Resolution of mandatrophy requires external intervention (regulation) or internal technical redesign (stable pricing, modified incentive structures), not internal clarification of existing logics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_three_constraints,
    'Is cryptocurrency one contested kernel with three competing readings (sound money, speculation asset, decentralization), or three structurally distinct constraints masquerading under one label?',
    'Coherence-boundary test: if the three readings share a single legitimacy claim structure (e.g., all ground authority in the protocol''s mathematical properties), they are readings of one kernel. If they ground legitimacy in incompatible claims (Austrian monetary theory vs. market microstructure vs. cryptographic autonomy), they are distinct kernels requiring separate constraint stories. Apply cs_structure validation: can one authority structure adjudicate disputes between readings, or would each reading require its own decision procedure?',
    'If one kernel: this story captures the contested reading coherence with omega variables addressing interpretation-layer drift. If three constraints: decompose into speculation_asset_extraction (ε~0.72), sound_money_coordination (ε~0.15), and decentralization_governance (ε~0.35), linked via network.affects_constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_vs_three_constraints, conceptual, 'Kernel identity: single contested reading or three distinct constraint claims?').

omega_variable(
    volatility_as_feature_or_bug,
    'Is volatility a necessary property of the price discovery mechanism (feature of decentralized exchange), or an exploitable extraction mechanism (bug that institutional traders profit from)?',
    'Comparative analysis: volatility levels in (a) early Bitcoin years pre-institutional entry, (b) post-2017 retail boom, (c) post-2020 institutional adoption. If volatility declined as liquidity increased (feature), the constraint should reclassify toward Rope. If volatility increased despite higher institutional participation (exploitation), reclassify toward Snare.',
    'Feature interpretation: ε drops to ~0.25, chi shifts toward rope. Bug interpretation: ε rises to ~0.72, chi shifts toward snare. Current value (0.58) represents the symmetric case where both mechanisms operate simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(volatility_as_feature_or_bug, empirical, 'Whether volatility is price discovery feature or extraction mechanism').

omega_variable(
    decentralization_authenticity_threshold,
    'At what concentration level (whale distribution, exchange custody, mining pool hash share) does the decentralization narrative cease to be coherent even as theater, requiring piton reclassification to snare?',
    'Time-series analysis of Gini coefficient for address distributions, custody concentration (CEX + institutional wallets as % of supply), and mining pool hash distribution. Comparison with threshold for loss of network resilience (51% attack feasibility) and governance legitimacy (top-10 holders can block protocol changes).',
    'If threshold crossed: piton degrades to snare (pure extraction via volatility under concentration cover). If threshold holds: piton persists as sustained institutional inertia around degraded ideal. Current estimate: Bitcoin at threshold; most altcoins past it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_authenticity_threshold, empirical, 'Decentralization concentration threshold for authenticity loss').

omega_variable(
    stablecoin_sunset_mechanism,
    'Do stablecoins and regulated on-ramps genuinely reduce volatility extraction (supporting scaffold perspective), or do they merely displace it to unregulated perpetual futures and leverage lending (converting volatility extraction into solvency extraction)?',
    'Structural tracking of volatility in regulated vs. unregulated segments; measurement of total notional leverage in ecosystem; analysis of liquidation cascades and contagion risk as extraction mechanism shifts from spot volatility to leverage volatility.',
    'If volatility reduced: scaffold perspective correct, sunset is structural. If volatility displaced: constraint persists in new form, scaffold is aspirational theater, and the true constraint is leverage_solvency_extraction (separate story, ε~0.75, Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stablecoin_sunset_mechanism, empirical, 'Whether stablecoins reduce volatility extraction or displace it').

omega_variable(
    early_adopter_extraction_legitimacy,
    'Is the wealth accumulation of early Bitcoin adopters (2010-2013) fair compensation for risk-taking and belief in an uncertain technology, or extractive advantage from information asymmetry and narrative capture before institutional scrutiny?',
    'Counterfactual analysis: would the same individuals achieve equivalent returns in (a) a similar early-stage technology without hype cycle (e.g., Tor network adoption), (b) traditional venture capital early-stage investing? If returns in Bitcoin were substantially higher despite equivalent risk, the excess return is extraction.',
    'If legitimate: beneficiary status of early adopters is sustainable under rope logic. If extraction: early adoption was the exploitation vector, and subsequent generations of retail speculators are repeat victims, supporting snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_adopter_extraction_legitimacy, empirical, 'Whether early-adopter wealth concentration is fair risk compensation or extraction').

omega_variable(
    coordination_vs_extraction_decomposition,
    'Can the genuine coordination function (distributed settlement, censorship resistance, programmable payments) be cleanly separated from the extraction mechanism (volatility capture, whale distribution, institutional gating), or are they structurally entangled?',
    'Technical analysis: can a stabilized version of the protocol (fixed money supply, low volatility, governance without governance tokens) maintain the coordination benefits while losing the extraction mechanism? Or does volatility enable the extraction precisely because it funds development, incentivizes participation, and creates the illusion of decentralization?',
    'If separable: two constraints (coordination_settlement_protocol, ε~0.10; speculation_extraction_engine, ε~0.68). If entangled: single constraint (current tangled_rope), and attempts to remove volatility would collapse the coordination function (coordination IS the extraction mechanism working).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_decomposition, conceptual, 'Whether coordination and extraction are separable or structurally entangled').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speculation_volatility_engine, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spec_vol_theater_2010, speculation_volatility_engine, theater_ratio, 0, 0.28).
narrative_ontology:measurement(spec_vol_theater_2013, speculation_volatility_engine, theater_ratio, 3, 0.42).
narrative_ontology:measurement(spec_vol_theater_2016, speculation_volatility_engine, theater_ratio, 6, 0.62).
narrative_ontology:measurement(spec_vol_theater_2019, speculation_volatility_engine, theater_ratio, 9, 0.68).
narrative_ontology:measurement(spec_vol_theater_2022, speculation_volatility_engine, theater_ratio, 12, 0.71).

% Extraction over time
narrative_ontology:measurement(spec_vol_extract_2010, speculation_volatility_engine, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(spec_vol_extract_2013, speculation_volatility_engine, base_extractiveness, 3, 0.31).
narrative_ontology:measurement(spec_vol_extract_2016, speculation_volatility_engine, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(spec_vol_extract_2019, speculation_volatility_engine, base_extractiveness, 9, 0.58).
narrative_ontology:measurement(spec_vol_extract_2022, speculation_volatility_engine, base_extractiveness, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speculation_volatility_engine, resource_allocation).
narrative_ontology:affects_constraint(speculation_volatility_engine, institutional_leverage_cycles).
narrative_ontology:affects_constraint(speculation_volatility_engine, exchange_custody_gatekeeping).
narrative_ontology:affects_constraint(speculation_volatility_engine, retail_psychology_trap).
narrative_ontology:affects_constraint(speculation_volatility_engine, sound_money_narrative_authority).

% DUAL FORMULATION NOTE:
% The specification-volatility-engine is downstream of three potential contested kernels: (1) sound-money reading (Austrian monetary legitimacy), (2) speculation-asset reading (market microstructure), (3) decentralization reading (cypherpunk governance). The engine's existence depends on all three readings remaining partially active. Upstream: institutional_leverage_cycles (derives from volatility), exchange_custody_gatekeeping (derives from volatility access control), retail_psychology_trap (derives from volatility FOMO dynamics), sound_money_narrative_authority (derives from scarcity mythology that justifies volatility as price discovery).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speculation_volatility_engine, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
