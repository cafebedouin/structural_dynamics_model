% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__digital_gold_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: bitcoin_whitepaper__digital_gold_reading
 *   human_readable: Bitcoin Digital Gold Reading: Asset Appreciation and Store-of-Value Optimization
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   Bitcoin's whitepaper contains three structurally distinct readings, each
 *   with different operational implications and victim sets. The digital gold
 *   reading interprets Bitcoin primarily as a scarce digital asset optimized
 *   for store of value and inflation hedging — emphasizing the fixed supply
 *   cap, predictable issuance schedule, and resistance to monetary
 *   debasement. This reading explicitly accepts high transaction fees and
 *   limited throughput as acceptable costs of maintaining scarcity and
 *   security. Early adopters and institutional investors benefit from
 *   appreciation and portfolio diversification; late entrants face pricing
 *   out through appreciation dynamics; transaction users face escalating fee
 *   pressure. The constraint exhibits tangled_rope classification at the
 *   aggregate level (genuine coordination function — the scarcity mechanism
 *   IS the value proposition — plus asymmetric extraction from late entrants
 *   and transaction users). The theatrical content is relatively low (0.48)
 *   because the store-of-value case rests on real cryptographic properties,
 *   not performative claims; the medium-of-exchange function has degraded to
 *   vestigial status (piton classification). The suppression measurement
 *   shows rising enforcement requirements as the constraint matures: earlier
 *   in Bitcoin's history (t=0), late entrants faced weaker price barriers and
 *   less network effect lock-in; as adoption increased and early holder
 *   accumulation concentrated, suppression of alternative narratives (p2p
 *   cash, protocol flexibility) increased to maintain the gold reading's
 *   coherence.
 *
 * KEY AGENTS:
 *   - Early Adopters: Beneficiaries (institutional/arbitrage) — captured appreciation from early accumulation; strong incentive to sustain gold narrative
 *   - Core Protocol Developers: Beneficiaries (institutional/arbitrage) — maintain protocol conservatism; benefit from scarcity narrative's legitimacy; have veto power over protocol changes
 *   - Institutional Investors: Beneficiaries (institutional/arbitrage) — entry point without endorsing payments thesis; portfolio hedge function drives demand
 *   - Late Entrants: Victims (powerless/trapped) — priced out by appreciation; cannot access early-stage upside; trapped in high-cost entry position
 *   - Price Volatility Targets: Victims (powerless/trapped) — leverage casualities, margin calls; volatility acceptable to hodlers but catastrophic to traders
 *   - Transaction Users: Victims (moderate/constrained) — face rising on-chain fees; can exit to Layer 2 but at cost of reduced security assurances
 *   - Layer 2 Infrastructure: Organized response (organized/constrained) — builds around the fee constraint; has agency but dependence on Layer 1
 *   - P2P Cash Reading: Sibling reading (institutional/arbitrage from some parties, powerless/trapped from p2p advocates) — foreclosed or marginalized by gold reading's dominance
 *   - Analytical Observer: Examines the constraint from civilizational horizon (analytical/analytical) — risks naturalizing the gold reading as inevitable or immutable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.62).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin Digital Gold Reading: Asset Appreciation and Store-of-Value Optimization").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, 'btc-dg-2026-02-26-001').
narrative_ontology:cs_kernel_codification('btc-dg-2026-02-26-001', fixed_text).
narrative_ontology:cs_authority_grounding('btc-dg-2026-02-26-001', lineage).
narrative_ontology:cs_interpretation_layer_present('btc-dg-2026-02-26-001').
narrative_ontology:cs_reading_relation('btc-dg-2026-02-26-001', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('btc-dg-2026-02-26-001', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('btc-dg-2026-02-26-001', foundational, scarcity_as_value_foundation).
narrative_ontology:cs_axiom_status(scarcity_as_value_foundation, holdable).
narrative_ontology:cs_axiom_grounding('btc-dg-2026-02-26-001', scarcity_as_value_foundation, empirically_contingent).
narrative_ontology:cs_axiom('btc-dg-2026-02-26-001', foundational, fee_market_acceptable_cost).
narrative_ontology:cs_axiom_status(fee_market_acceptable_cost, holdable).
narrative_ontology:cs_axiom_grounding('btc-dg-2026-02-26-001', fee_market_acceptable_cost, instrumental).
narrative_ontology:cs_reference_frame('btc-dg-2026-02-26-001', scarce_digital_asset_store_of_value).
narrative_ontology:cs_drift_state('btc-dg-2026-02-26-001', contemporary_institutional_adoption_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('btc-dg-2026-02-26-001', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, core_protocol_developers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, institutional_investors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrants).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, price_volatility_targets).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, transaction_fee_payers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE ENTRANTS (SNARE) — Trapped by forward-looking appreciation dynamics. Early adopters' accumulation drives unit price beyond reach. Transaction fees compound the barrier. No exit option: entry after price discovery means accepting marginal-cost position without early-stage upside. Experiences pure extraction — the digital gold narrative justifies holding by early cohorts, which constrains supply and prices late entrants out of the system.
constraint_indexing:constraint_classification(bitcoin_whitepaper__digital_gold_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: VOLATILITY TARGETS (SNARE) — Retail traders and leveraged participants trapped by the store-of-value narrative's contradiction: hodlers claim long-term stability, but digital gold is priced to extract margin calls and liquidations from short-term participants. The reading's emphasis on asset appreciation creates the volatility mechanism. No exit until liquidation. Experiences extraction through systematic disadvantage in timing and asymmetric information.
constraint_indexing:constraint_classification(bitcoin_whitepaper__digital_gold_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: TRANSACTION USERS (TANGLED ROPE) — Constrained by rising on-chain fees as block space becomes scarce. The digital gold reading accepts high fees as acceptable cost of security and scarcity — coordination mechanism for resource allocation via fee market. Benefits from network effects and censorship resistance; constrained by fee pressure during peak demand. Mixed extraction and coordination.
constraint_indexing:constraint_classification(bitcoin_whitepaper__digital_gold_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EARLY ADOPTERS & CORE DEVELOPERS (ROPE) — Net beneficiaries. Accumulation before price discovery, plus developer prestige and ecosystem control. The digital gold reading creates the appreciation mechanism that rewards holding. Experiences the constraint as coordination: scarcity rules and capped supply are the coordination commitment that sustains the value narrative. Arbitrage options available (sell at any point; influence protocol direction).
constraint_indexing:constraint_classification(bitcoin_whitepaper__digital_gold_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL INVESTORS (ROPE) — Portfolio diversification benefit. Digital gold positioning enables entry without endorsing payments infrastructure. Experiences constraint as pure coordination: scarcity and capped supply are the mechanism for maintaining non-correlation with equities and bonds. Low suppression from institutional perspective — can exit through liquidation markets anytime.
constraint_indexing:constraint_classification(bitcoin_whitepaper__digital_gold_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LAYER 2 & PAYMENT INFRASTRUCTURE (SCAFFOLD) — Organized response to the digital gold reading's acceptance of high base-layer fees. Lightning Network, sidechains, and custodial systems build scalable transaction pathways with sunset logic: if Layer 2 matures sufficiently, the on-chain fee constraint becomes obsolete (transitions from tangled_rope to rope for transaction users). Constrained by dependence on Layer 1 security but has agency and clear escape path.
constraint_indexing:constraint_classification(bitcoin_whitepaper__digital_gold_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: MEDIUM-OF-EXCHANGE FUNCTION (PITON) — The whitepaper's original vision of peer-to-peer electronic cash has been formalized into vestigial status. The digital gold reading explicitly deprioritizes transaction throughput and volatility acceptance makes medium-of-exchange unsuitable. The function persists (Bitcoin can still transact) but the protocol's incentive alignment has drifted: scarcity optimization overrides usability optimization. Theater ratio high: transactions continue to be theoretically possible, but the constraint structure (fee market, block size limit) makes them economically irrational except for high-value transfers. Institutional actors (miners, developers) maintain the vestigial function through backward compatibility, not because it serves medium-of-exchange goals.
constraint_indexing:constraint_classification(bitcoin_whitepaper__digital_gold_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, cryptographic scarcity of a fixed-supply asset appears as an immutable constraint: mathematics enforces the cap; no human authority can override it. The constraint appears to emerge naturally from the protocol's proof-of-work mechanics and consensus rules. However, this perspective risks a false summit: the scarcity is real (mathematical), but the INTERPRETATION of what scarcity means (asset vs. currency vs. protocol security) is socially contingent. The digital gold reading is one reading of the scarcity fact; the p2p_cash reading is another. The mathematics is mountain; the economics is tangled_rope.
constraint_indexing:constraint_classification(bitcoin_whitepaper__digital_gold_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__digital_gold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bitcoin_whitepaper__digital_gold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bitcoin_whitepaper__digital_gold_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, TR),
    TR >= 0.70.

:- end_tests(bitcoin_whitepaper__digital_gold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high. The digital gold reading creates real extraction through two mechanisms: (1) appreciation dynamics that price late entrants out of early-stage upside — a form of Cantillon effect where early adopters capture new money's first-round purchasing power; (2) fee market mechanics that allocate scarce block space to highest bidders, systematically excluding small transactions. The extractiveness has risen over time (0.15→0.58 across the measurement interval) as Bitcoin matured from a niche protocol to macro asset class — early volatility and network uncertainty meant less systematic extraction, whereas mature network with established price trajectory enables more predictable value capture by early cohorts. Suppression (0.62): Moderate-high. The gold reading suppresses alternatives through narrative dominance (the 'store of value' framing marginalizes p2p cash use cases), protocol conservatism (resistance to changes that would improve throughput), and institutional lock-in (major holders and exchanges invested in gold narrative have incentive to block competing visions). However, suppression is not total — Layer 2 solutions reduce fee suppression, the p2p cash reading remains live in some communities, and technical alternatives (Monero, Zcash) exist. Theater ratio (0.48): Moderate. The gold reading rests substantially on real cryptographic properties (the supply cap IS enforced by mathematics) and genuine economic reasoning (non-correlated asset, inflation hedge). Theater emerges in the scarcity narrative's conflation of mathematical immutability with economic inevitability, and in the medium-of-exchange function's theatrical persistence (Bitcoin can technically transact, but fee economics make it irrational except for high-value transfers). Claimed type (tangled_rope): Appropriate because the constraint contains both genuine coordination (the scarcity mechanism solves double-spending and enables non-custodial value transfer) and clear asymmetric extraction (early adopters vs. late entrants, hodlers vs. transaction users). The constraint is not pure snare (some benefit from coordination function) nor pure rope (extraction gap is substantial and structural).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between early adopters (rope) and late entrants (snare) is maximal — both experience the same constraint mechanics but derive opposite value from it. Early adopters see a coordination breakthrough (scarcity enables trustless value transfer); late entrants see a pricing mechanism that excludes them. The gap reveals that the constraint's apparent 'naturalness' (cryptography enforces the cap) masks a distributional choice (who benefits from scarcity). The piton perspective (medium-of-exchange function) highlights how the gold reading has actively degraded the payments function while maintaining its theoretical possibility — a divergence between stated capability and economic rationality. The analytical observer's mountain perspective is the most dangerous because it risks legitimizing the gold reading's choices as inevitable rather than contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) computation for each perspective derives from structural position — beneficiary/victim status, power level, and exit options. Early adopters with arbitrage options (can sell anytime, influence protocol) experience low d (0.10–0.20), producing negative or near-zero χ despite moderate suppression — they see the constraint as coordination. Late entrants with trapped status (high entry cost, no alternative paths to early-stage upside) experience high d (0.85–0.95), producing high χ and snare classification — they bear extraction. Transaction users with constrained status (can use Layer 2 or other systems but at cost of reduced security/privacy) experience moderate d (0.55–0.65), producing moderate χ and tangled_rope — mixed extraction and coordination. The piton classification at the civilizational institutional perspective reflects zero extraction experienced (miners and developers maintain the vestigial medium-of-exchange function out of institutional inertia and backward compatibility, not because it creates value) — the function persists through theater, not through active extraction. The analytical observer's mountain perspective carries the risk of naturalizing a contingent institutional reading (the gold reading) as an immutable law of cryptography, when in fact the whitepaper's cryptographic machinery is agnostic to whether Bitcoin functions as gold, cash, or immutable ledger.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING CONTEST: The mandatrophy is resolved by recognizing that the digital gold reading is one of three coherent readings of the Bitcoin whitepaper, each with different operational implications. The whitepaper contains support for all three: 'It is completely decentralized with no server or central authority' (p2p cash reading); 'The network is robust in its unstructured simplicity' (digital gold reading); 'The longest chain is the authoritative version' (protocol ossification reading). The mandatrophy is not 'which reading is right?' but 'which reading should be foundational?' The digital gold reading resolves mandatrophy within its own framework by explicitly accepting high fees and limited throughput as acceptable costs of scarcity — it does not deny the extraction; it justifies it as necessary for security. This is internally coherent but requires accepting victims (late entrants, transaction users). The resolution depends on the political choice: 'Is maximizing early-adopter wealth more important than maximizing adoption breadth?' The reading answers yes; the p2p cash reading would answer no.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_gold_vs_p2p_cash_incompatibility,
    'Do the digital gold reading and the p2p cash reading represent genuinely incompatible policy trajectories, or can a single protocol instantiate both simultaneously?',
    'Historical analysis of protocol decisions (block size debates, fee market evolution, layer 2 adoption rates). If high-value transactions dominate on-chain and payment transactions migrate to Layer 2, the readings coexist (different layers, different functions). If base-layer fees price out all small transactions, the p2p cash reading is foreclosed.',
    'If coexist: both readings remain live and the constraint family is stable. If foreclosed: the digital gold reading has structurally eliminated the p2p cash reading''s possibility space within a single protocol framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_gold_vs_p2p_cash_incompatibility, empirical, 'Compatibility of digital gold and p2p cash readings').

omega_variable(
    adoption_elasticity_to_fee_pressure,
    'How sensitive is Bitcoin adoption (especially in fee-sensitive jurisdictions and use cases) to sustained on-chain fee elevation? Does the digital gold reading''s acceptance of high fees constrain adoption growth?',
    'Cross-sectional adoption rates by region/sector vs. fee levels and volatility; correlation between layer 2 adoption and base-layer fee trends; surveys of adoption barriers in developing markets.',
    'If highly elastic (adoption drops with fees): the digital gold reading has significant victims and suppression is high. If inelastic: early adopters and institutional investors experience rope classification regardless of fee levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adoption_elasticity_to_fee_pressure, empirical, 'Adoption sensitivity to transaction fee pressure').

omega_variable(
    institutional_gold_narrative_sustainability,
    'Does the digital gold narrative (Bitcoin as inflation hedge, uncorrelated asset) remain coherent if macroeconomic conditions change? What scenarios would invalidate the store-of-value thesis?',
    'Stress-test against major currency collapse, deflation scenario, or sustained negative real interest rates. Examine correlation stability across market regimes and geopolitical events.',
    'If vulnerable: the institutional rope classification depends on maintained macroeconomic conditions — a contingent reading, not a structural one. If robust: digital gold remains a stable attractor even under regime change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_gold_narrative_sustainability, empirical, 'Macroeconomic sensitivity of digital gold narrative').

omega_variable(
    protocol_ossification_risk_from_gold_reading,
    'Does prioritizing store-of-value optimization (scarcity, predictable supply, resistance to change) lock the protocol into designs that become obsolete or vulnerable as technology evolves? Is there a long-term cost to the digital gold reading''s conservatism?',
    'Comparative analysis of Bitcoin''s upgrade velocity vs. other layer 1 systems; assess emergence of novel attack vectors or inefficiencies that would require protocol changes incompatible with the fixed-cap narrative.',
    'If yes: the digital gold reading trades short-term stability for long-term fragility. The piton perspective becomes dominant over centuries. If no: the reading''s conservatism is appropriate stewardship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_ossification_risk_from_gold_reading, conceptual, 'Risk of protocol ossification from commitment to fixed scarcity narrative').

omega_variable(
    kernel_reading_contest_unresolved,
    'Which reading of the Bitcoin whitepaper is the ''correct'' interpretation: digital gold (store of value, asset class), p2p cash (medium of exchange, payments), or protocol ossification (immutable ledger, infrastructure)?',
    'This is a conceptual omega with no empirical resolution. The whitepaper contains material supporting all three readings. The contest is about which axioms (scarcity vs. usability vs. immutability) should be foundational. Unresolved because the whitepaper''s ambiguity is intentional — Satoshi left room for multiple readings.',
    'The three readings generate structurally different constraints with different victim sets and extraction mechanisms. The digital gold reading accepts high fees and late-entrant exclusion; the p2p cash reading would prioritize accessibility; the ossification reading prioritizes immutability over efficiency. Each reading is internally coherent but incompatible with full commitment to the others at the margin.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_unresolved, conceptual, 'Unresolved contest over Bitcoin whitepaper''s foundational axioms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_dg_tr_t0, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(btc_dg_tr_t5, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(btc_dg_tr_t10, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(btc_dg_be_t0, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(btc_dg_be_t5, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(btc_dg_be_t10, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(btc_dg_su_t0, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(btc_dg_su_t5, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(btc_dg_su_t10, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_layer_2_scalability_bottleneck).

% DUAL FORMULATION NOTE:
% The Bitcoin whitepaper constraint family decomposes into three structurally distinct constraints, each with different ε values and victim/beneficiary sets. The digital_gold_reading (this file, ε=0.58) prioritizes scarcity and accepts fee pressure. The p2p_cash_reading (sibling, ε≈0.42) would prioritize throughput and minimize fees. The protocol_ossification_reading (sibling, ε≈0.35) prioritizes immutability regardless of fees or throughput. These are not three views of the same constraint — they have genuinely different ε values reflecting different observable consequences. The digital gold reading's acceptance of high fees (ε contribution: 0.20) becomes a central extraction mechanism in the p2p cash reading (ε contribution: 0.35+). The family is linked by network.affects_constraints because they share a kernel (the whitepaper) and policy choices in one reading constrain others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__digital_gold_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
