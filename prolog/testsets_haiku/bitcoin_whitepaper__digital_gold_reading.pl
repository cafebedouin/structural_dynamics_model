% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper__digital_gold_reading
 *   human_readable: Bitcoin as Digital Gold: Asset Appreciation and Store of Value
 *   domain: economic/monetary/technological
 *
 * SUMMARY:
 *   The digital gold reading instantiates bitcoin's design as a scarce,
 *   non-inflationary monetary asset optimized for store of value and
 *   inflation hedging rather than transaction throughput. This reading
 *   prioritizes the 21-million coin limit, proof-of-work security budget, and
 *   block-size constraints that make frequent settlement costly. Early
 *   adopters and institutional HODLers benefit from appreciation driven by
 *   scarcity narratives; late entrants and high-frequency users pay through
 *   entry-price asymmetry and transaction-fee barriers. The constraint is
 *   CLAIMED as tangled rope (coordination of settlement finality + asymmetric
 *   extraction of transaction-cost burden) while the measurement series track
 *   how extractiveness rises as adoption increases without proportional
 *   throughput expansion. The reading contests two sibling readings:
 *   p2p_cash_reading (bitcoin as transaction medium) and
 *   protocol_ossification_reading (stability as intrinsic virtue). This
 *   constraint instantiates the reading's core premise: scarcity and security
 *   are the valued properties, and transaction costs are an acceptable price
 *   for maintaining them.
 *
 * KEY AGENTS:
 *   - early_adopters: beneficiaries of appreciation under the digital gold reading; their HODL discipline validates the scarcity narrative
 *   - hodl_investors: institutional and high-net-worth beneficiaries; treat bitcoin as inflation hedge and store of value
 *   - mining_pool_operators: agenda-setters who enforce the scarcity constraint (21M cap, block-size limits) and set transaction fee standards
 *   - late_entrants: victims of entry-price asymmetry and opportunity cost of delayed accumulation
 *   - transaction_volume_users: victims of escalating transaction fees as block space fills with store-of-value activity
 *   - bandwidth_constrained_nodes: victims of storage and verification requirements that make distributed validation expensive
 *   - core protocol developers: institutional gatekeepers who resist changes that would prioritize throughput over settlement finality
 *   - monetary policy authorities: observers who view the digital gold reading as competitive to fiat systems and monitor for systemic risk
 *   - academic researchers: observers who analyze whether the reading's claims (true scarcity, decentralization, censorship resistance at scale) hold empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.45).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold: Asset Appreciation and Store of Value").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "economic/monetary/technological").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, '559ffab3-e6fe-4053-8c14-033555be8599').
narrative_ontology:cs_kernel_codification('559ffab3-e6fe-4053-8c14-033555be8599', fixed_text).
narrative_ontology:cs_authority_grounding('559ffab3-e6fe-4053-8c14-033555be8599', extraction).
narrative_ontology:cs_interpretation_layer_present('559ffab3-e6fe-4053-8c14-033555be8599').
narrative_ontology:cs_reading_relation('559ffab3-e6fe-4053-8c14-033555be8599', bitcoin_whitepaper__p2p_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('559ffab3-e6fe-4053-8c14-033555be8599', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('559ffab3-e6fe-4053-8c14-033555be8599', foundational, scarcity_is_primary_virtue).
narrative_ontology:cs_axiom_status(scarcity_is_primary_virtue, holdable).
narrative_ontology:cs_axiom_grounding('559ffab3-e6fe-4053-8c14-033555be8599', scarcity_is_primary_virtue, instrumental).
narrative_ontology:cs_axiom('559ffab3-e6fe-4053-8c14-033555be8599', foundational, settlement_finality_over_throughput).
narrative_ontology:cs_axiom_status(settlement_finality_over_throughput, holdable).
narrative_ontology:cs_axiom_grounding('559ffab3-e6fe-4053-8c14-033555be8599', settlement_finality_over_throughput, instrumental).
narrative_ontology:cs_reference_frame('559ffab3-e6fe-4053-8c14-033555be8599', fixed_supply_settlement_optimization).
narrative_ontology:cs_drift_state('559ffab3-e6fe-4053-8c14-033555be8599', contemporary_payment_layer_saturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('559ffab3-e6fe-4053-8c14-033555be8599', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, hodl_investors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, mining_pool_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrants).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, transaction_volume_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, bandwidth_constrained_nodes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquired bitcoin at low cost; benefit from appreciation as network value increases and scarcity narrative dominates. Hold position across market cycles. Their primary constraint is HODL discipline—resisting exit pressure during volatility. The digital gold reading validates their position: the scarce, non-inflationary design is the story that justifies holding through bear markets and against transaction-cost arguments.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_adopters, beneficiary,
    moderate, generational, arbitrage, global).

% Institutional and high-net-worth actors who entered after early adoption but before mass awareness. Hold large quantities; benefit directly from appreciation narratives. Can exit to other assets but choose not to, treating bitcoin as a core portfolio hedge against monetary expansion. The digital gold reading is the legitimating story for their position—it makes HODLing a rational monetary strategy, not speculation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, hodl_investors, beneficiary,
    powerful, generational, mobile, global).

% Control and coordinate the proof-of-work enforcement mechanism. Set the operational costs and transaction fee standards that govern what transactions are viable. Benefit from high fees during network congestion; benefit from appreciation of mined bitcoin; benefit from scarcity narrative that justifies energy expenditure. Enforce the 21M supply cap and block-size limitations that create the scarcity conditions the digital gold reading depends on. Can shift hash power to other chains but choose not to, capturing consensus-level authority over the protocol's properties.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, mining_pool_operators, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, mining_pool_operators, beneficiary).

% Entered the network after price appreciation and network maturation. Pay the full appreciated price to acquire coins; constrained by the binary choice of entry price (no gradual accumulation at lower cost possible; the network's history is fixed). Bear the cost of higher transaction fees as the network fills with store-of-value holders rather than frequent transactors. Exit option is constrained by their belief in bitcoin's value proposition and the sunk-cost psychology of entry at high price.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_entrants, payer,
    moderate, biographical, constrained, global).

% Need to move value frequently (merchants, remittance users, payment processors). Pay escalating transaction fees as block space fills with HODLers' storage activity. The digital gold reading explicitly deprioritizes their use case in favor of settlement-layer security; the constraint extracts from them by making frequent transactions prohibitively expensive while validating that this is the correct design. Their exit is to alternative chains or payment layers, but these lack bitcoin's network effects and settlement certainty.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, transaction_volume_users, payer,
    powerless, biographical, constrained, global).

% Run full nodes on limited infrastructure (developing regions, restricted networks, high-latency connections). Bear the cost of validating every transaction and storing the entire blockchain history, even though they are not mining and do not benefit from the security budget. The digital gold reading optimizes for settlement certainty over inclusivity; block-size caps and storage requirements remain steep. Their exit is to light wallets (SPV) or custodial solutions, both of which sacrifice the distributed-validation property the digital gold reading claims to protect.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, bandwidth_constrained_nodes, payer,
    powerless, biographical, trapped, global).

% Steward the protocol code and approve or reject changes. Under the digital gold reading, are gatekeepers of stability and scarcity properties. The constraint they enforce is protocol conservatism: changes that prioritize transaction throughput or lowered fees are resisted as threats to the settlement-layer security story that justifies the asset's value. Exit option exists (fork the protocol, migrate to altcoins) but rarely exercised because of consensus-lock and the coordination problem.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, developers_core_protocol, agenda_setter,
    institutional, generational, mobile, global).

% Central banks and finance ministries view bitcoin's scarcity and inflation-hedge narrative as a competitive threat to fiat monetary systems. Monitor adoption and regulatory pathways. Their analysis contradicts the digital gold reading: they emphasize the instability of unanchored asset prices, the resource waste of proof-of-work, and the risks of distributed settlement systems. They observe but do not directly enforce the constraint—their role is to decide whether to accommodate, regulate, or suppress the reading.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, monetary_policy_authorities, observer,
    institutional, generational, analytical, national).

% Study the technical and economic properties of bitcoin and competing designs. Analyze whether the digital gold reading's claims (truly scarce, perfectly decentralized, censorship-resistant at scale) hold up under scrutiny. Produce evidence about the concentration of mining power, the energy costs of the security budget, and the tradeoffs between security and accessibility. Their observations feed competing readings and regulatory decisions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, academic_blockchain_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__digital_gold_reading, hodl_investors).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__digital_gold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a distributed, permissionless ledger with cryptographic finality: any party can settle claims without trusting a central counterparty, and the settlement is immutable and universally verifiable. Solves the double-spend problem in a decentralized network using proof-of-work consensus.
% TRANSFER_FUNCTION: Moves block-reward bitcoin and transaction fees from network participants (early adopters and HODLers via appreciation) to mining operations and protocol developers (via consensus authority), while imposing transaction-cost barriers on frequent users and infrastructure operators running full nodes.
% ABSENT_VOICES: Users excluded from early adoption windows cannot advocate for their participation in the appreciation phase; their absence from the beneficiary set is enforced by the network's historical record (no mechanism to rewind entry price). Developers of alternative designs (rollups, sidechains, altcoins) are excluded from protocol governance and must fork or build on top, unable to reshape the core constraint. Payment networks that depend on frequent settlement are technically excluded when transaction volume saturates block space.
% DISAPPEARANCE_RATIONALE: If the digital gold reading's constraint (scarcity optimization, settlement-layer prioritization, high transaction costs) disappeared—if the protocol pivoted to throughput-first design, expanded block sizes, or adopted monetary expansion—the asset's inflation-hedge narrative would collapse, early adopters would exit to preserve gains (dumping price), and the network would reorganize around transaction-layer efficiency. The constraint's persistence depends on the reading's dominance in the mining-consensus coalition and investor belief structure.
% FOUNDING_PROBLEM: Fiat currencies are subject to central-bank inflation and monetary expansion; there was no digital asset with a mathematically guaranteed fixed supply immune to political monetary policy.
% FOUNDING_PROBLEM_CORROBORATION: The Whitepaper's abstract explicitly states the goal: 'create a system for electronic transactions without relying on trust in a financial institution.' Satoshi Nakamoto and early cryptographers including the cypherpunk tradition (e.g., Nick Szabo's bit gold proposal) attest the foundational problem. Independent monetary economists (e.g., from non-bitcoin-aligned institutions) acknowledge that central-bank monetary expansion is real and that some economic actors seek inflation hedges; they dispute whether bitcoin's design solves the problem without introducing new ones (energy waste, price volatility, settlement fragility). The founding problem is live for the reading's beneficiaries; contested by monetary authorities and by alternative-reading communities.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__digital_gold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper__digital_gold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the asymmetry in benefits: early participants acquire coins at low cost and benefit from appreciation driven by network effects and scarcity narratives; late entrants pay the full appreciated price and constrained access to transaction bandwidth. The extraction rises over the interval (0.42→0.68) as adoption widens entry costs and block-space scarcity intensifies fee pressure. Suppression (0.45) is moderate: the constraint is enforced through consensus rules (proof-of-work, block-size limits, 21M cap) rather than overt coercion, but its persistence depends on mining-coalition agreement to resist changes that would democratize participation or lower transaction costs. Theater ratio (0.22) is low: the settlement-security and scarcity narratives are substantively grounded in the protocol's design, though they increasingly serve to justify extraction rather than coordinate genuine transaction activity. Resistance (0.71) is high: alternative readings (p2p_cash, protocol_ossification) actively contest the digital gold framing, and users excluded from appreciation benefits mount real resistance through altcoins, layer-2 solutions, and regulatory pressure. Accessibility collapse (0.62) is moderate-to-high: alternatives exist (other cryptocurrencies, traditional banking, payment rails) but none capture bitcoin's settlement finality and decentralization properties equally, so the constraint persists despite contested legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter (mining/development) and beneficiary (HODL investor) seats, the constraint is pure coordination: it creates a trustless settlement layer with fixed supply, preventing the monetary-policy regime risk that fiat currencies face. From the payer seats (late entrants, transaction users, node operators), the same structure operates as enforced extraction: they pay for participation in a settlement system that explicitly de-prioritizes their use cases in favor of wealth storage. The engine should compute Tangled Rope from the payer perspective (high extraction, active enforcement, bounded exit) and approach Rope from the beneficiary perspective (genuine coordination value). The claim/metric gap is intentional: I claim Tangled Rope because the constraint simultaneously solves a real coordination problem (trustless settlement) AND extracts asymmetrically from those with different time horizons and participation windows.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is strongly skewed. Early adopters and HODLers (power: moderate to powerful, exit: mobile or arbitrage) experience d near 0.1-0.25 (net beneficiaries; could exit to other assets but choose not to; their participation subsidizes the network). Mining pools and core developers (power: institutional, exit: mobile) experience d near 0.2-0.35 (they set the rules but depend on consensus; could fork but rarely do; they extract rents from fee collection but face resistance). Late entrants and transaction users (power: powerless to moderate, exit: constrained to trapped) experience d near 0.8-0.95 (targeted by the constraint; high exit costs; pay without capturing coordination benefits). Bandwidth-constrained node operators (power: powerless, exit: trapped) approach full d=1.0: they must validate the entire history without benefit and cannot exit without losing security assurances. The divergence in d across seats is the core reason for per-seat type divergence: what computes as Rope for beneficiaries with high exit mobility computes as Snare for powerless transaction users with trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The digital gold reading avoids the mandatrophy hazard that the p2p_cash_reading runs into: the founding problem (central-bank monetary expansion risk) remains live for the beneficiaries, and the constraint's design (scarcity optimization) directly addresses it. No mandate has atrophied into theater. However, a secondary mandatrophy is latent: the founding problem for transaction users (need for censorship-resistant, low-cost payments) is NOT solved by this constraint, because settlement-layer prioritization explicitly de-services that use case. The constraint persists despite unmet founding problems for payer seats, sustained by the beneficiary coalition's control of consensus. This is not mandatrophy in the classical sense (function atrophied, constraint persists through inertia) but rather competitive-reading suppression: the digital gold reading's beneficiaries have enough power to enforce their interpretation, preventing the p2p_cash reading from reshaping the protocol toward its founding problem. The measurement series show low theater ratio (the scarcity and security claims are substantive) and rising extractiveness (the asymmetry is deepening over time), not classic piton dynamics (high theater, stable extraction). So the classification as Tangled Rope holds; mandatrophy is not the right frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scarcity_vs_protocol_softfork,
    'Is the 21-million-coin supply cap a structural mathematical limit or a consensus rule that miners could change via softfork?',
    'Historical analysis of coinbase reward schedule (hard-coded, irreversible via softfork) vs. observation of whether any future consensus has attempted to change it. Empirical test: if a softfork proposals to increase supply emerges and is rejected on merits, the answer is ''consensus rule.'' If no such proposal emerges despite repeated discussions, the answer is ''treated as immutable even though technically revisable.''',
    'If the 21M cap is consensus-enforced (not mathematical), scarcity depends on ongoing beneficiary-coalition agreement to reject expansion. The extractiveness would then be more contingent and less ''natural law'' than the digital gold reading claims. This feeds into potential mandatrophy if the coalition fragments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_vs_protocol_softfork, empirical, 'Whether the 21M limit is mathematical or consensus-vulnerable.').

omega_variable(
    settlement_throughput_necessity,
    'Does the digital gold reading''s claim that settlement-layer finality requires block-size constraints match the observed necessity, or is throughput limitation a design choice rather than a structural requirement?',
    'Comparison with alternative L1 designs (Ethereum, Cardano, Solana) that achieved higher throughput while maintaining decentralization claims. Technical analysis of whether the specific throughput constraints chosen (1 MB block size, 10-minute block time) are the minimum required for security or represent a more conservative tradeoff. Empirical observation of whether layer-2 solutions (Lightning, Stacks) can absorb transaction volume without undermining the digital gold reading''s security claims.',
    'If throughput limits are design choices rather than structural necessities, the extraction from transaction users is partially contingent—mining operators could raise block sizes without compromising the scarcity narrative. This would clarify whether transaction-cost barriers are essential to the digital gold reading or extractive overhead. If layer-2 solutions can fully absorb high-frequency usage, the constraint is less about coordination (trustless settlement) and more purely about asset appreciation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_throughput_necessity, empirical, 'Whether block-size limits are necessary for security or chosen for extraction.').

omega_variable(
    reading_kernel_contest_forecloses,
    'Do the three kernel readings (digital_gold, p2p_cash, protocol_ossification) logically foreclose each other within a single commitment framework, or do they coexist as live readings held by competing coalitions?',
    'Logical analysis of each reading''s core axiom: (1) digital gold reads scarcity as the primary virtue; (2) p2p_cash reads transaction accessibility as the primary virtue; (3) protocol_ossification reads consensus immutability as the primary virtue. If any two axioms are logically contradictory such that affirming one entails denying the other in the SAME framework, they foreclose. If each axiom can coexist in different frameworks or coalitions, they coexist.',
    'If the readings foreclose each other, the constraint is fundamentally contested and resolution requires one reading to win consensus decisively. If they coexist, the constraint persists through coalition division and the measurement of extractiveness must account for multiple simultaneous reading-streams. This affects whether the divergence in directionality is a seat-level artifact (same constraint, different percepts) or a fundamental framing ambiguity (different constraints masquerading as one).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_forecloses, conceptual, 'Whether kernel readings are logically contradictory or coalition-divided.').

omega_variable(
    early_adoption_asymmetry_is_extractive,
    'Is the entry-price asymmetry for late adopters—paying the full appreciated price while early participants paid pennies—a form of extraction, or a neutral outcome of adoption dynamics?',
    'Philosophical and economic analysis. If entry-price asymmetry is unavoidable and not structurally enforced (late entrants freely choose to buy at current price), it is a neutral consequence of adoption curves. If the constraint actively prevents late entrants from gaining earlier-equivalent participation (e.g., by suppressing throughput to make frequent lower-cost accumulation impossible), it is extractive. Empirical check: whether late entrants in lower-cost-base regions (developing economies) can accumulate fractional bitcoin at sustainable rates, or whether they face synchronous high-cost barriers (high price + high transaction fees + high access barriers).',
    'If entry-price asymmetry is extractive rather than neutral, the victim set is larger (all late entrants, not just high-frequency users) and the constraint is more purely redistributive than coordination-oriented. This would strengthen the Snare classification from the late-entrant perspective and weaken the Tangled Rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_adoption_asymmetry_is_extractive, preference, 'Whether entry-price asymmetry is structurally extractive or neutral adoption dynamics.').

omega_variable(
    mining_pool_decentralization_fiction,
    'How concentrated is mining power in practice, and does the consensus model depend on coordination within a small number of mining pools, making the ''decentralized consensus'' claim overstated?',
    'Empirical hashrate measurement and pool membership data. If the top 3-5 mining pools control 60%+ of hashrate and would need to explicitly coordinate to sustain the digital gold reading, then consensus is effectively oligarchic, not decentralized. If hashrate is more distributed, then the consensus model is more robust.',
    'If mining is highly concentrated, the agenda-setter role is less distributed than the constraint''s legitimacy claim requires, and the suppression measurement understates the actual enforcement coercion (institutional actors can simply coordinate rather than relying on cryptographic incentive alignment). This would shift the classification toward Snare (explicit cartel behavior) from some seats'' perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mining_pool_decentralization_fiction, empirical, 'Whether mining consensus is genuinely distributed or effectively oligarchic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2, 0.11).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 16, 0.22).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 16, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2, 0.32).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 16, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__digital_gold_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper kernel admits three structurally distinct readings, each with its own constraint story, ε value, beneficiary/victim structure, and classification. The digital_gold_reading (this file) treats scarcity and settlement-layer security as the primary virtues; extractiveness is 0.68 because late entrants and transaction users pay entry-price and throughput-cost barriers. The p2p_cash_reading treats transaction accessibility and censorship-resistance as primary; extractiveness would be lower because the constraint's design would optimize for throughput and lower fees. The protocol_ossification_reading treats consensus immutability as primary; extractiveness would be higher because stability requires suppressing changes even if beneficiaries demand them. The three readings coexist across different mining coalitions and investor bases; none forecloses the others logically, but they compete for protocol-development resources and consensus authority. Network edges represent that changes in this reading's beneficiary coalition (e.g., if transaction-volume coalitions gain enough hash power) would directly reshape the other readings' viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
