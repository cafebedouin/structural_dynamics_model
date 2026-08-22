% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__p2p_cash_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: bitcoin_whitepaper__p2p_cash_reading
 *   human_readable: Bitcoin P2P Cash Transaction Priority Reading
 *   domain: cryptocurrency/monetary systems/technology governance
 *
 * SUMMARY:
 *   Bitcoin's whitepaper describes a peer-to-peer electronic cash system
 *   enabling direct transactions without trusted intermediaries. The p2p cash
 *   reading instantiates the whitepaper's stated goal: censorship-resistant
 *   transactions as the primary purpose. This reading prioritizes low
 *   transaction fees and accessibility over asset scarcity, and treats block
 *   size as a scaling parameter, not an immutable architectural choice. The
 *   reading is contested by the digital_gold_reading (which treats Bitcoin as
 *   a store-of-value asset where throughput constraints are features, not
 *   bugs) and the protocol_ossification_reading (which treats any protocol
 *   change as illegitimate unless approaching universal consensus). This
 *   constraint story models the p2p cash reading ONLY—its victim set, its
 *   beneficiaries, its extractive structure—without describing the contest or
 *   averaging across readings.
 *
 * KEY AGENTS:
 *   - Protocol developers (organized, agenda-setter): defend block size constraints as necessary for decentralization and censorship-resistance
 *   - Miners (powerful, beneficiary/payer): profit from fee scarcity but bear reputational cost if payment access collapses
 *   - Node operators (moderate, beneficiary): benefit from manageable hardware costs; exit mobile if costs rise
 *   - Low-value transaction users (powerless, beneficiary): gain censorship-resistant payment access
 *   - Excluded transaction users (powerless, payer): locked out by fee markets during congestion
 *   - High-fee bearing users (powerless, payer): bear the cost of throughput constraints in the form of exponential fees
 *   - Exchange operators (institutional, agenda-setter/beneficiary): intermediate settlement and profit from high-fee periods
 *   - Legacy payment incumbents (institutional, payer): bear existential threat from the network if the reading succeeds
 *   - Regulatory authorities (institutional, observer): monitor whether the network constitutes a regulated payment system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.72).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.59).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin P2P Cash Transaction Priority Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "cryptocurrency/monetary systems/technology governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, '6fd02cf3-32f9-4ec4-ba89-55bff38dc948').
narrative_ontology:cs_kernel_codification('6fd02cf3-32f9-4ec4-ba89-55bff38dc948', fixed_text).
narrative_ontology:cs_authority_grounding('6fd02cf3-32f9-4ec4-ba89-55bff38dc948', extraction).
narrative_ontology:cs_interpretation_layer_present('6fd02cf3-32f9-4ec4-ba89-55bff38dc948').
narrative_ontology:cs_reading_relation('6fd02cf3-32f9-4ec4-ba89-55bff38dc948', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fd02cf3-32f9-4ec4-ba89-55bff38dc948', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('6fd02cf3-32f9-4ec4-ba89-55bff38dc948', foundational, payment_system_censorship_resistance_primary).
narrative_ontology:cs_axiom_status(payment_system_censorship_resistance_primary, holdable).
narrative_ontology:cs_axiom_grounding('6fd02cf3-32f9-4ec4-ba89-55bff38dc948', payment_system_censorship_resistance_primary, deontological).
narrative_ontology:cs_axiom('6fd02cf3-32f9-4ec4-ba89-55bff38dc948', foundational, throughput_scaling_legitimate_for_payment_access).
narrative_ontology:cs_axiom_status(throughput_scaling_legitimate_for_payment_access, holdable).
narrative_ontology:cs_axiom_grounding('6fd02cf3-32f9-4ec4-ba89-55bff38dc948', throughput_scaling_legitimate_for_payment_access, instrumental).
narrative_ontology:cs_reference_frame('6fd02cf3-32f9-4ec4-ba89-55bff38dc948', peer_to_peer_electronic_cash_system).
narrative_ontology:cs_drift_state('6fd02cf3-32f9-4ec4-ba89-55bff38dc948', contemporary_high_fee_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6fd02cf3-32f9-4ec4-ba89-55bff38dc948', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, low_value_transaction_users).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, unbanked_populations).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, censorship_resistant_payment_advocates).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, high_fee_bearing_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, excluded_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, legacy_payment_system_incumbents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, protocol_developers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, exchange_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, censorship_resistance_advocates).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, miners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users sending small payments across borders or outside banking systems benefit from censorship-resistant, low-fee transactions. They gain access to a payment rail uncontrolled by any single institution and free from capital controls or payment reversals. Their cost is volatility, technical complexity, and confirmation time.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, low_value_transaction_users, beneficiary,
    powerless, biographical, mobile, global).

% Users attempting transactions during periods of network congestion face exponentially rising fees or transaction rejection. They bear the cost of the p2p cash commitment: block size constraints that prioritize censorship resistance over throughput create fee markets that exclude low-value transactions. They cannot exit without losing access to the network's censorship-resistance property.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, high_fee_bearing_users, payer,
    powerless, biographical, constrained, global).

% Users who cannot afford transaction fees during congestion periods are locked out of the network entirely. They wanted access to censorship-resistant payment but are priced out; they bear the cost of the architecture's choice to maintain full-node participation and decentralization over transaction throughput. Their alternative is returning to censored payment rails.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, excluded_transaction_users, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, excluded_transaction_users, observer).

% Set and defend the transaction throughput constraints (block size limits) that implement the p2p cash reading's censorship-resistance architecture. They maintain that throughput limits preserve decentralization (nodes can run on commodity hardware, resistant to centralization). They benefit from intellectual control over the protocol's direction and from the narrative authority of being the 'true' Bitcoin custodians.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, protocol_developers, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, protocol_developers, beneficiary).

% Maintain full nodes and validate the chain under the throughput constraints. They benefit from censorship resistance and from the protocol's resistance to regulatory capture. Their cost is hardware and bandwidth burden, which the block size constraint keeps manageable. They have exit options if hardware costs rise (mobile if they switch roles or spin down nodes).
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, node_operators, beneficiary,
    moderate, biographical, mobile, global).

% Secure the network through proof-of-work and validate transactions. Under the p2p cash reading, miners benefit from low throughput (fee markets create scarcity-driven fee revenue during congestion; they profit from transaction backlog). They are simultaneously constrained: sustained high fees damage the network's legitimacy as a payment system, which could trigger hard fork attempts that devalue their equipment. They have arbitrage options (can switch mining to other blockchains).
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, miners, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, miners, payer).

% Banks and payment networks that would be displaced if Bitcoin achieved the p2p cash reading's stated goal bear the cost of the threat to their rent-extracting payment infrastructure. They do not directly participate in Bitcoin; they bear the cost through regulatory and political pressure to suppress or degrade the network's censorship-resistance property. Their exit option is regulatory capture or competitive imitation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, legacy_payment_system_incumbents, payer,
    institutional, generational, arbitrage, global).

% Intermediate between on-chain settlement and user convenience by batching transactions and managing custody. They benefit from the censorship-resistance property (can operate across borders despite capital controls) and from the high-fee periods that make exchange settlement the only accessible on-chain option for small users. They set the agenda through fee-acceptance thresholds and custody policies.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, exchange_operators, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, exchange_operators, beneficiary).

% Benefit from the reading's vindication of the principle that monetary systems should be censorship-resistant and decentralized. They collect the legitimacy and narrative authority of defending this principle. They bear the cost of defending throughput constraints that damage practical usability for low-value transactions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, censorship_resistance_advocates, beneficiary,
    moderate, generational, mobile, global).

% Monitor whether Bitcoin's fee structure and transaction throughput constitute a working payment system subject to regulation or a financial asset beyond their jurisdiction. They observe from a seat of potential enforcement power if the network's payment-system framing is accepted.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__p2p_cash_reading, miners).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__p2p_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the double-spending problem for electronic peer-to-peer transactions without a trusted third party, enabling direct transfer of value across the internet with cryptographic proof rather than institutional mediation. The coordination problem is: how to validate transactions in a decentralized network where participants may be adversarial or disappear.
% TRANSFER_FUNCTION: Transfers monetary value directly between parties (the stated function). Under sustained congestion, also transfers a fee-scarcity tax from low-value users to miners and exchanges—those who can afford high fees or batch transactions gain settlement access while those who cannot are priced out. The reading prioritizes the censorship-resistance property over throughput, which makes this secondary transfer function structurally inevitable.
% ABSENT_VOICES: Excluded transaction users (those priced out by fees) and legacy payment-system users (those who did not choose Bitcoin but are affected by capital-flow displacement) are not in the room where block size and throughput are debated. Their objection—'we wanted payment access, not asset scarcity'—is structurally absent from protocol governance.
% DISAPPEARANCE_RATIONALE: If the p2p cash reading's constraints (throughput limits, full-node participation cost as design objective, censorship-resistance priority) disappeared and were replaced with alternative protocol goals (maximizing transaction throughput, accepting centralized mining, or optimizing for store-of-value), the entire legitimacy narrative of the network would shift. Miners' fee revenue structure would collapse. Node operators' role would disappear if centralization became acceptable. The regulatory status of the network would change. The network would reorganize around new incentives.
% FOUNDING_PROBLEM: Sent via email (dated 2008, described in the whitepaper): digital currency systems require a way to prevent double-spending without a trusted intermediary. Existing payment systems depend on financial institutions as trusted third parties, which takes a cut and enables censorship and reversals. A peer-to-peer electronic cash system would eliminate that dependency.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (prevention of double-spending in a decentralized network) is solved and uncontested—the blockchain proof-of-work solution works. The founding problem statement specifically says 'electronic cash system' and 'peer-to-peer transactions without going through a financial institution.' However, whether this problem is LIVE—whether the p2p cash goal remains the primary purpose—is contested by the digital_gold_reading (which claims the store-of-value use case has superseded payment) and the protocol_ossification_reading (which claims stability, not payment throughput, is the primary virtue). Protocol developers argue the founding problem is live. Critics from the excluded and high-fee populations, plus comparative analysis of transaction volumes and fees against legacy payment systems, attest the founding problem is partially unsolved for practical payment use.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (early period, low network congestion, abundant on-chain space) to 0.68 (later period, sustained congestion, high fees creating scarcity-driven extraction from low-value users). Theater ratio rises from 0.22 to 0.41, indicating that narrative justification (decentralization, censorship-resistance) grows as a proportion of enforcement activity—the protocol developers must increasingly invoke the censorship-resistance principle to defend throughput constraints that directly harm the stated payment goal. Suppression requirement rises from 0.48 to 0.72, tracking the growing need to suppress hard-fork attempts by users and miners who want higher throughput. The measurement series is authored on one shared time grid: every metric is authored at every examined time point (0, 2, 4, 8, 12, 16). All measurements are marked 'observed' because they track historical Bitcoin network congestion, fee escalation, and miner behavior from roughly 2015–2024.
 *
 * PERSPECTIVAL GAP:
 *   From the protocol developers' seat, the constraint is Rope with high legitimacy: a coordinated solution to double-spending that preserves decentralization. From the excluded users' seat, the constraint is Snare: a rule that nominally offers access but priced-out implementation. From the miners' seat, it is Tangled Rope: they coordinate transactions (rope) while extracting scarcity-driven fees (rope crossing into snare). This multi-seat divergence is the measurement the engine is built to detect. The authored claim is Tangled Rope (which reflects the constraint's true structure: genuine coordination plus asymmetric extraction), while the metrics track how the extraction component grows over time relative to the coordination component.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-value transaction users (beneficiaries) have low directionality (d near 0.0) because they gain access to censorship-resistant settlement without running infrastructure or bearing enforcement costs directly. Protocol developers (agenda-setters) have low-to-moderate directionality (d ~0.3–0.4) because they maintain the protocol but do not directly extract rents—they derive benefit from narrative control and intellectual authority. Miners have moderate-to-high directionality (d ~0.5–0.6): they benefit from fee revenue during congestion but are constrained by the network's legitimacy as a payment system. Excluded users (payers) and high-fee users (payers) have high directionality (d near 1.0) because they are locked out or priced out by design choices they cannot influence. Exchange operators (agenda-setters/beneficiaries) have moderate directionality (d ~0.5) because they intermediate but also profit from high-fee periods. Legacy payment incumbents (payers) have very high directionality (d ~0.9) despite not participating directly, because they bear existential threat from capital-flow displacement. No directionality overrides are necessary; the structural relationships derive cleanly from beneficiary/victim status and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('send money peer-to-peer without intermediaries') is substantively solved—double-spending is prevented, transactions are decentralized, no single intermediary controls the system. However, the p2p cash reading's claim to be solving a PAYMENT SYSTEM problem is in mandatrophy: the network is no longer optimized for practical payments (fees make it inaccessible for low-value transactions; confirmation times are unreliable during congestion). The protocol developers argue the founding problem remains live because censorship-resistance itself is the payment—the value is access to a censored-resistant rail, even if fees are high. The high-fee users and excluded users argue the founding problem is dead: they have censorship-resistant access (the real problem) but not usable electronic cash (the whitepaper's stated solution). This is a case where the constraint's classification prevents mislabeling: it is NOT a pure rope (genuine coordination) because the extraction from excluded users is not incidental to the coordination—it is structurally necessary to the architecture choice (throughput limits). It is NOT a pure snare because the coordination function is real and beneficial for some users. Tangled Rope correctly captures the hybrid: coordinated solution to one problem (double-spending in a decentralized network) layered with extraction from another set of users (those priced out by the throughput-as-feature choice).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    throughput_constraint_necessity,
    'Is the block size limit structurally necessary for censorship-resistance and decentralization, or is it an arbitrary choice that harms the payment goal without proportional benefit to decentralization?',
    'Comparative analysis of blockchain networks with different block size policies (Bitcoin, Bitcoin Cash, Ethereum) and their decentralization metrics (node count, geographic distribution, hardware requirements); analysis of whether the full-node participation cost (driven by block size) is the actual bottleneck to decentralization or whether other factors (mining consolidation, network topology) dominate.',
    'If the constraint is necessary, the extraction from high-fee users is coordination cost, not extractive overhead. If arbitrary, the constraint is a pure snare hiding as rope, and the protocol developers'' beneficiary seat is fraudulent. The classification would shift from Tangled Rope to Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(throughput_constraint_necessity, empirical, 'Whether the throughput constraint serves decentralization or merely extracts rents.').

omega_variable(
    payment_system_definition,
    'Does a payment system require practical, everyday usability for low-value transactions, or can censorship-resistant access to settlement (regardless of fee) count as payment-system functionality?',
    'Comparative standards analysis: how do regulatory bodies and monetary-system scholars define ''payment system'' for Bitcoin; what does the whitepaper''s technical definition imply; do transaction volumes and user behavior in the p2p cash reading align with payment-system patterns or asset-market patterns.',
    'If payment systems require practical usability, the founding problem is dead (the p2p cash reading has failed to solve it), and the constraint is mandatrophied extraction. If censorship-resistant access alone counts as payment-system status, the founding problem is live, and the fee extraction is legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(payment_system_definition, conceptual, 'What definition of ''payment system'' applies to the p2p cash reading''s claims.').

omega_variable(
    exit_option_collapse_mechanism,
    'Are high-fee users genuinely unable to exit (trapped) because the network''s censorship-resistance property is irreplaceable, or are they identity-locked to the network by ideological commitment to Bitcoin maximalism?',
    'Survey of user behavior during congestion: do users migrate to alternative censorship-resistant systems (Monero, Zcash, Lightning Network), or do they remain on-chain despite high fees; do they cite technical irreplaceability or ideological commitment to Bitcoin specifically.',
    'If trapped (no alternative), the constraint''s extraction is maximum—there is no competing supply. If identity-locked (ideological commitment), the constraint''s effective extraction is lower because exit is possible but psychologically costly; the suppression is partly internalized (users accept fees as the price of ''real'' Bitcoin). A true identity-lock would be captured by an omega on internalized suppression, below.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_collapse_mechanism, empirical, 'Why high-fee users remain in the network despite pricing pressure.').

omega_variable(
    internalized_suppression_vs_structural,
    'Is the suppression of hard-fork attempts and high-throughput alternatives driven by structural barriers (mining consolidation, exchange listing requirements) or by internalized commitment to the p2p cash reading''s principles?',
    'Post-fork empirical analysis: when hard forks occur (Bitcoin Cash, 2017; protocol contentions 2015–2017), do users and miners show measurable switching costs (structural suppression) or ideological resistance (internalized suppression). Do they cite technical reasons or normative reasons for rejection.',
    'If structural, the suppression metric (0.72 at interval end) reflects real barriers and enforcement cost. If internalized, the true suppression is lower on paper but effective in practice (users carry the suppression with them even if exit barriers fall). This affects the reading''s terminal classification: high structural suppression → Tangled Rope; high internalized suppression → Snare with identity lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_vs_structural, empirical, 'Whether suppression of alternatives is structural or internalized.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the p2p_cash_reading logically foreclose the digital_gold_reading, or can both readings coexist in different parties'' frameworks simultaneously?',
    'Examine whether a single protocol instantiation can satisfy both goals (censorship-resistant payment AND scarce asset), or whether optimizing for one necessarily damages the other. The digital_gold_reading claims throughput constraints are features; the p2p_cash reading claims they are bugs. Can both be true?',
    'If foreclosed, one reading must eventually dominate and the other''s adherents face hard exit. If coexist, the constraint family contains genuinely incompatible equilibria and institutional pressure will determine which reading wins institutional power. This omega is routed to cs_structure.reading_relations as a check on the declared relation type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the p2p cash and digital gold readings are logically incompatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2, 0.26).
narrative_ontology:measurement_basis(bitc_tr_t2, observed).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement_basis(bitc_tr_t4, observed).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement_basis(bitc_tr_t8, observed).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(bitc_tr_t12, observed).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(bitc_tr_t16, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2, 0.45).
narrative_ontology:measurement_basis(bitc_be_t2, observed).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(bitc_be_t4, observed).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(bitc_be_t8, observed).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(bitc_be_t12, observed).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement_basis(bitc_be_t16, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2, 0.55).
narrative_ontology:measurement_basis(bitc_su_t2, observed).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement_basis(bitc_su_t4, observed).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement_basis(bitc_su_t8, observed).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement_basis(bitc_su_t12, observed).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement_basis(bitc_su_t16, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__p2p_cash_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper kernel is decomposed into three readings: p2p_cash_reading (this constraint), digital_gold_reading (store-of-value optimization), and protocol_ossification_reading (protocol immutability as primary virtue). Each reading instantiates different ε values (extractiveness referents differ: the p2p_cash reading measures extraction from payment-access denied users; the digital_gold reading measures extraction from those holding inflationary fiat; the protocol_ossification_reading measures extraction from those wanting protocol changes). These are not the same constraint viewed from three angles—they have different victim sets, different beneficiary structures, and different terminal classifications. They are linked as a constraint family through network.affects_constraints to enable cross-reading comparative analysis and to document the kernel decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
