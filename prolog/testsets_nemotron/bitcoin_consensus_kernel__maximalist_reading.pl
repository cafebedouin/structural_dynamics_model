% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__maximalist_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: bitcoin_consensus_kernel__maximalist_reading
 *   human_readable: Bitcoin Maximalist Reading: Immutable Monetary Covenant
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   The maximalist reading of the Bitcoin consensus kernel treats the
 *   whitepaper's 21M supply cap and fixed emission schedule as an immutable
 *   covenant — any protocol change altering monetary rules violates the
 *   founding social contract. This reading emerged from early community
 *   discourse (2010-2013) and solidified during the Blocksize War (2015-2017)
 *   as the defining boundary of 'Bitcoin.' It coordinates a global holder
 *   coalition around absolute scarcity but simultaneously extracts from
 *   protocol innovators, layer-2 builders, and users priced out of base-layer
 *   access. The constraint is claimed as tangled_rope: genuine coordination
 *   (trust-minimized scarcity) fused with asymmetric extraction (early
 *   allocative advantage locked in, innovation suppressed). The claim/metric
 *   gap is deliberate — the reading self-describes as rope (pure
 *   coordination), while the authored metrics describe substantial extraction
 *   and active suppression of alternatives.
 *
 * KEY AGENTS:
 *   - genesis_holders: Primary beneficiary (institutional/arbitrage) — locked-in allocative advantage from early distribution
 *   - early_adopters: Beneficiary (organized/arbitrage) — accumulated position before price discovery matured
 *   - institutional_hodlers: Beneficiary (institutional/arbitrage) — treat immutability as institutional-grade property right
 *   - mining_pool_operators: Agenda_setter (institutional/constrained) — enforce consensus rules; revenue depends on holder coalition legitimacy
 *   - layer2_builders: Victim (organized/constrained) — innovation constrained by base-layer rigidity; forced to build on constrained primitives
 *   - protocol_researchers: Victim (moderate/trapped) — research directions that would modify monetary rules are delegitimized
 *   - global_south_users: Victim (powerless/trapped) — priced out of base-layer sovereignty; exit options structurally limited
 *   - merchant_adoption_advocates: Victim (organized/constrained) — use case (medium of exchange) suppressed by store-of-value primacy
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.68).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.72).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Maximalist Reading: Immutable Monetary Covenant").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '2d44daf3-1de4-4926-ac62-b1af2a7a5c8b').
narrative_ontology:cs_kernel_codification('2d44daf3-1de4-4926-ac62-b1af2a7a5c8b', fixed_text).
narrative_ontology:cs_authority_grounding('2d44daf3-1de4-4926-ac62-b1af2a7a5c8b', lineage).
narrative_ontology:cs_interpretation_layer_present('2d44daf3-1de4-4926-ac62-b1af2a7a5c8b').
narrative_ontology:cs_reading_relation('2d44daf3-1de4-4926-ac62-b1af2a7a5c8b', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('2d44daf3-1de4-4926-ac62-b1af2a7a5c8b', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('2d44daf3-1de4-4926-ac62-b1af2a7a5c8b', foundational, monetary_rules_immutable_by_social_contract).
narrative_ontology:cs_axiom_status(monetary_rules_immutable_by_social_contract, holdable).
narrative_ontology:cs_axiom_grounding('2d44daf3-1de4-4926-ac62-b1af2a7a5c8b', monetary_rules_immutable_by_social_contract, conventional).
narrative_ontology:cs_axiom('2d44daf3-1de4-4926-ac62-b1af2a7a5c8b', secondary, base_layer_expressivity_sacrificed_for_verification_cost).
narrative_ontology:cs_axiom_status(base_layer_expressivity_sacrificed_for_verification_cost, holdable).
narrative_ontology:cs_axiom_grounding('2d44daf3-1de4-4926-ac62-b1af2a7a5c8b', base_layer_expressivity_sacrificed_for_verification_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('2d44daf3-1de4-4926-ac62-b1af2a7a5c8b', satoshi_whitepaper_covenant).
narrative_ontology:cs_drift_state('2d44daf3-1de4-4926-ac62-b1af2a7a5c8b', post_blocksize_war_institutionalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2d44daf3-1de4-4926-ac62-b1af2a7a5c8b', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, genesis_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, institutional_hodlers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, mining_pool_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, layer2_builders).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_researchers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, global_south_users).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, merchant_adoption_advocates).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, absolute_scarcity_as_social_contract).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, non_sovereign_money_impossibility_theorem).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold coins acquired at negligible cost (mining or early purchase). The immutable 21M cap guarantees their allocation share never dilutes. They can exit to any asset at any time — their position is the reference point for the system's value. They fund narrative infrastructure (media, conferences, lobbying) that reinforces the maximalist frame.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, genesis_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Accumulated significant holdings before 2017 price maturation. Their wealth is denominated in BTC; the immutability norm protects their purchasing power trajectory. They operate businesses (exchanges, custodians, media) that profit from the 'digital gold' narrative. Exit is trivial — they are the liquidity providers.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_adopters, beneficiary,
    organized, biographical, arbitrage, global).

% Corporate treasuries (MicroStrategy, Tesla), ETF issuers (BlackRock, Fidelity), nation-state holdings (El Salvador, Bhutan). They require legal and regulatory clarity that 'Bitcoin' means fixed supply. Their entry depended on the maximalist reading's victory in the Blocksize War. They lobby for regulatory frameworks that codify the maximalist definition.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, institutional_hodlers, beneficiary,
    institutional, generational, arbitrage, global).

% Operate the hash-rate that enforces consensus rules. Revenue comes from block subsidy (fixed schedule) + fees. They enforce the maximalist rule set because deviating risks chain split and revenue loss — the holder coalition's social consensus determines which chain is 'Bitcoin.' They cannot easily exit: capital-intensive hardware, energy contracts, and regulatory exposure tie them to the dominant chain. Some (e.g., Foundry, Antpool) have institutionalized relationships with holder coalition.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, mining_pool_operators, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, mining_pool_operators, beneficiary).

% Build Lightning, Ark, Fedimint, BitVM, rollups on Bitcoin. Base-layer rigidity (1MB block weight, limited script, no covenants) forces complex, fragile constructions. They pay opportunity cost: features that would be native on a more expressive chain require trusted bridges or complex protocols. They cannot fork the base layer (maximalist social consensus rejects it) and cannot leave (user liquidity is on Bitcoin). Some migrate to Ethereum/Solana for expressivity — constrained exit.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, layer2_builders, payer,
    organized, biographical, constrained, global).

% Academic and independent researchers exploring Bitcoin protocol improvements (drivechains, covenants, tail emission, privacy). Research proposing monetary rule changes is delegitimized — grant funding, conference acceptance, and community reception all penalize 'maximalism-heretical' work. Career capital is tied to Bitcoin ecosystem; pivoting to other chains loses domain expertise and network. Exit is identity-locked: 'Bitcoin researcher' identity fuses with maximalist frame.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_researchers, payer,
    moderate, biographical, trapped, global).

% Users in Argentina, Nigeria, Turkey, Lebanon, Venezuela seeking censorship-resistant savings and payments. Base-layer fees ($5-50) exceed daily wages — they are structurally excluded from self-custody. They use custodial solutions (Wallet of Satoshi, Strike, Binance) which reintroduce counterparty risk the protocol was meant to eliminate. They have no voice in consensus governance (no hash power, no node operation, no social capital in maximalist discourse). Exit means returning to local fiat or USD stablecoins — trapped by the very constraint that claims to serve them.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, global_south_users, payer,
    powerless, immediate, trapped, global).

% Businesses and payment processors (BitPay, Coinbase Commerce, OpenNode) trying to make Bitcoin a medium of exchange. The maximalist 'hodl' culture and fee volatility undermine payment use case. They pay the opportunity cost of a network optimized for store-of-value rather than payments. They can pivot to stablecoins or other chains for payments — constrained exit (brand/reputation tied to Bitcoin). Some advocate for layer2 adoption as compromise.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, merchant_adoption_advocates, payer,
    organized, biographical, constrained, global).

% Sees the full constraint family: maximalist_reading, utility_reading, pragmatic_synthesis. Observes how the kernel contest structures resource allocation, narrative control, and protocol evolution across the ecosystem. No material stake in any reading's victory.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the double-spend problem and the trust-in-issuance problem for a global, permissionless, non-sovereign money. The 21M cap and fixed schedule allow any participant to verify the total supply and their proportional share without trusting a central authority.
% TRANSFER_FUNCTION: Transfers option value of protocol evolution (scalability, privacy, expressive scripting, adaptive monetary policy) from builders, researchers, and excluded users to the holder coalition as locked-in allocative advantage. The transfer is not a flow of funds — it is a foreclosure of design space that would benefit the victims.
% ABSENT_VOICES: The 4 billion unbanked and underbanked who might use Bitcoin as daily money if base-layer fees were low and confirmation times fast. They are not in the consensus conversation — they lack hash power, node operation capacity, and English-language discourse access. Also absent: future generations who would inherit a monetary system frozen at 2017 design assumptions.
% DISAPPEARANCE_RATIONALE: If the maximalist constraint vanished overnight, multiple forks with different monetary rules (tail emission, larger blocks, expressive scripting) would compete. Hash rate would fragment. Holder coalition would fracture. Layer2 builders would migrate to more expressive base layers. Global south adoption patterns would shift. The 'Bitcoin' brand would become ambiguous. The world rearranges because the constraint currently structures the entire ecosystem's legitimacy, capital allocation, and development trajectory.
% FOUNDING_PROBLEM: Create digital cash that cannot be debased by any central authority — solving the trust-in-issuance problem that doomed prior attempts (e-gold, Liberty Reserve, centralized digital currencies). The whitepaper's fixed supply schedule was the mechanism, not the end.
% FOUNDING_PROBLEM_CORROBORATION: Satoshi's writings (whitepaper, forum posts, emails) emphasize 'peer-to-peer electronic cash' and scalability via payment channels — not immutable monetary policy as terminal value. The Blocksize War participants (Gregory Maxwell, Peter Todd, Luke Dashjr on small-block side; Gavin Andresen, Mike Hearn, Jeff Garzik, Roger Ver on big-block side) attest the founding problem was live and contested in 2015-2017. Independent economic historians (e.g., Finn Brunton, Nathaniel Popper) document the shift from 'cash system' to 'settlement layer' narrative. No corroboration from outside the beneficiary set for the claim that the founding problem is 'solved and frozen.'
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the transfer from would-be protocol improvements (scalability, privacy, expressive scripting) to the holder coalition's locked-in allocation. The 21M cap is not costless coordination — it forecloses entire design spaces (tail emission, demurrage, adaptive supply) that could serve different user populations. Suppression (0.72) is high because the constraint's persistence depends on active social enforcement: node operators who signal willingness to run alternative rules face social ostracization, delisting pressure, and hash-rate attacks (hashrate follow-the-chain dynamics). Theater ratio (0.25) is moderate — the 'store of value' narrative performs coordination work but increasingly masks the extractive core as the holder coalition institutionalizes. Accessibility collapse (0.78) is high because once the maximalist frame is accepted, alternatives (utility_reading, pragmatic_synthesis) appear as category errors rather than policy choices. Resistance (0.55) is significant — layer2 ecosystems, sovereign adoption (El Salvador), and academic critique sustain contestation.
 *
 * PERSPECTIVAL GAP:
 *   The holder coalition (genesis_holders, early_adopters, institutional_hodlers) experiences this as rope — a coordination miracle that solved trust in issuance. Layer2 builders and protocol researchers experience it as snare — their work is constrained by a rule they cannot change and that benefits others. Global_south_users experience it as piton — the constraint is inertially maintained, offering them no voice and no benefit. Mining_pool_operators sit in tension: they enforce the rule (agenda_setter) but their revenue depends on the holder coalition's continued legitimacy (constrained exit). The engine computes per-seat types from these structural positions; the authored claim (tangled_rope) reflects the aggregate structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (genesis_holders, early_adopters, institutional_hodlers) collect the allocative rent of fixed supply — their directionality d is low (beneficiary end). Victims (layer2_builders, protocol_researchers, global_south_users, merchant_adoption_advocates) bear the opportunity cost of foreclosed design space and priced-out access — their d is high (target end). Mining_pool_operators are structurally intermediate: they administer the constraint (agenda_setter) but are constrained by the holder coalition's social consensus (exit_options: constrained). The derivation chain places miners at d≈0.45 — slight beneficiary tilt from block rewards, but extraction exposure via hash-rate follow-the-chain dynamics. Analytical_observer at d=0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trust-minimized digital scarcity without central issuer) was live in 2008-2013. By 2017, the Blocksize War revealed the arrangement had metastasized: the immutability norm now serves to protect early allocation rather than solve the coordination problem. Mandatrophy is unresolved — the constraint persists because the holder coalition has institutionalized (ETFs, nation-state adoption, corporate treasuries) and the cost of challenging it exceeds any single actor's benefit. The constraint is not a piton (theater_ratio 0.25 < 0.5) — it actively coordinates a global coalition — but its coordination function has narrowed to 'protect the 21M cap' while its extraction surface has widened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is this constraint one reading of the bitcoin_consensus_kernel (maximalist_reading) rather than the kernel itself?',
    'Compare structural outputs across sibling readings (utility_reading, pragmatic_synthesis) — if they produce different beneficiary/victim sets, different extractiveness, different types, the kernel is contested and each reading is a distinct constraint.',
    'Confirms this JSON instantiates maximalist_reading only; sibling readings are separate constraint stories linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Kernel vs. reading identity for bitcoin consensus rules').

omega_variable(
    covenant_vs_coordination_boundary,
    'Does the 21M cap function as genuine coordination (solving trust in issuance) or as extraction (locking early allocative advantage)?',
    'Counterfactual: if a credible fork with tail emission achieved wider adoption, would the maximalist reading treat it as illegitimate (extraction defense) or as failed coordination (coordination failure)?',
    'If coordination: ε lower, rope-adjacent. If extraction: ε higher, snare-adjacent. Current ε=0.68 reflects mixed evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_vs_coordination_boundary, conceptual, 'Whether monetary immutability coordinates or extracts').

omega_variable(
    enforcement_as_miner_alignment,
    'Is miner enforcement of consensus rules alignment with holders or independent power?',
    'Observe miner behavior under contentious soft-fork activation (e.g., Taproot signaling) — do miners lead, follow, or negotiate with holder coalition?',
    'If miners independently enforce: suppression is structural (0.72 justified). If miners follow holder coalition: suppression is coordinated extraction (higher effective χ for holders).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_as_miner_alignment, empirical, 'Miner-holder power dynamics in consensus enforcement').

omega_variable(
    global_south_exit_as_coercion_evidence,
    'Do high-fee exclusion effects on global_south_users constitute structural suppression or market pricing?',
    'Measure adoption elasticity: if fee reductions (via layer2 or block space expansion) cause nonlinear adoption surges in excluded populations, the prior state was suppressive.',
    'If suppressive: global_south_users are victims of a snare component. If market pricing: they are constrained participants in a rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_south_exit_as_coercion_evidence, empirical, 'Whether fee-driven exclusion is coercive or allocative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_max_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(btc_max_tr_t5, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(btc_max_tr_t10, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(btc_max_tr_t15, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(btc_max_tr_t20, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(btc_max_tr_t25, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement(btc_max_tr_t30, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(btc_max_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(btc_max_be_t5, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(btc_max_be_t10, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(btc_max_be_t15, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(btc_max_be_t20, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(btc_max_be_t25, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(btc_max_be_t30, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(btc_max_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(btc_max_su_t5, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(btc_max_su_t10, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(btc_max_su_t15, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(btc_max_su_t20, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(btc_max_su_t25, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(btc_max_su_t30, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__maximalist_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, lightning_network_consensus).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_layer2_ecosystem).

% DUAL FORMULATION NOTE:
% Bitcoin consensus kernel decomposes into three readings with different ε and beneficiary/victim structures. Maximalist_reading (this story): ε=0.68, beneficiaries=holders/early_adopters, victims=builders/researchers/users. Utility_reading: ε≈0.15, beneficiaries=all_users, victims=minimal. Pragmatic_synthesis: ε≈0.35, beneficiaries=base_layer_holders, victims=layer2_builders (partial). The ε-invariance principle requires separate stories — the label 'Bitcoin consensus' conflates distinct structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__maximalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(bitcoin_consensus_kernel__maximalist_reading, organized, 0.35).
constraint_indexing:directionality_override(bitcoin_consensus_kernel__maximalist_reading, moderate, 0.65).
constraint_indexing:directionality_override(bitcoin_consensus_kernel__maximalist_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
