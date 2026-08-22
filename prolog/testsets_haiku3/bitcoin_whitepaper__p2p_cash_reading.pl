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
 *   human_readable: Bitcoin as Censorship-Resistant P2P Cash
 *   domain: economic/technological/monetary
 *
 * SUMMARY:
 *   The Bitcoin whitepaper articulates a vision of peer-to-peer electronic
 *   cash that solves double-spending through distributed consensus without
 *   trusted intermediaries. This constraint story instantiates one specific
 *   reading of that vision: Bitcoin as a censorship-resistant medium of
 *   exchange, where the primary legitimacy claim rests on transactional
 *   accessibility and low-friction value transfer. This reading prioritizes
 *   block size expansion, layer-2 scaling, and protocol evolution that
 *   enables high transaction throughput and low fees. The competing readings
 *   — digital gold (Bitcoin as scarce store of value) and protocol
 *   ossification (change illegitimacy unless near-universal) — are not
 *   described here but linked via network relations. The p2p cash reading has
 *   a real coordination function (double-spend prevention) but creates
 *   asymmetric extraction: users priced out of on-chain transactions during
 *   congestion, populations excluded by fee structures, and regulatory
 *   pressure demanding payment surveillance all bear costs to sustain the
 *   reading's legitimacy claim.
 *
 * KEY AGENTS:
 *   - p2p_cash_proponents: Developers and researchers defending the transactional reading; control protocol evolution agenda
 *   - unbanked_populations: Beneficiaries of censorship resistance; constrained by fees and technical barriers
 *   - high_fee_payers: Victimized by fee markets; face economic exclusion during congestion
 *   - excluded_transaction_participants: Powerless agents who cannot afford or technically manage on-chain transactions; identity-locked to alternative systems
 *   - digital_gold_proponents: Excluded stakeholders who contest the reading's prioritization of transaction volume over scarcity
 *   - miners_and_node_operators: Dual-positioned beneficiaries (fee collection) and payers (infrastructure costs, consensus disputes)
 *   - regulatory_authorities: Observers seeking to impose transaction reporting that contradicts the reading's censorship-resistance claim
 *   - layer_two_developers: Ambiguously positioned beneficiaries of on-chain scarcity who are simultaneously commodified by it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.71).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin as Censorship-Resistant P2P Cash").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "economic/technological/monetary").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, '8b13ae87-399b-4d49-bfb0-6acb6bf00f6f').
narrative_ontology:cs_kernel_codification('8b13ae87-399b-4d49-bfb0-6acb6bf00f6f', fixed_text).
narrative_ontology:cs_authority_grounding('8b13ae87-399b-4d49-bfb0-6acb6bf00f6f', distributed).
narrative_ontology:cs_reading_relation('8b13ae87-399b-4d49-bfb0-6acb6bf00f6f', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b13ae87-399b-4d49-bfb0-6acb6bf00f6f', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('8b13ae87-399b-4d49-bfb0-6acb6bf00f6f', foundational, transactional_accessibility_primary_value).
narrative_ontology:cs_axiom_status(transactional_accessibility_primary_value, holdable).
narrative_ontology:cs_axiom_grounding('8b13ae87-399b-4d49-bfb0-6acb6bf00f6f', transactional_accessibility_primary_value, instrumental).
narrative_ontology:cs_axiom('8b13ae87-399b-4d49-bfb0-6acb6bf00f6f', foundational, censorship_resistance_requires_low_friction).
narrative_ontology:cs_axiom_status(censorship_resistance_requires_low_friction, holdable).
narrative_ontology:cs_axiom_grounding('8b13ae87-399b-4d49-bfb0-6acb6bf00f6f', censorship_resistance_requires_low_friction, empirically_contingent).
narrative_ontology:cs_reference_frame('8b13ae87-399b-4d49-bfb0-6acb6bf00f6f', bitcoin_as_peer_to_peer_electronic_cash).
narrative_ontology:cs_drift_state('8b13ae87-399b-4d49-bfb0-6acb6bf00f6f', contemporary_proof_of_work_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8b13ae87-399b-4d49-bfb0-6acb6bf00f6f', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, p2p_cash_proponents).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, unbanked_populations).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, high_fee_payers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, excluded_transaction_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, miners_and_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, layer_two_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, miners_and_node_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developers, researchers, and advocates who maintain that Bitcoin's primary purpose is frictionless electronic payment between peers without intermediaries. They argue for block size increases, layer-2 scaling, and protocol changes that prioritize transaction throughput and low fees. They operate nodes, vote on protocol evolution through code contributions and consensus signaling, and articulate the legitimacy frame for Bitcoin as a transaction medium.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, p2p_cash_proponents, agenda_setter,
    organized, generational, mobile, global).

% Individuals in jurisdictions with capital controls, unstable currencies, or financial exclusion who depend on censorship-resistant payment channels to access global markets and preserve purchasing power. They benefit from Bitcoin's availability outside banking infrastructure but are constrained by rising on-chain transaction fees and technical barriers to participation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, unbanked_populations, beneficiary,
    powerless, biographical, trapped, global).

% Users transacting small amounts on-chain during periods of network congestion, paying fees that approach or exceed the transaction value. They bear the cost of the reading's implied fee-market structure, where scarcity of on-chain capacity is allocated by price rather than by protocol guarantee. Their alternatives are delaying transaction, using layer-2 solutions requiring technical sophistication, or abandoning Bitcoin for lower-cost systems.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, high_fee_payers, payer,
    powerless, biographical, constrained, global).

% Populations for whom the reading's emphasis on on-chain censorship resistance comes at the cost of practical usability: those without internet access for layer-2 systems, without the cognitive load tolerance for self-custody and fee management, or without the initial capital to front fees to establish themselves on the network. They are excluded not by protocol rule but by the economic structure the reading's fee prioritization instantiates.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, excluded_transaction_participants, payer,
    powerless, biographical, identity_locked, global).

% Advocates who read Bitcoin as a scarce store-of-value asset first and transactional medium second. They argue for protocol conservatism (restricted block size) to ensure scarcity and network security. They would object that the p2p cash reading sacrifices the asset's fundamental properties for transaction convenience and thus undermines Bitcoin's real value proposition.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, digital_gold_proponents, excluded,
    organized, generational, mobile, global).

% Operators of the consensus infrastructure who collect block rewards and transaction fees. They benefit from the constraint's enforcement (validating transactions, enforcing rules) and collect rents from fee markets during congestion. They also bear the cost of maintaining the network and adapting to protocol disputes about appropriate fee structures and block capacity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, miners_and_node_operators, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, miners_and_node_operators, payer).

% Government agencies, financial regulators, and AML/CFT bodies that view Bitcoin's censorship resistance as a compliance challenge. They observe the constraint's operation from the outside, seeking to impose transaction reporting requirements, exchange controls, or asset seizure capabilities that would undermine the reading's core legitimacy claim.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% Developers building layer-2 solutions (Lightning Network, sidechains, state channels) that enable micropayments and high-throughput transaction. They benefit from the on-chain constraint's scarcity (which justifies their layer-2 solutions) while also being harmed by it (reduced on-chain adoption, need to compete for users). They occupy a structurally ambiguous position: the reading requires them as a solution but also commodifies their work.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, layer_two_developers, beneficiary,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__p2p_cash_reading, miners_and_node_operators).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__p2p_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the double-spend problem in digital cash without a trusted intermediary: participants can verify transaction history and update account balances through distributed consensus, enabling peer-to-peer value transfer without banking infrastructure.
% TRANSFER_FUNCTION: Moves purchasing power from coin holders paying transaction fees to miners (block rewards and fee collection), and from high-frequency on-chain transactors to those willing to wait or use layer-2 solutions. In periods of congestion, the constraint moves the ability to transact on-chain from those unable to pay market-clearing fees to those with sufficient capital.
% ABSENT_VOICES: Digital gold advocates would argue that on-chain fee pressure is acceptable and even desirable for scarcity. Protocol ossification advocates would argue that frequent scaling changes threaten consensus stability. Regulatory authorities would argue the reading's censorship resistance is a policy failure. None of these are seated at the consensus table where the p2p cash reading's legitimacy is defended.
% DISAPPEARANCE_RATIONALE: If Bitcoin ceased functioning as a censorship-resistant transaction medium (through regulatory seizure, protocol collapse, or market displacement), users dependent on capital-control evasion, remittance corridors outside banking, and unmediated value transfer would face sharp friction. The disappearance would not restore banking access in excluded jurisdictions but would eliminate one option for circumventing it.
% FOUNDING_PROBLEM: Electronic transactions require either a trusted third party to prevent double-spending or a consensus mechanism to establish transaction order without trusting any single entity. Before Bitcoin, all electronic cash systems depended on centralized payment processors whose existence created censorship and surveillance risks.
% FOUNDING_PROBLEM_CORROBORATION: The p2p cash reading asserts the founding problem is live and still solved by Bitcoin's operation. The digital gold reading contests that p2p transaction is still the problem Bitcoin best solves (asset scarcity being the real value proposition). Regulatory testimony and economic analysis from authors outside the Bitcoin developer community attest that the founding problem persists in excluded jurisdictions but that Bitcoin's volatility and fee structure make it impractical for routine transaction in those contexts.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.35 to 0.62 as the interval progresses, driven by escalating on-chain fee pressure and increasing evidence that the reading's transaction-throughput promise is being traded for protocol stability and consensus conservatism. The theater_ratio plateaus at 0.48, indicating the constraint maintains genuine coordination (the double-spend solution is real) but with substantial performative overhead: rhetoric about Bitcoin as cash persists even as transaction economics push toward store-of-value behavior. Suppression_requirement rises to 0.71 and stabilizes, reflecting active regulatory pressure to undermine censorship resistance through KYC/AML requirements and transaction surveillance. The measurement series is authored on a shared time grid (every metric at every time point, same interval endpoints). The oscillation in extractiveness from t=16 to t=20 reflects the cyclical dynamics of fee-market pressure: periods of high on-chain congestion (high extraction) alternate with periods of layer-2 adoption growth (extraction moderated by technical solutions), but the baseline extraction remains elevated because the reading's promised accessibility never fully recovers.
 *
 * PERSPECTIVAL GAP:
 *   From the p2p cash agenda-setter seat (developers, proponents), the constraint is a genuine coordination mechanism that evolves to solve scaling challenges while preserving censorship resistance — extractiveness is moderate, suppression is external (regulatory), and the reading is empirically vindicated. From the high-fee-payer seat (small transactors), the same constraint operates as enforced extraction: they are priced out of on-chain use, face suppression through fee markets, and experience the reading as a legitimacy cover for scarcity-based rents. The engine computes these divergent types from the structural data: the proponents' mobile exit and agenda-setting role derive toward beneficiary directionality, while the high-fee-payers' trapped/constrained exit and victim designation derive toward target directionality, resulting in computed classifications that diverge substantially from the shared authored metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The p2p_cash_proponents occupy an organized, mobile position with agenda-setting authority: they control which protocol changes are legitimate, articulate the reading's core claim, and benefit from the constraint's operation (no single point of censorship for their transactions). Directionality for this seat is near-beneficiary (d ~0.2). High-fee-payers and excluded-transaction-participants are powerless, constrained/identity-locked, with no voice in consensus: they bear the cost of fee-market allocation and have few exit options that preserve censorship resistance. Directionality for these seats is near-target (d ~0.85). Miners are dual-positioned: they benefit from fee collection but are payers in the sense that they incur infrastructure costs and must invest in network maintenance that the reading's consensus pressure does not directly incentivize. Unbanked populations are beneficiaries (censorship resistance is their core value) but constrained by fees, yielding moderate d (~0.55). The reading_relations in cs_structure establish that digital_gold_reading coexists_with this reading (both are live positions in the Bitcoin ecosystem, held by different organized factions) and protocol_ossification_reading influences this reading (conservative protocol stances reduce the scaling solutions available to the p2p cash reading).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (double-spend in decentralized systems) is live and still solved by Bitcoin's operation. However, the founding_problem_status='contested' because the digital gold reading argues that asset scarcity, not transaction utility, is the solved problem, and the protocol ossification reading argues that consensus stability is the real value Bitcoin preserves. The six_questions mismatch consumer would examine disappearance_verdict='world_rearranges' (yes, Bitcoin's loss would sharply affect excluded populations who depend on it) against founding_problem_status='contested' — the mismatch suggests that while Bitcoin solves a real coordination problem, the p2p cash reading's particular instantiation of that problem may be contested. The theater_ratio holding at ~0.48 indicates substantial real function (the double-spend coordination) with meaningful performative overlay (the 'Bitcoin as cash' narrative persists even as on-chain economics contradict it). This is consistent with tangled_rope: genuine coordination paired with asymmetric extraction that requires active enforcement (suppression_requirement rising to 0.71 as regulatory pressure intensifies and technical barriers to layer-2 adoption grow).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fee_market_necessity,
    'Is the rising fee market a necessary feature of the reading''s security model, or an artifact of block-size restrictions that could be relaxed without sacrificing consensus properties?',
    'Empirical analysis of on-chain capacity constraints relative to transaction demand, and simulation of security properties under different block-size parameters. Historical comparison with protocol expansions (SegWit, taproot) and their effects on fee pressure.',
    'If fee markets are unnecessary (a policy choice to restrict capacity), the reading''s extractiveness could be substantially reduced by protocol changes the p2p cash proponents favor, strengthening their legitimacy claim. If fee markets are necessary for security, the extraction becomes a structural cost of the coordination function, and the victim set of excluded transactors is inherent to the reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fee_market_necessity, empirical, 'Whether on-chain fee pressure is a structural necessity or a policy choice.').

omega_variable(
    censorship_resistance_at_cost,
    'What proportion of the p2p cash reading''s value to unbanked populations comes from censorship resistance itself, versus from the ability to transact at all, and what is the tradeoff boundary where fee costs exceed the value of censorship-resistant access?',
    'Ethnographic research in jurisdictions with capital controls and financial exclusion; surveys of actual Bitcoin usage patterns and fee tolerance; comparison of Bitcoin adoption with alternative censorship-resistant payment methods (monero, altcoins, informal channels).',
    'If censorship resistance carries much more value than low-cost transactability, high fees are an acceptable cost and the victim set is smaller than modeled. If low-cost access is the binding constraint, high fees directly undermine the reading''s primary value proposition for unbanked populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_resistance_at_cost, empirical, 'Relative weight of censorship resistance versus transaction cost for the reading''s primary beneficiary populations.').

omega_variable(
    reading_incompatibility_with_digital_gold,
    'Can the p2p cash reading and digital_gold_reading genuinely coexist as feature of the same protocol, or does protocol choice (block size, fee policy, scaling approach) force a zero-sum choice between them?',
    'Game-theoretic analysis of incentive structures under different protocol parameters; historical analysis of fork disputes (Bitcoin Cash, etc.) showing which protocol features are traded off; examination of whether transaction-friendly parameters undermine scarcity properties or vice versa.',
    'If the readings truly coexist (no forced zero-sum), the constraint''s legitimacy can be defended across both reading communities simultaneously. If zero-sum, the coexists_with relation should be re-examined as a forecloses relation, and the p2p cash reading''s claims about protocol evolution become zero-sum political struggles rather than technical coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incompatibility_with_digital_gold, conceptual, 'Whether the p2p_cash and digital_gold readings are genuinely compatible or structurally foreclosing.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (external regulatory pressure, technical barriers imposed by others) or internalized (Bitcoin developers and miners have internalized conservative consensus norms that suppress their own transaction-scaling instincts)?',
    'Analysis of developer debates, code contributions, and fork proposals showing whether suppression comes from external pressure (government action, exchange delistings) or from internal consensus rules (rough consensus requirements, ''don''t fork without broad agreement''). Post-exit trajectory: would suppression persist after regulatory pressure lifted?',
    'If suppression is structural (external), the p2p cash reading''s extraction comes from regulatory override and could be reduced by jurisdictional choice. If suppression is internalized (developers have adopted conservative norms), the extraction is self-perpetuating and more resistant to external remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of transaction-scaling is structural or internalized in the Bitcoin developer community.').

omega_variable(
    layer_two_commodification,
    'Does the layer-2 ecosystem (Lightning Network, sidechains, rollups) genuinely serve the p2p cash reading''s goal of low-cost transaction, or does it create a new two-tier system where layer-2 adoption becomes mandatory for most users while on-chain transactions become an exclusive store-of-value feature?',
    'Empirical tracking of layer-2 adoption, user experience (complexity, capital lock-up, channel liquidity requirements), and fee structures. Comparison of intended vs. actual accessibility: are layer-2 systems usable for populations with limited internet, low technical literacy, or small transaction amounts?',
    'If layer-2 solutions genuinely democratize transaction access, the p2p cash reading''s victims (excluded transactors) migrate to a genuinely usable system and extraction decreases. If layer-2 becomes a commodified solution that creates new barriers, the reading''s extraction and victim count actually increase while theatrical coverage (theater_ratio) grows higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_two_commodification, empirical, 'Whether layer-2 solutions resolve or redistribute the reading''s victim set of excluded transactors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 16, 0.47).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(bitc_be_t20, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(bitc_su_t20, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__p2p_cash_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper kernel admits three structurally distinct readings, each instantiating a different constraint. The p2p_cash_reading (this story) prioritizes transactional accessibility and interprets block-size expansion as legitimate. The digital_gold_reading treats Bitcoin as a scarce store of value and interprets block-size restriction as necessary for scarcity. The protocol_ossification_reading treats protocol stability as the primary virtue and interprets change resistance as the core feature. Each reading has a distinct ε (epsilon), distinct beneficiary/victim structure, and distinct classification. The readings coexist in the Bitcoin ecosystem as holdings of different organized factions; they do not resolve into a single constraint. Each story is authored independently with its own metrics and stakeholders; the network.affects_constraints links record the causal dependencies (p2p cash's emphasis on transaction throughput influences protocol ossification's design space; digital gold's scarcity emphasis influences p2p cash's fee policy tradeoffs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
