% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__store_of_value_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: bitcoin_whitepaper_purpose__store_of_value_reading
 *   human_readable: Bitcoin Whitepaper Purpose: Store-of-Value Reading
 *   domain: monetary_theory/distributed_systems/technology_governance
 *
 * SUMMARY:
 *   Bitcoin's whitepaper is a contested kernel. The original document
 *   promised a peer-to-peer electronic cash system for everyday payments, but
 *   after Satoshi Nakamoto's 2011 disappearance, the Bitcoin community split
 *   interpretively. This constraint story instantiates the store-of-value
 *   reading: the claim that decentralization and full-node verifiability are
 *   the binding constraints, and on-chain capacity is deliberately
 *   subordinated to these goals. Under this reading, the 1MB block size limit
 *   is not a temporary bottleneck but a permanent feature protecting the
 *   system's core property—its resistance to corruption by any single party
 *   or coalition. Long-term holders and node operators benefit from this
 *   arrangement (a scarce, immutable store of value); low-value transaction
 *   users and payment-dependent merchants are pushed off-chain or out of the
 *   system entirely. The constraint is actively enforced through consensus
 *   rule-setting: node operators collectively reject block-size increases,
 *   mining pools signal for soft forks, and core developers gatekeep
 *   proposals that would deprioritize decentralization. The claim/metric gap
 *   is intentional: this reading is CLAIMED as a foundational consensus
 *   mechanism (rope-like coordination) but operates with substantial
 *   extraction (high suppression, rising theater as justifications for high
 *   fees accumulate) because it concentrates the benefits of scarcity on a
 *   specific beneficiary set while imposing costs on excluded users.
 *
 * KEY AGENTS:
 *   - long_term_hodlers: primary beneficiary — benefit from limited supply and immutable ledger
 *   - node_operators: primary beneficiary + agenda-setter — enforce the 1MB rule through consensus, gate-keep block-size proposals
 *   - core_developers: agenda-setter — propose consensus changes, recommend against higher throughput
 *   - low_value_transaction_users: primary victim — priced off base layer, forced to layer-2 or payment processors
 *   - payment_frequency_dependent_merchants: victim — uneconomical on-chain fees, squeezed toward centralized alternatives
 *   - lightning_network_operators: secondary beneficiary + agenda-setter — profit from scarcity-driven demand for layer-2 scaling
 *   - electronic_cash_advocates: excluded — would argue for higher capacity, lack consensus voting power
 *   - whitepaper_interpreters: observers — shape narrative legitimacy of the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.71).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Whitepaper Purpose: Store-of-Value Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "monetary_theory/distributed_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, '5706cbd1-56b6-4ba2-80f5-dd1c973b9f08').
narrative_ontology:cs_kernel_codification('5706cbd1-56b6-4ba2-80f5-dd1c973b9f08', fixed_text).
narrative_ontology:cs_authority_grounding('5706cbd1-56b6-4ba2-80f5-dd1c973b9f08', distributed).
narrative_ontology:cs_reading_relation('5706cbd1-56b6-4ba2-80f5-dd1c973b9f08', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('5706cbd1-56b6-4ba2-80f5-dd1c973b9f08', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('5706cbd1-56b6-4ba2-80f5-dd1c973b9f08', foundational, decentralization_over_throughput).
narrative_ontology:cs_axiom_status(decentralization_over_throughput, holdable).
narrative_ontology:cs_axiom_grounding('5706cbd1-56b6-4ba2-80f5-dd1c973b9f08', decentralization_over_throughput, instrumental).
narrative_ontology:cs_axiom('5706cbd1-56b6-4ba2-80f5-dd1c973b9f08', foundational, full_node_verifiability_binding).
narrative_ontology:cs_axiom_status(full_node_verifiability_binding, holdable).
narrative_ontology:cs_axiom_grounding('5706cbd1-56b6-4ba2-80f5-dd1c973b9f08', full_node_verifiability_binding, instrumental).
narrative_ontology:cs_reference_frame('5706cbd1-56b6-4ba2-80f5-dd1c973b9f08', peer_to_peer_decentralized_ledger).
narrative_ontology:cs_drift_state('5706cbd1-56b6-4ba2-80f5-dd1c973b9f08', post_2017_high_fee_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5706cbd1-56b6-4ba2-80f5-dd1c973b9f08', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_hodlers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, payment_frequency_dependent_merchants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulate and hold Bitcoin over years or decades as a store of value. They benefit from the constraint's prioritization of decentralization and verification over transaction throughput: limited supply, immutable ledger, and the inability to inflate the currency without a hard fork protect their holdings from dilution. Their purchase power depends on the reading that rejects high-frequency transaction capacity in favor of staking long-term verifiability.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_hodlers, beneficiary,
    organized, civilizational, arbitrage, global).

% Run full nodes that verify the entire history of transactions and enforce consensus rules. They benefit from the constraint's insistence on keeping block size capped (1MB) and verification accessible to modest hardware: a large, decentralized node set is only feasible when running a node is not computationally or financially prohibitive. They collectively enforce the rule through social consensus and soft forks (signaling). They set the de facto agenda by choosing which consensus rules to validate and signal for adoption.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators, agenda_setter).

% Maintain the Bitcoin Core reference implementation and propose consensus changes. They do not control Bitcoin unilaterally—nodes vote with their choice of software—but they gatekeep proposals and prioritize the store-of-value reading in code and documentation. Their constraint-setting power is advisory: they propose limits on block size, they recommend against changes that would centralize nodes, but adoption depends on node operator consensus. Their exit costs are high: abandoning Bitcoin means abandoning a decades-long project and reputation capital.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, core_developers, agenda_setter,
    powerful, generational, constrained, global).

% Attempt to use Bitcoin for everyday payments (coffee, groceries, remittances). They face escalating on-chain transaction fees as block space remains scarce (1MB blocks = ~7 transactions per second global capacity). As economic activity grows, low-value transactions are priced off the base layer. They are directed toward the Lightning Network (payment channels, off-chain) or second-layer solutions, which add complexity and custody risk. Identity lock arises from the narrative that 'you must use Bitcoin for transactions'—the medium's promise—even as the constraint makes that economically irrational for routine payments. They have no seat at the consensus rule-setting table.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_transaction_users, payer,
    powerless, biographical, identity_locked, global).

% Operate retail or service businesses requiring frequent, low-value transactions (food vendors, service providers). On-chain Bitcoin becomes economically unviable for their use case: a $5 transaction costs $0.50–$3 in fees during high-demand periods, making the network unsuitable for point-of-sale payments. They are squeezed toward centralized payment processors (PayPal, credit cards) or second-layer solutions (Lightning) that add intermediation and custody. Their spatial scope is regional because the constraint's impact is most acute in low-income or high-transaction-frequency regions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, payment_frequency_dependent_merchants, payer,
    moderate, biographical, constrained, regional).

% Build and maintain the Lightning Network and similar layer-2 scaling solutions. They benefit from the constraint's cap on on-chain capacity: the scarcity of block space creates demand for second-layer networks, making their infrastructure economically viable and necessary. They also exercise agenda-setting power by defining layer-2 protocol standards and choosing which base-layer features to support. Their exit is high: abandoning layer-2 infrastructure means abandoning network effects and invested capital.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_operators, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_operators, beneficiary).

% Academic researchers, historians, and technical commentators who read the Bitcoin whitepaper and assert what its binding constraints are. They produce no economic goods but shape the narrative interpretation of Bitcoin's purpose. This reading is held by a significant but contested coalition (store-of-value advocates, decentralization maximalists). They compete with the electronic_cash_reading for interpretive authority.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, whitepaper_interpreters, observer,
    powerful, generational, analytical, global).

% Would argue for higher on-chain capacity (larger blocks, faster transaction times) to enable Bitcoin to serve as everyday digital cash, consistent with the whitepaper's title. They are structurally excluded from consensus rule-setting because they lack node-operator consensus; their proposals (Bitcoin Cash fork, larger-block variants) operate as rival chains rather than reforms. Their exclusion is enforced by the very constraint this story describes: the store-of-value reading is socially embedded in the majority node-operator set.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, electronic_cash_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_hodlers).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__store_of_value_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a global distributed ledger system where every participating node can independently verify the entire transaction history without trusting a central authority. The coordination problem is: how do you achieve this without requiring each node to run expensive infrastructure that only well-resourced parties can afford? The binding answer is: keep blocks small (1MB) so verification remains cheap, and accept that on-chain throughput will be low.
% TRANSFER_FUNCTION: Transfers the benefit of a scarce, verifiable, globally-consistent ledger to long-term holders and node operators. It transfers the cost of low transaction throughput and high per-transaction fees to users who want to use Bitcoin for frequent, small-value payments. The transfer is mediated by consensus rules (block size cap) that make the base layer unsuitable for payments and push users toward off-chain alternatives.
% ABSENT_VOICES: Low-value transaction users and merchant communities in high-transaction-frequency domains (remittance corridors, retail, service economies) are absent from consensus rule-setting. They would argue that the whitepaper's title ('Bitcoin: A Peer-to-Peer Electronic Cash System') binds the system to supporting everyday payments, and that the store-of-value reading is a post-hoc reinterpretation imposed after mining became industrial. They lack voting representation in node-operator consensus and cannot fork the network without accepting minority status.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—if consensus rule-setting shifted to prioritize on-chain transaction volume over decentralization—the entire governance and economic structure of Bitcoin would reorganize: block sizes would increase, node-running costs would rise, the node operator base would shrink (consolidating to well-resourced parties), the Lightning Network ecosystem would contract, the appeal to decentralization maximalists would evaporate, and the long-term value narrative around Bitcoin would shift fundamentally. The constraint is not incidental; it is constitutive of the current reading's legitimacy.
% FOUNDING_PROBLEM: Bitcoin was designed to solve the double-spending problem and enable peer-to-peer transactions without a trusted intermediary. The original whitepaper framed this as digital cash. The founding problem admits two readings: (1) create a cash system anyone can transact with instantly and cheaply, or (2) create a decentralized store of value that no single entity can corrupt or inflate, even if transaction throughput is limited.
% FOUNDING_PROBLEM_CORROBORATION: Satoshi Nakamoto's 2011 disappearance eliminated a single authoritative interpreter. The whitepaper text itself is ambiguous: the title says 'cash,' the abstract promises 'no double-spending without a trusted third party,' but the technical sections emphasize proof-of-work consensus and node decentralization without mentioning transaction throughput targets. The store-of-value reading is corroborated by: (a) core Bitcoin developers and node-operator coalitions (2015–2026) who blocked block-size increases and advocated for layer-2 scaling; (b) mainstream institutional adoption narratives that treat Bitcoin as 'digital gold' rather than 'cash'; (c) the observed fact that after 2017, on-chain transaction fees made routine payments uneconomical and pushed users toward layer-2 networks. The electronic_cash_reading is corroborated by: (a) Bitcoin Cash advocates and some early Bitcoin developers who argue for increased throughput; (b) use-case data from low-income regions where high fees prevent adoption; (c) the original title and early adoption by payment-use communities. Neither reading is settled; both are actively defended.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__store_of_value_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__store_of_value_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs over the interval (0.35 → 0.68) because on-chain transaction fees rise as economic activity grows against fixed 1MB block capacity. At t=0 (early period, low volume), the 1MB cap is not yet extractive—plenty of block space, low fees, the coordination benefits dominate. By t=16 (current state, high volume), block space is permanently scarce, fees are high, and users are systematically excluded or forced toward intermediaries. Theater rises (0.25 → 0.42) because justifications accumulate: the 1MB cap is defended as necessary for decentralization and security, yet the real enforcement is now preserving scarcity and high fees that benefit holders and node operators. Suppression is high and stable (0.55 → 0.71) because the constraint's persistence depends on actively rejecting proposals to increase capacity—node operators vote against block-size increases, developers recommend against it, mining pools signal against larger blocks. This is not a spontaneous coordination outcome but an enforced rule. The accessibility collapse is moderate (0.58): alternatives to Bitcoin exist (other cryptocurrencies, payment systems, second-layer networks), so users are not completely trapped—they can exit, but at a cost (learn a new system, adopt layer-2 custody risk, or return to centralized payment processors). The resistance is high (0.72): electronic cash advocates, payment-dependent merchants, and low-income users all actively push for higher throughput, but they lack consensus voting power because node operators hold the consensus rules and control the gate.
 *
 * PERSPECTIVAL GAP:
 *   The node-operator and hodler seats compute this constraint as rope (genuine coordination that benefits them) while the low-value transaction user seat computes it as snare (pure extraction that traps them). From the node-operator seat: 'We maintain a decentralized, uncensorable ledger; the 1MB block size protects that from centralization; users who need faster transactions can use layer-2 networks; scarcity is a feature, not a bug.' From the low-value transaction user seat: 'I am told Bitcoin is for everyone, but fees make it impossible for me to use, and layer-2 custody risks are not acceptable; I am excluded by design, not by accident.' The engine computes these divergent types from the structural data: node operators and hodlers have the power and exit options (arbitrage) to make the constraint work for them; low-value users have none (identity_locked exit: they are told this is the future of money, but it works against them). The committer frame (this is a reading, not a fact) does not resolve the divergence—it heightens it. The store-of-value reading is a choice that certain factions made after Nakamoto's disappearance; the electronic_cash_reading is an equally defensible choice from the text. The engine's per-seat divergence is not a defect; it is the measurement that different readings instantiate different constraint structures for different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term hodlers and node operators are beneficiaries because they benefit from the constraint's enforcement: scarcity protects their holdings from dilution, decentralization protects their holdings from state confiscation, and the 1MB block cap keeps verification accessible so they retain agency in consensus rule-setting. Their directionality is low (d ~0.1–0.2): the constraint subsidizes them. Their power is organized/powerful (node operators form coalitions, hodlers have financial incentive to coordinate) and their exit options are arbitrage (they can move capital between different cryptosystems or traditional assets if Bitcoin loses its store-of-value narrative). Low-value transaction users have high directionality (d ~0.8–0.9): they are targets. Their power is powerless (individually, they have no voting stake in consensus; collectively they form a large group but one that lacks institutional representation). Their exit is identity_locked (the narrative 'Bitcoin is the future of money' is embedded in their adoption story; exiting means accepting they were deceived about Bitcoin's purpose). Core developers and lightning network operators occupy a middle position (d ~0.4–0.6): they benefit from the constraint's enforcement and gate-keep proposals, but they also depend on building community consensus and do not unilaterally control the network. No directionality overrides are needed; the beneficiary/victim + power + exit chain produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits a foundational mandatrophy: the founding problem (double-spending without trusted third party) has been solved by both the store-of-value AND electronic_cash readings. The store-of-value reading claims the founding problem is 'live' (decentralization is under ongoing threat from state regulation and institutional consolidation), but this is a different problem from the original: the original was technical (double-spending), the current is political/economic (maintaining decentralization against external pressure). The electronic_cash reading claims the founding problem is 'dead'—Bitcoin solved it and now persists as infrastructure for a use case (payments) it no longer enables cheaply. The mandatrophy is not resolved by technical data; it is resolved by which reading's framing you accept. The constraint prevents mandatrophy denial: the blockchain data and fee markets are public, and every user sees directly whether they can afford to transact. The divergence between the founding problem ('cash for everyone') and the current operation ('store of value for the committed') is irreducible under the store-of-value reading and constitutes the reading's core vulnerability to the electronic_cash challenge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_permanence,
    'Is the founding problem (peer-to-peer transactions without trusted intermediary) still live, or has it been solved and replaced by a different problem (maintaining decentralization against external pressure)?',
    'Longitudinal analysis of attack vectors: if the primary threats to Bitcoin are technical (double-spending, 51% attacks), the original founding problem remains live; if the primary threats are political/economic (state regulation, institutional consolidation, mining pool centralization), the founding problem has been solved and a new problem replaced it.',
    'If the founding problem is dead, the store-of-value reading is maintaining a solution to a solved problem while creating new costs (high fees, user exclusion) — reclassifying toward snare. If the founding problem is live, the constraint''s cost is justified as the price of solving it — maintaining tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_permanence, conceptual, 'Whether the constraint solves the original founding problem or a successor problem.').

omega_variable(
    decentralization_necessity,
    'Is the 1MB block size limit necessary for decentralization, or would blocks up to 4MB-8MB still enable sufficient decentralization while improving transaction throughput?',
    'Empirical data from Bitcoin Cash and other higher-block-size experiments: if those networks maintain sufficient node counts and geographic distribution, the necessity claim fails; technical analysis of node-running costs at various block sizes.',
    'If the 1MB limit is not necessary for decentralization, the store-of-value reading is using decentralization as a post-hoc justification for scarcity that benefits hodlers and node operators — pure extraction. If the limit is necessary, the constraint is a genuine coordination requirement to achieve the stated goal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_necessity, empirical, 'Whether the 1MB block size cap is structurally required for node decentralization.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Do the store-of-value and electronic-cash readings genuinely coexist (held by different parties, neither foreclosing the other), or does one reading logically foreclose the other?',
    'Formal logical analysis of the axioms: if a Bitcoin node can simultaneously enforce both full-node verifiability AND on-chain payment scalability without inconsistency, the readings coexist; if enforcing one requires rejecting the other''s core claim, one forecloses the other.',
    'If the readings coexist, this constraint is a political choice among live options, reclassifying as snare (extraction under the guise of necessity). If one forecloses the other, the foreclosed reading is empirically false or logically incoherent, and the surviving reading is legitimate — maintaining tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether the store-of-value and electronic-cash readings are logically independent or mutually exclusive.').

omega_variable(
    layer2_custody_vs_onchain_decentralization_tradeoff,
    'Is pushing users toward Lightning Network and layer-2 solutions a genuine decentralization tradeoff (accepting reduced on-chain throughput to preserve base-layer decentralization), or is it exporting the problem elsewhere (concentrating custody and verification on layer-2 operators)?',
    'Measurement of centralization metrics on Lightning Network nodes, payment channel operators, and layer-2 hub concentration; comparison to a counterfactual higher-block-size Bitcoin with base-layer decentralization.',
    'If layer-2 centralizes more than a higher-capacity base layer would, the constraint is moving centralization risk rather than reducing it — reclassifying as snare. If layer-2 maintains or improves decentralization, the constraint is a genuine Pareto improvement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(layer2_custody_vs_onchain_decentralization_tradeoff, empirical, 'Whether layer-2 scaling trades away decentralization vs. distributes verification risk differently.').

omega_variable(
    kernel_reading_satoshi_intent,
    'Which reading (store-of-value vs. electronic-cash) would Nakamoto have endorsed had he remained present and engaged?',
    'Textual analysis of Nakamoto''s 2008-2010 communications; reasoning from his stated goals and technical design choices; inference from the whitepaper''s structure and emphasis.',
    'Evidence for Nakamoto''s electronic-cash intent would delegitimize the store-of-value reading as post-hoc reinterpretation; evidence for store-of-value intent would validate the reading as true to the founder''s vision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_satoshi_intent, preference, 'Unknowable: Satoshi''s intent remains absent. This omega documents that the reading''s legitimacy is contested precisely because the oracle is absent (nakamoto_oracle_opacity).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement_basis(bitc_tr_t2, observed).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement_basis(bitc_tr_t4, observed).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement_basis(bitc_tr_t8, observed).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(bitc_tr_t12, observed).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement_basis(bitc_tr_t16, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement_basis(bitc_be_t2, observed).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement_basis(bitc_be_t4, observed).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(bitc_be_t8, observed).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement_basis(bitc_be_t12, observed).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement_basis(bitc_be_t16, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2, 0.6).
narrative_ontology:measurement_basis(bitc_su_t2, observed).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 4, 0.64).
narrative_ontology:measurement_basis(bitc_su_t4, observed).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement_basis(bitc_su_t8, observed).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(bitc_su_t12, observed).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(bitc_su_t16, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__store_of_value_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% This constraint instantiates the store-of-value reading of the bitcoin_whitepaper_purpose kernel. The sibling constraint bitcoin_whitepaper_purpose__electronic_cash_reading instantiates the competing reading that prioritizes on-chain transaction capacity for payments. Both readings reference the same whitepaper text but draw different structural conclusions about what is binding. The epsilon values differ substantially (store-of-value reads high extraction from the fee market; electronic-cash reads high extraction from the off-chain custody structure), and the beneficiary/victim sets are inverted. They are linked as a constraint family because interpreting the whitepaper is the shared problem; the readings are not alternative measurements of the same constraint but rather alternative constraints instantiated from the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__store_of_value_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
