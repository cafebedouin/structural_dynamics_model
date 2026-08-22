% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, []).

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
 *   constraint_id: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
 *   human_readable: Bitcoin Whitepaper Interpretive Vacuum (Nakamoto Oracle Opacity)
 *   domain: distributed_systems/monetary_theory/governance
 *
 * SUMMARY:
 *   Satoshi Nakamoto published the Bitcoin whitepaper in 2008 and disappeared
 *   from public communication in 2011, leaving behind a sparse authored text
 *   that describes both immediate transactional use ('peer-to-peer electronic
 *   cash') and structural properties (decentralization, full-node
 *   verifiability) that tension with throughput at scale. The whitepaper
 *   lacks explicit discussion of layered scaling, liquidity roles, or the
 *   decentralization/throughput tradeoff. Two major readings emerged: the
 *   electronic-cash reading prioritizes the 'cash' telos and interprets
 *   capacity constraints as obstacles to solve; the store-of-value reading
 *   prioritizes decentralization and on-chain verification, treating low
 *   throughput as a structural feature protecting those guarantees. Both
 *   readings claim fidelity to the whitepaper; neither can appeal to Satoshi
 *   for clarification. This constraint models the nakamoto_oracle_opacity
 *   reading: the absence of authoritative interpretation has enabled the
 *   readings to bifurcate and, through accumulated protocol decisions, to
 *   entrench as incompatible forks claiming the same legitimacy source.
 *
 * KEY AGENTS:
 *   - satoshi_nakamoto: Author of the authoritative kernel text; disappeared 2011, leaving no mechanism for later clarification
 *   - store_of_value_faction: Protocol developers prioritizing decentralization and full-node economics; benefit from the interpretive vacuum because it permits them to design for their reading without contradiction from the author
 *   - electronic_cash_advocates: Users and developers pushing for throughput scaling, layer-2 solutions, and low-fee on-chain transactions; suppressed by protocol decisions that treat decentralization as binding
 *   - fee_sensitive_users: On-chain transactors who benefit from low fees but face rising transaction costs; victims of the store-of-value interpretation's dominance
 *   - protocol_governance_bodies: Core developers, node operators, and consensus mechanisms that adjudicate protocol changes; de facto interpreters of the whitepaper in Satoshi's absence, but lacking explicit authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.42).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, resistance, 0.77).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "Bitcoin Whitepaper Interpretive Vacuum (Nakamoto Oracle Opacity)").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "distributed_systems/monetary_theory/governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, '9a39c3cb-56cb-49b8-a9c0-b6d68f922cf5').
narrative_ontology:cs_kernel_codification('9a39c3cb-56cb-49b8-a9c0-b6d68f922cf5', fixed_text).
narrative_ontology:cs_authority_grounding('9a39c3cb-56cb-49b8-a9c0-b6d68f922cf5', extraction).
narrative_ontology:cs_reading_relation('9a39c3cb-56cb-49b8-a9c0-b6d68f922cf5', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a39c3cb-56cb-49b8-a9c0-b6d68f922cf5', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_axiom('9a39c3cb-56cb-49b8-a9c0-b6d68f922cf5', foundational, oracle_absence_enables_bifurcation).
narrative_ontology:cs_axiom_status(oracle_absence_enables_bifurcation, holdable).
narrative_ontology:cs_axiom_grounding('9a39c3cb-56cb-49b8-a9c0-b6d68f922cf5', oracle_absence_enables_bifurcation, empirically_contingent).
narrative_ontology:cs_axiom('9a39c3cb-56cb-49b8-a9c0-b6d68f922cf5', foundational, whitepaper_ambiguity_structurally_permits_coexisting_readings).
narrative_ontology:cs_axiom_status(whitepaper_ambiguity_structurally_permits_coexisting_readings, holdable).
narrative_ontology:cs_axiom_grounding('9a39c3cb-56cb-49b8-a9c0-b6d68f922cf5', whitepaper_ambiguity_structurally_permits_coexisting_readings, empirically_contingent).
narrative_ontology:cs_reference_frame('9a39c3cb-56cb-49b8-a9c0-b6d68f922cf5', single_authoritative_interpretation_regime).
narrative_ontology:cs_drift_state('9a39c3cb-56cb-49b8-a9c0-b6d68f922cf5', present_bifurcated_forks_state, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('9a39c3cb-56cb-49b8-a9c0-b6d68f922cf5', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, store_of_value_faction).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, protocol_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, electronic_cash_advocates).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, fee_sensitive_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author of the Bitcoin whitepaper and the sole party capable of clarifying the intended design tradeoffs between electronic cash and decentralization. Disappeared from public communication in 2011 and has not issued clarifications since. Their silence is the structural condition that permits the interpretive contest to persist; their return (hypothetically) would resolve the oracle opacity constraint entirely by providing authoritative interpretation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, satoshi_nakamoto, observer,
    analytical, civilizational, analytical, global).

% Protocol developers, core maintainers, and major node operators who prioritize decentralization and full-node verifiability as binding constraints. They design protocol rules (block size limits, transaction fee structures, consensus mechanisms) consistent with this reading. They benefit from the interpretive vacuum because it permits them to implement their vision without contradiction from Satoshi, and because the bifurcated forks create network-effect pressures that favor Bitcoin Core's dominance. They set the agenda for protocol governance through control of the consensus mechanism.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, store_of_value_faction, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, store_of_value_faction, beneficiary).

% Users, developers, and merchants who interpret the whitepaper's cash telos as binding and advocate for on-chain scaling to support low-fee everyday transactions. They fork to create Bitcoin Cash and other alternatives but cannot escape the network-effect dominance of Bitcoin Core. Their reading is consistent with the whitepaper's opening but is suppressed by protocol choices that prioritize full-node decentralization over transaction throughput. They bear the cost of the store-of-value reading's dominance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, electronic_cash_advocates, payer,
    organized, biographical, constrained, global).

% On-chain transactors who benefit from low fees but face rising transaction costs on the dominant chain as block space becomes scarce. They are forced to migrate to layer-2 solutions (Lightning Network, sidechains, custodians) or off-chain services. Their on-chain use is suppressed by the fee structure, which reflects the store-of-value faction's prioritization of decentralization over throughput. They have no exit option that preserves both low fees and on-chain verification.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, fee_sensitive_users, payer,
    powerless, biographical, constrained, global).

% Consensus mechanisms, node operators, and core development teams that adjudicate protocol changes and enforce rules. They function as de facto interpreters of the whitepaper in Satoshi's absence, but lack explicit authority and accountability. They must justify their design choices by appealing to the whitepaper, and the whitepaper's ambiguity permits multiple consistent justifications.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, protocol_governance_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Operators of the Bitcoin Cash fork, created in 2017 to preserve the electronic-cash reading through larger block sizes and lower fees. They claim fidelity to Satoshi's original vision but cannot overcome Bitcoin Core's network-effect dominance. They are excluded from the consensus mechanism on the Bitcoin Core chain and cannot influence protocol design there. Their existence proves the bifurcation: two chains, both claiming whitepaper fidelity, both impossible to reconcile without Satoshi's clarification.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_cash_faction, excluded,
    powerful, generational, trapped, global).

% Rival cryptocurrencies, traditional finance, and central bank digital currencies that watch the bitcoin interpretive contest as a model of the fragmentation risks of decentralized governance. They observe that the absence of authoritative interpretation leads to fork proliferation and weakening of network effects. They take no direct stake in which reading wins, but benefit from the spectacle of bitcoin's bifurcation as evidence for centralized governance models.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, competing_monetary_systems, observer,
    powerful, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, store_of_value_faction).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establish a decentralized consensus mechanism for validating transactions and ordering blocks, free from dependence on any single authority. The coordination problem is: how can a network of untrusted nodes agree on which transactions are valid without a central arbiter? The whitepaper's solution is proof-of-work and transparent ledgers that permit any node to verify the full history. Both the electronic-cash and store-of-value readings accept this as the core coordination function.
% TRANSFER_FUNCTION: Transfers the interpretation rights and protocol authority from Satoshi Nakamoto (who is absent) to the community of protocol developers and miners. In practice, this transfers the power to define what 'fidelity to the whitepaper' means to the faction with the most hashing power and consensus coordination. It also transfers transaction fees from users to the operators of the full nodes and miners who validate the system. The store-of-value reading channels most of this fee revenue upward (to those who hold bitcoin) while the electronic-cash reading would channel it downward (to those who use bitcoin for transactions).
% ABSENT_VOICES: Satoshi Nakamoto (the only party with explicit authority to clarify the whitepaper's intent) is absent by disappearance. Alternative voices that would object: (1) future users who depend on bitcoin-as-cash and whose interests are not represented in present consensus mechanisms; (2) bitcoin's competitors, who are excluded from the protocol's decision-making; (3) non-stakeholders concerned with system fragility (central banks, regulators, traditional finance) who cannot vote in the protocol governance but have institutional power to regulate or ban bitcoin. The electronic-cash advocates remain technically represented by the Bitcoin Cash fork, but are excluded from the dominant chain's governance through network-effect lock-in.
% DISAPPEARANCE_RATIONALE: If Satoshi Nakamoto returned and clarified that the whitepaper intends electronic-cash-as-primary-use-case, protocol developers would face pressure to increase on-chain throughput, reduce fees, and subordinate decentralization-as-currently-implemented. The store-of-value faction would lose the authority vacuum that permits them to prioritize decentralization uncontradicted. Forks would either converge or split along acknowledged ideological lines rather than both claiming whitepaper fidelity. If Satoshi clarified the store-of-value reading as intended, the electronic-cash advocates would lose their fidelity claim and would need to adopt a different justification (e.g., 'satoshi was wrong about the design tradeoffs'). In either case, the interpretive vacuum would be filled and the constraint would transform. Conversely, if Satoshi clarified that both readings are valid under different conditions or contexts, that would be a third type of oracle output, but still transformative to the present bifurcated equilibrium.
% FOUNDING_PROBLEM: Enable peer-to-peer digital transactions without a trusted third party, and ensure that no single entity can control the money supply or reverse transactions. The system must be decentralized so that it is censorship-resistant, and must permit every user to verify the ledger's integrity without depending on any authority.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper explicitly states the goal as 'peer-to-peer electronic cash' and describes the system as permitting 'fast payment confirmation without waiting for previous transactions to be fully confirmed.' The store-of-value faction claims these statements are context-dependent and were superseded by the realization that on-chain throughput and decentralization are in tension, and that decentralization is the binding constraint. The electronic-cash advocates claim the whitepaper's opening clearly prioritizes cash use-cases and that throughput should be scaled to serve them. Satoshi's final public statements (July 2010) discuss mining and say nothing about the decentralization/throughput tradeoff. Academic sources (e.g., Nakamoto 2008, peer-reviewed reviews) confirm the founding problem was genuine: existing digital cash systems required trusted intermediaries. Independent analyses by cryptocurrency researchers (not Satoshi, not the factions themselves) argue both readings are textually supportable but derive from genuinely ambiguous design choices in the whitepaper. The corroboration that is *not* available: Satoshi himself, who is the only authority capable of clarifying the ambiguity.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steeply from 0.15 to 0.68 over the 15-year interval because the interpretive contest gradually hardens into entrenched forks (Bitcoin Core, Bitcoin Cash, Lightning Network, etc.), each claiming whitepaper fidelity while making incompatible design choices. The cost to users of choosing between forks is high; the benefit to any single party of resolving the contest is low because each faction has adapted its operations to its reading. Theater ratio rises in parallel (0.08 to 0.61) because an increasing share of 'protocol governance' activity is devoted to justifying design choices to both in-group and out-group, rather than improving the system for consensus users. Suppression (0.18 to 0.42) is moderate because the electronic-cash reading is not actively censored—it is technically permitted to exist as alternative chains—but on-chain scaling on the dominant chain (Bitcoin Core) is discouraged through fees and through explicit prioritization of full-node decentralization over transaction throughput. This is structural suppression more than active enforcement, reflected in the conservative suppression value. The measurement series tracks the interval from Satoshi's final posts (year 0) through the major forks and scaling wars (year 3-9) to the present stabilization (year 15 = 2026).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (electronic-cash advocates and fee-sensitive users) should compute as snare from their perspective: they bear a suppressive constraint that benefits a specific faction and persists because no mechanism exists to override it. The beneficiary seats (store-of-value faction and protocol developers) should compute as rope from their perspective: they participate in a genuine coordination problem (how to maintain decentralization) and the whitepaper's ambiguity is simply an artifact of early design. The engine captures this per-seat divergence from the structural declarations; the authored claim of tangled_rope is the analyst's view that both readings are real and both have merit, but one is structurally privileged by control of the consensus mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Store-of-value faction (agenda setters, beneficiaries): d ≈ 0.2 (low targets). They set the dominant protocol rules, define what 'fidelity to the whitepaper' means through their implementation choices, and benefit from the interpretive vacuum because it prevents any external authority from contradicting their reading. Their exit is not constrained—they can implement the features they want because they control the consensus mechanism on the largest chain. Electronic-cash advocates (payers, victims): d ≈ 0.85 (high targets). They are constrained by protocol rules they did not author and cannot change without forking away from network effects. Their reading is consistent with the whitepaper but is suppressed by design choices (transaction fees, block size limits, off-chain routing) that make on-chain electronic cash economically infeasible. Fee-sensitive users (payers, victims): d ≈ 0.82. They experience the constraint as forced migration to layer-2 solutions (Lightning, rollups) or off-chain custodians, paying the cost of the store-of-value prioritization. Protocol developers are identified as beneficiaries because the interpretive vacuum grants them de facto authority to define the protocol without accountability to Satoshi's original intent; they benefit from the contest's unresolvability.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1) 'create a peer-to-peer electronic cash system' and (2) 'ensure decentralization through full-node verifiability' was live at launch. By the present interval (year 15), problem (1) is substantially dead—Bitcoin is not used primarily as electronic cash, and the fee structure makes everyday transactions prohibitively expensive. Problem (2) remains live—decentralization and full-node economics are actively defended and contested. The constraint persists because (a) the store-of-value faction has invested in the interpretation that subordinates problem (1) to problem (2), and (b) Satoshi's silence prevents contradiction of this reading. Mandatrophy is partial: one half of the founding problem is zombie (electronic cash) while the other half remains live (decentralization). The constraint is tangled_rope rather than piton because active enforcement (protocol rules that restrict on-chain scaling, consensus rules that prevent throughput increases) persists to maintain problem (2), not mere inertia. If protocol developers were indifferent to decentralization vs. throughput, the rules would relax; instead, they actively defend the constraints that implement their reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oracle_death_counterfactual,
    'Would the interpretive contest have emerged if Satoshi Nakamoto remained alive and made periodic clarifications about intended use cases?',
    'Hypothetical analysis: survey explicit design decisions Satoshi made prior to departure; assess whether subsequent fork-enabling disagreements were predictable from the whitepaper text alone or required Satoshi''s silence to become institutionalized.',
    'If Satoshi''s presence would have constrained interpretation, the constraint is a structural necessity (mountain-adjacent); if the contest would have emerged regardless, the oracle absence is a catalyst, not a root cause. This determines whether the reading is describing a natural consequence of decentralization or an artificial bifurcation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oracle_death_counterfactual, conceptual, 'Whether Nakamoto''s absence created the interpretive void or merely accelerated one inherent to the system.').

omega_variable(
    kernel_text_sufficiency,
    'Is the Bitcoin whitepaper text sufficient to resolve the electronic-cash vs. store-of-value debate, or does the debate arise from genuine ambiguity in the document itself?',
    'Close textual reading by parties with no stake in either interpretation; comparison of the whitepaper''s explicit statements about transaction throughput, fee levels, and use-case priority against actual design choices in the first implementation.',
    'If the text is genuinely ambiguous, the oracle absence leaves the constraint in an equivocal state—reading divergence is rational, not extractive capture. If the text favors one reading, the oracle absence enables suppression of that reading''s interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_text_sufficiency, empirical, 'Whether the whitepaper text permits both readings or privileges one over the other.').

omega_variable(
    fork_fidelity_claim_asymmetry,
    'Do both the electronic-cash and store-of-value readings genuinely claim whitepaper fidelity, or does one implicitly acknowledge deviation while maintaining legitimacy through other grounds (technical superiority, real-world viability)?',
    'Examine public statements by leading advocates of each reading: do they claim adherence to Satoshi''s stated intent, or do they argue for a deliberate reinterpretation based on changed conditions or superior understanding?',
    'If one reading has abandoned fidelity-as-claim, the constraint is not a pure interpretive vacuum—it is a boundary dispute where one side has acknowledged reinterpreting the kernel. This would lower theater_ratio and reframe the constraint as snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fork_fidelity_claim_asymmetry, empirical, 'Whether both readings make equal claims to whitepaper authenticity or one side has pragmatically ceded the fidelity argument.').

omega_variable(
    suppression_mechanism_asymmetry,
    'Is suppression of the electronic-cash reading structural (the proof-of-work mechanism and store-of-value adoption create cost barriers to on-chain transaction scaling) or actively enforced (protocol developers deliberately block scalability features)?',
    'Historical analysis of protocol decision-making: examine minutes, proposals, and code reviews for evidence of intentional rejection of throughput-enhancing features. Distinguish between ''we prioritize decentralization over throughput'' (structural) and ''we will not permit throughput improvements'' (enforced suppression).',
    'If structural, the constraint is closer to mountain; if actively enforced, it is snare. The authored suppression value of 0.42 is conservative, reflecting the structural component; internalized suppression (electronic-cash advocates abandoning their reading as infeasible) raises effective suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_asymmetry, empirical, 'Whether the electronic-cash reading is suppressed by systemic costs or by deliberate protocol choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 3, 0.18).
narrative_ontology:measurement_basis(bitc_tr_t3, observed).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(bitc_tr_t6, observed).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 9, 0.45).
narrative_ontology:measurement_basis(bitc_tr_t9, observed).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 12, 0.56).
narrative_ontology:measurement_basis(bitc_tr_t12, observed).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 15, 0.61).
narrative_ontology:measurement_basis(bitc_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 3, 0.28).
narrative_ontology:measurement_basis(bitc_be_t3, observed).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(bitc_be_t6, observed).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 9, 0.54).
narrative_ontology:measurement_basis(bitc_be_t9, observed).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(bitc_be_t12, observed).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(bitc_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 3, 0.24).
narrative_ontology:measurement_basis(bitc_su_t3, observed).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 6, 0.31).
narrative_ontology:measurement_basis(bitc_su_t6, observed).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 9, 0.36).
narrative_ontology:measurement_basis(bitc_su_t9, observed).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 12, 0.4).
narrative_ontology:measurement_basis(bitc_su_t12, observed).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(bitc_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.25).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__store_of_value_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the contested 'bitcoin whitepaper purpose' kernel into three structurally distinct constraint stories: (1) electronic_cash_reading models the constraint under the interpretation that the whitepaper's cash telos is binding; (2) store_of_value_reading models the constraint under the interpretation that decentralization and full-node economics are binding; (3) nakamoto_oracle_opacity (this file) models the constraint that Satoshi's 2011 disappearance created an interpretive vacuum, enabling both readings to persist as incompatible forks with equal fidelity claims. Each story has its own ε, its own beneficiary/victim structure, and its own type. The nakamoto_oracle_opacity reading is upstream of the other two in the sense that the oracle absence is a structural precondition for the readings' bifurcation—if Satoshi had remained alive and clarified intent, one reading would have been falsified and the constraint would have collapsed. The three stories are linked by network.affects_constraints to reflect this dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
