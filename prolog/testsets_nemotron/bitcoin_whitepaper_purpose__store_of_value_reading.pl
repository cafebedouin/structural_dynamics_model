% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
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
 *   human_readable: Bitcoin Whitepaper Purpose — Store of Value Reading (Decentralization and Full-Node Verifiability as Binding Constraints)
 *   domain: technology_governance/distributed_systems/monetary_theory
 *
 * SUMMARY:
 *   This story captures the 'store of value' reading of Bitcoin's purpose:
 *   the whitepaper's system is fundamentally a decentralized, verifiable
 *   scarcity machine. On-chain capacity (1MB blocks, ~7 TPS) is deliberately
 *   constrained to keep full-node verification within reach of individuals
 *   running modest hardware. The coordination function is trust-minimized
 *   verification of a fixed supply schedule; the extraction function prices
 *   low-value users off the base layer, creating a fee market that transfers
 *   value to miners and routes demand to Layer 2 (Lightning). The constraint
 *   is a tangled rope: it genuinely coordinates a global monetary system
 *   without trusted intermediaries (the coordination function is real and
 *   valued by holders and node operators) AND it asymmetrically extracts from
 *   users who need cheap on-chain transactions (the victims). The
 *   claim/metric gap is intentional: proponents claim rope (pure coordination
 *   for sound money); the metrics reveal substantial extraction from a
 *   structurally excluded class.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.52).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Whitepaper Purpose — Store of Value Reading (Decentralization and Full-Node Verifiability as Binding Constraints)").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "technology_governance/distributed_systems/monetary_theory").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, '02d94e61-b64b-447d-bccf-ccdbfdc2cb7f').
narrative_ontology:cs_kernel_codification('02d94e61-b64b-447d-bccf-ccdbfdc2cb7f', fixed_text).
narrative_ontology:cs_authority_grounding('02d94e61-b64b-447d-bccf-ccdbfdc2cb7f', lineage).
narrative_ontology:cs_reading_relation('02d94e61-b64b-447d-bccf-ccdbfdc2cb7f', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('02d94e61-b64b-447d-bccf-ccdbfdc2cb7f', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('02d94e61-b64b-447d-bccf-ccdbfdc2cb7f', foundational, decentralization_and_verifiability_are_binding).
narrative_ontology:cs_axiom_status(decentralization_and_verifiability_are_binding, holdable).
narrative_ontology:cs_axiom_grounding('02d94e61-b64b-447d-bccf-ccdbfdc2cb7f', decentralization_and_verifiability_are_binding, deontological).
narrative_ontology:cs_axiom('02d94e61-b64b-447d-bccf-ccdbfdc2cb7f', foundational, on_chain_capacity_subordinated_to_verification_accessibility).
narrative_ontology:cs_axiom_status(on_chain_capacity_subordinated_to_verification_accessibility, holdable).
narrative_ontology:cs_axiom_grounding('02d94e61-b64b-447d-bccf-ccdbfdc2cb7f', on_chain_capacity_subordinated_to_verification_accessibility, instrumental).
narrative_ontology:cs_reference_frame('02d94e61-b64b-447d-bccf-ccdbfdc2cb7f', satoshi_whitepaper_and_early_communications).
narrative_ontology:cs_drift_state('02d94e61-b64b-447d-bccf-ccdbfdc2cb7f', post_blocksize_war_consensus, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('02d94e61-b64b-447d-bccf-ccdbfdc2cb7f', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_fee_onchain_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, miners).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, miners).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, decentralization_as_primary_security_model).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, verifiable_scarcity_through_full_node_accessibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as a savings vehicle; benefit from the scarcity guarantee that limited on-chain capacity and full-node verifiability protect. Can exit to other assets but view bitcoin's monetary properties as unique. Their influence is exercised through market demand and cultural signaling rather than protocol governance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    organized, generational, mobile, global).

% Run validating nodes to independently verify the chain state; the 1MB block size cap keeps hardware and bandwidth requirements within reach of motivated individuals. They set the de facto consensus rules by choosing which software to run. Exiting means abandoning the verification capability that gives them sovereignty over the rules they follow.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, agenda_setter).

% Need affordable on-chain transactions for everyday payments, remittances, or low-value transfers. Priced out by fee markets that emerge when demand exceeds the 1MB capacity. Cannot effectively exit to Lightning without capital, technical knowledge, and counterparty liquidity. Their needs are structurally excluded from the base layer by the capacity constraint.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_fee_onchain_users, payer,
    powerless, immediate, trapped, global).

% Build and operate Lightning routing nodes and services; they benefit from the demand for off-chain scaling created by on-chain capacity scarcity. They develop the protocol and software but depend on the base layer's security model. Can pivot to other L2 designs or chains but have sunk investment in the Lightning ecosystem.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_operators, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_operators, beneficiary).

% Secure the chain with proof-of-work; collect block subsidies and transaction fees. Benefit from fee pressure created by capacity scarcity but face revenue uncertainty when subsidies halve. Can redirect hashpower to other SHA-256 chains, giving them arbitrage-grade exit. Their incentives align with holders on scarcity but diverge on fee vs. subsidy composition.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, miners, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, miners, beneficiary).

% Maintain the reference implementation (Bitcoin Core); propose and review consensus changes. Their authority is informal — code changes only activate if node operators adopt them. They are analytically positioned: they see the full structure but do not directly collect rents or bear user costs. Their legitimacy rests on technical competence and historical continuity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, protocol_developers, agenda_setter,
    institutional, generational, analytical, global).

% Argue that the whitepaper's 'peer-to-peer electronic cash' title binds the system to everyday transactional use with low fees. They were structurally marginalized in the 2015–2017 block size debate; many migrated to Bitcoin Cash or other forks. Those remaining continue to advocate but have no pathway to change the consensus rules without a hard fork.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, electronic_cash_advocates, excluded,
    moderate, biographical, constrained, global).

% Monitor on-chain activity for illicit finance; the fee market and transparent ledger create both surveillance opportunities and friction for small users. They do not set protocol rules but their enforcement actions shape which use cases are viable on-chain. Their interest is in traceability, not in the decentralization/verifiability trade-off per se.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, regulators_and_aml_compliance, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global, trust-minimized monetary system where any participant can independently verify the total supply and transaction history without trusting third parties. The binding constraint is keeping full-node operation accessible to individuals, which caps on-chain throughput.
% TRANSFER_FUNCTION: Moves transaction fee revenue from low-fee users (who are priced out or pay high fees) to miners (who collect scarcity rents) and indirectly to Lightning operators (who capture the displaced demand). Long-term holders gain purchasing power protection from the credible scarcity commitment; node operators gain sovereignty at the cost of hardware vigilance.
% ABSENT_VOICES: Users in high-inflation economies who need cheap, censorship-resistant payments but lack the technical and capital prerequisites for Lightning. Merchants who would accept bitcoin for daily commerce if fees were predictable and low. These voices are absent because the system's architecture makes their use case economically nonviable on the base layer, and they lack organized representation in protocol governance.
% DISAPPEARANCE_RATIONALE: If the 1MB cap and the 'decentralization first' priority vanished overnight, block size would likely increase through miner/operator consensus, on-chain fees would drop, and everyday transactional use would become viable again — but the trust-minimized verification property would degrade as node operation centralizes. The monetary premium attached to credible scarcity would reprice. Lightning's value proposition would collapse. The world rearranges around a different security/tradeoff frontier.
% FOUNDING_PROBLEM: How to create a digital money that no single entity can debase, censor, or shut down — solving the double-spending problem without a trusted central party, while keeping the verification of that solution accessible to anyone who chooses to run a node.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper text and early mailing list archives corroborate the double-spending/decentralization problem as the founding problem. Long-term holders and Core developers attest it remains live (central banks still debase, states still censor). Electronic cash advocates and Lightning critics attest the problem has shifted: the system now serves a different user base (savers, not spenders) and the verification accessibility guarantee is eroding as the UTXO set grows and hardware requirements creep up. No single external authority adjudicates; the contest is structural.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the fee market's transfer from priced-out users to miners and the opportunity cost borne by those who cannot use the base layer. Suppression (0.52) is moderate: the constraint does not actively prevent alternatives (Lightning, other chains, fiat), but the network effect and brand dominance of 'Bitcoin' make exit costly for users who specifically need Bitcoin's liquidity and recognition. Theater ratio (0.18) is low but rising: the 'decentralization' rhetoric increasingly covers rent extraction by entrenched interests (miners, holders, Lightning businesses) as the fee market matures. Accessibility collapse (0.72) is high but not total: alternatives exist but are functionally distinct (Lightning has different trust assumptions; other chains have different security models). Resistance (0.41) is moderate: the block size wars (2015–2017) produced a hard fork (BCH) and ongoing debate, but the Core roadmap prevailed.
 *
 * PERSPECTIVAL GAP:
 *   From the holder/node-operator seat, this is a mountain-like coordination achievement: a system that credibly cannot be inflated or censored, verified by anyone. From the low-fee user seat, it is a snare: a payment system that prices them out while calling itself 'cash.' The engine will compute this divergence from the structural data. The store-of-value reading's claim of tangled_rope acknowledges both: genuine coordination at the cost of asymmetric exclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and full-node operators are beneficiaries (d ~ 0.15–0.25): they gain scarcity credibility and verification sovereignty. Low-fee on-chain users are victims (d ~ 0.9): they bear the full cost of the capacity constraint with no offsetting benefit on the base layer. Miners are near-symmetric (d ~ 0.5): they collect scarcity rents but face competitive pressure and halving risk. Lightning operators are beneficiaries (d ~ 0.2): they capture displaced demand. Protocol developers are analytical observers (d ~ 0.5): they maintain the code but do not directly extract. Electronic cash advocates are excluded (d ~ 0.8): their preferred reading was defeated in the consensus process. Regulators are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trust-minimized digital scarcity) remains live — fiat debasement and financial censorship persist. But the arrangement has drifted: the 'electronic cash' use case was subordinated, and the beneficiary set shifted from 'users' broadly to 'holders and verifiers' narrowly. Mandatrophy is contested: the constraint still solves its founding problem, but the user coalition it serves has narrowed. The theater ratio rise suggests performative maintenance of the 'peer-to-peer cash' branding while the actual function serves a different constituency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_legitimacy,
    'Which reading of the bitcoin_whitepaper_purpose kernel is the legitimate one, or is the kernel itself irreducibly ambiguous?',
    'No formal resolution mechanism exists; the kernel has no living authoritative interpreter. Legitimacy is determined by which reading commands the consensus of node operators (the de facto constitution). Historical analysis of Satoshi''s writings and early communications may inform but not decide.',
    'If the electronic_cash_reading is legitimate, this constraint is a snare (extraction under false pretenses). If the store_of_value_reading is legitimate, the extraction is the necessary price of the coordination function (tangled rope). If the kernel is irreducibly ambiguous, both readings are coexisting constraint instantiations with different ε and victim sets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy, conceptual, 'Irreducible ambiguity in the founding text''s telos — no authoritative resolution possible after Nakamoto''s departure').

omega_variable(
    lightning_viability_as_coordination_substitute,
    'Does Lightning Network genuinely substitute for on-chain capacity as a coordination mechanism for everyday payments, or is it a distinct system with different trust assumptions that fails to serve the same users?',
    'Empirical measurement of Lightning adoption, success rates, capital efficiency, and user demographics over time. Comparison of on-chain vs. Lightning outcomes for low-value, high-frequency payment use cases in high-inflation economies.',
    'If Lightning succeeds as a coordination substitute, the extraction from low_fee_onchain_users is mitigated (they have a viable exit) and the constraint leans toward rope. If Lightning fails to serve the same users, the extraction is unmitigated and the constraint leans toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lightning_viability_as_coordination_substitute, empirical, 'Whether the off-chain scaling layer functionally replaces the excluded on-chain use case').

omega_variable(
    node_operator_centralization_trajectory,
    'Is the full-node accessibility constraint actually holding, or is node operation quietly centralizing despite the 1MB cap (UTXO set growth, bandwidth, hardware creep, custodial capture)?',
    'Longitudinal measurement of node count, geographic distribution, hardware profiles, and custodial vs. self-custodied bitcoin ratios. Correlation with block size and transaction volume.',
    'If node operation is centralizing despite the cap, the coordination function (decentralized verification) is degrading — the constraint becomes a piton (theatrical maintenance of a lost function) or a snare (extraction persists after coordination fails). If accessibility holds, the tangled rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(node_operator_centralization_trajectory, empirical, 'Whether the primary coordination guarantee (accessible full-node verification) is empirically sustained').

omega_variable(
    suppression_mechanism_fee_market,
    'Is the suppression of low-fee users structural (protocol-enforced capacity cap) or economic (fee market pricing), and does the distinction matter for classification?',
    'Counterfactual analysis: if the cap were raised but demand kept fees high anyway, would low-value users return? Or is the cap the binding constraint? Protocol economics modeling and natural experiments from alternative chains with larger blocks.',
    'If suppression is primarily economic (demand-driven fees), the constraint''s active enforcement is lower than measured — the fee market, not the cap, does the excluding. If the cap is binding, suppression is structural and the 0.52 score is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_fee_market, conceptual, 'Structural vs. economic suppression mechanism for priced-out users').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 2009, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2009, 0.02).
narrative_ontology:measurement(bitc_tr_t2012, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2012, 0.03).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2017, 0.12).
narrative_ontology:measurement(bitc_tr_t2020, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(bitc_tr_t2025, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2009, 0.05).
narrative_ontology:measurement(bitc_be_t2012, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2012, 0.08).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2017, 0.35).
narrative_ontology:measurement(bitc_be_t2020, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(bitc_be_t2025, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2009, 0.05).
narrative_ontology:measurement(bitc_su_t2012, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2012, 0.08).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2017, 0.45).
narrative_ontology:measurement(bitc_su_t2020, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(bitc_su_t2025, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__store_of_value_reading, 0.1).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_scaling_constraint).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_block_size_cap_constraint).

% DUAL FORMULATION NOTE:
% Part of the bitcoin_whitepaper_purpose constraint family. This reading (store_of_value) and electronic_cash_reading share the kernel but instantiate different constraints with different ε, beneficiaries, and victims. They are linked via network.affects_constraints. The store-of-value reading's capacity constraint creates the demand that the Lightning reading serves; the electronic cash reading's failure to activate on-chain scaling created the structural conditions for Lightning's emergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__store_of_value_reading, organized, 0.15).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__store_of_value_reading, powerless, 0.9).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__store_of_value_reading, powerful, 0.45).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__store_of_value_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
