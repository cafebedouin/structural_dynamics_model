% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__electronic_cash_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: bitcoin_whitepaper_purpose__electronic_cash_reading
 *   human_readable: Bitcoin Whitepaper Electronic Cash Telos
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the electronic_cash_reading of the contested
 *   bitcoin_whitepaper_purpose kernel. It treats the whitepaper
 *   titleâ'Bitcoin: A Peer-to-Peer Electronic Cash System'âas a binding
 *   telos that mandates on-chain capacity expansion (8MB+ blocks), low
 *   per-transaction fees, and merchant payment adoption. The reading
 *   generates a structural conflict: payment processors and low-value
 *   transactors benefit from cheap on-chain settlement, while full node
 *   operators bear the uncompensated storage and bandwidth costs of a growing
 *   blockchain. Satoshi Nakamoto's disappearance in 2011 eliminated
 *   authoritative interpretation, leaving the whitepaper as contested
 *   substrate for competing protocol visions. This is one of three declared
 *   readings; the sibling store_of_value_reading subordinates on-chain
 *   capacity to decentralization and full-node verifiability.
 *
 * KEY AGENTS:
 *   - payment_processors: Primary beneficiary (organized/mobile) â capture volume from low-fee on-chain transactions
 *   - low_value_transactors: Secondary beneficiary (powerless/constrained) â depend on negligible fees for everyday use
 *   - full_node_operators: Primary target (moderate/constrained) â bear rising infrastructure costs from block expansion
 *   - big_block_advocates: Agenda setter (organized/mobile) â enforces the cash-telos interpretation through social and mining coordination
 *   - small_block_advocates: Observer (organized/mobile) â contests the reading and resists protocol changes that increase node burden
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.6).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Bitcoin Whitepaper Electronic Cash Telos").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, 'b373443e-421a-4b7e-bbed-6e89f371aa3d').
narrative_ontology:cs_kernel_codification('b373443e-421a-4b7e-bbed-6e89f371aa3d', fixed_text).
narrative_ontology:cs_authority_grounding('b373443e-421a-4b7e-bbed-6e89f371aa3d', distributed).
narrative_ontology:cs_reading_relation('b373443e-421a-4b7e-bbed-6e89f371aa3d', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_reading_relation('b373443e-421a-4b7e-bbed-6e89f371aa3d', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, coexists_with).
narrative_ontology:cs_axiom('b373443e-421a-4b7e-bbed-6e89f371aa3d', foundational, peer_to_peer_cash_paramount).
narrative_ontology:cs_axiom_status(peer_to_peer_cash_paramount, holdable).
narrative_ontology:cs_axiom_grounding('b373443e-421a-4b7e-bbed-6e89f371aa3d', peer_to_peer_cash_paramount, conventional).
narrative_ontology:cs_axiom('b373443e-421a-4b7e-bbed-6e89f371aa3d', foundational, on_chain_scaling_imperative).
narrative_ontology:cs_axiom_status(on_chain_scaling_imperative, holdable).
narrative_ontology:cs_axiom_grounding('b373443e-421a-4b7e-bbed-6e89f371aa3d', on_chain_scaling_imperative, instrumental).
narrative_ontology:cs_reference_frame('b373443e-421a-4b7e-bbed-6e89f371aa3d', peer_to_peer_electronic_cash).
narrative_ontology:cs_drift_state('b373443e-421a-4b7e-bbed-6e89f371aa3d', post_blocksize_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b373443e-421a-4b7e-bbed-6e89f371aa3d', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, full_node_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate merchant services, payment gateways, and point-of-sale integrations that depend on low on-chain transaction fees to make micropayments and retail settlement economically viable. They collect volume-based revenue from transaction throughput and would migrate to alternative chains or layer-2 networks if on-chain fees rose prohibitively.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors, beneficiary,
    organized, biographical, mobile, global).

% Individuals using Bitcoin for remittances, everyday purchases, or small-value transfers. Their participation depends on per-transaction fees remaining a negligible fraction of the payment amount. High fees push them toward custodial solutions, alternative cryptocurrencies, or traditional payment rails, but switching costs and liquidity constraints limit their exit.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, beneficiary,
    powerless, immediate, constrained, global).

% Independently validate and relay every transaction and block, bearing the storage, bandwidth, and compute costs of the blockchain. Expanded block capacity directly increases their hardware, bandwidth, and synchronization burdens, raising the cost of sovereign verification and pushing node operation toward datacenter-grade infrastructure. They cannot exit without abandoning their security model or leaving the network.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, full_node_operators, payer,
    moderate, biographical, constrained, global).

% Promote the whitepaper's title and use-case examples as binding protocol requirements, organize social and mining constituencies around on-chain scaling proposals, and advocate for consensus changes that expand block capacity. They can fork to alternative implementations or spin off separate networks if blocked from changing the base layer.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, big_block_advocates, agenda_setter,
    organized, generational, mobile, global).

% Contest the electronic cash reading's legitimacy, arguing that decentralization and full-node verifiability are the binding constraints. They advocate for layer-2 scaling and small blocks to preserve permissionless validation, and actively resist protocol changes that increase the node operator burden.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, small_block_advocates, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provide a peer-to-peer electronic cash system allowing online payments to be sent directly between parties without trusted financial intermediaries, by ensuring sufficient on-chain capacity for everyday transactions at low cost.
% TRANSFER_FUNCTION: Transfer the infrastructural burden of validating, storing, and transmitting large blocks from full node operators to the benefit of payment processors and low-value transactors who receive low-fee transactional utility.
% ABSENT_VOICES: Node operators in bandwidth- and storage-constrained regions, and small-block advocates who view on-chain scaling as a threat to decentralization, are marginalized in this reading's framework; their inclusion would challenge the assumption that node cost increases are acceptable externalities.
% DISAPPEARANCE_RATIONALE: Big-block advocates hold that abandoning the cash telos would dissolve Bitcoin's original value proposition and rearrange the ecosystem around settlement-only use, destroying merchant adoption and payment infrastructure. Small-block advocates hold that the network has already functionally evolved beyond on-chain cash, so removing this interpretive constraint would leave the world substantially unchanged.
% FOUNDING_PROBLEM: The double-spending problem for online payments without trusted third parties, requiring a peer-to-peer electronic cash system.
% FOUNDING_PROBLEM_CORROBORATION: The original whitepaper and cypherpunk literature corroborate the founding problem from outside the contemporary beneficiary set. Small-block advocates and academic distributed systems researchers corroborate that the problem has been solved without on-chain capacity expansion, attesting from seats that do not benefit from low-fee transactional throughput.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the asymmetric cost shift to node operators who subsidize network scaling through uncompensated hardware and bandwidth expenditure. Suppression (0.60) captures the active social and protocol-level enforcement required to maintain this reading against the dominant store-of-value narrative, including marginalization of dissent in governance forums. Theater ratio (0.50) registers the high performative component of the scaling warsâhash signaling, fork threats, and slogan-based governanceârelative to actual payment adoption. Accessibility collapse (0.40) is moderate because viable alternatives exist (altcoins, layer-2 networks, custodial solutions). Resistance (0.72) is high due to sustained and well-resourced opposition from the small-block faction. The temporal series trace the constraint's lifecycle: low extraction and theater in Bitcoin's early experimental phase, sharp escalation during the 2015â2017 blocksize war, and partial normalization afterward as layer-2 adoption absorbed some transactional demand.
 *
 * PERSPECTIVAL GAP:
 *   From the payment processor seat, the constraint is necessary infrastructure for a functional cash network; from the full node operator seat, the same constraint is an uncompensated cost imposition that threatens decentralization. The big_block_advocate and small_block_advocate seats experience the constraint as an ideological contest rather than an economic transfer, but their power and exit differences mean the former can credibly fork while the latter can only resist through soft-power obstruction.
 *
 * DIRECTIONALITY LOGIC:
 *   Payment processors and low-value transactors are structural beneficiaries: the constraint subsidizes their transaction costs and business models, pushing their directionality toward the beneficiary pole. Full node operators are structural victims: they pay for the network's scaling through real infrastructure costs without compensation, placing their directionality near the target pole. Big-block advocates, as agenda setters, have mobile exit (they can fork or spin off networks) and thus lower effective extraction despite their enforcement role. The engine will compute divergent seat types: beneficiaries likely seeing rope or tangled_rope, while node operators see snare-leaning tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâtrustless peer-to-peer electronic cashâwas genuine in 2008. Whether it remains live in 2026 is contested: the small-block camp argues it has been superseded by layer-2 and store-of-value use cases, which would make this reading a mandatrophic holdover. The big-block camp argues the problem is unsolved because high fees exclude everyday users. The R5 interview records this as contested, preventing automatic piton or scaffold classification. The temporal measurements show extraction peaked during the blocksize war and then slightly declined, suggesting the constraint's active enforcement phase may have passed, but the narrative persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    whitepaper_binding_force,
    'Is the whitepaper''s title and text a binding normative commitment for protocol evolution, or merely historical context that the operational network has superseded?',
    'Historical and linguistic analysis of the whitepaper''s intent structure, combined with observation of whether protocol governance treats the text as amendable precedent or immutable constitution.',
    'If the text is not binding, this reading collapses from a coordination claim into a contested political platform; the engine would re-evaluate the coordination_function as a cover story for rent-seeking by payment processors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whitepaper_binding_force, conceptual, 'Whether the whitepaper text constitutes a binding kernel or historical artifact.').

omega_variable(
    node_cost_extraction,
    'Does expanded on-chain block capacity impose uncompensated storage and bandwidth costs on full node operators sufficient to constitute asymmetric extraction?',
    'Empirical measurement of node operating costs under varying block size regimes, correlated with node count and geographic distribution of the node network.',
    'If costs are negligible or voluntarily borne, the victim classification weakens and the constraint edges toward rope; if costs are severe and concentrated, the asymmetric extraction gate for tangled_rope is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(node_cost_extraction, empirical, 'Whether node operator costs from large blocks constitute extraction.').

omega_variable(
    kernel_reading_sibling_relation,
    'This constraint is the electronic_cash_reading of kernel bitcoin_whitepaper_purpose. How would the store_of_value_reading restructure the beneficiary and victim sets, and are the two readings mutually exclusive at the consensus-rule level or merely complementary framings?',
    'Protocol analysis of whether a single chain can simultaneously satisfy both small-block decentralization requirements and large-block cash-throughput requirements without layer-2 subordination.',
    'If mutually exclusive, the coexists_with relation hardens into a zero-sum contest; if complementary, the constraint family may resolve into a scaffold or rope rather than a tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_relation, conceptual, 'Structural relationship between electronic cash and store of value readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 9, 0.7).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 15, 0.5).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 9, 0.68).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 3, 0.3).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 9, 0.8).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 15, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, global_infrastructure).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper_purpose kernel decomposes into at least three structurally distinct readings because the whitepaper text underdetermines protocol evolution after the founder's disappearance. The electronic_cash_reading and store_of_value_reading have different epsilon values, different beneficiary/victim structures, and different failure modes. They are not the same constraint viewed from two angles; they are competing instantiations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
