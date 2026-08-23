% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: bitcoin_whitepaper_purpose__electronic_cash_reading
 *   human_readable: Bitcoin Whitepaper 'Electronic Cash' Telos Binding Reading
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   The Bitcoin whitepaper titles the system 'A Peer-to-Peer Electronic Cash
 *   System.' The electronic_cash_reading treats this title as a binding
 *   telos: the protocol must support everyday transactional use with low
 *   fees, which requires expanded on-chain capacity (8MB+ blocks). This
 *   reading drove the Bitcoin Cash fork (2017) and continues to inform
 *   big-block advocacy. The constraint is the consensus rule limiting block
 *   size, which this reading demands be relaxed. Beneficiaries are payment
 *   processors, merchants, and low-value transactors who gain cheap on-chain
 *   transactions. Victims are node operators who bear increased storage,
 *   bandwidth, and validation costs. The store_of_value_reading prioritizes
 *   decentralization and full-node verifiability, treating the 1MB limit (and
 *   later SegWit) as protective. The nakamoto_oracle_opacity reading notes
 *   Satoshi's 2011 disappearance eliminated authoritative interpretation,
 *   leaving the whitepaper text as contested substrate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.45).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Bitcoin Whitepaper 'Electronic Cash' Telos Binding Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, '2e7f8411-b921-44e1-b96c-4fa21b66d792').
narrative_ontology:cs_kernel_codification('2e7f8411-b921-44e1-b96c-4fa21b66d792', fixed_text).
narrative_ontology:cs_authority_grounding('2e7f8411-b921-44e1-b96c-4fa21b66d792', lineage).
narrative_ontology:cs_interpretation_layer_present('2e7f8411-b921-44e1-b96c-4fa21b66d792').
narrative_ontology:cs_reading_relation('2e7f8411-b921-44e1-b96c-4fa21b66d792', bitcoin_whitepaper_purpose__store_of_value_reading, forecloses).
narrative_ontology:cs_reading_relation('2e7f8411-b921-44e1-b96c-4fa21b66d792', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, coexists_with).
narrative_ontology:cs_axiom('2e7f8411-b921-44e1-b96c-4fa21b66d792', foundational, whitepaper_cash_telos_binding).
narrative_ontology:cs_axiom_status(whitepaper_cash_telos_binding, holdable).
narrative_ontology:cs_axiom_grounding('2e7f8411-b921-44e1-b96c-4fa21b66d792', whitepaper_cash_telos_binding, conventional).
narrative_ontology:cs_axiom('2e7f8411-b921-44e1-b96c-4fa21b66d792', foundational, on_chain_scaling_required_for_cash).
narrative_ontology:cs_axiom_status(on_chain_scaling_required_for_cash, holdable).
narrative_ontology:cs_axiom_grounding('2e7f8411-b921-44e1-b96c-4fa21b66d792', on_chain_scaling_required_for_cash, instrumental).
narrative_ontology:cs_reference_frame('2e7f8411-b921-44e1-b96c-4fa21b66d792', satoshi_whitepaper_cash_vision).
narrative_ontology:cs_drift_state('2e7f8411-b921-44e1-b96c-4fa21b66d792', post_block_size_war, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2e7f8411-b921-44e1-b96c-4fa21b66d792', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, merchants).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, miners).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, miners).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__electronic_cash_reading, whitepaper_title_as_binding_commitment).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__electronic_cash_reading, on_chain_scaling_as_cash_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run full nodes validating all transactions and blocks. Bear storage, bandwidth, and compute costs proportional to block size. Receive no direct fee revenue. Larger blocks increase operational costs and may price out home operators, reducing decentralization. Exit options: switch to pruned/light client (reduced verification), stop running node, or fork to different chain. Cannot easily pass costs to users.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators, payer,
    organized, biographical, constrained, global).

% Build services (BitPay, Coinbase Commerce, BTCPay Server) enabling merchants to accept Bitcoin. Benefit from low on-chain fees and high throughput for payment volume. Can pass savings to merchants or capture margin. Exit options: support multiple chains, integrate Lightning, or pivot to other payment rails. Low switching cost relative to revenue dependence.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors, beneficiary,
    organized, biographical, mobile, global).

% Users making small everyday payments (coffee, remittances, micropayments). Benefit from fees low enough for transaction value. Currently priced out of on-chain use during congestion. Exit options: use custodial services, Lightning (with UX friction), or alternative chains. High dependence on fee level for basic utility.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, beneficiary,
    powerless, immediate, constrained, global).

% Accept Bitcoin for goods/services. Need low fees, fast confirmation, and reliable throughput. Benefit from on-chain scaling for direct payment acceptance without custodial intermediaries. Exit options: accept other cryptocurrencies, use payment processors that abstract chain choice, or stop accepting crypto. Moderate switching cost due to customer base.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, merchants, beneficiary,
    moderate, biographical, mobile, global).

% Maintain Bitcoin Core reference implementation. Decide which consensus changes to propose, review, and merge. Currently aligned with store_of_value_reading (SegWit, Taproot, no base-layer block size increase). Could change alignment but face strong social consensus pressure. Exit options: work on alternative implementations, fork, or leave project. High reputational and social capital investment.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, core_developers, agenda_setter,
    institutional, generational, analytical, global).

% Produce blocks and collect fees + subsidy. Larger blocks increase fee revenue but also validation cost, propagation delay, and orphan risk. Benefit from higher fee market if blocks are full; harmed if blocks are empty or orphaned. Exit options: mine other SHA-256 chains, switch pools, or sell hardware. Capital-intensive but mobile across compatible chains.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, miners, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__electronic_cash_reading, miners, beneficiary).

% Hold Bitcoin primarily as long-term savings vehicle. Prioritize censorship resistance, verifiability, and monetary hardness over payment throughput. Their preference for small blocks is structurally enforced by current consensus. Exit options: sell Bitcoin (identity rupture), accept layer-2 for payments, or advocate for protocol ossification. Identity fused with 'digital gold' narrative.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, store_of_value_advocates, excluded,
    organized, civilizational, identity_locked, global).

% Build layer-2 payment network on Bitcoin. Their success reduces pressure for on-chain scaling. They benefit from constrained base layer (creates demand for L2) but also need base layer capacity for channel operations. Analytical seat: they observe the constraint's effect on their design space without directly setting base-layer consensus.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, lightning_developers, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enable peer-to-peer electronic cash transactions for everyday commerce: low fees, fast confirmation, permissionless access, and merchant adoption at scale.
% TRANSFER_FUNCTION: Moves storage/bandwidth/validation costs from transactors (who pay subsidized fees) to node operators (who bear full marginal cost of larger blocks), mediated by the block size consensus rule.
% ABSENT_VOICES: Node operators in bandwidth-constrained regions (developing world, Tor-only operators) who would be priced out by 8MB+ blocks. Future users who need trust-minimized verification but cannot afford hardware for large-chain validation. Satoshi Nakamoto (the oracle) whose authoritative interpretation is permanently absent.
% DISAPPEARANCE_RATIONALE: If the block size limit vanished overnight, miners would produce larger blocks, fees would drop, payment processors and merchants would adopt on-chain payments, node operator costs would rise sharply, many home operators would drop off, decentralization metrics would degrade, and the store_of_value_reading constituency would likely fork to preserve small blocks — the ecosystem would reorganize around a new equilibrium.
% FOUNDING_PROBLEM: Create a peer-to-peer electronic cash system for everyday transactions without trusted intermediaries, as stated in the whitepaper title and abstract.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper text itself corroborates the 'electronic cash' framing. Early mailing list posts (2008-2010) show Satoshi discussing micropayments and high transaction volume. However, the store_of_value_reading is corroborated by Satoshi's 2010 writings on fee market necessity and 2011 handoff emphasizing 'core design' immutability. No external party (outside the Bitcoin community) adjudicates between these readings.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.58) reflects ongoing cost transfer from transactors to node operators via subsidized block space. Suppression (0.45) captures the consensus enforcement maintaining the block size limit against big-block demand. Theater ratio (0.32) acknowledges genuine payment coordination function but notes performative 'Satoshi's vision' rhetoric. Accessibility collapse (0.52) shows alternatives (Lightning, Liquid, BCH) exist but are incomplete substitutes for on-chain cash properties. Resistance (0.71) reflects sustained community and developer opposition to on-chain scaling. The 2017 spike in extractiveness and suppression maps to the block size war and BCH fork.
 *
 * PERSPECTIVAL GAP:
 *   From the node_operator seat, the constraint computes as snare (high extraction, no coordination benefit). From payment_processor seat, it computes as rope (genuine coordination, net benefit). From low_value_transactor seat, it computes as rope with low suppression. The agenda_setter (core developers) experiences it as tangled_rope: they must choose which reading to encode in consensus, knowing either choice extracts from one constituency. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Node operators are structural payers: they validate and propagate all transactions, bearing full marginal cost of larger blocks with no direct fee revenue. Payment processors and merchants are beneficiaries: they collect transaction volume at subsidized cost. Low-value transactors are beneficiaries: fees below economic cost of inclusion. Core developers (agenda_setters) hold institutional power over consensus rules but are split between readings. Miners have dual position: larger blocks increase fee revenue but also validation cost and orphan risk. The nakamoto_oracle_opacity reading creates an excluded seat: no authoritative interpreter exists to resolve the kernel.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (electronic cash for everyday use) is contested: the whitepaper states it, but 15 years of evolution produced a store-of-value dominant equilibrium. The electronic_cash_reading argues the founding problem is live and the current arrangement (small blocks) is mandatrophy — a constraint that persists despite its founding purpose being unmet. The store_of_value_reading argues the founding problem was mischaracterized: Bitcoin's true innovation is censorship-resistant digital scarcity, not payments. Neither side has external corroboration beyond textual interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    whitepaper_title_binding_vs_descriptive,
    'Does the whitepaper title ''Bitcoin: A Peer-to-Peer Electronic Cash System'' constitute a binding telos that governs protocol evolution, or is it a descriptive label for the system''s initial design?',
    'Textual analysis of whitepaper commitment language, Satoshi''s forum/posts commitments, and whether subsequent protocol changes (SegWit, Taproot) were evaluated against the title as constraint.',
    'If binding, the electronic_cash_reading''s claim to authority is strengthened and store_of_value_reading is a deviation; if descriptive, both readings are post-hoc interpretations of an underspecified kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whitepaper_title_binding_vs_descriptive, conceptual, 'Whether the whitepaper title functions as constitutional commitment or descriptive label.').

omega_variable(
    on_chain_scaling_necessity_for_cash,
    'Is expanded on-chain capacity (larger blocks) structurally necessary to achieve ''electronic cash'' properties, or can layer-2 solutions (Lightning) satisfy the cash telos while preserving node verifiability?',
    'Empirical observation of Lightning adoption, merchant acceptance, user experience, and whether off-chain systems replicate cash-like properties (finality, permissionlessness, offline capability).',
    'If layer-2 suffices, the extraction from node operators via big blocks is unnecessary coordination cost; if layer-2 fails cash properties, big-block extraction is the price of the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(on_chain_scaling_necessity_for_cash, empirical, 'Whether the coordination function requires on-chain extraction or can be achieved off-chain.').

omega_variable(
    node_operator_cost_as_extraction_vs_coordination,
    'Are the storage/bandwidth costs imposed on node operators by larger blocks extractive transfer to transactors, or the inherent coordination cost of a global payment system?',
    'Compare node operational costs under 1MB vs 8MB+ blocks against transaction fee revenue, and assess whether node operators receive compensating benefits (network value, miner revenue share).',
    'If coordination cost, the constraint is more rope-like; if extractive transfer, more snare-like. Affects χ computation for node_operator seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(node_operator_cost_as_extraction_vs_coordination, empirical, 'Whether node operator burden is necessary coordination overhead or asymmetric extraction.').

omega_variable(
    kernel_reading_foreclosure,
    'Does committing to the electronic_cash_reading logically foreclose the store_of_value_reading within a single protocol, or can both be maintained as layer-separated commitments?',
    'Analyze whether a single consensus layer can simultaneously optimize for high-throughput low-fee payments AND maximal decentralization/verifiability, or whether the trade-off is fundamental.',
    'If foreclosure, the kernel admits no stable synthesis; if coexistence, the readings map to different layers (base vs L2) and the contest is about layer assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the two readings are mutually exclusive at the consensus layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_wp_cash_tr_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(btc_wp_cash_tr_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2012, 0.08).
narrative_ontology:measurement(btc_wp_cash_tr_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(btc_wp_cash_tr_t2017, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2017, 0.35).
narrative_ontology:measurement(btc_wp_cash_tr_t2020, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(btc_wp_cash_tr_t2024, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(btc_wp_cash_be_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2009, 0.15).
narrative_ontology:measurement(btc_wp_cash_be_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2012, 0.18).
narrative_ontology:measurement(btc_wp_cash_be_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement(btc_wp_cash_be_t2017, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2017, 0.52).
narrative_ontology:measurement(btc_wp_cash_be_t2020, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(btc_wp_cash_be_t2024, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(btc_wp_cash_su_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2009, 0.1).
narrative_ontology:measurement(btc_wp_cash_su_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2012, 0.12).
narrative_ontology:measurement(btc_wp_cash_su_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(btc_wp_cash_su_t2017, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2017, 0.55).
narrative_ontology:measurement(btc_wp_cash_su_t2020, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement(btc_wp_cash_su_t2024, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_consensus_block_size_limit).

% DUAL FORMULATION NOTE:
% This constraint and store_of_value_reading form a kernel family from the bitcoin_whitepaper_purpose kernel. The electronic_cash_reading prioritizes the whitepaper title as binding telos requiring on-chain capacity expansion (extracting from node operators). The store_of_value_reading prioritizes decentralization/verifiability as binding constraints requiring on-chain capacity restraint (extracting from transactors via fees). They share the same kernel text but instantiate different constraints with different ε, beneficiaries, and victims. The nakamoto_oracle_opacity reading is a meta-constraint on the interpretive field itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__electronic_cash_reading, organized, 0.25).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__electronic_cash_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
