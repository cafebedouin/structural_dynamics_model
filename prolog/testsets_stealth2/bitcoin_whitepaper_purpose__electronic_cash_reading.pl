% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: bitcoin_whitepaper_purpose__electronic_cash_reading
 *   human_readable: Bitcoin Whitepaper 'Electronic Cash' Telos Binding (Capacity-First Reading)
 *   domain: economic/technological/governance
 *
 * SUMMARY:
 *   The whitepaper's title - 'Bitcoin: A Peer-to-Peer Electronic Cash System'
 *   - is read by this constraint's holders as a binding statement of purpose:
 *   the system exists to carry everyday electronic payments at negligible
 *   cost, and protocol development must subordinate other considerations to
 *   that telos. Instantiated as a governance constraint, the reading mandates
 *   expanded on-chain capacity (8MB-class blocks and beyond), prioritizes
 *   merchant payment adoption, and holds per-transaction fees near zero. Its
 *   operation coordinates a genuine public good - trustless retail-scale
 *   digital payment - while directing the compounding costs of an
 *   ever-growing ledger onto the people who validate it. Per the
 *   epsilon-invariance principle, this story decomposes the colloquial label
 *   'Bitcoin's purpose' into one of at least two structurally distinct
 *   constraints: the sibling store_of_value_reading assigns the opposite
 *   beneficiary/victim structure (node operators protected as the core
 *   constituency, transactors bearing market-clearing fees), and the two
 *   readings' epsilon values differ because their victim sets are disjoint.
 *   The epsilon referent here is the cash-reading arrangement itself - the
 *   capacity-first, low-fee regime this reading installs and defends -
 *   assessed as it actually operates, not as its advocates describe it. KEY
 *   AGENTS (by structural relationship): - low_value_transactors: primary
 *   beneficiary (powerless/mobile) - receives the low-fee payment service;
 *   diffuse and individually voiceless - merchant_payment_acquirers:
 *   secondary beneficiary (organized/arbitrage) - monetizes cheap base-layer
 *   rails - online_merchants: secondary beneficiary (moderate/mobile) -
 *   full_node_operators: primary target (moderate/constrained) - bears
 *   compounding validation costs - emerging_market_node_operators: secondary
 *   target (powerless/trapped) - bears disproportionate connectivity costs -
 *   cash_protocol_developers: agenda setter (institutional/mobile) -
 *   administers capacity policy - mining_pools: dual-positioned
 *   (powerful/arbitrage) - collects fee volume, bears node costs -
 *   layer_two_scaling_developers: excluded voice (organized/constrained) -
 *   monetary_economists: analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.6).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.48).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Bitcoin Whitepaper 'Electronic Cash' Telos Binding (Capacity-First Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "economic/technological/governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, '8030d3a6-fb20-4cf8-9b90-01793531e85a').
narrative_ontology:cs_kernel_codification('8030d3a6-fb20-4cf8-9b90-01793531e85a', fixed_text).
narrative_ontology:cs_authority_grounding('8030d3a6-fb20-4cf8-9b90-01793531e85a', distributed).
narrative_ontology:cs_reading_relation('8030d3a6-fb20-4cf8-9b90-01793531e85a', bitcoin_whitepaper_purpose__store_of_value_reading, forecloses).
narrative_ontology:cs_reading_relation('8030d3a6-fb20-4cf8-9b90-01793531e85a', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, coexists_with).
narrative_ontology:cs_axiom('8030d3a6-fb20-4cf8-9b90-01793531e85a', foundational, title_cash_designation_binding).
narrative_ontology:cs_axiom_status(title_cash_designation_binding, holdable).
narrative_ontology:cs_axiom_grounding('8030d3a6-fb20-4cf8-9b90-01793531e85a', title_cash_designation_binding, conventional).
narrative_ontology:cs_axiom('8030d3a6-fb20-4cf8-9b90-01793531e85a', foundational, everyday_transactional_usability_required).
narrative_ontology:cs_axiom_status(everyday_transactional_usability_required, holdable).
narrative_ontology:cs_axiom_grounding('8030d3a6-fb20-4cf8-9b90-01793531e85a', everyday_transactional_usability_required, instrumental).
narrative_ontology:cs_reference_frame('8030d3a6-fb20-4cf8-9b90-01793531e85a', peer_to_peer_electronic_cash_specification).
narrative_ontology:cs_drift_state('8030d3a6-fb20-4cf8-9b90-01793531e85a', post_block_size_war_contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8030d3a6-fb20-4cf8-9b90-01793531e85a', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_payment_acquirers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, online_merchants).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, emerging_market_node_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Make small everyday payments and remittances and need the per-transaction cost to stay near zero for the payment to make sense at all. They receive the low-fee service the capacity policy maintains. Switching to another chain, a custodial app, or card rails is easy for them, but they have no individual voice in protocol governance; their leverage is only their collective presence as a user base.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, beneficiary,
    powerless, immediate, mobile, global).

% Build payment products on top of the chain for online merchants, converting between cryptocurrency and national currencies and taking a percentage margin on volume. Cheap, reliable base-layer transactions are the input their business runs on, so they lobby for larger blocks and faster confirmation. They operate across several chains at once and can shift product lines if one network's fees become unusable.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_payment_acquirers, beneficiary,
    organized, biographical, arbitrage, global).

% Accept cryptocurrency at checkout as one option among several. Low fees and quick confirmation determine whether accepting it is worth the integration effort; sustained high fees lead them to drop the option. They can leave for card processors or other chains without heavy sunk technical cost.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, online_merchants, beneficiary,
    moderate, immediate, mobile, global).

% Run independently validating nodes, keeping full copies of the ledger and checking every block. Larger blocks multiply their storage, bandwidth, and archival costs year over year. Their reason for running a node at all is verification independence - trusting no third party - so stopping means giving up the thing they valued, and delegating to someone else's node defeats the purpose. Leaving is costly in exactly the currency they care about.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, full_node_operators, payer,
    moderate, generational, constrained, global).

% Run validating nodes from regions with expensive or metered connectivity and limited cheap storage. Each block-size increase raises their operating costs proportionally more than for operators on cheap fiber, and the local substitute - colocation abroad - reintroduces the reliance on foreign infrastructure they sought to avoid. Many simply stop validating and depend on nodes run elsewhere.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, emerging_market_node_operators, payer,
    powerless, biographical, trapped, regional).

% Maintain the client software that implements the capacity policy - block-size defaults, propagation behavior, hard-fork scheduling. They administer the arrangement this story describes, funded by donations and periodic development-fund proposals. Their standing depends on the cash framing remaining the governing interpretation, and they can and do fork away when outvoted.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, cash_protocol_developers, agenda_setter,
    institutional, generational, mobile, global).

% Aggregate hashpower and collect block subsidies plus per-transaction fees. Bigger blocks let them pack more transactions per block, partially offsetting low per-transaction fees with volume, but they also operate their own nodes and face the same cost growth, and they worry that a permanently thin fee market weakens long-term security funding. Their hashpower moves freely between chains, which is their main lever.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools, beneficiary,
    powerful, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools, payer).

% Build off-chain scaling systems - payment channels, statechains - premised on a small, expensive base layer with high-assurance settlement. Under the cash-first reading their lane is deprioritized: capacity goes to the base layer instead. They argue their architecture delivers cash-like payments sustainably, but they hold no agenda control in this reading's governance venues and were largely absent from its founding decisions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, layer_two_scaling_developers, excluded,
    organized, generational, constrained, global).

% Study the fee market, transaction velocity, and the security budget as the block subsidy declines. They publish analyses used by every other seat but command no enforcement power and collect nothing from the arrangement.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, monetary_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__electronic_cash_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__electronic_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single append-only ledger that lets strangers exchange value electronically without a trusted intermediary, solving double-spending by proof-of-work ordering; the cash reading directs that ledger to price everyday retail-scale payments into the base layer itself.
% TRANSFER_FUNCTION: Moves purchasing power between transacting parties across the network at near-zero fee; moves the compounding cost of storing and validating the ever-growing ledger onto node operators; moves block rewards and whatever fee revenue exists to miners.
% ABSENT_VOICES: Prospective node operators in low-bandwidth regions who are priced out before they ever participate; users of the dominant chain who lived through fee spikes with no vote in capacity policy; and layer-two scaling developers, whose alternative architecture was deprioritized rather than debated on its merits in this reading's governance venues.
% DISAPPEARANCE_RATIONALE: If the cash-telos binding vanished overnight, capacity policy would default to the settlement-first configuration: fees would float to market-clearing levels, everyday transactions would migrate to custodial and layer-two rails, node operating costs would stabilize, and the network's user base would reorganize around holders and large settlements. The payment-processor and retail-transactor constituencies would dissolve or move to other chains.
% FOUNDING_PROBLEM: Commerce on the internet depended on trusted financial institutions to process electronic payments, exposing users to transaction costs, fraud mediation, and arbitrary exclusion. The whitepaper proposed a peer-to-peer electronic cash that settles directly between parties, with cryptographic proof replacing the trusted intermediary.
% FOUNDING_PROBLEM_CORROBORATION: Attestation from outside the benefiting parties: the academic double-spending literature predating the whitepaper treats trustless electronic payment as an open problem; central-bank digital-currency research programs treat retail-grade digital cash as unsolved; merchants that discontinued on-chain crypto checkout attest the problem persists while the dominant chain's current arrangement fails it. Store-of-value advocates dispute the problem's liveness - they hold the cash function was always secondary - so the status is live but actively disputed rather than unanimously attested.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is substantial (0.60 at interval end) because the low-fee mandate is funded by an uncompensated transfer: node operators bear storage, bandwidth, and archival costs that grow with every capacity increase, while the benefit spreads as consumer surplus across millions of transactors. Suppression is moderate (0.48) and structural rather than internalized: the constraint holds by fork politics, client-software defaults, and during the 2015-2017 war by venue control and social pressure, not by any cognitive lock on its targets - node operators who object retain full analytical clarity about their position, which is why resistance is high (0.75). Accessibility collapse is low-moderate (0.40): alternatives persist everywhere (other chains, layer-two rails, custodial services, fiat), so understanding the constraint does not close the option set. Theater is moderate (0.33): the payment-processing function is real and continuous, but a visible share of activity is rhetorical maintenance - whitepaper-quoting contests, 'Satoshi's vision' invocations, anniversary branding - peaking during the war years. The claim and the metrics are independent authored facts: I claim tangled_rope because the structure shows a genuine coordination function (retail digital cash without intermediaries) fused with an asymmetric cost-bearing class (validators) under active enforcement; the engine computes per-seat types from the structural data. Temporal series run on one shared grid (T0=2009 genesis, T3~2012 early growth, T6~2015 war onset, T9~2018 post-fork 8MB-to-32MB era, T12~2021, T15~2024, T18~2027 projected); every tracked metric is authored at every point, and the final point carries projected basis.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (cash_protocol_developers), the arrangement is the project's defining purpose, faithfully implemented; from the transactor seat it is simple utility - nearly free payments; from the full-node-operator seat it is a compounding uncompensated burden that grows precisely because the system succeeds; from the miner seat it is ambivalent (volume offsets thin fees, but the security budget worries). Same protocol, same text, four different constraints experienced. The payer seats are diffuse and ideologically committed - node operators run nodes because verification independence is their identity, which blunts the coalition power their control of validation infrastructure would otherwise confer. Emerging-market operators are the clearest losers: they bear the steepest proportional costs and have the least exit, yet their absence from governance venues is structural, not chosen.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (low_value_transactors, merchant_payment_acquirers, online_merchants) drive those seats toward the beneficiary end of directionality; victim declarations (full_node_operators, emerging_market_node_operators) drive them toward the target end, amplified by constrained or trapped exit - a node operator who exits loses the very good the constraint promises, and an emerging-market operator has no domestic substitute. One override is authored: the powerful atom maps to d=0.35 because the only powerful seat, mining_pools, is genuinely dual-positioned. Derivation from its beneficiary declaration alone would place it near the full-beneficiary end, but it operates its own nodes, absorbs the same cost growth, and carries security-budget exposure; netting those against fee-volume gains puts it at mild beneficiary, not full beneficiary. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, and the global spatial scope of the constraint modestly amplifies effective extraction on the payer seats by making coordinated node-operator response harder to verify and organize.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - trusted intermediaries taxing and gating electronic commerce - is live, corroborated by pre-Bitcoin double-spending literature and by central-bank digital-currency programs, so no mandatrophy is declared and none should be inferred. The classification guards against two mislabels. Against rope: the victim set is real and identifiable, the cost asymmetry is structural, and enforcement is active, so pure-coordination framing would erase the validators' burden. Against snare: no seat captures the extraction - gain_flow is authored as an affirmatively checked 'diffuse', the largest recipients are the dispersed transacting public, processor margins are competitively compressed, miners net-lose under thin fees, and developers collect no rents - and the coordination function dominates operationally. One honest tension is flagged rather than smoothed: the receipt surface combination (diffuse gains, prohibitive fixing cost) pattern-matches the piton cell, but the piton test fails on the substance - the function is live (continuous payment volume), enforcement is functional rather than theatrical, theater_ratio sits well below atrophy levels, and the founding problem is corroborated as live from outside the beneficiary set. The diffuse/prohibitive combination here reflects a genuine public-good subsidy structure defended by a committed constituency, not inertial performance of a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which element of the whitepaper text is binding - the title''s ''electronic cash'' designation, or the decentralization and verification properties emphasized in the body?',
    'No living authority can adjudicate (see authoritative_interpreter_absence); resolution would require recovered authorial intent or a demonstrated protocol synthesis satisfying both teloi simultaneously.',
    'This story instantiates the electronic_cash_reading of kernel bitcoin_whitepaper_purpose; under the sibling store_of_value_reading the same protocol history computes as a different constraint with inverted beneficiary/victim sets - node operators protected as the constituency, transactors bearing market-clearing fees - and a correspondingly different epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: this constraint is one reading of the bitcoin_whitepaper_purpose kernel; the sibling reading would reassign who benefits and who pays.').

omega_variable(
    hardware_cost_trajectory,
    'Does consumer storage and bandwidth cost decline fast enough that 8MB-class and larger blocks remain verifiable by ordinary participants rather than only by well-resourced operators?',
    'Decade-scale comparison of consumer hardware cost curves against realized block growth and chain-size trajectories, stratified by region to catch connectivity-cost divergence.',
    'If hardware loses the race, node-operator cost-bearing compounds, validation concentrates, and the constraint drifts toward pure extraction with a centralized validation tier; if hardware wins, the measured extraction damps toward ordinary coordination cost and the rope component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_cost_trajectory, empirical, 'Whether the cash reading''s capacity bet is rescued or defeated by hardware economics.').

omega_variable(
    fee_subsidy_security_coupling,
    'Can a permanently low-fee base layer fund the proof-of-work security its cash function requires as block subsidies continue halving?',
    'Fee-revenue and hash-rate time series against the subsidy schedule, with stress models of post-halving security budgets under sustained low-fee regimes.',
    'If security funding fails, the constraint undermines its own coordination function and the tangled-rope balance collapses toward decay; if transaction volume compensates for thin per-unit fees, the low-fee regime is internally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_subsidy_security_coupling, empirical, 'Whether the low-fee mandate is self-undermining through the security budget.').

omega_variable(
    authoritative_interpreter_absence,
    'With the author gone since 2011, is any reading''s claim to textual fidelity falsifiable against its rivals?',
    'None available in principle short of new authorial evidence; the contest resolves only through adoption outcomes and chain survival, never through interpretation.',
    'All readings of this kernel persist indefinitely as live positions; per-seat classifications remain computable from structural operation, but kernel-level vindication is undecidable, so classification authority rests entirely on how each reading''s arrangement actually behaves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authoritative_interpreter_absence, conceptual, 'The oracle-opacity condition: no authoritative interpreter exists to settle which reading the text mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 3, 0.08).
narrative_ontology:measurement_basis(bitc_tr_t3, observed).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement_basis(bitc_tr_t6, observed).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 9, 0.3).
narrative_ontology:measurement_basis(bitc_tr_t9, observed).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(bitc_tr_t12, observed).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(bitc_tr_t15, observed).
narrative_ontology:measurement(bitc_tr_t18, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement_basis(bitc_tr_t18, projected).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 3, 0.18).
narrative_ontology:measurement_basis(bitc_be_t3, observed).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(bitc_be_t6, observed).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 9, 0.58).
narrative_ontology:measurement_basis(bitc_be_t9, observed).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(bitc_be_t12, observed).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(bitc_be_t15, observed).
narrative_ontology:measurement(bitc_be_t18, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement_basis(bitc_be_t18, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(bitcoin_whitepaper_purpose__electronic_cash_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Bitcoin's purpose'. The label conflates at least two structurally distinct claims with disjoint victim sets and therefore different epsilon values: this story (electronic_cash_reading - capacity-first, beneficiaries are transactors and payment processors, victims are node operators) and store_of_value_reading (verifiability-first, beneficiaries are node operators and self-verifying holders, victims are priced-out low-value transactors). The whitepaper text is the common upstream substrate; each reading cites it as warrant for its policy program, so the upstream text influences both downstream readings while the two readings foreclose each other within any single governance framework. nakamoto_oracle_opacity is recorded as the ambient interpretive condition enabling the family's persistence. Linkage via affects_constraints follows the epsilon-invariance authoring rule: one claim, one story, one stable epsilon, linked edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__electronic_cash_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
