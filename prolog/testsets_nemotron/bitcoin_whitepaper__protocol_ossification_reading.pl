% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__protocol_ossification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__protocol_ossification_reading, []).

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
 *   constraint_id: bitcoin_whitepaper__protocol_ossification_reading
 *   human_readable: Bitcoin Protocol Ossification Consensus Rule
 *   domain: technological/governance/economic
 *
 * SUMMARY:
 *   This constraint story captures the 'protocol ossification' reading of the
 *   Bitcoin whitepaper kernel: the claim that Bitcoin protocol changes
 *   require near-universal consensus, making the base protocol effectively
 *   immutable. This reading emerged after the 2017 block size wars as a
 *   coordination mechanism to prevent chain splits, but has evolved into a
 *   barrier that blocks legitimate protocol improvements (CTV, APO,
 *   drivechains, vault opcodes). The constraint coordinates around stability
 *   but extracts from use cases requiring base-layer evolution — a classic
 *   tangled rope. The victim set includes p2p cash advocates, L2 innovators
 *   needing new opcodes, base-layer application developers, and global south
 *   users priced out by fee markets.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper__protocol_ossification_reading, 0.72).
domain_priors:theater_ratio(bitcoin_whitepaper__protocol_ossification_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__protocol_ossification_reading, "Bitcoin Protocol Ossification Consensus Rule").
narrative_ontology:topic_domain(bitcoin_whitepaper__protocol_ossification_reading, "technological/governance/economic").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__protocol_ossification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__protocol_ossification_reading, 'cf463bee-2c75-425a-a23d-40f2df7e70ba').
narrative_ontology:cs_kernel_codification('cf463bee-2c75-425a-a23d-40f2df7e70ba', fixed_text).
narrative_ontology:cs_authority_grounding('cf463bee-2c75-425a-a23d-40f2df7e70ba', lineage).
narrative_ontology:cs_interpretation_layer_present('cf463bee-2c75-425a-a23d-40f2df7e70ba').
narrative_ontology:cs_reading_relation('cf463bee-2c75-425a-a23d-40f2df7e70ba', bitcoin_whitepaper__digital_gold_reading, influences).
narrative_ontology:cs_reading_relation('cf463bee-2c75-425a-a23d-40f2df7e70ba', bitcoin_whitepaper__p2p_cash_reading, forecloses).
narrative_ontology:cs_axiom('cf463bee-2c75-425a-a23d-40f2df7e70ba', foundational, protocol_changes_require_universal_consensus).
narrative_ontology:cs_axiom_status(protocol_changes_require_universal_consensus, holdable).
narrative_ontology:cs_axiom_grounding('cf463bee-2c75-425a-a23d-40f2df7e70ba', protocol_changes_require_universal_consensus, conventional).
narrative_ontology:cs_axiom('cf463bee-2c75-425a-a23d-40f2df7e70ba', foundational, stability_is_primary_monetary_virtue).
narrative_ontology:cs_axiom_status(stability_is_primary_monetary_virtue, holdable).
narrative_ontology:cs_axiom_grounding('cf463bee-2c75-425a-a23d-40f2df7e70ba', stability_is_primary_monetary_virtue, deontological).
narrative_ontology:cs_reference_frame('cf463bee-2c75-425a-a23d-40f2df7e70ba', satoshi_consensus_model).
narrative_ontology:cs_drift_state('cf463bee-2c75-425a-a23d-40f2df7e70ba', post_taproot_activation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cf463bee-2c75-425a-a23d-40f2df7e70ba', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, institutional_custodians).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, protocol_conservatives).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, p2p_cash_advocates).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, lightning_innovators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, base_layer_application_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, global_south_users_needing_cheap_transactions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_operators).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, stability_as_monetary_virtue).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, credible_neutrality_via_ossification).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, layered_innovation_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold BTC primarily as a store of value. Benefit from protocol stability that protects the monetary properties they invested in. Bear opportunity cost when useful features are blocked, but have liquid exit via exchanges.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders, payer).

% Provide regulated custody and financial products on Bitcoin. Require immutable protocol rules for compliance, auditing, and legal certainty. Capture fees from holding and wrapping BTC; protocol changes threaten product architecture.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, institutional_custodians, beneficiary,
    institutional, biographical, arbitrage, global).

% Core developers, Bitcoin OGs, and ideologically aligned node operators who define and enforce the 'universal consensus' standard. Their professional identity and social capital are fused to the ossification narrative. Exit means abandoning the intellectual framework they built.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, protocol_conservatives, agenda_setter,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, protocol_conservatives, beneficiary).

% Coordinate hash power and activate soft forks. Benefit from predictable protocol rules that stabilize mining economics. Bear costs of coordination and risk orphan blocks when consensus thresholds are ambiguous. Cannot easily exit without surrendering hash power market position.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_operators, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_operators, payer).

% Advocate for on-chain scaling and low-fee transactions. Pay through blocked BIPs, stalled fee markets, and exclusion from governance. Exit to BCH/BCSV or Lightning is possible but fragments network effects and abandons the 'Bitcoin' brand.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, p2p_cash_advocates, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, p2p_cash_advocates, excluded).

% Build L2 protocols requiring base-layer opcodes (e.g., SIGHASH_ANYPREVOUT, OP_CTV). Pay through delayed deployments, complex workarounds, and uncertainty. Exit to alt-L1s sacrifices Bitcoin's security model and liquidity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, lightning_innovators, payer,
    moderate, biographical, constrained, global).

% Build applications directly on Bitcoin script (e.g., DLCs, vaults, timestamps). Pay through a frozen opcode set and no pathway for new primitives. Cannot exit to L2 without redesign; cannot exit to other chains without losing Bitcoin's trust-minimized guarantees.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, base_layer_application_developers, payer,
    powerless, biographical, trapped, global).

% Rely on Bitcoin for censorship-resistant remittances and savings in unstable currency regimes. Pay through high on-chain fees that price out small-value use cases. No voice in consensus process; exit to stablecoins or custodial solutions reintroduces counterparty risk they sought to escape.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, global_south_users_needing_cheap_transactions, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, global_south_users_needing_cheap_transactions, excluded).

% Study consensus dynamics, incentive compatibility, and protocol evolution. Observe the constraint from outside the capture/payment relationship; no direct stake in protocol outcomes.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, protocol_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the 'who decides protocol rules' problem by establishing universal consensus as the legitimacy threshold, preventing contentious hard forks that could split the network and destroy monetary properties.
% TRANSFER_FUNCTION: Transfers option value from use cases requiring base-layer changes (p2p cash, complex scripting, L2-enabling opcodes) to holders of the status quo (long-term holders, institutions, conservative developers) by making protocol changes practically impossible.
% ABSENT_VOICES: End users in high-inflation economies who need cheap, censorship-resistant transactions but have no representation in the developer/ miner/ holder governance triad. Future users whose use cases don't exist yet but will require base-layer primitives. Competing protocol designers whose innovations are blocked by the consensus barrier.
% DISAPPEARANCE_RATIONALE: If the universal-consensus requirement vanished, multiple contested soft forks would activate within months (CTV, APO, drivechains), the protocol would fragment into competing rule sets, and Bitcoin's 'credible neutrality' narrative would collapse — though some argue this would enable needed innovation.
% FOUNDING_PROBLEM: Bitcoin's early history showed that protocol changes could split the network (e.g., the 2013 BerkeleyDB fork, the 2017 block size wars). The ossification norm emerged to prevent chain splits that would destroy the 'digital gold' monetary premium.
% FOUNDING_PROBLEM_CORROBORATION: Protocol conservatives and long-term holders attest the chain-split risk remains live. P2P cash advocates and L2 developers attest the founding problem (chaotic hard forks) was solved by SegWit's activation mechanism and the social consensus against contentious forks — the ossification norm now blocks legitimate improvements. Independent blockchain governance researchers (e.g., Zamyatin et al., 'Sok: Communication Abstractions for Blockchain Governance') corroborate that Bitcoin's governance has ossified beyond the chain-split prevention rationale.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__protocol_ossification_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__protocol_ossification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__protocol_ossification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(bitcoin_whitepaper__protocol_ossification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__protocol_ossification_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__protocol_ossification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper__protocol_ossification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the accumulating opportunity cost of blocked improvements: fee revenue that could be lower, L2 designs that could be simpler, use cases that could exist. Suppression (0.72) is high because the consensus threshold is actively enforced through social pressure, GitHub gatekeeping, and the threat of chain split stigma — not merely passive inertia. Theater (0.42) captures the gap between 'we're protecting stability' rhetoric and the reality that many blocked changes (CTV, APO) have broad technical consensus but are stalled by process. Accessibility collapse (0.78) is high because the 'universal consensus' standard is structurally unfalsifiable — no proposal can prove it meets the threshold. Resistance (0.35) is moderate: opponents organize (UASF movements, alternative clients) but lack the hash power or institutional leverage to overcome the consensus barrier.
 *
 * PERSPECTIVAL GAP:
 *   From the protocol conservative seat, this is a rope: genuine coordination preventing chain splits. From the p2p cash advocate seat, it's a snare: the coordination story is cover for protecting the store-of-value premium. From the L2 innovator seat, it's a tangled rope: they accept the coordination value but pay asymmetric extraction. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) reflects the author's structural judgment, not any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Protocol conservatives and institutional custodians are structural beneficiaries (d ~ 0.15-0.25): they capture the stability premium and face minimal extraction. Long-term holders are near-symmetric beneficiaries (d ~ 0.35): they gain monetary premium but lose option value. Miners are constrained beneficiaries (d ~ 0.4): they benefit from predictability but bear coordination costs. P2P cash advocates, L2 innovators, and base-layer developers are targets (d ~ 0.75-0.85): they pay the full extraction cost with constrained or trapped exit. Global south users are identity-locked targets (d ~ 0.9): they cannot exit the monetary need but are priced out of the tool.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing chaotic hard forks) was largely solved by 2017's SegWit activation and the social norm against contentious forks. The constraint persists because the 'universal consensus' standard is unfalsifiable and benefits the now-dominant store-of-value coalition. Mandatrophy is unresolved: the constraint's coordination function has atrophied relative to its extraction function, but the beneficiary coalition (holders, institutions, conservatives) has no incentive to declare victory and relax the threshold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_threshold_falsifiability,
    'Is the ''universal consensus'' standard operationally falsifiable, or does it function as an unfalsifiable veto that can be raised against any proposal?',
    'Track historical proposals: which achieved ''universal consensus'' by the standard''s own advocates, and which were rejected despite broad technical agreement? Code the rejection reasons.',
    'If unfalsifiable, the constraint is a snare disguised as coordination — the threshold exists to block, not to coordinate. If falsifiable, some blocked proposals genuinely lacked consensus, and the extraction is the price of a real coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_threshold_falsifiability, conceptual, 'Whether the consensus threshold is a genuine coordination mechanism or an unfalsifiable veto.').

omega_variable(
    layered_innovation_substitutability,
    'Can all valuable protocol innovations be built on higher layers (Lightning, RGB, Ark, BitVM) without base-layer changes, or are there fundamental primitives that require base-layer opcodes?',
    'Cryptographic and game-theoretic analysis: prove whether covenants, vaults, trust-minimized bridges, and fee sponsorship require base-layer changes or can be emulated with current script.',
    'If higher layers are fully substitutable, extraction is lower — victims can exit to L2. If base-layer primitives are irreducibly necessary, extraction is higher and victims are trapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(layered_innovation_substitutability, empirical, 'Whether base-layer ossification forces extraction or victims have viable L2 substitutes.').

omega_variable(
    kernel_framing_under_determination,
    'Does the ''protocol_ossification_reading'' represent a distinct structural constraint from the ''digital_gold_reading'', or is it merely the governance mechanism that implements the digital gold property?',
    'Analyze whether the ossification norm would persist if the store-of-value premium vanished. If the norm is instrumentally justified by digital gold, it is not an independent constraint. If it has independent ideological commitment, it is a distinct constraint.',
    'If instrumental, the two readings collapse into one constraint family with a single ε. If independent, they are separate constraints with different beneficiary/victim structures — the ossification norm may persist even if digital gold demand falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether protocol ossification is an independent constraint or the enforcement arm of digital gold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__protocol_ossification_reading, 2017, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bip_ossif_tr_t2017, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2017, 0.25).
narrative_ontology:measurement(bip_ossif_tr_t2019, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(bip_ossif_tr_t2021, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2021, 0.35).
narrative_ontology:measurement(bip_ossif_tr_t2023, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2023, 0.38).
narrative_ontology:measurement(bip_ossif_tr_t2025, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(bip_ossif_be_t2017, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2017, 0.45).
narrative_ontology:measurement(bip_ossif_be_t2019, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2019, 0.52).
narrative_ontology:measurement(bip_ossif_be_t2021, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(bip_ossif_be_t2023, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2023, 0.63).
narrative_ontology:measurement(bip_ossif_be_t2025, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bip_ossif_su_t2017, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2017, 0.55).
narrative_ontology:measurement(bip_ossif_su_t2019, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(bip_ossif_su_t2021, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2021, 0.65).
narrative_ontology:measurement(bip_ossif_su_t2023, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2023, 0.68).
narrative_ontology:measurement(bip_ossif_su_t2025, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__protocol_ossification_reading, information_standard).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__protocol_ossification_reading, 0.03).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_lightning_network__routing_constraint).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_fee_market__priority_auction).

% DUAL FORMULATION NOTE:
% This constraint (protocol_ossification_reading) and digital_gold_reading form a constraint family: the ossification norm is the governance mechanism that protects the digital gold property. The p2p_cash_reading is the structural antagonist — it requires the protocol evolution that ossification blocks. All three share the bitcoin_whitepaper kernel but instantiate different ε values and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__protocol_ossification_reading, organized, 0.2).
constraint_indexing:directionality_override(bitcoin_whitepaper__protocol_ossification_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
