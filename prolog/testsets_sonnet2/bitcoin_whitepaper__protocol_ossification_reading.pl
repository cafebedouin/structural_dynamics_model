% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__protocol_ossification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: bitcoin_whitepaper__protocol_ossification_reading
 *   human_readable: Bitcoin Base-Layer Ossification Doctrine (Near-Unanimity Requirement for Protocol Change)
 *   domain: cryptocurrency economics / monetary systems / technology governance
 *
 * SUMMARY:
 *   This story authors ONE reading of the Bitcoin whitepaper kernel: the
 *   protocol-ossification reading, which holds that base-layer protocol
 *   changes are illegitimate unless they approach universal consensus, and
 *   that stability is the primary virtue of the system. Under this reading,
 *   the 2015-2017 block-size conflict, the SegWit compromise, and the
 *   subsequent multi-year moratorium on further base-layer capacity increases
 *   are read as principled conservatism protecting the network from capture
 *   and chain-split risk. The same events, read through the sibling p2p-cash
 *   reading (censorship-resistant medium of exchange) or the digital-gold
 *   reading (scarce store of value), would emit different constraints with
 *   different ε values and different victim sets — this file does not attempt
 *   to average across those readings or describe the contest itself; it
 *   authors the ossification reading cleanly, on its own terms, as the
 *   arrangement under contest, evaluated by its own lights.
 *
 * KEY AGENTS:
 *   - long_term_holders: primary beneficiary — predictable monetary policy is their entire investment thesis
 *   - mining_pool_operators: beneficiary and partial agenda-setter — fee-market pressure from scarce block space is revenue
 *   - core_maintainer_coalition: agenda-setter — controls the reference implementation and what counts as 'approaching consensus'
 *   - merchants_needing_cheap_onchain_payments, unbanked_users_in_high_fee_periods, developing_world_remittance_users: primary targets — bear fee spikes and exclusion from base-layer use
 *   - alternative_scaling_chains: excluded — forked implementations that rejected the ossification norm were expelled from the social-consensus boundary of 'Bitcoin'
 *   - protocol_historians: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper__protocol_ossification_reading, 0.62).
domain_priors:theater_ratio(bitcoin_whitepaper__protocol_ossification_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__protocol_ossification_reading, "Bitcoin Base-Layer Ossification Doctrine (Near-Unanimity Requirement for Protocol Change)").
narrative_ontology:topic_domain(bitcoin_whitepaper__protocol_ossification_reading, "cryptocurrency economics / monetary systems / technology governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__protocol_ossification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__protocol_ossification_reading, 'a639761c-65b1-4cab-89b9-f1e65549ba59').
narrative_ontology:cs_kernel_codification('a639761c-65b1-4cab-89b9-f1e65549ba59', fixed_text).
narrative_ontology:cs_authority_grounding('a639761c-65b1-4cab-89b9-f1e65549ba59', practice).
narrative_ontology:cs_interpretation_layer_present('a639761c-65b1-4cab-89b9-f1e65549ba59').
narrative_ontology:cs_reading_relation('a639761c-65b1-4cab-89b9-f1e65549ba59', bitcoin_whitepaper__digital_gold_reading, influences).
narrative_ontology:cs_reading_relation('a639761c-65b1-4cab-89b9-f1e65549ba59', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_axiom('a639761c-65b1-4cab-89b9-f1e65549ba59', foundational, protocol_stability_is_primary_virtue).
narrative_ontology:cs_axiom_status(protocol_stability_is_primary_virtue, holdable).
narrative_ontology:cs_axiom_grounding('a639761c-65b1-4cab-89b9-f1e65549ba59', protocol_stability_is_primary_virtue, instrumental).
narrative_ontology:cs_axiom('a639761c-65b1-4cab-89b9-f1e65549ba59', foundational, legitimacy_requires_near_universal_consensus).
narrative_ontology:cs_axiom_status(legitimacy_requires_near_universal_consensus, holdable).
narrative_ontology:cs_axiom_grounding('a639761c-65b1-4cab-89b9-f1e65549ba59', legitimacy_requires_near_universal_consensus, conventional).
narrative_ontology:cs_reference_frame('a639761c-65b1-4cab-89b9-f1e65549ba59', satoshi_era_rough_consensus_norm).
narrative_ontology:cs_drift_state('a639761c-65b1-4cab-89b9-f1e65549ba59', post_block_size_war_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a639761c-65b1-4cab-89b9-f1e65549ba59', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, core_maintainer_coalition).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, layer2_infrastructure_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, merchants_needing_cheap_onchain_payments).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, unbanked_users_in_high_fee_periods).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, protocol_researchers_proposing_capacity_changes).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, developing_world_remittance_users).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, monetary_policy_credibility_requires_immutability).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, conservatism_prevents_catastrophic_bugs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin primarily as a store of value and benefit directly from any change being blocked unless it approaches unanimity: predictability of the 21 million supply cap and settlement rules is the entire basis of their valuation thesis. They can exit into other assets at will and lose nothing if base-layer capacity never grows, so they campaign hardest for stasis.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders, beneficiary,
    organized, civilizational, arbitrage, global).

% Earn revenue from transaction fee pressure that scarce block space generates; a near-unanimity requirement lets a coordinated minority of hashpower or node operators veto any change that would relieve fee pressure. They can redeploy hashpower to other chains but profit from the status quo as long as it holds.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_operators, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_operators, agenda_setter).

% Controls the reference implementation and the social process for merging changes; enforces the norm that anything short of overwhelming consensus is illegitimate to ship, which in practice means their gatekeeping judgment decides what counts as consensus. Their professional identity and reputational capital are built entirely around stewarding conservatism, making exit from this posture equivalent to abandoning their role.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, core_maintainer_coalition, agenda_setter,
    institutional, generational, identity_locked, global).

% Build businesses (payment channels, sidechains, custodial rails) whose entire value proposition depends on base-layer scarcity pushing transaction demand upward into their layer. A change that cheaply expanded base-layer throughput would directly compete with their product, so they lobby to keep the ossification norm intact.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, layer2_infrastructure_operators, beneficiary,
    organized, biographical, arbitrage, global).

% Want to accept bitcoin directly for everyday transactions but face fee spikes and confirmation delays whenever demand rises, because base-layer capacity has not scaled to meet transaction volume. Their only real options are routing through custodial or layer-2 intermediaries (reintroducing counterparty risk) or abandoning on-chain use entirely.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, merchants_needing_cheap_onchain_payments, payer,
    moderate, biographical, constrained, global).

% Rely on bitcoin as an alternative to banking infrastructure they cannot access, but during periods of network congestion the fees required for a confirmed transaction can exceed the value being sent, pricing them out of the very use case the network was pitched to them for. They have no seat in the governance process and no resources to run alternative infrastructure.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, unbanked_users_in_high_fee_periods, payer,
    powerless, immediate, trapped, global).

% Design and propose block-size, opcode, or throughput changes backed by technical analysis, but find proposals blocked indefinitely because any identifiable faction's objection is treated as sufficient to deny 'approaching universal consensus.' Their career investment in bitcoin-specific research makes exiting to a different chain costly, but staying means perpetual proposal death.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, protocol_researchers_proposing_capacity_changes, payer,
    moderate, biographical, constrained, global).

% Need low-cost cross-border transfers and were told bitcoin would serve this need; instead they absorb volatility plus fee volatility, and during congestion the fee alone can consume a meaningful share of a small remittance. They have essentially no voice in the technical governance debates that set these tradeoffs.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, developing_world_remittance_users, payer,
    powerless, immediate, trapped, global).

% Forked or independently built chains that adopted larger blocks or faster settlement were effectively expelled from the 'Bitcoin' brand and social consensus despite implementing changes the ossification-reading community rejected. Their technical arguments are excluded from the governance conversation by definition, since the base-layer community treats departure from stability-first governance as disqualifying.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, alternative_scaling_chains, excluded,
    organized, biographical, mobile, global).

% Study the block-size wars, SegWit activation, and subsequent governance disputes to trace how the near-unanimity norm formed, whose interests it now serves, and whether it functions as principled conservatism or as a veto captured by whoever currently benefits from scarcity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, protocol_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__protocol_ossification_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__protocol_ossification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Requiring near-universal consensus before changing consensus-critical rules genuinely solves a real coordination problem: a permissionless network with no central authority needs an extremely high bar for changing the rules everyone relies on, because a contested or narrowly-passed change risks a chain split that destroys the very property (a single canonical ledger) the system exists to provide.
% TRANSFER_FUNCTION: The near-unanimity requirement moves optionality and fee-market rents toward those already holding bitcoin, controlling mining infrastructure, or operating layer-2 businesses built on base-layer scarcity, and moves cost and exclusion onto users who need cheap, high-volume, on-chain settlement — remittance senders, small merchants, and anyone the original whitepaper's medium-of-exchange framing was aimed at.
% ABSENT_VOICES: Developing-world remittance users and unbanked populations who bear fee spikes directly have no technical or organizational presence in the mailing lists, BIP process, or conference circuit where 'consensus' is actually measured; their absence lets the maintainer coalition and long-term holders describe stasis as broadly agreed when the affected population was never consulted.
% DISAPPEARANCE_RATIONALE: If the near-unanimity norm vanished and base-layer parameters could change on ordinary majority technical judgment, block capacity, fee markets, layer-2 business models, and the investment thesis built on fixed-supply predictability would all be exposed to renegotiation — mining revenue models, L2 infrastructure valuations, and holder expectations about monetary policy immutability would all have to adjust.
% FOUNDING_PROBLEM: Early Bitcoin needed a way to prevent any single actor or coalition from unilaterally rewriting consensus rules, because a network without central authority that allows easy rule changes is vulnerable to capture, chain splits, or loss of the scarcity guarantee that gives the currency its value.
% FOUNDING_PROBLEM_CORROBORATION: The core maintainer coalition and long-term holders attest the founding problem (capture-resistance, credible scarcity) remains fully live and justifies continued conservatism. Independent academic analyses of the 2015-2017 block-size conflict and testimony from departed developers and merchant-adoption advocates argue the problem has been partially resolved by SegWit and Taproot's technical adequacy, and that the current near-unanimity bar now functions less as capture-resistance and more as a veto mechanism serving whichever coalition currently profits from scarcity — a reading corroborated by economists studying Bitcoin fee markets from outside the maintainer and holder communities.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__protocol_ossification_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__protocol_ossification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__protocol_ossification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__protocol_ossification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__protocol_ossification_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction rose from a modest 0.22 at the network's early stage (when block space was not yet scarce and stability was uncontroversial) to 0.58 by the interval's end, tracking the entrenchment of the near-unanimity norm as an effective veto after the block-size wars. Suppression climbed sharply (0.30 to 0.62) as the social and technical mechanisms for blocking base-layer change hardened — from informal mailing-list objection to a settled norm that any identifiable faction's dissent defeats a proposal. Theater ratio rose moderately (0.15 to 0.40) as 'consensus-seeking' processes increasingly function as venues for performing due diligence on proposals that have no realistic path to passage, rather than genuine deliberation. Accessibility collapse is high (0.72) because once a user or developer understands that any capacity-increasing proposal will be vetoed regardless of technical merit, the perceived alternative space (fork, or route through L2/custodial rails) collapses to those two paths. Resistance is high (0.70): researchers, merchants, and remittance-focused developers have repeatedly proposed and fought for base-layer changes, and that resistance is precisely what the near-unanimity requirement is built to absorb and defeat.
 *
 * PERSPECTIVAL GAP:
 *   From the core maintainer coalition's seat, the near-unanimity requirement is the mechanism that has protected Bitcoin from every proposed capture attempt and preserved the one property — a single canonical, predictable ledger — that gives the asset its value; refusing easy changes is fidelity to the founding design. From a remittance user or merchant's seat, the same requirement is experienced as an unaccountable veto that has priced them out of the use case they were told the system existed for, enforced by a community whose deliberative process they have no access to. The engine computes these as structurally different seat-level classifications from the same authored data; this file does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders, mining pool operators, the core maintainer coalition, and layer-2 infrastructure operators sit near the beneficiary end of directionality: their revenue, valuation thesis, or professional standing are all subsidized by the norm's persistence, and each retains meaningful exit options (arbitrage into other assets or chains, or institutional identity-lock that nonetheless carries real influence over outcomes). Merchants, unbanked users, remittance senders, and capacity researchers sit near the target end: they bear the fee-market consequences directly, have constrained or trapped exit options, and have no comparable influence over what counts as 'approaching consensus.' Alternative scaling chains are excluded rather than merely disadvantaged — their technical proposals are foreclosed from the conversation by the community boundary itself, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing unilateral rule changes that could enable capture or fracture the network's canonical ledger — was genuinely live in Bitcoin's early years, when the network had no track record and any change carried existential risk. Whether that problem remains as acute after more than a decade of battle-tested infrastructure, multiple successful soft forks (SegWit, Taproot), and a mature technical review process is exactly the contested genealogy question this story routes to the six_questions founding-problem fields rather than resolving unilaterally: the maintainer coalition and holders say the problem is still live; independent economists and departed developers say the arrangement has drifted from capture-resistance to rent-preserving veto. The classification as tangled_rope (rather than snare) reflects that the coordination function — genuine chain-split-avoidance value — has not disappeared, even as an asymmetric extraction pattern has grown up alongside it and now requires active social and technical enforcement (informal excommunication of dissenting implementations, gatekeeping of the BIP process) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ossification_capture_resistance_ambiguity,
    'Is the near-unanimity requirement still functioning as genuine capture-resistance (protecting the network from unilateral rule changes by any single powerful actor), or has it become a captured veto mechanism serving whichever coalition currently profits from base-layer scarcity?',
    'Compare technical review outcomes for capacity-neutral security patches (which pass) against capacity-increasing proposals with comparable engineering review and testing (which do not), controlling for objective technical risk; also examine whether objecting parties in blocked proposals have identifiable financial stakes in scarcity persisting.',
    'If the pattern shows technically comparable proposals blocked selectively based on whether they threaten scarcity-dependent revenue streams, the classification should move toward snare; if blocking is uncorrelated with beneficiary financial interest and tracks genuine unresolved technical risk, the tangled_rope classification with a live coordination function is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ossification_capture_resistance_ambiguity, empirical, 'Whether the ossification norm is genuine capture-resistance or a captured veto.').

omega_variable(
    kernel_reading_boundary_disagreement,
    'Where exactly does the ossification reading''s core commitment (stability as primary virtue) locate the boundary of legitimate change — is it a fixed technical property of Bitcoin, or a socially negotiated and shifting threshold that has hardened over time as certain actors'' interests became entrenched in the status quo?',
    'Historical analysis of what counted as ''sufficient consensus'' for SegWit versus what has been demanded of subsequent capacity proposals — if the bar has risen over time independent of technical stakes, that supports a socially-constructed and drifting threshold rather than a fixed principle.',
    'A rising, interest-correlated bar for ''consensus'' would support reading this constraint''s persistence as increasingly extractive over time (consistent with the authored measurement trend); a stable, principle-derived bar would support the maintainer coalition''s own account of consistent conservatism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_disagreement, conceptual, 'Whether the consensus bar is a fixed principle or a drifting, interest-correlated threshold.').

omega_variable(
    sibling_reading_foreclosure_question,
    'Does the ossification reading''s dominance within the Bitcoin developer and node-operator community effectively foreclose the p2p_cash_reading as a live option for the base protocol itself (as opposed to merely pushing it to Layer 2), even though the two readings are declared coexists_with at the kernel level?',
    'Track whether any base-layer capacity-increasing proposal aligned with the p2p_cash_reading''s transactional-use priorities has had a realistic path to activation in the post-block-size-war era, versus proposals routed entirely to off-chain layers.',
    'If no base-layer path remains realistically open, the coexists_with relation between this reading and p2p_cash_reading may understate the actual foreclosure occurring in practice, even though both readings remain nominally live as public positions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_question, conceptual, 'Whether practical foreclosure of on-chain scaling exceeds the declared coexists_with relation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__protocol_ossification_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(bitc_tr_t24, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(bitc_tr_t32, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(bitc_tr_t40, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(bitc_be_t24, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(bitc_be_t32, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(bitc_be_t40, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(bitc_su_t24, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(bitc_su_t32, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(bitc_su_t40, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__protocol_ossification_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__protocol_ossification_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__p2p_cash_reading).

% DUAL FORMULATION NOTE:
% These three constraint files decompose the natural-language label 'the Bitcoin whitepaper' into three structurally distinct readings sharing one kernel (bitcoin_whitepaper). Each reading has a different ε, different beneficiary/victim structure, and would classify differently under the engine. The ossification_reading (this file) is upstream-supportive of the digital_gold_reading's scarcity thesis and downstream-constraining on the p2p_cash_reading's transactional use case. Do not average ε across the three; each is a separate, ε-invariant constraint linked here for contamination-propagation analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
