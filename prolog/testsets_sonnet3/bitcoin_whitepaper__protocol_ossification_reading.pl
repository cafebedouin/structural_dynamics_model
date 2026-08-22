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
 *   human_readable: Bitcoin Protocol Ossification Reading — Near-Unanimous Consensus as Legitimacy Gate
 *   domain: cryptocurrency economics / monetary systems / technology governance
 *
 * SUMMARY:
 *   This story reads the Bitcoin whitepaper kernel through the lens that
 *   treats near-universal consensus as the legitimacy criterion for any
 *   base-protocol change, with stability itself as the primary virtue to be
 *   protected. On this reading, the 2015-2017 block size dispute is the
 *   paradigm case: a technically substantial faction favoring larger blocks
 *   was unable to overcome the effective veto exercised by core developers,
 *   node operators resisting resource requirements, and mining pools
 *   protecting sunk capital, and was ultimately pushed into a minority-chain
 *   fork (Bitcoin Cash) rather than achieving a base-layer change. The norm
 *   that 'contested = illegitimate' converted what could have been an
 *   ordinary engineering tradeoff into a permanent scope limitation on the
 *   base protocol, with layer-two systems absorbing the resulting demand for
 *   scaling. This is a distinct constraint from the p2p_cash_reading (which
 *   treats Bitcoin as a censorship-resistant payment medium and would
 *   evaluate scaling failure as a direct betrayal of the founding purpose)
 *   and from the digital_gold_reading (which treats Bitcoin purely as a store
 *   of value where transactional throughput is irrelevant or even
 *   undesirable). Each reading has a different beneficiary/victim structure
 *   and a different epsilon; they are linked here only through
 *   network.affects_constraints, not merged.
 *
 * KEY AGENTS:
 *   - existing_utxo_holders: primary beneficiary (organized/arbitrage) — protected asset value from monetary-policy stability
 *   - core_developers_maintaining_status_quo: agenda_setter (institutional/arbitrage) — controls what counts as consensus
 *   - mining_pool_incumbents: beneficiary and secondary agenda_setter (powerful/arbitrage) — sunk capital protected by stasis
 *   - users_needing_base_layer_scaling: primary target (powerless/trapped) — bears the cost of blocked capacity increases
 *   - excluded_altcoin_and_fork_communities: excluded voice (moderate/trapped) — pushed out when consensus failed
 *   - protocol_researchers: analytical observer — documents the governance mechanism without a stake
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
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__protocol_ossification_reading, "Bitcoin Protocol Ossification Reading — Near-Unanimous Consensus as Legitimacy Gate").
narrative_ontology:topic_domain(bitcoin_whitepaper__protocol_ossification_reading, "cryptocurrency economics / monetary systems / technology governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__protocol_ossification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__protocol_ossification_reading, '746d2571-0e09-4e46-b64a-60eb34237c32').
narrative_ontology:cs_kernel_codification('746d2571-0e09-4e46-b64a-60eb34237c32', fixed_text).
narrative_ontology:cs_authority_grounding('746d2571-0e09-4e46-b64a-60eb34237c32', practice).
narrative_ontology:cs_interpretation_layer_present('746d2571-0e09-4e46-b64a-60eb34237c32').
narrative_ontology:cs_reading_relation('746d2571-0e09-4e46-b64a-60eb34237c32', bitcoin_whitepaper__p2p_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('746d2571-0e09-4e46-b64a-60eb34237c32', bitcoin_whitepaper__digital_gold_reading, influences).
narrative_ontology:cs_axiom('746d2571-0e09-4e46-b64a-60eb34237c32', foundational, unanimity_approximation_required_for_legitimate_change).
narrative_ontology:cs_axiom_status(unanimity_approximation_required_for_legitimate_change, holdable).
narrative_ontology:cs_axiom_grounding('746d2571-0e09-4e46-b64a-60eb34237c32', unanimity_approximation_required_for_legitimate_change, conventional).
narrative_ontology:cs_axiom('746d2571-0e09-4e46-b64a-60eb34237c32', foundational, stability_outweighs_functional_improvement).
narrative_ontology:cs_axiom_status(stability_outweighs_functional_improvement, holdable).
narrative_ontology:cs_axiom_grounding('746d2571-0e09-4e46-b64a-60eb34237c32', stability_outweighs_functional_improvement, instrumental).
narrative_ontology:cs_reference_frame('746d2571-0e09-4e46-b64a-60eb34237c32', pre_block_size_war_rough_consensus_norm).
narrative_ontology:cs_drift_state('746d2571-0e09-4e46-b64a-60eb34237c32', post_blocksize_war_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('746d2571-0e09-4e46-b64a-60eb34237c32', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, existing_utxo_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, core_developers_maintaining_status_quo).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_incumbents).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, layer_two_infrastructure_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, users_needing_base_layer_scaling).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, developers_proposing_contested_soft_forks).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, merchants_requiring_low_fee_microtransactions).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, excluded_altcoin_and_fork_communities).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, monetary_policy_credibility_doctrine).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, schelling_point_coordination_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin already mined or acquired and benefit from the credibility of a fixed, unchanging monetary policy and settlement rules. A protocol that resists change protects the value of what they already hold against dilution or redesign risk. Their exit option is simply holding — they lose nothing by consensus-gating remaining static.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, existing_utxo_holders, beneficiary,
    organized, civilizational, arbitrage, global).

% Control the reference implementation and set the technical and social bar for what counts as 'consensus,' effectively deciding which proposals get airtime and which are declared too controversial to merit further discussion. Their institutional position and reputational capital are built on stewarding conservatism; loosening the consensus requirement would dilute their gatekeeping role.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, core_developers_maintaining_status_quo, agenda_setter,
    institutional, generational, arbitrage, global).

% Have optimized capital investment (ASICs, energy contracts) around the current block size, fee market, and difficulty regime. A stable protocol protects sunk infrastructure investment. They can threaten to withhold hash power from contested upgrades, effectively exercising a veto that reinforces the near-unanimity requirement.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_incumbents, agenda_setter).

% Operate payment channels, sidechains, and custodial services built specifically to route around base-layer limitations. Their business model depends on base-layer scaling remaining blocked — every year the base protocol stays frozen is a year their layer captures the transaction volume and fees that would otherwise be handled on-chain.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, layer_two_infrastructure_operators, beneficiary,
    organized, biographical, arbitrage, global).

% Want lower fees and higher throughput for everyday transactions but cannot get a base-layer capacity increase through the consensus gate because any change touching consensus rules faces an effective veto from any sufficiently motivated minority. Their only options are paying elevated fees, migrating to layer-two systems with their own trust assumptions, or leaving the network.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, users_needing_base_layer_scaling, payer,
    powerless, immediate, trapped, global).

% Draft technical proposals to change block size, add opcodes, or adjust fee mechanisms. Even with substantial technical merit and significant community support, proposals that fall short of near-universal buy-in are declared illegitimate and abandoned or forced into contentious hard forks that fracture the community and devalue their work.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, developers_proposing_contested_soft_forks, payer,
    moderate, biographical, constrained, global).

% Need small, frequent, cheap transactions for point-of-sale or machine-to-machine payments. Base-layer fee volatility driven by capacity constraints makes bitcoin unusable for this purpose without third-party payment processors, who reintroduce the intermediation bitcoin was meant to eliminate.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, merchants_requiring_low_fee_microtransactions, payer,
    powerless, immediate, constrained, global).

% Communities that forked away (or were pushed to fork) when their scaling proposals failed to reach near-universal consensus. They argue the ossification norm was applied selectively and that the 'legitimate chain' designation followed social and economic power, not technical merit. They are permanently excluded from the ongoing legitimacy conversation about the main chain.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, excluded_altcoin_and_fork_communities, excluded,
    moderate, generational, trapped, global).

% Study consensus mechanisms, game theory, and governance outcomes across cryptocurrency networks. They document how the near-unanimity requirement functions in practice — as a deliberate Schelling-point-preserving mechanism, as capture by incumbent capital, or both — without holding a stake in any particular outcome.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, protocol_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__protocol_ossification_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__protocol_ossification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Requiring near-universal consensus before any base protocol change prevents a small faction from unilaterally altering rules that everyone else relies on — it solves the genuine problem that a network without agreed change procedures fragments into incompatible, warring chains whenever a dispute arises.
% TRANSFER_FUNCTION: Moves the option value of protocol flexibility away from users who need base-layer changes (higher throughput, new capabilities) and toward holders and infrastructure operators who benefit from monetary-policy predictability and from directing transaction volume onto layers they control.
% ABSENT_VOICES: Users priced out of base-layer transactions and developers whose scaling proposals were declared 'too controversial' have no formal channel to force reconsideration — their objections were litigated in mailing lists and forums years ago and the ossification norm now treats that history as settled, closing the topic to renewed debate.
% DISAPPEARANCE_RATIONALE: Beneficiaries argue the world would rearrange catastrophically if the consensus norm vanished — the currency's core value proposition (predictable, un-inflatable, un-alterable money) would evaporate under contentious changes pushed through by transient majorities. Payers argue the norm's disappearance would simply let overdue capacity and feature improvements finally ship, and that the 'catastrophe' framing is itself a beneficiary narrative protecting a specific status quo.
% FOUNDING_PROBLEM: Early Bitcoin needed a way to prevent a single actor or faction from rewriting the rules of the ledger after the fact, especially the money supply schedule, which would destroy the currency's core promise of scarcity and predictability.
% FOUNDING_PROBLEM_CORROBORATION: Core developers and long-term holders attest the founding problem (preventing arbitrary rule-rewriting) remains fully live and justifies the current bar. Independent protocol researchers and departed fork communities attest that the norm has been extended well beyond monetary-policy immutability to block routine capacity and feature changes that carry no analogous risk to scarcity — a scope expansion the founding problem does not itself justify, corroborated by academic governance studies of the 2015-2017 block size dispute conducted by parties outside both the core development team and the layer-two ecosystem.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__protocol_ossification_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__protocol_ossification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__protocol_ossification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.58) reflects that the consensus-gate does not merely coordinate against arbitrary rewrites of the money supply — it has been generalized into a durable block on scaling and feature changes that serve a genuinely different, narrower set of interests (existing holders, mining incumbents, L2 operators) at the expense of users and merchants who need base-layer throughput. Suppression (0.62) captures the social-technical enforcement machinery: node operators refusing to run non-conforming clients, developers' proposals declared 'too controversial' without a formal path to override that judgment, and mining hash power functioning as an informal veto. Theater ratio (0.4) reflects that 'consensus' rhetoric increasingly performs a legitimacy function — invoking the founding scarcity-protection problem to justify blocking changes that carry no scarcity risk at all. Accessibility collapse (0.68) is high because, once a proposal is declared non-consensus, there is effectively no institutional path back to reconsideration short of a contentious hard fork that fragments the community. Resistance (0.72) is high because affected parties (scaling advocates, some merchants, departed fork communities) have vocally and repeatedly contested the norm, unlike a genuine mountain which would meet little resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, this looks like principled conservatism protecting a scarce, credible monetary asset from capture by transient majorities — exactly the coordination function the founding problem demands. From the payer seats, the same structure looks like a veto mechanism that outgrew its justification, blocking ordinary engineering improvements under cover of monetary-policy sanctity. The engine's per-seat computation should reflect this: agenda-setters and beneficiaries likely compute nearer rope/mountain, while payers compute nearer tangled_rope/snare given their trapped exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing holders, core developers, mining incumbents, and L2 operators are declared beneficiaries because the consensus-gate directly protects assets they hold or business models they built around base-layer stasis — their directionality sits near the full-beneficiary end. Users needing scaling and merchants needing microtransactions are declared victims because the same gate forecloses the specific base-layer changes their use case requires, and their exit options are trapped or constrained (leaving means either paying elevated fees, trusting L2 custodial risk, or abandoning the network entirely) — their directionality sits near the full-target end. Developers proposing contested forks occupy a moderate-power, constrained-exit position: they have professional and reputational investment in Bitcoin specifically, so full exit is costly even when their proposals are blocked.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing arbitrary rewriting of monetary policy — is genuinely still live (nothing has neutralized the risk that unilateral rule changes could destroy Bitcoin's scarcity guarantee). But the ossification norm as currently practiced has generalized far beyond that founding problem to block changes (block size, opcode additions, fee mechanism tweaks) that carry no comparable risk to monetary policy. This is the mandatrophy signature: a mandate whose original justification remains partially live is being used to block classes of change the original justification never covered. Classifying this as tangled_rope rather than mountain or pure snare captures both halves honestly — there is a real coordination function (preventing arbitrary supply-schedule changes) riding alongside a genuinely asymmetric extraction (blocking scaling changes that would benefit ordinary users while protecting incumbent capital and L2 rent-seeking).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_scope_vs_applied_scope,
    'Does the near-universal-consensus requirement, as actually applied, track the founding problem (preventing arbitrary monetary-policy rewrites) or has it been generalized to block a much wider class of ordinary engineering changes without equivalent justification?',
    'Comparative case analysis of past protocol proposals: classify each as touching monetary-policy-adjacent parameters versus purely capacity/feature parameters, and measure whether the consensus bar applied was proportionate to the actual risk in each case.',
    'If the bar has been applied uniformly regardless of risk category, this supports the tangled_rope reading (coordination function abused to block a broader set of changes than it was built to guard). If the bar tracks risk proportionately, the constraint is closer to a genuine, narrowly-scoped rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_scope_vs_applied_scope, empirical, 'Whether the applied scope of the consensus requirement matches its founding justification.').

omega_variable(
    which_reading_is_the_real_bitcoin,
    'Is the protocol_ossification_reading a faithful application of the whitepaper''s design intent, or a retrospective institutional narrative that displaced the original p2p-cash vision after the block size wars?',
    'Textual and historical analysis of the whitepaper and Satoshi''s early mailing-list posts regarding scaling assumptions, compared against the post-2017 institutional consensus doctrine; interview surviving participants from both sides of the block size dispute.',
    'If ossification displaced an original scaling-friendly design intent, the beneficiary structure identified here (incumbents, L2 operators) looks more like capture of a drifted mandate. If ossification was always the design intent, the coordination function is more clearly original and legitimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_real_bitcoin, conceptual, 'Committer-frame ambiguity: which reading of the kernel is the historically grounded one, and where the disagreement between readings is actually located (design intent vs. later institutional narrative).').

omega_variable(
    coalition_power_of_powerless_users,
    'Can users needing base-layer scaling and merchants needing microtransactions form effective coalition pressure (e.g., through economic majority signaling, exchange delisting threats, or coordinated node-operator campaigns) despite individually powerless positions?',
    'Track historical instances (e.g., UASF/BIP148 user-activated soft fork movement) where dispersed users organized to force protocol change against developer and miner preference, and assess success rate and durability of outcomes.',
    'If coalition mechanisms are real and have previously succeeded, the powerless-trapped classification for these stakeholders overstates their actual leverage and the constraint may be less purely extractive than the base metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_power_of_powerless_users, empirical, 'Whether dispersed, individually powerless users can exercise coalition power against the consensus gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__protocol_ossification_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 9, 0.34).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 15, 0.4).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 9, 0.5).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 3, 0.44).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 9, 0.57).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__protocol_ossification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__digital_gold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the bitcoin_whitepaper kernel per the epsilon-invariance principle. bitcoin_whitepaper__digital_gold_reading and bitcoin_whitepaper__p2p_cash_reading are separate constraint files with their own epsilon, beneficiary/victim structure, and classification. This file (protocol_ossification_reading) has a distinct victim set (use cases requiring base-protocol changes) not present in the sibling readings, and a distinct beneficiary set (incumbent holders, miners, L2 operators) organized around stasis rather than around store-of-value or medium-of-exchange framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
