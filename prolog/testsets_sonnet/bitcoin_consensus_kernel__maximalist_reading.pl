% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__maximalist_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__maximalist_reading
 *   human_readable: Maximalist Reading of the Bitcoin Consensus Kernel (21M Cap Immutability)
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This story instantiates the maximalist reading of the Bitcoin consensus
 *   kernel: the position that the whitepaper's monetary schedule (21 million
 *   cap, halving issuance curve) is a founding covenant immune to
 *   renegotiation, such that any protocol change touching issuance or
 *   block-space economics constitutes a betrayal of the network's original
 *   design rather than a legitimate governance decision. This reading is one
 *   of three structurally distinct claims that share the label 'the Bitcoin
 *   whitepaper's meaning' — the utility_reading (whitepaper as minimum viable
 *   mechanism, open to iteration) and pragmatic_synthesis (base-layer
 *   immutable, upper layers free) are separate constraints with different
 *   beneficiary/victim structures and are NOT blended into this file. The
 *   maximalist reading concentrates gains toward early holders and mining
 *   incumbents whose capital position depends on scarcity remaining absolute,
 *   and imposes costs on scalability and payment-use-case builders who are
 *   structurally locked out of altering the base layer regardless of
 *   technical merit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.68).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.71).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Maximalist Reading of the Bitcoin Consensus Kernel (21M Cap Immutability)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '8f8a1c3e-b119-46f0-8245-6aadae31f46d').
narrative_ontology:cs_kernel_codification('8f8a1c3e-b119-46f0-8245-6aadae31f46d', fixed_text).
narrative_ontology:cs_authority_grounding('8f8a1c3e-b119-46f0-8245-6aadae31f46d', practice).
narrative_ontology:cs_interpretation_layer_present('8f8a1c3e-b119-46f0-8245-6aadae31f46d').
narrative_ontology:cs_reading_relation('8f8a1c3e-b119-46f0-8245-6aadae31f46d', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('8f8a1c3e-b119-46f0-8245-6aadae31f46d', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('8f8a1c3e-b119-46f0-8245-6aadae31f46d', foundational, monetary_schedule_is_inviolable_covenant).
narrative_ontology:cs_axiom_status(monetary_schedule_is_inviolable_covenant, holdable).
narrative_ontology:cs_axiom_grounding('8f8a1c3e-b119-46f0-8245-6aadae31f46d', monetary_schedule_is_inviolable_covenant, deontological).
narrative_ontology:cs_axiom('8f8a1c3e-b119-46f0-8245-6aadae31f46d', secondary, any_base_layer_parameter_change_constitutes_a_new_asset).
narrative_ontology:cs_axiom_status(any_base_layer_parameter_change_constitutes_a_new_asset, holdable).
narrative_ontology:cs_axiom_grounding('8f8a1c3e-b119-46f0-8245-6aadae31f46d', any_base_layer_parameter_change_constitutes_a_new_asset, conventional).
narrative_ontology:cs_reference_frame('8f8a1c3e-b119-46f0-8245-6aadae31f46d', genesis_whitepaper_literalism).
narrative_ontology:cs_drift_state('8f8a1c3e-b119-46f0-8245-6aadae31f46d', post_scaling_wars_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('8f8a1c3e-b119-46f0-8245-6aadae31f46d', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, long_term_hodlers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, mining_incumbents).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, payment_layer_innovators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, late_entrant_users).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, sound_money_scarcity_doctrine).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, code_is_covenant_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquired coins when the network was small and cheap to mine or buy; their position appreciates directly with every rejection of proposals that would alter issuance or increase supply flexibility. They fund advocacy, sponsor core developers aligned with strict immutability, and treat any monetary-policy change proposal as an existential attack on their holdings.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_holders, beneficiary,
    organized, generational, arbitrage, global).

% Hold coins as a savings vehicle premised on the fixed 21 million cap; they participate in social-layer enforcement (community shaming, exchange delisting campaigns, node-operator coordination) against any client implementing a hard fork that touches issuance schedule or supply cap.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, long_term_hodlers, beneficiary,
    moderate, generational, mobile, global).

% Have sunk capital into ASIC hardware calibrated to the current difficulty and fee-market structure; benefit from a scarce, non-inflationary asset that appreciates in fiat terms, and from a base layer that stays simple and expensive to use (driving fee revenue). They enforce the covenant informally by refusing to signal for consensus changes that would alter the security-budget or issuance assumptions their hardware investment depends on.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, mining_incumbents, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, mining_incumbents, agenda_setter).

% Control merge rights on the reference client and treat the whitepaper's monetary schedule as a founding covenant not subject to renegotiation. They reject or indefinitely stall proposals (larger blocks, changed emission curves, protocol-level scaling primitives) framed as violations of the original design, regardless of technical merit, because their professional and reputational identity is fused with strict-constructionist orthodoxy.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, core_maintainers_maximalist_faction, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Build alternative transaction-throughput or programmability proposals and consistently see them rejected, forked away, or forced into vestigial workarounds because the maximalist reading treats any base-layer parameter change as covenant violation. Their exit options are effectively limited to abandoning the base chain entirely for an altcoin with none of Bitcoin's network effects, or building second layers that remain hostage to base-layer settlement costs the covenant preserves.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_developers, payer,
    moderate, biographical, constrained, global).

% Attempted to position Bitcoin as a medium of exchange for everyday payments; the maximalist reading's insistence on base-layer scarcity and minimal block space (justified as protecting monetary integrity) pushed transaction fees and confirmation times to levels incompatible with retail payment use cases, driving this function to competing chains or centralized custodial workarounds that reintroduce the trust assumptions the protocol was meant to eliminate.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, payment_layer_innovators, payer,
    moderate, biographical, constrained, global).

% Enter the network without pre-existing holdings; face high acquisition costs set by the fixed-supply narrative, high transaction fees defended as necessary discipline, and social pressure treating any complaint about accessibility as an attack on sound money. Their structural position is the mirror image of early holders: the same immutability that enriches early holders raises their cost of entry and use.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, late_entrant_users, payer,
    powerless, biographical, trapped, global).

% Argue that only the base-layer monetary rules need be immutable while upper layers (sidechains, payment channels, covenants) should be free to innovate; they are present in developer forums but structurally excluded from merge authority and are routinely labeled as covenant-breakers by the maximalist faction regardless of the layering distinction they draw.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, pragmatic_synthesis_advocates, excluded,
    moderate, biographical, constrained, global).

% Study the game-theoretic and cryptographic properties of consensus rule changes without a stake in any faction's narrative; publish analyses of what would actually happen to security and decentralization under various proposed changes, largely ignored by the maximalist enforcement apparatus unless the findings support the existing covenant.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__maximalist_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a credible, verifiable commitment device: participants can rely on a fixed, publicly auditable issuance schedule without trusting any central bank or committee, solving the genuine coordination problem of establishing scarce digital money in a trust-minimized environment.
% TRANSFER_FUNCTION: Moves optionality and adaptability away from later entrants, scaling developers, and payment-use-case builders, and moves appreciation and rent from network growth toward those who hold coins acquired before the current price level and toward miners whose hardware is calibrated to the current fee-and-issuance regime.
% ABSENT_VOICES: Pragmatic-synthesis advocates and utility-reading proponents raise layering arguments in developer forums and mailing lists but hold no merge authority; retail users priced out of base-layer transactions have no organized voice at all in protocol governance, which occurs through informal rough consensus dominated by long-tenured maintainers and large holders.
% DISAPPEARANCE_RATIONALE: If the maximalist reading's enforcement apparatus (social shaming, exchange coordination, maintainer gatekeeping) disappeared overnight, early holders and mining incumbents argue the entire value proposition collapses because trust in scarcity is what generates the asset's price; scalability developers and payment-layer innovators argue the network would simply mature into a more flexible, layered system without losing its core security guarantees. Both outcomes are structurally plausible, and no resolution mechanism exists that both factions would recognize as authoritative.
% FOUNDING_PROBLEM: The whitepaper was written to solve double-spending without a trusted third party, using proof-of-work and a fixed emission schedule as an incentive-compatible mechanism to bootstrap a decentralized ledger.
% FOUNDING_PROBLEM_CORROBORATION: Protocol researchers outside both the maximalist and pragmatic-synthesis factions attest that the double-spend problem was solved early and durably by the core consensus mechanism itself, and that the fixed-supply schedule's necessity for that solution (as opposed to its value as a social commitment device) is not established by the original paper's cryptographic argument — the whitepaper's stated problem (Byzantine agreement on transaction order) is largely orthogonal to whether the issuance curve itself must remain permanently fixed.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) and suppression (0.71) are both high because enforcement of the covenant reading operates primarily through social and reputational mechanisms — merge-right gatekeeping, exchange-listing coordination, community shaming of 'covenant-breakers' — that have hardened over the interval as the asset's market capitalization grew and early holders' stake in strict immutability increased proportionally. Theater ratio (0.42) reflects that a substantial and growing share of the enforcement activity is now performative (denouncing proposals as 'attacks on sound money' in venues where the technical merits are not actually adjudicated) rather than functional review of what a given change would do to security or decentralization.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (core maintainers, mining incumbents) the covenant reading looks like principled fidelity to a founding design that prevented exactly the kind of ad hoc monetary manipulation the whitepaper was written to escape — genuine coordination around scarcity. From the payer seats (scalability developers, payment innovators, late entrants) the same structure computes as entrenched rent-protection dressed in constitutional language, where 'immutability' selectively applies to the parameters that benefit incumbents while other protocol parameters (difficulty adjustment period, signature schemes) have in fact changed via soft fork when it suited the same coalition.
 *
 * DIRECTIONALITY LOGIC:
 *   Early holders and mining incumbents are declared beneficiaries because their capital position (coins acquired cheaply, or hardware calibrated to current issuance/fee assumptions) appreciates specifically because the covenant reading forecloses supply-flexibility or block-space-flexibility proposals — this is a direct, not incidental, benefit. Scalability developers, payment-layer innovators, and late entrants are declared victims because the same enforcement mechanism forecloses their proposals or raises their costs of participation as a structural side effect of preserving the beneficiaries' position, not as an incidental externality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Byzantine fault-tolerant agreement on transaction order without a trusted third party) was substantially solved early and is corroborated as solved by protocol researchers outside both factions. The maximalist reading's specific claim — that the emission schedule itself must remain permanently fixed as a matter of covenant rather than as one possible parameterization of the underlying consensus mechanism — is not independently corroborated by the same researchers, who note the double-spend solution does not logically require permanent issuance-curve fixity. This is the mandatrophy signature: the classification prevents mislabeling 'the security mechanism is sound' (genuinely vindicated) as equivalent to 'therefore the specific numeric parameters chosen in 2009 must never be revisited' (a separate, contested normative claim riding on the vindicated technical achievement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_vs_parameter_ambiguity,
    'Is the 21 million supply cap a load-bearing cryptographic requirement of the consensus mechanism, or a chosen parameter that could be altered by coordinated upgrade without compromising the security properties the whitepaper actually proves?',
    'Formal analysis of whether the incentive-compatibility proofs in the original design depend on the specific numeric cap, versus depending only on issuance being predictable and bounded (which would be satisfiable under alternative schedules).',
    'If the cap is not load-bearing, the maximalist reading''s claim of covenant violation is revealed as a normative preference dressed as technical necessity, strengthening the classification toward tangled_rope or snare; if load-bearing, the immutability claim gains genuine mountain-adjacent standing for that specific parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_vs_parameter_ambiguity, conceptual, 'Whether monetary policy immutability is cryptographically necessary or normatively chosen.').

omega_variable(
    which_reading_is_the_kernel_committer_frame,
    'Among the maximalist, utility, and pragmatic_synthesis readings, is there a fact of the matter about which reading the original whitepaper author(s) intended, or is the kernel genuinely underdetermined by the source text?',
    'Textual and historical analysis of contemporaneous statements, mailing list archives, and the author''s own later commentary (where available) against each reading''s core premise; absent authorial testimony, treat as irreducibly distributed authority.',
    'If the source text genuinely underdetermines the reading, no single reading can claim the label ''the'' Bitcoin covenant, and the maximalist reading''s rhetorical claim to exclusive fidelity is itself part of the extraction mechanism (converting an interpretive choice into an existential loyalty test). If a determinate original intent exists and matches maximalism, the reading gains a stronger legitimacy claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_kernel_committer_frame, conceptual, 'Whether the kernel has a determinate original reading or is genuinely distributed among competing interpretations.').

omega_variable(
    beneficiary_concentration_vs_diffuse_claim,
    'Given gain_flow is authored as diffuse, is that accurate, or does a subset of early holders and large mining operations in fact capture a disproportionate, identifiable share of the appreciation the covenant enforcement protects?',
    'On-chain distribution analysis of coin age and holding concentration (e.g. what share of supply is held by wallets active before a given early date) cross-referenced with mining pool concentration data.',
    'If gains are found to concentrate heavily in a small early-holder/mining-pool set, gain_flow should be re-authored to name that seat directly rather than diffuse, which would sharpen the classification toward snare for that concentrated seat while remaining tangled_rope in aggregate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concentration_vs_diffuse_claim, empirical, 'Whether the diffuse gain_flow designation understates beneficiary concentration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 9, 0.34).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 16, 0.42).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bitc_be_t3, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(bitc_be_t6, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 6, 0.53).
narrative_ontology:measurement(bitc_be_t9, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(bitc_be_t16, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 16, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(bitc_su_t3, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(bitc_su_t6, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(bitc_su_t9, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 9, 0.64).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(bitc_su_t16, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 16, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__maximalist_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language label 'the Bitcoin whitepaper's meaning' / 'the Bitcoin consensus kernel' per the ε-invariance principle. Each reading (maximalist_reading, utility_reading, pragmatic_synthesis) has its own ε, its own beneficiary/victim structure, and its own claimed_type — they are not measurement-basis variants of one constraint. maximalist_reading forecloses part of utility_reading's action space (any change framed as covenant violation is blocked regardless of the utility reading's iterative-improvement premise) while coexisting with and partially influencing pragmatic_synthesis (which accepts base-layer immutability but rejects the maximalist extension of immutability rhetoric to upper-layer innovation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
