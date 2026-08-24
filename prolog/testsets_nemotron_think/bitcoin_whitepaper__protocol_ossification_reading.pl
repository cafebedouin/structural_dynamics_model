% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__protocol_ossification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Bitcoin Protocol Ossification Norm
 *   domain: cryptocurrency/technology_governance
 *
 * SUMMARY:
 *   The protocol ossification reading of the Bitcoin whitepaper asserts that
 *   protocol changes are illegitimate unless approaching universal consensus,
 *   positioning stability as the primary virtue. This reading emerged from
 *   the block size wars (2015-2017) and solidified into a governance norm
 *   where any base-layer change requires overwhelming social consensus —
 *   practically an unachievable threshold. The constraint coordinates
 *   monetary credibility (genuine coordination function) while extracting
 *   option value from use cases requiring base-layer innovation (asymmetric
 *   extraction). Enforcement is active: social pressure, developer
 *   ostracization, miner signaling norms, and node operator veto power
 *   maintain the status quo. The claimed type is tangled_rope — genuine
 *   coordination (credible immutability) combined with extraction (blocking
 *   p2p cash, privacy, scaling).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper__protocol_ossification_reading, 0.78).
domain_priors:theater_ratio(bitcoin_whitepaper__protocol_ossification_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__protocol_ossification_reading, "Bitcoin Protocol Ossification Norm").
narrative_ontology:topic_domain(bitcoin_whitepaper__protocol_ossification_reading, "cryptocurrency/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__protocol_ossification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__protocol_ossification_reading, 'e11d9ab2-7531-4e27-90c9-810f810898de').
narrative_ontology:cs_kernel_codification('e11d9ab2-7531-4e27-90c9-810f810898de', fixed_text).
narrative_ontology:cs_authority_grounding('e11d9ab2-7531-4e27-90c9-810f810898de', lineage).
narrative_ontology:cs_interpretation_layer_present('e11d9ab2-7531-4e27-90c9-810f810898de').
narrative_ontology:cs_reading_relation('e11d9ab2-7531-4e27-90c9-810f810898de', bitcoin_whitepaper__p2p_cash_reading, forecloses).
narrative_ontology:cs_reading_relation('e11d9ab2-7531-4e27-90c9-810f810898de', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_axiom('e11d9ab2-7531-4e27-90c9-810f810898de', foundational, protocol_immutability_above_all).
narrative_ontology:cs_axiom_status(protocol_immutability_above_all, holdable).
narrative_ontology:cs_axiom_grounding('e11d9ab2-7531-4e27-90c9-810f810898de', protocol_immutability_above_all, deontological).
narrative_ontology:cs_axiom('e11d9ab2-7531-4e27-90c9-810f810898de', foundational, social_consensus_as_sovereign).
narrative_ontology:cs_axiom_status(social_consensus_as_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('e11d9ab2-7531-4e27-90c9-810f810898de', social_consensus_as_sovereign, conventional).
narrative_ontology:cs_reference_frame('e11d9ab2-7531-4e27-90c9-810f810898de', satoshi_immutable_protocol_vision).
narrative_ontology:cs_drift_state('e11d9ab2-7531-4e27-90c9-810f810898de', contemporary_layer2_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e11d9ab2-7531-4e27-90c9-810f810898de', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, institutional_investors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, store_of_value_advocates).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, base_layer_innovators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, p2p_cash_use_cases).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, privacy_enhancement_proposals).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, global_south_payment_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, core_developers).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, ossified_protocol_stability).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, social_consensus_governance).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, immutability_as_monetary_virtue).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as a store of value. The ossification norm guarantees the 21M cap and monetary policy cannot be changed, protecting their wealth from inflationary debasement. They can exit to other assets but choose bitcoin precisely for its credible immutability.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Allocate capital to bitcoin based on regulatory clarity and predictable protocol rules. Ossification provides the legal and compliance stability needed for institutional adoption. They benefit from the constraint's suppression of protocol risk.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, institutional_investors, beneficiary,
    institutional, biographical, mobile, global).

% Ideologically committed to bitcoin as digital gold. They actively promote the ossification narrative, police discourse against base-layer changes, and view any protocol modification as existential betrayal. Their identity is fused with the 'immutability' framing; exit means abandoning their intellectual framework.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, store_of_value_advocates, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, store_of_value_advocates, agenda_setter).

% Maintain the reference implementation. They are formally the agenda-setters for code changes but practically constrained by the ossification norm — proposing controversial changes risks reputation, funding, and social ostracization. They bear the cost of foregone innovation and the burden of maintaining compatibility with frozen primitives.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, core_developers, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, core_developers, payer).

% Developers with proposals requiring base protocol changes (e.g., covenants, drivechains, signature aggregation). The ossification norm blocks their work unless they achieve near-universal consensus — practically impossible. They either abandon proposals, move to altcoins, or build complex layer-2 workarounds.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, base_layer_innovators, payer,
    moderate, biographical, constrained, global).

% Users needing cheap, fast base-layer transactions for daily commerce — especially in high-inflation economies. Base layer fees and throughput constraints make bitcoin unusable for small payments. They cannot influence the consensus process and lack capital to exit to custodial alternatives.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, p2p_cash_use_cases, payer,
    powerless, immediate, trapped, global).

% Advocates for base-layer privacy improvements (e.g., confidential transactions, CoinJoin at protocol level). Ossification prevents these changes; privacy must be built on higher layers with inferior trust models. They bear surveillance costs that protocol-level privacy would eliminate.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, privacy_enhancement_proposals, payer,
    moderate, biographical, constrained, global).

% Users in Argentina, Nigeria, Turkey, etc. who need bitcoin for payments, not just savings. They pay high fees and suffer slow confirmations because base-layer scaling is blocked. They have no voice in the consensus process dominated by wealthy holders and institutional actors.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, global_south_payment_users, payer,
    powerless, immediate, trapped, global).

% Secure the network and signal support for protocol changes. They appear powerful but are constrained by the ossification norm — signaling for controversial changes risks chain splits and revenue loss. They enforce the status quo because deviating is economically suicidal.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, miners, agenda_setter,
    institutional, biographical, constrained, global).

% Run validating nodes and enforce consensus rules. They collectively veto protocol changes by refusing to upgrade. The ossification norm empowers them: running current software is the default, and coordination for change requires overwhelming social consensus they help define.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, node_operators, agenda_setter,
    organized, biographical, mobile, global).

% Altcoins and layer-1s (Ethereum, Solana, etc.) that offer base-layer programmability and scaling. They would compete for the use cases blocked by bitcoin's ossification but are structurally excluded from bitcoin's governance. Their existence is the exit option for frustrated users.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, competing_protocols, excluded,
    powerful, biographical, arbitrage, global).

% Researchers studying bitcoin's governance, monetary economics, and protocol evolution. They analyze the ossification norm's effects on innovation, adoption, and monetary credibility without participating in the consensus process.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, academic_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides credible monetary stability and predictability by making protocol changes extremely difficult, solving the coordination problem of trust in monetary policy immutability — holders, institutions, and applications can build on bitcoin without fear of inflationary or structural changes.
% TRANSFER_FUNCTION: Moves option value for base-layer innovation from developers and use-cases requiring protocol changes (p2p cash, privacy, scaling) to holders and institutions who value stability above all. The constraint extracts the ability to improve the base protocol and transfers that optionality to the status quo.
% ABSENT_VOICES: Users in high-inflation economies needing cheap payments (global_south_payment_users), privacy advocates needing base-layer fungibility (privacy_enhancement_proposals), developers with scaling and functionality proposals (base_layer_innovators) — they are excluded from the consensus process because the 'universal consensus' threshold effectively requires only the agreement of those who benefit from the status quo.
% DISAPPEARANCE_RATIONALE: If the ossification norm vanished overnight, base-layer development would resume actively: covenants, drivechains, privacy improvements, and scaling proposals would be seriously debated and potentially activated. Hard forks would proliferate as different factions pursue incompatible visions. The credible commitment to 21M cap and fixed rules would be contested, potentially undermining bitcoin's store-of-value narrative. The monetary system would reorganize around active governance rather than immutable rules.
% FOUNDING_PROBLEM: Early Bitcoin faced protocol instability risk from contentious hard forks (block size wars, SegWit2x). The ossification norm emerged to cement the 21M cap and core consensus rules as immutable, solving the coordination problem of convincing the world that bitcoin's monetary policy is genuinely unchangeable.
% FOUNDING_PROBLEM_CORROBORATION: Original cypherpunk mailing list archives and Satoshi's writings show explicit discussion of ossification as a desirable end state; but Lightning Network developers, global south users, and privacy researchers attest the founding problem (credible immutability) is substantially solved and the norm now blocks necessary evolution. No single party outside the beneficiary set corroborates that the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__protocol_ossification_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__protocol_ossification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__protocol_ossification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness (0.68) is substantial because the norm blocks economically valuable innovations (scaling, privacy, covenants) and transfers their option value to holders. Suppression (0.78) is high because the constraint's persistence depends on active social enforcement — developer reputation systems, miner signaling conventions, node operator coordination — not passive preference. Theater ratio (0.32) is moderate: the 'stability' narrative is genuinely believed by many, but a growing share of enforcement activity defends the norm itself rather than any functional necessity. Accessibility collapse (0.52) is partial: layer-2 solutions (Lightning, Liquid, Ark) provide some alternatives, but they inherit base-layer limitations and add trust assumptions. Resistance (0.42) is moderate: developers propose changes regularly but face near-insurmountable social consensus barriers.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (holders, institutions), the constraint is a Mountain — a natural law of credible monetary immutability. From the payer seats (innovators, global south users, privacy advocates), it is a Snare — active suppression of alternatives with no voice in governance. From the agenda-setter seats (core devs, miners, nodes), it is a Tangled Rope — they coordinate the enforcement but also pay the cost of foregone innovation. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and institutions are structural beneficiaries (d near 0.0) — the constraint subsidizes their store-of-value use case by credibly eliminating protocol risk. Base-layer innovators, p2p cash users, privacy advocates, and global south users are structural targets (d near 1.0) — they bear the full cost of frozen primitives with constrained or trapped exit. Core developers and miners sit near symmetric (d ~0.5): they administer the constraint but are also constrained by it. Node operators are agenda-setters with mobile exit (they can run alternative software) but identity-locked to the 'Bitcoin' brand. The ossification norm's 'universal consensus' threshold structurally weights the consensus process toward those who benefit from the status quo.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible immutability) is contested: beneficiaries claim it requires permanent ossification; payers argue it was solved years ago and the norm now serves as extraction. The mandate has not been formally resolved — no sunset clause exists, and the 'universal consensus' threshold makes resolution structurally impossible. This is a classic mandatrophy trap: the arrangement that solved the founding problem now prevents its own revision even as the problem shifts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_vs_constructed_barrier,
    'Is base-layer ossification a genuine coordination necessity for credible monetary immutability, or a constructed barrier protecting incumbent holder interests?',
    'Counterfactual analysis: if a credible commitment mechanism existed that allowed safe, opt-in base-layer upgrades (e.g., soft-fork-only covenants with lengthy activation), would holders still reject all changes? Natural experiment from altcoins with active governance (Ethereum, Tezos) — do they suffer monetary credibility loss?',
    'If ossification is necessary, the extraction is the price of coordination (tangled_rope holds). If constructed, the constraint is a snare using ''stability'' as cover for rent protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_vs_constructed_barrier, conceptual, 'Whether the ossification norm''s coordination function is structurally necessary or contingently constructed.').

omega_variable(
    layer2_substitution_adequacy,
    'Do higher-layer solutions (Lightning, Ark, Liquid, Fedimint) adequately substitute for the base-layer changes blocked by ossification?',
    'Empirical measurement of layer-2 adoption, trust assumptions, and functional coverage relative to base-layer proposals. User studies in global south contexts comparing custodial vs. self-custodial layer-2 experiences.',
    'If layer-2 adequately substitutes, extraction is lower (victims have functional exits). If substitution fails for key use cases (small payments, privacy, self-custody), extraction is higher and victims are genuinely trapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(layer2_substitution_adequacy, empirical, 'Whether higher-layer innovation absorbs the demand that base-layer ossification blocks.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of base-layer innovation structural (miner/node power, social consensus thresholds) or internalized (developer self-censorship, identity fusion with ''Bitcoin maximalism'')?',
    'Post-proposal tracking: when developers propose changes and face social backlash, do they abandon proposals due to external barriers (consensus impossible) or internal barriers (reputation, identity)? Survey of departed bitcoin developers.',
    'If internalized, effective suppression is higher than structural measures suggest — developers carry the constraint with them. If structural, exit to altcoins is a cleaner break.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in bitcoin''s governance.').

omega_variable(
    kernel_reading_identity,
    'Does this reading''s commitment to protocol_immutability_above_all logically foreclose the p2p_cash_reading, or do they merely compete for mindshare?',
    'Formal analysis of whether a single protocol can simultaneously satisfy ''changes require universal consensus'' and ''base layer must scale for global payments'' — if mathematically incompatible, forecloses; if compatible via layering, coexists_with.',
    'Forecloses means the kernel cannot hold both readings in one framework — bitcoin must choose. Coexists_with means both remain live positions across different factions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural relationship between protocol_ossification_reading and p2p_cash_reading within the bitcoin_whitepaper kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__protocol_ossification_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_ossification_tr_t0, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(btc_ossification_tr_t3, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 3, 0.15).
narrative_ontology:measurement(btc_ossification_tr_t6, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(btc_ossification_tr_t9, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 9, 0.27).
narrative_ontology:measurement(btc_ossification_tr_t12, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(btc_ossification_tr_t15, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 15, 0.32).

% Extraction over time
narrative_ontology:measurement(btc_ossification_be_t0, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(btc_ossification_be_t3, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(btc_ossification_be_t6, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(btc_ossification_be_t9, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 9, 0.58).
narrative_ontology:measurement(btc_ossification_be_t12, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(btc_ossification_be_t15, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 15, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(btc_ossification_su_t0, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(btc_ossification_su_t3, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(btc_ossification_su_t6, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(btc_ossification_su_t9, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 9, 0.72).
narrative_ontology:measurement(btc_ossification_su_t12, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(btc_ossification_su_t15, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 15, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__protocol_ossification_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__protocol_ossification_reading, 0.08).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, lightning_network_scaling).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_covenant_proposals).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_privacy_proposals).

% DUAL FORMULATION NOTE:
% Part of the bitcoin_whitepaper constraint family. This reading (protocol_ossification) treats the whitepaper as a fixed kernel requiring absolute stability. The digital_gold_reading treats it as a foundation for store-of-value optimizations. The p2p_cash_reading treats it as a starting point for electronic cash evolution. The three readings have divergent ε values and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__protocol_ossification_reading, institutional, 0.15).
constraint_indexing:directionality_override(bitcoin_whitepaper__protocol_ossification_reading, organized, 0.45).
constraint_indexing:directionality_override(bitcoin_whitepaper__protocol_ossification_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
