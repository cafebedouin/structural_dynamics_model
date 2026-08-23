% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__utility_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__utility_reading
 *   human_readable: Bitcoin Consensus Kernel — Utility Reading (Iterative Improvement)
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   The Bitcoin whitepaper (2008) describes a peer-to-peer electronic cash
 *   system built on a proof-of-work consensus mechanism. The utility reading
 *   interprets this as establishing a *minimum viable consensus mechanism* —
 *   a functional baseline that enables iterative improvement through soft
 *   forks, layered protocols (Lightning, sidechains), and protocol research.
 *   This reading sees the consensus mechanism as a coordination substrate: it
 *   solves the double-spend problem and provides a credible neutral
 *   settlement layer, but its rules are not sacrosanct. Beneficiaries are
 *   adopters, builders, and researchers who gain from evolving functionality.
 *   Victims are those who invested in or ideologically committed to monetary
 *   ossification guarantees (fixed supply, immutable rules) and experience
 *   the constraint's evolution as a violation of the founding covenant. The
 *   constraint requires active enforcement (miners, node operators) to
 *   maintain consensus; suppression is moderate (alternative chains exist but
 *   face network-effect barriers). Extraction accumulates over time as the
 *   protocol ossifies in some dimensions (base layer monetary rules) while
 *   innovating in others (layer 2), creating a tension captured by the
 *   tangled_rope claim.
 *
 * KEY AGENTS:
 *   - adopters_builders: Primary beneficiary (organized/biographical) — gain from protocol improvements and layer-2 innovation
 *   - layer2_developers: Primary beneficiary (organized/biographical) — build on the consensus substrate
 *   - protocol_researchers: Primary beneficiary (organized/biographical) — advance the consensus mechanism
 *   - monetary_ossification_proponents: Primary victim (organized/biographical) — bear cost of rule changes that threaten immutability guarantees
 *   - immutability_maximalists: Primary victim (organized/biographical) — ideological commitment to unchanging rules
 *   - miners: Agenda setter (institutional/biographical) — enforce consensus rules, collect block rewards and fees
 *   - node_operators: Agenda setter (organized/biographical) — validate and propagate blocks, set consensus parameters
 *   - general_users: Beneficiary/payer (moderate/biographical) — use the system for transactions, pay fees, benefit from security
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.45).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.3).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel — Utility Reading (Iterative Improvement)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '226969e4-c1e5-4c6f-af3f-a9c45c1b9de5').
narrative_ontology:cs_kernel_codification('226969e4-c1e5-4c6f-af3f-a9c45c1b9de5', fixed_text).
narrative_ontology:cs_authority_grounding('226969e4-c1e5-4c6f-af3f-a9c45c1b9de5', lineage).
narrative_ontology:cs_interpretation_layer_present('226969e4-c1e5-4c6f-af3f-a9c45c1b9de5').
narrative_ontology:cs_reading_relation('226969e4-c1e5-4c6f-af3f-a9c45c1b9de5', bitcoin_consensus_kernel__maximalist_reading, forecloses).
narrative_ontology:cs_reading_relation('226969e4-c1e5-4c6f-af3f-a9c45c1b9de5', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('226969e4-c1e5-4c6f-af3f-a9c45c1b9de5', foundational, consensus_mechanism_is_iteratively_improvable).
narrative_ontology:cs_axiom_status(consensus_mechanism_is_iteratively_improvable, holdable).
narrative_ontology:cs_axiom_grounding('226969e4-c1e5-4c6f-af3f-a9c45c1b9de5', consensus_mechanism_is_iteratively_improvable, conventional).
narrative_ontology:cs_axiom('226969e4-c1e5-4c6f-af3f-a9c45c1b9de5', foundational, monetary_ossification_is_a_cost_not_a_feature).
narrative_ontology:cs_axiom_status(monetary_ossification_is_a_cost_not_a_feature, holdable).
narrative_ontology:cs_axiom_grounding('226969e4-c1e5-4c6f-af3f-a9c45c1b9de5', monetary_ossification_is_a_cost_not_a_feature, instrumental).
narrative_ontology:cs_reference_frame('226969e4-c1e5-4c6f-af3f-a9c45c1b9de5', whitepaper_minimum_viable_consensus).
narrative_ontology:cs_drift_state('226969e4-c1e5-4c6f-af3f-a9c45c1b9de5', post_taproot_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('226969e4-c1e5-4c6f-af3f-a9c45c1b9de5', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, adopters_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer2_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, protocol_researchers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, monetary_ossification_proponents).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, immutability_maximalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, general_users).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, general_users).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, iterative_improvement_of_consensus).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, minimum_viable_consensus_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developers, entrepreneurs, and users who adopt Bitcoin and build applications, layer-2 protocols, and tools on top. They benefit from protocol improvements (SegWit, Taproot, Lightning) that expand functionality. Exit is constrained by network effects and sunk investment in the ecosystem, but they can migrate to other chains or layer-2s.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, adopters_builders, beneficiary,
    organized, biographical, constrained, global).

% Teams building Lightning, sidechains, statechains, and other layer-2 protocols that rely on Bitcoin's base consensus. They benefit from a stable but evolvable base layer. Their exit is constrained by the specificity of their constructions to Bitcoin's script and consensus rules.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer2_developers, beneficiary,
    organized, biographical, constrained, global).

% Academic and independent researchers studying consensus mechanisms, cryptoeconomics, and protocol design. They gain from the living laboratory of an evolving consensus protocol. Exit is relatively mobile — they can study other systems — but Bitcoin's dominance makes it a primary research target.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_researchers, beneficiary,
    organized, biographical, mobile, global).

% Investors, ideologues, and institutions who treat Bitcoin's fixed supply and immutable rules as a monetary guarantee. They experience protocol changes (even soft forks) as a violation of the social contract that gives Bitcoin its value proposition. Their exit is identity-locked: leaving would mean abandoning the 'digital gold' narrative they've built wealth and identity around.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, monetary_ossification_proponents, payer,
    organized, generational, identity_locked, global).

% Community members who believe any change to the consensus rules — including soft forks — is illegitimate. They bear the cost of seeing their preferred invariant violated. Exit is identity-locked because their participation is fused with the belief in absolute immutability; forking away (e.g., to Bitcoin Cash) already happened, but the remaining maximalists are locked into the main chain's evolution.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, immutability_maximalists, payer,
    organized, generational, identity_locked, global).

% Entities that secure the network via proof-of-work and enforce consensus rules. They set the agenda by signaling support for upgrades (BIP9, Speedy Trial) and can veto changes by not activating them. They collect block rewards and fees. Exit is arbitrage-grade: they can switch hashpower to other SHA-256 chains instantly based on profitability.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, miners, agenda_setter,
    institutional, biographical, arbitrage, global).

% Individuals and organizations running full nodes that validate and propagate blocks. They enforce consensus rules by choosing which software to run. They set the agenda by adopting or rejecting upgrades. Exit is mobile: they can switch to alternative clients or chains, but the cost of running a node is low and the social consensus matters.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, node_operators, agenda_setter,
    organized, biographical, mobile, global).

% Everyday users who transact on Bitcoin (on-chain or via Lightning). They benefit from censorship resistance and store-of-value properties. They pay fees (extraction) and face inflation risk if monetary rules change. Exit is constrained by liquidity, exchange access, and the network effect of Bitcoin's brand.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, general_users, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, general_users, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__utility_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__utility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a credible neutral settlement layer that solves the double-spend problem without trusted third parties, enabling a shared ledger across adversarial participants.
% TRANSFER_FUNCTION: Moves seigniorage (block rewards) and transaction fees from users and holders to miners; moves option value from ossification proponents (who lose immutability guarantees) to builders (who gain new functionality via protocol changes).
% ABSENT_VOICES: Future generations who will inherit the monetary system; regulators who are excluded from the consensus process but impose external constraints; non-technical holders who cannot participate in BIP discussions or signaling.
% DISAPPEARANCE_RATIONALE: If the consensus mechanism vanished overnight, the $1T+ network would fracture: miners would redirect hashpower, nodes would stall, users would lose settlement finality, and the monetary premium would collapse. Competing chains would capture fragments, but the specific coordination achievement — a globally recognized, censorship-resistant ledger with a known emission schedule — would be lost and not instantly reconstituted.
% FOUNDING_PROBLEM: Creating a censorship-resistant digital cash system without trusted third parties, solving the double-spend problem in a decentralized network.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the whitepaper itself, early mailing list archives (Satoshi, Finney, Wei Dai), and subsequent academic literature on Byzantine fault tolerance and cryptoeconomics. The status 'live' is corroborated by ongoing censorship events (e.g., Canadian trucker protests, Tornado Cash sanctions) where Bitcoin's censorship resistance is actively valued. No single beneficiary group monopolizes this attestation.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__utility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__utility_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__utility_reading_tests).
:- end_tests(bitcoin_consensus_kernel__utility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.45 reflects moderate but rising extraction: early years (T=0-5) the mechanism was close to minimum viable with low extraction; as the network grew and layer-2 protocols emerged, the base layer's monetary ossification guarantees became a contested resource — changes (SegWit, Taproot) extracted value from ossification proponents while subsidizing builders. Suppression 0.3 reflects that alternatives (other chains, forks) exist but face massive network-effect barriers; the constraint does not actively suppress exits but the cost of exit is high. Theater ratio 0.2 captures performative adherence to 'Satoshi's vision' by maximalists while the protocol evolves via BIP process. Accessibility collapse 0.4: the whitepaper's design space is partially collapsed by network effects, but layer-2 and sidechains reopen alternatives. Resistance 0.5: block size wars and activation controversies show real resistance to changes. The claimed_type tangled_rope captures the dual nature: genuine coordination (consensus on a shared ledger) + asymmetric extraction (ossification proponents pay for builders' innovation).
 *
 * PERSPECTIVAL GAP:
 *   From the adopter/builder seat, the constraint is a rope: a coordination mechanism that enables innovation and they voluntarily participate. From the ossification proponent seat, it is a snare: the promise of immutable monetary rules is violated by iterative changes they cannot prevent. From the miner/node operator seat, it is a scaffold: they maintain the mechanism with an implicit sunset (the transition to a fee-only security model). The engine computes these per-seat classifications from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (adopters_builders, layer2_developers, protocol_researchers) gain from the constraint's evolution — they have low directionality (d near 0). Victims (monetary_ossification_proponents, immutability_maximalists) bear the cost of rule changes that erode their guarantees — high directionality (d near 1). Miners and node operators are agenda_setters with institutional/organized power; they set the rules but also depend on the system's legitimacy — directionality near symmetric (d ~0.5). General users are dual-role (beneficiary/payer) with moderate power and biographical horizon — directionality slightly positive (d ~0.6) because they pay fees and face inflation risk but gain utility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (per R5) was creating a censorship-resistant digital cash without trusted third parties. That problem is live (censorship resistance remains valuable) but the specific solution (fixed monetary policy, immutable base layer) is contested. The utility reading argues the mandate has *not* atrophied — the consensus mechanism continues to solve the coordination problem and enables new solutions. The maximalist reading argues the mandate *has* atrophied — the original covenant is broken. The classification as tangled_rope (not piton) reflects that the constraint still performs its coordination function and is actively maintained, not merely performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_structure,
    'How does the utility reading''s structural relationship to the bitcoin_consensus_kernel differ from its sibling readings, and what classification consequences follow?',
    'Comparative analysis of the three declared readings (maximalist_reading, pragmatic_synthesis, utility_reading) on the same kernel, mapping each reading''s beneficiary/victim structure, extraction profile, and coordination claims to the engine''s classification logic.',
    'If the utility reading''s moderate extractiveness and coordination function are confirmed, it classifies as tangled_rope; if extraction is lower and coordination dominates, it may compute as rope; if extraction is higher with suppression, it may compute as snare. The sibling readings provide counterfactual baselines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_structure, conceptual, 'Committer-frame structural delta between utility_reading and its sibling readings on the bitcoin_consensus_kernel.').

omega_variable(
    monetary_ossification_as_victim_ambiguity,
    'Are monetary ossification guarantees a coherent victim class, or do they represent a preference for a specific property (immutability) that the utility reading treats as a cost?',
    'Empirical survey of holder behavior: do proponents of monetary ossification incur measurable costs when the consensus mechanism evolves (e.g., via soft forks), or do they merely express ideological opposition? Track capital flows and network participation before/after consensus changes.',
    'If ossification proponents bear measurable costs (lost optionality, opportunity cost, forced exit), they are structural victims and the constraint is extractive toward them. If they only express preference without structural cost, the extraction claim weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monetary_ossification_as_victim_ambiguity, empirical, 'Whether ''monetary ossification guarantees'' constitute a victim class with structural costs or a preference cohort.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_util_tr_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(btc_util_tr_t0, observed).
narrative_ontology:measurement(btc_util_tr_t5, bitcoin_consensus_kernel__utility_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(btc_util_tr_t5, observed).
narrative_ontology:measurement(btc_util_tr_t10, bitcoin_consensus_kernel__utility_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(btc_util_tr_t10, observed).
narrative_ontology:measurement(btc_util_tr_t15, bitcoin_consensus_kernel__utility_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(btc_util_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(btc_util_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(btc_util_be_t0, observed).
narrative_ontology:measurement(btc_util_be_t5, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 5, 0.25).
narrative_ontology:measurement_basis(btc_util_be_t5, observed).
narrative_ontology:measurement(btc_util_be_t10, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(btc_util_be_t10, observed).
narrative_ontology:measurement(btc_util_be_t15, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(btc_util_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(btc_util_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(btc_util_su_t0, observed).
narrative_ontology:measurement(btc_util_su_t5, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement_basis(btc_util_su_t5, observed).
narrative_ontology:measurement(btc_util_su_t10, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement_basis(btc_util_su_t10, observed).
narrative_ontology:measurement(btc_util_su_t15, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement_basis(btc_util_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__utility_reading, 0.1).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% The bitcoin_consensus_kernel decomposes into three readings with distinct ε profiles: maximalist_reading (ε≈0.1, mountain/rope), pragmatic_synthesis (ε≈0.3, rope/tangled_rope), utility_reading (ε≈0.45, tangled_rope). They share the same whitepaper kernel but instantiate different constraints with different beneficiary/victim structures and extraction levels. The utility reading's moderate extraction and coordination function make it the most extractive of the three, reflecting its permissive stance on base-layer changes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__utility_reading, institutional, 0.35).
constraint_indexing:directionality_override(bitcoin_consensus_kernel__utility_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
