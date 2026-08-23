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
 *   constraint_id: bitcoin_consensus_kernel__utility_reading
 *   human_readable: Bitcoin Consensus Kernel — Utility Reading (Iterative Improvement)
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint story captures the 'utility reading' of the Bitcoin
 *   consensus kernel: the whitepaper established a minimum viable consensus
 *   mechanism (Nakamoto consensus + difficulty adjustment + script) that
 *   *enables* iterative improvement via soft forks and layer-2 protocols. The
 *   reading treats the kernel as a foundation for evolution, not a frozen
 *   covenant. Beneficiaries are developers, builders, and adopters who gain
 *   from protocol upgrades; victims are the monetary ossification guarantees
 *   and identity-locked purists whose credibility erodes with each change.
 *   The constraint is a tangled rope: genuine coordination (safe upgrade
 *   pathway) + asymmetric extraction (immutability guarantee degraded for
 *   those who relied on it). Active enforcement: the social consensus process
 *   (BIP → activation → enforcement) must be actively maintained; if it
 *   atrophies, the network either ossifies (piton) or fractures (snare).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.42).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.28).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel — Utility Reading (Iterative Improvement)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '51356035-da11-4c41-8fc6-ba94d6bcb5c8').
narrative_ontology:cs_kernel_codification('51356035-da11-4c41-8fc6-ba94d6bcb5c8', fixed_text).
narrative_ontology:cs_authority_grounding('51356035-da11-4c41-8fc6-ba94d6bcb5c8', lineage).
narrative_ontology:cs_interpretation_layer_present('51356035-da11-4c41-8fc6-ba94d6bcb5c8').
narrative_ontology:cs_reading_relation('51356035-da11-4c41-8fc6-ba94d6bcb5c8', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('51356035-da11-4c41-8fc6-ba94d6bcb5c8', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('51356035-da11-4c41-8fc6-ba94d6bcb5c8', foundational, consensus_mechanism_enables_safe_evolution).
narrative_ontology:cs_axiom_status(consensus_mechanism_enables_safe_evolution, holdable).
narrative_ontology:cs_axiom_grounding('51356035-da11-4c41-8fc6-ba94d6bcb5c8', consensus_mechanism_enables_safe_evolution, instrumental).
narrative_ontology:cs_axiom('51356035-da11-4c41-8fc6-ba94d6bcb5c8', foundational, soft_fork_upgradability_preserves_value).
narrative_ontology:cs_axiom_status(soft_fork_upgradability_preserves_value, holdable).
narrative_ontology:cs_axiom_grounding('51356035-da11-4c41-8fc6-ba94d6bcb5c8', soft_fork_upgradability_preserves_value, empirically_contingent).
narrative_ontology:cs_reference_frame('51356035-da11-4c41-8fc6-ba94d6bcb5c8', nakamoto_consensus_minimum_viable).
narrative_ontology:cs_drift_state('51356035-da11-4c41-8fc6-ba94d6bcb5c8', post_taproot_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('51356035-da11-4c41-8fc6-ba94d6bcb5c8', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, protocol_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer2_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, institutional_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, exchange_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, monetary_ossification_guarantees).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, immutability_purists).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, hodl_faithful).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, hodl_faithful).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, miners_and_node_operators).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, minimum_viable_consensus_sufficient).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, iterative_improvement_preserves_value).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, soft_fork_upgradability_as_feature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the reference implementation and propose protocol changes through BIP process. Their authority derives from code contribution and community trust, not formal mandate. They can propose soft forks but cannot force adoption; must build consensus among miners, node operators, and users. Exit means abandoning the project, which carries reputation and career cost.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_developers, agenda_setter,
    organized, biographical, constrained, global).

% Build scaling and functionality layers (Lightning, RGB, Ark, etc.) that depend on base-layer scriptability and malleability fixes. They benefit from soft-fork-enabled opcodes and consensus improvements. Their exit is relatively easy — they can shift to other chains — but their capital and user base are Bitcoin-native.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer2_builders, beneficiary,
    moderate, biographical, mobile, global).

% Enter Bitcoin as treasury asset, collateral, or settlement layer. They benefit from protocol improvements that enhance compliance, custody, or throughput. They hold significant capital and can allocate across chains; their exit is portfolio rebalancing, not ideological.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, institutional_adopters, beneficiary,
    institutional, biographical, arbitrage, global).

% Operate trading venues and custodial services. Benefit from protocol upgrades that improve settlement finality, reduce reorg risk, or enable new products. Their business model adapts to protocol changes; they have no ideological lock-in.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, exchange_operators, beneficiary,
    institutional, biographical, arbitrage, global).

% Not a human agent but a structural property: the guarantee that the monetary rules (21M cap, emission schedule, consensus rules) will never change. This guarantee is eroded each time a soft fork activates, because it demonstrates that the social layer *can* coordinate changes. The 'victim' is the credibility of the immutability claim itself. No exit — the guarantee either holds or it doesn't.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, monetary_ossification_guarantees, payer,
    powerless, civilizational, identity_locked, universal).

% Participants whose core thesis is that Bitcoin's value proposition *is* its unchanging rules. They view every soft fork as a precedent that weakens the 'never changes' narrative. They cannot exit the constraint without abandoning their intellectual framework; their identity is fused with the immutability claim. They resist changes through social signaling, node operation, and narrative enforcement.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, immutability_purists, payer,
    organized, generational, identity_locked, global).

% Long-term holders who benefit from protocol improvements that increase adoption and price, but pay the cost of uncertainty about whether changes undermine the monetary properties they hold for. Their exit is selling — but that realizes the very risk they fear. They are caught between wanting Bitcoin to succeed and fearing success requires changes that break the promise.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, hodl_faithful, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, hodl_faithful, beneficiary).

% Enforce consensus rules by choosing which software to run. They bear the cost of upgrades (testing, coordination, risk of chain splits) and the cost of *not* upgrading (missing efficiency gains, losing to competitors). They can exit by switching chains or selling hardware, but Bitcoin-specific capital (ASICs) creates lock-in. Their signaling activates soft forks.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, miners_and_node_operators, agenda_setter,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, miners_and_node_operators, payer).

% Study consensus dynamics, incentive compatibility, and governance outcomes. They have no stake in protocol outcomes beyond analytical reputation. Their role is to model the constraint's behavior under different assumptions.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, academic_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how a decentralized network can safely evolve its consensus rules without fragmentation or capture. The whitepaper's minimal mechanism (Nakamoto consensus + difficulty adjustment + script opcodes) provides a foundation that *can* be extended via soft forks — a coordination protocol for protocol upgrades themselves.
% TRANSFER_FUNCTION: Moves decision-making authority over protocol evolution from 'no one can change anything' (maximalist ideal) to 'organized developers propose, miners signal, users validate, economic majority adopts' — transferring change-control from an immutable covenant to a live, multi-stakeholder governance process. The extractive transfer is the erosion of the 'immutable monetary policy' guarantee that early adopters relied on.
% ABSENT_VOICES: Future generations who will inherit the monetary system shaped by today's upgrade decisions; they cannot participate in current signaling. Also excluded: the 'immutability guarantee' itself as a structural property — it has no voice, only credibility that degrades with each activated change.
% DISAPPEARANCE_RATIONALE: If the iterative-improvement reading vanished overnight — i.e., if the community agreed that no further soft forks are legitimate — Bitcoin would ossify at current capability. Layer-2 innovation would stall at current opcodes; institutional adoption would face harder compliance ceilings; the 'Bitcoin as programmable settlement' narrative would collapse to 'Bitcoin as static gold.' The world rearranges because the *expectation of future improvement* is priced into current adoption and development.
% FOUNDING_PROBLEM: The whitepaper established a consensus mechanism that works *minimally* — it solves double-spend and emission but leaves scaling, privacy, scripting expressiveness, and upgradeability as unsolved. The founding problem is: how to improve the system without breaking the consensus that makes it valuable?
% FOUNDING_PROBLEM_CORROBORATION: Protocol developers (BIP authors, Core maintainers) attest the problem is live — scaling and script improvements remain active research. Layer-2 builders corroborate: their roadmaps depend on future base-layer opcodes. Independent academic research (e.g., on covenant opcodes, drivechains, cluster mempool) treats the problem as unsolved. No beneficiary group claims the problem is solved; the maximalist reading claims it *should not be solved*, which is a different position.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(bitcoin_consensus_kernel__utility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__utility_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) is moderate: the utility reading extracts credibility from the immutability guarantee (victim) to fund protocol evolution (beneficiaries). Not high because the extraction is diffuse (no single agent captures it) and the coordination function is real — soft forks have delivered SegWit, Taproot, Lightning-enabling opcodes. Suppression (0.28) is low-moderate: dissenters can run old nodes, sell coins, or fork (BCH, BSV) — exits exist but are costly for identity-locked agents. Theater (0.15) is low: the upgrade process is genuinely used, not performative. Accessibility collapse (0.45) reflects that alternatives (other chains, ossified Bitcoin) exist but carry high switching costs for Bitcoin-native capital. Resistance (0.55) is moderate: maximalist opposition is organized but has not blocked recent soft forks (Taproot activated).
 *
 * PERSPECTIVAL GAP:
 *   From the utility reading's seat, the constraint is a rope — a coordination mechanism for safe evolution. From the maximalist reading's seat, the *same mechanism* is a snare — extracting the immutability guarantee under cover of 'improvement.' The engine will compute different χ for each stakeholder seat; the divergence *is* the kernel contest. This story authors the utility reading's ε and structure; the maximalist reading is a separate constraint story with its own ε.
 *
 * DIRECTIONALITY LOGIC:
 *   Protocol developers and miners are agenda-setters (d ~ 0.3–0.4): they shape upgrades but must build consensus. Layer-2 builders, institutions, exchanges are beneficiaries (d ~ 0.1–0.2): they gain from improvements, exit is mobile/arbitrage. Immutability purists and monetary ossification guarantees are payers (d ~ 0.8–0.9): identity-locked, no meaningful exit, bear the credibility erosion. Hodl faithful are dual (d ~ 0.5): benefit from adoption gains, pay uncertainty costs. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to improve without breaking consensus) remains live — scaling, privacy, and covenant functionality are unsolved. The utility reading has not suffered mandatrophy; its mandate *is* the ongoing problem. Mandatrophy would occur if the community declared 'Bitcoin is feature-complete' and stopped proposing improvements while the upgrade machinery remained — that would be a piton. Current trajectory: active improvement continues, so no mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    utility_vs_maximalist_boundary,
    'Is the utility reading''s claim that soft forks are ''legitimate kernel evolution'' a genuine structural distinction from the pragmatic synthesis''s ''upper layers only,'' or does it collapse under scrutiny to the same thing?',
    'Analyze whether any activated or proposed soft fork (Taproot, CTV, APO, CAT) *requires* base-layer monetary rule changes or only script/expressiveness changes. If all are script-level, the utility reading''s ''kernel evolution'' may be pragmatically identical to pragmatic synthesis''s ''upper layers.''',
    'If identical, the utility reading is a rhetorical variant of pragmatic synthesis — same ε, same stakeholders, different framing. If distinct (some soft forks touch monetary rules), the utility reading has higher ε and different victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utility_vs_maximalist_boundary, conceptual, 'Whether utility and pragmatic synthesis readings are structurally distinct constraints.').

omega_variable(
    ossification_guarantee_as_victim,
    'Can a structural property (''monetary ossification guarantee'') be a victim in the same sense as a human agent? Does its degradation constitute extraction?',
    'Model the guarantee as a coordination asset: its value is the reduction in coordination cost for holders who trust rules won''t change. Measure the cost increase when trust degrades (e.g., higher risk premiums, reduced institutional allocation). If measurable, the guarantee is a valid victim stakeholder.',
    'If valid, the extraction is real and quantified. If invalid, the victim set shrinks to identity-locked humans only, lowering ε and potentially reclassifying toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ossification_guarantee_as_victim, conceptual, 'Whether a non-agent structural guarantee can bear extraction as a victim.').

omega_variable(
    miner_activation_power,
    'Do miners still hold effective veto power over soft forks (via signaling), or has UASF (user-activated soft fork) precedent shifted agenda-setting to developers/users?',
    'Track signaling thresholds and activation paths for recent/proposed soft forks. If miners'' signaling is ceremonial (always follow developer/user consensus), their agenda_setter role is theater; if they can and do block, they remain structural agenda-setters.',
    'If miners are ceremonial, the agenda_setter role shifts to protocol_developers + economic majority (users/exchanges), changing power distribution and directionality. If miners remain decisive, the current power mapping holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(miner_activation_power, empirical, 'Whether miner signaling is structural power or performative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 2009, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_util_tr_t2009, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2009, 0.02).
narrative_ontology:measurement(btc_util_tr_t2012, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2012, 0.05).
narrative_ontology:measurement(btc_util_tr_t2015, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(btc_util_tr_t2017, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2017, 0.12).
narrative_ontology:measurement(btc_util_tr_t2021, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2021, 0.14).
narrative_ontology:measurement(btc_util_tr_t2024, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(btc_util_be_t2009, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2009, 0.05).
narrative_ontology:measurement(btc_util_be_t2012, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2012, 0.12).
narrative_ontology:measurement(btc_util_be_t2015, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2015, 0.18).
narrative_ontology:measurement(btc_util_be_t2017, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2017, 0.32).
narrative_ontology:measurement(btc_util_be_t2021, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2021, 0.38).
narrative_ontology:measurement(btc_util_be_t2024, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(btc_util_su_t2009, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2009, 0.1).
narrative_ontology:measurement(btc_util_su_t2012, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2012, 0.15).
narrative_ontology:measurement(btc_util_su_t2015, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(btc_util_su_t2017, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2017, 0.28).
narrative_ontology:measurement(btc_util_su_t2021, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2021, 0.26).
narrative_ontology:measurement(btc_util_su_t2024, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__utility_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% Part of the bitcoin_consensus_kernel constraint family. This reading (utility) treats soft forks as legitimate kernel evolution with moderate extractiveness. The maximalist_reading treats any change as covenant violation (high extractiveness, snare). The pragmatic_synthesis restricts immutability to monetary rules only (low extractiveness, rope for upper layers). All three share the same kernel (whitepaper consensus mechanism) but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__utility_reading, organized, 0.35).
constraint_indexing:directionality_override(bitcoin_consensus_kernel__utility_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
