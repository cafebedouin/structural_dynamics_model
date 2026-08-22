% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: bitcoin_consensus_kernel__utility_reading
 *   human_readable: Bitcoin Consensus Kernel â Utility Reading (Iterative Bootstrap)
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint story captures the utility reading of the
 *   bitcoin_consensus_kernel: the whitepaper is interpreted as establishing a
 *   minimum viable consensus bootstrap explicitly designed for iterative
 *   improvement rather than an immutable monetary covenant. Under this
 *   reading, soft forks and layer-2 protocols are legitimate evolutions of
 *   the mechanism, and the coordination benefits of distributed consensus are
 *   real. However, the same iterative capacity asymmetrically erodes the
 *   monetary-ossification guarantees held by hard-money absolutists,
 *   producing moderate extractiveness concentrated on identity-locked
 *   guarantee-holders. The claim (tangled_rope) and metrics are authored
 *   independently: the engine may compute divergent per-seat classifications
 *   from the structural data.
 *
 * KEY AGENTS:
 *   - consensus_enforcers (agenda_setter/organized/arbitrage): miners and nodes administering rule enforcement and soft-fork activation
 *   - iterative_builders (beneficiary/moderate/constrained): developers and users capturing utility from protocol evolution
 *   - hard_money_guarantee_holders (payer/organized/identity_locked): ideological holders bearing the cost of eroded immutability expectations
 *   - cryptoeconomic_analysts (observer/analytical/analytical): external researchers observing the coordination-extraction tension
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.48).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.5).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel â Utility Reading (Iterative Bootstrap)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '2433a1f0-3e81-4b3f-8257-4f5d86f670ed').
narrative_ontology:cs_kernel_codification('2433a1f0-3e81-4b3f-8257-4f5d86f670ed', fixed_text).
narrative_ontology:cs_authority_grounding('2433a1f0-3e81-4b3f-8257-4f5d86f670ed', distributed).
narrative_ontology:cs_reading_relation('2433a1f0-3e81-4b3f-8257-4f5d86f670ed', bitcoin_consensus_kernel__maximalist_reading, forecloses).
narrative_ontology:cs_reading_relation('2433a1f0-3e81-4b3f-8257-4f5d86f670ed', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('2433a1f0-3e81-4b3f-8257-4f5d86f670ed', foundational, soft_fork_legitimacy).
narrative_ontology:cs_axiom_status(soft_fork_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2433a1f0-3e81-4b3f-8257-4f5d86f670ed', soft_fork_legitimacy, instrumental).
narrative_ontology:cs_axiom('2433a1f0-3e81-4b3f-8257-4f5d86f670ed', foundational, minimum_viable_bootstrap_intent).
narrative_ontology:cs_axiom_status(minimum_viable_bootstrap_intent, holdable).
narrative_ontology:cs_axiom_grounding('2433a1f0-3e81-4b3f-8257-4f5d86f670ed', minimum_viable_bootstrap_intent, empirically_contingent).
narrative_ontology:cs_reference_frame('2433a1f0-3e81-4b3f-8257-4f5d86f670ed', minimum_viable_consensus_bootstrap).
narrative_ontology:cs_drift_state('2433a1f0-3e81-4b3f-8257-4f5d86f670ed', layer_two_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2433a1f0-3e81-4b3f-8257-4f5d86f670ed', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, iterative_builders).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, hard_money_guarantee_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Miners and full nodes who enforce the longest-chain consensus rules, validate blocks, and coordinate soft-fork activation through hash-power and user-activated signaling. They administer the protocol's evolution by choosing which rule sets to enforce and which upgrades to adopt, with the ability to redirect hardware or exit to competing chains.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, consensus_enforcers, agenda_setter,
    organized, generational, arbitrage, global).

% Developers, entrepreneurs, and users building layer-2 protocols, sidechains, and soft-fork-dependent applications on Bitcoin. They benefit from the base layer's capacity to iterate without requiring hard-fork coordination or chain splits, though their sunk costs and network effects bind them to the Bitcoin ecosystem.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, iterative_builders, beneficiary,
    moderate, biographical, constrained, global).

% Investors and ideological holders who acquired bitcoin under the expectation of absolute protocol immutability and monetary-policy ossification. They bear the cost of diluted guarantees when soft forks alter script, privacy, or upgrade assumptions, even when the monetary supply cap remains untouched. Many remain in the ecosystem despite erosion because their self-concept and financial narrative are fused with hard-money absolutism.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, hard_money_guarantee_holders, payer,
    organized, generational, identity_locked, global).

% Researchers and open-source analysts studying the cryptoeconomic evolution of Bitcoin. They observe the tension between the coordination benefits of iterative improvement and the extraction costs borne by immutability-seeking holders, without enforcing rules or capturing transfers.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, cryptoeconomic_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__utility_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__utility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables distributed agreement on a shared ledger state without a central authority, while allowing the protocol to evolve through backward-compatible soft forks and layered extensions that preserve existing user balances and rule sets.
% TRANSFER_FUNCTION: Transfers monetary-ossification certainty from guarantee-holders to protocol adaptability, moving the cost of rigid immutability to those who bet on absolute stasis while distributing iterative-utility benefits to adopters and builders who leverage upgraded opcodes and layer-2 settlement.
% ABSENT_VOICES: Pure monetary maximalists who view any base-layer change as covenant violation are heard in discourse but structurally overruled by the soft-fork activation mechanism; holders of purely speculative ossification positions without technical signaling capacity are excluded from upgrade governance.
% DISAPPEARANCE_RATIONALE: If the consensus mechanism vanished, the distributed ledger would collapse into contradictory histories and double-spending would become possible. Without the specific utility-reading flexibility, layer-2 ecosystems and soft-fork upgrade paths would freeze, forcing migration to alternative chains or custodial overlays and reorganizing the digital-money landscape.
% FOUNDING_PROBLEM: The double-spending problem in digital cash and the need for a trust-minimized, peer-to-peer electronic cash system that could bootstrap without central issuance or identity.
% FOUNDING_PROBLEM_CORROBORATION: The cypherpunk mailing list archives and the whitepaper itself attest the bootstrapping problem from outside the beneficiary set; external monetary historians and competing protocol researchers corroborate that iterative improvement was anticipated, while Austrian-economist critics and maximalist historians from outside the benefiting parties contest that the founding problem is now dead and the constraint has drifted into a coordination-for-extraction pattern.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__utility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__utility_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__utility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_consensus_kernel__utility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 (moderate) because soft forks genuinely expand utility for builders but demonstrably dilute ossification guarantees for a vocal holder faction. Suppression is 0.50 because maintaining consensus and shepherding contested soft forks requires active social and technical enforcement against dissenting fork attempts. Theater ratio is 0.25: the hash-power security function is structurally real, with only a modest performative overlay around decentralization rhetoric. Accessibility collapse is 0.45 because network effects and liquidity make exit costly once capital and identity are committed, yet alternative chains and assets remain technically accessible. Resistance is 0.55 because maximalist opposition to soft forks (e.g., blocksize wars, NO2X campaigns) has been sustained and organized.
 *
 * PERSPECTIVAL GAP:
 *   The iterative_builder seat experiences the constraint as enabling infrastructure that solves genuine coordination problems (settlement finality, layer-2 anchoring). The hard_money_guarantee_holder seat experiences the identical soft-fork activation events as extraction from their settled expectations. The engine computes this divergence from the structural asymmetry in beneficiary/victim declarations and exit options, not from any authored classification override.
 *
 * DIRECTIONALITY LOGIC:
 *   Iterative_builders are beneficiaries (low d): they collect coordination surplus from a flexible base layer without bearing the enforcement burden. Hard_money_guarantee_holders are targets (high d): their expected immutability is the direct cost of the constraint's evolution, and identity-lock amplifies their effective extraction because ideological fusion suppresses exit. Consensus_enforcers sit near symmetric with slight beneficiary tilt: they pay operational costs of enforcement but control the upgrade path and capture fee revenue from the chain's continued operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids pure-snare mislabeling because the consensus mechanism performs a non-theatrical coordination function: preventing double-spending and enabling permissionless settlement. It avoids pure-rope mislabeling because the cost of flexibility is not symmetrically shared; guarantee-holders pay through eroded assurances they bought into and cannot block without full exit. The founding problemâbootstrapping trustless digital cashâis contested as to whether it remains live or has been superseded by an iterative-improvement mandate, keeping the mandatrophy question open rather than resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_immutability_vs_iterative_bootstrap,
    'Does the Bitcoin whitepaper establish an immutable monetary covenant intended to persist unchanged, or a minimum viable consensus bootstrap explicitly designed for iterative improvement?',
    'Textual analysis of the whitepaper and Satoshi''s subsequent communications for language of permanence versus language of future upgrade paths; technical analysis of the script system''s extensibility and the soft-fork upgrade mechanism built into the original code.',
    'If immutable covenant, the utility reading''s classification as iterative is a constructed misreading and extraction from guarantee-holders is high; if minimum viable bootstrap, the utility reading is structurally faithful and extraction is the asymmetric cost of genuine coordination evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_immutability_vs_iterative_bootstrap, conceptual, 'Whether the kernel is a permanent covenant or an evolutionary bootstrap').

omega_variable(
    suppression_mechanism_fork_resistance,
    'Is the suppression of ossification demands structural (hard forks are technically possible but socially discouraged through node signaling) or internalized (guarantee-holders have adopted the utility framing against their own immutability interests)?',
    'Observable fork attempts and their outcomes; holder sentiment surveys pre- and post-soft-fork; analysis of whether dissenting holders exit, convert, or remain and resist.',
    'If internalized, effective extraction exceeds the structural measure because guarantee-holders enforce the constraint on themselves after the social upgrade succeeds; if purely structural, extraction is bounded by the cost of coordinating a minority fork.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_fork_resistance, empirical, 'Structural versus internalized suppression of ossification demands').

omega_variable(
    sibling_maximalist_foreclosure,
    'Does the utility reading''s core premise of legitimate soft-fork evolution logically foreclose the maximalist reading''s claim of immutable covenant, or can both coexist as live positions?',
    'Logical analysis of whether a single actor can simultaneously hold that soft forks are legitimate upgrades AND that any protocol change violates the founding covenant.',
    'If foreclosed, the two readings are mutually exclusive constraint identities competing for kernel ownership; if coexisting, they are competing framings of the same structural arrangement whose divergence the engine measures as perspectival gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_maximalist_foreclosure, conceptual, 'Logical relation between utility and maximalist readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_consensus_kernel__utility_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_consensus_kernel__utility_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_consensus_kernel__utility_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__utility_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_consensus_kernel__utility_reading, theater_ratio, 15, 0.25).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(bitc_be_t3, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(bitc_be_t6, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(bitc_be_t9, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 9, 0.42).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 15, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bitc_su_t3, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 3, 0.3).
narrative_ontology:measurement(bitc_su_t6, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(bitc_su_t9, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 9, 0.45).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(bitc_su_t15, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 15, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, global_infrastructure).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% The bitcoin_consensus_kernel decomposes into three structurally distinct readings: maximalist (immutable covenant, negligible coordination function, high extraction from changemakers), pragmatic_synthesis (immutable base layer with innovative overlays, split coordination/extraction profile), and utility (iterative bootstrap, moderate extraction concentrated on ossification guarantee-holders). Each reading instantiates a different constraint with distinct beneficiary/victim structures and epsilon values. They are linked as a constraint family because they derive from the same natural-language kernel but encode incompatible structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
