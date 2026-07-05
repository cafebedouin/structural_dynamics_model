% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Bitcoin Consensus Kernel — Utility/Iterative-Improvement Reading
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This story instantiates the utility reading of the contested Bitcoin
 *   consensus kernel: the whitepaper as a minimum viable
 *   Byzantine-fault-tolerant mechanism deliberately left open for iterative
 *   improvement, not a sealed monetary covenant. Under this reading, soft
 *   forks (SegWit, Taproot) and layer-2 protocols (Lightning) are legitimate
 *   evolution of an intentionally extensible base design. This is a distinct
 *   constraint from the maximalist reading (whitepaper as immutable covenant)
 *   and the pragmatic synthesis (base layer immutable, upper layers open) —
 *   each of those is a separate story with its own ε, beneficiaries, and
 *   victims, linked via network.affects_constraints. Under the utility
 *   reading specifically, extraction is moderate: value flows from those who
 *   priced in monetary rigidity toward those positioned to capture the gains
 *   of continued protocol development.
 *
 * KEY AGENTS:
 *   - protocol_developers: primary agenda-setter, organized/mobile — proposes and ships changes
 *   - layer_two_builders: primary beneficiary, organized/mobile — business models depend on continued evolvability
 *   - long_term_holders_seeking_monetary_certainty: primary payer, moderate/constrained — bears erosion of the certainty premium
 *   - minority_fork_participants: secondary victim, powerless/trapped — bears network effect loss from rejecting changes
 *   - core_maximalist_holdouts: excluded voice, organized/identity_locked — objects to the entire premise of changeability
 *   - protocol_historians: analytical observer — assesses documentary record without stake in outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.42).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.38).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel — Utility/Iterative-Improvement Reading").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '33040279-3b6a-49bf-ab94-b1e24105abc7').
narrative_ontology:cs_kernel_codification('33040279-3b6a-49bf-ab94-b1e24105abc7', fixed_text).
narrative_ontology:cs_authority_grounding('33040279-3b6a-49bf-ab94-b1e24105abc7', practice).
narrative_ontology:cs_interpretation_layer_present('33040279-3b6a-49bf-ab94-b1e24105abc7').
narrative_ontology:cs_reading_relation('33040279-3b6a-49bf-ab94-b1e24105abc7', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('33040279-3b6a-49bf-ab94-b1e24105abc7', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('33040279-3b6a-49bf-ab94-b1e24105abc7', foundational, protocol_is_extensible_by_original_design).
narrative_ontology:cs_axiom_status(protocol_is_extensible_by_original_design, holdable).
narrative_ontology:cs_axiom_grounding('33040279-3b6a-49bf-ab94-b1e24105abc7', protocol_is_extensible_by_original_design, empirically_contingent).
narrative_ontology:cs_axiom('33040279-3b6a-49bf-ab94-b1e24105abc7', foundational, rough_consensus_among_practitioners_confers_legitimate_change_authority).
narrative_ontology:cs_axiom_status(rough_consensus_among_practitioners_confers_legitimate_change_authority, holdable).
narrative_ontology:cs_axiom_grounding('33040279-3b6a-49bf-ab94-b1e24105abc7', rough_consensus_among_practitioners_confers_legitimate_change_authority, conventional).
narrative_ontology:cs_reference_frame('33040279-3b6a-49bf-ab94-b1e24105abc7', minimum_viable_extensible_protocol).
narrative_ontology:cs_drift_state('33040279-3b6a-49bf-ab94-b1e24105abc7', post_segwit_taproot_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('33040279-3b6a-49bf-ab94-b1e24105abc7', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, protocol_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer_two_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, new_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, mining_pool_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, long_term_holders_seeking_monetary_certainty).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, minority_fork_participants).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, protocol_improvability_doctrine).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, rough_consensus_governance_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain reference client implementations and propose soft forks (BIPs) that extend functionality — SegWit, Taproot — arguing the whitepaper's design was explicitly minimal and meant to be built upon. They control the practical path of change even without formal authority, since miners and nodes must choose to adopt their releases. Their exit option is high: reputational capital transfers across projects if this one calcifies.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_developers, agenda_setter,
    organized, generational, mobile, global).

% Build payment channels, sidechains, and settlement layers (Lightning, federated sidechains) that depend on base-layer opcodes and soft-fork upgrades being possible. They collect fee revenue and equity value from the assumption that the base layer will continue to evolve to support their designs. If the kernel calcified as immutable, several of their business models would be foreclosed.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer_two_builders, beneficiary,
    organized, biographical, mobile, global).

% Enter the network expecting continued usability improvements — lower fees via batching/SegWit, better custody via Taproot, eventual scaling via layer 2. They benefit from the reading that treats iterative improvement as legitimate rather than as covenant violation; a frozen protocol would leave them without the utility gains they were promised by the roadmap.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, new_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Signal support for soft forks through hash power and collect transaction fee revenue that scales with usage the upgrades enable. They can arbitrage between competing forks or coin variants, giving them leverage over which reading of the kernel prevails in practice, since consensus changes require their cooperation.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, mining_pool_operators, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, mining_pool_operators, agenda_setter).

% Hold the asset specifically because they believe the whitepaper fixed an immutable monetary policy and settlement guarantee. Every soft fork, even non-monetary ones, is read by this group as evidence the 'rules' are negotiable, degrading the certainty premium they believed they purchased. Their exit is constrained: selling forfeits the position, forking off into a minority chain forfeits network effects and liquidity.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, long_term_holders_seeking_monetary_certainty, payer,
    moderate, civilizational, constrained, global).

% Users and node operators who rejected a given soft fork (e.g., small-blockers, opponents of Taproot) and continued running old rules or split into a minority chain. They bear the cost of network effect loss, liquidity fragmentation, and being characterized as obstructionist, even though their objection was to the changeability of the base layer itself.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, minority_fork_participants, payer,
    powerless, biographical, trapped, global).

% Believe the whitepaper's design should be read as a closed covenant, not a minimum viable starting point. They are structurally present in the ecosystem but the utility reading's governance process (rough consensus among developers, node operators, and miners) does not treat their objection as a veto — their voice registers as opposition to be overcome via signaling thresholds, not as grounds to halt change.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, core_maximalist_holdouts, excluded,
    organized, civilizational, identity_locked, global).

% Study the actual history of protocol changes, whitepaper text, and mailing-list debates to assess which reading better fits the documentary record. They have no stake in the outcome but their analysis is frequently cited by all sides to legitimate their preferred reading.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__utility_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__utility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates upgrade decisions across a decentralized network of developers, miners, and node operators so the protocol can add functionality (privacy, scaling, script capability) without requiring a hard fork or central authority to approve changes.
% TRANSFER_FUNCTION: Moves optionality and future utility gains toward those positioned to build on top of the evolving base layer (developers, L2 builders, new users chasing usability) and moves monetary-certainty value away from those who priced the asset on the assumption of a frozen rule set.
% ABSENT_VOICES: Maximalist holdouts who read the whitepaper as an immutable covenant are present in the ecosystem's discourse but structurally outvoted by the rough-consensus signaling mechanism; their objection is treated as noise to route around rather than as a binding constraint.
% DISAPPEARANCE_RATIONALE: If the practice of soft-fork iteration disappeared overnight, layer-2 builders and developers would say the network loses its ability to remain competitive and useful, while long-term holders and maximalist holdouts would say the network finally becomes what it was always supposed to be — a fixed monetary settlement layer. The two camps disagree not just on value but on which state is the 'disappearance' and which is the 'restoration.'
% FOUNDING_PROBLEM: The whitepaper needed to establish that a Byzantine-fault-tolerant, permissionless network could reach consensus on transaction history without a trusted third party — a minimum viable mechanism to bootstrap trustless value transfer, not a complete or final specification of every future protocol feature.
% FOUNDING_PROBLEM_CORROBORATION: Protocol developers and layer-2 builders (benefiting parties) attest the whitepaper explicitly frames the design as extensible. Independent corroboration comes from protocol historians analyzing the original mailing-list correspondence and the whitepaper's own language around versioning and node software updates, which shows Satoshi Nakamoto personally shipped protocol changes post-launch — evidence external to current beneficiaries that iteration was part of the original design posture, though maximalist-aligned historians dispute this reading of the same correspondence.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__utility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__utility_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored as moderate (0.42) because the utility reading's costs to holders are real but partial — soft-fork changes are backward compatible by design, so no one is forcibly expropriated, but the informational value of 'the rules cannot change' is progressively degraded for those who held that as their core investment thesis. Suppression (0.38) reflects the rough-consensus mechanism's real coercive edge: node operators and miners who reject a soft fork face declining relevance rather than formal exclusion, which is softer than a hard fork's outright chain split but still narrows practical alternatives over time. Theater ratio is low-moderate (0.28) and rising, tracking the increasing use of 'decentralized rough consensus' as a legitimating narrative for what is, in practice, coordinated action by a concentrated set of developers and large mining pools.
 *
 * DIRECTIONALITY LOGIC:
 *   Protocol developers and layer-2 builders sit near the beneficiary end: they set the practical agenda and capture the option value of continued evolution. New adopters benefit from ongoing usability improvements. Long-term holders seeking monetary certainty and minority fork participants sit near the target end: their exit options are constrained or trapped precisely because network effects lock in value even for those who reject specific changes. Mining pool operators are structurally ambiguous — beneficiaries of fee revenue growth but also agenda-setters via hash power signaling, hence the secondary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The utility reading resists mandatrophy in one direction and risks it in another: it correctly refuses to treat the founding whitepaper as frozen in amber when the whitepaper's own text and Nakamoto's own subsequent commits show iterative design intent — treating that founding problem as 'dead' would be historically false. But it risks its own mandatrophy if 'iterative improvement' becomes cover for changes that serve developer and L2-builder interests specifically rather than the network's original trust-minimization goal — the founding problem (permissionless BFT consensus without trusted third parties) could be quietly supplanted by a different mandate (maximizing transactional throughput or developer optionality) while retaining the same legitimating language.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    iterative_improvement_vs_captured_agenda,
    'Does ''iterative improvement'' under the utility reading track the founding problem (trust-minimized permissionless consensus) or has it drifted toward serving the interests of the developers and layer-2 businesses who now control the practical upgrade path?',
    'Trace whether proposed and shipped soft forks over the interval primarily improved base-layer security/decentralization properties versus primarily enabled new fee-generating business models for a concentrated set of developers and L2 operators.',
    'If drift toward captured-agenda is found, the utility reading''s own extraction estimate should rise and its classification should move from tangled_rope toward something closer to a snare on the affected stakeholder set; if not, the moderate tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iterative_improvement_vs_captured_agenda, empirical, 'Whether the utility reading''s iterative-improvement claim tracks its founding problem or has been captured by developer/builder interests.').

omega_variable(
    which_reading_the_original_authors_intended,
    'Did Satoshi Nakamoto and the whitepaper''s original text intend the protocol as extensible-by-design (supporting this reading) or as a fixed monetary policy where early software updates were merely bug fixes rather than precedent for ongoing evolution?',
    'Close textual and historical analysis of the whitepaper, the original client''s versioning history, and contemporaneous mailing-list correspondence, weighted for the fact that this evidence is contested by both maximalist and utility camps using the same source material.',
    'If the historical record clearly supports one reading, the other readings lose founding-problem-status corroboration and should be marked more strongly contested or effectively dead in their own stories; if the record is genuinely ambiguous, all three readings retain legitimate contested status indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_the_original_authors_intended, conceptual, 'Irreducible interpretive uncertainty about original design intent, contested by all three kernel readings using the same textual evidence.').

omega_variable(
    rough_consensus_legitimacy_threshold,
    'At what threshold of miner/node-operator non-adoption does a soft fork cease to be legitimate rough consensus and become an imposition on a dissenting minority?',
    'Compare adoption percentages and dissent intensity across historical soft forks (SegWit''s contentious activation via UASF vs. Taproot''s comparatively smooth rollout) against outcomes for minority participants.',
    'A low legitimacy threshold would mean many past soft forks under this reading were closer to suppression than coordination, raising the effective suppression score materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rough_consensus_legitimacy_threshold, conceptual, 'Where rough consensus transitions from coordination to coercion of a dissenting minority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_consensus_kernel__utility_reading, theater_ratio, 3, 0.14).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_consensus_kernel__utility_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_consensus_kernel__utility_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__utility_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_consensus_kernel__utility_reading, theater_ratio, 16, 0.28).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(bitc_be_t3, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 3, 0.24).
narrative_ontology:measurement(bitc_be_t6, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 6, 0.31).
narrative_ontology:measurement(bitc_be_t9, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 9, 0.35).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(bitc_be_t16, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 16, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bitc_su_t3, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 3, 0.24).
narrative_ontology:measurement(bitc_su_t6, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 6, 0.29).
narrative_ontology:measurement(bitc_su_t9, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 9, 0.32).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(bitc_su_t16, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 16, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__utility_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% Three sibling stories decompose the natural-language 'Bitcoin consensus kernel' concept per the ε-invariance principle: maximalist_reading (whitepaper as immutable covenant; hard forks and soft forks alike are covenant violations; near-mountain claim with contested metrics), pragmatic_synthesis (base layer immutable, upper layers open; splits the difference structurally), and this story, utility_reading (whitepaper as deliberately minimal and extensible; soft forks and L2 are legitimate). Each has a distinct ε because each identifies different beneficiaries, victims, and enforcement structures from the same underlying text and network. They are linked bidirectionally via affects_constraints as siblings in one contested kernel, not merged into a single averaged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
