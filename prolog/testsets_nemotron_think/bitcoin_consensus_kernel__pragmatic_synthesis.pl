% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__pragmatic_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__pragmatic_synthesis, []).

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
 *   constraint_id: bitcoin_consensus_kernel__pragmatic_synthesis
 *   human_readable: Bitcoin Pragmatic Synthesis: Base Immutability with Layered Innovation
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   The pragmatic synthesis reading of the Bitcoin consensus kernel asserts
 *   that the base layer's monetary rules (21M cap, issuance schedule,
 *   consensus validation) are immutable by covenant, while upper layers
 *   (Lightning, sidechains, client-side validation) provide a permissionless
 *   innovation surface. This reading emerged from the Blocksize War as a
 *   structural compromise: it grants maximalists their immutability anchor
 *   and utility advocates their innovation venue, but at the cost of
 *   maximalist ideological coherence (they must accept layers as legitimate)
 *   and utility advocate direct base-layer access (they must build on
 *   layers). The synthesis is a scaffold — it carries an implicit sunset:
 *   either the layered architecture proves sufficient and becomes permanent
 *   (scaffold→rope), or pressure for base changes resurges and the synthesis
 *   collapses into open conflict (scaffold→snare/tangled_rope).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__pragmatic_synthesis, 0.15).
domain_priors:suppression_score(bitcoin_consensus_kernel__pragmatic_synthesis, 0.1).
domain_priors:theater_ratio(bitcoin_consensus_kernel__pragmatic_synthesis, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, extractiveness, 0.15).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__pragmatic_synthesis, scaffold).
narrative_ontology:human_readable(bitcoin_consensus_kernel__pragmatic_synthesis, "Bitcoin Pragmatic Synthesis: Base Immutability with Layered Innovation").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__pragmatic_synthesis, "cryptoeconomics/monetary_systems/distributed_consensus").

narrative_ontology:has_sunset_clause(bitcoin_consensus_kernel__pragmatic_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__pragmatic_synthesis, '6b606e67-a606-47a4-ad7d-baf53c9f9e5b').
narrative_ontology:cs_kernel_codification('6b606e67-a606-47a4-ad7d-baf53c9f9e5b', fixed_text).
narrative_ontology:cs_authority_grounding('6b606e67-a606-47a4-ad7d-baf53c9f9e5b', lineage).
narrative_ontology:cs_interpretation_layer_present('6b606e67-a606-47a4-ad7d-baf53c9f9e5b').
narrative_ontology:cs_reading_relation('6b606e67-a606-47a4-ad7d-baf53c9f9e5b', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b606e67-a606-47a4-ad7d-baf53c9f9e5b', bitcoin_consensus_kernel__utility_reading, influences).
narrative_ontology:cs_axiom('6b606e67-a606-47a4-ad7d-baf53c9f9e5b', foundational, monetary_covenant_immutability).
narrative_ontology:cs_axiom_status(monetary_covenant_immutability, holdable).
narrative_ontology:cs_axiom_grounding('6b606e67-a606-47a4-ad7d-baf53c9f9e5b', monetary_covenant_immutability, deontological).
narrative_ontology:cs_axiom('6b606e67-a606-47a4-ad7d-baf53c9f9e5b', foundational, permissionless_layer_innovation).
narrative_ontology:cs_axiom_status(permissionless_layer_innovation, holdable).
narrative_ontology:cs_axiom_grounding('6b606e67-a606-47a4-ad7d-baf53c9f9e5b', permissionless_layer_innovation, instrumental).
narrative_ontology:cs_reference_frame('6b606e67-a606-47a4-ad7d-baf53c9f9e5b', segregated_immutability_architecture).
narrative_ontology:cs_drift_state('6b606e67-a606-47a4-ad7d-baf53c9f9e5b', contemporary_multilayer_ecosystem, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6b606e67-a606-47a4-ad7d-baf53c9f9e5b', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, layer2_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, pragmatic_users).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_core_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_purists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, miners_validators).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__pragmatic_synthesis, segregated_immutability_architecture).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__pragmatic_synthesis, permissionless_innovation_via_layers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the whitepaper as immutable covenant; view any layered innovation as diluting Bitcoin's monetary purity. Their ideological coherence fractures when the community accepts layers as legitimate — they cannot exit the dispute without abandoning their identity as guardians of the original vision. They bear the cost of watching the synthesis legitimize what they see as heresy.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_purists, payer,
    organized, generational, identity_locked, global).

% Build Lightning, RGB, Stacks, and other protocols on Bitcoin's base layer. They gain a stable, credible monetary base that doesn't change, enabling long-term infrastructure investment. Their exit is mobile — they can build on other chains — but they choose Bitcoin for its settlement assurances.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, layer2_builders, beneficiary,
    organized, biographical, mobile, global).

% Use Bitcoin for savings and Layer 2 for payments/complex operations. They benefit from both the base layer's monetary hardness and layers' functionality. They have low switching costs — can use other chains — but prefer the synthesis arrangement.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, pragmatic_users, beneficiary,
    moderate, biographical, mobile, global).

% Maintain the base layer consensus rules. They enforce the immutability boundary (no monetary policy changes) while merging soft-forks that enable layer functionality (Taproot, SegWit). Their exit is constrained by reputation and institutional role; they administer the synthesis de facto.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_core_developers, agenda_setter,
    institutional, generational, constrained, global).

% Argue for base-layer changes (block size, opcode additions, drivechains) to enable more direct utility. They are excluded from the synthesis's base-layer immutability commitment — their proposals are rejected as violating the kernel. They can exit to other chains (and many have), but their voice is absent from the base-layer governance.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, utility_advocates, excluded,
    organized, biographical, mobile, global).

% Secure the base layer and earn subsidies/fees. They benefit from the immutable monetary schedule (predictable issuance) and from layer fee markets. Their exit is constrained by sunk hardware capital and network effects — they follow the chain with the most accumulated work.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, miners_validators, beneficiary,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, immutable monetary base layer that coordinates global consensus on scarcity and settlement finality, while creating a permissionless interface for upper-layer innovation that does not require base-layer governance approval.
% TRANSFER_FUNCTION: Transfers the burden of ideological purity from the maximalist camp (who lose unchallenged narrative control) to the broader ecosystem (which gains functional innovation). No direct monetary transfer; the transfer is narrative authority and coalition membership.
% ABSENT_VOICES: Utility advocates who want base-layer scaling and feature additions are structurally excluded — the synthesis defines their desired changes as kernel violations. They are present in the broader ecosystem (other chains, layer proposals) but absent from the base-layer consensus process.
% DISAPPEARANCE_RATIONALE: If the synthesis vanished, either maximalists would enforce strict base-layer ossification (blocking Taproot-style upgrades) or utility advocates would push through base-layer changes — either way, the current layered ecosystem (Lightning, etc.) would lose its credible immutability anchor or its upgrade path, causing significant reorganization of capital and developer allocation.
% FOUNDING_PROBLEM: Bitcoin faced a binary impasse: maximalists demanded absolute base-layer immutability (no changes ever), while utility advocates demanded base-layer mutability for scaling and features. The network risked splitting into incompatible chains or ossifying into irrelevance.
% FOUNDING_PROBLEM_CORROBORATION: The Blocksize War (2015-2017) and subsequent Taproot activation (2021) are attested by participants on all sides as the crucible where this synthesis emerged. Maximalists (e.g., Pierre Rochard, Stephan Livera) acknowledge the synthesis exists but contest its legitimacy; layer builders (Lightning Labs, Blockstream) attest it enables their work; core developers (Greg Maxwell, Pieter Wuille) authored the soft-fork path that instantiated it. No single camp controls the narrative.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__pragmatic_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__pragmatic_synthesis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__pragmatic_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__pragmatic_synthesis_tests).
:- end_tests(bitcoin_consensus_kernel__pragmatic_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because participation in layers is voluntary and the base layer imposes no transfer — the 'cost' to maximalists is narrative/ideological, not material. Suppression is minimal (0.1) — maximalists can and do run nodes that ignore layer traffic; utility advocates can and do fork to other chains. Theater is low (0.1) — the coordination function (stable base + permissionless layers) is genuine and operational. Accessibility collapse is moderate (0.4) — alternatives exist (other L1s, maximalist forks like Bitcoin SV) but the synthesis captures the dominant network effect. Resistance is moderate (0.3) — ongoing debates about drivechains, covenants, and Ossification show the settlement is contested.
 *
 * PERSPECTIVAL GAP:
 *   From the maximalist seat, the synthesis is a snare — it presents as coordination but extracts their narrative monopoly. From the layer-builder seat, it is a rope — genuine coordination enabling innovation without permission. From the core developer seat, it is a scaffold — a temporary governance architecture that must eventually resolve into either ossification or controlled evolution. The engine computes these per-seat types from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Maximalist purists are payers (d ~0.7) — they bear the ideological cost of the synthesis legitimating layers. Their identity_locked exit reflects that their self-concept is fused to the 'immutable covenant' narrative; leaving the dispute means abandoning their role as guardians. Layer2 builders and pragmatic users are beneficiaries (d ~0.2) — they gain functional utility from the arrangement. Core developers are agenda_setters (d ~0.3) — they administer the boundary but are constrained by social consensus. Miners are beneficiaries (d ~0.2) — they gain predictable issuance and fee markets. Utility advocates are excluded (d ~0.8) — their desired changes are ruled out of bounds by the synthesis's base-layer immutability axiom.
 *
 * MANDATROPHY ANALYSIS:
 *   The synthesis prevents mislabeling by making the coordination function (stable base for layers) and the extraction (ideological coherence from maximalists) structurally visible. A pure coordination reading would miss the maximalist cost; a pure extraction reading would miss the genuine layer enablement. The mandatrophy is unresolved — the founding problem (binary impasse) remains live, and the synthesis's sunset clause is implicit, not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'Is the pragmatic_synthesis a stable architectural principle or a temporary truce in the Bitcoin governance war?',
    'Observe whether base-layer change proposals (drivechains, covenants, OP_CAT) are evaluated against the synthesis criteria (does this violate base immutability?) or whether the criteria themselves shift under pressure.',
    'If temporary truce, the scaffold classification is accurate and sunset is imminent. If stable principle, the constraint may recompute as rope (coordination without extraction) once maximalist resistance becomes marginal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, empirical, 'Commitment-system framing: this constraint is one reading of the bitcoin_consensus_kernel; sibling readings are maximalist_reading and utility_reading. The synthesis attempts to satisfy both camps by segregating immutability (base) from flexibility (layers).').

omega_variable(
    ideological_coherence_as_extraction,
    'Does the loss of maximalist ideological coherence constitute genuine extraction, or is it the cost of participating in a pluralistic consensus?',
    'Measure whether maximalist-aligned capital and hashpower remain on the main chain or fragment to alternatives. If they stay despite narrative loss, the cost is expressive not extractive.',
    'If expressive, extractiveness is overstated and the constraint is closer to rope. If extractive (maximalists are coerced into accepting layers they reject), the scaffold classification holds with higher effective extraction for the maximalist seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ideological_coherence_as_extraction, conceptual, 'Whether ideological/narrative displacement counts as extraction in the DR framework.').

omega_variable(
    layer_separation_stability,
    'Can the base/layer separation hold technically and socially as layer activity scales (e.g., Lightning routing fees, MEV on layers, bridge risks)?',
    'Monitor whether layer failures or externalities create pressure for base-layer interventions (e.g., covenants to fix Lightning, drivechains for sidechains).',
    'If separation holds, scaffold→rope transition. If base changes become necessary for layer viability, the synthesis collapses and the constraint recomputes as tangled_rope (coordination + extraction via base changes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_separation_stability, empirical, 'Technical and economic stability of the segregated architecture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__pragmatic_synthesis, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 3, 0.07).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 6, 0.08).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 9, 0.09).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 12, 0.1).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(bitc_be_t3, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 3, 0.08).
narrative_ontology:measurement(bitc_be_t6, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 6, 0.1).
narrative_ontology:measurement(bitc_be_t9, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 9, 0.12).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 12, 0.14).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 15, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(bitc_su_t3, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 3, 0.08).
narrative_ontology:measurement(bitc_su_t6, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 6, 0.1).
narrative_ontology:measurement(bitc_su_t9, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 9, 0.1).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 12, 0.1).
narrative_ontology:measurement(bitc_su_t15, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 15, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__pragmatic_synthesis, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__pragmatic_synthesis, 0.08).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, lightning_network_governance).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_ossification_debate).

% DUAL FORMULATION NOTE:
% This constraint is the pragmatic_synthesis reading of the bitcoin_consensus_kernel. The maximalist_reading declares absolute base immutability (no layers as legitimate); the utility_reading declares base mutability for iterative improvement. The pragmatic synthesis segregates: base immutable, layers permissionless. All three readings share the kernel but instantiate different constraints with different ε, beneficiaries, and victims. The ε-invariance principle requires separate stories because measuring 'Bitcoin governance' one way (base layer only) gives low ε (maximalist), another way (including layers) gives higher ε (utility), and the synthesis gives low ε with a victim (maximalist coherence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__pragmatic_synthesis, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
