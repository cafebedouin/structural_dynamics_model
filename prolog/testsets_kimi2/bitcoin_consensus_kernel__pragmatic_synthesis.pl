% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__pragmatic_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: bitcoin_consensus_kernel__pragmatic_synthesis
 *   human_readable: Bitcoin Pragmatic Synthesis: Immutable Base with Flexible Layers
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint story captures the pragmatic_synthesis reading of the
 *   bitcoin_consensus_kernel. The kernel is the Bitcoin whitepaper and its
 *   associated protocol consensus; the maximalist_reading treats it as an
 *   immutable covenant forbidding any departure, the utility_reading treats
 *   it as a minimum viable consensus enabling iterative base-layer
 *   improvement, and this readingâthe pragmatic_synthesisâinterprets it
 *   as mandating base-layer monetary immutability while legitimizing
 *   innovation on upper layers. The constraint arranges a social and
 *   technical scaffold that spatially segregates immutable monetary rules
 *   from flexible layer development, attempting to satisfy both immutability
 *   advocates and innovation seekers. The cost is borne by ideological
 *   purists on both sides whose coherent narratives are disrupted by the
 *   bifurcation, and by base reformists who are blocked from changing the
 *   protocol. As a kernel reading, this file instantiates ONLY the pragmatic
 *   synthesis; the sibling readings are separate constraints linked via
 *   cs_structure.
 *
 * KEY AGENTS:
 *   - protocol_maintainers: Agenda-setter (organized/constrained) â enforce the base-layer consensus boundary and evaluate layer proposals for kernel safety
 *   - layer_innovators: Primary beneficiary (moderate/mobile) â build on upper layers without altering base consensus
 *   - monetary_maximalists: Secondary beneficiary (organized/identity_locked) â retain hard-money assurances at the base layer
 *   - ideological_purists: Primary payer (moderate/identity_locked) â bear the cost of narrative incoherence as the community adopts pragmatic layering
 *   - base_reformists: Secondary payer (moderate/mobile) â blocked from base-layer protocol upgrades by the immutability commitment
 *   - pragmatic_users: Diffuse beneficiary (organized/mobile) â consume both stable savings and layered services
 *   - cryptoeconomic_researchers: Analytical observer (analytical/analytical) â evaluate security and governance of the layered model
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__pragmatic_synthesis, 0.28).
domain_priors:suppression_score(bitcoin_consensus_kernel__pragmatic_synthesis, 0.35).
domain_priors:theater_ratio(bitcoin_consensus_kernel__pragmatic_synthesis, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, extractiveness, 0.28).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__pragmatic_synthesis, scaffold).
narrative_ontology:human_readable(bitcoin_consensus_kernel__pragmatic_synthesis, "Bitcoin Pragmatic Synthesis: Immutable Base with Flexible Layers").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__pragmatic_synthesis, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:has_sunset_clause(bitcoin_consensus_kernel__pragmatic_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__pragmatic_synthesis, 'e7727d70-23f6-4c78-9b0b-f178b69b1368').
narrative_ontology:cs_kernel_codification('e7727d70-23f6-4c78-9b0b-f178b69b1368', fixed_text).
narrative_ontology:cs_authority_grounding('e7727d70-23f6-4c78-9b0b-f178b69b1368', distributed).
narrative_ontology:cs_reading_relation('e7727d70-23f6-4c78-9b0b-f178b69b1368', bitcoin_consensus_kernel__maximalist_reading, influences).
narrative_ontology:cs_reading_relation('e7727d70-23f6-4c78-9b0b-f178b69b1368', bitcoin_consensus_kernel__utility_reading, influences).
narrative_ontology:cs_axiom('e7727d70-23f6-4c78-9b0b-f178b69b1368', foundational, layer_innovation_without_kernel_breach).
narrative_ontology:cs_axiom_status(layer_innovation_without_kernel_breach, holdable).
narrative_ontology:cs_axiom_grounding('e7727d70-23f6-4c78-9b0b-f178b69b1368', layer_innovation_without_kernel_breach, conventional).
narrative_ontology:cs_axiom('e7727d70-23f6-4c78-9b0b-f178b69b1368', foundational, base_layer_monetary_ossification).
narrative_ontology:cs_axiom_status(base_layer_monetary_ossification, holdable).
narrative_ontology:cs_axiom_grounding('e7727d70-23f6-4c78-9b0b-f178b69b1368', base_layer_monetary_ossification, conventional).
narrative_ontology:cs_reference_frame('e7727d70-23f6-4c78-9b0b-f178b69b1368', immutable_base_flexible_layers).
narrative_ontology:cs_drift_state('e7727d70-23f6-4c78-9b0b-f178b69b1368', post_taproot_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e7727d70-23f6-4c78-9b0b-f178b69b1368', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, layer_innovators).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, monetary_maximalists).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, pragmatic_users).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, ideological_purists).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, base_reformists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Review and enforce consensus rules that prevent base-layer changes to monetary policy while evaluating layer-two proposals for kernel safety. They coordinate soft-fork activation and police boundary violations that would breach the base-layer settlement guarantees.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, protocol_maintainers, agenda_setter,
    organized, generational, constrained, global).

% Develop payment channels, sidechains, and rollup-style constructions that inherit Bitcoin's settlement assurances without altering the base protocol. They require the base layer to remain stable so their constructions can rely on fixed economic and cryptographic assumptions.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, layer_innovators, beneficiary,
    moderate, biographical, mobile, global).

% Accumulate bitcoin as a hard-money savings instrument and defend the fixed issuance schedule and 21-million cap. They accept upper-layer experimentation only to the extent that it does not alter or threaten the base monetary rules they view as sacred.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, monetary_maximalists, beneficiary,
    organized, generational, identity_locked, global).

% Maintain that Bitcoin must remain a single monolithic chain exactly as envisaged in the whitepaper, with no sanctioned deviation. They experience the pragmatic synthesis as a fracture in the unified narrative and covenant they invested their identity in, leaving them rhetorically isolated within their own community.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, ideological_purists, payer,
    moderate, biographical, identity_locked, global).

% Advocate for base-layer protocol upgrades to add expressive smart-contract functionality or adjust parameters in response to competitive pressure. They are blocked by the immutability consensus and must either accept layered workarounds they view as inferior or migrate to alternative base layers.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, base_reformists, payer,
    moderate, biographical, mobile, global).

% Hold bitcoin for long-term savings while occasionally using layered services for payments or applications. They are indifferent to whether innovation occurs at the base or at layers, provided the monetary policy remains predictable and the system remains usable.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, pragmatic_users, beneficiary,
    organized, biographical, mobile, global).

% Publish independent analyses of whether layered constructions preserve Bitcoin's security model, whether the governance synthesis is economically stable, and whether the base layer faces systemic risk from layered dependencies.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, cryptoeconomic_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the governance deadlock between immutability advocates and innovation advocates by spatially segregating their domains: the base layer guarantees fixed monetary rules while upper layers host iterative development, allowing both camps to build without vetoing each other.
% TRANSFER_FUNCTION: Transfers governance authority over non-monetary features from the base layer to upper-layer protocols, while transferring the cost of narrative incoherence to ideological purists on both sides who must accept a bifurcated vision of the system.
% ABSENT_VOICES: Absolute maximalists who reject any layering as covenant violation and base-layer reformists who believe monetary policy should adapt to economic conditions are audible in forums but structurally overridden by the synthesis; developers of competing Layer-1 smart-contract platforms are excluded from Bitcoin governance discourse entirely.
% DISAPPEARANCE_RATIONALE: If the pragmatic synthesis vanished, the community would revert to governance deadlock between immutability and utility factions, base-layer soft forks would become contentious again, capital would flee toward chains offering either pure simplicity or flexible base layers, and the layered infrastructure would lose its social license.
% FOUNDING_PROBLEM: The Bitcoin community faced a governance crisis circa 2015-2017: maximalists viewed any base-layer change as a violation of the founding covenant, while utilitarians demanded iterative upgrades to compete with smart-contract platforms, creating a paralysis that threatened capital flight and developer attrition.
% FOUNDING_PROBLEM_CORROBORATION: Independent blockchain governance researchers and academic cryptoeconomists outside the Bitcoin developer and investor communities corroborate that Bitcoin's ossification created a pressure cooker for layer innovation; both sides of the internal dispute attest to the deadlock in public conferences, though their descriptions of the problem's severity diverge.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__pragmatic_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__pragmatic_synthesis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__pragmatic_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, 0.28, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored low (0.28) because the pragmatic synthesis is explicitly designed to minimize extraction: it expands the design space without confiscating resources or suppressing exit. Suppression (0.35) is moderate because maintaining the base/layer boundary requires active social enforcement, including the blocking of base-layer hard forks and narrative policing of deviations. Theater ratio (0.20) is low-to-moderate: most base-layer and layer activity is functional, though some governance theater exists around soft-fork signaling and activation rituals. Accessibility collapse (0.45) is moderate because alternatives (other chains, competing readings) remain visible and viable. Resistance (0.40) reflects ongoing contestation from maximalists who reject layers as illegitimate and from reformists who reject ossification. The metric profile supports a low-extraction scaffold rather than a rope (because it requires enforcement to maintain the boundary) or a mountain (because it is constructed social consensus, not natural law).
 *
 * PERSPECTIVAL GAP:
 *   From the layer innovator and pragmatic user seats, the constraint is coordination: it solves a governance deadlock by partitioning domains so both camps can build without veto. From the ideological purist seat, the same constraint extracts by polluting a previously coherent narrative with sanctioned exceptions. From the base reformist seat, the constraint extracts by foreclosing a development path they view as necessary for competitiveness. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Protocol maintainers sit near symmetric: they administer the boundary without collecting concentrated rents. Layer innovators and monetary maximalists are beneficiaries (low d): the former receives a secure, predictable base to build on, the latter receives immutable monetary policy. Pragmatic users are diffuse beneficiaries. Ideological purists and base reformists are payers (high d): the former loses narrative coherence, the latter loses the ability to change the base layer. No concentrated agenda-setter captures the extraction; the costs are diffuse to identity-locked and constrained agents.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the pragmatic synthesis as pure extraction (snare) by requiring beneficiaries and a genuine coordination function: the arrangement solves a real deadlock between immutability and innovation advocates. It prevents mislabeling as a mountain by refusing emerges_naturally: the base/layer segregation is a constructed social consensus, not a physical or cryptographic law. The scaffold classification is appropriate because the arrangement's primary justification is transitionalâresolving a governance crisis during the bootstrapping of layered infrastructureâand it carries a declared sunset as the architecture matures and the synthesis either succeeds or fractures into one of the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_or_steady_state,
    'Is the pragmatic synthesis genuinely a transitional scaffold toward a mature layered ecosystem, or has it become a permanent steady-state arrangement?',
    'Observe whether social discourse and protocol development treat the layered architecture as provisional or as the definitive long-term roadmap; a permanent steady-state would reclassify the constraint toward rope or tangled_rope.',
    'If steady-state, the low extraction metric may understate persistence and the sunset clause is a fiction; if scaffold, the transitional framing is structurally honest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_or_steady_state, conceptual, 'Whether the pragmatic synthesis is transitional or permanent').

omega_variable(
    ideological_extraction_or_side_effect,
    'Is the loss of ideological coherence a genuine extraction cost borne by payers, or merely a side effect of political compromise with no structural beneficiary?',
    'Examine whether any specific agent captures authority, status, or resources from the ideological fragmentation of the community; if none, the victim classification may be overstated.',
    'If no capturer exists, the constraint may be a rope rather than a scaffold with victims; if a capturer exists, they should be named in gain_flow.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ideological_extraction_or_side_effect, conceptual, 'Whether ideological fragmentation is extracted or incidental').

omega_variable(
    base_immutability_enforcement_drift,
    'Does the enforcement required to maintain base-layer immutability decay as the layer ecosystem matures, or does it intensify?',
    'Track the frequency and social cost of base-layer hard-fork attempts and their suppression over the measurement interval.',
    'Rising suppression would signal Goodhart drift toward extraction and would undermine the scaffold claim; decay would support the transience thesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(base_immutability_enforcement_drift, empirical, 'Whether enforcement drifts upward or downward over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__pragmatic_synthesis, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btcpk_tr_t0, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0, 0.08).
narrative_ontology:measurement(btcpk_tr_t3, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 3, 0.1).
narrative_ontology:measurement(btcpk_tr_t6, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 6, 0.12).
narrative_ontology:measurement(btcpk_tr_t9, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 9, 0.15).
narrative_ontology:measurement(btcpk_tr_t12, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 12, 0.17).
narrative_ontology:measurement(btcpk_tr_t15, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(btcpk_be_t0, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(btcpk_be_t3, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 3, 0.17).
narrative_ontology:measurement(btcpk_be_t6, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 6, 0.2).
narrative_ontology:measurement(btcpk_be_t9, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 9, 0.23).
narrative_ontology:measurement(btcpk_be_t12, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(btcpk_be_t15, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 15, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(btcpk_su_t0, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(btcpk_su_t3, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 3, 0.25).
narrative_ontology:measurement(btcpk_su_t6, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 6, 0.28).
narrative_ontology:measurement(btcpk_su_t9, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 9, 0.31).
narrative_ontology:measurement(btcpk_su_t12, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(btcpk_su_t15, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 15, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
