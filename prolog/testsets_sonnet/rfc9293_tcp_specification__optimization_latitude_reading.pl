% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__optimization_latitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__optimization_latitude_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: rfc9293_tcp_specification__optimization_latitude_reading
 *   human_readable: RFC 9293 as Semantic Contract with Implementation Latitude
 *   domain: network_protocol_engineering
 *
 * SUMMARY:
 *   RFC 9293 (the 2022 consolidation of the TCP specification lineage
 *   originating in RFC 793) defines TCP's semantic contract: reliable,
 *   ordered, flow-controlled byte-stream delivery between endpoints. Under
 *   this reading, the specification deliberately fixes the OUTCOME while
 *   leaving the MEANS — congestion control algorithm choice, retransmission
 *   timing heuristics, buffer management — to implementer discretion,
 *   provided the observable interoperability guarantees hold. This is the
 *   reading under which BBR, CUBIC, DCTCP, and other congestion-control
 *   variants can be deployed by different vendors without any of them being
 *   'non-compliant' with the standard, because the standard was never a
 *   mechanism mandate. This is a genuinely low-extraction, low-suppression
 *   coordination structure: it solves interoperability without requiring
 *   central control of implementation detail, and no party captures rent from
 *   the flexibility itself. This story is deliberately narrow — it addresses
 *   only the optimization-latitude claim about RFC 9293, not the
 *   middlebox-path-dependency claim or the strict-invariance claim, which are
 *   separate constraints in the same kernel family with their own ε values.
 *
 * KEY AGENTS:
 *   - protocol_stack_implementers: beneficiary/agenda_setter (organized/mobile) — exercise the latitude directly
 *   - congestion_control_researchers: beneficiary (moderate/mobile) — design and deploy within the envelope
 *   - network_operators: beneficiary (organized/mobile) — benefit from stability plus tuning freedom
 *   - end_users: beneficiary (powerless/mobile) — receive the outcome guarantee transparently
 *   - ietf_tsvwg_and_authors: agenda_setter (institutional/analytical) — drew and maintain the semantic boundary
 *   - legacy_implementations: excluded (powerless/trapped) — benefit from interoperability but not from the optimization space
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.08).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.05).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "RFC 9293 as Semantic Contract with Implementation Latitude").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "network_protocol_engineering").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, '1a2b77e5-4bea-4846-b678-78a53780c806').
narrative_ontology:cs_kernel_codification('1a2b77e5-4bea-4846-b678-78a53780c806', fixed_text).
narrative_ontology:cs_authority_grounding('1a2b77e5-4bea-4846-b678-78a53780c806', expertise).
narrative_ontology:cs_interpretation_layer_present('1a2b77e5-4bea-4846-b678-78a53780c806').
narrative_ontology:cs_reading_relation('1a2b77e5-4bea-4846-b678-78a53780c806', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a2b77e5-4bea-4846-b678-78a53780c806', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('1a2b77e5-4bea-4846-b678-78a53780c806', foundational, outcome_specification_suffices_for_interoperability).
narrative_ontology:cs_axiom_status(outcome_specification_suffices_for_interoperability, holdable).
narrative_ontology:cs_axiom_grounding('1a2b77e5-4bea-4846-b678-78a53780c806', outcome_specification_suffices_for_interoperability, conventional).
narrative_ontology:cs_axiom('1a2b77e5-4bea-4846-b678-78a53780c806', foundational, implementation_diversity_is_compatible_with_semantic_conformance).
narrative_ontology:cs_axiom_status(implementation_diversity_is_compatible_with_semantic_conformance, holdable).
narrative_ontology:cs_axiom_grounding('1a2b77e5-4bea-4846-b678-78a53780c806', implementation_diversity_is_compatible_with_semantic_conformance, empirically_contingent).
narrative_ontology:cs_reference_frame('1a2b77e5-4bea-4846-b678-78a53780c806', outcome_bound_semantic_contract).
narrative_ontology:cs_drift_state('1a2b77e5-4bea-4846-b678-78a53780c806', post_bbr_deployment_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1a2b77e5-4bea-4846-b678-78a53780c806', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, protocol_stack_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, end_users).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, congestion_control_researchers).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, specification_by_outcome_not_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operating-system and network-stack vendors (Linux, Windows, BSD-derived stacks) implement TCP's state machine and choose their own congestion control, buffer management, and retransmission timing strategies as long as the observable byte-stream semantics — in-order, reliable, flow-controlled delivery — are preserved. They gain freedom to ship performance improvements (new congestion control algorithms, tuned retransmission behavior) without needing a new RFC or cross-vendor renegotiation for every change.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, protocol_stack_implementers, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__optimization_latitude_reading, protocol_stack_implementers, agenda_setter).

% Researchers and engineers who design algorithms like BBR and DCTCP work inside the semantic envelope the specification leaves open: they can deploy fundamentally different loss/delay response strategies as long as interoperability with RFC 9293-compliant peers is preserved. Their exit option is real — competing designs can be tried and abandoned without touching the standard.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, congestion_control_researchers, beneficiary,
    moderate, biographical, mobile, global).

% ISPs and datacenter operators benefit from a stable interoperable behavioral contract while independently tuning stack behavior (buffer sizing, ECN handling, congestion signaling) to their network conditions. They can adopt vendor-specific optimizations selectively without waiting on standards-body ratification of implementation details.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, beneficiary,
    organized, biographical, mobile, global).

% Applications and end users experience the guaranteed outcome (reliable, ordered byte delivery) regardless of which implementation or optimization strategy sits underneath. They never interact with the latitude directly; they receive its benefit (continuous performance improvement) without bearing its coordination cost.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, end_users, beneficiary,
    powerless, immediate, mobile, global).

% The IETF working group and RFC 9293 authors drew the boundary between what must be invariant (the state machine's externally observable guarantees) and what is left to implementer discretion (internal algorithms, timing heuristics, buffer strategies). They maintain this boundary through the RFC text itself and subsequent clarifying RFCs, but exercise no ongoing enforcement — compliance is voluntary and market-tested through interoperability, not policed.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, ietf_tsvwg_and_authors, agenda_setter,
    institutional, civilizational, analytical, global).

% Older or minimal TCP implementations that cannot adopt newer optimizations (due to embedded constraints, unmaintained codebases, or hardware limits) still benefit from interoperability but cannot participate in the latitude the specification grants — they are structurally present in the ecosystem but have no voice in how the optimization space evolves.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, legacy_implementations, excluded,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees that any two RFC 9293-compliant endpoints can exchange a reliable, ordered, flow-controlled byte stream regardless of internal implementation choices, solving the genuine cross-vendor interoperability problem without requiring every implementer to use identical internal algorithms.
% TRANSFER_FUNCTION: Moves almost nothing coercively between parties — it distributes discretion outward from the specification to individual implementers, converting a potential single-point bottleneck (mandating one congestion control algorithm) into a shared invariant core plus a competitive optimization periphery.
% ABSENT_VOICES: Legacy or resource-constrained implementations that cannot adopt newer optimizations have no seat in shaping how the latitude is used, though they still receive its interoperability guarantee; middlebox operators whose devices inspect and sometimes rewrite TCP behavior are not bound by this reading's contract at all, which is exactly the tension the sibling middlebox_realism_reading addresses.
% DISAPPEARANCE_RATIONALE: If the semantic-contract-with-latitude arrangement disappeared and were replaced by either total silence (no contract) or a fully rigid single-implementation mandate, the internet's ability to evolve congestion control and performance techniques (as with BBR's large-scale deployment) without breaking interoperability would collapse — either fragmentation (no shared contract) or ossification (no room for innovation) would follow.
% FOUNDING_PROBLEM: Early internet-scale TCP deployment needed multiple independent implementations (from different vendors and research groups) to interoperate reliably while still permitting performance research and evolution, without forcing a single canonical implementation on every stack.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the direct beneficiary set by the sustained multi-decade record of independently-developed, interoperating TCP stacks (Linux, Windows, BSD, embedded stacks) and by IETF working-group deliberation records showing explicit intent to separate 'must produce this outcome' from 'must use this mechanism'; network measurement studies of real-world congestion-control diversity (documenting BBR, CUBIC, Reno coexistence) provide independent empirical attestation that the latitude is exercised, not merely theoretical.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__optimization_latitude_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__optimization_latitude_reading_tests).
:- end_tests(rfc9293_tcp_specification__optimization_latitude_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) and essentially flat across the interval because no party captures rent from the coordination structure itself — the specification's discretion-preserving design is the coordination function, not a cover for extraction. Suppression is low (0.05) because compliance is market-tested through interoperability failure, not policed by any enforcement body; an implementation that violates the semantic contract simply fails to interoperate and loses adoption, which is a natural consequence rather than an imposed sanction. Theater ratio stays near zero because there is essentially no performative compliance layer — the specification's stated function (interoperable byte-stream delivery) is what actually gets delivered. Accessibility collapse is moderate (0.2), reflecting that once an implementer commits to TCP as the transport, the state-machine semantics genuinely constrain design space, but multiple implementation paths remain fully viable within that constraint — this is not a mountain-grade collapse.
 *
 * DIRECTIONALITY LOGIC:
 *   All active parties in this reading sit near the beneficiary end of directionality: implementers, researchers, operators, and end users all gain from the coordination without a corresponding extraction target. This is structurally distinct from a snare or tangled rope precisely because no beneficiary group's gain is another party's identifiable, asymmetric loss — legacy implementations are excluded from participating in the optimization space but are not extracted from; they simply do not benefit from the periphery while still receiving the coordination core. No victims are declared because the structural analysis under this reading finds no party paying a cost through the mechanism the way a tangled rope or snare would require.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for interoperable byte-stream delivery across independently developed implementations) remains fully live in 2024 exactly as in 1981 — the scale has grown enormously but the coordination need has not disappeared or been supplanted. This is what prevents the classification from drifting toward piton: the mandate has not outlived its function, and there is no atrophied core being maintained by inertia. The specification's continued relevance is corroborated by ongoing active innovation (BBR's large-scale rollout, ECN/DCTCP adoption) that depends on the latitude the specification preserves, not despite it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    outcome_means_boundary_stability,
    'Is the boundary between ''must-preserve outcome'' and ''implementer''s discretion over means'' itself stable, or does it silently shift as new optimization techniques (e.g., ECN-dependent congestion signaling) begin to depend on behaviors the original specification treated as pure implementation detail?',
    'Trace IETF errata and companion RFCs (e.g., RFC 8985 for RACK, RFC 3168 for ECN) to see whether they clarify or quietly relocate the outcome/means boundary as new optimizations are standardized.',
    'If the boundary drifts to accommodate new optimizations after the fact, the latitude reading may be retrofitting legitimacy onto de facto standardization pressure rather than describing a stable original design choice — this would not change the current low ε but would bear on how durable the reading is going forward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outcome_means_boundary_stability, conceptual, 'Whether the semantic-contract boundary is a fixed design commitment or a boundary that migrates with de facto optimization adoption.').

omega_variable(
    latitude_reading_vs_middlebox_reality,
    'Does the optimization_latitude_reading''s assumption of a clean semantic contract survive contact with the middlebox_realism_reading''s claim that path-dependent middlebox behavior partially determines what ''TCP'' actually does on the wire, independent of what either endpoint implements?',
    'Compare measured behavior of optimization techniques (e.g., ECN, window scaling) across paths with and without middlebox interference; where interference is common, the latitude the specification ''grants'' may be latitude in name that is not actually exercisable end-to-end.',
    'If middlebox interference substantially constrains which optimizations are practically deployable, the optimization_latitude_reading''s low ε is accurate for the specification-as-written but may overstate the latitude''s real-world exercisability — this bears on the influences edge between the two readings, not on this reading''s own ε, which remains scoped to the specification-level claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latitude_reading_vs_middlebox_reality, empirical, 'Whether specification-granted latitude is fully exercisable given real deployed middlebox populations.').

omega_variable(
    strict_invariance_foreclosure_check,
    'Does treating implementation-level congestion control as discretionary (this reading) logically foreclose the strict_invariance_reading''s claim that the full state machine, including timing behavior, must be replicated exactly?',
    'Examine whether any deployed congestion-control variant has ever caused an interoperability failure attributable to state-machine divergence rather than mere performance difference — if none has, the two readings are describing different layers (semantic core vs. performance periphery) and can coexist rather than foreclose.',
    'If no such failure exists, coexists_with is the correct relation; if congestion-control choice has in practice caused protocol-level interoperability breaks, the invariance reading''s stronger claim would be partially vindicated and the relation should be revisited toward influences or tension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_invariance_foreclosure_check, empirical, 'Whether implementation latitude over congestion control is compatible with strict state-machine invariance, or whether the two readings actually contradict each other in deployed practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 1981, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1981, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1981, 0.03).
narrative_ontology:measurement(rfc9_tr_t1999, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1999, 0.03).
narrative_ontology:measurement(rfc9_tr_t2009, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2009, 0.04).
narrative_ontology:measurement(rfc9_tr_t2017, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2017, 0.05).
narrative_ontology:measurement(rfc9_tr_t2022, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2022, 0.05).
narrative_ontology:measurement(rfc9_tr_t2024, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1981, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1981, 0.05).
narrative_ontology:measurement(rfc9_be_t1999, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1999, 0.05).
narrative_ontology:measurement(rfc9_be_t2009, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2009, 0.06).
narrative_ontology:measurement(rfc9_be_t2017, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2017, 0.07).
narrative_ontology:measurement(rfc9_be_t2022, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2022, 0.08).
narrative_ontology:measurement(rfc9_be_t2024, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2024, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(rfc9293_tcp_specification__optimization_latitude_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__optimization_latitude_reading, 0.02).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the rfc9293_tcp_specification kernel. optimization_latitude_reading (this file) claims a low-extraction Rope built on outcome/means separation. strict_invariance_reading claims the specification mandates exact state-machine replication, treating this reading's 'discretion' as a misreading of what must actually be invariant. middlebox_realism_reading claims specification authority is subordinate to deployed middlebox behavior, treating the semantic contract as aspirational rather than descriptive of what 'TCP' does end-to-end. Each has its own ε, its own beneficiary/victim structure (this one has no victims; the others may differ), and its own classification. Network edges here mark structural influence, not equivalence — a change in real-world middlebox prevalence (middlebox_realism_reading) could erode confidence in this reading's clean outcome/means boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
