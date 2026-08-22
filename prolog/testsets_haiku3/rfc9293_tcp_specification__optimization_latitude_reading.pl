% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__optimization_latitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: rfc9293_tcp_specification__optimization_latitude_reading
 *   human_readable: RFC 9293 TCP Specification: Outcome-Defined Latitude Reading
 *   domain: network_protocol_engineering/internet_standards/distributed_systems
 *
 * SUMMARY:
 *   RFC 9293 (TCP Specification) is contested as a kernel because it can be
 *   read as specifying either (1) a fixed semantic contract with flexible
 *   implementation (this reading), (2) an invariant state machine that all
 *   implementations must replicate exactly (strict_invariance_reading), or
 *   (3) an idealized behavior that deployed TCP has deviated from through
 *   middlebox adaptation (middlebox_realism_reading). This story instantiates
 *   the optimization_latitude_reading: the specification permits different
 *   congestion-control algorithms and performance optimizations as long as
 *   they preserve reliable, in-order delivery and maintain interoperability
 *   with other TCP implementations. This reading enables algorithmic
 *   innovation (BBR, DCTCP, CUBIC) without fragmenting the protocol. The
 *   reading is contentious because it transfers specification authority from
 *   prescriptive detail to behavioral outcomes, making compliance testing
 *   harder and creating space for performance-driven divergence.
 *
 * KEY AGENTS:
 *   - RFC Working Group (TCPM): maintains specification authority and decides what divergence is permitted
 *   - High-performance implementers (Google, Netflix, Linux kernel): benefit from latitude to deploy novel congestion-control algorithms
 *   - Conformance testers: must verify behavioral contract rather than code structure
 *   - Interoperability stakeholders (Internet users, application developers): benefit from diverse implementations that remain interoperable
 *   - Strict invariance advocates: excluded from decision-making but argue for tighter specification
 *   - Legacy implementations: trapped in the original algorithm, view latitude as protocol fragmentation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.18).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.12).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "RFC 9293 TCP Specification: Outcome-Defined Latitude Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "network_protocol_engineering/internet_standards/distributed_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, 'c083c12f-f637-484c-9788-ca8607aa2278').
narrative_ontology:cs_kernel_codification('c083c12f-f637-484c-9788-ca8607aa2278', fixed_text).
narrative_ontology:cs_authority_grounding('c083c12f-f637-484c-9788-ca8607aa2278', expertise).
narrative_ontology:cs_interpretation_layer_present('c083c12f-f637-484c-9788-ca8607aa2278').
narrative_ontology:cs_reading_relation('c083c12f-f637-484c-9788-ca8607aa2278', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('c083c12f-f637-484c-9788-ca8607aa2278', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('c083c12f-f637-484c-9788-ca8607aa2278', foundational, semantic_specification_sufficiency).
narrative_ontology:cs_axiom_status(semantic_specification_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('c083c12f-f637-484c-9788-ca8607aa2278', semantic_specification_sufficiency, instrumental).
narrative_ontology:cs_axiom('c083c12f-f637-484c-9788-ca8607aa2278', foundational, implementation_flexibility_doctrine).
narrative_ontology:cs_axiom_status(implementation_flexibility_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('c083c12f-f637-484c-9788-ca8607aa2278', implementation_flexibility_doctrine, conventional).
narrative_ontology:cs_reference_frame('c083c12f-f637-484c-9788-ca8607aa2278', outcome_defined_contract_framework).
narrative_ontology:cs_drift_state('c083c12f-f637-484c-9788-ca8607aa2278', contemporary_bbr_dctcp_adoption, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c083c12f-f637-484c-9788-ca8607aa2278', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, high_performance_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, congestion_control_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, conformance_testers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, interoperability_stakeholders).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, conformance_testers).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, semantic_specification_principle).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, implementation_flexibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and evolves RFC 9293 as the authoritative specification of TCP behavior. Decides what outcomes are normative and which details are implementation-permitted. Authors clarifications and errata that resolve ambiguities about permissible optimization latitude.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, rfc_working_group, agenda_setter,
    institutional, generational, arbitrage, global).

% Deploy congestion-control variants (BBR, DCTCP, CUBIC) and other performance optimizations that preserve the reliable byte stream contract while diverging from the original Reno implementation. Benefit from the latitude because they can innovate without forking the protocol or losing interoperability.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, high_performance_implementers, beneficiary,
    institutional, generational, mobile, global).

% Test that implementations produce the specified behavioral contract (reliable delivery, congestion adaptation) without mandating specific internal algorithms. They benefit from having a well-defined semantic boundary; they also pay the cost of needing to test behavior rather than code structure, which is harder than line-by-line compliance checking.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, conformance_testers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__optimization_latitude_reading, conformance_testers, payer).

% Internet users, application developers, network operators. Benefit because diverse implementations competing on performance improvements does not break end-to-end connectivity — any TCP sender interoperates with any TCP receiver as long as both honor the semantic contract.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, interoperability_stakeholders, beneficiary,
    powerful, generational, mobile, global).

% Argue that latitude in implementation creates protocol fragmentation risk and that all implementations should replicate the same state machine exactly. They are excluded from primary decision-making but maintain technical positions in forum discussions and academic literature arguing for stricter specifications.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, strict_invariance_advocates, excluded,
    moderate, biographical, constrained, global).

% Deploy packet inspection, traffic shaping, and performance optimization equipment that interacts with TCP at the wire protocol level. They observe and respond to implementation choices but do not set specification policy directly; their constraints flow back to the working group through operational reports and compatibility incidents.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_operators, observer,
    powerful, biographical, mobile, global).

% Older, conservative TCP stacks that implement only the original algorithm. They would object to aggressive optimization as protocol deviation, but they are not represented in standards bodies and their concern is historical rather than prospective.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, legacy_implementations, excluded,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__optimization_latitude_reading, high_performance_implementers).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__optimization_latitude_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines a semantic contract (reliable, in-order byte stream delivery with congestion adaptation) that all implementations must achieve, without dictating the internal algorithms or performance optimization choices. This enables heterogeneous implementations to interoperate: a sender using BBR congestion control talks to a receiver using Reno, and the contract holds.
% TRANSFER_FUNCTION: Moves authority from implementation prescriptivism to outcome specification: the specification transfers the burden of correctness from the reader ("replicate this exact state machine") to the implementer ("produce this behavioral outcome and prove it"). This is a gain in information asymmetry for sophisticated implementers who can afford custom optimization; it is a burden shift away from the specification authority.
% ABSENT_VOICES: Strict invariance advocates and legacy-stack maintainers are excluded from primary decision-making. They would argue for tighter specification coupling to prevent protocol fragmentation, but they are not in the room when congestion-control variants are approved.
% DISAPPEARANCE_RATIONALE: If this reading of permissible latitude disappeared and the specification became prescriptive (strict_invariance_reading), TCP development would slow — all new congestion-control algorithms would require protocol extension or IETF exemption rather than implementation autonomy. Without the latitude, high-performance variants (BBR, DCTCP) would either not exist or would exist as protocol extensions (QUIC-like) rather than TCP implementations.
% FOUNDING_PROBLEM: Early TCP implementations were limited in performance optimization opportunity because the specification left room for interpretation only in debugging detail; as link speeds and network scale grew, performance constraints emerged that could only be solved by algorithmic innovation in congestion control. The founding problem was: how can the protocol evolve with new congestion-control science without forking TCP or requiring specification revision for every innovation?
% FOUNDING_PROBLEM_CORROBORATION: The IETF TCPM working group's adoption of multiple congestion-control algorithms (BBR, CUBIC, DCTCP) as standards-track or informational RFCs confirms the problem persists. Academic literature on congestion control (Mathis, Jacobson, Cardwell lineages) and deployment metrics from major networks (Google, Netflix, AWS) show continuous algorithmic innovation. Independent performance analysis confirms that algorithmic latitude is necessary for modern network conditions.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__optimization_latitude_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.18) because the constraint does not require payment, does not lock in beneficiaries, and does not restrict exit — implementers can adopt the specification or ignore it; the coordination benefit is mutual (reliable byte stream serves all parties). Suppression is very low (0.12) because the constraint persists through shared interest in interoperability, not through active enforcement of compliance (no TCP police); networks route whatever TCP implementations exist as long as they interoperate. Theater is minimal (0.08) because the specification work is substantive (real coordiation problem solved) with little performative overhead. The measurement series shows extractiveness rising slightly in the early interval (early adoption of new congestion-control standards) and then stabilizing once the reading is well-established as the working norm. Suppression requirement rises slightly as compliance testing sophistication increases but remains low because voluntary coordination is the primary mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the working group's seat, this is well-calibrated coordination: permit latitude where it doesn't break the contract, enabling innovation. From strict-invariance advocates' seat, it is a retreat from specification authority and a risk to protocol stability. The engine should compute different types from these positions: beneficiary seats experience Rope (genuine coordination enabling mutual gain), while excluded/target seats experience Tangled Rope (coordination for some serves as constraint on others' control). This gap is the core measurement the engine performs.
 *
 * DIRECTIONALITY LOGIC:
 *   High-performance implementers sit near the beneficiary end (d~0.15): they get algorithmic freedom and can innovate without protocol fragmentation risk. The working group sits near symmetric (d~0.5): they maintain the specification (cost) but derive authority and influence from doing so (benefit). Interoperability stakeholders and conformance testers sit slightly beneficiary-ward because they get the coordination without bearing innovation risk. Strict invariance advocates sit target-ward (d~0.75): they pay the cost of being excluded from decisions that permit divergence they view as risky, with no offsetting benefit. The structural asymmetry is that latitude benefits those with optimization capability and hurts those seeking uniformity — the specification choice privileges innovation over uniformity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to evolve TCP congestion control without forking the protocol) remains live and is corroborated by working-group decisions, academic literature, and deployment metrics. The constraint has not undergone mandate drift: the specification remains a specification of behavioral outcomes, not a mandate to replicate implementation detail. There is no mandatrophy signal because the constraint's function and its operation remain aligned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semantic_contract_sufficiency,
    'Is the semantic contract (reliable, in-order delivery with congestion responsiveness) sufficient to guarantee interoperability across all permissible optimization variants, or do implementation divergences in corner cases (packet loss patterns, reordering, pathological RTT behavior) create incompatibilities that break the contract?',
    'Large-scale interoperability testing across diverse congestion-control algorithms and implementations under controlled adverse conditions (packet loss, reordering, latency jitter). Document any cases where two conformant implementations fail to interoperate.',
    'If the semantic contract proves insufficient, latitude must be narrowed — strict_invariance_reading becomes the necessary reading. If the contract holds across all tested variants, latitude is vindicated and optimization_latitude_reading is the stable reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_contract_sufficiency, empirical, 'Whether behavioral specification suffices for interoperability without prescriptive implementation detail.').

omega_variable(
    innovation_velocity_dependency,
    'Does TCP performance innovation fundamentally depend on implementation latitude within the semantic contract, or can all necessary optimization advances be achieved through RFC extensions (like TCP-friendly congestion control RFCs) without modifying core TCP?',
    'Historical analysis of congestion-control evolution: compare the rate of algorithm adoption (BBR, DCTCP, CUBIC) under the latitude reading with the rate of TCP extension adoption. If extensions accommodate innovation at comparable velocity, latitude is not essential; if extensions lag significantly, latitude is foundational to innovation.',
    'If innovation requires latitude, the optimization_latitude_reading is structurally necessary to the protocol''s relevance. If extensions suffice, latitude is a convenience that could be traded for stricter specification without innovation loss.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(innovation_velocity_dependency, empirical, 'Whether algorithmic innovation depends on core TCP flexibility or can be accommodated through extensions.').

omega_variable(
    specification_ambiguity_as_de_facto_latitude,
    'To what extent does the permissible latitude arise from explicit specification choice versus from ambiguities or underspecified details in RFC 9293 that implementers exploit?',
    'Detailed reading of RFC 9293 errata and working-group discussions identifying sections that have been clarified or narrowed over time. Compare errata that tightened specification to errata that explicitly permitted latitude.',
    'If latitude is intentional policy (reading choice), it is defensible and stable. If latitude arises from ambiguity, it may be eliminated by clarification without changing the reading. This distinction is critical to the axioms: is optimization_latitude_reading built on a foundational principle (semantic specification) or on accidental gaps?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_ambiguity_as_de_facto_latitude, conceptual, 'Whether latitude is intentional specification design or de facto result of ambiguity.').

omega_variable(
    kernel_reading_coexistence_fragility,
    'Can the three kernel readings (optimization_latitude, strict_invariance, middlebox_realism) coexist indefinitely as live positions in standards discourse, or do deployment pressures eventually foreclose one reading?',
    'Long-term monitoring of IETF discussion patterns, RFC approvals, and deployment metrics. Observe whether any reading accumulates exclusivity or whether practical necessity forces a choice.',
    'If readings remain indefinitely coexistent (each adopted by different implementers and networks), the kernel is truly contested and the Rope classification holds. If one reading eventually forecloses others (strict_invariance forces all implementations to conform, or middlebox_realism forces specs to acknowledge deployment reality), the kernel resolves and the readings become historical variants rather than live alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_fragility, empirical, 'Stability of kernel reading coexistence under deployment pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(rfc9_tr_t0, projected).
narrative_ontology:measurement(rfc9_tr_t5, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement_basis(rfc9_tr_t5, observed).
narrative_ontology:measurement(rfc9_tr_t10, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(rfc9_tr_t10, observed).
narrative_ontology:measurement(rfc9_tr_t15, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(rfc9_tr_t15, observed).
narrative_ontology:measurement(rfc9_tr_t20, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(rfc9_tr_t20, observed).
narrative_ontology:measurement(rfc9_tr_t25, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement_basis(rfc9_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(rfc9_be_t0, projected).
narrative_ontology:measurement(rfc9_be_t5, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement_basis(rfc9_be_t5, observed).
narrative_ontology:measurement(rfc9_be_t10, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement_basis(rfc9_be_t10, observed).
narrative_ontology:measurement(rfc9_be_t15, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement_basis(rfc9_be_t15, observed).
narrative_ontology:measurement(rfc9_be_t20, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(rfc9_be_t20, observed).
narrative_ontology:measurement(rfc9_be_t25, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement_basis(rfc9_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(rfc9_su_t0, projected).
narrative_ontology:measurement(rfc9_su_t5, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement_basis(rfc9_su_t5, observed).
narrative_ontology:measurement(rfc9_su_t10, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement_basis(rfc9_su_t10, observed).
narrative_ontology:measurement(rfc9_su_t15, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement_basis(rfc9_su_t15, observed).
narrative_ontology:measurement(rfc9_su_t20, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(rfc9_su_t20, observed).
narrative_ontology:measurement(rfc9_su_t25, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement_basis(rfc9_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__optimization_latitude_reading, 0.05).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__middlebox_realism_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, congestion_control_algorithm_innovation).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, tcp_interoperability_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel rfc9293_tcp_specification. The kernel constraint family decomposes into three structurally distinct constraints: (1) optimization_latitude_reading (this file) — specification defines semantic contract, permits algorithmic divergence; (2) strict_invariance_reading — specification mandates invariant state machine that all implementations must replicate; (3) middlebox_realism_reading — specification describes idealized endpoint behavior, real TCP is shaped by deployed middlebox population. Each reading has distinct beneficiary/victim structures and extractiveness profiles. They coexist as live alternatives in IETF discourse but would foreclose each other if global TCP adoption enforced one reading exclusively. Epsilon values differ because the readings instantiate different referents: optimization_latitude ε is the cost of ensuring interoperability while permitting algorithmic freedom; strict_invariance ε is the cost of enforcement-based protocol uniformity; middlebox_realism ε is the cost of gap between spec and practice. Network edges link family members (each reading influences all others) and downstream constraints that depend on which reading is adopted (congestion control innovation, interoperability certification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
