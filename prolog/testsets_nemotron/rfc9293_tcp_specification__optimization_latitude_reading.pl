% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__optimization_latitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: rfc9293_tcp_specification__optimization_latitude_reading
 *   human_readable: TCP Specification Optimization Latitude (RFC 9293 Reading)
 *   domain: network_protocol_engineering/internet_standards/distributed_systems_coordination
 *
 * SUMMARY:
 *   RFC 9293 (the 2022 consolidation of TCP specifications) specifies the
 *   semantic contract of TCP — reliable, in-order byte-stream delivery —
 *   while explicitly permitting implementation latitude in how that contract
 *   is achieved. Congestion control algorithms, loss recovery mechanisms, and
 *   pacing strategies are not mandated; implementers may deploy CUBIC, BBR,
 *   DCTCP, Copa, or future innovations provided they preserve the observable
 *   semantics. This reading treats the specification as a coordination
 *   mechanism (Rope) that enables decentralized performance innovation
 *   without breaking global interoperability. The constraint's extractiveness
 *   has declined over decades as the latitude was formalized and the
 *   ecosystem adapted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.08).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.15).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "TCP Specification Optimization Latitude (RFC 9293 Reading)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "network_protocol_engineering/internet_standards/distributed_systems_coordination").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, '47c15401-7a0b-4f99-adba-30f66d1588fc').
narrative_ontology:cs_kernel_codification('47c15401-7a0b-4f99-adba-30f66d1588fc', formalized).
narrative_ontology:cs_authority_grounding('47c15401-7a0b-4f99-adba-30f66d1588fc', expertise).
narrative_ontology:cs_interpretation_layer_present('47c15401-7a0b-4f99-adba-30f66d1588fc').
narrative_ontology:cs_reading_relation('47c15401-7a0b-4f99-adba-30f66d1588fc', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('47c15401-7a0b-4f99-adba-30f66d1588fc', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('47c15401-7a0b-4f99-adba-30f66d1588fc', foundational, semantic_contract_suffices_for_interoperability).
narrative_ontology:cs_axiom_status(semantic_contract_suffices_for_interoperability, holdable).
narrative_ontology:cs_axiom_grounding('47c15401-7a0b-4f99-adba-30f66d1588fc', semantic_contract_suffices_for_interoperability, empirically_contingent).
narrative_ontology:cs_axiom('47c15401-7a0b-4f99-adba-30f66d1588fc', foundational, algorithmic_diversity_improves_global_performance).
narrative_ontology:cs_axiom_status(algorithmic_diversity_improves_global_performance, holdable).
narrative_ontology:cs_axiom_grounding('47c15401-7a0b-4f99-adba-30f66d1588fc', algorithmic_diversity_improves_global_performance, instrumental).
narrative_ontology:cs_reference_frame('47c15401-7a0b-4f99-adba-30f66d1588fc', rfc793_single_algorithm_mandate).
narrative_ontology:cs_drift_state('47c15401-7a0b-4f99-adba-30f66d1588fc', rfc9293_consolidation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('47c15401-7a0b-4f99-adba-30f66d1588fc', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, protocol_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, end_users).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_vendors).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, interoperability_through_outcome_specification).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, performance_innovation_without_breaking_compatibility).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, decentralized_protocol_evolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop TCP stacks for operating systems, kernels, and user-space libraries. The specification's outcome-oriented contract gives them freedom to implement congestion control (CUBIC, BBR, DCTCP), loss recovery, and pacing algorithms without seeking standards-body approval for each variant. They benefit from a large installed base that interoperates regardless of their internal choices. Exit is mobile — they can switch implementations or contribute to multiple stacks.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, protocol_implementers, beneficiary,
    organized, generational, mobile, global).

% Experience faster, more reliable connections as implementers deploy optimized congestion control and loss recovery. They do not choose the TCP implementation directly (it comes with their OS/device) but benefit from the innovation the latitude enables. Exit is constrained — they cannot easily change the transport protocol their applications use, but the protocol's universality means they rarely need to.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, end_users, beneficiary,
    organized, biographical, constrained, global).

% Operate the routers, middleboxes, and links that carry TCP traffic. They benefit from a protocol that adapts to diverse network conditions without requiring coordinated upgrades. They bear operational costs when new congestion control variants interact poorly with their traffic management (e.g., BBR vs. bufferbloat, ECN marking policies). Their exit is constrained by the protocol's universality — they cannot replace TCP, but they can deploy AQM, ECN, and pacing to shape its behavior.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, payer).

% Build firewalls, NATs, load balancers, and WAN optimizers that inspect or modify TCP headers. The optimization latitude means they must handle a wider range of valid TCP behaviors (e.g., varied window scaling, selective acknowledgments, congestion control signals) without breaking flows. They pay in engineering complexity and ongoing firmware updates. Their exit is constrained — the installed base of middleboxes must interoperate with evolving endpoint implementations.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_vendors, payer,
    institutional, biographical, constrained, global).

% Steward the TCP specification through the IETF standards process. They maintain the semantic contract (reliable, ordered byte stream) while adjudicating whether proposed changes preserve interoperability. They do not extract from the constraint; they administer its evolution. Their exit is analytical — they observe the system from the standards-body seat.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, rfc_editor_iesg, agenda_setter,
    institutional, generational, analytical, global).

% Design and evaluate new congestion control algorithms (BBR, Copa, PCC, etc.). The optimization latitude is their research substrate — it permits deploying novel algorithms without standards-track approval. They observe the constraint's operation but do not administer or pay for it directly.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, congestion_control_researchers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates interoperability across independently developed TCP implementations by specifying only the observable behavioral contract (reliable, in-order byte delivery) while leaving the internal algorithms (congestion control, loss recovery, pacing) to implementers. This solves the collective-action problem of global deployment: no single party controls all endpoints, but all must interoperate.
% TRANSFER_FUNCTION: Transfers implementation freedom from a hypothetical central authority (which would dictate algorithms) to distributed implementers. No material transfer occurs — the constraint prevents extraction by any single party over the protocol's evolution. The 'gain' is the innovation velocity enabled by permissionless algorithm deployment.
% ABSENT_VOICES: Legacy middlebox deployments that ossify the protocol (e.g., boxes that drop packets with unknown TCP options, or that reset connections on non-standard congestion signals). These are not represented in the IETF process but their behavior constrains what optimization latitude is practically usable. Also absent: users on severely pathologically managed networks (captive portals, heavy-handed traffic shaping) where the protocol's semantic guarantees are violated by the path, not the endpoints.
% DISAPPEARANCE_RATIONALE: If the optimization latitude vanished — if RFC 9293 mandated a single congestion control algorithm and exact state-machine replication — the TCP ecosystem would lose the ability to adapt to new network conditions (datacenter, wireless, satellite, high-BDP paths) without a coordinated global standards action. Innovation would shift to application-layer transports (QUIC) or require a new transport protocol. The current arrangement enables continuous, decentralized evolution.
% FOUNDING_PROBLEM: Early TCP (RFC 793) specified a single congestion control algorithm (Tahoe/Reno) that became inadequate for high-bandwidth, high-latency, and wireless paths. The Internet needed a way to evolve transport performance without breaking global interoperability or requiring flag-day upgrades.
% FOUNDING_PROBLEM_CORROBORATION: The IETF TSVWG and TCPM working groups explicitly charter congestion control as an area for continuous innovation (RFC 8087, RFC 8312). Independent research (Cardwell et al. on BBR, Alizadeh et al. on DCTCP, the IRTF ICCRG) demonstrates ongoing deployment of novel algorithms within the RFC 9293 semantic bounds. No party claims the founding problem is solved; the latitude is the mechanism that keeps it live.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(rfc9293_tcp_specification__optimization_latitude_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness is low (0.08) because no party collects rents from the latitude — implementers gain performance, users gain throughput, operators gain adaptability. Suppression is low (0.15) because alternatives (QUIC, SCTP, application-layer transports) exist and are deployable; the constraint does not suppress exits. Theater ratio is low (0.12) because the specification's administrative overhead (IETF process) is small relative to the functional coordination it provides. Accessibility collapse is moderate (0.35) because the semantic contract (reliable byte stream) does constrain what transports can do — but the space of valid implementations is wide. Resistance is low (0.25) because the latitude is broadly supported by implementers and researchers; the only resistance comes from middlebox ossification, which is a path property, not a specification property.
 *
 * PERSPECTIVAL GAP:
 *   From the implementer seat, the constraint is a genuine rope — they coordinate on outcomes, innovate on means. From the middlebox vendor seat, the same latitude appears as a cost (handling diverse valid behaviors) — but they are payers by virtue of their position in the path, not because the specification extracts from them. From the end-user seat, the constraint is invisible infrastructure that simply works better over time. The engine will compute these per-seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Protocol implementers are structural beneficiaries (d ~ 0.1): they gain implementation freedom and a global interoperability surface. End users are beneficiaries (d ~ 0.15): they gain performance without action. Network operators are dual-positioned: beneficiaries of adaptability (d ~ 0.2), payers of operational complexity (d ~ 0.4). Middlebox vendors are payers (d ~ 0.6): they bear compatibility costs from the latitude. The RFC Editor/IESG is the agenda setter (d ~ 0.5): they administer but do not extract. Researchers are observers (d ~ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (interoperable reliable transport) remains live and the coordination function (outcome specification enabling algorithmic diversity) is the active mechanism — not an atrophied remnant. The specification has not become theatrical; the IETF process actively maintains it while preserving the latitude. No mandatrophy is present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    middlebox_ossification_practical_latitude,
    'Does deployed middlebox behavior practically reduce the optimization latitude below what the specification permits, making the constraint''s coordination function partially illusory?',
    'Longitudinal measurement of TCP option tolerance, ECN propagation, and congestion signal handling across the global middlebox population (e.g., via traceroute-style probes, QUIC vs. TCP differential measurement).',
    'If middleboxes effectively forbid certain valid optimizations (e.g., dropping packets with unrecognized options, resetting on ECN), the practical latitude is narrower than the specified latitude — the constraint''s rope function is degraded by path ossification, not by the specification itself. This would increase effective extractiveness for implementers (who must work around middleboxes) and shift the classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_ossification_practical_latitude, empirical, 'Whether path ossification makes the specified latitude partially theoretical.').

omega_variable(
    quic_displacement_trajectory,
    'Will QUIC''s encrypted transport model (which moves congestion control to user space and hides it from middleboxes) displace TCP''s optimization latitude as the primary substrate for transport innovation?',
    'Track QUIC adoption share, TCP congestion control deployment velocity, and whether new algorithms deploy first on QUIC or TCP over the next decade.',
    'If QUIC becomes the dominant innovation substrate, TCP''s optimization latitude becomes a legacy coordination mechanism — the constraint persists but its functional relevance declines, potentially shifting toward piton. If TCP and QUIC co-evolve, the latitude remains live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quic_displacement_trajectory, empirical, 'Whether the constraint''s coordination function will be superseded by a competing transport.').

omega_variable(
    reading_relations_ambiguity,
    'Does the optimization_latitude_reading genuinely coexist with strict_invariance_reading and middlebox_realism_reading, or does it structurally foreclose one of them within a single implementation framework?',
    'Analyze whether a single TCP implementation can simultaneously honor the semantic contract (this reading), replicate an invariant state machine (strict_invariance_reading), and defer to middlebox behavior (middlebox_realism_reading) — or whether satisfying one reading''s core premise requires violating another''s.',
    'If the readings foreclose each other within a single framework, the kernel''s contest is a genuine logical partition (different implementation philosophies cannot be reconciled). If they coexist, the kernel supports pluralism — different parties hold different readings while the protocol functions. This determines the reading_relations values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_ambiguity, conceptual, 'Structural relationship between the three declared readings of the RFC 9293 kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 1981, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1981, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1981, 0.05).
narrative_ontology:measurement(rfc9_tr_t1990, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(rfc9_tr_t2000, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(rfc9_tr_t2010, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(rfc9_tr_t2016, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2016, 0.11).
narrative_ontology:measurement(rfc9_tr_t2022, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2022, 0.12).
narrative_ontology:measurement(rfc9_tr_t2026, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2026, 0.12).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1981, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1981, 0.25).
narrative_ontology:measurement(rfc9_be_t1990, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(rfc9_be_t2000, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(rfc9_be_t2010, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement(rfc9_be_t2016, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2016, 0.1).
narrative_ontology:measurement(rfc9_be_t2022, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2022, 0.09).
narrative_ontology:measurement(rfc9_be_t2026, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2026, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t1981, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 1981, 0.1).
narrative_ontology:measurement(rfc9_su_t1990, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(rfc9_su_t2000, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(rfc9_su_t2010, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement(rfc9_su_t2016, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 2016, 0.15).
narrative_ontology:measurement(rfc9_su_t2022, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 2022, 0.15).
narrative_ontology:measurement(rfc9_su_t2026, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 2026, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__optimization_latitude_reading, 0.02).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__middlebox_realism_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, quic_transport_protocol).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, congestion_control_algorithms_deployment).

% DUAL FORMULATION NOTE:
% Part of the RFC 9293 kernel family (constraint_id: rfc9293_tcp_specification). This reading (optimization_latitude) treats the specification as an outcome contract enabling algorithmic diversity. The strict_invariance_reading treats it as a state-machine invariant. The middlebox_realism_reading treats it as descriptive of ideal endpoints but subordinate to path reality. All three share the same referent (RFC 9293 text) but instantiate different constraints with different ε values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rfc9293_tcp_specification__optimization_latitude_reading, institutional, 0.2).
constraint_indexing:directionality_override(rfc9293_tcp_specification__optimization_latitude_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
