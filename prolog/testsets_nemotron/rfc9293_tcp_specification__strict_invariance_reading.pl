% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__strict_invariance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__strict_invariance_reading, []).

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
 *   constraint_id: rfc9293_tcp_specification__strict_invariance_reading
 *   human_readable: RFC 9293 TCP Strict Invariant Specification Reading
 *   domain: technological/distributed_systems/network_protocols
 *
 * SUMMARY:
 *   RFC 9293 (the TCP bis document obsoleting RFC 793) specifies an invariant
 *   state machine for the Transmission Control Protocol. The
 *   strict_invariance_reading holds that the specification defines an exact,
 *   reproducible state machine that all conforming implementations must
 *   replicate precisely. Deviations — whether from middlebox intervention,
 *   implementation optimization, or intentional divergence — are violations
 *   that degrade global interoperability. This reading treats the
 *   specification as a pure coordination mechanism: the cost of compliance is
 *   the cost of participation in the global Internet, and no party extracts
 *   rent from the constraint's operation. The beneficiary set includes all
 *   participants in the Internet ecosystem who gain reliable, interoperable
 *   transport. The constraint has no victims under this reading — any
 *   implementation that cannot comply is simply non-conforming, not
 *   victimized.
 *
 * KEY AGENTS:
 *   - protocol_implementers: Primary beneficiaries (powerful/arbitrage) — gain interoperable transport by implementing the specification exactly
 *   - network_operators: Beneficiaries (organized/arbitrage) — operate infrastructure that relies on predictable TCP behavior
 *   - application_developers: Beneficiaries (moderate/arbitrage) — build on reliable byte-stream abstraction without protocol concerns
 *   - end_users: Beneficiaries (powerless/arbitrage) — receive working Internet applications as downstream beneficiaries
 *   - middlebox_vendors: Excluded (powerful/trapped) — their products modify TCP in ways this reading classifies as violations
 *   - standards_observers: Observers (analytical/analytical) — analyze specification compliance and interoperability outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__strict_invariance_reading, 0.02).
domain_priors:suppression_score(rfc9293_tcp_specification__strict_invariance_reading, 0.15).
domain_priors:theater_ratio(rfc9293_tcp_specification__strict_invariance_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__strict_invariance_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__strict_invariance_reading, "RFC 9293 TCP Strict Invariant Specification Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__strict_invariance_reading, "technological/distributed_systems/network_protocols").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__strict_invariance_reading, 'efd367d3-b27e-4040-869c-9808a8e54858').
narrative_ontology:cs_kernel_codification('efd367d3-b27e-4040-869c-9808a8e54858', formalized).
narrative_ontology:cs_authority_grounding('efd367d3-b27e-4040-869c-9808a8e54858', expertise).
narrative_ontology:cs_interpretation_layer_present('efd367d3-b27e-4040-869c-9808a8e54858').
narrative_ontology:cs_reading_relation('efd367d3-b27e-4040-869c-9808a8e54858', rfc9293_tcp_specification__middlebox_realism_reading, forecloses).
narrative_ontology:cs_reading_relation('efd367d3-b27e-4040-869c-9808a8e54858', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('efd367d3-b27e-4040-869c-9808a8e54858', foundational, specification_is_exact_state_machine).
narrative_ontology:cs_axiom_status(specification_is_exact_state_machine, holdable).
narrative_ontology:cs_axiom_grounding('efd367d3-b27e-4040-869c-9808a8e54858', specification_is_exact_state_machine, conventional).
narrative_ontology:cs_axiom('efd367d3-b27e-4040-869c-9808a8e54858', foundational, middlebox_modification_is_violation).
narrative_ontology:cs_axiom_status(middlebox_modification_is_violation, holdable).
narrative_ontology:cs_axiom_grounding('efd367d3-b27e-4040-869c-9808a8e54858', middlebox_modification_is_violation, conventional).
narrative_ontology:cs_axiom('efd367d3-b27e-4040-869c-9808a8e54858', secondary, interoperability_requires_exact_conformance).
narrative_ontology:cs_axiom_status(interoperability_requires_exact_conformance, holdable).
narrative_ontology:cs_axiom_grounding('efd367d3-b27e-4040-869c-9808a8e54858', interoperability_requires_exact_conformance, empirically_contingent).
narrative_ontology:cs_reference_frame('efd367d3-b27e-4040-869c-9808a8e54858', rfc9293_formalized_state_machine).
narrative_ontology:cs_drift_state('efd367d3-b27e-4040-869c-9808a8e54858', post_rfc9293_publication, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('efd367d3-b27e-4040-869c-9808a8e54858', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, protocol_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, network_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, application_developers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, end_users).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__strict_invariance_reading, interoperability_through_specification_fidelity).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__strict_invariance_reading, postel_law_reversal).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__strict_invariance_reading, ossification_prevention_via_strict_compliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement TCP stacks (Linux, BSD, Windows, embedded) for OS distributions and network equipment. Gain interoperable transport by following the specification exactly. Can choose alternative transports (QUIC, UDP) or implement non-compliant variants, but the coordination value of strict compliance is high. No party extracts rent from their compliance — the IETF process is open and the specification is royalty-free.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, protocol_implementers, beneficiary,
    powerful, biographical, arbitrage, global).

% Operate the routers, links, and middleboxes that carry TCP traffic. Benefit from predictable, standard-compliant endpoint behavior that their infrastructure can process efficiently. Can deploy alternative transports or optimize for specific TCP variants, but the universal coordination good of a single reliable transport specification outweighs customization.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, network_operators, beneficiary,
    organized, generational, arbitrage, global).

% Build applications on top of TCP's reliable byte-stream abstraction. Do not implement the protocol themselves but depend on OS-provided stacks conforming to the specification. Gain the ability to write network applications without protocol expertise. Exit is arbitrage-grade: can use QUIC, WebTransport, or application-layer reliability over UDP.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, application_developers, beneficiary,
    moderate, biographical, arbitrage, global).

% Use Internet applications that ultimately rely on TCP. Benefit from the global interoperability the specification enables. No direct interaction with the constraint; exit is not meaningful at this level (users do not choose transports). Included as ultimate downstream beneficiaries of the coordination good.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, end_users, beneficiary,
    powerless, biographical, arbitrage, global).

% Build NATs, firewalls, load balancers, and proxies that modify TCP headers and state (sequence numbers, window scaling, timestamps). This reading classifies their modifications as specification violations. They are structurally excluded from the beneficiary set because their products' function depends on deviating from the invariant state machine. They cannot exit the constraint's judgment without changing their product architecture — they are trapped in violation.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, middlebox_vendors, excluded,
    powerful, biographical, trapped, global).

% Researchers, IETF participants, and analysts who study TCP compliance, interoperability, and evolution. Neither collect from nor pay into the constraint. Provide the analytical seat from which the constraint's structural properties are assessed.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, standards_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__strict_invariance_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__strict_invariance_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, universal, exact specification for the TCP state machine so that any two conforming implementations can interoperate without negotiation, probing, or pairwise compatibility testing. Solves the N² interoperability problem for reliable transport by reducing it to N independent conformance tests against a fixed specification.
% TRANSFER_FUNCTION: Moves implementation effort (building a correct TCP stack) from each implementer into the shared specification artifact. No money, status, or attention transfers between parties — the specification is a public good maintained by the IETF. The 'cost' is the engineering effort to implement the state machine exactly; the 'benefit' is interoperability with all other conforming implementations.
% ABSENT_VOICES: Middlebox vendors and operators who depend on TCP modification would object to the strict invariance framing — they are excluded from the IETF consensus process that produces the specification, and their deployed base creates structural pressure against strict compliance. They are not in the room when the specification is ratified.
% DISAPPEARANCE_RATIONALE: If the strict invariance constraint vanished overnight (no specification, no conformance expectation), TCP implementations would diverge into incompatible dialects within months. Middlebox behavior would become the de facto standard. Application developers would lose the reliable byte-stream abstraction. The Internet would fragment into isolated TCP variants or shift entirely to QUIC. Global interoperability would collapse.
% FOUNDING_PROBLEM: Early Internet (pre-RFC 793) had no universal reliable transport: NCP was host-specific, proprietary protocols fragmented the network, and applications could not assume interoperable communication across administrative boundaries.
% FOUNDING_PROBLEM_CORROBORATION: The IETF transport area (TSVWG) continues to maintain and extend TCP (RFC 9293, RFC 9294 for ACCECN, ongoing work on RACK/TLP/BPB). The problem of universal reliable transport remains live — QUIC addresses it for HTTP but TCP remains the substrate for most non-HTTP traffic. No party outside the beneficiary set disputes that the problem persists.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__strict_invariance_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__strict_invariance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__strict_invariance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(rfc9293_tcp_specification__strict_invariance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__strict_invariance_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__strict_invariance_reading_tests).
:- end_tests(rfc9293_tcp_specification__strict_invariance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02): the specification is maintained by the IETF through open consensus; no party collects from compliance. Compliance cost is the cost of building a TCP stack — a coordination cost, not an extraction. Suppression is low (0.15): non-conforming implementations exist and interoperate partially (e.g., through middlebox accommodation), but the specification itself does not coerce — it defines the contract. Theater is negligible (0.05): the specification's function (interoperability) is its actual operation. Accessibility collapse is high (0.88): alternative transport protocols exist (QUIC, SCTP) but TCP's network effect makes deviation from its state machine costly for interoperability. Resistance is low (0.08): the constraint is welcomed by implementers as the coordination mechanism. The measurement grid uses RFC milestones (1981 RFC 793, 1988 Van Jacobson congestion control, 2012 RFC 6298 RTO revision, 2022 RFC 9293) as the shared time points.
 *
 * PERSPECTIVAL GAP:
 *   From the strict_invariance_reading's analytical seat, the constraint is a pure rope: universal coordination with no extraction. From the middlebox_realism_reading's seat (a sibling reading of the same kernel), the same specification functions as a snare — middleboxes extract value from endpoints by forcing accommodation of their modifications. From the optimization_latitude_reading's seat, the specification is a tangled_rope — coordination with extraction from implementers who must invest in strict compliance while others optimize freely. The engine computes this divergence from the structural data across readings.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries (implementers, operators, developers, users) have arbitrage-grade exit: they can choose alternative transports (QUIC, UDP-based protocols) or different network layers. The specification does not trap them — it offers a coordination good they voluntarily adopt. Directionality for all beneficiary seats derives toward d ≈ 0.1 (near-beneficiary). No victim seats declared. Middlebox vendors are excluded — they are not governed by the specification (they are network-path elements, not endpoints) and their non-compliance is not a cost the specification imposes on them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reliable inter-process communication over unreliable networks) remains live — the Internet still requires a universal reliable transport. The specification has evolved (congestion control, ECN, RACK, TLP) but the invariant state machine core persists. No mandatrophy: the constraint's function matches its mandate. The rising suppression_requirement over time (0.05 → 0.15) reflects growing middlebox ossification pressure, not mandate drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Does the strict_invariance_reading represent the actual operative commitment of RFC 9293, or is it one of several competing readings of the same kernel?',
    'Trace the IETF standardization process: examine WG minutes, author intent, and the formal status of the bis document relative to RFC 793. Determine whether the IETF community treats strict invariance as the binding interpretation or as a contested claim among implementers.',
    'If the strict reading is the sole authoritative interpretation, the constraint is a genuine rope. If it is one of multiple live readings, the kernel is contested and the constraint is one reading among others — the ε and type apply only to this reading''s instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether the strict invariance framing is the unique authoritative reading or one of several competing readings of the rfc9293_tcp_specification kernel').

omega_variable(
    middlebox_adaptation_vs_violation,
    'Are deployed middlebox behaviors (NAT, firewalls, load balancers modifying TCP fields) violations of the strict invariance constraint, or have they become de facto extensions that the constraint must accommodate?',
    'Longitudinal measurement of middlebox behavior prevalence vs. interoperability failure rates. If strict implementations fail in production while ''flexible'' ones succeed, the constraint''s coordination function is undermined by structural reality.',
    'If middlebox modification is structurally inevitable and coordination depends on accommodating it, the strict_invariance_reading''s claimed rope function collapses — the constraint becomes a tangled_rope (coordination + extraction: strict implementations pay for middlebox tolerance) or snare (middleboxes extract from endpoints).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_adaptation_vs_violation, empirical, 'Whether middlebox behavior constitutes violation of or adaptation to the strict invariance constraint').

omega_variable(
    optimization_latitude_boundary,
    'Where does the boundary lie between permissible implementation optimization (e.g., RACK, TLP, BBR) and violation of the invariant state machine?',
    'Analyze RFC 9293''s normative language for ''MAY/SHOULD/MUST'' on specific state transitions. Survey major TCP stacks (Linux, BSD, Windows) for divergence points that preserve interoperability.',
    'If the boundary admits significant optimization latitude without interoperability loss, the strict_invariance_reading overstates the constraint''s rigidity — the true constraint is closer to optimization_latitude_reading. If optimizations that preserve semantics are violations, the strict reading is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(optimization_latitude_boundary, conceptual, 'Whether optimization within semantic bounds is permitted by or violates the invariant state machine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__strict_invariance_reading, 1981, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_strict_tr_t1981, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1981, 0.02).
narrative_ontology:measurement(rfc9293_strict_tr_t1988, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1988, 0.03).
narrative_ontology:measurement(rfc9293_strict_tr_t2012, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2012, 0.04).
narrative_ontology:measurement(rfc9293_strict_tr_t2022, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2022, 0.05).

% Extraction over time
narrative_ontology:measurement(rfc9293_strict_be_t1981, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1981, 0.01).
narrative_ontology:measurement(rfc9293_strict_be_t1988, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1988, 0.01).
narrative_ontology:measurement(rfc9293_strict_be_t2012, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2012, 0.02).
narrative_ontology:measurement(rfc9293_strict_be_t2022, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2022, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(rfc9293_strict_su_t1981, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 1981, 0.05).
narrative_ontology:measurement(rfc9293_strict_su_t1988, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 1988, 0.1).
narrative_ontology:measurement(rfc9293_strict_su_t2012, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2012, 0.12).
narrative_ontology:measurement(rfc9293_strict_su_t2022, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 2022, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__strict_invariance_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__strict_invariance_reading, 0.02).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__middlebox_realism_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, quic_transport_protocol).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, tcp_congestion_control_algorithms).

% DUAL FORMULATION NOTE:
% Part of the rfc9293_tcp_specification constraint family. The strict_invariance_reading treats the specification as an invariant state machine (rope, ε≈0.02). The middlebox_realism_reading treats the deployed middlebox population as the de facto constraint (snare/tangled_rope, ε≈0.4-0.6). The optimization_latitude_reading treats semantic compliance as the constraint (tangled_rope, ε≈0.1-0.2). The three readings share the kernel text but instantiate different constraints with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
