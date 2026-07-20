% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__middlebox_realism_reading, []).

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
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: RFC 9293 Middlebox Realism Reading: Specification Subordinated to Deployed Middlebox Population
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   This constraint is the middlebox_realism_reading of the contested kernel
 *   rfc9293_tcp_specification. The kernel is RFC 9293, the TCP specification,
 *   which presents itself as an authoritative endpoint behavior standard.
 *   This reading holds that real TCP is path-dependent and shaped by the
 *   deployed middlebox population (NATs, firewalls, DPI, proxies); the
 *   specification is aspirational because middleboxes actively modify, drop,
 *   and reinterpret traffic, and specification authority is subordinate to
 *   what the network actually does. The constraint coordinates global
 *   interoperability in name while extracting control from endpoints in
 *   practice. Sibling readingsâstrict_invariance_reading and
 *   optimization_latitude_readingâare instantiated as separate constraints.
 *
 * KEY AGENTS:
 *   - middlebox_operators (institutional/mobile): Primary beneficiariesâISPs and enterprises that operate packet-modifying infrastructure and resist transport encryption.
 *   - state_surveillance_infrastructure (institutional/constrained): Secondary beneficiaryârelies on middlebox visibility for lawful intercept and censorship.
 *   - network_equipment_vendors (institutional/arbitrage): Tertiary beneficiaryâprofit from selling middlebox hardware whose value depends on continued meddling.
 *   - tcp_endpoints (moderate/constrained): Primary targetsâend hosts that must accept modification or lose global reachability.
 *   - application_developers (moderate/constrained): Secondary targetsâbear the engineering costs of coding around middlebox interference.
 *   - ietf_standards_body (institutional/constrained): Agenda setter with aspirational authorityâmaintains the specification but cannot enforce it against the deployed base.
 *   - quic_proponents (organized/mobile): Excluded voicesâadvocate for encrypted transports that would restore endpoint autonomy and destroy the middlebox value proposition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.72).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.78).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "RFC 9293 Middlebox Realism Reading: Specification Subordinated to Deployed Middlebox Population").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering/internet_standards").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '1c47a7ef-4d50-41dd-b63d-fcb5087a43ff').
narrative_ontology:cs_kernel_codification('1c47a7ef-4d50-41dd-b63d-fcb5087a43ff', formalized).
narrative_ontology:cs_authority_grounding('1c47a7ef-4d50-41dd-b63d-fcb5087a43ff', practice).
narrative_ontology:cs_reading_relation('1c47a7ef-4d50-41dd-b63d-fcb5087a43ff', rfc9293_tcp_specification__strict_invariance_reading, influences).
narrative_ontology:cs_reading_relation('1c47a7ef-4d50-41dd-b63d-fcb5087a43ff', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_axiom('1c47a7ef-4d50-41dd-b63d-fcb5087a43ff', foundational, deployed_path_as_operative_standard).
narrative_ontology:cs_axiom_status(deployed_path_as_operative_standard, holdable).
narrative_ontology:cs_axiom_grounding('1c47a7ef-4d50-41dd-b63d-fcb5087a43ff', deployed_path_as_operative_standard, empirically_contingent).
narrative_ontology:cs_axiom('1c47a7ef-4d50-41dd-b63d-fcb5087a43ff', foundational, network_operator_modification_authority).
narrative_ontology:cs_axiom_status(network_operator_modification_authority, holdable).
narrative_ontology:cs_axiom_grounding('1c47a7ef-4d50-41dd-b63d-fcb5087a43ff', network_operator_modification_authority, conventional).
narrative_ontology:cs_reference_frame('1c47a7ef-4d50-41dd-b63d-fcb5087a43ff', ietf_authoritative_specification).
narrative_ontology:cs_drift_state('1c47a7ef-4d50-41dd-b63d-fcb5087a43ff', contemporary_internet, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1c47a7ef-4d50-41dd-b63d-fcb5087a43ff', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_infrastructure).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, network_equipment_vendors).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, tcp_endpoints).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, application_developers).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, traffic_shaping_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy and operate firewalls, NATs, proxies, and deep-packet inspection systems that inspect, modify, drop, or rate-limit TCP packets. They extract policy enforcement, surveillance compliance, and traffic-management capacity from the divergence between RFC 9293's endpoint-centric model and operational reality. They resist transport innovations (e.g., encrypted headers) that would restore endpoint autonomy and render their equipment obsolete.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators, beneficiary,
    institutional, generational, mobile, global).

% Relies on middlebox visibility into TCP flows for lawful intercept, censorship, metadata extraction, and traffic analysis. Benefits from the fact that the deployed network preserves inspection points that RFC 9293's ideal end-to-end model would eliminate. Their operational continuity depends on the constraint's active enforcement by middlebox operators.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_infrastructure, beneficiary,
    institutional, generational, constrained, global).

% Manufacture and sell the middlebox hardware and software that implements the de facto protocol by modifying traffic. Profit from the gap between specification and reality by selling DPI, NAT, and firewall products whose value proposition assumes continued meddling in transport-layer semantics.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, network_equipment_vendors, beneficiary,
    institutional, biographical, arbitrage, global).

% Implement TCP stacks according to RFC 9293 but must deviate or accept modification by middleboxes to achieve global connectivity. Path MTU discovery is routinely broken, TCP options are stripped, and congestion signals are rewritten by middleboxes. They cannot opt out without losing reachability to large portions of the Internet.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, tcp_endpoints, payer,
    moderate, biographical, constrained, global).

% Build applications assuming end-to-end TCP semantics, but must code defensively against middlebox interference: conservative option usage, TLS fingerprint randomization, complex retry logic, and NAT traversal hacks. They bear the engineering and reliability costs of the specification-reality divergence.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, application_developers, payer,
    moderate, biographical, constrained, global).

% Authors and maintains RFC 9293 describing ideal endpoint behavior. Must temper specifications to accommodate known middlebox pathologies, documenting workarounds rather than enforcing the original end-to-end architecture. Their formal authority is aspirational because the deployed network determines what is actually viable.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_standards_body, agenda_setter,
    institutional, generational, constrained, global).

% Design and advocate for QUIC and other encrypted transports that restore endpoint autonomy by making transport headers opaque to middleboxes. Treated as external disruptive forces by incumbent operators and equipment vendors; their proposals threaten the middlebox ecosystem and are resisted through deployment barriers and standards-process opposition.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, quic_proponents, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__middlebox_realism_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__middlebox_realism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a globally interoperable reliable byte-stream abstraction across heterogeneous autonomous networks by standardizing endpoint state-machine behavior, sequence numbering, and congestion control.
% TRANSFER_FUNCTION: Transfers control over packet semantics, option processing, timing, and flow visibility from endpoints and applications to middlebox operators, surveillance infrastructure, and network-equipment vendors, under the cover of a shared specification that remains nominally endpoint-centric.
% ABSENT_VOICES: End users who assume their TCP connections are private and end-to-end; peer-to-peer and real-time application developers whose architectures are broken by NAT and DPI; encrypted-transport designers whose solutions are stonewalled by operator resistance; civil-society advocates for net neutrality who are not seated in the IETF operational community.
% DISAPPEARANCE_RATIONALE: If middleboxes ceased modifying traffic and the specification became fully enforceable, NAT traversal libraries would become unnecessary, TCP options and extensions would become usable, application designs would simplify, encrypted transports would deploy faster, and the surveillance and policy-enforcement infrastructure would lose its passive inspection points. The global Internet protocol stack would reorganize toward the specified end-to-end ideal.
% FOUNDING_PROBLEM: Achieving reliable, ordered, and congestion-responsive data delivery across unreliable packet-switched networks with heterogeneous link characteristics, endpoint implementations, and administrative boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Internet historians and early RFC authors (e.g., Postel, Cerf) attest the founding problem was reliable transport. Network operators and security practitioners attest the current middlebox layer addresses distinct problemsâaddress scarcity, perimeter security, and regulatory complianceârather than the original transport problem. QUIC Working Group documents and independent measurement studies (e.g., IMC, SIGCOMM) corroborate that middlebox ossification is now an independent obstacle to transport evolution, not a solution to the founding problem.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because middleboxes extract policy control and surveillance capacity from every flow, transferring semantic authority from endpoints to network operators. Suppression (0.78) is higher still because endpoints cannot unilaterally reject middlebox modification without losing connectivity; the constraint is enforced by the topology of the deployed network. Theater_ratio (0.60) is elevated because the IETF continues to publish and update RFC 9293 as if it governed the network, while operational reality is determined by middlebox behaviorâmuch of standards activity becomes performative maintenance of an authority fiction. Accessibility_collapse (0.80) is high: once an engineer understands that middleboxes determine what works, the alternative of strict RFC compliance collapses because it fails in the wild. Resistance (0.55) reflects the partial pushback via QUIC, TLS 1.3, and encrypted DNS, which have gained traction but face deployment barriers from incumbent operators.
 *
 * PERSPECTIVAL GAP:
 *   From the middlebox operator and equipment-vendor seats, the constraint is necessary network management and security hygieneâgenuine coordination without which the Internet would fragment or collapse into abuse. From the endpoint and application-developer seats, the same structure reads as extraction of control by a network layer that has captured transport semantics for surveillance and policy enforcement. The IETF seat experiences cognitive dissonance: it authors an ideal model that it knows is systematically overridden. The engine computes this divergence from the structural beneficiary/victim asymmetry and the differential exit options (operators can change their boxes; endpoints cannot escape them).
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox operators, state surveillance, and equipment vendors are structural beneficiaries: they collect control and revenue from the gap between specification and reality, so their directionality sits near the beneficiary end (low d). TCP endpoints and application developers are structural targets: they pay the cost in lost autonomy and defensive engineering, with constrained exit, so their directionality sits near the target end (high d). The IETF is an agenda setter whose limited enforcement power gives it a mid-range d; it is neither a primary beneficiary of extraction nor a direct target, but its authority is undermined by the constraint it documents. QUIC proponents are excluded and mobileâtheir high exit options and outsider status place them near the analytical edge.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâreliable transport across unreliable networksâhas been solved for decades. The middlebox layer persists not to solve that original problem but to address address scarcity, perimeter security, and policy enforcement. This is a classic mandatrophy risk: a coordination mechanism outlives its founding problem and accumulates extraction. However, because the constraint still carries a genuine coordination function (global TCP interoperability is real and not trivially replaceable), the classification stops at Tangled Rope rather than Snare. A pure Snare would have no remaining coordination value; here, the middleboxes do mediate connectivity across heterogeneous networks, even as they extract control. The theater_ratio captures the growing distance between the coordination story and the extraction reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_subordination_degree,
    'To what degree is RFC 9293 specification authority actually subordinate to deployed middlebox behavior, versus middleboxes gradually adapting to specification updates?',
    'Longitudinal analysis of TCP option deployment rates post-RFC publication versus middlebox pass-through rates; measurement of RFC 8312 (CUBIC) and RFC 9000 (QUIC) adoption barriers attributable to middlebox intolerance.',
    'If specifications can still drive deployment, the constraint is less extractive than this reading claims; if middleboxes consistently veto standards evolution, the reading is confirmed and the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_subordination_degree, empirical, 'Whether deployed middlebox population dominates specification authority.').

omega_variable(
    middlebox_coordination_extraction_boundary,
    'Do middleboxes perform a genuine coordination function (address scarcity mitigation, DDoS protection, congestion management) that justifies their packet modification, or is their interference primarily extractive control (surveillance, policy enforcement, rent-seeking)?',
    'Audit of middlebox modification types across major autonomous systems: classify NAT, security filtering, traffic shaping, and DPI by necessity to basic network function versus policy or surveillance goals.',
    'If the majority of modifications are necessary for basic connectivity, the constraint may be a Rope or Tangled Rope with strong coordination; if majority is policy enforcement or surveillance, the Snare classification becomes more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_coordination_extraction_boundary, conceptual, 'Boundary between middlebox coordination function and extraction.').

omega_variable(
    kernel_reading_contest,
    'Does the middlebox realism reading foreclose the strict invariance reading, or can they coexist as descriptions of different layers (specification vs. implementation)?',
    'Formal analysis of whether an endpoint can simultaneously hold the strict state-machine view (for compliance testing) and the path-dependent middlebox view (for operational engineering) without logical contradiction.',
    'If coexisting, the readings describe different constraints linked by network affects; if foreclosing, they are rival commitment systems within the same kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Logical relationship between middlebox realism and strict invariance readings.').

omega_variable(
    endpoint_exit_structural_or_internalized,
    'Is the endpoint inability to escape middlebox modification a structural constraint (no alternative viable paths to global connectivity) or internalized (developers have accepted middlebox interference as natural and do not attempt alternatives even where available)?',
    'Measure QUIC and TCP Fast Open adoption rates as natural experiments in endpoint exit; if adoption surges where paths exist, constraint is structural; if developers avoid features despite availability, internalization is present.',
    'Structural suppression confirms high extractiveness via external enforcement; internalized suppression means effective extraction exceeds the structural measure because endpoints carry the constraint even where paths are open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endpoint_exit_structural_or_internalized, empirical, 'Structural versus internalized suppression of endpoint autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rfc9_tr_t8, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(rfc9_tr_t16, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(rfc9_tr_t24, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(rfc9_tr_t32, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 32, 0.58).
narrative_ontology:measurement(rfc9_tr_t40, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 40, 0.6).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(rfc9_be_t8, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(rfc9_be_t16, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(rfc9_be_t24, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(rfc9_be_t32, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(rfc9_be_t40, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(rfc9_su_t8, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(rfc9_su_t16, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(rfc9_su_t24, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(rfc9_su_t32, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(rfc9_su_t40, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
