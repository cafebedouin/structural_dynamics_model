% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__optimization_latitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: RFC 9293 — Semantic Contract with Implementation Latitude
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   This story instantiates the optimization_latitude reading of the RFC 9293
 *   kernel: the specification is read as a behavioral contract (reliable
 *   byte-stream delivery, defined state-machine transitions) that binds only
 *   at the level of observable outcomes, leaving implementers free to choose
 *   internal mechanisms — retransmission timers, congestion-control
 *   algorithms, buffer strategies — so long as the wire-visible contract
 *   holds. Under this reading the coordination function is doing almost all
 *   the structural work: heterogeneous vendors converge on interoperability
 *   without converging on implementation, and that latitude is precisely what
 *   has allowed decades of performance innovation (Reno, CUBIC, BBR, DCTCP)
 *   to be deployed without renegotiating the standard. This is a Rope
 *   reading, not a Tangled Rope or Mountain: there is no identifiable victim
 *   class paying a structural cost through this arrangement, and no active
 *   enforcement mechanism coerces compliance — adoption is voluntary and
 *   self-interested because interoperability is a precondition for a stack's
 *   usefulness at all.
 *
 * KEY AGENTS:
 *   - protocol_stack_implementers: organized/mobile — benefit from design freedom within the semantic floor
 *   - network_operators: organized/mobile — deploy variant algorithms suited to local traffic
 *   - application_developers: moderate/mobile — consume the stable byte-stream abstraction
 *   - internet_users: powerless/constrained — diffuse beneficiaries of invisible performance gains
 *   - ietf_tcpm_working_group: institutional/analytical — maintains the semantic/mechanism boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.08).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.12).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "RFC 9293 — Semantic Contract with Implementation Latitude").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "network_protocol_engineering/internet_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, 'f3a040d4-0680-47b4-a0b6-946f1e4791b8').
narrative_ontology:cs_kernel_codification('f3a040d4-0680-47b4-a0b6-946f1e4791b8', formalized).
narrative_ontology:cs_authority_grounding('f3a040d4-0680-47b4-a0b6-946f1e4791b8', expertise).
narrative_ontology:cs_interpretation_layer_present('f3a040d4-0680-47b4-a0b6-946f1e4791b8').
narrative_ontology:cs_reading_relation('f3a040d4-0680-47b4-a0b6-946f1e4791b8', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3a040d4-0680-47b4-a0b6-946f1e4791b8', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('f3a040d4-0680-47b4-a0b6-946f1e4791b8', foundational, means_end_separability_holds).
narrative_ontology:cs_axiom_status(means_end_separability_holds, holdable).
narrative_ontology:cs_axiom_grounding('f3a040d4-0680-47b4-a0b6-946f1e4791b8', means_end_separability_holds, empirically_contingent).
narrative_ontology:cs_axiom('f3a040d4-0680-47b4-a0b6-946f1e4791b8', secondary, voluntary_conformance_suffices_for_interoperability).
narrative_ontology:cs_axiom_status(voluntary_conformance_suffices_for_interoperability, holdable).
narrative_ontology:cs_axiom_grounding('f3a040d4-0680-47b4-a0b6-946f1e4791b8', voluntary_conformance_suffices_for_interoperability, instrumental).
narrative_ontology:cs_reference_frame('f3a040d4-0680-47b4-a0b6-946f1e4791b8', behavioral_contract_with_mechanism_freedom).
narrative_ontology:cs_drift_state('f3a040d4-0680-47b4-a0b6-946f1e4791b8', post_congestion_control_diversification_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f3a040d4-0680-47b4-a0b6-946f1e4791b8', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, protocol_stack_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, application_developers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, internet_users).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, behavioral_contract_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, means_end_separability_in_protocol_design).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operating-system and network-stack vendors (Linux, BSD, Windows) implement TCP against the RFC 9293 semantic contract — reliable, ordered, byte-stream delivery with the defined state machine transitions — but are free to choose their own congestion control, retransmission timing, and buffer management internals. They ship BBR, CUBIC, or other algorithms as competitive differentiators without needing sign-off from any body, so long as the wire-visible interoperability contract holds.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, protocol_stack_implementers, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__optimization_latitude_reading, protocol_stack_implementers, agenda_setter).

% ISPs and datacenter operators deploy whichever TCP variant best serves their traffic profile (e.g., DCTCP inside a datacenter, BBR on wide-area links) because the specification separates observable behavior from internal mechanism. They benefit directly from the latitude: performance tuning is a local decision, not a renegotiation of the standard.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, beneficiary,
    organized, biographical, mobile, global).

% Build software against the socket-level guarantee of an in-order, complete byte stream. They do not need to know or care which congestion control algorithm sits underneath; the abstraction the specification guarantees is exactly the one they consume, and switching underlying stacks costs them nothing structurally.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, application_developers, beneficiary,
    moderate, biographical, mobile, global).

% Experience faster page loads and video streams as vendors iterate on performance internals invisibly. They have no direct voice in protocol design and limited ability to choose their access provider's stack, but the latitude the specification grants is what lets performance improvements reach them without any renegotiation of interoperability they would need to understand or consent to.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, internet_users, beneficiary,
    powerless, immediate, constrained, global).

% Maintains and periodically revises the specification text, adjudicating which behavioral guarantees are load-bearing for interoperability versus which are implementation choices. Documents consensus about the semantic/mechanism boundary but does not enforce compliance directly; authority rests on voluntary adoption and rough consensus.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, ietf_tcpm_working_group, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single behavioral contract (reliable, ordered, flow-controlled byte-stream delivery over an unreliable network) that all implementations must honor at the wire, so that any two conforming stacks can interoperate regardless of internal design — solving the genuine coordination problem of letting heterogeneous vendors build independently while guaranteeing they can talk to each other.
% TRANSFER_FUNCTION: Moves almost nothing extractively: the specification transfers design freedom TO implementers (they are not required to match a reference implementation) while imposing a shared minimum behavioral floor on all of them. What is 'transferred' is coordination capacity — the ability to interoperate — not a rent from one party to another.
% ABSENT_VOICES: End users and smaller application developers are not present in IETF working-group deliberations, but their interests (speed, reliability) are structurally aligned with the specification's outcome guarantees rather than opposed to them; there is no identifiable party who would object to the latitude itself, though some might contest specific boundary calls about what counts as 'semantic' versus 'implementation.'
% DISAPPEARANCE_RATIONALE: If the behavioral contract vanished, implementers would lose the shared floor that lets independently built stacks interoperate; the internet would fragment into incompatible dialects requiring pairwise negotiation, and decades of accumulated performance innovation (which relies on the semantic/mechanism split to be deployable without renegotiating interoperability) would be jeopardized.
% FOUNDING_PROBLEM: Early internet protocol work needed a way for independently developed network stacks, built by different vendors on different hardware, to reliably exchange ordered byte streams over an unreliable, lossy packet network — while leaving room for the field to keep improving performance as hardware and network conditions changed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the IETF standards body by decades of empirical measurement studies (e.g., academic Internet-measurement literature tracking congestion-control deployment) showing continued interoperability across heterogeneous stacks despite substantial internal algorithmic divergence (Reno-family, CUBIC, BBR, DCTCP) — independent evidence that the semantic/mechanism split continues to do the coordination work it was designed for.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.08) and essentially flat across four decades because this reading identifies no rent-collection structure: the specification's latitude provision does not transfer value from a payer class to a beneficiary class, it expands the design space available to everyone bound by the same outcome guarantee. Suppression is low (0.12) — compliance is not coerced by any enforcement body; a non-conforming stack simply fails to interoperate, which is a natural consequence of the coordination problem, not an imposed penalty. Theater ratio stays near zero because the working group's activity (revising boundary calls between semantic and implementation) is substantive standards work, not performative maintenance of a hollowed-out function. accessibility_collapse is moderate (0.4), not near-zero and not near-mountain-high: alternatives to TCP altogether (QUIC, raw UDP-based protocols) are real and increasingly used, so the constraint has not achieved total closure of the option space — but within the TCP-compatible world, once you commit to a conforming stack you cannot deviate from the observable contract without breaking interoperability, so accessibility isn't trivially open either. Resistance is low (0.15): implementers do not experience the semantic floor as an imposition worth fighting; disputes are occasional and technical (e.g., debates over what counts as observable behavior for algorithms like BBR that alter timing-sensitive semantics), not adversarial contests over extraction.
 *
 * PERSPECTIVAL GAP:
 *   Under this reading there is minimal seat divergence: the agenda-setting seat (implementers who both write and consume the spec via the IETF process) and the beneficiary seats (operators, developers, users) experience largely the same structure — voluntary coordination they opt into because it serves their own interoperability and performance goals. This is itself diagnostic: a reading with near-zero payer/beneficiary asymmetry and no coercive enforcement is the Rope signature. The sibling readings (strict_invariance, middlebox_realism) would show sharper divergence — strict_invariance because it treats deviation as illegitimate regardless of benefit, middlebox_realism because it introduces an unaccountable third party (deployed middlebox behavior) that neither implementers nor the spec authors control.
 *
 * DIRECTIONALITY LOGIC:
 *   All named parties are coded as beneficiaries because, under this reading, the latitude provision structurally subsidizes everyone bound by it: implementers gain design freedom, operators gain deployment flexibility, developers gain a stable abstraction, and users gain invisible performance improvements — none of them pay a countervailing cost through this specific structural mechanism. There is no victim class to declare because the reading's own premise (means-end separability) is precisely the claim that no one is harmed by permitting variation in mechanism, so long as the outcome contract holds. If a coalition of implementers were shown to exploit ambiguity in 'observable behavior' to gain unfair competitive advantage while degrading others' interoperability, that would be evidence for a different constraint (arguably the middlebox_realism or a capture-flavored sibling), not evidence against this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (heterogeneous vendors needing to interoperate while retaining room to improve performance) is authored as still live and unambiguously served by the current arrangement — this is not a case of an atrophied mandate persisting on inertia. The specification continues to be revised (RFC 9293 itself supersedes RFC 793) precisely because the working group treats the semantic/mechanism boundary as a living, contestable line rather than a frozen relic, which is the opposite of mandatrophy. If a future measurement showed theater_ratio rising sharply (e.g., the working group producing revisions that only ratify vendor fait accompli without genuine functional re-examination), that would be the signal to revisit this classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semantic_mechanism_boundary_stability,
    'Is the line between ''observable semantic behavior'' (which the spec fixes) and ''implementation mechanism'' (which is left free) itself stable, or does it shift as new algorithms (e.g., BBR''s departure from loss-based congestion signaling) reveal that some ''mechanism'' choices actually alter externally observable behavior in ways the original drafters didn''t anticipate?',
    'Track IETF TCPM working-group deliberations and errata over algorithms that provoked boundary disputes (e.g., ECN, explicit congestion notification interactions with middleboxes, BBR fairness debates) to see whether the community treats these as clarifications of an already-fixed boundary or as genuine renegotiations of what counts as semantic versus mechanism.',
    'If the boundary is genuinely unstable and periodically renegotiated under vendor pressure, this reading''s Rope classification is more fragile than authored — repeated ad hoc boundary-shifting in favor of whichever vendor ships first would start to look like informal capture of the standards process, pushing toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_mechanism_boundary_stability, conceptual, 'Whether the semantic/mechanism split this reading depends on is a stable structural fact or a continuously renegotiated boundary.').

omega_variable(
    kernel_reading_selection_basis,
    'Among the three declared readings of the RFC 9293 kernel (optimization_latitude, strict_invariance, middlebox_realism), what determines which reading is operative for a given deployment context — is it a property of the deployment (e.g., datacenter-internal traffic favors optimization_latitude while public-internet traffic behind heavy NAT/middlebox infrastructure favors middlebox_realism), or is it a genuinely contested interpretive question with no fact of the matter?',
    'Empirical survey of measurement studies (e.g., middlebox-ossification literature) correlating the applicability of each reading with deployment topology — if optimization_latitude describes datacenter/controlled-network contexts well but middlebox_realism dominates public-internet path behavior, the three readings may be less genuinely competing and more context-indexed.',
    'If reading applicability is context-indexed rather than genuinely contested, this story''s Rope classification would need a scope qualifier (e.g., valid within administratively controlled network segments) rather than standing as a general claim about RFC 9293''s operation across the whole internet.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether the three kernel readings are genuinely rival interpretations or are each accurate within different deployment contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 1981, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1981, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1981, 0.03).
narrative_ontology:measurement(rfc9_tr_t1990, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1990, 0.03).
narrative_ontology:measurement(rfc9_tr_t1999, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1999, 0.04).
narrative_ontology:measurement(rfc9_tr_t2008, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2008, 0.04).
narrative_ontology:measurement(rfc9_tr_t2016, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2016, 0.05).
narrative_ontology:measurement(rfc9_tr_t2022, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2022, 0.05).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1981, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1981, 0.05).
narrative_ontology:measurement(rfc9_be_t1990, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(rfc9_be_t1999, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1999, 0.06).
narrative_ontology:measurement(rfc9_be_t2008, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2008, 0.07).
narrative_ontology:measurement(rfc9_be_t2016, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2016, 0.08).
narrative_ontology:measurement(rfc9_be_t2022, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2022, 0.08).

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
% This story is one of three siblings decomposing the colloquial 'RFC 9293 / TCP specification authority' claim per the ε-invariance principle. optimization_latitude_reading authors low, stable ε (~0.08) reflecting a pure-coordination structure with design freedom subsidizing all bound parties. strict_invariance_reading is expected to author higher accessibility_collapse and treat implementation divergence as illegitimate, likely still landing near Rope or Mountain but with a starkly different resistance/accessibility profile. middlebox_realism_reading is expected to author substantially higher ε and suppression, since it locates real operative authority in an unaccountable population of deployed network intermediaries whose incentives are not aligned with either spec authors or endpoint implementers — that asymmetry is the candidate tangled_rope or snare shape in the family. All three share the same kernel text (RFC 9293) but diverge in what they take the kernel's authority to actually govern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
