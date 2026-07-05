% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__strict_invariance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: rfc9293_tcp_specification__strict_invariance_reading
 *   human_readable: RFC 9293 TCP State Machine — Strict Invariance Reading
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   This story instantiates the strict_invariance_reading of the RFC 9293
 *   kernel: the claim that the TCP state machine is a single invariant target
 *   that all conforming implementations must replicate exactly, with any
 *   deviation classified as a specification violation rather than a
 *   legitimate alternative. Under this reading the constraint is a Rope — a
 *   coordination requirement whose entire justification is enabling universal
 *   interoperability without bilateral negotiation, with negligible
 *   extraction because no party collects rent from the invariance requirement
 *   itself. This is a genuinely distinct constraint from its siblings, not a
 *   different observable of the same one: the optimization_latitude_reading
 *   treats the specification as bounding behavioral outcomes while permitting
 *   internal implementation freedom (a looser, more permissive constraint
 *   with different victim sets), and the middlebox_realism_reading treats the
 *   specification's authority as subordinate to deployed network reality
 *   (potentially a tangled_rope or even snare, since middleboxes that violate
 *   the specification for traffic-shaping reasons might extract value at the
 *   expense of endpoint correctness). Per the epsilon-invariance principle,
 *   these are three separate files with three separate epsilon values, linked
 *   by kernel structure, not one story averaged across readings.
 *
 * KEY AGENTS:
 *   - internet_standards_body: agenda_setter (institutional/analytical) — administers the specification text
 *   - protocol_implementers: beneficiary (organized/mobile) — build stacks against the invariant reference
 *   - interoperating_endpoint_operators: beneficiary/payer (organized/mobile) — gain predictability, pay implementation cost
 *   - implementations_relying_on_strict_conformance: payer (moderate/constrained) — bear breakage when peers deviate
 *   - network_protocol_researchers: observer (analytical/analytical) — treat exact replication as the premise for formal guarantees
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__strict_invariance_reading, 0.06).
domain_priors:suppression_score(rfc9293_tcp_specification__strict_invariance_reading, 0.18).
domain_priors:theater_ratio(rfc9293_tcp_specification__strict_invariance_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__strict_invariance_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__strict_invariance_reading, "RFC 9293 TCP State Machine — Strict Invariance Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__strict_invariance_reading, "network_protocol_engineering/internet_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__strict_invariance_reading, 'ca6b1b85-e674-49bc-82a6-828eee162618').
narrative_ontology:cs_kernel_codification('ca6b1b85-e674-49bc-82a6-828eee162618', formalized).
narrative_ontology:cs_authority_grounding('ca6b1b85-e674-49bc-82a6-828eee162618', expertise).
narrative_ontology:cs_interpretation_layer_present('ca6b1b85-e674-49bc-82a6-828eee162618').
narrative_ontology:cs_reading_relation('ca6b1b85-e674-49bc-82a6-828eee162618', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca6b1b85-e674-49bc-82a6-828eee162618', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('ca6b1b85-e674-49bc-82a6-828eee162618', foundational, state_machine_is_literal_conformance_target).
narrative_ontology:cs_axiom_status(state_machine_is_literal_conformance_target, holdable).
narrative_ontology:cs_axiom_grounding('ca6b1b85-e674-49bc-82a6-828eee162618', state_machine_is_literal_conformance_target, conventional).
narrative_ontology:cs_axiom('ca6b1b85-e674-49bc-82a6-828eee162618', secondary, specification_authority_is_self_executing).
narrative_ontology:cs_axiom_status(specification_authority_is_self_executing, holdable).
narrative_ontology:cs_axiom_grounding('ca6b1b85-e674-49bc-82a6-828eee162618', specification_authority_is_self_executing, instrumental).
narrative_ontology:cs_reference_frame('ca6b1b85-e674-49bc-82a6-828eee162618', rfc793_original_specification_baseline).
narrative_ontology:cs_drift_state('ca6b1b85-e674-49bc-82a6-828eee162618', post_middlebox_proliferation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ca6b1b85-e674-49bc-82a6-828eee162618', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, protocol_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, interoperating_endpoint_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, internet_standards_body).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, implementations_relying_on_strict_conformance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, interoperating_endpoint_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The IETF process produced RFC 9293 as the authoritative consolidation of TCP behavior, obsoleting RFC 793 and its accumulated errata. It administers the specification text and the errata/update process but does not enforce compliance directly — conformance is voluntary and self-interested, not policed.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, internet_standards_body, agenda_setter,
    institutional, civilizational, analytical, global).

% Operating system vendors and network stack authors write TCP implementations against the invariant state machine. They benefit because a single, exactly-replicated reference removes the need to negotiate behavior bilaterally with every peer they might ever talk to — the coordination gain is the entire value proposition. They can choose to deviate at the cost of interoperability, so exit exists but is self-defeating.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, protocol_implementers, beneficiary,
    organized, generational, mobile, global).

% Organizations running servers and clients get connections that behave predictably across the entire internet because every conforming stack replicates the same state transitions. They pay the modest engineering cost of implementing the full state machine correctly (including corner cases like simultaneous close and TIME-WAIT) rather than a simplified subset.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, interoperating_endpoint_operators, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__strict_invariance_reading, interoperating_endpoint_operators, payer).

% Embedded and legacy stacks that implemented the state machine exactly as specified, with no defensive tolerance for deviation, break when they encounter a peer or intermediary that does not replicate the invariant transitions bit-for-bit. Under the strict reading, this breakage is correctly attributed to the deviating party, but the conforming implementation still bears the practical cost of the failed connection with no recourse beyond a bug report against the other side.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, implementations_relying_on_strict_conformance, payer,
    moderate, biographical, constrained, global).

% Study the state machine's formal properties (safety, liveness, exactly-once close semantics) and treat exact replication as the premise that makes those proofs meaningful. From this seat the specification's value is inseparable from universal, literal compliance.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, network_protocol_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one authoritative, exactly specified state machine so that any two independently written TCP implementations, developed decades apart by unrelated organizations, can establish and tear down a connection correctly without ever negotiating behavior out-of-band.
% TRANSFER_FUNCTION: Moves engineering effort from the transaction (bilateral negotiation of connection semantics between every pair of communicating hosts) into the specification (one-time, front-loaded correctness work amortized across the entire global implementer population). No party extracts rent through this reading; the transfer is effort-shifting, not wealth-shifting.
% ABSENT_VOICES: Implementers of deliberately non-conforming or 'lenient' stacks that accept malformed sequences for robustness are not represented in this reading's framework — the strict reading treats their leniency as a category error (protocol violation), not a legitimate alternative design philosophy, so their objection is structurally out of scope here rather than merely unheard.
% DISAPPEARANCE_RATIONALE: If the invariant state machine were abandoned as a binding target, implementers would each choose their own interpretation of connection semantics; multi-vendor interoperability would degrade unpredictably wherever implementations diverged, and every pairwise connection would require either bilateral testing or defensive fallback logic — the coordination the specification currently provides for free would have to be reconstructed piecemeal by each pair of communicating parties.
% FOUNDING_PROBLEM: Early TCP implementations (pre-RFC 793 and its long errata tail) diverged on edge-case behavior — simultaneous open/close, retransmission timing, sequence number wraparound — producing interoperability failures between independently developed stacks as the network scaled past a small trusted set of implementers.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by the IETF TCPM working group's documented rationale for consolidating decades of errata and interoperability reports into RFC 9293, and by independent academic protocol-conformance testing literature showing implementations still diverge on state-machine corner cases absent a strict invariant reference; no corroborating source disputes that the coordination problem remains live, though the middlebox_realism and optimization_latitude readings dispute whether strict textual invariance is still the right solution to it.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__strict_invariance_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__strict_invariance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__strict_invariance_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__strict_invariance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__strict_invariance_reading, 0.06, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.06) and essentially flat across the interval because under the strict reading no party collects rent from the invariance requirement — it is a pure coordination cost, amortized once in the specification rather than repeatedly in bilateral negotiation. Suppression (0.18) is modest and reflects only the specification's own internal logic (undefined/erroneous behavior is disallowed, not that alternative implementations are coercively blocked from existing — they simply fail to interoperate, which is a natural consequence, not an enforcement mechanism). Accessibility collapse is comparatively high (0.72) because once an implementer understands the state machine, the 'alternative' of inventing a different connection-establishment protocol collapses in practice — you either replicate the invariant machine or you are not speaking TCP to the rest of the internet. Resistance is low (0.22): implementers overwhelmingly comply because the coordination benefit dominates any cost of exact replication; the theater ratio stays low and only creeps slightly upward (0.02 to 0.08) as later-era conformance testing and certification suites add some process overhead around what remains a substantively functional requirement.
 *
 * PERSPECTIVAL GAP:
 *   From the standards body and protocol-implementer seats, the invariant state machine is unambiguously coordination-positive infrastructure. From the seat of an implementation that built strict, non-defensive conformance assumptions, the same invariance requirement becomes a source of fragility whenever ANY other party in the ecosystem deviates — not because the constraint extracts from them, but because it offers no tolerance for the deviations of others. This is a structural asymmetry the engine should register as different exposure to the same low-extraction constraint, not as evidence the constraint is itself extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Protocol implementers and endpoint operators sit near the beneficiary end: they get universal interoperability essentially for free once they've paid the one-time cost of correct implementation, and they retain mobile exit (a vendor can choose not to build a TCP stack, or can choose a different transport protocol, without being trapped). The standards body sits in an agenda-setting but non-extractive position — it does not collect from the specification's operation, consistent with the Rope reading's premise. The victim group under this reading is narrower and more particular than in the sibling readings: only implementations that built brittle strict-conformance assumptions (no defensive tolerance for peer deviation) bear a real cost, and that cost falls on them specifically because OTHER parties fail to replicate the invariant machine — the constraint itself does not extract from them, but its invariance premise leaves no slack for handling the deviations that violate it. This is a much smaller and differently-shaped victim set than the middlebox_realism_reading would name.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (divergent early implementations breaking interoperability) remains live by the corroborating IETF working-group record and independent conformance-testing literature, so this reading shows no mandatrophy: the coordination function the invariant state machine exists to serve is still operative, not vestigial. The strict reading's classification as a clean Rope with negligible extraction and no institutional capture is precisely what distinguishes it from a mislabeled-extraction case — there is no beneficiary skimming rent off the invariance requirement, only a genuine unresolved coordination problem (universal interoperability among independently written stacks) that the state machine continues to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    invariance_versus_outcome_framing,
    'Does RFC 9293 actually mandate an invariant STATE MACHINE (transition-by-transition replication), or does it mandate a set of observable OUTCOMES (in-order reliable delivery, correct connection lifecycle) achievable by multiple internally distinct state machines? The strict_invariance_reading and optimization_latitude_reading disagree on exactly this point, and the disagreement is located in how RFC 9293''s prose describing ''the'' state machine (Section 3.3) is read relative to its stated purpose of ensuring interoperability rather than implementation uniformity.',
    'Textual and historical analysis of IETF TCPM working group deliberation records: did the working group intend the state diagram as a literal conformance target, or as a canonical illustrative model with permitted internal variation? Cross-reference against actual accepted-conformant implementations (e.g., differing retransmission and congestion-control internals coexisting as RFC 9293-conformant) to see which reading the standards body''s own enforcement behavior (accepting/rejecting implementations) actually supports.',
    'If the outcome-oriented reading is correct, this strict_invariance_reading''s classification as the sole legitimate coordination structure is too narrow, and much of what this story labels as ''deviation'' would need to be reclassified as within-bounds optimization, shrinking the victim set to near zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invariance_versus_outcome_framing, conceptual, 'Whether RFC 9293''s kernel commits to state-machine literalism or outcome equivalence — the fault line between this reading and optimization_latitude_reading.').

omega_variable(
    middlebox_authority_subordination,
    'Given that a substantial fraction of internet paths pass through middleboxes (NATs, firewalls, TCP-terminating proxies) that do not implement RFC 9293''s state machine at all, is the specification''s claimed authority over ''global interoperability'' descriptively accurate, or is compliance with the strict reading a minority condition that happens to work because endpoints defensively tolerate deviation the specification does not sanction?',
    'Empirical internet measurement studies (e.g., large-scale TCP behavior fingerprinting across deployed middleboxes) quantifying what fraction of real-world connections traverse a fully RFC 9293-conformant path end-to-end versus a path with at least one non-conformant intermediary.',
    'If most real connections traverse at least one non-conformant middlebox, the strict_invariance_reading describes an idealized minority case, and the middlebox_realism_reading''s claim that specification authority is subordinate to deployed network reality would be empirically favored, though the two readings remain structurally distinct constraints regardless of which better predicts observed traffic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_authority_subordination, empirical, 'Whether strict textual conformance describes the actual operative internet or an idealized subset of it.').

omega_variable(
    fsm_naturalness_of_coordination_claim,
    'Is the invariant state machine''s status as pure, non-extractive coordination (Rope) genuinely accurate, or does it function as cover for the standards body''s institutional authority — i.e., does treating the specification as an inevitable, natural coordination requirement obscure that the IETF/TCPM working group benefits (in standing, relevance, and control over the specification''s future evolution) from the strict reading being accepted as canonical?',
    'Examine whether the standards body captures any resource, influence, or gatekeeping benefit from the strict reading being dominant (e.g., control over certification, influence over vendor roadmaps) versus whether its role is purely custodial with no such capture.',
    'If the standards body captures meaningful institutional benefit from strict-reading dominance beyond the coordination function itself, the beneficiary declaration on internet_standards_body would need scrutiny as a potential false-summit signal rather than a purely custodial role.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fsm_naturalness_of_coordination_claim, conceptual, 'Whether declaring the standards body a beneficiary reflects genuine custodial coordination or masks institutional rent-seeking, relevant because this reading declares beneficiaries on a constraint claimed as low-extraction coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__strict_invariance_reading, 1981, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1981, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1981, 0.02).
narrative_ontology:measurement(rfc9_tr_t1990, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1990, 0.03).
narrative_ontology:measurement(rfc9_tr_t2001, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2001, 0.05).
narrative_ontology:measurement(rfc9_tr_t2010, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2010, 0.06).
narrative_ontology:measurement(rfc9_tr_t2022, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2022, 0.08).
narrative_ontology:measurement(rfc9_tr_t2024, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1981, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1981, 0.03).
narrative_ontology:measurement(rfc9_be_t1990, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1990, 0.04).
narrative_ontology:measurement(rfc9_be_t2001, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2001, 0.05).
narrative_ontology:measurement(rfc9_be_t2010, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement(rfc9_be_t2022, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2022, 0.06).
narrative_ontology:measurement(rfc9_be_t2024, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2024, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(rfc9293_tcp_specification__strict_invariance_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__strict_invariance_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__strict_invariance_reading, 0.02).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints instantiating readings of the rfc9293_tcp_specification kernel. strict_invariance_reading (this file) claims Rope with negligible extraction and a narrow victim set (brittle strict-conformance implementations only). optimization_latitude_reading claims outcome-bounded coordination with broader implementer freedom. middlebox_realism_reading claims the specification's authority is conditional on deployed network reality, likely yielding a tangled_rope or higher-extraction classification since middleboxes may extract value (e.g., traffic shaping, surveillance insertion) at the expense of endpoint-specified correctness. Each carries its own epsilon; they are linked here rather than merged, per the epsilon-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
