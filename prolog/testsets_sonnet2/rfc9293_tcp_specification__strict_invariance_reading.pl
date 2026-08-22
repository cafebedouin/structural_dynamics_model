% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__strict_invariance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   This constraint is the strict-invariance reading of the RFC 9293 TCP
 *   specification kernel: it treats the specified state machine as an exact,
 *   non-negotiable contract that every conformant implementation must
 *   replicate bit-for-bit in its transition logic, on the theory that global
 *   interoperability depends on there being exactly one reference behavior
 *   rather than a family of compatible behaviors. Under this reading any
 *   deviation — including deviation introduced by on-path middleboxes rather
 *   than by the endpoints themselves — counts as a violation of the
 *   coordination contract, and any implementation harmed by such deviation is
 *   a victim of a violation, not evidence that the invariant reading itself
 *   is unrealistic. This is a deliberately narrow reading: it does not
 *   describe what TCP implementations actually do in the presence of deployed
 *   middlebox populations (that is the middlebox_realism_reading, a separate
 *   constraint), nor does it describe the latitude the specification actually
 *   grants at the level of implementation strategy for equivalent externally
 *   observable behavior (that is the optimization_latitude_reading, also a
 *   separate constraint). Both siblings are linked via
 *   network.affects_constraints and are not part of this file's
 *   classification.
 *
 * KEY AGENTS:
 *   - protocol_conformant_implementers: primary beneficiary (organized/constrained) — gains cross-vendor interoperability by matching the invariant exactly
 *   - standards_body_ietf: agenda_setter (institutional/analytical) — publishes and maintains the specification text under this reading's literal-replication frame
 *   - implementations_broken_by_middlebox_deviation: primary target under this reading (moderate/trapped) — bears the cost when path-level deviation breaks connections despite own conformance
 *   - middlebox_vendors: excluded voice (powerful/mobile) — their modification of TCP behavior in flight has no standing in this reading's account of legitimate conformance
 *   - end_users_of_networked_applications: diffuse beneficiary (powerless/trapped) — receives working connectivity without visibility into the mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__strict_invariance_reading, 0.06).
domain_priors:suppression_score(rfc9293_tcp_specification__strict_invariance_reading, 0.28).
domain_priors:theater_ratio(rfc9293_tcp_specification__strict_invariance_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__strict_invariance_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__strict_invariance_reading, "RFC 9293 TCP State Machine — Strict Invariance Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__strict_invariance_reading, "network_protocol_engineering/internet_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__strict_invariance_reading, 'cca2630e-087d-4031-96f6-af17cece6dab').
narrative_ontology:cs_kernel_codification('cca2630e-087d-4031-96f6-af17cece6dab', formalized).
narrative_ontology:cs_authority_grounding('cca2630e-087d-4031-96f6-af17cece6dab', expertise).
narrative_ontology:cs_interpretation_layer_present('cca2630e-087d-4031-96f6-af17cece6dab').
narrative_ontology:cs_reading_relation('cca2630e-087d-4031-96f6-af17cece6dab', rfc9293_tcp_specification__middlebox_realism_reading, coexists_with).
narrative_ontology:cs_reading_relation('cca2630e-087d-4031-96f6-af17cece6dab', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('cca2630e-087d-4031-96f6-af17cece6dab', foundational, state_machine_literal_replication_required).
narrative_ontology:cs_axiom_status(state_machine_literal_replication_required, holdable).
narrative_ontology:cs_axiom_grounding('cca2630e-087d-4031-96f6-af17cece6dab', state_machine_literal_replication_required, conventional).
narrative_ontology:cs_axiom('cca2630e-087d-4031-96f6-af17cece6dab', secondary, path_modification_is_external_violation_not_specification_failure).
narrative_ontology:cs_axiom_status(path_modification_is_external_violation_not_specification_failure, holdable).
narrative_ontology:cs_axiom_grounding('cca2630e-087d-4031-96f6-af17cece6dab', path_modification_is_external_violation_not_specification_failure, conventional).
narrative_ontology:cs_reference_frame('cca2630e-087d-4031-96f6-af17cece6dab', invariant_state_machine_as_sole_conformance_criterion).
narrative_ontology:cs_drift_state('cca2630e-087d-4031-96f6-af17cece6dab', contemporary_middlebox_saturated_internet, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cca2630e-087d-4031-96f6-af17cece6dab', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, protocol_conformant_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, internet_interoperability_regime).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, standards_body_ietf).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, implementations_broken_by_middlebox_deviation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, end_users_of_networked_applications).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operating system vendors and network stack authors who implement the TCP state machine exactly as specified. They gain predictable interoperability with every other conformant peer on the internet without needing to test against every possible counterpart. Their cost is the engineering discipline of matching the spec's state transitions precisely, including edge cases that rarely fire.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, protocol_conformant_implementers, beneficiary,
    organized, generational, constrained, global).

% Maintains and publishes the specification text, adjudicates errata, and issues updates through rough consensus. Under this reading, IETF's authority is read as demanding literal replication of the state machine, not as offering behavioral latitude. It does not enforce compliance directly — conformance is voluntary and socially/technically self-selecting through interoperability testing.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, standards_body_ietf, agenda_setter,
    institutional, civilizational, analytical, global).

% Implementers who built strictly to the state machine's letter and then discover that on-path middleboxes (NAT devices, stateful firewalls, TCP proxies) rewrite or drop packets in ways the invariant reading treats as violations occurring elsewhere on the path, not as their own fault. From the strict-invariance seat these implementations are correct but suffer real breakage anyway, because the network does not universally honor the invariant the specification presumes. They cannot fix the deviating middleboxes; their only recourse is defensive workaround code that this reading treats as unwarranted deviation from the true standard.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, implementations_broken_by_middlebox_deviation, payer,
    moderate, immediate, trapped, global).

% Build and sell devices that inspect and modify TCP traffic in transit for security, translation, or performance reasons. Under the strict-invariance reading their modifications are simply violations of the specification's invariant contract; they have no standing in this reading's account of what conformance means, despite controlling a large share of deployed path behavior. Their perspective — that path modification is a legitimate deployed reality — is the subject of a sibling reading, not this one.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, middlebox_vendors, excluded,
    powerful, biographical, mobile, global).

% Ordinary users whose browsers, email clients, and apps rely on TCP connections working the same way regardless of which two endpoints on the planet are talking. They benefit from the invariant reading's coordination function without any awareness of the state machine; when strict-invariance-breaking deviation occurs it surfaces to them only as an inexplicable connection failure.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, end_users_of_networked_applications, beneficiary,
    powerless, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, exactly-replicated state machine lets any two independently built TCP implementations anywhere on the internet establish, maintain, and tear down a reliable byte-stream connection without prior negotiation about protocol semantics — coordination is achieved by fixing one invariant reference rather than requiring pairwise agreement.
% TRANSFER_FUNCTION: Under this reading nothing is transferred between parties in steady state; the constraint moves engineering discipline (the cost of exact replication) onto implementers in exchange for interoperability, and moves the cost of any deviation entirely onto whichever implementation is judged non-conformant, including deviation introduced by third parties on the path.
% ABSENT_VOICES: Middlebox vendors and network operators who modify TCP behavior in flight are excluded from this reading's account of legitimate protocol authority; their operational reality is treated as a violation rather than as a competing source of legitimacy. They would object that the strict-invariance frame ignores three decades of deployed path modification that the internet routes around, not through.
% DISAPPEARANCE_RATIONALE: If the state machine's specified invariance were abandoned as a shared reference (not merely violated in practice, but no longer treated as the canonical target), independently built implementations would lose their common contract; interoperability would depend on ad hoc bilateral testing between every vendor pair, and the low-friction any-to-any connectivity the modern internet assumes would fragment along vendor and version lines.
% FOUNDING_PROBLEM: In the early internet, independently developed host implementations needed a way to establish reliable connections over an unreliable, heterogeneous packet-switched network without each vendor needing to test against every other vendor's stack; a single, precisely specified state machine solved the pairwise-agreement combinatorics problem.
% FOUNDING_PROBLEM_CORROBORATION: Protocol conformance test suite maintainers (e.g. independent TAHI-style and academic TCP conformance testing groups) outside the IETF and outside vendor beneficiary groups attest that strict conformance to RFC 9293's state machine remains the basis for interoperability test criteria. Network operators and middlebox vendors, who are not beneficiaries under this reading, dispute that literal invariance is still achievable or even the operative standard in practice, given decades of accreted middlebox behavior the specification does not govern.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__strict_invariance_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__strict_invariance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__strict_invariance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored very low (0.06) because, under strict invariance, there is no party collecting rents from the arrangement — the cost of conformance is borne by implementers in exchange for a coordination good they genuinely receive, and the standards body extracts no toll. Suppression is moderate (0.28), reflecting the real technical and market cost of shipping a non-conformant implementation (interoperability failure, market rejection) even though no central authority coerces compliance — this is the coordination-cost floor for information_standard coordination, not extractive suppression. Theater ratio starts negligible and drifts slowly upward (0.02 to 0.10) as the specification accretes errata and clarifying text (culminating in RFC 9293's 2022 consolidation of RFC 793 plus decades of errata) that address gaps between the letter of the invariant and observed deployment reality — a small but real rise in the ratio of clarifying/defensive text to core specification. Accessibility collapse is high (0.72): once an implementer accepts the invariant-state-machine framing, there is essentially no alternative reference architecture that preserves the same interoperability guarantee — you either replicate the state machine or you accept degraded interoperability. Resistance is moderate (0.35), corresponding to the ongoing, real friction from implementers and vendors who find the strict invariant unrealistic given path behavior they cannot control.
 *
 * DIRECTIONALITY LOGIC:
 *   Protocol-conformant implementers and end users sit near the beneficiary end: they receive the coordination good (working interoperability) and pay only the ordinary cost of engineering to spec. The standards body is an agenda-setter with analytical exit — it sets the reference text but bears no direct extraction either way. Implementations broken by middlebox deviation sit at the target end specifically because, under this reading, their harm is real but structurally unaddressed: the invariant reading locates fault entirely off-path (in the middlebox) and offers the injured conformant implementation no remedy within the specification's own terms — trapped exit, no recourse. Middlebox vendors are excluded rather than positioned on the beneficiary/victim axis at all under this reading, because this reading does not grant their modification any legitimacy to evaluate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling independently built implementations to interoperate without pairwise testing — remains substantially live: the modern internet still depends on a shared reference behavior for TCP. This is not a case of an obsolete mandate persisting through inertia; RFC 9293 was itself a 2022 consolidation reaffirming and clarifying the invariant, evidence the underlying coordination function is still actively maintained rather than vestigial. The contested status in the six_questions answer reflects the KERNEL-level dispute (whether strict invariance is still the operative standard given middlebox reality), not a mandatrophy finding within this reading — this reading asserts the founding problem is live and the specification's invariance is still the correct target, while acknowledging that dispute exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    invariance_vs_deployed_reality_locus,
    'Is the correct unit of specification conformance the exact state machine (this reading) or only the externally observable outcome guarantee (the optimization_latitude sibling), and does either framing survive contact with a network path that includes non-conformant middleboxes (the middlebox_realism sibling)?',
    'Comparative empirical study of interoperability failure rates attributable to (a) genuine endpoint state-machine divergence versus (b) on-path middlebox interference, across a large sample of production TCP connections; if failures are overwhelmingly attributable to (b), the strict-invariance reading''s victim attribution (blaming path elements rather than revising the specification''s scope) is empirically well-founded, whereas if attributable to (a) it would support the optimization-latitude reading''s looser conformance bar.',
    'If path-level interference dominates, this reading''s classification of implementations_broken_by_middlebox_deviation as victims of an external violation (rather than victims of an unrealistic specification) is vindicated, keeping this constraint''s ε low. If genuine state-machine divergence between conformant endpoints dominates, that would suggest the strict-invariance reading undercounts real extraction hidden in its own conformance costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invariance_vs_deployed_reality_locus, empirical, 'Where the true source of TCP interoperability failure lies, and which kernel reading that locates fault in best matches deployed reality.').

omega_variable(
    single_reference_necessity,
    'Is a single, exactly-replicated reference state machine actually necessary for internet-scale TCP interoperability, or would a family of behaviorally-equivalent state machines (differing only in internal representation) achieve the same coordination outcome — meaning the ''strict'' framing asserts more than the coordination problem requires?',
    'Formal equivalence analysis (bisimulation or similar) between RFC 9293''s specified state machine and known-conformant alternative internal implementations that pass standard interoperability test suites; if multiple internally distinct state machines are shown behaviorally equivalent under all tested conditions, strict internal invariance is not required for the coordination function this reading claims it enables.',
    'If equivalence classes are wide, this reading''s premise (that internal state-machine replication, not just outcome equivalence, is what interoperability requires) is overclaiming, and the true coordination requirement is closer to the optimization_latitude_reading''s — which would mean this reading''s zero-extraction claim rests on an unnecessarily narrow conformance bar that manufactures apparent ''violations'' where none functionally exist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(single_reference_necessity, conceptual, 'Whether the invariant-state-machine framing states the coordination requirement precisely or overstates it relative to the optimization-latitude framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__strict_invariance_reading, 1981, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1981, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1981, 0.02).
narrative_ontology:measurement(rfc9_tr_t1999, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1999, 0.04).
narrative_ontology:measurement(rfc9_tr_t2007, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2007, 0.06).
narrative_ontology:measurement(rfc9_tr_t2015, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(rfc9_tr_t2022, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2022, 0.09).
narrative_ontology:measurement(rfc9_tr_t2025, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1981, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1981, 0.03).
narrative_ontology:measurement(rfc9_be_t1999, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1999, 0.04).
narrative_ontology:measurement(rfc9_be_t2007, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2007, 0.05).
narrative_ontology:measurement(rfc9_be_t2015, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2015, 0.05).
narrative_ontology:measurement(rfc9_be_t2022, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2022, 0.06).
narrative_ontology:measurement(rfc9_be_t2025, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2025, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(rfc9293_tcp_specification__strict_invariance_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__strict_invariance_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__strict_invariance_reading, 0.02).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__middlebox_realism_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__optimization_latitude_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'RFC 9293 specifies TCP behavior.' Each reading of the rfc9293_tcp_specification kernel is authored as a separate constraint with its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle: strict_invariance_reading (this file, near-zero extraction, pure Rope, victims are collateral casualties of path-level deviation under this reading's own account); optimization_latitude_reading (specifies outcomes not internal mechanism, wider latitude, expected lower suppression); middlebox_realism_reading (subordinates specification authority to deployed path behavior, expected to treat 'violation' language itself as the extractive fiction and locate the real coordination in what the network does, not what the text says).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
