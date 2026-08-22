% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__strict_invariance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: RFC 9293 TCP State Machine — Strict Invariance Reading
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   RFC 9293 (2022) is the current standards-track consolidation of the TCP
 *   specification originally set out in RFC 793 (1981). This story authors
 *   the strict-invariance reading of that specification: the position that
 *   the document specifies a literal state machine — precise states and
 *   precise transition conditions — that every conformant implementation must
 *   replicate exactly, and that any deviation, whether by a vendor's
 *   optimization or by a middlebox rewriting traffic in flight, is a
 *   specification violation rather than a legitimate variant. Under this
 *   reading the constraint is pure coordination: a shared, non-owned
 *   reference that lets arbitrarily many independent implementers
 *   interoperate without bilateral negotiation, with no party positioned to
 *   extract rent from the specification's operation. This is one of three
 *   linked readings of the RFC 9293 kernel; the optimization_latitude_reading
 *   treats the same text as specifying behavioral outcomes with
 *   implementation latitude, and the middlebox_realism_reading treats the
 *   specification's authority as subordinate to what deployed middleboxes
 *   actually do to traffic on the wire. Each reading is authored as its own
 *   constraint with its own ε; this file's ε (0.08) reflects the
 *   near-mountain character of the strict reading taken on its own terms, not
 *   an average across readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__strict_invariance_reading, 0.08).
domain_priors:suppression_score(rfc9293_tcp_specification__strict_invariance_reading, 0.22).
domain_priors:theater_ratio(rfc9293_tcp_specification__strict_invariance_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__strict_invariance_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__strict_invariance_reading, "RFC 9293 TCP State Machine — Strict Invariance Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__strict_invariance_reading, "network_protocol_engineering/internet_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__strict_invariance_reading, '84b38f57-bf09-4fe9-a0d0-46a99e1abf0b').
narrative_ontology:cs_kernel_codification('84b38f57-bf09-4fe9-a0d0-46a99e1abf0b', formalized).
narrative_ontology:cs_authority_grounding('84b38f57-bf09-4fe9-a0d0-46a99e1abf0b', expertise).
narrative_ontology:cs_interpretation_layer_present('84b38f57-bf09-4fe9-a0d0-46a99e1abf0b').
narrative_ontology:cs_reading_relation('84b38f57-bf09-4fe9-a0d0-46a99e1abf0b', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_reading_relation('84b38f57-bf09-4fe9-a0d0-46a99e1abf0b', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('84b38f57-bf09-4fe9-a0d0-46a99e1abf0b', foundational, state_machine_literal_conformance_required).
narrative_ontology:cs_axiom_status(state_machine_literal_conformance_required, holdable).
narrative_ontology:cs_axiom_grounding('84b38f57-bf09-4fe9-a0d0-46a99e1abf0b', state_machine_literal_conformance_required, conventional).
narrative_ontology:cs_axiom('84b38f57-bf09-4fe9-a0d0-46a99e1abf0b', secondary, deviation_is_violation_not_variance).
narrative_ontology:cs_axiom_status(deviation_is_violation_not_variance, holdable).
narrative_ontology:cs_axiom_grounding('84b38f57-bf09-4fe9-a0d0-46a99e1abf0b', deviation_is_violation_not_variance, conventional).
narrative_ontology:cs_reference_frame('84b38f57-bf09-4fe9-a0d0-46a99e1abf0b', original_state_machine_specification_1981).
narrative_ontology:cs_drift_state('84b38f57-bf09-4fe9-a0d0-46a99e1abf0b', post_rfc9293_consolidation_2022, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('84b38f57-bf09-4fe9-a0d0-46a99e1abf0b', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, conformant_endpoint_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, global_internet_users).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, interoperability_test_bodies).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, implementations_relying_on_strict_conformance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Vendors and open-source maintainers who implement the TCP state machine exactly as specified in RFC 9293 — SYN-SENT, SYN-RECEIVED, ESTABLISHED, and the teardown states transitioning precisely on the documented events. They gain predictable interoperability with every other conformant stack on the internet without needing bilateral testing against each peer. Their only real exit from the specification is to accept silent interop failures with some fraction of the network.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, conformant_endpoint_implementers, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__strict_invariance_reading, conformant_endpoint_implementers, agenda_setter).

% End users whose connections traverse TCP stacks they never see or choose. They benefit from the invariant state machine because it is what makes a connection from any client to any server reliably establish and tear down, but they have no visibility into or say over whether any given implementation actually conforms.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, global_internet_users, beneficiary,
    powerless, biographical, trapped, global).

% IETF working groups, bakeoff organizers, and conformance test suite maintainers who use RFC 9293's state machine as the reference against which implementations are checked. They have no enforcement power beyond publishing test results and interoperability reports; their authority is entirely reputational.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, interoperability_test_bodies, observer,
    institutional, civilizational, analytical, global).

% Stacks (embedded devices, legacy systems, security-sensitive implementations doing strict state validation for attack resistance) that assume every peer replicates the invariant machine exactly. When a peer deviates — whether through vendor extension, buggy implementation, or middlebox rewriting in flight — these implementations bear the cost: dropped connections, security bypass, or silent data corruption, with no recourse because the specification gives them no mechanism to detect or negotiate around deviation.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, implementations_relying_on_strict_conformance, payer,
    moderate, biographical, trapped, global).

% Researchers and engineers proposing performance optimizations (e.g., alternative congestion signaling folded into state transitions, experimental options) who would argue the state machine's literal invariance is unnecessarily rigid — the same behavioral guarantees could be met with more implementation latitude. Under the strict-invariance reading their proposals are read as violations rather than legitimate extensions unless separately standardized.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, protocol_extension_authors, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__strict_invariance_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__strict_invariance_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single documented state machine lets every independent implementer build a TCP stack that will interoperate with every other conformant stack without pairwise testing or negotiation — the state machine is a shared, freely available coordination point for a genuinely decentralized, multi-vendor network.
% TRANSFER_FUNCTION: Under this reading, nothing is systematically transferred between parties — conformance costs (engineering discipline, testing effort) are borne by each implementer for their own benefit, and the arrangement moves reliability and predictability outward to every party on the network rather than extracting from any of them.
% ABSENT_VOICES: Protocol extension authors and researchers who believe the invariant reading is too rigid for legitimate performance work are not part of the specification's own self-description; their objections surface in separate RFCs and drafts, not within RFC 9293's normative text itself.
% DISAPPEARANCE_RATIONALE: If the invariant state machine specification vanished and no shared reference remained, implementers would revert to ad hoc bilateral interoperability testing, TCP's global reliability guarantees would fragment along vendor lines, and the internet's transport layer would lose the property that lets arbitrary endpoints establish a byte-stream connection without prior negotiation.
% FOUNDING_PROBLEM: Early TCP implementations diverged in how they handled state transitions, sequence number edge cases, and connection teardown, producing interoperability failures across a network with no central operator; a single normatively invariant state machine was built to give every implementer the same reference so independently written stacks would interoperate.
% FOUNDING_PROBLEM_CORROBORATION: IETF interoperability bakeoffs and independent conformance test suites (outside any single vendor's control) continue to find real-world divergence when implementations deviate from the RFC 9293 state machine, corroborating that the coordination problem the specification was built to solve remains active rather than historical.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__strict_invariance_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__strict_invariance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__strict_invariance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__strict_invariance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__strict_invariance_reading, 0.08, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.08) because under the strict-invariance reading no party collects rent from the specification's operation — its entire function is coordination, and the beneficiaries (implementers, end users) are also the ones who bear the (modest) conformance-engineering cost. Suppression is moderate (0.22) rather than negligible because the reading does suppress an alternative: it treats implementation latitude and middlebox modification as illegitimate rather than as a design option, foreclosing the interpretive space the sibling readings occupy. Theater ratio is low and rises only slightly over the interval (0.05 to 0.10) as conformance testing infrastructure (bakeoffs, formal verification efforts) has grown without displacing the specification's real coordination function. Accessibility collapse is moderately high (0.62): once an implementer accepts the strict reading, the state machine leaves little room for alternative interpretation of what conformance means, though the reading itself remains contestable at the framing level (hence it is not mountain-grade collapse).
 *
 * DIRECTIONALITY LOGIC:
 *   Conformant implementers and global internet users sit near the beneficiary end: the state machine subsidizes their interoperability at low cost to them under this reading. Implementations relying on strict conformance for security or correctness guarantees are the payer/target group — when the ecosystem does not uniformly honor the strict reading (a fact this story's own reading treats as violation, not as legitimate variance), these implementations are the ones who suffer silent failures or security bypass, and they have no exit: they are trapped by the specification's own logic, which gives them no negotiation mechanism for detecting or accommodating deviation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (divergent implementations breaking interoperability on a network with no central operator) remains live and independently corroborated by ongoing conformance test failures, so this reading does not present as mandatrophy — the mandate has not outlived its function. The classification prevents the strict-invariance reading from being mislabeled as extraction: even though the reading forecloses legitimate optimization space (a real cost, captured in the suppression metric and in the excluded protocol_extension_authors seat), no party collects a rent from that foreclosure, which is what keeps this reading's ε low and its type rope rather than tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    invariance_versus_optimization_boundary,
    'Does RFC 9293 actually specify a literal, invariant state machine that forecloses implementation latitude, or does it specify behavioral outcomes that leave room for internal optimization — i.e., is the strict-invariance reading or the optimization-latitude reading the more accurate account of the text''s own normative force?',
    'Close textual analysis of RFC 9293''s normative language (MUST/SHOULD/MAY usage around state transitions) combined with IETF working-group discussion records from the RFC 793bis process that produced RFC 9293, which would show whether the drafters intended literal state-machine conformance or outcome-level conformance.',
    'If the optimization-latitude reading is textually correct, this story''s strict reading over-claims foreclosure and its suppression metric (0.22) is too high; if the strict reading is correct, implementations that have taken optimization latitude are in a weaker position than they assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invariance_versus_optimization_boundary, conceptual, 'Whether RFC 9293''s text supports literal state-machine invariance or outcome-level conformance with latitude.').

omega_variable(
    middlebox_authority_subordination,
    'Is the specification''s authority genuinely primary (violations are violations regardless of deployment reality), or is it subordinate to the empirical fact of what middleboxes actually do to TCP traffic in the deployed internet — i.e., does the middlebox_realism_reading''s claim that specification authority is secondary to network reality undercut this reading''s premise?',
    'Longitudinal measurement of middlebox interference rates (options stripping, window scaling removal, sequence number rewriting) against RFC 9293 conformance, tracking whether implementers who follow the strict reading experience material interoperability harm from middlebox non-conformance that the strict reading has no mechanism to address.',
    'If middlebox interference is pervasive and unremediable through appeal to the specification, the strict-invariance reading''s claim to being the operative coordination mechanism (rather than a formally correct but practically subordinate ideal) is weakened, which would push this reading''s suppression and accessibility_collapse metrics toward more contested values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_authority_subordination, empirical, 'Whether specification-level invariance is empirically overridden by deployed middlebox behavior.').

omega_variable(
    victim_set_scope_under_strict_reading,
    'Is ''implementations relying on strict conformance'' a genuine victim class under this reading, or does the strict-invariance reading''s own logic deny that any harm occurs (since deviation is defined as violation, not as a legitimate cost the reading must account for)?',
    'Examine documented interoperability incident reports where strict-conformance-dependent implementations (e.g., embedded TCP stacks, intrusion-detection systems doing stateful reassembly) failed against non-conformant peers, and assess whether the strict reading''s proponents treat these as specification failures, peer failures, or unaddressed gaps.',
    'If the strict reading''s own community treats these incidents as peer failures with no remedy, the victim declaration in this story is validated as a real, if reading-internal, cost; if the community treats them as edge cases outside the specification''s scope entirely, the victim set may be narrower than authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_scope_under_strict_reading, conceptual, 'Whether harm to strict-conformance-dependent implementations is a cost internal to this reading or definitionally excluded by it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__strict_invariance_reading, 1981, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1981, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1981, 0.05).
narrative_ontology:measurement(rfc9_tr_t1990, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 1990, 0.06).
narrative_ontology:measurement(rfc9_tr_t2000, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(rfc9_tr_t2010, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(rfc9_tr_t2018, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(rfc9_tr_t2025, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1981, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1981, 0.05).
narrative_ontology:measurement(rfc9_be_t1990, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(rfc9_be_t2000, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2000, 0.06).
narrative_ontology:measurement(rfc9_be_t2010, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2010, 0.07).
narrative_ontology:measurement(rfc9_be_t2018, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2018, 0.08).
narrative_ontology:measurement(rfc9_be_t2025, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 2025, 0.08).

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
% This story is one of three linked readings of the rfc9293_tcp_specification kernel. strict_invariance_reading (this file, rope, ε=0.08) claims the state machine is literally binding with zero deviation tolerance. optimization_latitude_reading claims the specification fixes behavioral outcomes while permitting implementation latitude. middlebox_realism_reading claims specification authority is subordinate to deployed middlebox behavior. Each reading is authored with its own ε and its own type per the ε-invariance principle; they are linked here rather than merged because the strict and latitude readings differ in how much suppression they assign to implementation variance, and the middlebox reading differs in where it locates operative authority entirely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
