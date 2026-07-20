% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__strict_invariance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: RFC 9293 Strict Invariance Reading
 *   domain: network_protocol_engineering
 *
 * SUMMARY:
 *   This constraint story instantiates the strict_invariance_reading of the
 *   RFC 9293 TCP specification kernel. Under this reading, RFC 9293 specifies
 *   an invariant state machine that implementations must replicate exactly;
 *   any deviation, including middlebox modification, is a violation. The
 *   constraint is claimed as a pure rope â a coordination mechanism that
 *   solves the collective-action problem of global interoperability without
 *   extraction. The metrics are authored independently: extraction is
 *   negligible, suppression minimal, and the theater ratio near zero,
 *   consistent with a technical standard whose function is purely
 *   coordinative. The victim seat (strict_stack_implementations) acknowledges
 *   that implementations relying on strict guarantees bear fragility costs
 *   when the operational network deviates, but the constraint itself extracts
 *   almost nothing.
 *
 * KEY AGENTS:
 *   - ietf_standards_body (agenda setter, institutional)
 *   - compliant_implementations (beneficiary, organized)
 *   - strict_stack_implementations (payer, moderate)
 *   - middlebox_operators (excluded, powerful)
 *   - performance_optimizers (excluded, moderate)
 *   - internet_end_users (beneficiary, organized)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__strict_invariance_reading, 0.02).
domain_priors:suppression_score(rfc9293_tcp_specification__strict_invariance_reading, 0.05).
domain_priors:theater_ratio(rfc9293_tcp_specification__strict_invariance_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__strict_invariance_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__strict_invariance_reading, "RFC 9293 Strict Invariance Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__strict_invariance_reading, "network_protocol_engineering").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__strict_invariance_reading, 'e63b5393-9e1b-47f5-a025-6c456ec01242').
narrative_ontology:cs_kernel_codification('e63b5393-9e1b-47f5-a025-6c456ec01242', formalized).
narrative_ontology:cs_authority_grounding('e63b5393-9e1b-47f5-a025-6c456ec01242', lineage).
narrative_ontology:cs_interpretation_layer_present('e63b5393-9e1b-47f5-a025-6c456ec01242').
narrative_ontology:cs_reading_relation('e63b5393-9e1b-47f5-a025-6c456ec01242', rfc9293_tcp_specification__optimization_latitude_reading, forecloses).
narrative_ontology:cs_reading_relation('e63b5393-9e1b-47f5-a025-6c456ec01242', rfc9293_tcp_specification__middlebox_realism_reading, forecloses).
narrative_ontology:cs_axiom('e63b5393-9e1b-47f5-a025-6c456ec01242', foundational, protocol_identity_equals_state_machine_identity).
narrative_ontology:cs_axiom_status(protocol_identity_equals_state_machine_identity, holdable).
narrative_ontology:cs_axiom_grounding('e63b5393-9e1b-47f5-a025-6c456ec01242', protocol_identity_equals_state_machine_identity, conventional).
narrative_ontology:cs_axiom('e63b5393-9e1b-47f5-a025-6c456ec01242', foundational, middlebox_modification_is_protocol_violation).
narrative_ontology:cs_axiom_status(middlebox_modification_is_protocol_violation, holdable).
narrative_ontology:cs_axiom_grounding('e63b5393-9e1b-47f5-a025-6c456ec01242', middlebox_modification_is_protocol_violation, conventional).
narrative_ontology:cs_reference_frame('e63b5393-9e1b-47f5-a025-6c456ec01242', strict_state_machine_invariance).
narrative_ontology:cs_drift_state('e63b5393-9e1b-47f5-a025-6c456ec01242', contemporary_middlebox_pervasive_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e63b5393-9e1b-47f5-a025-6c456ec01242', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, compliant_implementations).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, internet_end_users).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, strict_stack_implementations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the RFC series and the TCP specification as a formalized state machine. Publishes errata and updates but treats the core invariants as non-negotiable for interoperability. Its authority derives from the open standards process and the textual lineage of RFCs.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, ietf_standards_body, agenda_setter,
    institutional, generational, analytical, global).

% Implement the RFC 9293 state machine exactly. Benefit from guaranteed interoperability with all other compliant stacks without pairwise negotiation. Cannot deviate without losing interoperability.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, compliant_implementations, beneficiary,
    organized, biographical, constrained, global).

% Implement the specification literally and rely on wire-level invariants holding end-to-end. Bear the cost when middleboxes or non-compliant peers violate those invariants, requiring defensive engineering and workarounds.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, strict_stack_implementations, payer,
    moderate, biographical, constrained, global).

% Operate NATs, firewalls, and accelerators that modify TCP headers or segment boundaries in flight. Under this reading they are non-compliant actors, yet they remain pervasive and largely unaccountable to the standards body.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, middlebox_operators, excluded,
    powerful, biographical, mobile, global).

% Seek to optimize TCP for datacenters, high-BDP paths, or constrained environments by relaxing timing or window behaviors. Their proposed deviations are treated as violations under strict invariance.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, performance_optimizers, excluded,
    moderate, biographical, constrained, global).

% Rely on TCP for everyday connectivity. Benefit from the interoperability that the invariant state machine preserves, without needing to know which stack is running at either end.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, internet_end_users, beneficiary,
    organized, immediate, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, unambiguous transport state machine that eliminates pairwise negotiation of core semantics across the global Internet, enabling any two compliant implementations to communicate reliably.
% TRANSFER_FUNCTION: Transfers implementation certainty and interoperability risk: each endpoint trades local engineering discretion for the assurance that remote peers will behave identically; no monetary transfer occurs, but the cost of strict compliance is borne by implementers while the benefit of interoperability is shared.
% ABSENT_VOICES: Middlebox operators who alter TCP streams and performance-oriented implementers seeking local optimizations are structurally excluded or labeled as violators; their operational reality is acknowledged only as non-compliant.
% DISAPPEARANCE_RATIONALE: If the invariant state machine vanished, the global assumption of identical TCP behavior would collapse. Implementations would diverge, middlebox intolerance would fragment connectivity, and the Internet would splinter into incompatible transport islands.
% FOUNDING_PROBLEM: Reliable, ordered byte-stream transport across independently administered networks required a common protocol; without a shared specification, every host pair would require bespoke integration.
% FOUNDING_PROBLEM_CORROBORATION: Network operators, systems researchers, and implementers outside the IETF attest that the absence of a unified transport standard would cause immediate interoperability collapse; operational experience with protocol diversity corroborates the live status.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__strict_invariance_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__strict_invariance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__strict_invariance_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__strict_invariance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__strict_invariance_reading, 0.02, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is 0.02 because the specification itself moves no rents; it is a published text. Suppression is 0.05 because adoption is voluntary and non-compliant stacks simply fail to interoperate rather than being actively coerced. Theater ratio is 0.01 because enforcement is not performative â there is no compliance theater, only operational success or failure. Accessibility collapse is high (0.92) because once the spec is understood, ad-hoc alternatives collapse: deviating means non-interoperation. Resistance is low (0.08) because the industry generally accepts the standard, though middlebox operators and optimizers constitute a latent resistance.
 *
 * PERSPECTIVAL GAP:
 *   The IETF and compliant implementations experience the constraint as coordination: a solved problem with clear rules. Strict_stack_implementations experience it as a source of fragility: they pay in defensive engineering when the operational network violates the invariants they rely on. Performance_optimizers experience it as exclusion: they are barred from beneficial deviation. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The IETF sits near the agenda-setter center; compliant implementations and end users are structural beneficiaries (low d) because the spec subsidizes their interoperability. Strict_stack_implementations are payers (high d) because they bear the cost of invariant fragility in a non-compliant world. Middlebox_operators and performance_optimizers are excluded, with d near the target end because the constraint is designed to suppress their behavior.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable as mandatrophy: the founding problem (global transport interoperability) remains live, and the constraint's function has not atrophied. The low theater ratio and live founding problem status prevent piton misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    middlebox_prevalence_vs_strict_invariance,
    'Is the deployed middlebox population so pervasive that the strict invariance reading is operationally false, making strict implementations structurally fragile?',
    'Empirical measurement of path-level TCP invariant preservation across diverse network paths.',
    'If middleboxes universally violate invariants, strict invariance is a rope only in specification and a snare/tangled rope in operation; if paths are clean, strict invariance remains viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_prevalence_vs_strict_invariance, empirical, 'Whether operational reality matches the strict state machine.').

omega_variable(
    optimization_latitude_separability,
    'Can performance optimizations be separated from the core invariants without breaking interoperability, or does any latitude collapse the coordination function?',
    'Controlled interop testing of optimized stacks against strict stacks.',
    'If separable, strict invariance forecloses genuine coordination-compatible improvements; if inseparable, strictness is the price of the rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(optimization_latitude_separability, conceptual, 'Whether optimization and invariance are structurally separable.').

omega_variable(
    kernel_reading_underdetermination,
    'Does the RFC 9293 text itself logically compel strict invariance, or does it underdetermine the reading such that latitude is equally valid?',
    'Close textual and formal analysis of the specification''s normative language versus descriptive language.',
    'If the text underdetermines, strict invariance is one reading among many and its foreclosure claims are overreach; if it compels, the reading is textually grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the kernel text supports only strict invariance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__strict_invariance_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_strict_tr_t0, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(rfc9293_strict_tr_t10, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 10, 0.01).
narrative_ontology:measurement(rfc9293_strict_tr_t20, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 20, 0.01).
narrative_ontology:measurement(rfc9293_strict_tr_t30, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 30, 0.01).

% Extraction over time
narrative_ontology:measurement(rfc9293_strict_be_t0, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(rfc9293_strict_be_t10, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 10, 0.02).
narrative_ontology:measurement(rfc9293_strict_be_t20, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 20, 0.02).
narrative_ontology:measurement(rfc9293_strict_be_t30, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 30, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(rfc9293_tcp_specification__strict_invariance_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__strict_invariance_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
