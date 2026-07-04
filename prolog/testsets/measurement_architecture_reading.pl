% ============================================================================
% CONSTRAINT STORY: measurement_architecture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_measurement_architecture_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: measurement_architecture_reading
 *   human_readable: Observer/Committer Asymmetry in Measurement Architecture
 *   domain: epistemology/formal_systems
 *
 * SUMMARY:
 *   The observer/committer distinction in measurement architecture
 *   establishes two independent axes (co-equal as meters, per Theorem 7) but
 *   treats them asymmetrically in audit: the committer axis is the
 *   orientation face, audited against the observer axis as reference frame,
 *   not vice versa. This reading frames the asymmetry as a structural fact
 *   about measurement systems requiring directional validation—a coordination
 *   solution, not an ontological claim. The constraint is claimed as mountain
 *   (emerges from measurement theory) while declaring beneficiaries
 *   (theorists and audit designers who gain conceptual clarity), triggering
 *   FSM evaluation. The modest extraction (0.18) reflects the cost of
 *   adopting new vocabulary and the asymmetry's role in legitimating specific
 *   audit architectures.
 *
 * KEY AGENTS:
 *   - formal_measurement_theorists: Analytical seat, arbitrage exit — develop the architecture and benefit from its structural clarity
 *   - audit_framework_designers: Institutional seat, mobile exit — implement systems grounded in the asymmetry
 *   - ontological_realists: Analytical seat, excluded — object that asymmetry implies ontological privilege
 *   - vocabulary_minimalists: Analytical seat, excluded — contend the distinction is unnecessary neologism
 *   - applied_measurement_practitioners: Organized seat, observer — use the formalism in practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(measurement_architecture_reading, 0.18).
domain_priors:suppression_score(measurement_architecture_reading, 0.12).
domain_priors:theater_ratio(measurement_architecture_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(measurement_architecture_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(measurement_architecture_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(measurement_architecture_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(measurement_architecture_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(measurement_architecture_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(measurement_architecture_reading, mountain).
narrative_ontology:human_readable(measurement_architecture_reading, "Observer/Committer Asymmetry in Measurement Architecture").
narrative_ontology:topic_domain(measurement_architecture_reading, "epistemology/formal_systems").

domain_priors:emerges_naturally(measurement_architecture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(measurement_architecture_reading, 'ddb92425-8611-433b-8302-cb476764f275').
narrative_ontology:cs_kernel_codification('ddb92425-8611-433b-8302-cb476764f275', formalized).
narrative_ontology:cs_authority_grounding('ddb92425-8611-433b-8302-cb476764f275', expertise).
narrative_ontology:cs_interpretation_layer_present('ddb92425-8611-433b-8302-cb476764f275').
narrative_ontology:cs_reading_relation('ddb92425-8611-433b-8302-cb476764f275', seat_gauge_orientation_kernel__ontological_commitment_reading, coexists_with).
narrative_ontology:cs_reading_relation('ddb92425-8611-433b-8302-cb476764f275', seat_gauge_orientation_kernel__vocabulary_collision_reading, coexists_with).
narrative_ontology:cs_axiom('ddb92425-8611-433b-8302-cb476764f275', foundational, audit_asymmetry_structural_not_ontological).
narrative_ontology:cs_axiom_status(audit_asymmetry_structural_not_ontological, holdable).
narrative_ontology:cs_axiom_grounding('ddb92425-8611-433b-8302-cb476764f275', audit_asymmetry_structural_not_ontological, empirically_contingent).
narrative_ontology:cs_axiom('ddb92425-8611-433b-8302-cb476764f275', secondary, dual_axis_independence_preserving).
narrative_ontology:cs_axiom_status(dual_axis_independence_preserving, holdable).
narrative_ontology:cs_axiom_grounding('ddb92425-8611-433b-8302-cb476764f275', dual_axis_independence_preserving, conventional).
narrative_ontology:cs_reference_frame('ddb92425-8611-433b-8302-cb476764f275', measurement_theoretic_independence).
narrative_ontology:cs_drift_state('ddb92425-8611-433b-8302-cb476764f275', contemporary_audit_formalization, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ddb92425-8611-433b-8302-cb476764f275', '').
narrative_ontology:cs_kernel_id(measurement_architecture_reading, seat_gauge_orientation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(measurement_architecture_reading, formal_measurement_theorists).
narrative_ontology:constraint_beneficiary(measurement_architecture_reading, audit_framework_designers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop measurement architectures where the observer/committer distinction provides structural clarity: two independent anchors (co-equal as meters) that enable triangulation without requiring ontological commitment to either axis. The asymmetry in audit (one-directional: committer is audited against observer, not vice versa) gives them a non-arbitrary orientation principle for formal systems.
narrative_ontology:constraint_stakeholder(measurement_architecture_reading, formal_measurement_theorists, beneficiary,
    analytical, generational, analytical, universal).

% Build accountability systems that need a privileged reference frame for validation. The observer axis provides that frame: committer claims are audited against observer-accessible evidence, establishing a direction of epistemic flow. They benefit from the asymmetry being structural rather than conventional—it grounds audit architecture in measurement theory rather than institutional fiat.
narrative_ontology:constraint_stakeholder(measurement_architecture_reading, audit_framework_designers, beneficiary,
    institutional, biographical, mobile, global).

% Argue that the asymmetry smuggles in ontological privilege for the observer axis—that 'audited against' implies 'more real than.' They would prefer either full symmetry (both axes equally conventional) or explicit ontological grounding. Their objection is excluded from the measurement-architecture framing by construction: the reading treats audit direction as a structural fact about measurement, not a metaphysical claim.
narrative_ontology:constraint_stakeholder(measurement_architecture_reading, ontological_realists, excluded,
    analytical, generational, analytical, universal).

% Contend that 'observer' and 'committer' are unnecessary neologisms—that the distinction could be expressed in standard measurement vocabulary (reference frame, coordinate choice, gauge freedom) without inventing new terms. They are excluded because the reading's claim is precisely that standard vocabulary collapses the structural asymmetry the architecture depends on.
narrative_ontology:constraint_stakeholder(measurement_architecture_reading, vocabulary_minimalists, excluded,
    analytical, biographical, analytical, universal).

% Implement measurement systems in domains where audit matters (scientific instrumentation, financial reporting, regulatory compliance). They see the architecture as a formalization of what they already do: one axis is 'what we measure,' the other is 'what frame we measure from,' and audit always runs one direction. The formalism gives them a principled answer to 'why this direction and not the other.'
narrative_ontology:constraint_stakeholder(measurement_architecture_reading, applied_measurement_practitioners, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a non-arbitrary orientation principle for measurement architectures that need both independent anchors (for triangulation) and directional audit (for validation): the observer axis is the reference frame, the committer axis is the orientation face.
% TRANSFER_FUNCTION: No material transfer. The constraint coordinates conceptual labor: theorists building measurement systems get a structural answer to 'which axis is privileged for audit' without having to ground it in metaphysics or convention.
% ABSENT_VOICES: Ontological realists who want the asymmetry to rest on metaphysical claims about reality, and vocabulary minimalists who want the distinction absorbed into standard measurement terminology. Both are excluded because the reading's point is that the asymmetry is structural-but-not-ontological and requires new vocabulary to express.
% DISAPPEARANCE_RATIONALE: If the distinction vanished, measurement architectures requiring directional audit would either collapse the two axes into one (losing triangulation) or treat the audit direction as arbitrary convention (losing the principled grounding). Formal systems depending on the asymmetry would need reconstruction.
% FOUNDING_PROBLEM: Measurement systems with two independent axes (needed for triangulation, per Theorem 7) but requiring directional validation (audit must run one way, not both) had no principled answer for which axis is the reference frame—the choice appeared arbitrary or metaphysically loaded.
% FOUNDING_PROBLEM_CORROBORATION: Applied measurement practitioners in scientific instrumentation and regulatory compliance attest the problem is live: they need both independent axes and a non-arbitrary audit direction. Formal measurement theorists outside the benefiting parties confirm the architecture solves a real coordination problem in systems design.
narrative_ontology:disappearance_verdict(measurement_architecture_reading, world_rearranges).
narrative_ontology:founding_problem_status(measurement_architecture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(measurement_architecture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-03',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(measurement_architecture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(measurement_architecture_reading, 0.18, 'claude-sonnet-4-5-20250929', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(measurement_architecture_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(measurement_architecture_reading, ExtMetricName, E),
    domain_priors:suppression_score(measurement_architecture_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(measurement_architecture_reading),
    narrative_ontology:constraint_metric(measurement_architecture_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(measurement_architecture_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(measurement_architecture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.18) because the constraint solves a real coordination problem (non-arbitrary audit direction) with minimal overhead. The modest extraction comes from: (1) vocabulary adoption cost—'observer' and 'committer' are new terms practitioners must learn; (2) the asymmetry legitimating specific audit architectures, which benefits designers of those architectures. Suppression is very low (0.12) because alternatives (symmetric treatment, ontological grounding, standard vocabulary) remain accessible—the architecture is adopted where it fits, not enforced. Theater is minimal (0.08): the distinction does real work in formal systems; very little activity is performative defense. Accessibility collapse is high (0.82) because once you understand the measurement problem (need both independent axes AND directional audit), the asymmetry follows structurally—alternatives don't solve the same problem. Resistance is low (0.15) because the constraint operates in formal theory, where structural arguments face less friction than in domains with material stakes.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (theorists, audit designers), the asymmetry is a structural discovery—measurement systems requiring directional validation need this orientation principle. From the excluded seats (ontological realists, vocabulary minimalists), the asymmetry is a constructed choice that could have been made differently. The engine computes this divergence from the structural data; the claimed type (mountain) is independent of the metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Formal measurement theorists and audit framework designers are beneficiaries: they gain conceptual clarity and a principled grounding for audit direction. Their d values sit near the beneficiary end (low/negative χ). Ontological realists and vocabulary minimalists are excluded rather than targeted—they object to the framing but bear no extraction from its operation. Applied practitioners are observers: they use the architecture where it fits, with mobile exit. The asymmetry in beneficiary concentration (theorists and designers vs. diffuse practitioner adoption) is what triggers FSM: a claimed mountain with identifiable beneficiaries requires omega documentation of the natural-law vs. constructed ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophy: the founding problem (need for non-arbitrary audit direction in dual-axis measurement systems) is live, and the architecture is adopted where it solves that problem. The modest extraction reflects vocabulary cost and legitimation benefit, not rent-seeking on an obsolete function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_ontological_asymmetry,
    'Is the audit asymmetry (committer audited against observer, not vice versa) a structural fact about measurement architecture, or does it smuggle in an ontological claim that the observer axis is ''more real''?',
    'Philosophical analysis of whether directional audit can be grounded in measurement theory alone, or whether it requires metaphysical premises about observer-independence. If the asymmetry can be derived from the functional requirements of validation systems without ontological premises, it is structural; if it collapses without such premises, the ontological realists are correct.',
    'If structural, the constraint is a genuine mountain (emerges from measurement theory). If ontological, it is a constructed framing that benefits theorists who prefer non-metaphysical grounding, and the FSM signature is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_ontological_asymmetry, conceptual, 'Whether audit asymmetry is structural or ontologically loaded.').

omega_variable(
    vocabulary_necessity,
    'Are ''observer'' and ''committer'' necessary new terms, or could the distinction be expressed in standard measurement vocabulary (reference frame, gauge choice, coordinate selection) without loss?',
    'Attempt to reconstruct the architecture using only standard measurement terminology. If the reconstruction preserves the asymmetry and its audit implications, the new vocabulary is unnecessary; if the asymmetry collapses or becomes ambiguous, the vocabulary is doing real work.',
    'If the vocabulary is unnecessary, the modest extraction (0.18) includes a component of neologism cost that could be avoided. If necessary, the extraction is justified coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vocabulary_necessity, conceptual, 'Whether new vocabulary is structurally necessary or avoidable overhead.').

omega_variable(
    beneficiary_concentration_vs_natural_law,
    'Does the constraint''s benefit to formal measurement theorists and audit framework designers indicate it is a constructed framing serving their interests, or is the benefit incidental to a genuine structural discovery?',
    'Historical and sociological analysis: if the architecture was developed by theorists with a stake in non-ontological grounding and adopted primarily in domains where that grounding matters, the beneficiary concentration is evidence of construction. If it was discovered independently and adopted across diverse domains, the benefit is incidental.',
    'This is the FSM omega: if the beneficiary concentration is structural (the architecture genuinely solves a problem theorists face), the mountain claim holds. If it is constructed to serve theorist interests, FSM reclassification to tangled_rope is warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_concentration_vs_natural_law, empirical, 'Whether beneficiary concentration indicates construction or incidental benefit from natural law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(measurement_architecture_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meas_tr_t0, measurement_architecture_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(meas_tr_t5, measurement_architecture_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(meas_tr_t10, measurement_architecture_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(meas_tr_t15, measurement_architecture_reading, theater_ratio, 15, 0.075).
narrative_ontology:measurement(meas_tr_t20, measurement_architecture_reading, theater_ratio, 20, 0.08).

% Extraction over time
narrative_ontology:measurement(meas_be_t0, measurement_architecture_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(meas_be_t5, measurement_architecture_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(meas_be_t10, measurement_architecture_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement(meas_be_t15, measurement_architecture_reading, base_extractiveness, 15, 0.175).
narrative_ontology:measurement(meas_be_t20, measurement_architecture_reading, base_extractiveness, 20, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(meas_su_t0, measurement_architecture_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(meas_su_t5, measurement_architecture_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement(meas_su_t10, measurement_architecture_reading, suppression_requirement, 10, 0.115).
narrative_ontology:measurement(meas_su_t15, measurement_architecture_reading, suppression_requirement, 15, 0.118).
narrative_ontology:measurement(meas_su_t20, measurement_architecture_reading, suppression_requirement, 20, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(measurement_architecture_reading, information_standard).
narrative_ontology:affects_constraint(measurement_architecture_reading, ontological_commitment_reading).
narrative_ontology:affects_constraint(measurement_architecture_reading, vocabulary_collision_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the seat_gauge_orientation_kernel. The kernel decomposes into three structurally distinct claims: (1) measurement_architecture_reading (this file): two axes, one seat, asymmetric audit is structural; (2) ontological_commitment_reading: asymmetry is ontologically grounded; (3) vocabulary_collision_reading: distinction is unnecessary neologism. Each reading has different ε (this reading: low extraction from coordination; ontological reading: higher extraction from metaphysical commitment; vocabulary reading: extraction from terminological proliferation) and different beneficiary structures. Linked via network.affects_constraints because they share the kernel and influence each other's legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
