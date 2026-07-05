% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__husk_reading, []).

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
 *   constraint_id: preparedness_commitment__husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: institutional_memory/disaster_preparedness
 *
 * SUMMARY:
 *   This story instantiates the husk reading of the preparedness_commitment
 *   kernel: an institution's disaster-response drill regime has drifted into
 *   pure form-compliance. Certification cycles are passed, checklists
 *   completed, drill attendance logged — but the routines no longer transmit
 *   the improvisational, cross-functional judgment that response to a real
 *   novel-stress event requires. The regime persists because it is auditable
 *   and defensible (certification bodies and drill contractors profit from
 *   the apparatus continuing to run), not because it prepares anyone for
 *   anything. This is distinct from the competence_reading (where drills
 *   genuinely maintain live capacity) and the hybrid_reading (where memorial
 *   and functional elements coexist in useful tension) — those are separate
 *   constraints, not alternate measurements of this one.
 *
 * KEY AGENTS:
 *   - compliance_certification_bodies: institutional beneficiary — issues certifications, collects fees, has no exposure to response failure
 *   - drill_contractors: institutional beneficiary — designs and runs the scripted drill product, paid regardless of outcome
 *   - frontline_response_personnel: primary victim — must perform under the false confidence the certification implies, bears the gap between credentialed and actual competence
 *   - affected_populations_at_disaster_onset: primary victim — receives the degraded response when novel conditions defeat scripted routines
 *   - agency_leadership: agenda_setter — administers the drill calendar and could redesign it, but bears none of the downstream cost of failure
 *   - external_auditors: observer — reviews compliance documentation, rarely observes live novel-stress performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__husk_reading, 0.58).
domain_priors:suppression_score(preparedness_commitment__husk_reading, 0.44).
domain_priors:theater_ratio(preparedness_commitment__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__husk_reading, piton).
narrative_ontology:human_readable(preparedness_commitment__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_commitment__husk_reading, "institutional_memory/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__husk_reading, 'dd05dd53-2072-4075-ad9c-3d6c84edf435').
narrative_ontology:cs_kernel_codification('dd05dd53-2072-4075-ad9c-3d6c84edf435', formalized).
narrative_ontology:cs_authority_grounding('dd05dd53-2072-4075-ad9c-3d6c84edf435', practice).
narrative_ontology:cs_interpretation_layer_present('dd05dd53-2072-4075-ad9c-3d6c84edf435').
narrative_ontology:cs_reading_relation('dd05dd53-2072-4075-ad9c-3d6c84edf435', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd05dd53-2072-4075-ad9c-3d6c84edf435', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('dd05dd53-2072-4075-ad9c-3d6c84edf435', foundational, form_compliance_substitutes_for_retained_competence).
narrative_ontology:cs_axiom_status(form_compliance_substitutes_for_retained_competence, holdable).
narrative_ontology:cs_axiom_grounding('dd05dd53-2072-4075-ad9c-3d6c84edf435', form_compliance_substitutes_for_retained_competence, empirically_contingent).
narrative_ontology:cs_axiom('dd05dd53-2072-4075-ad9c-3d6c84edf435', secondary, certification_metrics_decouple_from_incident_outcomes_over_time).
narrative_ontology:cs_axiom_status(certification_metrics_decouple_from_incident_outcomes_over_time, holdable).
narrative_ontology:cs_axiom_grounding('dd05dd53-2072-4075-ad9c-3d6c84edf435', certification_metrics_decouple_from_incident_outcomes_over_time, empirically_contingent).
narrative_ontology:cs_reference_frame('dd05dd53-2072-4075-ad9c-3d6c84edf435', founding_era_live_lesson_transmission).
narrative_ontology:cs_drift_state('dd05dd53-2072-4075-ad9c-3d6c84edf435', contemporary_certification_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dd05dd53-2072-4075-ad9c-3d6c84edf435', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__husk_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, compliance_certification_bodies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, drill_contractors).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, frontline_response_personnel).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, affected_populations_at_disaster_onset).
narrative_ontology:constraint_vindicates(preparedness_commitment__husk_reading, institutional_continuity_of_memory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues preparedness certifications on a recurring cycle, collects fees and maintains regulatory relevance from the certification apparatus continuing to operate. Has no exposure to actual incident outcomes and no mechanism that ties certification revenue to real-world response performance.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, compliance_certification_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Designs and delivers the standardized drill product agencies purchase to satisfy certification requirements. Paid per drill delivered regardless of whether participants demonstrate transferable competence; has strong incentive to keep drills scripted and repeatable rather than genuinely novel, since novel scenarios are harder to standardize and sell.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, drill_contractors, beneficiary,
    organized, biographical, mobile, regional).

% Sets the drill calendar, selects contractors, and reports certification status upward. Could redesign the program toward novel-stress testing but is evaluated by superiors on certification compliance rather than incident outcomes, so has weak incentive to bear the cost and risk of overhauling a system that currently satisfies oversight.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, agency_leadership, agenda_setter,
    institutional, biographical, constrained, regional).

% Attends scripted drills, passes certification checkpoints, and is deployed to real incidents carrying institutional and public confidence built on that certification. When novel conditions arise, must improvise beyond what the drill regime prepared them for; bears personal and professional risk from the resulting performance gap. Cannot unilaterally exit the certification system without losing employment standing.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, frontline_response_personnel, payer,
    moderate, biographical, constrained, local).

% Receives whatever response quality the certified agency delivers during an actual incident. Has no visibility into whether the responding agency's preparedness is real competence or memorial performance, and no ability to select a different responder in the moment of need.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, affected_populations_at_disaster_onset, payer,
    powerless, immediate, trapped, local).

% Reviews compliance documentation and drill attendance records during certification renewal. Rarely observes live novel-stress performance directly, and typically has no independent access to post-incident after-action data that would reveal the gap between certified status and actual response competence.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, external_auditors, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: transmit hard-won disaster-response knowledge across personnel turnover and long gaps between real incidents, so institutional memory does not reset with every retirement or hiring cycle.
% TRANSFER_FUNCTION: Moves certification fees and contractor payments from agency budgets to certification bodies and drill vendors; moves false operational confidence from the credentialing system to frontline personnel and the public; moves the cost of the resulting competence gap onto frontline personnel (professional/personal risk) and affected populations (degraded response) at the moment of a real, novel-stress incident.
% ABSENT_VOICES: Affected populations at the moment of disaster have no seat in the design or audit of the drill regime and would object most forcefully if they understood the certification-competence gap, but they are structurally absent from every planning and certification conversation — they appear only as the ultimate recipients of the response, never as participants in shaping it.
% DISAPPEARANCE_RATIONALE: Agency leadership and certification bodies would say the world rearranges badly if the certification regime vanished — no structured mechanism would remain to transmit preparedness knowledge at all, even in degraded form. Frontline personnel and independent safety researchers would say the world barely changes operationally, since the certification's practical link to real competence is already weak; removing the memorial shell might even force honest reckoning with the actual capacity gap sooner. The dispute is genuine and unresolved within the story.
% FOUNDING_PROBLEM: Disaster response knowledge is hard-won, rare to exercise in real conditions, and easily lost across personnel turnover and long intervals between major incidents; the drill and certification regime was built to prevent that institutional forgetting.
% FOUNDING_PROBLEM_CORROBORATION: Agency leadership and certification bodies attest the founding problem remains live and that the current regime addresses it. Independent after-action reviewers and frontline personnel unions, both outside the certifying and contracting relationship, attest that the founding problem has been substantially replaced by a certification-compliance problem — the original memory-retention function has atrophied while the credentialing infrastructure built to serve it persists and has become self-justifying.
narrative_ontology:disappearance_verdict(preparedness_commitment__husk_reading, contested).
narrative_ontology:founding_problem_status(preparedness_commitment__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__husk_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and theater_ratio (0.81) are authored high and rising because the story specifically tracks a Goodhart-drift trajectory: as certification cycles repeat, the metric being optimized (compliance documentation) increasingly substitutes for the goal (operational readiness). Suppression is moderate (0.44), not high — nothing coercively blocks the agency from redesigning drills; the constraint persists through institutional inertia and misaligned incentives, not active enforcement, which is why requires_active_enforcement is false and the claimed type is piton rather than tangled_rope or snare. Accessibility_collapse (0.62) reflects that alternative preparedness models (live-fire scenario training, red-team novel-stress injection) are known and technically available but have been crowded out procedurally, not forbidden. Resistance is low (0.35): frontline staff grumble but rarely mount organized pushback against a system that still pays them and credentials them.
 *
 * PERSPECTIVAL GAP:
 *   From agency leadership's seat, the drill program looks like successful, well-funded institutional memory preservation — certifications renew on schedule, no one is disciplined. From the frontline seat, the same program looks like an elaborate rehearsal for a play that will not resemble the actual emergency. The engine should compute these divergently from the structural data (exit options, power, who bears novel-stress failure cost) rather than from either party's self-report.
 *
 * DIRECTIONALITY LOGIC:
 *   Certification bodies and drill contractors sit near the full-beneficiary end: they collect fees/legitimacy from the apparatus continuing to run and bear zero cost if it fails operationally. Frontline personnel and affected populations sit near the full-target end: personnel carry the false-confidence gap into real incidents, and populations receive whatever degraded response results — neither group can exit the arrangement (personnel are employed within it; populations do not choose their disaster-response agency). Agency leadership is the agenda_setter with the power to redesign but with weak incentive to do so, since certification success is what leadership is evaluated on, not incident outcomes — this is exactly the structural condition a piton requires: someone could fix it, but the cost of fixing (redesigning drills, admitting current competence gaps, possibly delaying certification) exceeds what leadership currently bears.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutions forget hard-won disaster lessons between rare events) is genuinely live — this is not a case of an obsolete mandate persisting for no reason. What has drifted is the MEANS: the drill apparatus was built to solve real memory loss, and the coordination function (transmitting institutional knowledge forward) was real at founding. Over time the measurable proxy (certification pass rate) replaced the underlying goal (retained operational competence) — classic Goodhart substitution. Classifying this as piton rather than snare prevents mislabeling a genuinely degraded-but-once-functional coordination mechanism as pure predation: no one is designing the drift for profit capture at the frontline's expense; the certification bodies and contractors are incidental beneficiaries of an inertial system, not architects of an extraction scheme. This is also why the classification differs from tangled_rope: there is no single administering party actively enforcing continuation against resistance — everyone involved could plausibly desire reform, but no one bears sufficient concentrated cost to force it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_hybrid_boundary,
    'Is the observed drill regime a pure husk (form without any retained function) or does it retain a residual competence core that the husk reading is under-crediting — i.e., is this constraint actually the hybrid reading misclassified?',
    'Novel-stress tabletop or live exercise with scenario elements deliberately outside the drill script; measure whether responders adapt (competence present) or freeze/default to scripted behavior that fails the novel element (husk confirmed).',
    'If adaptive capacity is found, this constraint is mis-instantiated and the situation belongs to hybrid_reading or competence_reading instead; if adaptive capacity is absent, the husk classification and its piton/theater metrics stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_hybrid_boundary, empirical, 'Whether the drill regime retains any real adaptive competence beneath its performative surface.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three declared readings of the preparedness_commitment kernel (competence, husk, hybrid) is the operative one for THIS institution, and on what evidence was husk selected over hybrid?',
    'Cross-reference certification pass rates against post-incident after-action reports across multiple real disaster events; a husk reading predicts certification success decoupled from incident outcomes, while hybrid predicts partial correlation.',
    'Selecting husk versus hybrid changes the beneficiary/victim structure materially — hybrid would credit the drill apparatus with partial coordination function, reducing extraction and raising the constraint toward tangled_rope rather than piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Committer-axis ambiguity: which kernel reading best fits the observed institution, and what evidence justified the husk selection.').

omega_variable(
    theater_ratio_measurement_basis,
    'Is the high theater_ratio (0.81) measuring genuine absence of retained competence, or measuring the visible/auditable layer of a system whose real competence lives in informal, unaudited channels (tacit knowledge held by veteran staff) that the formal drill apparatus does not capture?',
    'Interview and shadow veteran frontline personnel during actual incidents versus during scheduled drills; compare improvisation patterns.',
    'If tacit competence exists outside the audited system, true extraction is lower than measured and the husk label overstates degradation relative to the institution''s actual response capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_measurement_basis, empirical, 'Whether formal theater conceals informal competence not captured by the audited constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__husk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__husk_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prep_tr_t6, preparedness_commitment__husk_reading, theater_ratio, 6, 0.53).
narrative_ontology:measurement(prep_tr_t12, preparedness_commitment__husk_reading, theater_ratio, 12, 0.63).
narrative_ontology:measurement(prep_tr_t18, preparedness_commitment__husk_reading, theater_ratio, 18, 0.71).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__husk_reading, theater_ratio, 24, 0.77).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__husk_reading, theater_ratio, 30, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__husk_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(prep_be_t6, preparedness_commitment__husk_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(prep_be_t12, preparedness_commitment__husk_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(prep_be_t18, preparedness_commitment__husk_reading, base_extractiveness, 18, 0.51).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__husk_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__husk_reading, base_extractiveness, 30, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_commitment__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__husk_reading, 0.08).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the preparedness_commitment kernel, linked via network edges rather than merged into a single observer-relative story. competence_reading models institutions where drills genuinely transmit live capacity (near-rope territory: low extraction, real coordination function intact). hybrid_reading models institutions with layered memorial/functional elements coexisting (tangled or scaffold territory depending on sunset design). This story (husk_reading) models the degraded case: form persists, function has atrophied, and the apparatus is maintained by parties who profit from its continuation without exposure to its failure. Each carries its own ε, its own stakeholder set, and its own classification; none is a re-measurement of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
