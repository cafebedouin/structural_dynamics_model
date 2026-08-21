% ============================================================================
% CONSTRAINT STORY: cold_reader_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cold_reader_reading, []).

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
 *   constraint_id: cold_reader_reading
 *   human_readable: Cold-Reader Standard: Blindness as Absence of Negotiation History, Not Absence of Standing Frame
 *   domain: epistemology_of_evaluation
 *
 * SUMMARY:
 *   This story is one reading within the blindness_decomposition_kernel: the
 *   kernel is the contested question of what 'blindness' should mean when
 *   constructing a review protocol for an AI agent evaluating project
 *   decisions it was not present for. This reading, cold_reader_reading,
 *   holds that blindness properly refers only to absence of negotiation
 *   history — the arguments, concessions, and deliberative back-and-forth
 *   that produced a given ruling — and NOT to absence of the project's
 *   standing framing (its rule files, its category taxonomy, its declared
 *   conventions). The reasoning is that the entity being simulated is not a
 *   hypothetical ignorant outsider but the next real worker who will actually
 *   inherit this project: that worker will have the rule files and categories
 *   on day one, and will lack only the history of how any specific rule got
 *   argued into its current shape. Under this reading, injecting the standing
 *   project context into the reviewer's starting condition is not
 *   contamination — it raises fidelity, because it makes the simulated
 *   starting condition track the real one more closely. This is why the
 *   reading is structurally closest to a rope: it is a coordination device
 *   (calibrating reviewer input to match a real target condition) with
 *   essentially no victim set, rather than an extraction structure with
 *   someone paying through the same mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cold_reader_reading, 0.12).
domain_priors:suppression_score(cold_reader_reading, 0.08).
domain_priors:theater_ratio(cold_reader_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cold_reader_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(cold_reader_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(cold_reader_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cold_reader_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(cold_reader_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cold_reader_reading, rope).
narrative_ontology:human_readable(cold_reader_reading, "Cold-Reader Standard: Blindness as Absence of Negotiation History, Not Absence of Standing Frame").
narrative_ontology:topic_domain(cold_reader_reading, "epistemology_of_evaluation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cold_reader_reading, 'e990fde2-f38b-4772-bdee-637445c1b269').
narrative_ontology:cs_kernel_codification('e990fde2-f38b-4772-bdee-637445c1b269', distributed).
narrative_ontology:cs_authority_grounding('e990fde2-f38b-4772-bdee-637445c1b269', distributed).
narrative_ontology:cs_reading_relation('e990fde2-f38b-4772-bdee-637445c1b269', cold_reader_reading__frame_independence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e990fde2-f38b-4772-bdee-637445c1b269', cold_reader_reading__presentation_audit_reading, coexists_with).
narrative_ontology:cs_axiom('e990fde2-f38b-4772-bdee-637445c1b269', foundational, standing_frame_is_successor_condition_not_confound).
narrative_ontology:cs_axiom_status(standing_frame_is_successor_condition_not_confound, holdable).
narrative_ontology:cs_axiom_grounding('e990fde2-f38b-4772-bdee-637445c1b269', standing_frame_is_successor_condition_not_confound, empirically_contingent).
narrative_ontology:cs_axiom('e990fde2-f38b-4772-bdee-637445c1b269', foundational, blindness_targets_deliberation_history_only).
narrative_ontology:cs_axiom_status(blindness_targets_deliberation_history_only, holdable).
narrative_ontology:cs_axiom_grounding('e990fde2-f38b-4772-bdee-637445c1b269', blindness_targets_deliberation_history_only, conventional).
narrative_ontology:cs_reference_frame('e990fde2-f38b-4772-bdee-637445c1b269', onboarding_parity_standard).
narrative_ontology:cs_drift_state('e990fde2-f38b-4772-bdee-637445c1b269', post_protocol_adoption, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e990fde2-f38b-4772-bdee-637445c1b269', '').
narrative_ontology:cs_kernel_id(cold_reader_reading, blindness_decomposition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cold_reader_reading, reviewing_agent).
narrative_ontology:constraint_beneficiary(cold_reader_reading, project_maintainers).
narrative_ontology:constraint_beneficiary(cold_reader_reading, next_real_worker).
narrative_ontology:constraint_vindicates(cold_reader_reading, starting_condition_fidelity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Is handed the project's standing framing — rule files, category schemas, prior conventions — but explicitly withheld the specific negotiation history (which arguments were made, who conceded what, why this particular threshold was chosen) that produced any single contested decision under review. This is the design point: the reviewer is not asked to simulate an outsider with no context, but to simulate the next worker who will pick up the project cold, inherit its existing rule files, and have to act without having sat through the argument that produced the current state. The reviewer benefits because the standing frame is exactly the information load a real successor carries, making the review a faithful proxy rather than an artificially crippled one.
narrative_ontology:constraint_stakeholder(cold_reader_reading, reviewing_agent, beneficiary,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(cold_reader_reading, reviewing_agent, agenda_setter).

% Author and revise the rule files and category schemas that constitute the standing frame. They benefit from a review protocol that tests whether their documented framing is actually sufficient for a new worker, rather than a protocol that tests something else (raw naive comprehension) and produces a false negative about their documentation's adequacy.
narrative_ontology:constraint_stakeholder(cold_reader_reading, project_maintainers, beneficiary,
    organized, biographical, mobile, local).

% The person or agent this whole protocol is simulating in advance. They will arrive with the rule files and categories already provided by the project, but without having lived through the negotiation that produced any specific ruling. The cold-reader protocol's entire justification is that testing against this worker's actual starting condition, rather than a harder or easier hypothetical, is what makes the test meaningful for the successor's real task.
narrative_ontology:constraint_stakeholder(cold_reader_reading, next_real_worker, beneficiary,
    moderate, biographical, constrained, local).

% Hold that the standing frame itself is part of what should be tested for independence — that giving the reviewer the project's own categories pre-loads the evaluation with the very apparatus whose neutrality is in question. Under this reading they are not part of the conversation about what 'blind' should mean; their objection is answered elsewhere (frame_independence_reading), not here.
narrative_ontology:constraint_stakeholder(cold_reader_reading, frame_independence_advocates, excluded,
    analytical, immediate, analytical, local).

% Hold that what matters is whether the presentation itself (independent of any withheld history) telegraphs the intended verdict — a concern about staging, not about negotiation history. Under this reading their concern is orthogonal and is handled in a separate constraint, not folded into this one.
narrative_ontology:constraint_stakeholder(cold_reader_reading, presentation_audit_advocates, excluded,
    analytical, immediate, analytical, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cold_reader_reading, diffuse).
narrative_ontology:fixing_cost_class(cold_reader_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of designing a review protocol that tests documentation adequacy rather than testing an artificially harder (total-ignorance) or artificially easier (full-history) condition than any real successor will actually face. Coordinates reviewer, maintainers, and the anticipated future worker around one shared definition of 'blind.'
% TRANSFER_FUNCTION: Moves informational load from 'everything including negotiation history' down to 'standing frame only,' calibrating the reviewer's starting condition to match the real successor's starting condition. No monetary or status transfer; the thing moved is epistemic footing.
% ABSENT_VOICES: Advocates of frame-independence review (who would say the standing categories themselves need testing) and presentation-audit advocates (who would say staging cues are the real risk) are not part of this reading's internal conversation — they are structurally routed to sibling readings of the same kernel, not silenced within this one.
% DISAPPEARANCE_RATIONALE: If this specific calibration (standing frame in, negotiation history out) disappeared, the review protocol would either revert to giving reviewers full history (destroying the blindness test entirely) or strip the standing frame too (testing an unrealistically naive reader who no real successor resembles). Maintainers and the reviewing agent would say the world rearranges — reviews would stop being predictive of real successor performance. Frame-independence advocates would say little changes because they consider the standing-frame injection itself the confound, not its absence.
% FOUNDING_PROBLEM: Early review protocols conflated 'blind' with 'ignorant of everything the project has ever decided,' producing reviews that tested a condition no real worker would ever occupy — either far too hard (invent conventions from nothing) or, when overcorrected, far too easy (given the negotiation transcripts that produced the current rules). Neither matched the actual handoff condition a successor experiences.
% FOUNDING_PROBLEM_CORROBORATION: The next_real_worker's actual onboarding experience is the outside corroborating fact: successors observably receive rule files and category schemas on day one and do not receive negotiation transcripts. This is attested by the project's own onboarding practice, independent of any reviewer's or maintainer's self-interested framing, and observable by anyone who inspects what a new contributor is actually handed versus what they are not.
narrative_ontology:disappearance_verdict(cold_reader_reading, contested).
narrative_ontology:founding_problem_status(cold_reader_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cold_reader_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(cold_reader_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cold_reader_reading, 0.12, 'claude-sonnet-5', 'blind_reviewer_jurisdiction_2026_20260820_211650', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cold_reader_reading_tests).
:- end_tests(cold_reader_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.12) because there is no identifiable party whose interests are structurally subordinated by this calibration choice — the reviewing agent, the maintainers, and the future worker are all better served by a review that tests real-world starting conditions than by one that tests an artificial extreme. Suppression is low (0.08) because no alternative reading is coercively foreclosed by adopting this one; sibling readings remain fully live elsewhere in the kernel. Theater ratio is low and slightly declining (0.20 to 0.15) reflecting that as the standing-frame-injection practice becomes normalized, less performative justification is needed to defend it — it settles into unremarked practice rather than requiring active defense.
 *
 * PERSPECTIVAL GAP:
 *   Under this reading, the reviewing agent's seat and the maintainers' seat converge rather than diverge, which is itself the structural signal that distinguishes a rope from a tangled rope or snare reading of the same underlying kernel — there is no seat here that experiences the standing-frame injection as extraction, because the injection's entire justification is fidelity to a condition all seats agree is the real target.
 *
 * DIRECTIONALITY LOGIC:
 *   All three named beneficiaries — reviewing_agent, project_maintainers, next_real_worker — sit near the coordination end of directionality: the reviewing agent gets a fairer, more realistic task; maintainers get a review that actually tests their documentation rather than testing an unrepresentative extreme; the future worker (the target of simulation) gets a review protocol calibrated to their real situation rather than to a fictional harder or easier one. There is no victim group authored for this reading because the sibling readings (frame_independence_reading, presentation_audit_reading) carry the objections that would, under a different framing, generate a victim class — those objections belong structurally to the sibling constraints, not this one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (calibrating 'blind' review to a real starting condition rather than an artificial one) remains live: as long as projects hand new contributors rule files but not negotiation transcripts, the gap this reading addresses persists. There is no mandatrophy here — the mandate has not outlived its function, because the function (fidelity to actual onboarding conditions) is continuously re-verified by observing what real onboarding actually looks like.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    standing_frame_neutrality_assumption,
    'Is the project''s standing framing (rule files, category schemas) itself neutral with respect to the specific decision under review, or does it already encode the bias the review is meant to test for?',
    'Compare review outcomes when the same disputed ruling is evaluated (a) with the standing frame provided under this reading''s protocol and (b) with the standing frame withheld under the frame_independence_reading''s protocol. Divergence in outcome would indicate the frame is not neutral and partially validates the sibling reading''s concern.',
    'If the standing frame is shown to encode bias relevant to the specific ruling under test, this reading''s core premise (that frame-injection only raises fidelity, never contaminates) weakens, and the kernel''s center of gravity shifts toward frame_independence_reading for that class of rulings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standing_frame_neutrality_assumption, conceptual, 'Whether the standing frame is a neutral onboarding fact or an embedded bias vector.').

omega_variable(
    successor_fidelity_measurement,
    'Does giving the reviewer the standing frame but withholding negotiation history actually produce review outcomes that predict real successor performance better than the alternative calibrations do?',
    'Track a cohort of real successor workers who inherit projects cold; compare their actual early-tenure decisions against what the cold-reader-calibrated review protocol predicted, versus what a frame-independence-calibrated or presentation-audit-calibrated protocol would have predicted.',
    'Empirical validation would confirm the rope classification (genuine coordination value, no cover story); empirical failure would suggest the calibration is itself a post-hoc justification for a convenient default rather than a fidelity-tracking design choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(successor_fidelity_measurement, empirical, 'Whether the calibration choice is empirically validated against real onboarding outcomes.').

omega_variable(
    kernel_decomposition_completeness,
    'Are these three readings (cold_reader, frame_independence, presentation_audit) jointly exhaustive of what ''blindness'' could mean in this evaluation context, or is there a fourth reading not yet articulated?',
    'Solicit additional framings from parties not yet consulted in the kernel''s construction — e.g., a reading focused on temporal blindness (withholding knowledge of outcomes/consequences rather than history or frame).',
    'If a fourth reading exists with a materially different beneficiary/victim structure, the kernel_id''s sibling set is incomplete and this reading''s network links should be revised.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_decomposition_completeness, conceptual, 'Whether the kernel''s declared sibling readings exhaust the conceptual space.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cold_reader_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cold_tr_t0, cold_reader_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cold_tr_t2, cold_reader_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement(cold_tr_t4, cold_reader_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(cold_tr_t6, cold_reader_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement(cold_tr_t8, cold_reader_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(cold_tr_t10, cold_reader_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(cold_tr_t12, cold_reader_reading, theater_ratio, 12, 0.15).

% Extraction over time
narrative_ontology:measurement(cold_be_t0, cold_reader_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cold_be_t2, cold_reader_reading, base_extractiveness, 2, 0.1).
narrative_ontology:measurement(cold_be_t4, cold_reader_reading, base_extractiveness, 4, 0.11).
narrative_ontology:measurement(cold_be_t6, cold_reader_reading, base_extractiveness, 6, 0.11).
narrative_ontology:measurement(cold_be_t8, cold_reader_reading, base_extractiveness, 8, 0.12).
narrative_ontology:measurement(cold_be_t10, cold_reader_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(cold_be_t12, cold_reader_reading, base_extractiveness, 12, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(cold_reader_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cold_reader_reading, information_standard).
narrative_ontology:affects_constraint(cold_reader_reading, frame_independence_reading).
narrative_ontology:affects_constraint(cold_reader_reading, presentation_audit_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the single natural-language concept 'blind review' under blindness_decomposition_kernel. cold_reader_reading (this file) authors near-zero extraction and a rope classification, treating standing-frame injection as fidelity-raising. frame_independence_reading is expected to author higher extraction/suppression where the standing frame itself is treated as a contamination vector requiring blinding. presentation_audit_reading targets a third, largely orthogonal concern (staging/telegraphing in presentation) with its own ε. All three share the same kernel_id and must remain linked via affects_constraints; none should be read as a measurement of the same ε under a different observable — per the ε-invariance principle, differing ε values across these three files signal three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
