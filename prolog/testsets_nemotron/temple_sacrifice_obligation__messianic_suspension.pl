% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation — Messianic Suspension Reading
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This constraint story instantiates the messianic_suspension reading of
 *   the temple_sacrifice_obligation kernel. Under this reading, the biblical
 *   obligation to offer sacrifices in the Jerusalem Temple is neither
 *   fulfilled nor violated in the present era — it is structurally suspended,
 *   awaiting a future messianic restoration that will reactivate the
 *   obligation. The authority structure (halakhic tradition) does not treat
 *   the current absence of sacrifice as a gap requiring substitute
 *   performance; rather, the obligation's dormancy is itself the correct
 *   state. Study of sacrifice laws (kodashim) preserves the knowledge for the
 *   restoration but does not constitute compliance, preparation, or
 *   occupation of the obligation. No current agent is obligated to sacrifice,
 *   no agent is penalized for non-sacrifice, and no agent collects rents from
 *   the suspension. The constraint's ε is near-zero because there is no
 *   standing arrangement extracting from anyone — the arrangement is the
 *   absence of an arrangement, grounded in a theological claim about
 *   historical epoch.
 *
 * KEY AGENTS:
 *   - halakhic_authority: agenda_setter (institutional/generational/analytical/universal) — maintains the suspension doctrine, adjudicates the boundary between suspension and violation
 *   - study_communities: observer (organized/biographical/mobile/global) — engage with sacrifice law as theoretical knowledge, no compliance burden
 *   - messianic_restoration_event: non-agent — the future trigger that would reactivate the obligation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.08).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.12).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, mountain).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation — Messianic Suspension Reading").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/halakhic/commitment_system").

domain_priors:emerges_naturally(temple_sacrifice_obligation__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, '794bf7d1-358c-44e9-be6c-f77e5cd06265').
narrative_ontology:cs_kernel_codification('794bf7d1-358c-44e9-be6c-f77e5cd06265', fixed_text).
narrative_ontology:cs_authority_grounding('794bf7d1-358c-44e9-be6c-f77e5cd06265', lineage).
narrative_ontology:cs_interpretation_layer_present('794bf7d1-358c-44e9-be6c-f77e5cd06265').
narrative_ontology:cs_reading_relation('794bf7d1-358c-44e9-be6c-f77e5cd06265', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('794bf7d1-358c-44e9-be6c-f77e5cd06265', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('794bf7d1-358c-44e9-be6c-f77e5cd06265', foundational, obligation_suspended_not_violated).
narrative_ontology:cs_axiom_status(obligation_suspended_not_violated, holdable).
narrative_ontology:cs_axiom_grounding('794bf7d1-358c-44e9-be6c-f77e5cd06265', obligation_suspended_not_violated, theological).
narrative_ontology:cs_axiom('794bf7d1-358c-44e9-be6c-f77e5cd06265', foundational, study_is_knowledge_maintenance_only).
narrative_ontology:cs_axiom_status(study_is_knowledge_maintenance_only, holdable).
narrative_ontology:cs_axiom_grounding('794bf7d1-358c-44e9-be6c-f77e5cd06265', study_is_knowledge_maintenance_only, theological).
narrative_ontology:cs_reference_frame('794bf7d1-358c-44e9-be6c-f77e5cd06265', messianic_epoch_boundary).
narrative_ontology:cs_drift_state('794bf7d1-358c-44e9-be6c-f77e5cd06265', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('794bf7d1-358c-44e9-be6c-f77e5cd06265', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, messianic_restoration_deferral).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, knowledge_maintenance_without_compliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the doctrinal boundary that the sacrifice obligation is suspended, not violated, not fulfilled, and not substitutable. Adjudicates disputes about what counts as legitimate engagement with sacrifice law in the Temple's absence. Does not collect revenue or compliance from the suspension itself. Authority derives from lineage (chain of transmission) and the claim to correctly interpret the epoch.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, halakhic_authority, agenda_setter,
    institutional, generational, analytical, universal).

% Engage with kodashim (sacrifice law) as theoretical study — preserving knowledge, analyzing parameters, maintaining the textual tradition. Under this reading, study is explicitly not compliance, not preparation, not occupation of the obligation. Participation is voluntary; exit is mobile (one can study other topics). No material penalty for non-study, no material reward for study beyond the epistemic value.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, study_communities, observer,
    organized, biographical, mobile, global).

% The future messianic restoration that would reactivate the sacrifice obligation. Not a current agent; a structural trigger in the reading's own framework. The suspension reading treats the current epoch as defined by the restoration's absence.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, messianic_restoration_event, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__messianic_suspension, messianic_restoration_event).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__messianic_suspension, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable doctrinal framework for relating to a destroyed Temple's central obligation without requiring substitute performance, invented rituals, or declaring the obligation void. Coordinates consensus around 'waiting' as the correct posture.
% TRANSFER_FUNCTION: No transfer occurs under this reading. The obligation is suspended — no agent pays, no agent receives. Study of sacrifice law is epistemically motivated, not a transfer mechanism.
% ABSENT_VOICES: Those who would argue the obligation is permanently void (rejected by the authority structure) and those who would demand immediate substitute performance (also rejected). Both positions are structurally excluded from the halakhic conversation by the suspension doctrine itself.
% DISAPPEARANCE_RATIONALE: If the suspension doctrine vanished overnight, the kernel (the biblical obligation) would remain, but the adjudication of its current status would become contested. The world does not rearrange because no current arrangements depend on the suspension — no one is currently sacrificing, no one is currently penalized for not sacrificing, no institution collects from the suspension. The constraint's disappearance would reopen the interpretive question, not restructure material reality.
% FOUNDING_PROBLEM: How does a covenantal obligation centered on a specific physical Temple persist when the Temple is destroyed and the priesthood is dispersed, without either inventing substitutes that claim equal status or declaring the obligation void?
% FOUNDING_PROBLEM_CORROBORATION: The suspension reading is attested by the halakhic authority structure itself (Maimonides, Mishneh Torah, Hilkhot Melakhim; Talmud Bavli, multiple tractates). The founding problem's live status is corroborated by the existence of two other live readings (study_as_occupation, study_as_archiving) held by different communities within the same tradition — the dispute is ongoing and no single reading has achieved consensus.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_unchanged).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_obligation__messianic_suspension),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.08 (near-zero) because no transfer occurs from any current agent to any other — the obligation is structurally absent. Suppression is 0.12 (low but non-zero) because the authority structure maintains a boundary: one may not *invent* a substitute sacrifice or declare the obligation permanently void; the suspension framing is enforced as the only legitimate reading of the kernel. Theater ratio is 0.05 because there is no performative compliance — study is explicitly not compliance. Accessibility collapse is 0.15 because alternative framings (study as occupation, study as archiving) remain live and structurally distinct; the suspension reading does not collapse them. Resistance is 0.05 because no current agent is coerced by this reading — the constraint is accepted as the correct description of the epoch.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat classifications from the structural data. The halakhic_authority seat (agenda_setter, institutional power, analytical exit) experiences this as a mountain — a theological fact about the epoch. Study_communities (observer, organized power, mobile exit) experience it as a mountain — a domain of knowledge with no compliance demand. The messianic_restoration_event is a non-agent future trigger. No seat experiences extraction because no seat bears cost or transfers value under this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries and no victims declared because the suspension reading creates no transfer. The halakhic authority does not collect from the suspension; it maintains a doctrinal boundary. Study communities do not pay for the suspension; they study voluntarily. The messianic restoration is a future event, not a current agent. Directionality is near-symmetric (d ≈ 0.5) for all current seats because costs ≈ benefits ≈ 0.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to relate to a Temple-centered obligation when the Temple is destroyed) is live — the suspension reading is one of three live answers. The mandate has not atrophied because the kernel itself (the obligation) remains the organizing commitment; the suspension reading is the authority structure's current adjudication of that commitment, not a vestigial remnant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the messianic_suspension reading a distinct constraint with its own ε, or a perspectival variant of the temple_sacrifice_obligation kernel?',
    'Compare ε values and beneficiary/victim structures across the three declared readings. If ε differs materially or the victim set changes, they are distinct constraints per ε-invariance.',
    'If distinct constraints, each reading gets its own classification. If perspectival variants, they share a constraint with observer-dependent classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three kernel readings instantiate one constraint or three.').

omega_variable(
    suspension_vs_violation_boundary,
    'Does ''suspended pending restoration'' structurally differ from ''violated but excused'' in a way that affects extraction on any current agent?',
    'Identify any current agent who bears cost, loses standing, or faces penalty under one framing but not the other. If no such agent exists, the distinction is semantic, not structural.',
    'If no current agent is differentially affected, the suspension/violation distinction does not generate extractiveness and the constraint remains a mountain from all current seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_violation_boundary, conceptual, 'Whether the suspension framing has material structural consequences for present actors.').

omega_variable(
    study_function_ambiguity,
    'Does the study of sacrifice law under this reading serve as knowledge-maintenance-only, or does it functionally substitute for compliance in a way that generates beneficiary structure?',
    'Track whether study communities receive material benefits (funding, status, exemption from other obligations) that depend on the ''maintenance of knowledge'' claim. If benefits track the claim, study is a coordination function with beneficiaries.',
    'If study generates beneficiaries, the constraint may be a rope or tangled_rope rather than a mountain. If study is purely epistemic with no material flows, the mountain classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_function_ambiguity, empirical, 'Whether ''maintenance of knowledge-in-waiting'' masks a coordination function with beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 2500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_tr_t0, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_tr_t100, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 100, 0.05).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_tr_t100, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_tr_t500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 500, 0.05).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_tr_t500, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_tr_t1000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1000, 0.05).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_tr_t1000, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_tr_t2000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 2000, 0.05).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_tr_t2000, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_tr_t2500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 2500, 0.05).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_tr_t2500, observed).

% Extraction over time
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_be_t0, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_be_t100, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 100, 0.08).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_be_t100, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_be_t500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 500, 0.08).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_be_t500, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_be_t1000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1000, 0.08).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_be_t1000, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_be_t2000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_be_t2000, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_be_t2500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 2500, 0.08).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_be_t2500, observed).

% Suppression requirement over time
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_su_t0, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_su_t0, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_su_t100, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 100, 0.12).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_su_t100, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_su_t500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 500, 0.12).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_su_t500, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_su_t1000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1000, 0.12).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_su_t1000, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_su_t2000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_su_t2000, observed).
narrative_ontology:measurement(temple_sacrifice_obligation__messianic_suspension_su_t2500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 2500, 0.12).
narrative_ontology:measurement_basis(temple_sacrifice_obligation__messianic_suspension_su_t2500, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__messianic_suspension, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint family (temple_sacrifice_obligation kernel) decomposes the single natural-language concept 'what happens to the sacrifice obligation after the Temple's destruction' into three structurally distinct constraints. The messianic_suspension reading has near-zero ε (no current transfer, no victims). The study_as_occupation reading may have higher ε if study communities receive material benefits tied to 'occupying' the obligation. The study_as_archiving reading sits between — study as knowledge-preservation with possible institutional support. Each reading gets its own constraint_id, its own ε, its own stakeholder surface. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
