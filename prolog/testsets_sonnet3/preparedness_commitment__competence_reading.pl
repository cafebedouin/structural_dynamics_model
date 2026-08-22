% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Competence (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This story instantiates the competence reading of the
 *   preparedness_commitment kernel: the claim that recurring emergency drills
 *   constitute live, exercised knowledge that genuinely maintains operational
 *   capacity across staff turnover, rather than merely performing readiness.
 *   Under this reading, the exercise regime does what it says it does —
 *   decision-quality is tested, transferred, and periodically re-validated,
 *   and the D5 generational-turnover break (loss of tacit competence at
 *   veteran retirement) is avoided or contained because the drills
 *   successfully substitute for lived incident experience. This is a distinct
 *   constraint from the husk_reading (same routines, but memorial rather than
 *   functional) and the hybrid_reading (routines with both memorial and
 *   functional layers) — each carries its own epsilon and its own
 *   classification, linked here only by the shared kernel_id.
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: agenda_setter/beneficiary (institutional/constrained) — designs and runs the drill cycle, bears functional risk
 *   - frontline_responders: beneficiary/payer (moderate/constrained) — pays drill-time cost, receives genuinely transferable competence
 *   - at_risk_populations: beneficiary (powerless/trapped) — receives improved incident outcomes as the downstream effect
 *   - budget_constrained_training_staff: payer (moderate/constrained) — bears the cost of high-fidelity exercise design
 *   - incoming_cohort_trainees: beneficiary (powerless/constrained) — receives transferred tacit knowledge across generational turnover
 *   - external_auditors: observer (institutional/analytical) — verifies competence transfer via outcome data, not attendance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.22).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Competence (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_commitment__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, 'b5b83c47-01f0-461d-b826-2f2a9c0b539f').
narrative_ontology:cs_kernel_codification('b5b83c47-01f0-461d-b826-2f2a9c0b539f', implicit).
narrative_ontology:cs_authority_grounding('b5b83c47-01f0-461d-b826-2f2a9c0b539f', practice).
narrative_ontology:cs_interpretation_layer_present('b5b83c47-01f0-461d-b826-2f2a9c0b539f').
narrative_ontology:cs_reading_relation('b5b83c47-01f0-461d-b826-2f2a9c0b539f', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5b83c47-01f0-461d-b826-2f2a9c0b539f', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('b5b83c47-01f0-461d-b826-2f2a9c0b539f', foundational, exercised_practice_transfers_real_competence).
narrative_ontology:cs_axiom_status(exercised_practice_transfers_real_competence, holdable).
narrative_ontology:cs_axiom_grounding('b5b83c47-01f0-461d-b826-2f2a9c0b539f', exercised_practice_transfers_real_competence, empirically_contingent).
narrative_ontology:cs_axiom('b5b83c47-01f0-461d-b826-2f2a9c0b539f', secondary, outcome_validated_drills_substitute_for_lived_incident_experience).
narrative_ontology:cs_axiom_status(outcome_validated_drills_substitute_for_lived_incident_experience, holdable).
narrative_ontology:cs_axiom_grounding('b5b83c47-01f0-461d-b826-2f2a9c0b539f', outcome_validated_drills_substitute_for_lived_incident_experience, instrumental).
narrative_ontology:cs_reference_frame('b5b83c47-01f0-461d-b826-2f2a9c0b539f', drill_tested_operational_competence).
narrative_ontology:cs_drift_state('b5b83c47-01f0-461d-b826-2f2a9c0b539f', contemporary_multi_generation_turnover, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b5b83c47-01f0-461d-b826-2f2a9c0b539f', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, frontline_responders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, at_risk_populations).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, emergency_management_agencies).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, budget_constrained_training_staff).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, incoming_cohort_trainees).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, frontline_responders).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, operational_readiness_is_maintained_through_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and run the drill cycle, decide what counts as a passing exercise, and allocate budget to refresher training. They collect the institutional legitimacy of being seen as prepared, and they bear reputational and functional risk if the next real event exposes gaps the drills should have caught. Exit from the obligation to run drills is not really available to them; they can only vary how rigorously they run them.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, emergency_management_agencies, beneficiary).

% Firefighters, dispatchers, and field coordinators who go through recurring drills, tabletop exercises, and after-action reviews. The drills cost them time and cognitive effort now, but in this reading the training genuinely transfers — decision-making under simulated stress carries over to real incidents, and they personally experience the payoff when a drilled response goes right. Newer staff absorb tacit knowledge from veterans through the exercises rather than losing it at turnover.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, frontline_responders, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, frontline_responders, payer).

% Residents in flood plains, seismic zones, or wildfire corridors who depend on responders' actual competence during an incident. They have no way to verify from the outside whether the drills are real or performative; they simply receive better or worse outcomes depending on whether the underlying training held. In this reading, their outcomes improve because the drilled competence is genuine.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, at_risk_populations, beneficiary,
    powerless, biographical, trapped, local).

% The instructors and simulation designers who must build and re-run exercises rigorous enough to actually test decision-making, often against shrinking budgets and competing operational demands. They pay the cost of designing high-fidelity scenarios rather than checkbox drills; in this reading that cost is worth bearing because it is what keeps the competence real.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, budget_constrained_training_staff, payer,
    moderate, biographical, constrained, regional).

% New hires and volunteers entering the system after founding personnel retire or move on. They receive the accumulated tacit knowledge through structured, tested exercises rather than through informal mentorship alone, which in this reading successfully bridges generational turnover without a competence gap opening up.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, incoming_cohort_trainees, beneficiary,
    powerless, biographical, constrained, local).

% Independent accreditation bodies and after-action review panels that assess whether drills produced measurable decision-quality improvements, using incident outcome data and exercise performance metrics rather than attendance records alone.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, external_auditors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Recurring, realistic exercises transfer and continuously re-validate the tacit operational knowledge needed to respond effectively to low-frequency, high-consequence events, and they carry that knowledge across staff turnover and generational replacement.
% TRANSFER_FUNCTION: Moves scarce budget and staff time from other operational priorities into designing and running high-fidelity drills; in return it moves demonstrated decision-competence from veteran responders into incoming cohorts and keeps the whole agency's operational capacity current.
% ABSENT_VOICES: Communities that have never experienced a major incident have no seat in deciding how much of the training budget goes to fidelity versus coverage; they would want assurance the competence is real but cannot observe drill quality directly and are not consulted on exercise design.
% DISAPPEARANCE_RATIONALE: If the drill-and-refresher cycle stopped, decision-quality under real incident conditions would measurably decay within one to two staff turnover cycles as tacit knowledge fails to transfer; response times and outcome quality for at-risk populations would degrade in ways that would eventually surface as real event failures, not merely as a paperwork gap.
% FOUNDING_PROBLEM: Low-frequency, high-consequence emergencies (major floods, earthquakes, multi-agency mass-casualty events) do not happen often enough for staff to build real competence through lived experience alone, and knowledge held by veteran responders is lost wholesale at retirement unless it is actively re-taught and tested.
% FOUNDING_PROBLEM_CORROBORATION: Independent accreditation reviews and post-incident after-action reports (produced by external auditors and, in several cases, legislative oversight committees rather than the agencies themselves) attest that agencies running high-fidelity, outcome-tested drills show measurably better real-incident decision quality than those running attendance-only exercises — corroboration external to the agencies that benefit from being seen as prepared.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because, under this reading, the resources spent on drills produce a real, measurable return in decision-quality — this is coordination cost, not rent. Suppression is low-moderate (0.22) because agencies are institutionally obligated to run some form of preparedness program but are not coercing participants against their interest; responders participate because the training pays off for them directly. Theater ratio is deliberately low (0.12) — the defining structural fact of this reading is that the performative-to-functional ratio stays low across the interval; a rising theater ratio would be evidence for the husk_reading, not this one. All three tracked metrics ride the same six-point time grid.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (agencies) and the payer seat (training staff) would compute close to the same type in this reading precisely because the coordination function is real and not covering an extraction — this is the structural signature that should distinguish the competence reading from its siblings under engine computation. If the training-staff seat instead computed as heavily extracted despite the low authored suppression, that divergence would itself be evidence the competence reading does not hold and the husk or hybrid reading is closer to the ground truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Agencies sit near symmetric-to-beneficiary: they administer the system and collect legitimacy, but they also bear real downside risk if competence fails, so their directionality is not treated as pure extraction. Frontline responders and incoming trainees are beneficiaries because the competence reading holds that the training they receive is genuinely useful to them, not merely costly. At-risk populations are pure beneficiaries with no seat in design — their d is low because the constraint subsidizes their safety without extracting anything from them directly. Budget-constrained training staff are the closest thing to a payer seat: their cost (design labor, shrinking budget) is real, though the reading holds it is justified by the competence it produces.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (loss of tacit competence at generational turnover, absent lived incident frequency) is authored as still live and still being solved by the current arrangement, so mandatrophy is not present in this reading — the mandate has not outlived its function. This is the key place where this reading and the husk_reading diverge: the husk_reading would author the same founding problem as dead-but-persisting (mandatrophy present) despite an outwardly identical drill calendar.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_memorial_kernel_ambiguity,
    'Do the drills in this jurisdiction actually transfer operational decision-competence across generational turnover, or do they merely perform continuity while the underlying competence quietly degrades (the husk_reading)?',
    'Compare drilled-response decision quality against real-incident outcome data across at least one full staff-turnover cycle, using blind after-action scoring rather than self-reported exercise completion; a widening gap between drill scores and real-incident performance would falsify the competence reading for this jurisdiction.',
    'If the gap is found, this story''s low extractiveness and low theater_ratio are wrong and the constraint should be re-authored as the husk_reading (or hybrid_reading) with substantially different metrics — the classification computed from this reading''s data would no longer describe the actual arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_memorial_kernel_ambiguity, empirical, 'Whether the observable routine (scheduled drills) corresponds to the competence reading or the husk reading in this specific jurisdiction.').

omega_variable(
    founding_problem_liveness_contested,
    'Is the founding problem (rare-event competence loss at turnover) genuinely still live for this jurisdiction, or has the region''s actual incident frequency changed enough that the original justification no longer applies even if the drills remain effective at what they test?',
    'Track incident frequency and severity trends against the drill cycle''s design assumptions over multiple decades; if incident frequency has risen enough that lived experience now substitutes for drilled knowledge, the founding problem''s liveness would need re-assessment independent of drill quality.',
    'A dead founding problem with a persisting, high-fidelity drill program would not be mandatrophy in the classic sense (function is real) but would suggest resources are misallocated relative to actual risk — a distinct concern from the husk/competence distinction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_liveness_contested, conceptual, 'Whether the founding problem''s continued liveness is independent of whether the drills remain functionally effective.').

omega_variable(
    cs_framing_kernel_vs_institution,
    'Should the commitment system here be framed as the drill-and-exercise institution itself (agencies, training staff, accreditation bodies), or as the deeper legitimacy claim that ''demonstrated readiness'' licenses continued public funding and authority for emergency management agencies?',
    'Trace funding decisions: if budget allocation tracks measured competence outcomes, the institution-level framing is sufficient; if funding tracks the mere existence of a drill calendar regardless of outcome data, the deeper legitimacy-claim framing is doing the real work and should be modeled as its own kernel layer.',
    'Under the institution-level framing this story''s authority_grounding (expertise, with an interpretation layer) is stable; under the legitimacy-claim framing, authority_grounding might shift toward extraction if funding is shown to be decoupled from actual outcome data, changing the cs_structure classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_institution, conceptual, 'Alternative framing of the commitment system as the training institution itself versus the funding-legitimacy claim layered above it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t8, preparedness_commitment__competence_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(prep_tr_t16, preparedness_commitment__competence_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__competence_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(prep_tr_t32, preparedness_commitment__competence_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(prep_be_t8, preparedness_commitment__competence_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(prep_be_t16, preparedness_commitment__competence_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__competence_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(prep_be_t32, preparedness_commitment__competence_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(prep_su_t8, preparedness_commitment__competence_reading, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(prep_su_t16, preparedness_commitment__competence_reading, suppression_requirement, 16, 0.21).
narrative_ontology:measurement(prep_su_t24, preparedness_commitment__competence_reading, suppression_requirement, 24, 0.21).
narrative_ontology:measurement(prep_su_t32, preparedness_commitment__competence_reading, suppression_requirement, 32, 0.22).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__competence_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is the competence_reading member of the preparedness_commitment kernel family (3 stories: competence_reading, husk_reading, hybrid_reading). All three share the observable surface (a recurring drill/exercise calendar) but diverge sharply on epsilon and classification: competence_reading authors low extraction and rope-consistent metrics because the coordination function is taken to be real; husk_reading authors substantially higher theater_ratio and treats the same calendar as memorial performance covering an atrophied function; hybrid_reading splits the difference structurally, authoring separate memorial and competence layers rather than picking one. Per the epsilon-invariance principle, these are three distinct constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
