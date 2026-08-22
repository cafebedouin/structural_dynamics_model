% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__hybrid_dependency, []).

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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Hybrid Simulation/Real-World Competence Exercise Requirement
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the 'hybrid_dependency' reading of the
 *   competence-exercise kernel: the claim that neither pure simulation nor
 *   pure real-world catastrophe exposure is sufficient to maintain rare-event
 *   competence in high-reliability aviation operations, and that a structured
 *   mix of simulator training, periodic real aircraft time, and non-jeopardy
 *   line audits is required. The requirement is administered by regulators
 *   and carrier safety departments, who benefit from the liability shield and
 *   public trust it confers, while the operational cost — schedule
 *   disruption, career risk exposure during audits, and the disproportionate
 *   real-aircraft-hour burden on junior crew — falls on line pilots. This is
 *   presented as a Tangled Rope: it solves a genuine coordination problem
 *   (competence maintenance under rare-event risk) but does so through a
 *   structure that extracts disproportionately from junior and lower-power
 *   crew relative to the carriers and regulators who administer it, and that
 *   extraction requires active enforcement (mandatory currency requirements,
 *   audit failure consequences) to persist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.38).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.42).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Simulation/Real-World Competence Exercise Requirement").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, 'e0ecb92a-194e-49b9-99a9-845853df2572').
narrative_ontology:cs_kernel_codification('e0ecb92a-194e-49b9-99a9-845853df2572', formalized).
narrative_ontology:cs_authority_grounding('e0ecb92a-194e-49b9-99a9-845853df2572', expertise).
narrative_ontology:cs_interpretation_layer_present('e0ecb92a-194e-49b9-99a9-845853df2572').
narrative_ontology:cs_reading_relation('e0ecb92a-194e-49b9-99a9-845853df2572', competence_exercise_requirement__simulation_as_adequate_exercise, coexists_with).
narrative_ontology:cs_reading_relation('e0ecb92a-194e-49b9-99a9-845853df2572', competence_exercise_requirement__catastrophe_as_necessary_anchor, influences).
narrative_ontology:cs_axiom('e0ecb92a-194e-49b9-99a9-845853df2572', foundational, real_world_anchoring_is_necessary_supplement).
narrative_ontology:cs_axiom_status(real_world_anchoring_is_necessary_supplement, holdable).
narrative_ontology:cs_axiom_grounding('e0ecb92a-194e-49b9-99a9-845853df2572', real_world_anchoring_is_necessary_supplement, empirically_contingent).
narrative_ontology:cs_axiom('e0ecb92a-194e-49b9-99a9-845853df2572', foundational, non_jeopardy_audit_suffices_as_anchor_without_catastrophe).
narrative_ontology:cs_axiom_status(non_jeopardy_audit_suffices_as_anchor_without_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('e0ecb92a-194e-49b9-99a9-845853df2572', non_jeopardy_audit_suffices_as_anchor_without_catastrophe, instrumental).
narrative_ontology:cs_reference_frame('e0ecb92a-194e-49b9-99a9-845853df2572', post_automation_complacency_training_reform).
narrative_ontology:cs_drift_state('e0ecb92a-194e-49b9-99a9-845853df2572', contemporary_scheduling_pressure_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e0ecb92a-194e-49b9-99a9-845853df2572', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, flying_public).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, carrier_safety_departments).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, regulatory_agencies).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, line_pilots).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, junior_crew).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, training_departments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, training_departments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the minimum required mix of simulator hours, line-operating experience, and non-jeopardy audit checks that a pilot must accumulate to remain current. Administers the certification regime and can shift the ratio between simulation and real-world components in response to accident data or lobbying. Does not fly the routes or absorb the schedule cost of compliance.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Design and administer the internal training curricula that satisfy the hybrid requirement, and benefit from the liability shield and public trust the requirement provides. They also enforce scheduling around line-check slots and real-aircraft rotations, absorbing organizational cost but capturing reputational and legal benefit.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, carrier_safety_departments, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, carrier_safety_departments, agenda_setter).

% Must periodically surrender scheduled flying time and personal time to complete simulator sessions, line checks, and non-jeopardy audits on top of an already demanding roster. Cannot substitute pure simulation for the real-aircraft and line-operations components without losing currency and, ultimately, their certification and livelihood. Their only real exit is leaving the profession.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, line_pilots, payer,
    moderate, biographical, constrained, national).

% Bear the requirement most acutely: junior first officers and new-type-rated pilots need the largest volume of real-aircraft anchoring hours precisely when they have the least seniority to secure favorable scheduling for it, and the least standing to contest audit outcomes that could end a probationary career.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, junior_crew, payer,
    powerless, biographical, trapped, national).

% Fly on aircraft crewed by pilots whose competence is maintained under this hybrid regime. They cannot observe or verify the training mix directly and have no exit from the constraint's operation other than not flying; they benefit passively from a requirement they never negotiate.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, flying_public, beneficiary,
    powerless, immediate, trapped, global).

% Absorb the operational cost of scheduling real aircraft out of revenue service for training legs and coordinating non-jeopardy audits around live operations, while also gaining professional legitimacy and budget from being the visible custodians of the safety function.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, training_departments, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, training_departments, beneficiary).

% Have a commercial interest in the requirement shifting further toward simulation (their product) and away from real-aircraft time, but are not parties to the certification debate and have no direct say in how the hybrid ratio is set, despite lobbying through industry associations.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, simulator_manufacturers, excluded,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__hybrid_dependency, carrier_safety_departments).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__hybrid_dependency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that neither pure simulation nor pure real-world practice alone reliably maintains rare-event competence: simulation builds procedural muscle memory and allows rehearsal of failures too dangerous to stage live, while real aircraft time and non-jeopardy line audits anchor that rehearsal against the sensory, organizational, and interpersonal texture of actual operations that simulation cannot fully replicate.
% TRANSFER_FUNCTION: Moves scheduled flying hours, rest time, and career risk exposure from pilots (especially junior crew) to the training and safety apparatus, in exchange for certified currency; moves liability protection and public trust capital to carriers and regulators who administer the requirement.
% ABSENT_VOICES: Simulator manufacturers, who profit from the simulation component and have a structural interest in expanding its share of the requirement, are lobbying stakeholders but not seated parties to the certification-ratio decision. Passengers, the ultimate beneficiaries, have no voice in setting the mix at all.
% DISAPPEARANCE_RATIONALE: If the real-world anchoring component vanished and only simulation training remained, carriers would save substantial scheduling and revenue-service cost, but the competence base would drift toward a fragile equilibrium calibrated to simulator physics rather than live aircraft behavior — this is a testable degradation, not a mere preference shift, and the certification and insurance regime would need to be rebuilt around a different risk model.
% FOUNDING_PROBLEM: Early flight training and later jet-era CRM failures showed that pilots who trained exclusively in early low-fidelity simulators or exclusively through infrequent real-world exposure both developed dangerous competence gaps — the hybrid requirement was built after accident investigations (e.g., loss-of-control and automation-complacency events) identified specific failure modes traceable to each pure approach.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident investigation boards (not carriers or regulators themselves) continue to cite both simulator-only training gaps and real-world exposure gaps in post-incident reports; academic human-factors researchers outside the airline industry corroborate that the specific failure modes the hybrid regime targets remain observed in current data, not merely historical.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__hybrid_dependency_tests).
:- end_tests(competence_exercise_requirement__hybrid_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) — the requirement is not primarily rent-extraction but does impose real, unevenly distributed cost. Suppression is moderate (0.42): pilots cannot simply opt out of the real-aircraft component without losing certification, but the requirement is not coercive in the sense of foreclosing exit from the profession altogether. Theater ratio is modest but rising (0.18 to 0.28) reflecting a genuine risk in hybrid regimes: as scheduling pressure grows, the real-world anchoring component can drift toward minimally-compliant, ritualized line checks rather than genuinely diagnostic exercise — a Goodhart-style drift the temporal series is intended to flag for review, not to assert has already occurred.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator and safety-department seats, the requirement reads as pure coordination — a necessary technical standard they steward on behalf of public safety. From the junior-crew seat, the same requirement reads as an unevenly distributed burden imposed by parties who do not fly the audited legs or absorb the schedule disruption. The engine's per-seat computation should reflect this asymmetry rather than resolving it toward either seat's self-report.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory agencies and carrier safety departments sit near the beneficiary end: they administer the requirement, capture the liability and trust benefits, and bear little of the schedule or career-risk cost directly. Line pilots and especially junior crew sit near the target end: they supply the real-aircraft hours and absorb audit risk with constrained or trapped exit (their only alternative to compliance is exiting the profession). The flying public is a passive, powerless beneficiary — trapped in the sense of having no practical alternative to flying but also collecting the safety benefit without bearing any of the transfer cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid framing prevents mislabeling this as pure extraction: there is a real, well-corroborated coordination function (closing failure modes neither pure simulation nor pure real-exposure training close) that would not disappear if the constraint were removed — accident data would likely worsen. It also prevents mislabeling it as pure coordination (a Rope): the cost of the real-world anchoring component falls disproportionately and structurally on the least powerful seats (junior crew), and the requirement is actively enforced against them via certification consequences, which is exactly the asymmetric-extraction-plus-coordination signature of Tangled Rope rather than Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_ratio_correctness,
    'Is the specific mix of simulation hours to real-world anchoring hours mandated by current regulation actually calibrated to the failure modes it targets, or is the ratio a historical artifact that has not been re-derived from current accident and near-miss data?',
    'Longitudinal comparison of competence-related incident rates across carriers or jurisdictions with different simulation-to-real-world ratios, controlling for fleet type and route complexity.',
    'If the ratio is poorly calibrated, the requirement is extracting real-world hour costs from pilots without a corresponding safety return, shifting the classification toward a less-justified extraction; if well-calibrated, the coordination function is strongly validated and extraction is better understood as a necessary cost of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_ratio_correctness, empirical, 'Whether the mandated simulation/real-world mix is empirically calibrated or a legacy artifact.').

omega_variable(
    reading_boundary_location,
    'Where exactly does the hybrid_dependency reading''s boundary with simulation_as_adequate_exercise sit — at what simulator fidelity level would the real-world anchoring component become redundant, if ever?',
    'Track simulator fidelity improvements (motion, visual, and physiological cueing) against incident rates in carriers that have experimentally reduced real-world hour requirements; identify whether a fidelity threshold exists past which the two readings converge.',
    'If a fidelity threshold is found where outcomes are statistically indistinguishable from the hybrid regime, this reading''s core claim (that real-world anchoring is irreducibly necessary) would be falsified for that fidelity class, collapsing this story into the simulation_as_adequate_exercise reading rather than the two remaining structurally distinct constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_location, conceptual, 'Whether the hybrid reading''s distinguishing premise survives future simulator fidelity gains.').

omega_variable(
    junior_crew_burden_asymmetry,
    'Is the disproportionate real-aircraft-hour burden on junior crew a necessary feature of any hybrid regime (junior pilots need more anchoring because they have less accumulated tacit experience) or an avoidable scheduling-power artifact (senior pilots use seniority to claim favorable slots, pushing burden downward)?',
    'Compare hour-allocation patterns across carriers with seniority-based versus needs-based scheduling systems for training and audit slots.',
    'If seniority-driven, the asymmetric extraction on junior crew is a fixable organizational artifact rather than an inherent feature of the coordination function, which would narrow the tangled-rope classification''s extractive component without touching its coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(junior_crew_burden_asymmetry, empirical, 'Whether junior-crew burden asymmetry is structurally necessary or a scheduling-power artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t4, competence_exercise_requirement__hybrid_dependency, theater_ratio, 4, 0.2).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__hybrid_dependency, theater_ratio, 8, 0.22).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_requirement__hybrid_dependency, theater_ratio, 12, 0.24).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__hybrid_dependency, theater_ratio, 16, 0.25).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__hybrid_dependency, theater_ratio, 20, 0.27).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__hybrid_dependency, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comp_be_t4, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(comp_be_t12, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 24, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comp_su_t4, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 4, 0.37).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(comp_su_t12, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 20, 0.415).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__hybrid_dependency, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, catastrophe_as_necessary_anchor).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the competence_exercise_requirement kernel. simulation_as_adequate_exercise claims high-fidelity simulation alone suffices (lower extraction, closer to Rope). catastrophe_as_necessary_anchor claims only real catastrophic or near-miss events provide irreducible exercise (structurally cannot be safely administered as policy, and would likely classify very differently given its ethical impossibility as a designed regime). This story (hybrid_dependency) claims an intermediate, actively-administered mixed regime, with its own distinct beneficiary/victim structure and extraction profile — it is not an average of the other two but a structurally distinct claim about what a working competence regime requires.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
