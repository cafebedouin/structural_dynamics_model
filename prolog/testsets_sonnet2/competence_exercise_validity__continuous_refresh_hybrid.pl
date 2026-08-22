% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__continuous_refresh_hybrid, []).

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
 *   constraint_id: competence_exercise_validity__continuous_refresh_hybrid
 *   human_readable: Continuous Drill-Cycle Requirement for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint governs whether safety-critical personnel must undergo
 *   recurring drill cycles rather than a single simulation-based
 *   certification. The continuous-refresh-hybrid reading holds that
 *   simulation is necessary (it is the only way to safely exercise
 *   low-frequency catastrophic scenarios) but not sufficient (competence
 *   decays without repeated rehearsal, so validity depends on process
 *   continuity, not on any one exercise being a perfect proxy for real
 *   catastrophe). The claim is tangled_rope because the coordination function
 *   is genuine — decay is real and documented — but the cadence and cost
 *   allocation increasingly serve administrator credibility and vendor
 *   revenue as much as retention, and shift workers bear a disproportionate
 *   share of the recurring cost without a comparable voice in cadence design.
 *
 * KEY AGENTS:
 *   - drill_program_administrators: sets cadence, captures institutional credibility
 *   - frontline_operators: benefits from retained competence, pays in fatigue and disruption
 *   - shift_workers_subject_to_drill_load: powerless payer, trapped by certification dependency
 *   - understaffed_facility_operators: constrained payer, cannot fund cycle without cutting coverage
 *   - simulation_vendors: organized beneficiary profiting from cadence length and frequency
 *   - safety_regulators: analytical observer setting minimum legal cadence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.42).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.38).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.42).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Continuous Drill-Cycle Requirement for Competence Retention").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, '14142a4d-8e2b-4cd0-a107-03a0d8476f0c').
narrative_ontology:cs_kernel_codification('14142a4d-8e2b-4cd0-a107-03a0d8476f0c', distributed).
narrative_ontology:cs_authority_grounding('14142a4d-8e2b-4cd0-a107-03a0d8476f0c', expertise).
narrative_ontology:cs_interpretation_layer_present('14142a4d-8e2b-4cd0-a107-03a0d8476f0c').
narrative_ontology:cs_reading_relation('14142a4d-8e2b-4cd0-a107-03a0d8476f0c', competence_exercise_validity__simulation_as_proxy, influences).
narrative_ontology:cs_reading_relation('14142a4d-8e2b-4cd0-a107-03a0d8476f0c', competence_exercise_validity__real_catastrophe_only, influences).
narrative_ontology:cs_axiom('14142a4d-8e2b-4cd0-a107-03a0d8476f0c', foundational, competence_is_process_dependent_not_state_validated).
narrative_ontology:cs_axiom_status(competence_is_process_dependent_not_state_validated, holdable).
narrative_ontology:cs_axiom_grounding('14142a4d-8e2b-4cd0-a107-03a0d8476f0c', competence_is_process_dependent_not_state_validated, empirically_contingent).
narrative_ontology:cs_axiom('14142a4d-8e2b-4cd0-a107-03a0d8476f0c', foundational, simulation_necessary_but_insufficient_alone).
narrative_ontology:cs_axiom_status(simulation_necessary_but_insufficient_alone, holdable).
narrative_ontology:cs_axiom_grounding('14142a4d-8e2b-4cd0-a107-03a0d8476f0c', simulation_necessary_but_insufficient_alone, empirically_contingent).
narrative_ontology:cs_reference_frame('14142a4d-8e2b-4cd0-a107-03a0d8476f0c', process_dependent_competence_model).
narrative_ontology:cs_drift_state('14142a4d-8e2b-4cd0-a107-03a0d8476f0c', post_incident_review_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('14142a4d-8e2b-4cd0-a107-03a0d8476f0c', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, public_safety_beneficiaries).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, drill_program_administrators).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, shift_workers_subject_to_drill_load).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, understaffed_facility_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, simulation_vendors).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, schedules, and audits the recurring drill cycle — tabletop exercises, full-scale simulations, refresher certifications — and sets the cadence (quarterly, annual) that facilities must meet. Justifies the cadence by citing skill-decay research and near-miss postmortems where lapsed drilling correlated with slower real-event response. Bears little of the labor cost directly; captures institutional credibility and regulatory standing from being able to show a maintained program.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, drill_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Undergo the recurring drills, which genuinely rehearse muscle memory, communication protocols, and decision sequences under time pressure. Their real competence during actual incidents demonstrably degrades within months of the last drill, so they benefit from the cycle continuing — but they also absorb the recurring time cost, schedule disruption, and drill fatigue that a single validation event would not impose.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators, payer).

% Rotating shift staff who must fit drill participation around already-compressed schedules; missed drills trigger recertification requirements and pay docking in some facilities. They cannot decline the cycle without losing their qualification to work the floor, and understaffing means drills are frequently scheduled on top of, not instead of, regular duty.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, shift_workers_subject_to_drill_load, payer,
    powerless, immediate, trapped, local).

% Facility managers who must staff and fund the recurring drill infrastructure — trainers, simulators, backfill coverage — out of operating budgets that were not sized for continuous drilling. They cannot exit the requirement without losing accreditation, but chronically lack the headcount to run drills without cutting into live operational coverage.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, understaffed_facility_operators, payer,
    moderate, biographical, constrained, regional).

% Communities near the regulated facilities who benefit diffusely and invisibly when drilled response teams handle real incidents competently. They have no direct role in setting or bearing the cycle's cost and would only notice its absence after a failure.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, public_safety_beneficiaries, beneficiary,
    powerless, generational, analytical, regional).

% Firms that build and sell simulator hardware, tabletop exercise packages, and certification tracking software. They profit directly from the cadence requirement and lobby for longer, more frequent, more instrumented cycles regardless of marginal retention benefit past a certain point.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, simulation_vendors, beneficiary,
    organized, biographical, mobile, national).

% Set the minimum legal drill cadence and audit compliance records. They rely on incident data and skill-decay literature to justify cadence requirements and can raise or lower the mandated frequency based on post-incident review.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__continuous_refresh_hybrid, diffuse).
narrative_ontology:fixing_cost_class(competence_exercise_validity__continuous_refresh_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a real and repeatedly-demonstrated retention problem: procedural and psychomotor competence for low-frequency, high-consequence events decays measurably within months without rehearsal, so a recurring cycle — not a single test — is what keeps response capability at the level a real event requires.
% TRANSFER_FUNCTION: Moves scheduling authority, budget allocation, and labor time from facility operators and shift workers toward drill administration infrastructure and simulation vendors, in exchange for maintained (rather than merely certified) response competence.
% ABSENT_VOICES: Shift workers rarely have a formal channel to contest cadence design against their scheduling reality; their objection — that the cycle is calibrated to institutional risk-aversion rather than to the actual decay curve of their specific skill set — is absorbed as compliance friction rather than surfaced as design input.
% DISAPPEARANCE_RATIONALE: If the continuous drill requirement vanished and only initial simulation-based certification remained, response competence would measurably decay within the documented skill-half-life window; incident response times and error rates during real events would rise, and facilities would eventually revert to ad hoc re-training after the first bad outcome revealed the gap.
% FOUNDING_PROBLEM: Early safety programs relied on one-time simulation certification; multiple post-incident reviews found that certified personnel performed far below their tested competence when the real event came years after their last drill, because the underlying skills had decayed even though the certification remained formally valid.
% FOUNDING_PROBLEM_CORROBORATION: Independent incident-review boards and academic skill-decay researchers outside the drill administration apparatus corroborate that decay is real and documented; facility operators and shift workers, who bear the cycle's cost rather than administer it, independently corroborate that the underlying retention problem is genuine even as they contest the cadence's calibration and cost-sharing.
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__continuous_refresh_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__continuous_refresh_hybrid, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).
:- end_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) and rising slowly: the coordination function (real skill decay, real retention benefit) is genuine and dominant, but a growing share of the cadence is set by administrative and vendor interests rather than by the decay curve alone, producing gradual rent accumulation on top of a legitimate coordination core. Theater ratio rises modestly (0.30) reflecting some drift toward compliance-documentation exercises rather than pure skill rehearsal, but stays below the level that would indicate the function has substantially atrophied. Suppression is moderate (0.38): the requirement is enforced through certification-linked employment consequences, not through concealment of alternatives — workers know a single-validation alternative exists, they simply cannot access it without losing accreditation.
 *
 * PERSPECTIVAL GAP:
 *   From the administrator seat, the cycle is a well-evidenced, institutionally validated coordination mechanism defending against demonstrated real-world decay. From the shift-worker seat, the same cycle computes closer to extraction: the decay science may be real, but the specific cadence, timing, and cost-sharing were never negotiated with the people bearing the recurring burden, and cadence increases track vendor and regulatory incentives as much as any measured decay rate for their specific role.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontline operators and public safety beneficiaries sit near the coordination/beneficiary end — decayed competence directly threatens them and the public they serve, so the cycle genuinely protects their interests even though operators also bear its cost, hence the dual role. Shift workers and understaffed facility operators sit nearer the target end: they bear the recurring cost of a cadence set above them, with constrained or trapped exit because certification lapse ends their ability to work the role at all. Simulation vendors and drill administrators sit at the clear beneficiary end — they set or profit from cadence without bearing its operational cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — competence decay after one-time certification — remains empirically live (corroborated by independent incident-review boards, not just the administering bodies), which is exactly why this does not resolve as pure extraction: unlike a scaffold whose transition ended or a piton whose function atrophied, the underlying skill-decay mechanism this constraint responds to continues to operate. The tangled_rope classification exists precisely to prevent this genuine, still-necessary coordination function from being mislabeled as pure extraction merely because vendors and administrators have layered rent-seeking cadence-inflation on top of it — and to prevent the coordination function from being used to wave away the real, disproportionate cost borne by powerless shift workers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cadence_calibration_to_decay_curve,
    'Is the mandated drill cadence actually calibrated to the empirical skill-decay half-life for each specific role, or is it set above the decay-justified minimum by administrative risk-aversion and vendor lobbying?',
    'Compare role-specific skill-decay curves from independent research against the mandated cadence for that role; a persistent gap where cadence exceeds decay-justified frequency across many roles would indicate rent-seeking inflation layered on the coordination core.',
    'If cadence is well-calibrated, the extraction score should be near the coordination floor; if cadence systematically exceeds the decay-justified minimum, effective extraction is higher than the base score reflects and the tangled_rope character strengthens over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cadence_calibration_to_decay_curve, empirical, 'Whether the recurring cadence tracks actual decay science or exceeds it for institutional reasons.').

omega_variable(
    kernel_reading_boundary,
    'This story instantiates the continuous_refresh_hybrid reading of the competence_exercise_validity kernel. The sibling readings — simulation_as_proxy (simulation itself counts as valid ongoing exercise) and real_catastrophe_only (only actual catastrophe truly exercises competence, simulation is an insufficient substitute) — locate the disagreement differently: simulation_as_proxy treats the exercise-validity question as resolved once initial certification is achieved, while real_catastrophe_only treats simulation as never validly exercising competence regardless of repetition. Where does the actual empirical disagreement live?',
    'Longitudinal comparison of response performance in real incidents between personnel drilled continuously vs. certified once vs. never drilled but theoretically trained; a real-event performance gap between the continuous-drill group and the one-time-certified group (holding real-event exposure constant) would support this reading over simulation_as_proxy; a persistent gap between drilled-and-never-tested-in-reality personnel and real-catastrophe survivors would lend weight to real_catastrophe_only''s skepticism of simulation''s sufficiency.',
    'If continuous drilling closes the real-event performance gap to near the level of real-catastrophe-experienced personnel, this reading is strongly vindicated over both siblings. If a persistent gap remains regardless of drill frequency, real_catastrophe_only gains ground and this reading''s ''necessary but not sufficient'' framing would need revision toward ''insufficient regardless of frequency.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Where the three kernel readings'' disagreement is empirically located and what would adjudicate it.').

omega_variable(
    vendor_capture_of_cadence_design,
    'Do simulation vendors and drill administrators exercise disproportionate influence over cadence-setting relative to their stake in worker and facility-operator outcomes?',
    'Trace cadence-standard revision history against vendor contract renewal cycles and lobbying records; correlation between contract timing and cadence increases would indicate capture.',
    'If capture is present, a portion of the measured extraction is attributable to rent-seeking rather than genuine retention need, and the beneficiary/victim structure should weight administrators and vendors more heavily as extraction recipients.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_capture_of_cadence_design, empirical, 'Whether cadence-setting has been captured by parties who profit from cadence length independent of retention benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comp_tr_t4, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 4, 0.18).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 8, 0.21).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 12, 0.24).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 16, 0.26).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 20, 0.28).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comp_be_t4, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(comp_be_t8, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(comp_be_t12, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(comp_be_t16, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(comp_be_t24, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t4, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(comp_su_t8, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(comp_su_t12, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(comp_su_t16, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(comp_su_t24, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__continuous_refresh_hybrid, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__real_catastrophe_only).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the competence_exercise_validity kernel. simulation_as_proxy holds that simulation itself constitutes valid ongoing exercise once certified (lower authored extraction, since the coordination function is treated as satisfied by simulation alone). real_catastrophe_only holds that simulation categorically cannot exercise true competence (higher authored extraction on the simulation-as-substitute claim, since simulation is read as institutional theater covering an unexercisable gap). This reading (continuous_refresh_hybrid) sits between them: simulation is necessary infrastructure but validity depends on the continuity of the drill process, not on any single exercise's fidelity to real catastrophe. Each reading has a distinct ε because each authors a different standing arrangement under contest — the frequency-and-necessity claim, the proxy-sufficiency claim, and the categorical-insufficiency claim are structurally different assertions, not the same claim measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
