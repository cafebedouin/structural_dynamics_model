% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__simulation_as_sufficient, []).

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
 *   constraint_id: competence_retention_exercise__simulation_as_sufficient
 *   human_readable: Simulation-as-Sufficient Reading of Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the 'simulation_as_sufficient' reading of
 *   the competence_retention_exercise kernel: the claim that high-fidelity
 *   simulator exercise is structurally equivalent to real
 *   catastrophe-response performance, such that simulator scores are a valid
 *   and sufficient basis for certifying operator competence. Under this
 *   reading, training infrastructure becomes the primary (often sole)
 *   competence-maintenance mechanism; real catastrophes are actively
 *   prevented rather than incorporated as feedback; and competence is
 *   operationally defined by simulator performance metrics rather than by
 *   field outcomes. The reading has a genuine coordination function — it
 *   solves the real problem that catastrophic events are too rare and too
 *   destructive to train on directly — but it is also structurally convenient
 *   for every institutional party positioned to benefit from not having to
 *   validate the equivalence claim against real-world outcomes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.42).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.38).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "Simulation-as-Sufficient Reading of Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, '70f17bea-2ab3-40f0-ac49-06dcf1c083b5').
narrative_ontology:cs_kernel_codification('70f17bea-2ab3-40f0-ac49-06dcf1c083b5', distributed).
narrative_ontology:cs_authority_grounding('70f17bea-2ab3-40f0-ac49-06dcf1c083b5', practice).
narrative_ontology:cs_interpretation_layer_present('70f17bea-2ab3-40f0-ac49-06dcf1c083b5').
narrative_ontology:cs_reading_relation('70f17bea-2ab3-40f0-ac49-06dcf1c083b5', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_reading_relation('70f17bea-2ab3-40f0-ac49-06dcf1c083b5', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('70f17bea-2ab3-40f0-ac49-06dcf1c083b5', foundational, procedural_cognitive_equivalence).
narrative_ontology:cs_axiom_status(procedural_cognitive_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('70f17bea-2ab3-40f0-ac49-06dcf1c083b5', procedural_cognitive_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('70f17bea-2ab3-40f0-ac49-06dcf1c083b5', secondary, simulator_metrics_are_valid_competence_proxy).
narrative_ontology:cs_axiom_status(simulator_metrics_are_valid_competence_proxy, holdable).
narrative_ontology:cs_axiom_grounding('70f17bea-2ab3-40f0-ac49-06dcf1c083b5', simulator_metrics_are_valid_competence_proxy, instrumental).
narrative_ontology:cs_reference_frame('70f17bea-2ab3-40f0-ac49-06dcf1c083b5', high_fidelity_transfer_paradigm).
narrative_ontology:cs_drift_state('70f17bea-2ab3-40f0-ac49-06dcf1c083b5', post_repeated_incident_investigation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('70f17bea-2ab3-40f0-ac49-06dcf1c083b5', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulator_vendors).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, regulatory_certification_bodies).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, operations_management).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, training_directorate_staff).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, downstream_public_exposed_to_residual_risk).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, high_fidelity_transfer_hypothesis).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, procedural_equivalence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, administers, and certifies the simulator curriculum; sets pass/fail thresholds on simulator performance metrics and treats those metrics as the operational definition of competence. Their institutional standing, budget, and professional identity depend on simulation being accepted as sufficient — they collect authority and resources from the arrangement's continuation.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, training_directorate_staff, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell increasingly expensive high-fidelity training platforms whose market value depends entirely on the claim that simulator exercises are structurally equivalent to real catastrophic events. They have no exposure to the consequences if the equivalence claim is wrong; they sell upgrades either way.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulator_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Accept simulator hours and simulator-scored competence as satisfying licensing and recertification requirements, which lets them discharge their oversight mandate without needing to observe operators' actual catastrophe-response performance, which by design almost never occurs. Certifying on simulator metrics is administratively cheap and legally defensible.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, regulatory_certification_bodies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, regulatory_certification_bodies, agenda_setter).

% Benefits from a competence-maintenance regime that produces a certified, insurable workforce without disrupting production schedules the way real incident exposure or extended field drills would. Simulation lets staffing and shift economics proceed undisturbed while liability is nominally discharged.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, operations_management, beneficiary,
    institutional, biographical, arbitrage, national).

% Are the ones whose actual catastrophe-response competence is on the line during a real event, yet their professional standing, promotion, and continued licensure are gated on simulator scores that may not capture the physiological stress, ambiguity, and irreversible stakes of a genuine catastrophe. If the equivalence claim is false, they are the ones who discover this in the moment it matters most, having been told by the system that they were already prepared.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Lives, works, or resides near the facilities or systems whose operators are certified competent on the strength of simulator performance. Has no visibility into whether the equivalence claim holds, no seat at the table when certification standards are set, and bears the consequence if a real catastrophe reveals a gap between simulated and actual competence.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, downstream_public_exposed_to_residual_risk, payer,
    powerless, generational, trapped, regional).

% Study the transfer validity of simulation training and would testify to gaps between simulator fidelity and real physiological/organizational stress responses, but are rarely invited into certification-standard-setting processes, which are dominated by vendors, regulators, and the institutions being certified.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, independent_safety_researchers, excluded,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, repeatable, safety-preserving way to maintain and verify catastrophe-response competence without requiring operators to actually experience catastrophic events, which are rare, destructive, and ethically impossible to induce for training purposes.
% TRANSFER_FUNCTION: Moves the burden of proving competence from lived catastrophic experience onto simulator performance metrics; moves budget from incident-response capacity building toward simulator infrastructure procurement; moves regulatory liability from certifying bodies onto the equivalence claim itself, and ultimately onto frontline operators if the claim proves false during a real event.
% ABSENT_VOICES: Independent transfer-validity researchers and the downstream public are structurally absent from the standard-setting process; both would raise the question of whether simulator scores actually predict real-event performance, but neither sits on the certification committees where the equivalence claim is operationalized.
% DISAPPEARANCE_RATIONALE: If simulation-as-sufficient were abandoned overnight, training directorates, vendors, and certifying bodies would face an immediate crisis — there is no scalable substitute for competence verification that doesn't rely on either real catastrophes (unacceptable) or near-misses (insufficiently frequent). Whether the 'world rearranges' or 'the world was already miscalibrated and this just reveals it' is exactly the contested question the sibling readings dispute.
% FOUNDING_PROBLEM: Organizations operating catastrophe-prone systems (nuclear, aviation, maritime, chemical process) needed a way to build and verify operator competence for events too rare, too destructive, or too ethically fraught to allow direct experiential training.
% FOUNDING_PROBLEM_CORROBORATION: Training directorates and vendors attest the problem is solved — simulator fidelity now approximates real cognitive/procedural load. Independent human-factors researchers outside the certifying institutions (published transfer-validity studies, post-incident investigation boards in aviation and nuclear sectors) have documented specific gaps — stress physiology, ambiguity tolerance, consequence-weight perception — that simulators do not reproduce, suggesting the founding problem is only partially solved and the equivalence claim outruns the evidence.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, contested).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).
:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) rather than severe: the coordination function is real and substantial, so this is not a pure extraction story. But extraction is rising over the interval as simulator infrastructure has scaled into a self-sustaining budget and certification ecosystem that is structurally insulated from disconfirmation (an actual catastrophe would validate or invalidate the equivalence claim, but the entire point of the arrangement is to prevent catastrophes from occurring, which means the claim is rarely tested against the outcome it purports to substitute for). Theater ratio rises modestly (0.12 to 0.31) as simulator-metric optimization increasingly substitutes for the harder-to-measure question of real transfer validity — a mild Goodhart drift where scoring well on the simulator becomes the target rather than a proxy for it. Suppression is moderate: the arrangement does not coercively suppress dissent so much as structurally exclude the parties (independent researchers, downstream public) who would most directly test the equivalence claim from the standard-setting process.
 *
 * PERSPECTIVAL GAP:
 *   From the training directorate's seat, this looks like professionalized, evidence-based competence management — simulator fidelity has improved for decades and the discipline has real expertise. From the frontline operator's seat under real catastrophic stress, the gap between simulator confidence and lived uncertainty is exactly what the sibling readings (catastrophe_as_necessary, near_miss_as_bridge) predict will surface. The engine should compute a divergence between the agenda_setter/beneficiary seats (which see coordination) and the payer seats (which carry the risk of a false equivalence) — that divergence is the analytical content of this reading, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Training directorate staff, simulator vendors, certification bodies, and operations management are declared beneficiaries because each collects something (authority, revenue, discharged liability, undisturbed production economics) from the equivalence claim's acceptance, without being the party who actually pays if the claim is wrong. Frontline operators are the target: their real competence is what is actually at stake during a genuine catastrophe, and they bear whatever gap exists between simulator performance and real-event performance, having been certified by a system that told them the gap is negligible. The downstream public is also a victim: powerless, trapped by geography or infrastructure dependency, generational time horizon, bearing residual risk they cannot see or contest. This is why the constraint reads as tangled_rope rather than snare: the coordination function (competence maintenance without requiring real catastrophes) is genuine and serves operators and public alike when the equivalence claim holds — the extraction is the asymmetry in who bears the cost when it doesn't.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (training for events too rare/destructive to experience directly) remains genuinely live — this is not a dead mandate propped up by inertia. What keeps the classification from collapsing into either a pure rope (if the equivalence claim is simply true) or a pure snare (if it is simply false) is that the claim's truth is precisely the unresolved, structurally hard-to-test question the sibling readings dispute. Tangled rope captures a constraint that is doing real coordination work while also being systematically insulated, by its own success at catastrophe prevention, from the disconfirming evidence that would settle whether it deserves the confidence placed in it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulator_transfer_validity,
    'Does simulator performance actually predict real-catastrophe performance, or does it predict only simulator-specific performance that fails to transfer under genuine physiological stress, ambiguity, and irreversible stakes?',
    'Longitudinal comparison of simulator-certified operators'' performance in the rare real incidents that do occur, cross-referenced against post-incident investigation findings on decision quality, stress response, and procedural adherence; meta-analysis of transfer-validity literature from aviation, nuclear, and maritime domains.',
    'If transfer validity is high, this reading approaches a genuine rope with modest inherent extraction (the coordination cost of maintaining simulator infrastructure). If transfer validity is low or unverifiable, the constraint is closer to a snare wearing coordination language, with frontline operators and the public bearing an undisclosed competence gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulator_transfer_validity, empirical, 'Whether simulator fidelity actually produces equivalent real-event competence.').

omega_variable(
    kernel_reading_contest_location,
    'Given that this constraint is one of three live readings of the competence_retention_exercise kernel, where exactly does the disagreement with the sibling readings live — in the empirical transfer-validity question, or in a deeper disagreement about what ''genuine competence'' even means (procedural correctness vs. lived judgment under irreversible stakes)?',
    'Structured elicitation from safety engineering academics, incident investigators, and training directorates on whether they disagree about facts (does simulation transfer) or about the definition of competence itself (is procedural equivalence sufficient, or does competence require having genuinely faced irreversible stakes).',
    'If the disagreement is purely empirical, better simulator validation studies could in principle resolve the kernel contest. If the disagreement is conceptual (a contested definition of competence), no amount of transfer-validity data will settle which reading is correct — the three readings would remain permanently coexisting, each internally coherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether the kernel contest is empirically resolvable or reflects an irreducible conceptual disagreement about competence.').

omega_variable(
    self_insulating_evidence_structure,
    'Is the arrangement''s success at preventing catastrophes (which is good) also what prevents the equivalence claim from ever being tested against real outcomes (which insulates the claim from disconfirmation)?',
    'Examine whether certifying bodies and training directorates have mechanisms for detecting a false equivalence claim BEFORE a real catastrophe occurs (e.g., red-team adversarial testing, physiological stress instrumentation during simulation, blind comparison against near-miss outcomes) — or whether the only real test is the catastrophe itself.',
    'If no pre-catastrophe validation mechanism exists, the constraint''s coordination function and its extractive insulation from disconfirmation are two sides of the same structural feature, which is a strong argument for treating this as tangled_rope rather than pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_insulating_evidence_structure, conceptual, 'Whether catastrophe prevention structurally doubles as evidence-suppression for the equivalence claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 8, 0.16).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 16, 0.2).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 24, 0.24).
narrative_ontology:measurement(comp_tr_t32, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 32, 0.28).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(comp_be_t32, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 16, 0.32).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 24, 0.35).
narrative_ontology:measurement(comp_su_t32, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 32, 0.37).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__simulation_as_sufficient, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the competence_retention_exercise kernel. catastrophe_as_necessary and near_miss_as_bridge are separate constraint files, not alternative measurements of this one — each has its own ε, beneficiary/victim structure, and claimed type. This reading (simulation_as_sufficient) has lower authored extraction than a catastrophe_as_necessary reading would (which would treat the absence of real catastrophic exposure itself as a suppressed alternative) and a different beneficiary structure than near_miss_as_bridge (which would center incident-investigation bodies rather than simulator vendors as primary beneficiaries).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
