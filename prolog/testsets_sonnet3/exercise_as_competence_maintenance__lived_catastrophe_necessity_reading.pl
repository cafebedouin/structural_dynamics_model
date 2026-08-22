% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
 *   human_readable: Simulation-as-Competence-Maintenance Regime (Lived-Catastrophe-Necessity Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This story instantiates the lived-catastrophe-necessity reading of the
 *   exercise-as-competence-maintenance kernel. Under this reading, simulation
 *   and tabletop exercises exercise procedural memory but categorically
 *   cannot exercise judgment-under-genuine-irreversible-stakes — that
 *   capacity, once formed, only decays or fails to form absent actual
 *   catastrophe exposure, and no amount of simulation fidelity closes the
 *   gap. The constraint under contest is the standing institutional
 *   arrangement in which simulation-based certification is treated (by
 *   regulators, vendors, and leadership) as sufficient evidence of
 *   operational readiness. Sibling readings of the same kernel —
 *   simulation_sufficiency_reading (fidelity closes the gap entirely) and
 *   hybrid_decay_reading (the kernel has two separable components, one
 *   exercisable by simulation and one not) — are NOT this constraint; they
 *   are separate stories linked via network.affects_constraints. This
 *   reading's ε is authored for the standing simulation-certification
 *   arrangement AS THIS READING SEES IT: substantially extractive because it
 *   converts an unresolvable epistemic gap (you cannot ethically manufacture
 *   real catastrophe to test judgment) into a legible compliance product that
 *   institutions and vendors monetize while frontline operators and the
 *   public absorb the undisclosed residual risk.
 *
 * KEY AGENTS:
 *   - exercise_program_administrators: agenda_setter (institutional/arbitrage) — designs and certifies the proxy
 *   - training_vendors: beneficiary (organized/mobile) — sells the simulation product whose sufficiency is contested
 *   - frontline_operators_never_tested_live: payer (powerless/trapped) — bears the atrophy and the blame when it surfaces
 *   - downstream_public_exposed_to_untested_competence: payer (powerless/trapped) — bears the consequences without visibility into the wager
 *   - safety_researchers_studying_near_misses: observer (analytical/analytical) — sees the pattern across domains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.62).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.58).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Simulation-as-Competence-Maintenance Regime (Lived-Catastrophe-Necessity Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '789b5dd0-0256-4e70-a890-2dbcc9bed6a3').
narrative_ontology:cs_kernel_codification('789b5dd0-0256-4e70-a890-2dbcc9bed6a3', distributed).
narrative_ontology:cs_authority_grounding('789b5dd0-0256-4e70-a890-2dbcc9bed6a3', practice).
narrative_ontology:cs_interpretation_layer_present('789b5dd0-0256-4e70-a890-2dbcc9bed6a3').
narrative_ontology:cs_reading_relation('789b5dd0-0256-4e70-a890-2dbcc9bed6a3', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('789b5dd0-0256-4e70-a890-2dbcc9bed6a3', exercise_as_competence_maintenance__hybrid_decay_reading, coexists_with).
narrative_ontology:cs_axiom('789b5dd0-0256-4e70-a890-2dbcc9bed6a3', foundational, judgment_under_stakes_categorically_unsimulable).
narrative_ontology:cs_axiom_status(judgment_under_stakes_categorically_unsimulable, holdable).
narrative_ontology:cs_axiom_grounding('789b5dd0-0256-4e70-a890-2dbcc9bed6a3', judgment_under_stakes_categorically_unsimulable, empirically_contingent).
narrative_ontology:cs_axiom('789b5dd0-0256-4e70-a890-2dbcc9bed6a3', secondary, competence_decays_covertly_absent_real_activation).
narrative_ontology:cs_axiom_status(competence_decays_covertly_absent_real_activation, holdable).
narrative_ontology:cs_axiom_grounding('789b5dd0-0256-4e70-a890-2dbcc9bed6a3', competence_decays_covertly_absent_real_activation, empirically_contingent).
narrative_ontology:cs_reference_frame('789b5dd0-0256-4e70-a890-2dbcc9bed6a3', post_event_lesson_codification_practice).
narrative_ontology:cs_drift_state('789b5dd0-0256-4e70-a890-2dbcc9bed6a3', contemporary_simulation_industrialization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('789b5dd0-0256-4e70-a890-2dbcc9bed6a3', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_program_administrators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, training_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, regulatory_compliance_officers).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, institutional_leadership).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, frontline_operators_never_tested_live).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, downstream_public_exposed_to_untested_competence).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, post_incident_scapegoated_staff).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, readiness_certification_regime).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_frequency_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, schedule, and certify tabletop and simulated crisis exercises as the institution's demonstrated readiness. They control what counts as 'exercised,' set the cadence, and issue certifications that satisfy regulators and boards. They bear no personal exposure when a real catastrophe reveals the exercises did not transfer to lived judgment.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell simulation platforms, tabletop scenario packages, and certification services. Revenue depends on the belief that simulation is sufficient or at least the best available substitute for lived catastrophe; they have no incentive to publicize that fidelity ceilings exist that no simulation product can cross.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, training_vendors, beneficiary,
    organized, biographical, mobile, national).

% Accept exercise completion records as proof of institutional readiness because auditing lived-catastrophe judgment is not administratively tractable. Their compliance regime is built on a checkable proxy (exercises conducted) rather than the unmeasurable target (competence under real stakes), and they benefit from the proxy's legibility even when they suspect its insufficiency.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, regulatory_compliance_officers, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, regulatory_compliance_officers, agenda_setter).

% Point to exercise completion rates in public accountability moments and budget justifications. They are structurally insulated from the operational floor where atrophy actually occurs, and typically rotate out before a real catastrophe would test the claim they signed off on.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, institutional_leadership, beneficiary,
    institutional, biographical, arbitrage, national).

% Perform the drills, pass the certifications, and are told they are ready. Under this reading, their judgment-under-genuine-stakes has never been exercised and is silently atrophying or was never formed; when a real event finally arrives, they discover the gap in real time, often with lethal consequences for themselves or others, and are frequently blamed individually for a structural non-exercise.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, frontline_operators_never_tested_live, payer,
    powerless, biographical, trapped, local).

% Rely on the operators certified as ready — pilots, plant supervisors, emergency responders, ICU staff — without any way to independently verify whether that readiness has ever been exercised against real stakes rather than simulated ones. They bear the consequences of the competence gap without having consented to or even perceived the wager.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, downstream_public_exposed_to_untested_competence, payer,
    powerless, immediate, trapped, regional).

% After a real catastrophe exposes the gap between simulated and lived competence, individual operators are frequently disciplined or terminated for 'failure to perform to standard,' while the institutional decision to rely on simulation as sufficient evidence of readiness is not examined. The individual absorbs blame for a structural bet the institution made.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, post_incident_scapegoated_staff, payer,
    powerless, biographical, trapped, local).

% Study incident reports and near-miss data across industries (aviation, nuclear, medicine, emergency response) to compare simulated-only-trained operator performance against operators with lived-catastrophe exposure. They document the recurring finding that simulated training predicts procedural compliance but not judgment quality under genuine irreversible stakes.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, safety_researchers_studying_near_misses, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, diffuse).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simulated exercises genuinely coordinate procedural knowledge: checklist execution, role assignment, communication protocols, and equipment familiarity are transferable from rehearsal to reality, and without some form of structured practice these procedural elements would be worse, not better.
% TRANSFER_FUNCTION: Moves certification legitimacy and budget justification from the operational floor to institutional leadership and regulators, while moving the actual risk of untested judgment-under-stakes onto frontline operators and the public they serve — the institution collects the appearance of readiness; the exposed population bears the cost if that appearance is false.
% ABSENT_VOICES: Frontline operators rarely have standing to declare their own exercises insufficient — doing so would undermine their own certification and employability. Survivors and families of prior real-catastrophe failures where simulation-trained operators failed under lived stakes are the strongest evidentiary voice for this reading but are structurally excluded from program design; their testimony surfaces only in post-incident litigation, after the fact.
% DISAPPEARANCE_RATIONALE: If exercise programs vanished, regulatory and institutional signaling would collapse immediately (world_rearranges for the certification economy), but under this reading the actual competence floor would not change much, since the reading holds that simulation was never doing the exercising work claimed for it — the felt safety would disappear while the real safety gap, already present, would simply become visible. Whether the world 'rearranges' or 'stays the same' therefore depends on which layer — the compliance layer or the competence layer — is being asked about, which is exactly the contest this reading exists to name.
% FOUNDING_PROBLEM: Institutions cannot ethically or practically expose operators to repeated real catastrophes merely to maintain their skills, so simulation was built as the only available proxy for keeping competence sharp between rare real events.
% FOUNDING_PROBLEM_CORROBORATION: Exercise administrators and training vendors attest the founding problem is solved: simulation maintains readiness. Independent safety researchers studying post-incident reports across aviation, nuclear, and emergency medicine attest, from outside the certifying institutions, that simulated-only operators repeatedly show procedural compliance but degraded judgment under first genuine irreversible stakes — corroboration exists outside the beneficiary set, but it is contested by the vendors and administrators whose programs it implicates.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 and rising because, under this reading, the compliance regime's central claim (simulation maintains readiness) is treated as false at the margin that matters most — judgment under irreversible stakes — while the institutional apparatus built on that claim continues to collect legitimacy, budget, and reduced liability exposure from it. Theater ratio is authored high and rising (0.45 to 0.71) because, under this reading, an increasing share of exercise activity is oriented toward producing auditable completion records rather than toward closing a gap this reading holds simulation structurally cannot close — more exercises, more theater, no corresponding closure of the judgment gap. Suppression is moderate and rising: it is not coercive in the classic sense but operates through the unfalsifiability of the claim from inside the system — an operator who says 'I have never actually been tested' has no legible way to act on that claim without risking their certification and employment.
 *
 * DIRECTIONALITY LOGIC:
 *   Exercise administrators, vendors, compliance officers, and leadership are beneficiaries: they collect certification legitimacy, revenue, or reduced personal liability from the simulation-as-sufficient framing, and under this reading that framing is a covering story for an irreducible gap. Frontline operators are targets: they perform the exercises in good faith, are told they are ready, and bear both the atrophy and the blame if a real event exposes the gap — trapped exit because leaving the profession does not resolve the systemic pattern, and dissenting from inside risks their certification. The public is an even more powerless target: fully exposed, with no visibility into whether the operators they depend on have ever had their judgment tested under real stakes, and no exit at all from the arrangement (you cannot opt out of being a hospital patient or a passenger).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — you cannot ethically manufacture repeated real catastrophes to keep operators sharp — is undeniably still live; simulation is not obsolete machinery kept out of inertia. What this reading contests is not the founding problem's liveness but whether the substitute (simulation) was ever adequate to the full scope of what it was asked to replace. This blocks a simple mandatrophy verdict: the arrangement is not a dead mandate persisting by inertia (a piton), because the coordination function (procedural transfer) is genuinely live and valuable. It is closer to a tangled rope: real coordination value (procedural competence, equipment familiarity, communication protocol transfer) bundled with an asymmetric extraction (compliance legitimacy collected by administrators and vendors, residual risk borne silently by operators and the public) that requires active enforcement (certification gatekeeping, career risk to dissent) to hold together. The classification prevents mislabeling this as pure extraction (which would ignore the real procedural value simulation provides) or as pure coordination (which would ignore that the claim of sufficiency, under this reading, is doing extractive cover-story work for an epistemic gap that cannot be closed by the product being sold).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_lived_catastrophe,
    'Is the categorical claim of this reading — that no simulation fidelity, however high, can exercise judgment-under-irreversible-stakes — empirically correct, or does the hybrid_decay_reading''s separable-components model better fit the evidence (some judgment-relevant competence IS simulation-exercisable, some is not)?',
    'Longitudinal comparison of operator performance under first genuine real-stakes exposure, stratified by simulation fidelity and frequency, across domains (aviation check rides vs actual emergencies, surgical simulation vs first solo high-stakes procedure, nuclear control room drills vs actual excursion events). If performance gaps shrink toward zero as fidelity rises without limit, simulation_sufficiency_reading is better supported; if a persistent floor gap remains regardless of fidelity, this reading is better supported; if only some competence dimensions show a persistent gap, hybrid_decay_reading is better supported.',
    'If the categorical claim is wrong and sufficiently high fidelity does close the gap, this reading''s classification of the standing arrangement as tangled_rope (extractive cover-story riding on real coordination) collapses toward rope (genuine, adequate coordination) — the extraction claim depends entirely on the gap being real and undisclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_lived_catastrophe, conceptual, 'Whether this reading''s categorical simulation-insufficiency claim, versus the sibling readings'' threshold or component-separable claims, is the structurally correct account of the kernel.').

omega_variable(
    covert_atrophy_detectability,
    'Can institutions detect, prior to a real catastrophe, whether a given operator''s judgment-under-stakes has covertly atrophied, or is this reading correct that the atrophy is fundamentally undetectable until activated by real stakes?',
    'Development and validation of proxy measures (physiological stress response under high-fidelity simulation, decision-latency under time pressure, near-miss recovery quality) that correlate with subsequent real-event performance, tested prospectively against actual incident outcomes.',
    'If reliable proxies exist, the ''covert'' character of the atrophy this reading asserts is overstated, weakening the extraction claim (administrators could detect and address the gap but choose the cheaper compliance-theater path instead — which would actually strengthen a snare reading rather than tangled_rope). If no reliable proxy exists, the undetectability is structural and the tangled_rope reading (real coordination bundled with unavoidable, not merely convenient, extraction) is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covert_atrophy_detectability, empirical, 'Whether covert competence atrophy under this reading is detectable pre-catastrophe or only revealed by catastrophe itself.').

omega_variable(
    victim_set_boundary_downstream_public,
    'Does the downstream public genuinely bear this constraint''s cost as an identifiable victim group, or is their exposure too diffuse and probabilistic to count as directional extraction rather than generalized societal risk?',
    'Actuarial and incident-attribution analysis: what fraction of real-catastrophe harm to the public is attributable to the simulation-sufficiency gap specifically, versus other causal factors (equipment failure, staffing shortages, unrelated human error)?',
    'If the gap is a major causal contributor to public harm in catastrophic incidents, the victim classification is well-grounded and the extraction is concentrated and severe; if the gap is a minor contributor relative to other systemic failures, the public''s inclusion as victims may overstate this constraint''s extractiveness relative to other contributing constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_boundary_downstream_public, empirical, 'Whether the downstream public constitutes a genuine, attributable victim group of this specific simulation-sufficiency gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 8, 0.52).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 16, 0.58).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 24, 0.63).
narrative_ontology:measurement(exer_tr_t32, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 32, 0.68).
narrative_ontology:measurement(exer_tr_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 40, 0.71).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(exer_be_t32, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(exer_be_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(exer_su_t32, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(exer_su_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the exercise_as_competence_maintenance kernel. simulation_sufficiency_reading claims high-fidelity simulation genuinely exercises the kernel (low authored ε for the standing certification arrangement, closer to rope). hybrid_decay_reading claims the kernel decomposes into a simulation-exercisable procedural component and a non-simulation-exercisable judgment component (mixed ε, likely tangled_rope but with a narrower victim set limited to the judgment-component gap). This story (lived_catastrophe_necessity_reading) claims a categorical, non-threshold gap that no fidelity closes, producing the widest victim set (all exposed to any operator never tested under real stakes) and the highest authored extractiveness of the three. Each story authors its own ε, beneficiaries, and victims independently per the ε-invariance principle; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
