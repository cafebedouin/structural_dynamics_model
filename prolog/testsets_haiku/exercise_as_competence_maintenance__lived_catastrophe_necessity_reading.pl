% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Lived Catastrophe as Competence Kernel Exercise Requirement
 *   domain: safety/organizational/epistemological
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'exercise_as_competence_maintenance.' The lived-catastrophe-necessity
 *   reading holds that only actual catastrophe exercises the full competence
 *   kernel; simulation is rehearsal but not the thing itself. Under this
 *   reading, competence atrophies covertly in operators trained only through
 *   simulation, and the victim set includes all populations exposed to such
 *   operators. This reading is contested by the
 *   simulation_sufficiency_reading (high-fidelity simulation constitutes
 *   genuine exercise) and the hybrid_decay_reading (simulation exercises
 *   procedural competence but not judgment-under-stakes; the kernel has two
 *   separable components). This story authors ONLY the lived-catastrophe
 *   reading as a clean constraint — it does not describe the contest
 *   internally or average ε across readings.
 *
 * KEY AGENTS:
 *   - populations_exposed_to_untested_operators: powerless, trapped — bear hidden risk if competence atrophies under simulation-only training
 *   - simulation_program_funders: organized, constrained exit — invest in a mechanism the reading frames as insufficient
 *   - operators_under_catastrophe_pressure: moderate power, identity_locked — caught between the reading's demand (real stakes required) and ethical prohibition (cannot deliberately create catastrophe)
 *   - institutional_learning_narratives: non-agent entity; benefits from the reading's sustenance of 'we learned through suffering' discourse
 *   - catastrophe_response_authorities: institutional, agenda-setter — derive authority from catastrophe investigation and standard revision
 *   - simulation_sufficiency_advocates: excluded by the reading's core premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.68).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.71).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Lived Catastrophe as Competence Kernel Exercise Requirement").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety/organizational/epistemological").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921').
narrative_ontology:cs_kernel_codification('ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921', distributed).
narrative_ontology:cs_authority_grounding('ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921', practice).
narrative_ontology:cs_interpretation_layer_present('ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921').
narrative_ontology:cs_reading_relation('ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921', exercise_as_competence_maintenance__simulation_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921', foundational, only_catastrophe_exercises_full_competence_kernel).
narrative_ontology:cs_axiom_status(only_catastrophe_exercises_full_competence_kernel, holdable).
narrative_ontology:cs_axiom_grounding('ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921', only_catastrophe_exercises_full_competence_kernel, empirically_contingent).
narrative_ontology:cs_axiom('ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921', foundational, competence_atrophies_covertly_without_real_stakes).
narrative_ontology:cs_axiom_status(competence_atrophies_covertly_without_real_stakes, holdable).
narrative_ontology:cs_axiom_grounding('ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921', competence_atrophies_covertly_without_real_stakes, empirically_contingent).
narrative_ontology:cs_reference_frame('ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921', competence_certified_through_real_catastrophe_exposure).
narrative_ontology:cs_drift_state('ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921', contemporary_simulation_sufficiency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ec7e7a4a-e17c-4882-8ac8-9d3a03a5a921', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, catastrophe_victims_identified_post_hoc).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, institutional_learning_narratives).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, populations_exposed_to_untested_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_program_funders).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, operators_under_catastrophe_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, operators_under_catastrophe_pressure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents, patients, travelers, workers subject to operators (emergency responders, surgeons, pilots, nuclear plant engineers) whose competence has never been tested under actual stakes. They depend on institutional certification that simulation is sufficient, but carry the risk if it is not. Their situation is invisible until failure occurs.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, populations_exposed_to_untested_operators, payer,
    powerless, immediate, trapped, local).

% Public health agencies, military budgets, aviation authorities invest in simulation and tabletop exercises as the standard competence-maintenance mechanism. Under the lived-catastrophe reading, they are funding rehearsal, not the actual exercise the kernel requires. Their investment fails to prevent the atrophy the reading asserts.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_program_funders, payer,
    organized, biographical, constrained, national).

% Emergency room physicians, flight deck officers, wildfire incident commanders bear the stress of catastrophic real-stakes decision-making. Under the lived-catastrophe reading, their competence cannot be certified without exposure to actual catastrophe, yet they cannot be deliberately exposed to it as 'training.' They are trapped between the reading's demand (real stakes needed) and ethical constraint (cannot deliberately create catastrophe to train).
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, operators_under_catastrophe_pressure, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, operators_under_catastrophe_pressure, beneficiary).

% The lived-catastrophe reading sustains a narrative of institutional learning from disaster: 'We learned because we suffered.' This narrative vindicates post-hoc investigation, memorialization, and policy revision. The constraint's persistence and the reading's empirical claim co-constitute institutional legitimacy after failure.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, institutional_learning_narratives, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, institutional_learning_narratives).

% Academic programs, commercial vendors, military training commands invest in simulation technology and fidelity research. The lived-catastrophe reading creates demand for their services (simulation is framed as necessary but insufficient, so continuous investment in higher-fidelity simulation is justified), but also subjects their core claim (that simulation is sufficient) to contestation.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_research_enterprise, agenda_setter,
    institutional, biographical, mobile, global).

% Public health emergencies, aviation accident investigation boards, nuclear regulators. Under the lived-catastrophe reading, their authority derives partly from the constraint itself: actual catastrophes are the evidence they cite to revise standards, mandate retraining, and justify budget. The constraint sustains their investigatory and standard-setting power.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, catastrophe_response_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Researchers, vendors, and trainers who argue that high-fidelity simulation constitutes genuine exercise of the competence kernel and that catastrophe is an unethical and inefficient way to maintain competence. They are structurally excluded from the lived-catastrophe reading's authorization structure because their core empirical claim contradicts the reading's axiom.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_sufficiency_advocates, excluded,
    organized, biographical, mobile, global).

% Safety engineers, organizational psychologists, and standards bodies that seek middle-ground positions: simulation exercises procedural competence while judgment-under-catastrophic-stakes requires exposure. They observe the constraint without endorsing either pole of the kernel contest, but they feel pressure from the lived-catastrophe reading's absolutism.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, hybrid_framework_mediators, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, catastrophe_response_authorities).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared epistemological standard for what constitutes 'exercised competence' in high-stakes domains (medicine, aviation, nuclear operations, emergency management). The reading coordinates around the claim that only actual catastrophe activates the full kernel of competence, replacing individual operator judgment with a collectively binding standard.
% TRANSFER_FUNCTION: Transfers legitimacy and investigatory power from individual operators and simulation programs to institutional authorities that manage catastrophe response and post-hoc learning. It also transfers risk: populations exposed to operators certified only through simulation carry the hidden cost of untested competence.
% ABSENT_VOICES: Operators who successfully manage catastrophe through simulation-trained competence have no voice in this reading: their success proves the reading false, but the reading structure excludes their testimony as selection bias. Simulation vendors and high-fidelity research communities are structurally excluded by the reading's core premise.
% DISAPPEARANCE_RATIONALE: If this constraint and reading disappeared, certification standards would shift: operators certified through high-fidelity simulation alone would carry full legitimacy. Institutional investment in simulation would accelerate. Catastrophe response systems would lose the narrativized learning authority and would instead credit fidelity-based competence certification.
% FOUNDING_PROBLEM: Early in high-stakes domains (aviation, nuclear), operators trained only on theoretical knowledge and low-stakes practice failed catastrophically when actual high-stakes pressure arrived. The constraint was built to ensure operators are tempered by exposure to real stakes before they face irreversible decisions.
% FOUNDING_PROBLEM_CORROBORATION: Early aviation and nuclear accident investigations (1950s–1970s) documented pilot and engineer training inadequacy under real-emergency pressure. Contemporary sources dispute this: air transport safety statistics show simulation-trained pilot populations perform within certified-equivalent benchmarks (FAA data, ICAO standards analysis); no systematic empirical study establishes that simulation-only operators fail more than catastrophe-exposed operators in aggregate. Simulation_sufficiency_advocates and safety engineering researchers outside catastrophe-response authorities attest the founding problem is resolved by contemporary simulation fidelity. Catastrophe_response_authorities continue to cite the problem as live, but their testimony is self-interested (their investigatory mandate depends on continued catastrophe).
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68) is high because the constraint imposes hidden costs on populations (risk of operator incompetence), transfers investigatory power to institutional authorities, and sustains an epistemological standard (catastrophe as proof) that cannot be falsified by operational success under simulation. Suppression (0.71) is nearly as high because the reading's core claim — competence atrophies without catastrophe — is asserted as self-evident rather than empirically demonstrated; the covert atrophy claim immunizes itself against contradiction (if operators trained through simulation perform well, the reading interprets that as selection bias or as competence not-yet-eroded). Theater_ratio (0.52) is moderately high because simulation exercises are framed simultaneously as 'necessary' (justifying continued investment) and 'insufficient' (preserving the need for catastrophe), creating a performative structure where simulation must be impressive but ultimately vindicated by catastrophe. The measurement trajectory shows extractiveness and suppression both rising steeply in the interval 0–20, then plateauing, suggesting the reading consolidated its interpretive authority during that period (approximately corresponding to the post-2004 institutionalization of disaster-learning frameworks in organizational theory). The shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the catastrophe_response_authorities and institutional_learning_narratives seat, this reading is validating and authority-granting: actual catastrophes become the evidence of truth, and investigation becomes the path to knowledge. From the simulation_program_funders and operators seats, the reading is delegitimizing and impossible to satisfy: simulation is framed as necessary but permanently insufficient, and operators face an unresolvable bind (competence requires catastrophe but catastrophe cannot be deliberately created). The simulation_sufficiency_advocates seat experiences the reading as a falsification framework that reverses burden of proof: they must prove simulation is sufficient in the face of an axiom that only catastrophe suffices. The engine should compute starkly different type-classifications across these seats from the same structural data — the directionality and power atoms produce divergent effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Populations_exposed_to_untested_operators are full targets (d→1.0): powerless, trapped exit, bear invisible risk. Simulation_program_funders are near-targets (d~0.75): their investment is framed as insufficient, so they must perpetually increase spending to offset the reading's claim of covert atrophy. Operators_under_catastrophe_pressure occupy a torn position (d~0.5–0.65 depending on whether identity-lock pushes toward trapped): they benefit from the reading's insistence on high standards (legitimacy of their role) but are trapped by the impossible demand (catastrophe required but prohibited). Catastrophe_response_authorities are beneficiaries (d~0.15): they derive authority from investigating catastrophes and revising standards; the constraint sustains their mandate. Simulation_research_enterprise is near-beneficiary (d~0.2): the reading creates permanent demand for higher-fidelity simulation, justifying continued investment and research careers. The reading itself (the axiom that only catastrophe exercises competence) coalesces institutional power around catastrophe investigation and post-hoc learning narratives.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — operators trained only theoretically fail under real-stakes pressure — was genuine in early aviation and nuclear operations. Contemporary evidence disputes it: simulation-trained populations in air transport show certified-equivalent performance, and no systematic study establishes aggregate competence loss in simulation-trained cohorts. The founding_problem_status 'contested' is correct, but the reading's persistence appears less grounded in active coordination function (which one could argue) and more in the reading's epistemological immunity to falsification: the claim of covert atrophy cannot be empirically refuted (successful simulation-trained operators are explained away as 'not yet eroded' or 'happened to avoid triggering events that would expose the decay'). This is a mandatrophy candidate: the founding problem is dead (contemporary evidence), the arrangement persists (the reading is still cited in safety standards and organizational learning literature), and no party can fix it without abandoning the reading itself. The theater_ratio rise (0.35→0.52) suggests increasing performativity of simulation: exercises are staged to demonstrate 'readiness' while the reading simultaneously asserts readiness cannot be demonstrated except by catastrophe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covert_competence_decay_empirical,
    'Does operator competence in high-stakes domains actually decay without exposure to real catastrophe, or is simulation-maintained competence stable over time?',
    'Longitudinal studies tracking operator performance across operators trained through simulation vs. those with catastrophe exposure, controlling for selection effects and measuring performance metrics independent of catastrophe event participation. Retrospective analysis of operator error rates in actual emergencies, stratified by training modality.',
    'If simulation-trained cohorts show comparable or superior performance, the reading''s core empirical claim is falsified and the constraint''s extractiveness drops precipitously (from 0.68 to ~0.25, becoming genuine coordination). If competence decay is demonstrated, the reading''s authority is vindicated and suppression justifies itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_competence_decay_empirical, empirical, 'Whether the lived-catastrophe reading''s assertion of covert competence atrophy is empirically true.').

omega_variable(
    reading_foreclosure_via_axiom_contradiction,
    'Does the lived-catastrophe reading''s core axiom (only catastrophe exercises competence) logically foreclose the simulation_sufficiency_reading, or do they coexist as incommensurable frameworks?',
    'Philosophical analysis of whether the readings differ only on empirical grounds (when catastrophe-training vs. simulation-training is more effective) or on foundational ontological grounds (what counts as ''exercise'' of a competence kernel). If empirical dispute, they can be settled by evidence; if foundational dispute, they coexist irreducibly.',
    'If foreclosure is structural (one reading logically rules out the other''s premises), the engine should classify the relation as ''forecloses'' and flag the kernel as containing genuinely incommensurable positions. If coexistence is true (different empirical premises but compatible logical structures), the relation is ''coexists_with'' and the contest can be settled by evidence. This affects how institutional standardization can proceed — foreclosure suggests one reading must abandon its core axiom to resolve, while coexistence suggests empirical research can adjudicate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_via_axiom_contradiction, conceptual, 'Whether the lived-catastrophe and simulation-sufficiency readings are in logical contradiction or empirical disagreement.').

omega_variable(
    simulation_fidelity_asymptotic_ceiling,
    'Is there a fidelity ceiling beyond which simulation cannot improve without becoming actual catastrophe? That is, does the simulation_sufficiency reading''s path lead asymptotically toward this reading''s requirement?',
    'Theoretical analysis from simulation engineering and cognitive science: what dimensions of judgment-under-catastrophic-stakes cannot be simulated, and why? Do they correspond to irreducible properties of irreversibility, identity-level stakes, or group-crisis dynamics?',
    'If an asymptotic ceiling exists and corresponds to the lived-catastrophe reading''s axiom, the two readings converge: simulation_sufficiency is false in the limit, and the reading becomes validated not through catastrophe but through simulation''s inherent limitations. If no ceiling exists, simulation can indefinitely improve and approach sufficiency, making the lived-catastrophe reading''s axiom empirically obsolete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_asymptotic_ceiling, conceptual, 'Whether simulation-training''s improvement trajectory asymptotes toward the lived-catastrophe reading''s requirement.').

omega_variable(
    identity_lock_mechanism_under_prohibition,
    'Why are operators_under_catastrophe_pressure identity_locked to this constraint when they could exit (change careers, move to domains without the requirement)?',
    'Qualitative study of operator career psychology: do operators stay because they internalize the reading''s axiom (competence requires catastrophe, so remaining in the domain constitutes a commitment to true competence)? Do they stay because the identity of ''expert operator'' is fused with ''survivor of catastrophe''? Do they stay because career switching costs are prohibitive despite nominal mobility?',
    'If the lock is internalized (the operator believes the reading and sees catastrophe exposure as necessary for authentic competence), the suppression the constraint exercises is substantially internalized — operators perpetuate the constraint''s logic without external enforcement. If the lock is identity-fusion (being a surgeon means having managed a code), then the reading sustains a professional identity structure. If the lock is economic, the reading''s suppression is structural (trapped despite nominal exit). Each mechanism has different implications for how the constraint would change if the reading were abandoned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_under_prohibition, empirical, 'Mechanism of identity-lock for operators under catastrophic decision pressure.').

omega_variable(
    institutional_authority_dependence,
    'Do catastrophe_response_authorities'' investigatory mandate and standard-setting power depend structurally on the lived-catastrophe reading, or would their authority persist if the reading were abandoned?',
    'Institutional history analysis: when catastrophe_response_authorities (accident investigation boards, regulatory bodies) cite the lived-catastrophe reading as justification for their mandate, is the reading necessary to that mandate, or merely rhetorically convenient? Would shifting to simulation-sufficiency invalidate their authority or merely change the source of their findings?',
    'If their authority depends on the reading (catastrophe as the source of truth), then abandoning the reading would dissolve their mandate, and their resistance to simulation_sufficiency is structurally self-interested rather than epistemically grounded. If their authority is independent (investigation and standard-setting are legitimate regardless of the reading''s truth), then the reading is a cover story for institutional power, not its foundation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_authority_dependence, empirical, 'Whether institutional authorities'' power derives from or is merely enhanced by the lived-catastrophe reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(exer_tr_t5, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(exer_tr_t15, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 20, 0.49).
narrative_ontology:measurement(exer_tr_t25, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement(exer_tr_t30, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(exer_tr_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(exer_be_t5, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(exer_be_t15, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(exer_be_t25, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(exer_be_t30, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(exer_be_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(exer_su_t5, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(exer_su_t15, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(exer_su_t25, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(exer_su_t30, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(exer_su_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'exercise_as_competence_maintenance.' The lived-catastrophe-necessity reading asserts that only actual catastrophe exercises the competence kernel and that competence atrophies without real-stakes activation. The simulation_sufficiency_reading contests this by asserting that high-fidelity simulation constitutes genuine exercise; the hybrid_decay_reading mediates by distinguishing procedural competence (exercised by simulation) from judgment-under-stakes (requiring exposure). Each reading instantiates a different constraint with a different ε-value for the same institutional arrangement (operators trained through simulation with varying assumed competence decay). The readings are linked by kernel membership and network edges representing structural influence: the lived-catastrophe reading influences both siblings by establishing catastrophe as an epistemological standard, and coexists with them as live positions in safety engineering discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
