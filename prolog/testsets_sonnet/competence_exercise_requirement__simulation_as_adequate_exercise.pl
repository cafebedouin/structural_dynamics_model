% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__simulation_as_adequate_exercise, []).

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
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: High-Fidelity Simulation as Adequate Exercise of Operator Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates one reading of the
 *   competence-exercise-requirement kernel: that high-fidelity simulation
 *   with structured debriefing is a sufficient substitute for real-world
 *   exercise of the competence a safety-critical operator must maintain. The
 *   reading is dominant in commercial aviation recurrent training and nuclear
 *   control-room recertification, where simulator log hours function as the
 *   audited compliance metric. The reading's core evidentiary claim is that
 *   decades without a simulator-trained-crew-attributable catastrophe
 *   validate the substitution. Sibling readings
 *   (catastrophe_as_necessary_anchor, hybrid_dependency) are NOT modeled here
 *   — they are separate constraints with their own ε and stakeholder
 *   structures, linked via network.affects_constraints. This story's ε
 *   reflects only the structural claim that simulation-as-sufficient, as
 *   actually operated (regulator-approved, vendor-supplied,
 *   operator-administered), carries a moderate but rising extractive load: it
 *   externalizes unverified tail-risk onto operators and the public while
 *   concentrating training-cost savings and compliance-metric convenience
 *   among operators, vendors, and regulators.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.38).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.42).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "High-Fidelity Simulation as Adequate Exercise of Operator Competence").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, '56b84a09-34b6-4b7d-9108-d8db27df3104').
narrative_ontology:cs_kernel_codification('56b84a09-34b6-4b7d-9108-d8db27df3104', formalized).
narrative_ontology:cs_authority_grounding('56b84a09-34b6-4b7d-9108-d8db27df3104', expertise).
narrative_ontology:cs_interpretation_layer_present('56b84a09-34b6-4b7d-9108-d8db27df3104').
narrative_ontology:cs_reading_relation('56b84a09-34b6-4b7d-9108-d8db27df3104', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_reading_relation('56b84a09-34b6-4b7d-9108-d8db27df3104', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('56b84a09-34b6-4b7d-9108-d8db27df3104', foundational, engineered_fidelity_substitutes_for_lived_exposure).
narrative_ontology:cs_axiom_status(engineered_fidelity_substitutes_for_lived_exposure, holdable).
narrative_ontology:cs_axiom_grounding('56b84a09-34b6-4b7d-9108-d8db27df3104', engineered_fidelity_substitutes_for_lived_exposure, empirically_contingent).
narrative_ontology:cs_axiom('56b84a09-34b6-4b7d-9108-d8db27df3104', secondary, absence_of_catastrophe_constitutes_positive_validation).
narrative_ontology:cs_axiom_status(absence_of_catastrophe_constitutes_positive_validation, holdable).
narrative_ontology:cs_axiom_grounding('56b84a09-34b6-4b7d-9108-d8db27df3104', absence_of_catastrophe_constitutes_positive_validation, empirically_contingent).
narrative_ontology:cs_reference_frame('56b84a09-34b6-4b7d-9108-d8db27df3104', simulator_certification_as_sufficient_proxy).
narrative_ontology:cs_drift_state('56b84a09-34b6-4b7d-9108-d8db27df3104', post_multidecade_catastrophe_free_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('56b84a09-34b6-4b7d-9108-d8db27df3104', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, simulator_training_vendors).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, nuclear_utility_management).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_certification_bodies).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, passengers_and_public).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, line_check_airmen).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_compliance_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets training curricula around simulator hours because simulators are cheaper, safer, and more schedulable than line operations or real aircraft time. Collects direct cost savings from reduced revenue-flight training time and reduced insurance exposure. Can point to catastrophe-free decades as proof the model works, and lobbies regulators to keep simulator-hour minimums as the compliance bar rather than requiring costlier real-world anchoring.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators, beneficiary).

% Sells full-motion simulator time and certification packages to operators. Revenue scales directly with the claim that simulation is sufficient; has commercial interest in the regulatory kernel staying fixed at 'simulation adequate' rather than drifting toward 'simulation plus mandatory real-world hours,' which would shrink their addressable spend per trainee relative to line-time alternatives.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, simulator_training_vendors, beneficiary,
    organized, biographical, mobile, global).

% Writes the certification standard that recognizes simulator hours as satisfying recurrent competence requirements. Benefits from a auditable, standardized, low-variance compliance metric (simulator log hours) that is far easier to inspect and defend than judgment calls about accumulated real-world exposure. Has institutional incentive to treat the absence of catastrophe as validation of the standard it wrote.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_certification_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_certification_bodies, beneficiary).

% Runs control-room simulator drills as the primary vehicle for operator recertification because it avoids the cost and risk of any real-world equivalent (there is no non-catastrophic real anchor for a reactor excursion). Strongly favors the simulation-adequate reading because the alternative readings have no safe real-world analog to point to.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, nuclear_utility_management, beneficiary,
    institutional, generational, constrained, national).

% Pilots and reactor operators who must demonstrate competence primarily through simulator sessions scored against a checklist. Many report that simulator scenarios, however realistic, do not reproduce the physiological stress, ambiguous cue environment, or organizational politics of a genuine emergency; some carry private doubt about their own readiness that the certification paperwork does not capture. Cannot decline the simulator-based pathway without losing certification and employment; exit from the training regime means exit from the profession.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, frontline_operators, payer,
    moderate, biographical, constrained, national).

% Senior instructors who observe the gap between simulator performance and real-world judgment firsthand but whose professional standing and pay depend on certifying trainees against the simulator-based standard the regulator has set. Their informal observations about simulator-real gaps rarely enter the formal record because there is no channel that credits them over the logged simulator score.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, line_check_airmen, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, line_check_airmen, excluded).

% Bear the tail-risk consequence if simulator-validated competence proves inadequate during a genuine low-frequency, high-consequence event. Have no seat in setting the competence standard and no visibility into whether the catastrophe-free record reflects genuine competence or reflects that the rare events which would test the difference simply have not occurred yet.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, passengers_and_public, excluded,
    powerless, biographical, trapped, national).

% Study accident investigations and near-miss reports to assess whether simulator-trained competence transfers to real events. Their findings are the primary independent check on whether the catastrophe-free record is validation or survivorship bias, but their analyses arrive after the fact and rarely change certification standards prospectively.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, safety_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__simulation_as_adequate_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a scalable, repeatable, auditable way to verify and refresh operator competence across a large workforce without exposing trainees, aircraft, reactors, or the public to the risk of live-fire training on catastrophic scenarios.
% TRANSFER_FUNCTION: Moves training cost and liability risk away from operators (who would otherwise need costlier real-world exposure or accept higher catastrophe risk) and concentrates residual uncertainty about competence transfer onto frontline operators and the public, who cannot independently verify whether simulator performance predicts real performance.
% ABSENT_VOICES: Line check airmen who observe simulator-real gaps have no formal channel; passengers and the public have no representation in setting certification standards; safety researchers studying transfer validity are consulted post-hoc, after accidents, not prospectively in standard-setting.
% DISAPPEARANCE_RATIONALE: If simulator-based certification were no longer accepted as adequate exercise of the competence kernel, operators would need to fund real-world anchoring (line time, non-jeopardy audits, actual equipment hours) or accept unverified competence gaps; training costs would rise sharply, simulator vendor revenue models would need restructuring, and regulatory certification bodies would need an entirely different audit instrument than logged simulator hours.
% FOUNDING_PROBLEM: Live training on catastrophic failure modes (engine-out at altitude, reactor excursion, cascading grid failure) is either impossible or unacceptably dangerous to rehearse for real; some substitute exercise mechanism was needed to build and verify competence without incurring the catastrophe being trained against.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and operators attest the problem is solved: decades without major simulator-trained-crew-caused catastrophe is offered as validation. Independent safety researchers and several accident investigation boards (outside the operator/vendor/regulator beneficiary set) attest the founding problem is only partially solved — post-accident analyses of events like Colgan 3407 and several loss-of-control accidents identified specific competence gaps that simulator training had not exercised, suggesting the catastrophe-free record may reflect the rarity of the triggering conditions rather than validated competence transfer.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).
:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate-low (0.22) and rises to 0.38 over the interval as simulator-based certification becomes the near-universal compliance pathway and alternative anchoring practices atrophy from disuse, concentrating the cost-savings benefit while the unverified-competence-transfer risk accumulates on operators and the public. Theater ratio rises modestly (0.12 to 0.28) reflecting the growing gap between the debriefing-and-scoring ritual and genuine uncertainty about whether scores predict real-event performance. Suppression is moderate (0.42 by interval end): it is not heavily coercive in the classic sense, but the certification pathway forecloses meaningful alternatives for frontline operators — no operator can substitute their own judgment about readiness for the logged simulator score without risking license.
 *
 * PERSPECTIVAL GAP:
 *   From the operator/vendor/regulator seat, this looks like a mature, audited, successful safety system — the catastrophe-free decades ARE the proof. From the frontline-operator and line-check-airman seat, the same system looks like a certification ritual that cannot see its own blind spot: it certifies against the scenarios its designers anticipated, not against the scenario that will actually occur. From the passenger/public seat, the system is invisible until it fails. These are not disagreements about facts; they are structurally different exposures to the same arrangement, and the engine's per-seat computation is expected to diverge accordingly.
 *
 * DIRECTIONALITY LOGIC:
 *   Operators, vendors, and regulators sit near the beneficiary end: they set the standard, administer it, and capture the cost/liability savings or revenue it generates. Frontline operators and line check airmen sit nearer the target end: they must produce the certified performance and bear personal and reputational consequence if the substitution proves inadequate during an actual rare event, without having a voice in whether the substitution is validated. Passengers and the public sit furthest toward the target end with the least agency: trapped exit, no representation, bearing tail-risk they cannot price or avoid.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (verify competence without incurring catastrophe) remains genuinely live — that part of the coordination function is real and durable, which is why this is authored as tangled_rope rather than snare. What keeps the reading from being a clean rope is that the specific instantiation (simulator-hours-as-sufficient, administered by parties with strong commercial and institutional incentive to declare it sufficient) has drifted from 'best available proxy under real constraint' toward 'convenient compliance metric insulated from post-hoc correction,' per the safety-researcher corroboration disagreement in six_questions. The classification prevents mislabeling this as pure extraction (it is not — the coordination function is genuine and the catastrophe-free record is real evidence, not fabricated) while also preventing it from being certified as a costless rope (the beneficiary concentration and the excluded/trapped seats are structurally real).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_free_record_as_validation_or_survivorship,
    'Does the decades-long catastrophe-free record validate that simulation is an adequate substitute for real-world exercise, or does it reflect survivorship bias — the rare triggering conditions simulators are weakest at reproducing simply have not occurred yet?',
    'Systematic post-hoc analysis of near-miss and accident investigations (e.g. loss-of-control events, reactor near-excursions) for evidence of specific competence gaps that simulator curricula did not exercise, compared against base rates of triggering-condition occurrence.',
    'If the record is genuine validation, this reading''s low-to-moderate extraction and tangled_rope classification are well-supported. If it is survivorship bias, effective extraction is substantially understated and the reading functions closer to a snare on the public/passenger seat, with risk concentrated on those least able to detect it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_free_record_as_validation_or_survivorship, empirical, 'Whether the catastrophe-free record is validating evidence or an artifact of rare-event non-occurrence.').

omega_variable(
    kernel_reading_committer_structure,
    'Which of the three declared readings of the competence_exercise_requirement kernel (simulation_as_adequate_exercise, catastrophe_as_necessary_anchor, hybrid_dependency) is operative determines who counts as adequately trained and who counts as a victim of undertraining — the readings disagree specifically about whether real-world anchoring is a necessary structural element or an optional supplement.',
    'This is a committer-frame disagreement, not an empirical one in the first instance: it is located in whether ''adequate exercise'' is defined by outcome record (this reading), by irreducible real-event content (catastrophe_as_necessary_anchor), or by a conjunctive requirement (hybrid_dependency). Resolution would require regulatory bodies to adjudicate which definition governs certification, informed by the empirical omega above.',
    'If the hybrid_dependency reading were adopted as the operative kernel reading, current simulation-only certification regimes would be structurally insufficient and would need real-world anchoring requirements added, changing the beneficiary/victim structure of this constraint entirely. If catastrophe_as_necessary_anchor were adopted, current regimes would be judged as structurally incapable of producing verified competence at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'This story is one reading of a contested kernel; the sibling readings would each change the classification of what counts as adequate exercise and who bears the risk of inadequacy.').

omega_variable(
    regulatory_capture_of_certification_standard,
    'Is the regulatory certification body''s endorsement of simulator-sufficiency an independent safety judgment, or is it substantially shaped by the operators and vendors who benefit from the lower-cost standard and who fund much of the applied research the standard rests on?',
    'Trace funding sources and personnel overlap between simulator research programs, vendor-funded studies cited in rulemaking, and the regulatory bodies that adopt those standards; compare against independently funded transfer-validity research.',
    'If substantially captured, the regulatory_certification_bodies seat should be read closer to a co-beneficiary than a neutral agenda-setter, which would raise the effective extraction attributable to the enforcement mechanism itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_of_certification_standard, empirical, 'Whether regulatory endorsement of the simulation-sufficient standard is independent or captured.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 8, 0.16).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 16, 0.2).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 24, 0.23).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 32, 0.26).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 24, 0.33).
narrative_ontology:measurement(comp_be_t32, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 32, 0.36).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(comp_su_t32, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__simulation_as_adequate_exercise, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__hybrid_dependency).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the competence_exercise_requirement kernel. simulation_as_adequate_exercise (this story) authors a moderate, rising ε concentrated as convenience/cost-savings for operators, vendors, and regulators, with unverified tail-risk externalized to frontline operators and the public. catastrophe_as_necessary_anchor authors a structurally different claim — that simulator-only regimes are categorically inadequate — which would produce a much higher ε and a different victim set (everyone currently certified under simulator-only regimes). hybrid_dependency sits between: it treats simulation as necessary-but-insufficient and would show intermediate ε, since real-world anchoring requirements partially re-distribute the cost-savings currently concentrated in this reading. Each story retains its own ε; they are not to be averaged or reconciled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
