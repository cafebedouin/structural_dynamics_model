% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__simulation_as_proxy, []).

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
 *   constraint_id: competence_exercise_validity__simulation_as_proxy
 *   human_readable: Simulation-as-Sufficient-Proxy for Catastrophe Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story is one reading of the contested kernel 'competence exercise
 *   validity' — the question of what counts as sufficient exercise to
 *   maintain operator competence for catastrophic-risk domains. This reading
 *   holds that simulation-based drills ARE valid, sufficient exercise: they
 *   occupy and maintain the relevant competence, the accumulated safety
 *   record demonstrates adequacy, and regulatory sign-off on drill completion
 *   is a legitimate closure of the readiness question. The sibling readings
 *   (real_catastrophe_only, continuous_refresh_hybrid) are separate
 *   constraints, not alternatives folded into this one — they carry their own
 *   ε and their own beneficiary/victim structure. Under this reading, ε is
 *   authored against the standing simulation-as-sufficient arrangement as its
 *   own proponents would defend it, not against what a stricter reading would
 *   replace it with: the arrangement genuinely coordinates around a real
 *   problem (you cannot rehearse catastrophe), but the metrics show the
 *   coordination has drifted toward a compliance-and-cost-avoidance function
 *   whose extraction lands on operators and the public who bear residual
 *   risk. Over the interval, rising theater_ratio (0.32 to 0.61) tracks a
 *   program that has become progressively more oriented toward audit-artifact
 *   production than toward closing the fidelity gap investigators keep
 *   finding.
 *
 * KEY AGENTS:
 *   - training_program_administrators: agenda_setter, institutional/arbitrage — designs and certifies the proxy standard
 *   - regulatory_compliance_officers: beneficiary, institutional/arbitrage — accepts the proxy as sufficient audit evidence
 *   - operations_leadership: beneficiary, powerful/mobile — cites safety record and drill completion as readiness proof
 *   - frontline_operators: payer, moderate/constrained — bears any real competence gap the proxy leaves open
 *   - public_exposed_to_residual_risk: payer, powerless/trapped — lives with the consequences of unverified readiness
 *   - incident_investigators: observer, institutional/analytical — sees the gap between simulated and real performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.58).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.52).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Simulation-as-Sufficient-Proxy for Catastrophe Competence").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, 'db525a23-236e-4562-a6b3-adc1bb22c75e').
narrative_ontology:cs_kernel_codification('db525a23-236e-4562-a6b3-adc1bb22c75e', distributed).
narrative_ontology:cs_authority_grounding('db525a23-236e-4562-a6b3-adc1bb22c75e', practice).
narrative_ontology:cs_interpretation_layer_present('db525a23-236e-4562-a6b3-adc1bb22c75e').
narrative_ontology:cs_reading_relation('db525a23-236e-4562-a6b3-adc1bb22c75e', competence_exercise_validity__real_catastrophe_only, forecloses).
narrative_ontology:cs_reading_relation('db525a23-236e-4562-a6b3-adc1bb22c75e', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_axiom('db525a23-236e-4562-a6b3-adc1bb22c75e', foundational, simulation_completion_constitutes_competence_evidence).
narrative_ontology:cs_axiom_status(simulation_completion_constitutes_competence_evidence, holdable).
narrative_ontology:cs_axiom_grounding('db525a23-236e-4562-a6b3-adc1bb22c75e', simulation_completion_constitutes_competence_evidence, conventional).
narrative_ontology:cs_axiom('db525a23-236e-4562-a6b3-adc1bb22c75e', secondary, safety_record_absent_incidents_demonstrates_adequacy).
narrative_ontology:cs_axiom_status(safety_record_absent_incidents_demonstrates_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('db525a23-236e-4562-a6b3-adc1bb22c75e', safety_record_absent_incidents_demonstrates_adequacy, empirically_contingent).
narrative_ontology:cs_reference_frame('db525a23-236e-4562-a6b3-adc1bb22c75e', simulation_sufficiency_standard).
narrative_ontology:cs_drift_state('db525a23-236e-4562-a6b3-adc1bb22c75e', post_investigator_gap_findings_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('db525a23-236e-4562-a6b3-adc1bb22c75e', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, training_program_administrators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_officers).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, operations_leadership).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, public_exposed_to_residual_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and certify the simulation curriculum, set drill frequency and pass thresholds, and report completion rates upward as evidence of competence maintenance. Their budget and headcount are justified by the existence and continuity of the simulation program itself, not by any measured reduction in real incident severity.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, training_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Accept simulation completion logs and drill pass rates as the audit artifact satisfying competence-retention requirements. This lets them close inspections and issue certifications without adjudicating whether the drills actually reproduce the cognitive and physiological conditions of real catastrophe.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_officers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_officers, agenda_setter).

% Point to safety record and simulation throughput numbers in board reports and public communications as proof of organizational readiness. Avoid the cost, disruption, and liability exposure that would come from running higher-fidelity exercises or admitting the simulation regime may be inadequate.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, operations_leadership, beneficiary,
    powerful, biographical, mobile, national).

% Complete mandated simulation drills on schedule, which count toward their certification and continued employment, but privately report that the drills do not replicate the sensory overload, time pressure, and irreversible stakes of a real event. If a real catastrophe occurs, they bear the direct consequence of any competence gap the simulation failed to close, and cannot decline the drills without risking their credentials.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Live or work near the facility whose safety case rests partly on the claim that simulation-trained operators are adequately prepared for catastrophic scenarios. Have no visibility into drill fidelity and no channel to contest whether simulation-based certification is a genuine safety guarantee or a paperwork substitute.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, public_exposed_to_residual_risk, payer,
    powerless, generational, trapped, regional).

% Conduct post-incident reviews after real events and compare operator performance against simulation training records. Their findings occasionally surface gaps between simulated and real performance but are typically framed as isolated deviations rather than evidence against the validity of the simulation regime itself.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, incident_investigators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simulation-based training solves a genuine coordination problem: it is impossible, unethical, or prohibitively costly to expose operators to real catastrophic conditions repeatedly for training purposes, so a shared, repeatable, auditable proxy is needed to distribute competence-maintenance obligations across an organization and a regulatory system.
% TRANSFER_FUNCTION: Moves the burden of proving safety from continuous, costly, high-fidelity readiness demonstration onto a standardized, lower-cost simulation artifact; the savings in cost, disruption, and legal exposure accrue to program administrators, compliance officers, and leadership, while the risk of competence shortfall in an actual event is transferred onto frontline operators and the exposed public.
% ABSENT_VOICES: Frontline operators who privately doubt drill fidelity rarely have a formal channel to contest the adequacy of the simulation regime without appearing non-compliant or unfit; the public living near the facility has no seat in certifying whether simulation equals readiness at all.
% DISAPPEARANCE_RATIONALE: If simulation-as-sufficient-proxy were withdrawn as the accepted standard, compliance officers would need a new certification basis, training budgets and schedules would be renegotiated, and operations leadership would lose its current low-cost safety-record narrative — the entire audit and certification apparatus built around drill completion logs would need to be rebuilt around a different evidentiary standard.
% FOUNDING_PROBLEM: Organizations running catastrophic-risk operations (nuclear, aviation, chemical process, emergency response) needed a way to maintain and verify operator competence for events too rare, too dangerous, or too destructive to practice on directly.
% FOUNDING_PROBLEM_CORROBORATION: Training administrators and compliance officers attest the problem is fully addressed by current simulation fidelity and completion metrics. Independent incident investigators and academic human-factors researchers outside the certifying bodies have published post-incident analyses noting operator performance gaps under real conditions that simulation-based certification did not predict — this is corroboration from outside the benefiting parties that the founding problem may remain partly live despite the compliance apparatus treating it as solved.
narrative_ontology:disappearance_verdict(competence_exercise_validity__simulation_as_proxy, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__simulation_as_proxy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__simulation_as_proxy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__simulation_as_proxy, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__simulation_as_proxy, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__simulation_as_proxy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__simulation_as_proxy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a genuine coordination function (simulation solves a real problem: you cannot ethically or practically rehearse actual catastrophe) layered with asymmetric cost distribution — the administrative and compliance apparatus captures the savings from lower-cost, lower-disruption training while operators and the public absorb the risk if drill fidelity proves inadequate. Theater ratio is the most diagnostic metric here and rises steadily across the interval (0.32 to 0.61), tracking a program increasingly oriented around producing auditable completion records rather than closing known fidelity gaps identified by investigators. Suppression (0.52) is moderate: operators are not coerced in an overt sense, but their certification and employment depend on treating the simulation regime as adequate, and dissenting professional judgment about drill fidelity has limited institutional channels. Accessibility collapse (0.48) is only moderate because alternative validation approaches (the sibling readings) remain conceptually available and are actively argued for by investigators and some practitioners — the proxy-as-sufficient view has not fully foreclosed the debate.
 *
 * DIRECTIONALITY LOGIC:
 *   Training administrators and compliance officers sit near the beneficiary end: they set the standard, collect institutional credit for maintaining it, and face minimal personal exposure if the standard proves inadequate in a real event. Operations leadership benefits similarly through cost avoidance and safety-record narratives usable in public and regulatory communication. Frontline operators sit toward the target end: their competence certification depends on compliance with a standard they cannot easily contest, and they bear the direct consequence of any gap between simulated and real performance. The public sits furthest toward the target end with the least power and no exit: they are trapped by geography and have no visibility into drill fidelity at all, yet carry the generational risk if the proxy standard is wrong.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how do you verify competence for events too catastrophic to practice on directly — remains genuinely live; simulation is not a manufactured problem. What has drifted is the standard of sufficiency: the mandate to verify competence has been substituted by the narrower, cheaper mandate to verify drill completion. Classifying this as tangled_rope rather than snare or mountain captures that a real coordination function persists (simulation is not fake) even as the standard's operational meaning has narrowed toward what is auditable rather than what is protective — exactly the substitution the founding_problem_status='contested' and the corroboration split (administrators say solved; investigators say partly open) are meant to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_sufficiency,
    'Does simulation-based training actually reproduce the cognitive, physiological, and decision-making conditions of real catastrophe closely enough that drill performance predicts real-event performance?',
    'Systematic comparison of pre-incident simulation scores against post-incident investigator assessments of operator performance across a large sample of real catastrophic events, controlling for event severity and type.',
    'If fidelity is high, this reading''s claim that simulation is a valid sufficient proxy is empirically supported and the tangled_rope classification may understate the coordination function relative to extraction. If fidelity is systematically low, the coordination story is closer to cover for cost avoidance, pushing the constraint toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_sufficiency, empirical, 'Whether simulation drill performance predicts real-catastrophe performance.').

omega_variable(
    committer_framing_which_reading_governs_certification,
    'Which reading of the competence_exercise_validity kernel actually governs formal certification decisions — is it genuinely the simulation_as_proxy standard, or do regulators informally apply something closer to continuous_refresh_hybrid while only documenting simulation completion?',
    'Audit of actual regulatory decision records versus the stated certification criteria — do certifications ever get withheld or escalated on grounds beyond drill completion (e.g., drill frequency, refresher cadence) that would indicate hybrid reasoning is doing real work under a simulation_as_proxy label?',
    'If regulators are actually applying hybrid reasoning while labeling it simulation-sufficiency, then this story''s beneficiary/victim structure is accurate for the DECLARED standard but understates the real operative standard, which would need its own separate story rather than being folded in here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_which_reading_governs_certification, conceptual, 'Whether the declared reading matches the operative certification practice.').

omega_variable(
    safety_record_as_proof_circularity,
    'Is ''safety record proves adequacy'' (this reading''s core evidentiary claim) circular — does the absence of catastrophic incidents to date reflect genuine competence adequacy, or does it reflect that catastrophic events are rare regardless of training quality, making the safety record uninformative about simulation validity?',
    'Base-rate analysis: compare incident rates at facilities using simulation-only training against any available facilities or historical periods using higher-fidelity or continuous-refresh training, controlling for exposure and event rarity.',
    'If the safety record is largely a base-rate artifact rather than evidence of training adequacy, the reading''s central justification collapses and the extraction (cost/risk shifted onto operators and the public) is no longer offset by a demonstrated safety benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_record_as_proof_circularity, empirical, 'Whether the absence of incidents is evidence of adequacy or an artifact of event rarity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__simulation_as_proxy, theater_ratio, 0, 0.32).
narrative_ontology:measurement(comp_tr_t4, competence_exercise_validity__simulation_as_proxy, theater_ratio, 4, 0.38).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_validity__simulation_as_proxy, theater_ratio, 8, 0.45).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_validity__simulation_as_proxy, theater_ratio, 12, 0.51).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_validity__simulation_as_proxy, theater_ratio, 16, 0.55).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__simulation_as_proxy, theater_ratio, 20, 0.58).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_validity__simulation_as_proxy, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comp_be_t4, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(comp_be_t8, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(comp_be_t12, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(comp_be_t16, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(comp_be_t24, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comp_su_t4, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(comp_su_t8, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(comp_su_t12, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(comp_su_t16, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(comp_su_t24, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__simulation_as_proxy, 0.1).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, real_catastrophe_only).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the competence_exercise_validity kernel, decomposed per the epsilon-invariance principle: measuring competence-exercise sufficiency by simulation-completion metrics versus by real-event/refresh-cycle standards yields structurally distinct extraction profiles and beneficiary/victim sets, so each reading is authored as its own constraint file with its own epsilon. This file (simulation_as_proxy) links to real_catastrophe_only and continuous_refresh_hybrid; each of those files should reciprocally link back here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
