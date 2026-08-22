% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__simulation_sufficiency_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__simulation_sufficiency_reading
 *   human_readable: Simulated Catastrophe as Competence Maintenance (Simulation-Sufficiency Reading)
 *   domain: safety/organizational_learning
 *
 * SUMMARY:
 *   This constraint embodies one specific reading of a contested kernel: the
 *   claim that simulated catastrophe — conducted at regulatory-mandated
 *   fidelity — constitutes genuine exercise of organizational crisis-response
 *   competence, and that fidelity metrics determine training effectiveness.
 *   Organizations are required by law to conduct periodic drills; regulators
 *   treat documented exercise completion as proof of readiness; simulation
 *   vendors are paid to deliver these exercises. This reading treats the
 *   simulation-fidelity assumption as settled: IF the simulation is
 *   high-fidelity THEN personnel are competent. The sibling readings contest
 *   this: the hybrid-decay reading says simulation exercises procedure but
 *   not judgment-under-stakes (two different kernels); the
 *   lived-catastrophe-necessity reading denies that simulation can substitute
 *   for real-stakes activation. This constraint is the regulatory-sufficiency
 *   reading — what the rule actually says and requires.
 *
 * KEY AGENTS:
 *   - Regulatory agencies: set the mandate, approve vendors, audit compliance via documentation
 *   - Simulation service providers: deliver exercises, incentivized by steady contract stream, benefit from the assumption that fidelity predicts competence
 *   - Organization administration: implements exercises, demonstrates compliance, defines internal readiness as simulation-based
 *   - Personnel: required to participate, bear time cost and train-under-assumption risk
 *   - Personnel inadequately trained by simulation: powerless ex-ante, identified only in catastrophe; experience the real-stakes gap between simulation and reality
 *   - Oversight authorities: investigate actual failures post-hoc; can validate or challenge the simulation-sufficiency assumption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.62).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.48).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Simulated Catastrophe as Competence Maintenance (Simulation-Sufficiency Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety/organizational_learning").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, '4c0f56c2-d171-4527-9abe-ab4c41a3009e').
narrative_ontology:cs_kernel_codification('4c0f56c2-d171-4527-9abe-ab4c41a3009e', distributed).
narrative_ontology:cs_authority_grounding('4c0f56c2-d171-4527-9abe-ab4c41a3009e', extraction).
narrative_ontology:cs_interpretation_layer_present('4c0f56c2-d171-4527-9abe-ab4c41a3009e').
narrative_ontology:cs_reading_relation('4c0f56c2-d171-4527-9abe-ab4c41a3009e', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_reading_relation('4c0f56c2-d171-4527-9abe-ab4c41a3009e', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('4c0f56c2-d171-4527-9abe-ab4c41a3009e', foundational, simulation_fidelity_predicts_competence).
narrative_ontology:cs_axiom_status(simulation_fidelity_predicts_competence, holdable).
narrative_ontology:cs_axiom_grounding('4c0f56c2-d171-4527-9abe-ab4c41a3009e', simulation_fidelity_predicts_competence, empirically_contingent).
narrative_ontology:cs_axiom('4c0f56c2-d171-4527-9abe-ab4c41a3009e', foundational, catastrophe_testing_infeasible).
narrative_ontology:cs_axiom_status(catastrophe_testing_infeasible, holdable).
narrative_ontology:cs_axiom_grounding('4c0f56c2-d171-4527-9abe-ab4c41a3009e', catastrophe_testing_infeasible, conventional).
narrative_ontology:cs_reference_frame('4c0f56c2-d171-4527-9abe-ab4c41a3009e', regulatory_mandate_simulation_as_sufficient).
narrative_ontology:cs_drift_state('4c0f56c2-d171-4527-9abe-ab4c41a3009e', contemporary_post_incident_accumulation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c0f56c2-d171-4527-9abe-ab4c41a3009e', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_agencies).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_service_providers).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, organization_administration).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, personnel_inadequately_trained_by_simulation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, personnel_engaged_in_exercises).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates periodic simulated catastrophe exercises (fire drills, emergency evacuations, crisis response tabletops) as legally sufficient proof of organizational readiness. Sets the simulation standards, approves simulation vendors, and uses compliance documentation to assert that competence is maintained. Benefits from the constraint because it provides verifiable, auditable evidence of preparedness without requiring actual catastrophe.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Sells and operates simulation exercises: tabletop exercises, virtual scenarios, controlled physical drills. Collects revenue from organizations required by regulation to conduct exercises. Benefits from the constraint because regulatory mandates drive a steady stream of exercise contracts. Incentivized to demonstrate that fidelity of simulation predicts real-world performance.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_service_providers, beneficiary,
    organized, biographical, arbitrage, regional).

% Implements and oversees mandated simulations. Benefits by demonstrating regulatory compliance on paper without the operational disruption of real catastrophe and without needing to invest in deeper structural readiness. Selects simulation vendors, runs internal drills, and documents completion. Also sets the organizational standard of what 'competent' means — operationalizing the simulation-sufficiency reading rather than the lived-necessity reading.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, organization_administration, beneficiary,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, organization_administration, agenda_setter).

% Required to participate in mandated simulations during work time. Must treat the simulation as high-fidelity training even when they recognize its limits (compressed timescales, absence of real stakes, no simultaneous system failures, no resource scarcity). Constrained by employment and by regulation; cannot opt out of drills. Pay through time diverted from productive work and through potential inadequate preparation if simulation fidelity does not predict real-world performance.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, personnel_engaged_in_exercises, payer,
    moderate, biographical, constrained, local).

% Personnel who face an actual catastrophe for which simulation fidelity was insufficient. The constraint treats them as adequately trained (because the organization completed its mandated exercise); in reality they are inadequately prepared because the simulation did not capture judgment-under-real-stakes, simultaneous failures, or resource constraints. They bear the consequences of the simulation-sufficiency assumption: injury, death, operational failure. This group is defined by whether the simulation's predictive fidelity fails; ex-ante, they are identified as 'those for whom simulation would be insufficient' — a group that becomes visible only in catastrophe.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, personnel_inadequately_trained_by_simulation, payer,
    powerless, biographical, trapped, local).

% Investigates actual catastrophes to determine whether organizations were adequately prepared. Can audit simulation fidelity post-hoc; can challenge the regulatory assumption that simulation is sufficient by identifying specific skill gaps that the simulation did not exercise. Their analysis either vindicates the simulation-sufficiency reading or becomes evidence for the hybrid/lived-necessity reading.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, oversight_authorities, observer,
    institutional, generational, analytical, national).

% Safety researchers, organizational psychologists, crisis responders who argue that real-stakes judgment, resource constraint adaptation, and second-and-third-order failures can only be truly exercised through actual catastrophe (or extremely high-fidelity simulation indistinguishable from it). They would argue for investment in structural resilience over simulation efficiency. Excluded because the regulatory mandate treats their objection as disproven by successful exercises; their voice enters only through post-hoc investigation of failures.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, lived_catastrophe_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_service_providers).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__simulation_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides organizations and regulators with a verifiable, repeatable mechanism to exercise and certify crisis response competence without incurring the catastrophic risk and cost of actual failure. Solves the problem: how do we test whether personnel can act competently in an emergency without waiting for the emergency to occur?
% TRANSFER_FUNCTION: Transfers compliance burden (time, money, attention) from organizations to simulation providers and from organizations' core mission to regulatory administration. The constraint moves financial resources from organizations to simulation vendors; it also moves temporal and cognitive resources from productive work to drill participation.
% ABSENT_VOICES: Personnel who would argue from experience that simulation fidelity is insufficient; families of those harmed when actual catastrophe revealed simulation gaps; organizations in under-resourced sectors (rural emergency response, small hospitals) that cannot afford high-fidelity simulation and are left with the assumption that low-cost drills are equally sufficient; researchers who study real-world crisis decision-making and see patterns in judgment and resource adaptation that no table-top exercise captures.
% DISAPPEARANCE_RATIONALE: If mandated simulations disappeared, organizations would either collapse into reactive-only posture (wait for catastrophe to learn), or would shift investment toward structural redundancy, real-time monitoring, and just-in-time training triggered by early warning. Actual catastrophes would become the primary competence-testing mechanism, with severe consequences. The regulatory mandate as it stands would vanish; the certification logic would have to be replaced.
% FOUNDING_PROBLEM: In the 1960s–1980s, many organizations had no mechanism to test crisis response outside of actual failure. Early catastrophes revealed staggering gaps in drill readiness, communication, and decision-making. The founding problem: how to prepare for low-probability, high-consequence events without incurring the full cost of those events.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies attest the founding problem is LIVE and that simulation is the solution. Post-incident investigations of major failures (hospital emergencies, industrial crises, military operations) document cases where simulations DID predict and prevent failure, supporting the simulation-sufficiency reading. However, they also document cases where simulation gaps correlated with real-world failures, and peer-reviewed research on crisis decision-making shows that judgment-under-real-stakes differs from judgment-under-simulation in ways that fidelity metrics alone do not capture. The founding problem is answered differently by different seats: regulators say 'simulation solves it,' lived-necessity advocates say 'only catastrophe teaches the full kernel,' hybrid advocates say 'simulation teaches procedure but not judgment.'
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__simulation_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__simulation_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.62 over the interval. Initial extractiveness is moderate because the coordination function (testing without catastrophe) is genuine — organizations do need a way to certify readiness. Extractiveness increases as simulation vendor capture solidifies: regulatory mandates create stable contracts, vendors optimize for fidelity metrics that regulators measure (time-to-decision, communication protocol accuracy) rather than for judgment-under-real-stakes. The constraint plateaus at ~0.62 because the underlying asymmetry is stable: organizations comply, vendors profit, and the victim group (those harmed by simulation gaps) is identified only in catastrophe — they have no voice to press against the constraint until failure. Theater ratio rises sharply from 0.38 to 0.48–0.51, then plateaus: early exercises have real pedagogical intent; over time, they become compliance theater — the same drills repeated because the regulation requires repetition, not because learning is occurring. Suppression is moderate (0.42→0.48) because the constraint is enforced through regulation and contract, not through coercion of individual personnel; the suppression that exists is the suppression of the lived-necessity reading itself — it is regulated out of authorized discourse as 'impossible' (you cannot actually test catastrophe response through actual catastrophe).
 *
 * PERSPECTIVAL GAP:
 *   The regulatory-agency seat and the harmed-personnel seat will compute maximally divergent types. From the agency seat: the constraint is rope (genuine coordination solution; fidelity predicts competence; simulation is the right mechanism). From the harmed-personnel seat: the constraint is snare (simulation is cover for avoiding real-stakes investment; the assumption of sufficiency is enforced to protect vendor revenue and regulatory simplicity; the victim group is silent until catastrophe). The payer-personnel seat is intermediate: they see theater rising over time and recognize that drills become compliance exercises, but they also know some exercises do improve readiness. The engine computes this divergence from the structural data — the authored claim (rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory agencies sit at the beneficiary end (d ≈ 0.15): they set the mandate and use compliance documentation to assert preparation; they do not bear the cost of inadequate simulation. Simulation vendors sit at near-zero d (0.10): they collect revenue from a stable, recurring contract stream with minimal exposure to failure consequences. Organization administration sits near-symmetric (d ≈ 0.45): they benefit from having a certified, documented readiness posture, but they also bear some cost (exercise time, vendor fees) and face reputational risk if an actual catastrophe reveals simulation gaps. Personnel sit at moderate target (d ≈ 0.65): they pay through time and through train-under-assumption risk; they gain workplace safety certification but at the cost of potential inadequate preparation. The victim group — personnel harmed when simulation fidelity is insufficient — sits at maximum target (d ≈ 1.0 ex-post): they experience the full cost of the simulation-sufficiency assumption with no benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested. Regulators claim the problem is LIVE and simulation SOLVES it: we cannot test with real catastrophe, so simulation is the only option. Lived-necessity advocates claim the problem is DEAD: simulation has become so routine (theater_ratio 0.51) that the real learning happens only when the simulation breaks or an actual event occurs. Hybrid advocates claim the problem is MISDEFINED: there are two kernels (procedure, judgment) and simulation addresses one but not the other. The classification resists mandatrophy because the constraint has a genuine coordination function — it DOES provide a repeatable test mechanism — which makes it rope-like. But the constraint is also substantially extractive (0.62) because vendors and regulators benefit from the simplification it enables, and the victim group is suppressed-by-definition until catastrophe. This is a tangled rope: genuine coordination (how-do-we-test) + asymmetric extraction (simulation vendors paid, agency accountability simplified, personnel underprep risk transferred) + active enforcement (regulation mandates exercises, compliance audited, alternative readings excluded from authorized discourse). The mandatrophy resolution: if all actual catastrophes revealed that simulation fidelity WAS sufficient (theater_ratio dropped, success cases accumulated), the rope would be vindicated. If catastrophes revealed systematic gaps correlating with simulation fidelity metrics, the reading would shift toward hybrid or snare. The current state is intermediate: some successes, some failures, enough ambiguity that the regulation persists and the constraint remains rope-coded at the agency seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_as_competence_proxy,
    'Does fidelity of simulation (measured by standard metrics: time-to-decision, communication accuracy, protocol adherence) actually predict competence in real catastrophe?',
    'Post-incident analysis of organizations that experienced actual catastrophe: does high-fidelity-simulation history correlate with better real-world performance, or do gaps appear in judgment, resource adaptation, and simultaneous-failure handling?',
    'If high fidelity predicts real-world competence, the simulation-sufficiency reading is vindicated and extractiveness drops (the coordination genuinely works). If fidelity does not predict competence (judgment gaps, resource constraints, second-order failures appear), the reading shifts toward hybrid (two kernels) or snare (simulation is cover).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_as_competence_proxy, empirical, 'Validity of simulation-fidelity metrics as predictors of real-world crisis competence.').

omega_variable(
    judgment_under_stakes_exerciseability,
    'Can judgment-under-real-stakes (decision-making under genuine uncertainty with real consequences) be exercised and retained through simulation, or is it a qualitatively different competence kernel that requires actual catastrophe or extremely high-fidelity simulation?',
    'Neurocognitive and organizational psychology research on decision-making under real vs. simulated stakes; comparison of judgment trajectories in simulated vs. lived crises; analysis of whether simulation-trained personnel show judgment gaps when the simulated scenario is altered or unexpected factors appear.',
    'If judgment-under-stakes is exercisable through simulation, the rope classification holds. If it requires real-stakes activation, the constraint is hybrid (procedure + judgment are two kernels) or the lived-necessity reading is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judgment_under_stakes_exerciseability, empirical, 'Whether judgment-under-real-stakes is a separable kernel from procedure, or whether it can be exercised through simulation.').

omega_variable(
    victim_set_definition_ambiguity,
    'Are personnel inadequately trained by simulation a pre-existing victim group whose inadequacy can be measured ex-ante, or do they only become identifiable ex-post, after catastrophe reveals the simulation gap?',
    'Develop ex-ante metrics of simulation inadequacy (deviation from real-world decision patterns, judgment errors under pressure, resource-adaptation failures) and test whether these metrics predict real-world performance better than regulation-approved fidelity metrics.',
    'If inadequacy is identifiable ex-ante, the victim group becomes visible to the constraint''s operation and can press for change; the suppression of the lived-necessity reading becomes visible as a design choice rather than inevitable. If inadequacy is only ex-post identifiable, the victim group is suppressed-by-definition and the constraint''s asymmetry is structural (cannot be reformed without the evidence that reform would require).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_definition_ambiguity, empirical, 'Whether inadequate-training is an ex-ante or ex-post defined victim group.').

omega_variable(
    reading_foreclosure_via_regulatory_definition,
    'Is the lived-catastrophe-necessity reading genuinely alternative and coexisting, or has it been structurally foreclosed by defining ''actual catastrophe testing'' as impossible/infeasible?',
    'Analyze whether the regulatory system''s treatment of the lived-necessity reading is epistemic (the reading is empirically false) or administrative (the reading is excluded as operationally impossible, regardless of truth).',
    'If foreclosure is epistemic, the readings coexist and whichever has better evidence wins. If foreclosure is administrative (regulation says ''you cannot test by catastrophe, therefore simulation is the only option''), the constraint includes an hidden suppression of an alternative reading — it is not rope but tangled rope with a coercive element.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_via_regulatory_definition, conceptual, 'Whether the lived-necessity reading is foreclosed or coexisting with the simulation-sufficiency reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(exer_tr_t5, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 5, 0.44).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(exer_tr_t15, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement(exer_tr_t25, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 25, 0.51).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(exer_be_t5, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(exer_be_t15, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(exer_be_t25, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(exer_su_t5, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(exer_su_t15, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(exer_su_t25, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__simulation_sufficiency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__hybrid_decay_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the exercise_as_competence_maintenance kernel. The sibling readings (hybrid_decay_reading, lived_catastrophe_necessity_reading) occupy the same regulatory domain and are triggered by the same founding problem (how to test crisis competence without catastrophe). All three readings share the kernel but disagree about what is being exercised (fidelity of simulation vs. judgment-under-stakes vs. separable components) and what counts as validation. The simulation-sufficiency reading treats the question as empirically settled; the hybrid reading treats it as mis-specified (two kernels); the lived-necessity reading treats it as unanswerable without catastrophe. The three stories are linked by affects_constraints to enable family-level analysis of the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
