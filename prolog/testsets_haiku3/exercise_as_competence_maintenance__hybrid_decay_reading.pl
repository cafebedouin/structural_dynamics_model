% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__hybrid_decay_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Simulation-Based Competence Maintenance Under Hybrid Decay
 *   domain: organizational/safety
 *
 * SUMMARY:
 *   Organizations use standardized simulation exercises to maintain
 *   operational competence across distributed sites, avoiding the cost and
 *   uncontrollability of real-catastrophe activation. This reading
 *   (hybrid_decay_reading) asserts that simulation effectively maintains
 *   procedural competence (checklists, sequences, muscle memory) but does NOT
 *   exercise judgment competence (real-time improvisation, constraint
 *   recognition, priority-setting under ambiguity). As a result, operational
 *   staff pass simulation metrics while judgment capacity decays unmeasured.
 *   The arrangement benefits training administrators and budget holders
 *   (low-cost compliance), but extracts from operational staff (judgment
 *   decay) and high-consequence-failure bearers (who experience judgment
 *   failures in novel scenarios). This is one reading of the contested kernel
 *   'exercise_as_competence_maintenance'; sibling readings
 *   (simulation_sufficiency_reading and lived_catastrophe_necessity_reading)
 *   propose different competence boundaries and different victim sets.
 *
 * KEY AGENTS:
 *   - training_administrators: institutional agenda-setter, designs the simulation standard (d near 0.0, beneficiary)
 *   - operational_staff: moderate power, constrained exit, bears judgment decay (d near 0.8, target)
 *   - high_consequence_failure_bearers: powerless, trapped, experience novel-scenario failures (d near 1.0, victim)
 *   - budget_holders: institutional beneficiary, cost savings (d near 0.15, beneficiary)
 *   - procedural_compliance_auditors: institutional beneficiary, measurable metrics (d near 0.2, beneficiary)
 *   - lived_catastrophe_advocates: moderate power, excluded from standard-setting (d near 0.65, could be target if included)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.62).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Simulation-Based Competence Maintenance Under Hybrid Decay").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "organizational/safety").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, '1464af45-4224-43db-a457-68b25f2e638a').
narrative_ontology:cs_kernel_codification('1464af45-4224-43db-a457-68b25f2e638a', implicit).
narrative_ontology:cs_authority_grounding('1464af45-4224-43db-a457-68b25f2e638a', extraction).
narrative_ontology:cs_interpretation_layer_present('1464af45-4224-43db-a457-68b25f2e638a').
narrative_ontology:cs_reading_relation('1464af45-4224-43db-a457-68b25f2e638a', exercise_as_competence_maintenance__simulation_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('1464af45-4224-43db-a457-68b25f2e638a', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('1464af45-4224-43db-a457-68b25f2e638a', foundational, competence_bifurcation).
narrative_ontology:cs_axiom_status(competence_bifurcation, holdable).
narrative_ontology:cs_axiom_grounding('1464af45-4224-43db-a457-68b25f2e638a', competence_bifurcation, empirically_contingent).
narrative_ontology:cs_axiom('1464af45-4224-43db-a457-68b25f2e638a', secondary, judgment_decay_invisible_to_audit).
narrative_ontology:cs_axiom_status(judgment_decay_invisible_to_audit, holdable).
narrative_ontology:cs_axiom_grounding('1464af45-4224-43db-a457-68b25f2e638a', judgment_decay_invisible_to_audit, empirically_contingent).
narrative_ontology:cs_reference_frame('1464af45-4224-43db-a457-68b25f2e638a', competence_through_diverse_activation).
narrative_ontology:cs_drift_state('1464af45-4224-43db-a457-68b25f2e638a', simulation_standardization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1464af45-4224-43db-a457-68b25f2e638a', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, training_administrators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, budget_holders).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, procedural_compliance_auditors).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, operational_staff_judgment_decay).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, high_consequence_failure_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, schedule, and mandate simulation exercises as the primary competence-maintenance mechanism. They have authority to deem an organization 'competence-ready' based on simulation pass/fail metrics. They justify the choice of simulation over live-stakes practice by cost, safety, and repeatability. They benefit from a standardized, measurable, low-incident exercise regime that demonstrates compliance to regulators.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, training_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Must develop and maintain both procedural competence (checklists, muscle memory, sequence execution) and judgment competence (improvisation, priority setting, real-time constraint recognition under ambiguity and incomplete information). Simulation exercises the former reliably; judgment atrophies in the absence of high-stakes activation where errors carry actual consequences. Their competence certification depends on simulation scores, but their actual judgment capacity degrades unmeasured.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, operational_staff_judgment_decay, payer,
    moderate, biographical, constrained, national).

% Experience the consequences when operational staff face a real crisis and must improvise beyond their procedural scripts. If judgment capacity has decayed through exclusive simulation practice, failures in novel scenarios fall disproportionately on those harmed — patients in medical crises, populations in environmental emergencies, residents in infrastructure failures. They bear extraction through degraded decision-making they did not consent to and cannot monitor.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, high_consequence_failure_bearers, payer,
    powerless, immediate, trapped, universal).

% Simulation is substantially cheaper than repeated live-stakes exercises (which require controlled real-world conditions, acceptance of actual risk, or expensive near-realistic scenarios). They benefit from demonstrable competence at low operational cost and can report compliance to oversight bodies.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, budget_holders, beneficiary,
    institutional, generational, mobile, national).

% Can objectively measure procedural competence from simulation scores; they have metrics, reproducibility, and clear pass/fail thresholds. Judgment competence is difficult to operationalize and measure; its decay is invisible to audit. They benefit from the shift to measurable simulation because their regulatory authority rests on observable metrics.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, procedural_compliance_auditors, beneficiary,
    institutional, generational, analytical, national).

% Would argue that judgment capacity requires exposure to real-stakes decision-making and that simulation, however high-fidelity, cannot substitute for the cognitive load and affective state of actual consequence. They are excluded from the competence-maintenance standard-setting process because their insistence on live-stakes exercises conflicts with the training administrator's efficiency mandate.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, lived_catastrophe_advocates, excluded,
    moderate, biographical, constrained, national).

% Study whether simulation fidelity correlates with judgment-competence retention and whether the hybrid model (procedures + simulation, judgment + decay) accurately describes real-world competence trajectories. They provide independent evidence on the reading's structural claim.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_fidelity_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, training_administrators).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem of maintaining operational readiness across many sites without continuous real-catastrophe activation (which would be destructive, uncontrollable, and rare). Standardized simulation schedules coordinate training across organizations, enable measurement and compliance demonstration, and reduce the frequency and severity of actual crisis exposure.
% TRANSFER_FUNCTION: Moves risk and decision burden from the training-administration system (which avoids expensive, uncontrolled live-stakes exercises) to operational staff (who face real crises with degraded judgment capacity) and to those harmed by novel-scenario failures (patients, populations, residents). The arrangement transfers the cost of procedural standardization directly to judgment atrophy.
% ABSENT_VOICES: Operational staff who have experienced actual high-stakes crises and would attest to the difference between simulation and real-judgment activation are not reliably included in the competence-standard-setting process. Catastrophe-affected populations would dispute the adequacy of simulation-based training, but they are typically consulted only after failures occur.
% DISAPPEARANCE_RATIONALE: If simulation-based competence maintenance vanished, organizations would need to either accept live-stakes judgment activation (more frequent real crises), invest in expensive high-fidelity near-real scenarios, or revert to judgment-based selection (less measurable). The shift from judgment-as-selection to procedure-as-standard would reverse; the cost structure of training would reorganize around judgment cultivation rather than procedure validation.
% FOUNDING_PROBLEM: Before standardized simulation, competence maintenance relied on rare, uncontrolled catastrophes and informal judgment apprenticeship. This was unpredictable, costly in actual harm, and difficult to audit. Simulation offered a way to exercise procedures repeatedly and measure readiness without waiting for real catastrophes or imposing random crisis frequency on populations.
% FOUNDING_PROBLEM_CORROBORATION: Training administrators and budget holders attest the founding problem is still live and that simulation solves it. Operational staff with lived catastrophe experience attest the founding problem was partially solved (procedure consistency improved) but a new problem was created (judgment decay). Independent research on simulation fidelity and judgment retention provides corroboration from outside the benefiting parties; findings are split between high-fidelity studies (simulation can preserve judgment if stress/consequence realism is engineered) and field studies (standard simulation protocols show judgment decay in novel-scenario performance).
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.62 over the interval as organizations mature the simulation regime and judgment decay becomes structural — the procedure-based approach is locked in, judgment expectations fade, and the gap between certified competence and actual judgment capacity widens. Theater ratio rises from 0.35 to 0.48, indicating a growing share of the exercise machinery is devoted to demonstrating procedural compliance rather than cultivating judgment. Suppression requirement is moderate (0.52–0.59) because the arrangement is enforced through measurement and audit (operational staff are required to pass simulation), not through direct coercion, but alternatives (live-stakes judgment exercises, expensive near-real scenarios) are effectively suppressed by cost and regulatory preference. The measurement series tracks the constraint's accumulation over time as organizations standardize around the hybrid model and the judgment component fades from the competence frame.
 *
 * PERSPECTIVAL GAP:
 *   Training administrators compute the constraint as rope (genuine coordination for readiness at low cost, measurement/compliance working as intended); operational staff compute it as snare (judgment decay is an unseen extraction masked by simulation compliance). The engine computes per-seat types from structural data: the administrator seat sees high beneficiary directionality (d near 0.0) and moderate extraction; the staff seat sees high target directionality (d near 0.8) and extraction concentrated in the unmeasured judgment component. The claimed_type is tangled_rope because both elements are present: real coordination function (avoiding catastrophe frequency) AND asymmetric extraction (judgment decay concentrated on operationals). The metrics reflect the staff/failure-bearer reading; the administrator reading would author lower extractiveness and higher accessibility_collapse (alternatives seem viable to them).
 *
 * DIRECTIONALITY LOGIC:
 *   Training administrators and budget holders occupy beneficiary seats (d near 0.0–0.2): they collect from the arrangement through cost savings and measurable compliance without bearing judgment decay directly. Operational staff occupy high-target seats (d near 0.8): they must pass simulation metrics, their judgment capacity decays unmeasured, and they bear the costs when novel scenarios require improvisation. High-consequence-failure bearers occupy full-target seats (d near 1.0): they experience extraction without any say in the competence standard, trapped in the consequence space. The gap between administrator and operational-staff seats is what drives the tangled_rope classification: genuine coordination (everyone benefits from avoiding random real-catastrophe activation), but asymmetric extraction (the cost of procedure standardization is shifted to judgment decay, which affects staff and failure-bearers disproportionately).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncontrolled, costly competence maintenance through real-catastrophe exposure) is genuinely solved by simulation-based scheduling. But the founding problem was only PARTIALLY the real issue — the other part was how to measure and audit competence reliably. Simulation solved both, but by choosing procedure-measurability over judgment-cultivation. Mandatrophy is present: the arrangement was built to solve 'maintain readiness without catastrophe frequency,' which it does for procedural readiness. But judgment readiness, which was always part of competence, has become invisible to the auditing system. A competent operator now means 'passes simulation,' not 'makes sound judgment under stakes.' The founding_problem_status is contested because training administrators attest the problem remains (judgment is still exercised, implicitly, through simulation fidelity), while operational staff and lived-catastrophe researchers attest the founding problem was solved incompletely — only the measurable part was addressed, and the unmeasured part decayed. This is the core mandatrophy signal: the arrangement works for its stated purpose (procedure consistency, cost control) but has atrophied from its original human purpose (operational judgment at all times).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedure_judgment_separation,
    'Are procedural competence and judgment competence structurally separable, or does mastering procedures (under simulation) implicitly maintain judgment capacity?',
    'Post-simulation stress tests with novel scenarios (not trained on): compare judgment quality on novel problems between high-simulation-score staff and staff with equivalent live-experience exposure. If simulation-trained staff show judgment decay on novel problems, the separation is real.',
    'If separable, the victim set includes operational staff (judgment decay) and high-consequence-failure bearers (novel-scenario harms), and the arrangement is tangled_rope with substantial extraction. If inseparable (judgment is implicit in procedure mastery), the arrangement is rope (genuine coordination with minimal asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedure_judgment_separation, empirical, 'Whether judgment competence decays independently of procedural competence under simulation-only regimes.').

omega_variable(
    simulation_fidelity_threshold,
    'Is there a simulation-fidelity threshold beyond which judgment capacity IS retained (e.g., full consequence simulation, stress induction, incomplete information)? Or is judgment decay inherent to any simulation setup?',
    'Comparative study of simulation protocols with varying fidelity (low-fidelity checklists vs. high-fidelity scenario with consequences) and their correlation with post-deployment judgment retention.',
    'If a threshold exists and can be engineered, the arrangement could remain tangled_rope but with lower extractiveness (judgment decay becomes avoidable rather than structural). If judgment decay is inherent to simulation (real stakes cannot be simulated), the arrangement approaches snare for judgment competence (extraction is unavoidable and hidden).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether high-enough-fidelity simulation can exercise judgment competence or whether judgment fundamentally requires real-stakes exposure.').

omega_variable(
    reading_bifurcation_contest,
    'Is the competence kernel a single unified capacity (as simulation_sufficiency_reading assumes) or two separable components — procedures and judgment (as hybrid_decay_reading asserts) — or is judgment completely inseparable from catastrophic activation (as lived_catastrophe_necessity_reading claims)?',
    'The three sibling readings of this kernel differ in their competence-kernel definition. This reading asserts bifurcation. Direct resolution would require a theory of competence that all parties accept, which does not exist. Indirect resolution: which reading''s predicted failure mode matches observed real-world performance gaps?',
    'If the hybrid bifurcation is correct, judgment decay is a hidden extraction mechanism (this reading, victim set includes operational staff and failure-bearers). If simulation_sufficiency is correct, high-fidelity simulation is sufficient and extractiveness is lower (beneficiary set expands). If lived_catastrophe_necessity is correct, the victim set is operational staff (who are certified but unprepared) and catastrophe-affected populations (who bear the cost of training by catastrophe).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_bifurcation_contest, conceptual, 'Committer-frame uncertainty: which reading''s definition of the competence kernel is structurally correct?').

omega_variable(
    measurement_frame_capture,
    'Does the shift from judgment-based selection (before simulation) to procedure-based measurement (after simulation) constitute a legitimate efficiency gain, or a measurement-driven capture of what competence means?',
    'Audit record comparison: in organizations where both judgment-based and simulation-based certifications coexist, do they identify the same competent staff, or does the measurement system identify different people as competent?',
    'If the frames identify different people, measurement capture has occurred: the constraint''s primary beneficiary (audit administrators) has redefined competence to match what their measurement system detects, extracting from those (operational staff, failure-bearers) whose actual competence is unmeasured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_frame_capture, empirical, 'Whether the measurement frame shift is a discovery of efficiency or a capture of the competence definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(exer_tr_t5, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(exer_tr_t15, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(exer_tr_t25, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(exer_be_t5, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(exer_be_t15, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(exer_be_t25, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(exer_su_t5, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(exer_su_t15, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(exer_su_t25, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__hybrid_decay_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% The constraint family 'exercise_as_competence_maintenance' decomposes into three distinct readings of a contested kernel: (1) hybrid_decay_reading (THIS story): simulation exercises procedures, judgment decays separately, bifurcation of competence components; (2) simulation_sufficiency_reading: high-fidelity simulation is sufficient for full competence retention, no bifurcation; (3) lived_catastrophe_necessity_reading: judgment requires real-stakes activation, simulation is rehearsal, competence is monolithic and catastrophe-dependent. Each reading has different ε (extractiveness), different victim sets, and different dominant constraint types. They are linked through the kernel 'exercise_as_competence_maintenance' (the standing arrangement — how competence is maintained through exercise). The three readings diverge on what the competence kernel consists of (procedures only? procedures + judgment? judgment fundamentally tied to real stakes?) and what decay looks like. This story instantiates the hybrid_decay reading: asserting bifurcation between procedural and judgment competence, with judgment decay invisible to the simulation measurement frame. Each reading is a complete constraint story with independent ε, victim sets, and stakeholder roles; they coexist as live positions held by different institutional actors (training administrators favor simulation_sufficiency; operational staff and catastrophe researchers argue for hybrid_decay or lived_catastrophe necessity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
