% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: exercise_as_competence_maintenance__simulation_sufficiency_reading
 *   human_readable: Simulation Sufficiency as Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint story models the institutionalized commitment that
 *   high-fidelity catastrophe simulation constitutes genuine exercise of the
 *   safety-competence kernel, encoded in regulatory drill mandates and
 *   simulator-based certification. It is one reading of the contested
 *   exercise_as_competence_maintenance kernel, where the
 *   simulation_sufficiency reading treats performance metrics as dispositive
 *   evidence of readiness. The sibling lived_catastrophe_necessity reading
 *   holds that only real events exercise the kernel, while the hybrid_decay
 *   reading splits the kernel into procedural and judgment components. This
 *   story isolates the simulation_sufficiency reading as a structurally
 *   independent constraint.
 *
 * KEY AGENTS:
 *   - regulatory_compliance_authorities (agenda_setter/institutional): encode the mandate, benefit from liability offload and auditability
 *   - simulation_industry_vendors (beneficiary/organized): capture revenue from fidelity-metric-driven procurement
 *   - operating_organizations (beneficiary/powerful): achieve compliance at lower cost than continuous real-readiness
 *   - frontline_operators (payer/moderate): bear operational risk and identity-lock into simulator-certified competence
 *   - affected_public (payer/powerless): suffer harm when simulation gaps manifest in real catastrophes
 *   - safety_research_dissenters (excluded/moderate): empirically challenge transfer validity but are kept out of standard-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.62).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.5).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Simulation Sufficiency as Competence Maintenance").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'b311c2b4-930d-4163-8a93-f2c24db83761').
narrative_ontology:cs_kernel_codification('b311c2b4-930d-4163-8a93-f2c24db83761', formalized).
narrative_ontology:cs_authority_grounding('b311c2b4-930d-4163-8a93-f2c24db83761', expertise).
narrative_ontology:cs_interpretation_layer_present('b311c2b4-930d-4163-8a93-f2c24db83761').
narrative_ontology:cs_reading_relation('b311c2b4-930d-4163-8a93-f2c24db83761', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('b311c2b4-930d-4163-8a93-f2c24db83761', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('b311c2b4-930d-4163-8a93-f2c24db83761', foundational, simulation_exercise_is_genuine_exercise_of_kernel).
narrative_ontology:cs_axiom_status(simulation_exercise_is_genuine_exercise_of_kernel, holdable).
narrative_ontology:cs_axiom_grounding('b311c2b4-930d-4163-8a93-f2c24db83761', simulation_exercise_is_genuine_exercise_of_kernel, empirically_contingent).
narrative_ontology:cs_axiom('b311c2b4-930d-4163-8a93-f2c24db83761', foundational, simulator_performance_metrics_are_dispositive_for_competence).
narrative_ontology:cs_axiom_status(simulator_performance_metrics_are_dispositive_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('b311c2b4-930d-4163-8a93-f2c24db83761', simulator_performance_metrics_are_dispositive_for_competence, conventional).
narrative_ontology:cs_reference_frame('b311c2b4-930d-4163-8a93-f2c24db83761', simulation_sufficiency_framework).
narrative_ontology:cs_drift_state('b311c2b4-930d-4163-8a93-f2c24db83761', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b311c2b4-930d-4163-8a93-f2c24db83761', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_compliance_authorities).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_industry_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, operating_organizations).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, affected_public).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_operators).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_based_training_sufficiency).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, metric_driven_safety_assurance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets mandatory drill frequencies and simulator fidelity thresholds; treats simulator performance metrics as dispositive evidence of organizational competence; collects compliance data and benefits from clear audit trails and liability offload.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_compliance_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Designs and sells high-fidelity simulation systems; benefits when regulatory mandates require simulator-based competence verification; revenue is tied to procurement cycles driven by fidelity-metric requirements.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_industry_vendors, beneficiary,
    organized, biographical, mobile, global).

% Satisfy safety mandates through scheduled simulation programs rather than maintaining costly continuous real-readiness postures; benefit from reduced compliance costs and predictable training schedules.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, operating_organizations, beneficiary,
    powerful, biographical, constrained, national).

% Must demonstrate competence via simulator performance metrics to maintain certification and employment; professional identity is fused with simulator-validated credentials; bears the immediate operational risk when simulation scenarios fail to match real catastrophe dynamics.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_operators, payer,
    moderate, biographical, identity_locked, national).

% Lives and works near high-risk installations; suffers harm in catastrophic events where responding organizations had been certified as competent based on simulation performance that did not predict real-world failure modes; has no voice in setting fidelity standards.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, affected_public, payer,
    powerless, immediate, trapped, local).

% Conduct empirical research showing poor transfer from simulator performance to real-catastrophe outcomes; findings are excluded from regulatory standard-setting because the mandate treats simulation fidelity as self-evidently sufficient.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_research_dissenters, excluded,
    moderate, generational, constrained, global).

% Evaluates the relationship between simulation fidelity claims and actual catastrophe outcomes; neither benefits from nor pays into the arrangement; tracks whether the regulatory framework responds to empirical challenges.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_systems_analyst, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes safety competence verification repeatable, schedulable, and scalable across organizations by substituting simulator performance for rare and dangerous real-catastrophe exposure.
% TRANSFER_FUNCTION: Moves compliance burden from continuous real-readiness investment to simulator metric achievement; moves safety assurance liability from regulators and operators to the simulation technology; transfers costs of fidelity gaps to frontline operators and the affected public when real catastrophes expose preparation failures.
% ABSENT_VOICES: Safety researchers documenting simulator-to-real-catastrophe transfer failures; frontline operators reporting scenario mismatches; affected communities with no seat at fidelity-standard tables; advocates for lived-experience or hybrid training models.
% DISAPPEARANCE_RATIONALE: If the mandate vanished, operating organizations would need to redesign readiness programs; regulatory bodies would lose their primary audit instrument; the simulation industry's procurement pipeline would contract; safety assurance would reorganize around alternative competence verification, likely increasing real-stakes exposure or operational redundancy costs.
% FOUNDING_PROBLEM: Real catastrophes are too rare and dangerous to serve as routine training exercises; organizations need a repeatable, safe, and measurable method to maintain response competence between actual events.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and operating organizations attest the problem is live and that simulation is the only scalable solution. Safety researchers outside the benefiting parties corroborate the rarity problem but dispute that simulation constitutes genuine exercise of the full competence kernel, citing empirical evidence of judgment failure under real stakes despite high simulator scores.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__simulation_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__simulation_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is moderate-high because the constraint substitutes a controllable, measurable activity for the unpredictable reality it claims to prepare for, externalizing fidelity risk to operators and the public. Suppression (0.50) is moderate: the primary mechanism is institutional exclusion of dissenting research from regulatory frameworks rather than overt coercion. Theater_ratio (0.55) reflects the drift from genuine training to metric-driven compliance theater as the mandate matures. Accessibility_collapse (0.65) captures how alternatives (real-stakes validation, hybrid models) become institutionally invisible once simulation sufficiency is accepted. Resistance (0.40) is moderate: dissent exists but is structurally excluded from standards bodies. The temporal series show extraction and theater rising together as the mandate hardens.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (regulators, vendors, operators) experience the constraint as coordination â scalable, auditable, repeatable safety preparation. The payer seats (frontline operators, affected public) experience the same structure as extraction when simulation fidelity fails to predict real-catastrophe performance. The engine computes this divergence from the structural data; the authored claim does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory compliance authorities sit near the beneficiary end (d low): they set the rules, collect compliance data, and offload liability. Simulation vendors and operating organizations also sit near the beneficiary end. Frontline operators sit nearer the target end (d moderate-high) because their professional identity is locked to simulator metrics and they bear immediate operational risk. The affected public sits at the full-target end (d near 1.0): they have no exit, no voice, and bear the ultimate cost of fidelity gaps. Safety research dissenters are excluded rather than targeted â their exclusion is the enforcement mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by requiring active enforcement (regulatory mandates) and naming both beneficiaries (coordinators) and victims (those harmed by gaps). Without the victim declaration, the story might read as a Rope or Scaffold; without the coordination function, it might read as a Snare. The Tangled Rope classification captures that the coordination (standardized training) and extraction (fidelity-gap externalization) are structurally inseparable in this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_validity,
    'Does high simulator performance predict competent real-catastrophe response, or only performance in simulator scenarios?',
    'Prospective cohort studies tracking simulator scores against real-event outcomes; natural experiments where organizations with differing simulator-to-real-exposure ratios respond to identical incidents.',
    'If transfer is weak, the constraint is more extractive than coordinated, and the victim set expands; if transfer is strong, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_validity, empirical, 'Empirical validity of simulation-to-real transfer').

omega_variable(
    fidelity_gap_prospective_detection,
    'Can inadequacies in simulation fidelity be detected before they cause harm, or only retrospectively through accident investigation?',
    'Independent red-teaming of simulator scenarios against historical catastrophe variability; adversarial testing of scenario libraries.',
    'Prospective detection would enable correction and reduce victim set; retrospective-only detection means the constraint systematically externalizes risk to future accident victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_gap_prospective_detection, empirical, 'Whether fidelity gaps are detectable before harm occurs').

omega_variable(
    committer_kernel_reading_scope,
    'This constraint is the simulation_sufficiency reading of the exercise_as_competence_maintenance kernel; what structural elements change if the lived_catastrophe_necessity or hybrid_decay readings are adopted?',
    'Cross-reading structural comparison via separate constraint stories for each sibling reading.',
    'The victim set, beneficiary set, and enforcement requirements would restructure; this reading''s simulation-industry beneficiaries may become payers under sibling readings, and the affected_public victim set would shift in composition and magnitude.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_scope, conceptual, 'Committer frame: structural delta under sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ex_sim_suff_tr_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ex_sim_suff_tr_t5, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(ex_sim_suff_tr_t10, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(ex_sim_suff_tr_t15, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(ex_sim_suff_tr_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ex_sim_suff_tr_t25, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(ex_sim_suff_tr_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(ex_sim_suff_be_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ex_sim_suff_be_t5, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(ex_sim_suff_be_t10, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(ex_sim_suff_be_t15, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ex_sim_suff_be_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(ex_sim_suff_be_t25, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(ex_sim_suff_be_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ex_sim_suff_su_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ex_sim_suff_su_t5, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(ex_sim_suff_su_t10, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(ex_sim_suff_su_t15, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(ex_sim_suff_su_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(ex_sim_suff_su_t25, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 25, 0.45).
narrative_ontology:measurement(ex_sim_suff_su_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the exercise_as_competence_maintenance constraint family. The kernel decomposes into three structurally distinct claims per the epsilon-invariance principle: simulation_sufficiency (this file), lived_catastrophe_necessity, and hybrid_decay. Each has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
