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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Competence Exercise Requirement — Hybrid Dependency
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The hybrid competence exercise requirement mandates that operators in
 *   high-reliability domains (aviation, nuclear, maritime) maintain
 *   proficiency through a combination of high-fidelity simulation AND
 *   periodic real-world exposure — line operations under observation,
 *   non-jeopardy audits, and minimum actual aircraft/plant time. The
 *   constraint emerged after automation-era accidents revealed that
 *   simulation-alone training produced crews competent in scripted scenarios
 *   but fragile in novel real-world conditions. It is a tangled rope: it
 *   coordinates a genuine safety need (baseline competence across the system)
 *   while extracting asymmetric costs (crews lose recovery time, training
 *   organizations bear infrastructure costs, operators lose revenue capacity)
 *   and requires active enforcement (regulatory audits, certificate actions).
 *   The claim/metric gap is deliberate: the constraint is CLAIMED as
 *   tangled_rope from this reading's seat, while the metrics describe a
 *   coordination function with moderate but rising extractiveness — the
 *   engine measures divergence from other readings' claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.28).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.15).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.28).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Competence Exercise Requirement — Hybrid Dependency").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '09a50676-b4d1-4f33-8f44-6239d44cfdb9').
narrative_ontology:cs_kernel_codification('09a50676-b4d1-4f33-8f44-6239d44cfdb9', formalized).
narrative_ontology:cs_authority_grounding('09a50676-b4d1-4f33-8f44-6239d44cfdb9', lineage).
narrative_ontology:cs_interpretation_layer_present('09a50676-b4d1-4f33-8f44-6239d44cfdb9').
narrative_ontology:cs_reading_relation('09a50676-b4d1-4f33-8f44-6239d44cfdb9', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('09a50676-b4d1-4f33-8f44-6239d44cfdb9', competence_exercise_requirement__catastrophe_as_necessary_anchor, coexists_with).
narrative_ontology:cs_axiom('09a50676-b4d1-4f33-8f44-6239d44cfdb9', foundational, real_world_anchoring_irreducible).
narrative_ontology:cs_axiom_status(real_world_anchoring_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('09a50676-b4d1-4f33-8f44-6239d44cfdb9', real_world_anchoring_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('09a50676-b4d1-4f33-8f44-6239d44cfdb9', foundational, simulation_foundation_necessary).
narrative_ontology:cs_axiom_status(simulation_foundation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('09a50676-b4d1-4f33-8f44-6239d44cfdb9', simulation_foundation_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('09a50676-b4d1-4f33-8f44-6239d44cfdb9', post_automation_accident_reform).
narrative_ontology:cs_drift_state('09a50676-b4d1-4f33-8f44-6239d44cfdb9', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('09a50676-b4d1-4f33-8f44-6239d44cfdb9', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, operating_crews).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, training_organizations).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, operating_crews).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, training_organizations).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, competence_requires_periodic_real_world_anchoring).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, simulation_alone_creates_fragile_equilibrium).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Crews benefit from maintained competence through hybrid simulation/line operations regimes. They bear costs through mandatory line audits, non-jeopardy evaluations, and actual aircraft time requirements that compete with revenue operations and personal recovery time. Exit from the regime means loss of certification; lateral movement to other operators carries the same requirements.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, operating_crews, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, operating_crews, payer).

% Regulators set and enforce the competence exercise standards — defining simulation fidelity requirements, mandating line operation checks, and specifying minimum real aircraft exposure. They collect no direct revenue from the regime but derive legitimacy and mandate fulfillment from demonstrated safety outcomes. Their exit is analytical: they observe and adjust the constraint but are not personally subject to its operational burdens.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Training organizations (simulator manufacturers, airline training departments, third-party providers) benefit from mandated hybrid regimes that sustain demand for both high-fidelity simulation and line-check infrastructure. They bear costs of maintaining simulator fleets, qualifying instructors for line operations, and meeting regulatory audit standards. Their exit is mobile — they can serve different regulatory jurisdictions or pivot to adjacent training markets.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, training_organizations, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, training_organizations, payer).

% Advocates and researchers who argue high-fidelity simulation with structured debriefing constitutes adequate competence exercise. They are excluded from the regulatory standard-setting process where line-operation mandates are codified. Their preferred reading (simulation_as_adequate_exercise) is structurally foreclosed by the hybrid regime's requirement for real-world anchoring. Exit from the exclusion requires shifting the regulatory consensus — effectively trapped in the current framework.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, simulation_purists, excluded,
    moderate, biographical, trapped, national).

% Proponents of the view that only real catastrophic events or near-misses provide irreducible competence exercise. They are excluded because the hybrid regime explicitly substitutes controlled line operations and non-jeopardy audits for catastrophe-dependent learning. Their reading (catastrophe_as_necessary_anchor) coexists as a live critique but cannot be instantiated as policy without abandoning the controlled-anchoring premise. Exit requires a paradigm shift in safety philosophy — trapped in current discourse.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, catastrophe_anchoring_advocates, excluded,
    moderate, biographical, trapped, national).

% Safety science researchers, organizational learning theorists, and accident investigators who study the constraint from outside. They evaluate whether the hybrid regime actually maintains competence or whether simulation decay and line-operation ritualization undermine it. They bear no operational costs and collect no rents; their exit is analytical.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of maintaining operator competence in high-reliability domains where individual operators and organizations under-invest in proficiency without a shared standard — coordination prevents free-riding on others' safety investments and ensures baseline readiness across the system.
% TRANSFER_FUNCTION: Moves training capacity, line-operation slots, and real aircraft time from revenue operations and organizational budgets into mandated competence exercise. Operators and training organizations supply the resources; crews receive the exercise; the safety outcome is the collective return.
% ABSENT_VOICES: Simulation purists (who would argue high-fidelity simulation with debriefing is adequate) and catastrophe-anchoring advocates (who would argue only real events exercise competence) are structurally excluded from the regulatory consensus that codifies hybrid requirements. They would object to the cost and operational disruption of mandated line audits and real-aircraft minimums, but their exclusion is what makes the hybrid regime enforceable.
% DISAPPEARANCE_RATIONALE: If the hybrid competence exercise requirement vanished overnight, operators would regress to simulation-only or reduced-training regimens within 2–3 budget cycles. Competence would degrade unevenly — some organizations maintaining voluntary standards, others cutting to minimum regulatory floors — creating a patchwork of readiness that would manifest in increased incident rates within 5–7 years. The safety regulator's mandate would be materially undermined.
% FOUNDING_PROBLEM: Post-deregulation and post-automation expansion (1980s–2000s), multiple high-profile accidents revealed that simulator-trained crews encountered catastrophic skill degradation when faced with real-world anomalies that simulators could not replicate — sensory cues, physiological stress, organizational pressure, and emergent system interactions. The hybrid regime was built to guarantee periodic real-world anchoring without requiring actual catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by accident investigation boards (NTSB, AAIB, BEA) across multiple jurisdictions — independent of training organizations and regulators. Simulation purists contest the status, arguing modern high-fidelity simulation with physiological stress induction and scenario variability now covers the anchoring gap. Catastrophe-anchoring advocates contest from the other side, arguing controlled line operations lack the irreducible stakes of real events. No single party's self-assertion settles it.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.28) reflects the real resource transfer: crews give up off-duty time and physiological recovery; operators give up revenue flight hours for training; training organizations maintain dual infrastructure. The cost is not trivial but is bounded and reciprocated by safety outcomes. Suppression (0.15) is low — the constraint operates through professional standards and certificate maintenance, not coercion; exit options exist (career change, jurisdiction shift) but are constrained by industry-wide adoption. Theater ratio (0.22) is moderate and rising: line checks increasingly become ritualized box-ticking rather than genuine competence tests, and simulator scenarios drift toward the testable rather than the challenging. Accessibility collapse (0.45) is moderate — alternatives (pure simulation, voluntary proficiency) exist but are structurally discouraged by insurance, regulation, and industry norms. Resistance (0.35) is meaningful: operators negotiate minimums, crews resist schedule intrusions, and training organizations lobby for simulator-only credit.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator's seat, the hybrid regime is a rope with minor extractive overhead — coordination dominates. From the crew's seat, it is a tangled rope — genuine safety coordination experienced as enforced extraction of personal time. From the training organization's seat, it is a resource_allocation coordination mechanism with extractive compliance costs. The engine computes these per-seat types from the structural data; the declared claim (tangled_rope) represents the analytical observer's synthesis.
 *
 * DIRECTIONALITY LOGIC:
 *   Operating crews are dual-positioned: beneficiaries of the safety outcome (d ~ 0.3 toward beneficiary) but payers of the time/physiological cost (d ~ 0.6 toward payer) — net directionality near symmetric. Safety regulators are near-pure agenda setters (d ~ 0.1) — they set standards, bear no operational cost, collect legitimacy. Training organizations are dual: beneficiaries of sustained demand (d ~ 0.25) but payers of dual-infrastructure costs (d ~ 0.55) — net slight payer tilt. Simulation purists and catastrophe advocates are excluded — their directionality is not computed as they hold no structural seat in the enforced regime. The analytical observer is neutral (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (automation-era competence fragility) is contested: simulation purists argue it is substantially solved by modern fidelity; catastrophe advocates argue the hybrid regime substitutes controlled exposure for irreducible stakes. The mandate persists because no faction has enough power to rewrite the standard, and the safety record since adoption is cited by all sides as vindication. This is mandatrophy in the narrow sense — the original justification is contested but the arrangement persists because the cost of resolution exceeds any faction's willingness to bear it. The hybrid structure itself prevents collapse into either pure simulation (which would be a snare on crews) or catastrophe-dependence (which would be a snare on the public).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_threshold,
    'At what fidelity level does simulation become adequate for competence maintenance, making real-world anchoring marginally redundant?',
    'Longitudinal studies comparing incident rates between operators with simulation-only vs. hybrid regimes, controlling for fleet type, operational tempo, and crew demographics. Requires regulatory permission for controlled simulation-only cohorts.',
    'If a threshold exists, the hybrid regime''s real-world component becomes extractive overhead above that threshold — reclassification toward snare for operators above the threshold. If no threshold exists, the hybrid regime remains tangled_rope indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether simulation technology can eventually close the anchoring gap.').

omega_variable(
    line_operation_ritualization,
    'Is the rising theater ratio driven by structural ritualization of line audits (making them performative) or by genuine improvement in baseline competence making deep testing less necessary?',
    'Comparative analysis of line-check findings vs. subsequent incident trajectories; auditor interviews on whether checks probe edges or verify minimums; correlation of theater_ratio trajectory with independent competence metrics (e.g., simulator scenario performance on novel failures).',
    'If ritualization, the constraint is drifting toward piton — coordination function atrophying while enforcement persists. If genuine competence improvement, the theater rise is a false signal and the constraint remains tangled_rope with improving efficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(line_operation_ritualization, empirical, 'Whether increasing performativity signals functional decay or success.').

omega_variable(
    committer_structure_hybrid_dependency,
    'How does this reading''s structural relationship to its sibling readings affect its classification stability?',
    'Track regulatory adoption patterns: if jurisdictions adopting hybrid_dependency subsequently resist simulation-as-adequate proposals, the forecloses relation is structurally real. If simulation-as-adequate gains traction in hybrid jurisdictions, the forecloses claim is overstated and the relation is coexists_with.',
    'If forecloses is real, this reading''s classification is stabilized by its structural dominance in the regulatory frame. If coexists_with, the classification is contestable and may drift as the regulatory consensus shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_hybrid_dependency, conceptual, 'Committee frame: this reading forecloses simulation_as_adequate_exercise within a single regulatory framework, coexists_with catastrophe_as_necessary_anchor, and influences the catastrophe reading by providing a controlled substitute for its irreducible anchor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comp_tr_t4, competence_exercise_requirement__hybrid_dependency, theater_ratio, 4, 0.15).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__hybrid_dependency, theater_ratio, 8, 0.18).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_requirement__hybrid_dependency, theater_ratio, 12, 0.2).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__hybrid_dependency, theater_ratio, 16, 0.21).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__hybrid_dependency, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(comp_be_t4, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(comp_be_t12, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(comp_su_t4, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 4, 0.12).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 8, 0.13).
narrative_ontology:measurement(comp_su_t12, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 12, 0.14).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 16, 0.15).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, resource_allocation).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__hybrid_dependency, 0.18).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, simulation_fidelity_standards).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, line_operation_audit_regime).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, crew_duty_time_limits).

% DUAL FORMULATION NOTE:
% Part of the competence_exercise_requirement constraint family with simulation_as_adequate_exercise and catastrophe_as_necessary_anchor. This reading (hybrid_dependency) is the instantiated regulatory standard; the others are live alternative framings that contest its extraction-coordination boundary. All three share the kernel but instantiate different ε values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_requirement__hybrid_dependency, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
