% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Hybrid Competence Exercise Requirement
 *   domain: safety engineering / organizational learning / high-reliability organizations
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_dependency reading of the
 *   competence_exercise_requirement kernel in safety-critical organizations.
 *   It holds that procedural competence for rare, high-consequence events
 *   cannot be maintained by simulation alone, nor by waiting for
 *   catastrophes, but requires a structured hybrid of simulation-based
 *   repetition and periodic real-world anchoring (line operations,
 *   non-jeopardy audits, actual aircraft time). The constraint is
 *   operationalized through regulatory certification standards,
 *   organizational training departments, and professional licensure rules. As
 *   a rope-class claim, it asserts a genuine coordination function; the
 *   authored metrics are descriptively independent and reflect modest
 *   coordination overhead rather than asymmetric extraction.
 *
 * KEY AGENTS:
 *   - regulatory_authority: Agenda-setter (institutional/analytical) â mandates the hybrid standard and audits compliance.
 *   - safety_critical_operators: Coordinated beneficiary (powerful/constrained) â bears capital and scheduling costs, receives maintained competence and liability reduction.
 *   - certified_practitioners: Coordinated beneficiary (moderate/identity_locked) â supplies labor and attention; professional identity fused with recurrent exercise.
 *   - traveling_public: Diffuse beneficiary (powerless/constrained) â receives safety externalities without direct voice in regime design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.32).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.28).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.32).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Competence Exercise Requirement").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety engineering / organizational learning / high-reliability organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '31bf90f8-666a-4791-a276-aab00eccdce6').
narrative_ontology:cs_kernel_codification('31bf90f8-666a-4791-a276-aab00eccdce6', formalized).
narrative_ontology:cs_authority_grounding('31bf90f8-666a-4791-a276-aab00eccdce6', expertise).
narrative_ontology:cs_interpretation_layer_present('31bf90f8-666a-4791-a276-aab00eccdce6').
narrative_ontology:cs_reading_relation('31bf90f8-666a-4791-a276-aab00eccdce6', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('31bf90f8-666a-4791-a276-aab00eccdce6', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_axiom('31bf90f8-666a-4791-a276-aab00eccdce6', foundational, simulation_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(simulation_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('31bf90f8-666a-4791-a276-aab00eccdce6', simulation_necessary_but_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('31bf90f8-666a-4791-a276-aab00eccdce6', foundational, real_world_anchoring_required).
narrative_ontology:cs_axiom_status(real_world_anchoring_required, holdable).
narrative_ontology:cs_axiom_grounding('31bf90f8-666a-4791-a276-aab00eccdce6', real_world_anchoring_required, empirically_contingent).
narrative_ontology:cs_reference_frame('31bf90f8-666a-4791-a276-aab00eccdce6', structured_exercise_competence_state).
narrative_ontology:cs_drift_state('31bf90f8-666a-4791-a276-aab00eccdce6', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('31bf90f8-666a-4791-a276-aab00eccdce6', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, safety_critical_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, certified_practitioners).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, traveling_public).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, simulation_insufficiency_thesis).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, real_world_anchoring_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates minimum hybrid exercise standards through certification rules and audits compliance via inspection. Does not directly pay exercise costs but invests authority in the hybrid standard; could amend the standard if evidence shifts.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Airlines, utilities, or hospitals that must schedule simulator sessions, line operations, and non-jeopardy audits to maintain certification. Bear direct capital and labor costs but receive maintained operational competence and reduced catastrophic liability in return.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, safety_critical_operators, beneficiary,
    powerful, biographical, constrained, national).

% Pilots, control-room operators, or surgeons who undergo recurrent simulation and supervised real-world performance checks. Experience the regime as recurrent time demand and evaluative stress, but their professional identity and license continuity depend on participation; exit means leaving the profession.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, certified_practitioners, beneficiary,
    moderate, biographical, identity_locked, regional).

% Passengers, patients, or service recipients who depend on operator competence for survival but do not participate in exercise regime design and cannot individually opt out of the transportation or care system.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, traveling_public, beneficiary,
    powerless, immediate, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of maintaining rare-event procedural competence across organizational lifecycles by combining repeatable low-risk simulation with intermittent real-world performance verification, preventing individual operators and organizations from underinvesting in perishable skills.
% TRANSFER_FUNCTION: Moves organizational resources (capital for simulators, aircraft or line time, audit bandwidth) and practitioner cognitive effort into a structured exercise regime, returning distributed collective competence and reduced catastrophic failure probability.
% ABSENT_VOICES: Resource-constrained operators in low-margin jurisdictions who cannot afford full hybrid compliance and would advocate for pure-simulation waivers; pure-simulation technology vendors who argue fidelity has obviated real-world exposure; catastrophe-as-teacher proponents who believe only actual failure provides irreducible exercise and view hybrid regimes as falsely reassuring.
% DISAPPEARANCE_RATIONALE: Without the hybrid requirement, safety-critical organizations would likely retreat to pure simulation (cheaper, more scalable) or reactive post-event training, eroding the context-specific competence that real-world anchoring preserves; incident rates in rare abnormal scenarios would rise as the fragile-equilibrium effect took hold.
% FOUNDING_PROBLEM: Perishable procedural competence in high-consequence, low-frequency operational scenarios: skills decay without exercise, pure simulation cultivates context-specific fragility and negative transfer, and relying on actual catastrophic events for training is ethically unacceptable and organizationally ruinous.
% FOUNDING_PROBLEM_CORROBORATION: Independent safety investigation boards (NTSB, BEA, TSB) and peer-reviewed HRO research attest that competence decay in rare-event procedures is a recurrent accident contributing factor; these sources sit outside the regulated operators and practitioner unions, corroborating that the founding problem persists.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.32, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.32) reflects the real resource cost of maintaining dual-track exercise infrastructure, not rent-seeking. Suppression (0.28) is low because the constraint persists primarily through professional norm internalization and regulatory certification rather than through active suppression of alternatives; pure-simulation and catastrophe-only schools remain speakable. Theater ratio (0.18) is low because real-world anchoring produces observable functional outcomes (checkride performance, audit results) rather than performative compliance. Accessibility collapse (0.42) is moderate: once an organization adopts the safety paradigm, alternatives look negligent, but they remain technically conceivable. Resistance (0.25) is modest, arising mainly from cost-pressure in resource-constrained operators. The measurement series show gradual metric rise as regulatory formalization increased between 1985 and 2025, but the trajectory stays well below extraction-dominant thresholds.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory authority experiences the constraint as a legitimate expertise-based standard that prevents moral hazard; the certified practitioner experiences it as a recurrent evaluative burden tied to professional survival. The engine will compute a symmetric-to-beneficiary directionality for practitioners because they are structurally declared beneficiaries, but their identity-locked exit and the evaluative stress they bear create seat-specific friction that the directionality derivation captures as elevated effective cost despite beneficiary status.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries (operators, practitioners, public) bear some coordination cost, so the structural derivation yields low-to-moderate d for each. The regulatory authority is neither beneficiary nor victim in the base_properties arrays; its d is derived from agenda-setter power and analytical exit, positioning it near the beneficiary end because the constraint validates its authority. No victims are declared because the framework models this as a net-benefit coordination arrangement rather than an extraction structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâperishable rare-event competenceâremains live, corroborated by independent accident investigations. The constraint is not a piton because its function is not atrophied; real-world anchoring continues to produce measurable competence differences in line checks and emergency event performance. It is not a scaffold because no credible empirical path promises to make the hybrid regime obsolete; simulation fidelity improvements have not eliminated the negative-transfer and context-gap problems that real-world anchoring addresses. Mandatrophy_resolved is therefore false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the hybrid_dependency reading a permanent coordination standard or a transitional scaffold awaiting sufficient simulation fidelity?',
    'Longitudinal comparison of rare-event procedural performance across jurisdictions that maintain hybrid minima versus those that have relaxed real-world-hour requirements; also, cognitive-transfer benchmarks from high-fidelity simulation research.',
    'If transitional, the constraint should be reclassified as scaffold and carry a sunset clause tied to fidelity metrics; if permanent, it remains rope with modest coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether hybrid exercise is a permanent or transitional arrangement.').

omega_variable(
    sibling_axiom_override,
    'Does accumulating evidence from high-fidelity simulation studies constitute an axiom-overriding challenge to the hybrid reading''s claim that simulation is insufficient?',
    'Meta-analysis of simulator-to-real-world transfer studies and incident trend data in domains that have relaxed real-world minima.',
    'If the empirical challenge succeeds, the hybrid reading''s foundational axiom shifts from holdable to overridden, and the constraint''s classification would shift toward scaffold or piton depending on institutional persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_axiom_override, empirical, 'Empirical challenge to the insufficiency axiom from simulation fidelity advances.').

omega_variable(
    cost_benefit_asymmetry,
    'Do safety_critical_operators and certified_practitioners experience the hybrid regime as net beneficiaries, or does cost concentration create a latent payer class?',
    'Economic cost-benefit analysis at the operator and individual practitioner level, including insurance premium effects, wage differentials, and career-length safety outcomes.',
    'If costs exceed benefits for the bearing parties, the constraint reclassifies as tangled_rope; if benefits are net, it remains rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_benefit_asymmetry, conceptual, 'Whether declared beneficiaries are genuinely net beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cer_hybrid_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cer_hybrid_tr_t8, competence_exercise_requirement__hybrid_dependency, theater_ratio, 8, 0.1).
narrative_ontology:measurement(cer_hybrid_tr_t16, competence_exercise_requirement__hybrid_dependency, theater_ratio, 16, 0.12).
narrative_ontology:measurement(cer_hybrid_tr_t24, competence_exercise_requirement__hybrid_dependency, theater_ratio, 24, 0.14).
narrative_ontology:measurement(cer_hybrid_tr_t32, competence_exercise_requirement__hybrid_dependency, theater_ratio, 32, 0.16).
narrative_ontology:measurement(cer_hybrid_tr_t40, competence_exercise_requirement__hybrid_dependency, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(cer_hybrid_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cer_hybrid_be_t8, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 8, 0.19).
narrative_ontology:measurement(cer_hybrid_be_t16, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 16, 0.23).
narrative_ontology:measurement(cer_hybrid_be_t24, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 24, 0.27).
narrative_ontology:measurement(cer_hybrid_be_t32, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 32, 0.3).
narrative_ontology:measurement(cer_hybrid_be_t40, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 40, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(cer_hybrid_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(cer_hybrid_su_t8, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 8, 0.22).
narrative_ontology:measurement(cer_hybrid_su_t16, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 16, 0.25).
narrative_ontology:measurement(cer_hybrid_su_t24, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 24, 0.28).
narrative_ontology:measurement(cer_hybrid_su_t32, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 32, 0.3).
narrative_ontology:measurement(cer_hybrid_su_t40, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 40, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, resource_allocation).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, crew_resource_management).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, safety_management_systems).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_exercise_requirement kernel, decomposed per the Îµ-invariance principle because sibling readings instantiate structurally distinct claims with different Îµ values, beneficiary structures, and empirical foundations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
