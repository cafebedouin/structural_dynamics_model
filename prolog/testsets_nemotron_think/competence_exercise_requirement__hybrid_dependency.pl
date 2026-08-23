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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear, healthcare) face a
 *   constraint: competence requires both simulation foundation and periodic
 *   real-world anchoring. Regulators mandate this hybrid regime. The
 *   constraint is claimed as a tangled rope: it coordinates safety (genuine
 *   coordination) but extracts disproportionate resources from frontline
 *   operators and small operators (asymmetric extraction). The engine will
 *   compute per-seat classifications from the structural data; the claimed
 *   type and metrics are authored independently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.45).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.6).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.45).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Competence Exercise Requirement").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, 'e9aaddc5-ad36-4c29-910c-1ce5da7f9914').
narrative_ontology:cs_kernel_codification('e9aaddc5-ad36-4c29-910c-1ce5da7f9914', formalized).
narrative_ontology:cs_authority_grounding('e9aaddc5-ad36-4c29-910c-1ce5da7f9914', lineage).
narrative_ontology:cs_interpretation_layer_present('e9aaddc5-ad36-4c29-910c-1ce5da7f9914').
narrative_ontology:cs_reading_relation('e9aaddc5-ad36-4c29-910c-1ce5da7f9914', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('e9aaddc5-ad36-4c29-910c-1ce5da7f9914', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_axiom('e9aaddc5-ad36-4c29-910c-1ce5da7f9914', foundational, competence_requires_hybrid_exercise).
narrative_ontology:cs_axiom_status(competence_requires_hybrid_exercise, holdable).
narrative_ontology:cs_axiom_grounding('e9aaddc5-ad36-4c29-910c-1ce5da7f9914', competence_requires_hybrid_exercise, empirically_contingent).
narrative_ontology:cs_axiom('e9aaddc5-ad36-4c29-910c-1ce5da7f9914', secondary, simulation_foundation_necessary).
narrative_ontology:cs_axiom_status(simulation_foundation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e9aaddc5-ad36-4c29-910c-1ce5da7f9914', simulation_foundation_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('e9aaddc5-ad36-4c29-910c-1ce5da7f9914', hybrid_exercise_regime).
narrative_ontology:cs_drift_state('e9aaddc5-ad36-4c29-910c-1ce5da7f9914', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e9aaddc5-ad36-4c29-910c-1ce5da7f9914', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, public_passengers).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, training_providers).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, small_operators).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, hybrid_exercise_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the requirement for hybrid competence exercise (simulation plus periodic real-world anchoring). They justify it as necessary for safety. They control licensing and oversight, and benefit from institutional authority and budgets tied to the regulatory regime.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regulators, agenda_setter,
    institutional, generational, arbitrage, global).

% Must complete both simulation sessions and real-world line operations/audits. They bear the time, fatigue, and career risk of these exercises. Exit is constrained by licensing requirements and industry norms; leaving the profession is the only full exit.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, frontline_operators, payer,
    moderate, biographical, constrained, global).

% Benefit from the safety assurance that the hybrid regime aims to provide. They have no practical exit from reliance on high-reliability organizations (aviation, nuclear, etc.) and no voice in setting the exercise standards.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, public_passengers, beneficiary,
    powerless, immediate, trapped, global).

% Sell simulation hardware, software, and services mandated by the hybrid requirement. They benefit financially from the simulation foundation component. They can exit by shifting to other markets, but the regulatory mandate creates a captive revenue stream.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, training_providers, beneficiary,
    organized, biographical, mobile, global).

% Bear disproportionate costs of the hybrid regime because they lack economies of scale for simulation and real-world anchoring. They cannot easily exit the market due to capital commitments and regulatory barriers, but they are not the primary targets of the constraint's design.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, small_operators, payer,
    powerless, biographical, constrained, regional).

% Study the effectiveness of hybrid exercise regimes across industries. They do not directly pay or collect from the constraint but produce evidence that influences regulatory evolution.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, safety_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that competence in high-reliability domains is maintained through a combination of simulation-based training (which allows safe practice of rare scenarios) and periodic real-world anchoring (which preserves tacit skills and organizational learning that simulation alone cannot capture).
% TRANSFER_FUNCTION: Moves time, money, and operational risk from frontline operators and small operators to regulators and training providers, in exchange for a safety assurance that benefits the public. The transfer is mediated by regulatory mandate and market for simulation services.
% ABSENT_VOICES: Critics who argue that high-fidelity simulation with advanced debriefing is sufficient (simulation_as_adequate_exercise reading) are structurally excluded from regulatory rulemaking. Also absent are voices from operators who have experienced catastrophic events and argue only those provide true competence (catastrophe_as_necessary_anchor reading).
% DISAPPEARANCE_RATIONALE: If the hybrid requirement vanished, regulators would likely revert to either pure simulation mandates (driven by cost pressure) or pure real-world experience requirements (driven by traditionalist factions). Training providers would lose mandated simulation revenue. Frontline operators would see a shift in training burden. The safety assurance landscape would reorganize around a new equilibrium.
% FOUNDING_PROBLEM: Early high-reliability operations relied solely on apprenticeship and line experience, which proved insufficient for rare catastrophic scenarios. Pure simulation was introduced but found to erode tacit skills and organizational memory over time. The hybrid regime was built to solve the dual problem of rare-event preparedness and skill retention.
% FOUNDING_PROBLEM_CORROBORATION: Regulators and training providers attest the dual problem persists. Independent safety analysts and some operator unions attest that the founding problem is substantially solved by modern simulation, and the hybrid regime now serves as rent extraction. No consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.45) reflects the resource transfer from operators to training providers and regulatory apparatus. Suppression (0.6) reflects active enforcement excluding pure-simulation or pure-experience alternatives. Theater ratio (0.3) indicates some performative compliance (e.g., checklist exercises) but the core function remains substantive. Accessibility collapse (0.5) shows alternatives exist but are structurally discouraged. Resistance (0.4) captures operator pushback on cost and fatigue. Measurements share a single time grid (0-30 years) showing gradual extraction increase as simulation technology expands, theater creep, and stable enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator's seat, the hybrid regime is a necessary coordination mechanism. From the frontline operator's seat, it is an extractive burden with limited exit. The engine computes this divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulators are agenda_setters with arbitrage exit (d near 0.0). Frontline operators and small_operators are payers with constrained exit (d near 1.0). Public_passengers are beneficiaries but trapped (d near 0.5). Training_providers are beneficiaries with mobile exit (d near 0.0). Safety_analysts are observers (d=0.5). The engine derives directionality from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rare-event preparedness + skill retention) is contested: some argue modern simulation solves both, making the real-world component rent-seeking. Others argue real-world anchoring is irreplaceable. The constraint persists because no coalition has enough power to change the regulatory mandate, and the cost of fixing (redesigning training systems) is prohibitive for any single actor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_necessity_empirical_basis,
    'Is the claim that competence requires both simulation and real-world anchoring empirically substantiated, or is it a constructed constraint that benefits training providers and regulators?',
    'Controlled studies comparing safety outcomes under pure simulation, hybrid, and pure real-world regimes; meta-analysis of incident data before/after hybrid mandate adoption.',
    'If empirical support is weak, the constraint may be a false summit (mountain claim with beneficiaries) or a snare; if strong, it remains a genuine tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_necessity_empirical_basis, empirical, 'Whether the hybrid requirement''s coordination function is empirically validated or a cover for extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of pure-simulation alternatives structural (regulatory barriers) or internalized (industry belief that real-world anchoring is morally necessary)?',
    'Post-deregulation observation: if a jurisdiction allows pure simulation and operators voluntarily retain real-world anchoring, suppression is partly internalized.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint persists even without active enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression in the hybrid regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cerhd_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cerhd_tr_t5, competence_exercise_requirement__hybrid_dependency, theater_ratio, 5, 0.22).
narrative_ontology:measurement(cerhd_tr_t10, competence_exercise_requirement__hybrid_dependency, theater_ratio, 10, 0.25).
narrative_ontology:measurement(cerhd_tr_t15, competence_exercise_requirement__hybrid_dependency, theater_ratio, 15, 0.28).
narrative_ontology:measurement(cerhd_tr_t20, competence_exercise_requirement__hybrid_dependency, theater_ratio, 20, 0.3).
narrative_ontology:measurement(cerhd_tr_t25, competence_exercise_requirement__hybrid_dependency, theater_ratio, 25, 0.3).
narrative_ontology:measurement(cerhd_tr_t30, competence_exercise_requirement__hybrid_dependency, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(cerhd_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cerhd_be_t5, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(cerhd_be_t10, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(cerhd_be_t15, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(cerhd_be_t20, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(cerhd_be_t25, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(cerhd_be_t30, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cerhd_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cerhd_su_t5, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(cerhd_su_t10, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(cerhd_su_t15, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(cerhd_su_t20, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(cerhd_su_t25, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(cerhd_su_t30, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__hybrid_dependency, 0.1).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement__simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement__catastrophe_as_necessary_anchor).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the competence_exercise_requirement kernel. The hybrid reading asserts both simulation and real-world anchoring are necessary; the simulation_as_adequate reading asserts simulation suffices; the catastrophe reading asserts only real catastrophic events provide exercise. The three readings form a constraint family linked by mutual structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
