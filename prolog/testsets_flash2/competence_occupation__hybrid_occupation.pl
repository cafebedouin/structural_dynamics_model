% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Hybrid Competence Occupation Model
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   This constraint describes the prevailing model for competence maintenance
 *   in high-reliability organizations, which mandates a hybrid approach
 *   combining simulations, refreshers, procedural reinforcement, and line
 *   audits. While the core problem of skill decay is real, there is no
 *   consensus on the optimal configuration, leading to an accumulation of
 *   mechanisms. This story instantiates the 'hybrid_occupation' reading of
 *   the 'competence_occupation' kernel, where multiple mechanisms are deemed
 *   necessary, and optimization is an ongoing research problem.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.65).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.7).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.65).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Hybrid Competence Occupation Model").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, '1dce1aa5-c927-4949-988d-2f8576c05f0a').
narrative_ontology:cs_kernel_codification('1dce1aa5-c927-4949-988d-2f8576c05f0a', formalized).
narrative_ontology:cs_authority_grounding('1dce1aa5-c927-4949-988d-2f8576c05f0a', lineage).
narrative_ontology:cs_interpretation_layer_present('1dce1aa5-c927-4949-988d-2f8576c05f0a').
narrative_ontology:cs_reading_relation('1dce1aa5-c927-4949-988d-2f8576c05f0a', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('1dce1aa5-c927-4949-988d-2f8576c05f0a', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_axiom('1dce1aa5-c927-4949-988d-2f8576c05f0a', foundational, multi_mechanism_necessity).
narrative_ontology:cs_axiom_status(multi_mechanism_necessity, holdable).
narrative_ontology:cs_axiom_grounding('1dce1aa5-c927-4949-988d-2f8576c05f0a', multi_mechanism_necessity, empirically_contingent).
narrative_ontology:cs_axiom('1dce1aa5-c927-4949-988d-2f8576c05f0a', foundational, continuous_optimization_problem).
narrative_ontology:cs_axiom_status(continuous_optimization_problem, holdable).
narrative_ontology:cs_axiom_grounding('1dce1aa5-c927-4949-988d-2f8576c05f0a', continuous_optimization_problem, empirically_contingent).
narrative_ontology:cs_reference_frame('1dce1aa5-c927-4949-988d-2f8576c05f0a', post_skill_decay_discovery_era).
narrative_ontology:cs_drift_state('1dce1aa5-c927-4949-988d-2f8576c05f0a', contemporary_training_landscape, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1dce1aa5-c927-4949-988d-2f8576c05f0a', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_providers).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, organizational_budgets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandate continuous competence maintenance through diverse mechanisms, often without specifying optimal configurations, leading to a 'more is better' approach. They benefit from perceived safety and compliance.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Profit from the continuous demand for simulations, refresher courses, and audit services. They have an incentive to promote multi-mechanism approaches, even if their efficacy is not fully proven.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_providers, beneficiary,
    organized, biographical, mobile, regional).

% Bear the direct burden of continuous training, often experiencing fatigue and skepticism about the effectiveness of some methods. Their professional identity is tied to maintaining competence, making exit difficult.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Incur significant costs for implementing and maintaining multiple training and audit mechanisms. They seek cost-effective solutions but are constrained by regulatory mandates and the imperative for safety.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, organizational_budgets, payer,
    powerful, immediate, constrained, national).

% Study the efficacy of different competence maintenance strategies and the optimal configuration of hybrid models. They provide evidence that can influence regulatory policy but do not directly participate in the constraint's operation.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous maintenance of critical operational competence across high-reliability organizations by requiring diverse training and verification methods, aiming to prevent skill decay and ensure readiness for rare, high-consequence events.
% TRANSFER_FUNCTION: Transfers significant financial resources from organizational budgets to training providers and regulatory compliance efforts, and transfers time and cognitive load from frontline operators to continuous training and auditing activities.
% ABSENT_VOICES: Operators who advocate for more targeted, evidence-based training rather than a 'shotgun' approach, and budget holders who would demand clearer ROI on each training mechanism, are often marginalized in favor of broad regulatory compliance and vendor interests.
% DISAPPEARANCE_RATIONALE: If this multi-mechanism requirement vanished, organizations would likely revert to less frequent, cheaper training methods, potentially leading to skill decay and increased risk of incidents. The entire safety training industry would need to reconfigure, and regulatory frameworks would lose a key pillar.
% FOUNDING_PROBLEM: The problem of skill decay in high-consequence, low-frequency events, where operators must maintain readiness for situations they rarely encounter in real-world operations.
% FOUNDING_PROBLEM_CORROBORATION: Safety incident reports, human factors research, and expert testimony from independent safety boards consistently corroborate the ongoing challenge of competence maintenance for rare events. While the specific 'hybrid' solution is debated, the underlying problem is widely acknowledged by all parties, including those who bear the costs.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely addresses a coordination problem (maintaining competence for rare events) but also involves significant asymmetric extraction. Extractiveness is moderate-high (0.65) due to the cumulative cost of multiple, often unoptimized, training mechanisms. Suppression (0.70) is high because regulatory mandates and the imperative for safety leave little room for organizations or operators to opt out or significantly reduce training burdens. Theater ratio (0.40) is moderate, reflecting that while some training is genuinely effective, a portion is maintained for compliance or to satisfy a 'more is better' mentality without clear evidence of marginal utility.
 *
 * PERSPECTIVAL GAP:
 *   Safety regulators and training providers experience this as a necessary, if complex, coordination mechanism, benefiting from its operation. Frontline operators and organizational budgets, however, experience it as an extractive burden, paying the costs in time, effort, and financial resources. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety regulators (agenda_setter) and training providers (beneficiary) have low directionality, as they benefit from the system's operation and expansion. Frontline operators and organizational budgets (payers) have high directionality, as they bear the direct costs and burdens. Frontline operators are 'identity_locked' due to their professional commitment to safety, making exit from the system effectively impossible without leaving the profession.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintaining competence) is still live, but the 'hybrid' approach itself shows signs of mandatrophy in its lack of optimization and accumulation of mechanisms. The classification as a Tangled Rope, rather than a pure Rope, captures this hybrid nature, preventing mislabeling genuine coordination as pure extraction while acknowledging the extractive elements. The rising theater ratio and extractiveness over time suggest a drift towards less efficient, more performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_configuration_ambiguity,
    'What is the optimal configuration and weighting of different competence maintenance mechanisms for specific high-reliability domains?',
    'Longitudinal, comparative studies across organizations and domains, correlating specific hybrid configurations with safety outcomes and skill decay rates.',
    'Resolution could lead to more targeted, efficient training mandates, reducing extractiveness and theater ratio by eliminating less effective mechanisms, potentially reclassifying the constraint closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_configuration_ambiguity, empirical, 'Uncertainty regarding the most effective and efficient mix of training and audit mechanisms.').

omega_variable(
    simulation_sufficiency_vs_hybrid,
    'Is simulation-based training, when designed optimally, sufficient to occupy the competence kernel, or is a hybrid approach fundamentally necessary?',
    'Empirical evidence from advanced simulation environments demonstrating full competence occupation and transfer to real-world performance, without the need for other mechanisms.',
    'If simulation is sufficient, the ''hybrid_occupation'' reading would be foreclosed, and the constraint would shift towards a simpler, potentially less extractive ''simulation_sufficiency'' model. If not, the hybrid model''s necessity is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_sufficiency_vs_hybrid, conceptual, 'Ambiguity over whether simulations alone can fully address competence decay, or if multiple mechanisms are truly indispensable.').

omega_variable(
    real_incident_necessity_challenge,
    'Does the ''real_incident_necessity'' reading (only actual catastrophic incidents provide authentic conditions) pose an insurmountable challenge to the ''hybrid_occupation'' reading''s claims of full competence occupation?',
    'Theoretical and empirical work demonstrating that hybrid mechanisms can effectively replicate the critical cognitive and emotional stressors of real incidents, leading to equivalent competence occupation.',
    'If the ''real_incident_necessity'' claim is validated, the ''hybrid_occupation'' reading''s claim of full competence occupation would be severely undermined, potentially reclassifying it as a Piton (theatrical maintenance) or Snare (false promise of safety).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(real_incident_necessity_challenge, conceptual, 'The challenge posed by the ''real_incident_necessity'' reading to the efficacy of hybrid training.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__hybrid_occupation, theater_ratio, 5, 0.33).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__hybrid_occupation, theater_ratio, 10, 0.36).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__hybrid_occupation, theater_ratio, 15, 0.38).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comp_be_t5, competence_occupation__hybrid_occupation, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(comp_be_t10, competence_occupation__hybrid_occupation, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(comp_be_t15, competence_occupation__hybrid_occupation, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(comp_su_t5, competence_occupation__hybrid_occupation, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(comp_su_t10, competence_occupation__hybrid_occupation, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(comp_su_t15, competence_occupation__hybrid_occupation, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(comp_su_t20, competence_occupation__hybrid_occupation, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
