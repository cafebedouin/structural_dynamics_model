% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__simulation_sufficiency, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation-Based Competence Sufficiency
 *   domain: high_reliability_organizations/safety_training/competence_maintenance
 *
 * SUMMARY:
 *   This constraint asserts that simulation-based drills are sufficient for
 *   maintaining operational competence in high-reliability organizations. It
 *   is a specific reading of the broader 'competence_occupation' kernel,
 *   which is contested. This reading emphasizes measurable training
 *   compliance and the scalability of simulation, leading to a significant
 *   industry built around providing these solutions. While it offers a
 *   coordination function (standardized training, reduced risk of real-world
 *   incidents), it also extracts resources and attention from alternative
 *   competence maintenance strategies, potentially creating a false sense of
 *   security.
 *
 * KEY AGENTS:
 *   - simulation_industry: Primary beneficiary (institutional/arbitrage) — profits from the mandate for simulation.
 *   - training_departments: Agenda setter/beneficiary (organized/constrained) — administers compliance, benefits from clear metrics.
 *   - frontline_operators: Payer/victim (moderate/identity_locked) — bears the risk of potential competence gaps, constrained by training mandates.
 *   - organizational_safety_culture: Victim (institutional/constrained) — suffers from potential over-reliance on simulation.
 *   - regulators: Agenda setter/observer (institutional/analytical) — mandates training, observes outcomes, can alter the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.6).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.7).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.6).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation-Based Competence Sufficiency").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "high_reliability_organizations/safety_training/competence_maintenance").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '0b41c583-52b8-492f-a55a-1ec0ec5e9ef7').
narrative_ontology:cs_kernel_codification('0b41c583-52b8-492f-a55a-1ec0ec5e9ef7', formalized).
narrative_ontology:cs_authority_grounding('0b41c583-52b8-492f-a55a-1ec0ec5e9ef7', expertise).
narrative_ontology:cs_interpretation_layer_present('0b41c583-52b8-492f-a55a-1ec0ec5e9ef7').
narrative_ontology:cs_reading_relation('0b41c583-52b8-492f-a55a-1ec0ec5e9ef7', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_reading_relation('0b41c583-52b8-492f-a55a-1ec0ec5e9ef7', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('0b41c583-52b8-492f-a55a-1ec0ec5e9ef7', foundational, simulation_fidelity_is_sufficient).
narrative_ontology:cs_axiom_status(simulation_fidelity_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('0b41c583-52b8-492f-a55a-1ec0ec5e9ef7', simulation_fidelity_is_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('0b41c583-52b8-492f-a55a-1ec0ec5e9ef7', secondary, measurable_compliance_equals_competence).
narrative_ontology:cs_axiom_status(measurable_compliance_equals_competence, holdable).
narrative_ontology:cs_axiom_grounding('0b41c583-52b8-492f-a55a-1ec0ec5e9ef7', measurable_compliance_equals_competence, conventional).
narrative_ontology:cs_reference_frame('0b41c583-52b8-492f-a55a-1ec0ec5e9ef7', simulation_driven_competence_assurance).
narrative_ontology:cs_drift_state('0b41c583-52b8-492f-a55a-1ec0ec5e9ef7', contemporary_operational_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0b41c583-52b8-492f-a55a-1ec0ec5e9ef7', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, training_departments).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, organizational_safety_culture).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__simulation_sufficiency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__simulation_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates (standardized training, perceived risk reduction) but also extracts (resources flow to the simulation industry, potential for competence gaps for operators). Extractiveness is high (0.6) due to the significant investment in simulation and the potential for misallocated resources if sufficiency is overstated. Suppression (0.7) is high because alternative views on competence occupation are often marginalized in favor of auditable training metrics. Theater ratio (0.4) is moderate, reflecting that while simulations have real value, a portion of their function is performative compliance. The increasing trend in extractiveness and suppression over time reflects the growing institutionalization and commercialization of this approach.
 *
 * PERSPECTIVAL GAP:
 *   The simulation industry and training departments experience this as a beneficial coordination mechanism, providing clear metrics and a scalable solution. Frontline operators and the broader safety culture, however, may experience it as an extractive force, diverting resources from more effective, but less measurable, competence maintenance strategies, and potentially leaving them underprepared for novel real-world challenges. Regulators sit in an analytical seat, evaluating the effectiveness and potential for regulatory capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The simulation industry is a clear beneficiary (d=0.0-0.1) as the constraint directly drives demand for their products. Training departments also benefit (d=0.1-0.2) from having a clear, auditable framework for competence. Frontline operators are targets (d=0.7-0.8) as they must comply with training, potentially at the expense of other forms of skill development, and bear the ultimate risk of competence gaps. Organizational safety culture is also a target (d=0.8-0.9) if the focus on simulation leads to a diluted understanding of true operational readiness. Regulators are analytical (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (overstating its coordination function) or a pure Snare (ignoring its genuine, albeit potentially overstated, coordination benefits). The 'contested' status of the founding problem (is fragmented/unsafe training still the primary problem, or has the solution itself become a problem?) highlights the potential for mandatrophy, where the initial coordination function is overshadowed by extraction. The omegas address the core ambiguities of whether the claimed sufficiency is empirically valid and whether the constraint is a genuine solution or a preference-driven reading of the competence kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_vs_realism,
    'Does the fidelity of simulation-based drills genuinely occupy the competence kernel as effectively as real-world experience, or is there a critical gap?',
    'Longitudinal studies comparing performance outcomes of simulation-trained vs. real-incident-experienced operators, particularly under novel or high-stress conditions not easily simulated.',
    'If a critical gap exists, the constraint is more extractive (false sense of security, misallocated resources) and less coordinative than claimed, shifting it towards a Snare. If fidelity is sufficient, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_vs_realism, empirical, 'Assesses the empirical claim of simulation sufficiency against actual competence occupation.').

omega_variable(
    kernel_reading_simulation_sufficiency,
    'Is this constraint a genuine reflection of competence maintenance, or a reading of the ''competence_occupation'' kernel that prioritizes measurable training compliance and benefits the simulation industry?',
    'Analysis of resource allocation: if investment in simulation significantly outweighs investment in other competence maintenance mechanisms (e.g., real-world operational experience, continuous procedural reinforcement), it suggests a preference-driven reading.',
    'If it''s a preference-driven reading, the constraint''s ''naturalness'' is reduced, and its extractive component (benefiting the simulation industry) is amplified, pushing it closer to a Snare. This reading (simulation_sufficiency) forecloses the ''real_incident_necessity'' reading within a single operational framework, as one asserts sufficiency and the other necessity of real events.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_simulation_sufficiency, conceptual, 'This constraint is the ''simulation_sufficiency'' reading of the ''competence_occupation'' kernel. It treats simulation as sufficient, contrasting with ''real_incident_necessity'' (which it forecloses) and ''hybrid_occupation'' (with which it coexists but influences by diverting resources).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__simulation_sufficiency, theater_ratio, 5, 0.28).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__simulation_sufficiency, theater_ratio, 10, 0.35).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__simulation_sufficiency, theater_ratio, 15, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(comp_be_t5, competence_occupation__simulation_sufficiency, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(comp_be_t10, competence_occupation__simulation_sufficiency, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(comp_be_t15, competence_occupation__simulation_sufficiency, base_extractiveness, 15, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(comp_su_t5, competence_occupation__simulation_sufficiency, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(comp_su_t10, competence_occupation__simulation_sufficiency, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(comp_su_t15, competence_occupation__simulation_sufficiency, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, safety_compliance_auditing).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'competence_occupation' kernel. Its structural properties and beneficiaries differ significantly from sibling readings like 'real_incident_necessity' and 'hybrid_occupation', which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
