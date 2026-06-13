% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Exercised Competence
 *   domain: disaster_preparedness/governance
 *
 * SUMMARY:
 *   This constraint, 'Preparedness as Live Exercised Competence,' describes a
 *   system where drills and inspections are genuinely designed and executed
 *   to maintain and improve operational capacity for disaster response. It
 *   emphasizes active learning, skill retention, and adaptive capability over
 *   symbolic performance. The constraint is framed as a Rope, reflecting its
 *   coordination function and broad benefits, with minimal extraction and
 *   suppression. This is one reading of the broader 'preparedness_retention'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.1).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Competence").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "disaster_preparedness/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, 'e7b80aeb-7b86-4447-9703-79de8072ca88').
narrative_ontology:cs_kernel_codification('e7b80aeb-7b86-4447-9703-79de8072ca88', formalized).
narrative_ontology:cs_authority_grounding('e7b80aeb-7b86-4447-9703-79de8072ca88', expertise).
narrative_ontology:cs_interpretation_layer_present('e7b80aeb-7b86-4447-9703-79de8072ca88').
narrative_ontology:cs_reading_relation('e7b80aeb-7b86-4447-9703-79de8072ca88', preparedness_retention__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('e7b80aeb-7b86-4447-9703-79de8072ca88', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('e7b80aeb-7b86-4447-9703-79de8072ca88', foundational, competence_is_exercised_knowledge).
narrative_ontology:cs_axiom_status(competence_is_exercised_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('e7b80aeb-7b86-4447-9703-79de8072ca88', competence_is_exercised_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('e7b80aeb-7b86-4447-9703-79de8072ca88', secondary, adaptive_capacity_is_paramount).
narrative_ontology:cs_axiom_status(adaptive_capacity_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('e7b80aeb-7b86-4447-9703-79de8072ca88', adaptive_capacity_is_paramount, instrumental).
narrative_ontology:cs_reference_frame('e7b80aeb-7b86-4447-9703-79de8072ca88', functional_operational_readiness).
narrative_ontology:cs_drift_state('e7b80aeb-7b86-4447-9703-79de8072ca88', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e7b80aeb-7b86-4447-9703-79de8072ca88', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, general_population).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, first_responders).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, public_health_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, government_budget_managers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits directly from effective disaster response and mitigation, experiencing reduced harm and faster recovery. Pays indirectly through taxes that fund preparedness efforts.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, general_population, beneficiary,
    organized, generational, analytical, national).

% Actively participate in and lead drills, inspections, and training. Their competence is directly maintained by these practices, enabling effective response. They advocate for resource allocation to support genuine training.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, first_responders, agenda_setter,
    institutional, biographical, constrained, local).

% Relies on the operational capacity maintained by preparedness practices to execute public health interventions during crises. Benefits from a resilient system that prevents widespread health impacts.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, public_health_agencies, beneficiary,
    institutional, generational, constrained, national).

% Allocate funds for preparedness activities. They face pressure to optimize fiscal efficiency and may view extensive, competence-focused drills as costly, potentially seeking to reduce investment.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, government_budget_managers, payer,
    institutional, immediate, mobile, national).

% Design, implement, and evaluate preparedness programs, including drills and inspections. They are responsible for ensuring these practices genuinely build and retain operational capacity.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_management_officials, agenda_setter,
    institutional, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__competence_reading, general_population).
narrative_ontology:fixing_cost_class(preparedness_retention__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions and knowledge of diverse agencies and personnel (first responders, public health, local government) to ensure a unified, effective response to disasters, preventing chaotic and ineffective individual efforts.
% TRANSFER_FUNCTION: Transfers resources (time, funding, personnel) into training, drills, and infrastructure maintenance, which in turn generates and retains operational competence and adaptive capacity for disaster response.
% ABSENT_VOICES: Future victims of unaddressed risks, who would advocate for more robust, competence-driven preparedness, are absent from current resource allocation debates. Also, those who would benefit from alternative, more efficient competence-building methods might be excluded if current practices become entrenched.
% DISAPPEARANCE_RATIONALE: If the commitment to live exercised knowledge vanished, operational competence would degrade rapidly. Agencies would lose coordination, response times would lengthen, and the ability to adapt to novel threats would diminish, leading to significantly higher casualties and economic losses in future disasters.
% FOUNDING_PROBLEM: The problem of maintaining complex operational capacity and institutional memory for infrequent, high-impact events, where knowledge degrades without active exercise.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested by disaster recovery reports, post-mortem analyses of past failures, and expert consensus in emergency management and organizational learning. Academic research on skill decay and institutional memory also corroborates the ongoing nature of this challenge.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.1) because resources are genuinely converted into competence, with minimal overhead or rent-seeking. Suppression is low (0.05) as participation is driven by shared understanding of necessity and professional standards, not coercion. Theater ratio is negligible (0.01) because the focus is on functional outcomes, not performative display. The metrics reflect a system that effectively solves a coordination problem with broad benefits.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the general population and first responders, this constraint is a clear Rope, providing essential coordination and safety. From the perspective of budget managers, it might appear as a cost center, but the long-term benefits of avoided disaster costs make it a net positive. The key is the low ceremony-to-competence ratio, ensuring that the investment translates directly into functional capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   The general population, first responders, and public health agencies are primary beneficiaries, gaining safety and effective response. Government budget managers are payers, bearing the fiscal cost, but the return on investment in competence is high. There are no identifiable victims, as the system is designed for collective benefit. Directionality for beneficiaries is low (subsidized), for payers it's near symmetric (costs balanced by system-wide benefits).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine competence-preserving practices as mere bureaucratic overhead or performative 'husk.' By focusing on live exercised knowledge, it ensures the mandate (maintaining operational capacity) remains directly tied to its function, avoiding mandatrophy where the form persists without the substance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_husk_ambiguity,
    'Is the observed preparedness truly competence-preserving, or is it drifting towards ceremonial performance (husk_reading)?',
    'Independent, unannounced operational audits and real-world stress tests (e.g., during minor incidents) that assess adaptive capacity and actual skill retention, rather than compliance with procedural checklists.',
    'If found to be drifting towards ''husk,'' the constraint''s extractiveness and theater_ratio would be significantly higher, and its classification would shift towards Piton or Snare, as resources are consumed for symbolic rather than functional outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_husk_ambiguity, empirical, 'Distinguishing genuine competence from performative compliance.').

omega_variable(
    resource_allocation_efficiency,
    'Is the resource allocation for competence-preserving practices truly optimal, or is there over-investment leading to fiscal inefficiency?',
    'Comparative analysis with similar systems globally, benchmarking cost-effectiveness of different training methodologies, and cost-benefit analysis of avoided disaster impacts versus preparedness spending.',
    'If over-investment is identified, the ''victim'' set would expand to include ''fiscal efficiency,'' and the extractiveness, while still low, would be slightly higher due to inefficient resource use, potentially nudging it towards a very benign Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Assessing the efficiency of resource allocation for preparedness.').

omega_variable(
    reading_framing_choice,
    'Is ''preparedness_retention__competence_reading'' the most appropriate framing, or does the ''hybrid_reading'' (stratified competence) better capture the reality of preparedness across different institutional levels?',
    'Detailed sociological and organizational studies across various levels of government and civil society to map where genuine competence resides versus where ceremonial practices dominate.',
    'If the ''hybrid_reading'' is adopted, this constraint would likely be re-scoped to specific, high-competence institutions, and a separate, more extractive ''husk_reading'' would apply to other societal layers, leading to a family of linked constraints with different classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'Under-determination of framing between competence-only and hybrid readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__competence_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__competence_reading, theater_ratio, 20, 0.03).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__competence_reading, theater_ratio, 30, 0.02).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__competence_reading, theater_ratio, 40, 0.01).
narrative_ontology:measurement(prep_tr_t50, preparedness_retention__competence_reading, theater_ratio, 50, 0.01).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__competence_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__competence_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__competence_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__competence_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(prep_be_t50, preparedness_retention__competence_reading, base_extractiveness, 50, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__competence_reading, suppression_requirement, 10, 0.09).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__competence_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__competence_reading, suppression_requirement, 30, 0.07).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__competence_reading, suppression_requirement, 40, 0.06).
narrative_ontology:measurement(prep_su_t50, preparedness_retention__competence_reading, suppression_requirement, 50, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__competence_reading, 0.1).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'preparedness_retention' kernel. It focuses on the functional, competence-preserving aspect, contrasting with the 'husk_reading' (ceremonial performance) and 'hybrid_reading' (stratified competence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
