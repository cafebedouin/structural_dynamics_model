% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness (Hybrid Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes disaster preparedness as a stratified system
 *   where some components (e.g., engineering inspections) maintain genuine
 *   competence, while others (e.g., certain evacuation drills) have become
 *   ritualized performances. This 'hybrid reading' acknowledges both
 *   functional and performative aspects, leading to a mixed classification.
 *   The system extracts resources from taxpayers and frontline responders,
 *   while providing genuine benefits from competent subsystems and political
 *   cover for public officials. The claimed type is Tangled Rope, reflecting
 *   the mix of coordination and extraction, with a significant theater ratio
 *   indicating the performative elements.
 *
 * KEY AGENTS:
 *   - competent_subsystems: Beneficiary (institutional/constrained) — maintain genuine readiness.
 *   - ritualized_subsystems: Payer (organizational/identity_locked) — performative, consume resources.
 *   - public_officials: Agenda Setter (institutional/constrained) — oversee system, gain political capital.
 *   - taxpayers: Payer (organized/constrained) — bear financial costs.
 *   - frontline_responders: Payer (moderate/constrained) — directly affected by preparedness quality.
 *   - analytical_observers: Observer (analytical/analytical) — study effectiveness, no direct power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.45).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.3).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster Preparedness (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, '66fe9c2a-8d90-4ffb-8616-7c570d768ec2').
narrative_ontology:cs_kernel_codification('66fe9c2a-8d90-4ffb-8616-7c570d768ec2', formalized).
narrative_ontology:cs_authority_grounding('66fe9c2a-8d90-4ffb-8616-7c570d768ec2', lineage).
narrative_ontology:cs_interpretation_layer_present('66fe9c2a-8d90-4ffb-8616-7c570d768ec2').
narrative_ontology:cs_reading_relation('66fe9c2a-8d90-4ffb-8616-7c570d768ec2', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('66fe9c2a-8d90-4ffb-8616-7c570d768ec2', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_axiom('66fe9c2a-8d90-4ffb-8616-7c570d768ec2', foundational, preparedness_is_stratified).
narrative_ontology:cs_axiom_status(preparedness_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('66fe9c2a-8d90-4ffb-8616-7c570d768ec2', preparedness_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('66fe9c2a-8d90-4ffb-8616-7c570d768ec2', secondary, ritual_coexists_with_competence).
narrative_ontology:cs_axiom_status(ritual_coexists_with_competence, holdable).
narrative_ontology:cs_axiom_grounding('66fe9c2a-8d90-4ffb-8616-7c570d768ec2', ritual_coexists_with_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('66fe9c2a-8d90-4ffb-8616-7c570d768ec2', mixed_operational_and_symbolic_readiness).
narrative_ontology:cs_drift_state('66fe9c2a-8d90-4ffb-8616-7c570d768ec2', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('66fe9c2a-8d90-4ffb-8616-7c570d768ec2', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, competent_subsystems).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, public_officials).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, taxpayers).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, frontline_responders).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).
:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting that while some resources are genuinely used for coordination, a significant portion is diverted to maintaining ritualized components. Suppression (0.30) is low, as resistance to the ritualized aspects is diffuse and often internalized rather than actively suppressed. The theater ratio (0.55) is high, indicating that more than half of the activity is performative rather than functional. Accessibility collapse (0.40) is moderate, as alternatives to the current stratified system exist but are difficult to implement due to institutional inertia. Resistance (0.20) is low, as the diffuse nature of the problem makes organized resistance challenging.
 *
 * PERSPECTIVAL GAP:
 *   Public officials and competent subsystems likely perceive the entire system as a necessary Rope or even a Mountain, emphasizing the genuine coordination and natural necessity of preparedness. Taxpayers and frontline responders, especially those affected by failures, would experience it as more extractive, closer to a Snare or Piton, due to the costs of ritualized components and the risks of inadequate readiness. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent subsystems are beneficiaries (d=0.0-0.2) as they receive funding and support for their effective work. Public officials are agenda setters and beneficiaries (d=0.1-0.3) due to political gains. Ritualized subsystems are payers (d=0.6-0.8) as they consume resources without proportional output, and their personnel are identity-locked into maintaining the status quo. Taxpayers and frontline responders are payers (d=0.7-0.9) as they bear the costs and risks. Analytical observers are neutral (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This hybrid reading prevents mislabeling the entire preparedness system as either fully competent (Rope/Mountain) or fully atrophied (Piton/Snare). By acknowledging both functional and ritualized components, it captures the nuanced reality where some parts still serve their mandate while others persist due to inertia or performative value. The moderate D5 risk (drift to Piton) is specifically due to the high theater ratio and the potential for ritualized components to further displace genuine competence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_ritual_boundary,
    'What is the precise boundary between genuinely competent and ritualized components within the preparedness system, and how does it shift over time?',
    'Longitudinal, independent operational audits and post-disaster performance reviews, comparing actual outcomes to resource allocation and training inputs.',
    'A clearer boundary would allow for targeted resource reallocation, potentially reducing extractiveness and theater ratio by defunding ritualized components and strengthening competent ones. This could shift the constraint towards a more effective Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_ritual_boundary, empirical, 'Distinguishing functional from performative preparedness elements.').

omega_variable(
    mandate_drift_mechanism,
    'Is the drift towards ritualization primarily driven by resource scarcity, political incentives for ''activity'' over ''outcomes'', or a loss of institutional memory?',
    'Comparative case studies across different jurisdictions and historical periods, analyzing policy changes, budget allocations, and institutional learning processes.',
    'Identifying the primary driver would inform intervention strategies: resource injection, accountability reforms, or knowledge management initiatives. This would directly impact the potential for mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_drift_mechanism, empirical, 'Understanding the causes of preparedness ritualization.').

omega_variable(
    reading_framing_impact,
    'Does framing preparedness as ''stratified'' (this reading) lead to different policy outcomes compared to framing it as ''fully competent'' or ''fully atrophied''?',
    'Analysis of policy debates and resource allocation decisions in contexts where different framings are dominant. Compare the observed balance of competence and ritual.',
    'If this ''hybrid'' framing leads to more nuanced and effective policy interventions, it supports its utility. If it merely provides cover for inaction, its conceptual value is diminished.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_impact, conceptual, 'Impact of the ''hybrid'' framing on policy and outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1980, preparedness_persistence__hybrid_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(prep_tr_t1990, preparedness_persistence__hybrid_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(prep_tr_t2000, preparedness_persistence__hybrid_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(prep_tr_t2010, preparedness_persistence__hybrid_reading, theater_ratio, 2010, 0.53).
narrative_ontology:measurement(prep_tr_t2024, preparedness_persistence__hybrid_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(prep_be_t1980, preparedness_persistence__hybrid_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(prep_be_t1990, preparedness_persistence__hybrid_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(prep_be_t2000, preparedness_persistence__hybrid_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(prep_be_t2010, preparedness_persistence__hybrid_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(prep_be_t2024, preparedness_persistence__hybrid_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1980, preparedness_persistence__hybrid_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(prep_su_t1990, preparedness_persistence__hybrid_reading, suppression_requirement, 1990, 0.27).
narrative_ontology:measurement(prep_su_t2000, preparedness_persistence__hybrid_reading, suppression_requirement, 2000, 0.29).
narrative_ontology:measurement(prep_su_t2010, preparedness_persistence__hybrid_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(prep_su_t2024, preparedness_persistence__hybrid_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__hybrid_reading, 0.1).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'preparedness_persistence' kernel, alongside 'competence_reading' and 'husk_reading'. Each reading offers a distinct structural interpretation of disaster preparedness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
