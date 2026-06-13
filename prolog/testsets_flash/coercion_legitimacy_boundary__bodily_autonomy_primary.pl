% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Medical Intervention
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint asserts that medical intervention without an individual's
 *   consent is fundamentally impermissible, irrespective of any potential
 *   collective benefit. It prioritizes individual bodily autonomy as a
 *   primary ethical and legal boundary. This reading implies that even in
 *   public health crises, non-consensual interventions (like mandatory
 *   vaccination or treatment) are illegitimate. The constraint's low
 *   extractiveness and suppression reflect a system where individual choice
 *   is largely respected, and enforcement of non-consensual measures is
 *   minimal or absent. Immunocompromised individuals are listed as victims
 *   because this reading, by prioritizing individual autonomy, implicitly
 *   accepts their increased exposure risk from unvaccinated populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.3).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.1).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.3).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy as Primary in Medical Intervention").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, 'eb7317de-2fce-45b4-9321-d8f7369d463c').
narrative_ontology:cs_kernel_codification('eb7317de-2fce-45b4-9321-d8f7369d463c', formalized).
narrative_ontology:cs_authority_grounding('eb7317de-2fce-45b4-9321-d8f7369d463c', lineage).
narrative_ontology:cs_interpretation_layer_present('eb7317de-2fce-45b4-9321-d8f7369d463c').
narrative_ontology:cs_reading_relation('eb7317de-2fce-45b4-9321-d8f7369d463c', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('eb7317de-2fce-45b4-9321-d8f7369d463c', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('eb7317de-2fce-45b4-9321-d8f7369d463c', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('eb7317de-2fce-45b4-9321-d8f7369d463c', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('eb7317de-2fce-45b4-9321-d8f7369d463c', foundational, consent_precedes_benefit).
narrative_ontology:cs_axiom_status(consent_precedes_benefit, holdable).
narrative_ontology:cs_axiom_grounding('eb7317de-2fce-45b4-9321-d8f7369d463c', consent_precedes_benefit, deontological).
narrative_ontology:cs_reference_frame('eb7317de-2fce-45b4-9321-d8f7369d463c', post_nuremberg_code_ethics).
narrative_ontology:cs_drift_state('eb7317de-2fce-45b4-9321-d8f7369d463c', contemporary_pandemic_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('eb7317de-2fce-45b4-9321-d8f7369d463c', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_seeking_autonomy).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).
:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it establishes a clear boundary that benefits individuals by protecting their autonomy, with minimal extraction or suppression. Its persistence relies on broad societal and legal acceptance of individual rights, rather than active enforcement against dissenters. The low extractiveness (0.3) reflects the 'cost' of not being able to compel action for collective benefit, which is diffuse. Suppression (0.1) is low because the constraint is largely self-enforcing through cultural norms and legal frameworks, rather than requiring active coercion. Theater ratio is negligible (0.05) as its function is direct and not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'individuals_seeking_autonomy', this constraint is a clear Rope, protecting a fundamental right. From the perspective of 'immunocompromised_individuals', it might be perceived as a Snare, as their safety is compromised by the autonomy of others, leading to a higher effective extraction for them. 'Mandate_enforcers' benefit from a clear, non-interventionist boundary, simplifying their role.
 *
 * DIRECTIONALITY LOGIC:
 *   'Individuals_seeking_autonomy' are clear beneficiaries (d=0.0-0.1) as the constraint directly protects their choice. 'Mandate_enforcers' are also beneficiaries (d=0.1-0.2) as this reading simplifies their role by setting a clear, non-coercive boundary. 'Immunocompromised_individuals' are victims (d=0.8-0.9) because the constraint's prioritization of individual autonomy over collective health directly increases their risk of exposure to preventable diseases.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a fundamental rights protection as a form of extraction. While there are costs to collective health, the constraint's primary function is to coordinate individual liberty, not to extract from it. The low extractiveness and suppression metrics confirm it is not a Snare or Tangled Rope, despite the 'victim' declaration for immunocompromised individuals. The 'victim' status here highlights a consequence of the constraint's operation, not an extractive mechanism within it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine expression of a fundamental right, or a specific reading of a contested kernel?',
    'Analysis of legal precedent and philosophical arguments across different jurisdictions and ethical traditions.',
    'If a fundamental right, its classification as a Rope is robust. If a reading, its classification is contingent on the acceptance of the ''coercion_legitimacy_boundary'' kernel and the ''bodily_autonomy_primary'' interpretation, which could shift if other readings gain prominence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading (''bodily_autonomy_primary'') of the ''coercion_legitimacy_boundary'' kernel.').

omega_variable(
    sibling_reading_impact_proportionality,
    'How would the ''proportionality_reading'' (coercion scales with disease severity) alter the beneficiary/victim structure?',
    'Empirical data on disease severity and transmission rates, combined with legal analysis of proportionality tests.',
    'The ''proportionality_reading'' would shift the balance, potentially reducing the ''immunocompromised_individuals'' victim set under low-severity threats, but increasing it under high-severity threats where mandates are deemed legitimate. It would also shift ''mandate_enforcers'' from beneficiaries to agenda_setters with a more active role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_proportionality, empirical, 'Impact of ''proportionality_reading'' on constraint structure.').

omega_variable(
    sibling_reading_impact_public_health,
    'How would the ''public_health_primary'' reading (state may compel intervention for collective harm) alter the beneficiary/victim structure?',
    'Analysis of public health law and emergency powers, and epidemiological data on collective harm.',
    'The ''public_health_primary'' reading would significantly expand the ''immunocompromised_individuals'' beneficiary set (as they are protected by mandates), and shift ''individuals_seeking_autonomy'' into a victim role. ''Mandate_enforcers'' would become primary agenda_setters, and the constraint would likely reclassify as a Tangled Rope or Snare due to increased extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_public_health, conceptual, 'Impact of ''public_health_primary'' reading on constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.05).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 10, 0.05).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 20, 0.05).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 30, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 30, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'coercion_legitimacy_boundary' kernel. Other readings (e.g., 'public_health_primary', 'proportionality_reading') would yield different classifications and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
