% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Proportionality Principle for Public Health Interventions
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint embodies the principle that public health interventions
 *   must be proportional to the threat level posed by a disease, balancing
 *   population harm and individual autonomy. The severity of intervention
 *   (e.g., mandatory vaccination vs. mask mandates) is scaled by disease
 *   characteristics like transmissibility and case-fatality rate. For highly
 *   transmissible and severe diseases (e.g., measles), more restrictive
 *   measures are deemed legitimate, impacting individual autonomy more. For
 *   less severe threats (e.g., seasonal flu), interventions are expected to
 *   be milder, with greater deference to individual choice. This reading
 *   introduces a conditional structure to public health policy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.45).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.3).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality Principle for Public Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '56e25dfb-0684-4edc-bbda-7b697e3d5d84').
narrative_ontology:cs_kernel_codification('56e25dfb-0684-4edc-bbda-7b697e3d5d84', formalized).
narrative_ontology:cs_authority_grounding('56e25dfb-0684-4edc-bbda-7b697e3d5d84', lineage).
narrative_ontology:cs_interpretation_layer_present('56e25dfb-0684-4edc-bbda-7b697e3d5d84').
narrative_ontology:cs_reading_relation('56e25dfb-0684-4edc-bbda-7b697e3d5d84', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('56e25dfb-0684-4edc-bbda-7b697e3d5d84', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('56e25dfb-0684-4edc-bbda-7b697e3d5d84', foundational, intervention_severity_must_scale_with_threat).
narrative_ontology:cs_axiom_status(intervention_severity_must_scale_with_threat, holdable).
narrative_ontology:cs_axiom_grounding('56e25dfb-0684-4edc-bbda-7b697e3d5d84', intervention_severity_must_scale_with_threat, empirically_contingent).
narrative_ontology:cs_axiom('56e25dfb-0684-4edc-bbda-7b697e3d5d84', foundational, individual_autonomy_is_a_conditional_right).
narrative_ontology:cs_axiom_status(individual_autonomy_is_a_conditional_right, holdable).
narrative_ontology:cs_axiom_grounding('56e25dfb-0684-4edc-bbda-7b697e3d5d84', individual_autonomy_is_a_conditional_right, deontological).
narrative_ontology:cs_reference_frame('56e25dfb-0684-4edc-bbda-7b697e3d5d84', balanced_ethical_framework).
narrative_ontology:cs_drift_state('56e25dfb-0684-4edc-bbda-7b697e3d5d84', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('56e25dfb-0684-4edc-bbda-7b697e3d5d84', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, general_public).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, individuals_subject_to_mild_interventions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, individuals_subject_to_severe_interventions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for implementing public health measures, they interpret and apply the proportionality principle to justify interventions. They benefit from a framework that legitimizes their actions but are constrained by legal and ethical challenges to proportionality.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from reduced disease transmission and severity due to effective, proportionally applied public health measures. They bear some costs in terms of individual liberty but generally accept these for collective safety.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, general_public, beneficiary,
    organized, generational, constrained, national).

% Experience a curtailment of autonomy (e.g., mask mandates, contact tracing) for diseases with lower threat levels. They bear the direct cost of compliance, which may feel disproportionate to their perceived risk.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, individuals_subject_to_mild_interventions, payer,
    moderate, biographical, constrained, local).

% Face significant restrictions (e.g., mandatory vaccination, quarantine) for highly threatening diseases. While the proportionality principle justifies these, the impact on their autonomy and daily life is substantial, and their options for non-compliance are severely limited.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, individuals_subject_to_severe_interventions, payer,
    powerless, biographical, identity_locked, local).

% Monitor public health policies to ensure interventions remain proportional and do not unduly infringe on individual rights. They challenge policies they deem disproportionate, acting as a check on the agenda-setter.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health responses by providing a framework to balance collective safety with individual rights, ensuring interventions are justified by the specific disease threat.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy to public health authorities in exchange for collective protection from disease, with the extent of transfer determined by the disease's characteristics.
% ABSENT_VOICES: Those who reject any state intervention in health matters, regardless of proportionality, are often marginalized in policy discussions. Their voices would argue for absolute bodily autonomy, which this reading explicitly balances against collective harm.
% DISAPPEARANCE_RATIONALE: Without the proportionality principle, public health interventions would either become overly coercive (if public health is primary) or ineffective (if individual autonomy is absolute), leading to chaotic and inconsistent responses to disease outbreaks. The legal and ethical landscape of public health would be fundamentally altered.
% FOUNDING_PROBLEM: The challenge of legitimizing state interventions in individual health during epidemics, avoiding both tyranny and anarchy, by finding a principled balance between collective good and individual rights.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, ethicists, and international human rights organizations consistently affirm the ongoing relevance of proportionality in public health law and policy, providing corroboration from outside the immediate public health authority beneficiaries.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).
:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the burden placed on individuals by necessary, but sometimes intrusive, interventions. Suppression (0.30) is also moderate, as enforcement is required for compliance, but it's not absolute, allowing for some resistance or legal challenge. Theater ratio is low (0.10) because the principle aims for genuine, evidence-based justification. Accessibility collapse (0.40) is moderate, as alternatives to interventions exist (e.g., personal precautions) but are constrained by the collective good. Resistance (0.25) is present but not overwhelming, as the proportionality argument often garners broad, if not universal, acceptance.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities may perceive this as a clear, justifiable Rope, enabling effective disease management. Individuals whose autonomy is curtailed, even proportionally, may experience it as more extractive, especially if they disagree with the threat assessment or the weighting of values. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The general public benefits from effective disease control, while public health authorities benefit from a clear framework for action (d near beneficiary end). Individuals subject to interventions, particularly for less severe threats, bear the costs of autonomy infringement (d near target end). The proportionality principle aims to keep these costs within a justifiable range, preventing the constraint from becoming purely extractive.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_weighting_ambiguity,
    'How are ''population harm'' and ''individual autonomy'' precisely weighted against each other for different disease characteristics?',
    'Development of a standardized, empirically-informed risk-benefit calculus for public health interventions, subject to judicial review.',
    'Clearer weighting would reduce contestation and improve consistency in policy application; ambiguity allows for arbitrary or politically motivated interpretations, potentially increasing extraction or suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_weighting_ambiguity, conceptual, 'Ambiguity in weighting population harm vs. individual autonomy.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine application of proportionality, or is it a cover for prioritizing public health or individual autonomy?',
    'Analysis of policy outcomes: if interventions consistently align with a balanced weighting across diverse disease contexts, it supports the proportionality reading. If outcomes consistently favor one pole (e.g., always prioritizing public health regardless of individual burden), it suggests a different underlying reading.',
    'If it''s a genuine proportionality reading, the constraint functions as a Rope. If it''s a disguised ''public_health_primary'' or ''bodily_autonomy_primary'' reading, the classification would shift to a Tangled Rope or Snare, respectively, with different beneficiary/victim structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''proportionality_reading'' of the ''legitimate_health_intervention'' kernel. Sibling readings are ''public_health_primary'' and ''bodily_autonomy_primary''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__proportionality_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__proportionality_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__proportionality_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__proportionality_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimate_health_intervention' kernel, focusing on proportionality. It is distinct from 'public_health_primary' and 'bodily_autonomy_primary' readings, which emphasize different foundational principles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
