% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__neurodiversity_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Taxonomy as Pathologization of Neurodiversity
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint represents the neurodiversity reading of the DSM taxonomy
 *   kernel: that its categories pathologize natural human neurological
 *   variation, serving to enforce institutional behavioral norms rather than
 *   objectively describe disease. The constraint is claimed as a Snare
 *   because its primary function, from this reading's perspective, is
 *   extraction through pathologization and coercive normalization, with the
 *   coordination story (standardized diagnosis) serving as cover. The metrics
 *   reflect high extractiveness and suppression, as the system actively
 *   enforces conformity and limits alternatives for neurodivergent
 *   individuals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.85).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.78).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Taxonomy as Pathologization of Neurodiversity").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, 'cd5f5e56-2c8f-4288-9548-41feda493b5e').
narrative_ontology:cs_kernel_codification('cd5f5e56-2c8f-4288-9548-41feda493b5e', formalized).
narrative_ontology:cs_authority_grounding('cd5f5e56-2c8f-4288-9548-41feda493b5e', lineage).
narrative_ontology:cs_interpretation_layer_present('cd5f5e56-2c8f-4288-9548-41feda493b5e').
narrative_ontology:cs_reading_relation('cd5f5e56-2c8f-4288-9548-41feda493b5e', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd5f5e56-2c8f-4288-9548-41feda493b5e', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('cd5f5e56-2c8f-4288-9548-41feda493b5e', foundational, neurological_variation_is_natural).
narrative_ontology:cs_axiom_status(neurological_variation_is_natural, holdable).
narrative_ontology:cs_axiom_grounding('cd5f5e56-2c8f-4288-9548-41feda493b5e', neurological_variation_is_natural, deontological).
narrative_ontology:cs_axiom('cd5f5e56-2c8f-4288-9548-41feda493b5e', foundational, pathologization_is_harm).
narrative_ontology:cs_axiom_status(pathologization_is_harm, holdable).
narrative_ontology:cs_axiom_grounding('cd5f5e56-2c8f-4288-9548-41feda493b5e', pathologization_is_harm, deontological).
narrative_ontology:cs_reference_frame('cd5f5e56-2c8f-4288-9548-41feda493b5e', neurodiversity_affirmation_framework).
narrative_ontology:cs_drift_state('cd5f5e56-2c8f-4288-9548-41feda493b5e', contemporary_dsm_v_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cd5f5e56-2c8f-4288-9548-41feda493b5e', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, neurotypical_majority).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are labeled with DSM diagnoses, leading to medical interventions, coercive normalization, and denial of accommodations in various institutional settings. Their neurological differences are framed as deficits requiring correction, rather than variations to be accommodated. Exit means denying their own neurological reality or facing severe social and institutional penalties.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals, payer,
    powerless, biographical, identity_locked, global).

% Schools, employers, carceral systems, and healthcare providers benefit from the DSM taxonomy by having a standardized framework to classify, manage, and often exclude or 'treat' individuals who do not conform to their behavioral norms. This reduces the institutional burden of adapting to diverse needs.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems, beneficiary,
    institutional, generational, constrained, national).

% Actively resist the pathologizing framework of the DSM, advocating for a social model of disability and neurodiversity affirmation. They bear the cost of challenging a deeply entrenched medical paradigm, often facing professional marginalization and public misunderstanding.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates, payer,
    moderate, generational, constrained, global).

% Benefits from the DSM's implicit reinforcement of neurotypical norms as 'normal' or 'healthy,' which simplifies social interaction and reduces the perceived need for societal adaptation to neurodivergent ways of being. They are not directly involved in the taxonomy's creation but benefit from its social effects.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurotypical_majority, beneficiary,
    organized, biographical, mobile, global).

% Are the primary authors and enforcers of the DSM taxonomy. They define diagnostic criteria, conduct research, and implement treatments based on these categories. Their professional identity and authority are deeply intertwined with the DSM framework.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_professionals, agenda_setter,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__neurodiversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized language and framework for classifying mental health conditions, facilitating communication among clinicians, researchers, and insurance providers, and guiding treatment decisions.
% TRANSFER_FUNCTION: Transfers social and institutional power to define 'normal' and 'pathological' from diverse individual experiences to a centralized medical authority, resulting in the extraction of self-determination and resources from neurodivergent individuals for 'treatment' and 'accommodation' that often means normalization.
% ABSENT_VOICES: The lived experience and self-advocacy of neurodivergent individuals are often marginalized or reframed through the lens of pathology within the DSM's development and application. Indigenous and non-Western understandings of mental well-being are also largely excluded.
% DISAPPEARANCE_RATIONALE: If the DSM taxonomy vanished overnight, the entire system of psychiatric diagnosis, treatment, insurance billing, and institutional accommodation would collapse. Society would be forced to confront neurological variation without a pathologizing framework, leading to a radical reorganization of social norms, educational practices, and employment structures.
% FOUNDING_PROBLEM: To create a common nomenclature for mental disorders, improve diagnostic reliability, and facilitate research into causes and treatments for mental illness.
% FOUNDING_PROBLEM_CORROBORATION: Psychiatric professionals and pharmaceutical companies largely attest the problem is still live, citing ongoing need for diagnostic clarity and treatment. Neurodiversity advocates and critical psychiatrists attest the founding problem has been superseded by the taxonomy's role in social control and market creation; sociological studies and patient advocacy narratives from outside the benefiting parties support this shifted-function reading.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the act of pathologizing natural variation itself constitutes a significant harm, leading to denial of self-determination, forced interventions, and systemic discrimination. Suppression (0.78) is also high, as the medical authority of the DSM actively suppresses alternative understandings of neurodiversity and limits access to accommodations that do not conform to a deficit model. Theater ratio is low (0.20) because the diagnostic function is genuinely performed, but its underlying purpose, from this reading, is primarily social control rather than objective medical care. Accessibility collapse is moderate (0.65) because while neurodiversity frameworks exist, they are often marginalized by the dominant medical model. Resistance is high (0.70) due to active advocacy from neurodiversity movements.
 *
 * PERSPECTIVAL GAP:
 *   The neurodiversity reading fundamentally diverges from the biomedical reading on the nature of the 'problem' itself. From the biomedical perspective, the DSM is a tool for identifying and treating disease; from the neurodiversity perspective, it is a tool for social control that creates disease where none exists. This divergence leads to vastly different classifications and perceived beneficiaries/victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Neurodivergent individuals are the primary victims (d near 1.0), bearing the costs of pathologization and coercive normalization. Institutional systems (schools, employers) and the neurotypical majority are beneficiaries (d near 0.0), as the taxonomy simplifies their environment by framing non-conformity as illness. Psychiatric professionals, as agenda-setters, benefit from maintaining their authority and professional identity tied to the DSM (d near 0.15).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the DSM as a 'Rope' (pure coordination) by highlighting the coercive and extractive aspects of pathologization. It also distinguishes it from a 'Piton' by emphasizing the active enforcement and clear beneficiaries of the current system, rather than mere institutional inertia. The 'Snare' classification captures the active harm and suppression inherent in framing natural variation as illness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objective_vs_social_construction,
    'To what extent do DSM categories reflect objective, discoverable neurobiological disease entities versus socially constructed labels for behavioral non-conformity?',
    'Longitudinal studies correlating diagnostic categories with specific, consistent neurobiological markers, independent of behavioral presentation or social context. If such markers are consistently found, it would support the biomedical reading; if not, it would support the social constructionist view.',
    'If categories are primarily social constructs, the extractiveness of the DSM as a Snare is amplified, as it extracts from individuals based on arbitrary social norms. If objective, the extractiveness might be re-evaluated as a necessary cost of medical intervention, potentially shifting the classification towards a Tangled Rope or even a Rope (from the biomedical perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(objective_vs_social_construction, empirical, 'Ambiguity between objective disease and social construction in DSM categories.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of neurodivergent individuals structural (institutional barriers, lack of legal accommodation) or internalized (self-stigma, belief in one''s own pathology)?',
    'Post-diagnosis trajectory: if individuals continue to experience self-stigma and limit their own agency even after structural barriers are removed or accommodations are offered, it suggests a significant internalized component. If removal of structural barriers leads to immediate and sustained improvements in well-being and agency, it suggests primarily structural suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — neurodivergent individuals carry the suppression with them after formal ''exit'' from coercive environments. This would amplify the Snare classification''s severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for neurodivergent individuals.').

omega_variable(
    framing_underdetermination_dsm_kernel,
    'Is the neurodiversity reading the most defensible framing of the DSM taxonomy, or do alternative framings (biomedical, critical psychiatry) offer equally coherent, albeit different, structural analyses?',
    'A meta-analysis of all three readings, assessing their internal consistency, empirical support for their core claims, and explanatory power for observed social and medical phenomena. The framing that best accounts for the full range of evidence without internal contradiction would be preferred.',
    'If an alternative framing (e.g., biomedical) is found to be more robust, the classification of the DSM could shift dramatically (e.g., to a Rope or Tangled Rope from that perspective). This omega acknowledges the inherent conceptual contestation of the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_dsm_kernel, conceptual, 'Acknowledges that the choice of reading for the DSM kernel is a conceptual choice with significant classification implications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dsm__tr_t10, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(dsm__tr_t20, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(dsm__tr_t30, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(dsm__be_t10, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(dsm__be_t20, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(dsm__be_t30, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(dsm__su_t10, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(dsm__su_t20, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(dsm__su_t30, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, identity_coordination).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, mental_health_insurance_billing_codes).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, special_education_placement_criteria).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, disability_accommodation_laws).

% DUAL FORMULATION NOTE:
% This constraint is the 'neurodiversity_reading' of the 'dsm_taxonomy_kernel'. It is structurally distinct from the 'biomedical_reading' and 'critical_psychiatry_reading' of the same kernel, which would yield different extractiveness values and classifications. All three readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
