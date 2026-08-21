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
 *   human_readable: DSM Taxonomy: Neurodiversity Reading
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint story represents the 'neurodiversity reading' of the DSM
 *   taxonomy kernel. From this perspective, the DSM's categories function as
 *   a snare, pathologizing natural human neurological variation that
 *   conflicts with prevailing institutional behavioral norms. It highlights
 *   how the diagnostic system, while ostensibly providing a common language
 *   for mental health, primarily serves to enforce conformity and extract
 *   compliance from neurodivergent individuals, benefiting institutional
 *   systems that demand standardized behavior.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.85).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.78).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Taxonomy: Neurodiversity Reading").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, '908b5273-9827-451a-99f4-9d4a749ada0d').
narrative_ontology:cs_kernel_codification('908b5273-9827-451a-99f4-9d4a749ada0d', fixed_text).
narrative_ontology:cs_authority_grounding('908b5273-9827-451a-99f4-9d4a749ada0d', lineage).
narrative_ontology:cs_interpretation_layer_present('908b5273-9827-451a-99f4-9d4a749ada0d').
narrative_ontology:cs_reading_relation('908b5273-9827-451a-99f4-9d4a749ada0d', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('908b5273-9827-451a-99f4-9d4a749ada0d', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('908b5273-9827-451a-99f4-9d4a749ada0d', foundational, neurological_variation_is_natural).
narrative_ontology:cs_axiom_status(neurological_variation_is_natural, holdable).
narrative_ontology:cs_axiom_grounding('908b5273-9827-451a-99f4-9d4a749ada0d', neurological_variation_is_natural, deontological).
narrative_ontology:cs_axiom('908b5273-9827-451a-99f4-9d4a749ada0d', foundational, social_model_of_disability_applies).
narrative_ontology:cs_axiom_status(social_model_of_disability_applies, holdable).
narrative_ontology:cs_axiom_grounding('908b5273-9827-451a-99f4-9d4a749ada0d', social_model_of_disability_applies, conventional).
narrative_ontology:cs_reference_frame('908b5273-9827-451a-99f4-9d4a749ada0d', pathologization_of_difference_framework).
narrative_ontology:cs_drift_state('908b5273-9827-451a-99f4-9d4a749ada0d', contemporary_neurodiversity_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('908b5273-9827-451a-99f4-9d4a749ada0d', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_of_conformity).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_profession).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, parents_of_neurodivergent_children).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct costs of pathologization, including stigma, medical interventions aimed at 'normalizing' behavior, and denial of accommodations. Their neurological identity is pathologized, making 'exit' from the diagnostic framework a challenge to their self-concept and access to support.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals, payer,
    powerless, biographical, identity_locked, universal).

% Benefit from the DSM's categories by having a standardized framework to classify, manage, and exclude individuals whose behaviors conflict with institutional norms (e.g., schools, employers, carceral systems). This allows them to avoid adapting environments to diverse needs.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_of_conformity, beneficiary,
    institutional, generational, constrained, global).

% Defines, updates, and administers the diagnostic categories, maintaining its authority and professional domain. It benefits from the perceived scientific legitimacy and the demand for its services in diagnosing and 'treating' conditions.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_profession, agenda_setter,
    institutional, generational, constrained, global).

% Critique the DSM's pathologizing framework, advocate for neurodiversity-affirming approaches, and work to shift societal understanding of neurological variation. They are outside the formal diagnostic process but actively resist its implications.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates, observer,
    organized, generational, analytical, global).

% Navigate the diagnostic system to access services and support for their children, often internalizing the pathologizing language. They bear emotional and financial costs, caught between seeking help within the existing framework and challenging its premises.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, parents_of_neurodivergent_children, payer,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_of_conformity).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__neurodiversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, internationally recognized nomenclature for mental health conditions, facilitating communication among clinicians, researchers, and policymakers.
% TRANSFER_FUNCTION: Transfers social and medical control over neurological differences from individuals and their communities to medical and institutional authorities, along with resources allocated based on diagnostic labels.
% ABSENT_VOICES: Neurodivergent individuals, particularly those with significant communication barriers or who reject the medical model of disability, are often excluded from the definitional processes and debates surrounding DSM categories. Their lived experiences are frequently interpreted through a pathologizing lens.
% DISAPPEARANCE_RATIONALE: If DSM categories vanished overnight, the entire infrastructure of psychiatric diagnosis, treatment, research funding, insurance coverage, and educational/workplace accommodations would collapse. Society would be forced to fundamentally rethink how it understands and responds to human neurological variation, leading to a radical reorganization of medical, social, and educational systems.
% FOUNDING_PROBLEM: To create a common, reliable, and valid system for classifying mental disorders, moving away from disparate and often idiosyncratic diagnostic practices.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric profession and many clinicians assert the problem of diagnostic reliability and validity remains live. Neurodiversity advocates and critical theorists argue that while diagnostic consistency was a goal, the current system primarily serves social control and pharmaceutical markets, citing sociological analyses and the persistent lack of biological markers for most DSM categories.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.85) because the act of pathologizing natural variation itself constitutes a significant harm and a denial of self-determination, leading to coercive interventions and denial of accommodation. Suppression (0.78) is substantial due to the institutional power wielded by the psychiatric profession and the societal pressure for conformity, which limits alternatives and exit options for neurodivergent individuals. Theater ratio (0.45) is moderate and increasing, reflecting a growing disconnect between the claimed scientific objectivity of the categories and their actual function in social control, as evidenced by ongoing critiques from neurodiversity advocates. Resistance (0.70) is significant, driven by organized neurodiversity movements.
 *
 * PERSPECTIVAL GAP:
 *   The psychiatric profession and institutional beneficiaries perceive the DSM as a necessary tool for identifying and treating illness, a rope or scaffold for coordination. Neurodivergent individuals and their advocates, however, experience it as a snare that enforces conformity and extracts compliance, pathologizing their very existence. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Neurodivergent individuals are the primary targets (payers), bearing the costs of pathologization and coercive normalization. Institutional systems (schools, employers, carceral systems) are beneficiaries, as the DSM provides a framework to manage and justify non-accommodation for individuals who don't fit their norms. The psychiatric profession acts as an agenda-setter, defining the categories and maintaining its authority, thus also benefiting. Neurodiversity advocates are observers, actively resisting the constraint's effects.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the DSM as a neutral 'information standard' (rope) by exposing the active enforcement, high extraction, and identifiable victims inherent in its operation from a neurodiversity perspective. It highlights how the original mandate of diagnostic clarity has drifted into a mechanism for social control and institutional convenience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsm_kernel_reading_identity,
    'Is this constraint accurately representing the ''neurodiversity reading'' of the DSM taxonomy kernel, or does it conflate with other critical perspectives?',
    'Comparison with canonical neurodiversity texts and advocacy positions; expert review by neurodiversity scholars.',
    'If conflated, the specific mechanisms of pathologization and social control might be obscured, leading to an inaccurate classification of the constraint''s extractive nature from this particular reading''s perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dsm_kernel_reading_identity, conceptual, 'Ensuring the fidelity of the neurodiversity reading.').

omega_variable(
    pathologization_vs_disability_support,
    'To what extent does the DSM''s pathologization of neurodivergence hinder access to necessary support and accommodations, versus providing a framework for it?',
    'Empirical studies on the impact of diagnosis on access to services and quality of life for neurodivergent individuals, comparing outcomes in medical vs. neurodiversity-affirming frameworks.',
    'If pathologization primarily hinders support, the extractiveness and suppression are higher. If it is a necessary (though flawed) gateway to support, the constraint might have a more complex, tangled-rope-like function, with lower net extraction for some seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathologization_vs_disability_support, empirical, 'Distinguishing between pathologization as harm and as a flawed gateway to support.').

omega_variable(
    social_vs_biological_etiology_ambiguity,
    'Are the ''disorders'' described in the DSM primarily social constructs reflecting societal norms, or are they inherent biological conditions?',
    'Ongoing scientific research into neurobiological markers for DSM conditions, alongside sociological and anthropological analyses of diagnostic trends and cultural influences.',
    'If primarily social constructs, the constraint''s ''naturalness'' claim is weaker, reinforcing its snare-like nature. If strong biological bases are found, the ''emerges_naturally'' aspect might increase, potentially shifting the classification towards a false summit mountain or a more legitimate rope, though still with high extraction if the ''treatment'' is coercive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_vs_biological_etiology_ambiguity, empirical, 'Ambiguity regarding the etiology of DSM-defined conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dsm__tr_t7, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 7, 0.35).
narrative_ontology:measurement(dsm__tr_t14, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 14, 0.4).
narrative_ontology:measurement(dsm__tr_t21, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 21, 0.42).
narrative_ontology:measurement(dsm__tr_t28, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 28, 0.43).
narrative_ontology:measurement(dsm__tr_t35, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 35, 0.44).
narrative_ontology:measurement(dsm__tr_t44, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 44, 0.45).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(dsm__be_t7, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 7, 0.75).
narrative_ontology:measurement(dsm__be_t14, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 14, 0.8).
narrative_ontology:measurement(dsm__be_t21, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 21, 0.82).
narrative_ontology:measurement(dsm__be_t28, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 28, 0.83).
narrative_ontology:measurement(dsm__be_t35, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 35, 0.84).
narrative_ontology:measurement(dsm__be_t44, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 44, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(dsm__su_t7, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 7, 0.68).
narrative_ontology:measurement(dsm__su_t14, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 14, 0.72).
narrative_ontology:measurement(dsm__su_t21, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 21, 0.75).
narrative_ontology:measurement(dsm__su_t28, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 28, 0.76).
narrative_ontology:measurement(dsm__su_t35, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 35, 0.77).
narrative_ontology:measurement(dsm__su_t44, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 44, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, mental_healthcare_funding_models).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, educational_accommodation_policies).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
