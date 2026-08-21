% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__biomedical_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint represents the biomedical reading of the DSM taxonomy,
 *   asserting that its categories correspond to objective neurobiological
 *   disease entities. This reading underpins the authority of the psychiatric
 *   establishment and the pharmaceutical industry, enabling significant
 *   extraction from diagnosed individuals. The constraint is claimed as a
 *   'tangled_rope' because it does provide a coordination function
 *   (standardized communication) but is characterized by high extraction and
 *   active enforcement to maintain its dominance against alternative
 *   readings. This is one reading of the 'dsm_taxonomy_kernel', with sibling
 *   readings from critical psychiatry and neurodiversity perspectives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.85).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.9).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '6d277a52-04a9-4d66-b137-7112873ac276').
narrative_ontology:cs_kernel_codification('6d277a52-04a9-4d66-b137-7112873ac276', formalized).
narrative_ontology:cs_authority_grounding('6d277a52-04a9-4d66-b137-7112873ac276', expertise).
narrative_ontology:cs_interpretation_layer_present('6d277a52-04a9-4d66-b137-7112873ac276').
narrative_ontology:cs_reading_relation('6d277a52-04a9-4d66-b137-7112873ac276', dsm_taxonomy_kernel__neurodiversity_reading, forecloses).
narrative_ontology:cs_reading_relation('6d277a52-04a9-4d66-b137-7112873ac276', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('6d277a52-04a9-4d66-b137-7112873ac276', foundational, mental_illness_is_brain_disease).
narrative_ontology:cs_axiom_status(mental_illness_is_brain_disease, holdable).
narrative_ontology:cs_axiom_grounding('6d277a52-04a9-4d66-b137-7112873ac276', mental_illness_is_brain_disease, empirically_contingent).
narrative_ontology:cs_axiom('6d277a52-04a9-4d66-b137-7112873ac276', foundational, dsm_categories_reflect_objective_reality).
narrative_ontology:cs_axiom_status(dsm_categories_reflect_objective_reality, holdable).
narrative_ontology:cs_axiom_grounding('6d277a52-04a9-4d66-b137-7112873ac276', dsm_categories_reflect_objective_reality, empirically_contingent).
narrative_ontology:cs_reference_frame('6d277a52-04a9-4d66-b137-7112873ac276', scientific_medical_model).
narrative_ontology:cs_drift_state('6d277a52-04a9-4d66-b137-7112873ac276', contemporary_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6d277a52-04a9-4d66-b137-7112873ac276', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, diagnosed_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, families_of_diagnosed_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, promulgates, and enforces the DSM diagnostic criteria. Benefits from the authority and funding associated with identifying and treating 'diseases'. Their professional identity and institutional power are deeply tied to the biomedical model of mental illness.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, constrained, global).

% Develops and markets drugs for DSM-defined conditions. Benefits directly from the expansion of diagnostic categories and the medicalization of distress, creating vast markets for psychotropic medications.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Schools, workplaces, and legal systems that use DSM diagnoses to manage, exclude, or modify individuals whose behavior deviates from norms. Benefits from a medicalized framework that justifies interventions and maintains social order.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity, beneficiary,
    organized, biographical, mobile, national).

% Are labeled with disease entities, which can lead to involuntary treatment, loss of legal capacity, social stigma, and lifelong reliance on medication. Their identity often becomes fused with their diagnosis, making exit from the medical framework extremely difficult.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, diagnosed_individuals, payer,
    powerless, biographical, identity_locked, local).

% Bear the emotional, financial, and social costs of managing a family member's DSM diagnosis. They often internalize the biomedical framework, seeking medical solutions and enforcing compliance, which can constrain their own choices and resources.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, families_of_diagnosed_individuals, payer,
    moderate, generational, constrained, local).

% Academics and practitioners who challenge the biomedical model, arguing that DSM categories are social constructs. They are often marginalized within mainstream psychiatry, their research underfunded, and their perspectives excluded from diagnostic manual revisions.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatrists, excluded,
    moderate, generational, constrained, global).

% Advocate for recognizing neurological variations as natural human diversity rather than pathology. They are excluded from the core diagnostic process and their perspectives are often dismissed as anti-science by the biomedical establishment.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized language and framework for clinicians, researchers, and insurance companies to communicate about mental distress, facilitating diagnosis, treatment planning, and research funding based on a shared understanding of 'disease'.
% TRANSFER_FUNCTION: Transfers authority, resources, and legitimacy to the psychiatric establishment and pharmaceutical industry, while transferring diagnostic labels, treatment mandates, and social control to individuals exhibiting non-conforming behaviors.
% ABSENT_VOICES: Critical psychiatrists and neurodiversity advocates are largely excluded from the DSM's core definitional processes; they would argue for a more nuanced, less pathologizing, and less medicalized understanding of mental distress and neurological variation.
% DISAPPEARANCE_RATIONALE: If the DSM taxonomy vanished overnight, the entire edifice of psychiatric diagnosis, pharmaceutical treatment, insurance billing, and legal frameworks for mental health would collapse. Clinical practice would become highly fragmented, research funding would be disrupted, and the social control mechanisms tied to diagnosis would cease to function, forcing a radical reorganization of how society addresses mental distress.
% FOUNDING_PROBLEM: To provide a common, reliable nomenclature for mental disorders, moving beyond idiosyncratic clinical descriptions and facilitating empirical research into etiology and treatment.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment attests the problem is still live, citing ongoing need for diagnostic reliability and research. Critical psychiatrists and neurodiversity advocates attest the founding problem has been superseded by the construction of disease entities that serve institutional and commercial interests, with corroboration from sociological studies of medicalization and patient advocacy groups.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the biomedical framing enables extensive medical interventions, often lifelong, with significant financial and social costs for individuals. Suppression (0.90) is also very high, as the medical model actively marginalizes and discredits alternative explanations for distress, enforcing conformity through diagnostic power and treatment mandates. The theater ratio (0.20) is relatively low, as the system genuinely performs its diagnostic and treatment functions, even if the underlying assumptions are contested. Accessibility collapse is high (0.75) because once an individual is diagnosed within this framework, alternative understandings or exits become very difficult to access or legitimize. Resistance is moderate (0.40) from patient advocacy and critical academic movements, but this resistance is largely external to the core operation of the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the psychiatric establishment, the DSM is a necessary scientific tool for identifying and treating disease, a 'rope' that coordinates care. From the perspective of diagnosed individuals, it can feel like a 'snare' that traps them in a medicalized identity with limited exit options. The engine's classification will reflect this divergence based on the structural data provided for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The psychiatric establishment, pharmaceutical industry, and institutions requiring behavioral conformity are clear beneficiaries (low d), gaining authority, market share, and social control. Diagnosed individuals and their families are the primary targets (high d), bearing the costs of medicalization, stigma, and loss of autonomy. Critical psychiatrists and neurodiversity advocates are excluded, actively suppressed from influencing the core diagnostic process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_corroboration_of_disease_entities,
    'To what extent do DSM categories map to objectively verifiable neurobiological disease entities, as opposed to symptom clusters or social constructs?',
    'Longitudinal empirical research identifying specific, consistent biomarkers or genetic markers for each DSM category, independent of symptom presentation.',
    'Strong empirical corroboration would strengthen the biomedical reading, potentially shifting its classification closer to a ''mountain'' for the psychiatric establishment. Lack of corroboration would weaken its scientific legitimacy, supporting alternative readings and potentially reclassifying it as a ''snare'' or ''tangled_rope'' for all seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_corroboration_of_disease_entities, empirical, 'The degree to which DSM categories reflect objective biological reality.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., legal mandates for treatment, insurance requirements) or internalized (e.g., individuals internalizing their diagnosis and self-policing behavior)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., self-stigma, identity fusion) after structural barriers are removed (e.g., legal changes, insurance reform), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient and extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in psychiatric diagnosis.').

omega_variable(
    framing_underdetermination_biomedical_vs_critical,
    'Is the DSM taxonomy fundamentally a scientific endeavor to classify disease (biomedical reading) or a socio-economic mechanism to create markets for pharmaceuticals (critical psychiatry reading)?',
    'Analysis of funding sources for diagnostic research, historical evolution of diagnostic criteria in relation to drug development cycles, and the impact of pharmaceutical marketing on diagnostic prevalence.',
    'If the critical psychiatry framing is adopted, the constraint''s extractiveness and suppression would be re-evaluated as higher, and its coordination function would be seen as a cover for pure extraction, likely reclassifying it as a ''snare'' for all seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_biomedical_vs_critical, conceptual, 'Alternative framings of the DSM taxonomy as either scientific or market-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dsm__tr_t10, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(dsm__tr_t20, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(dsm__tr_t30, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(dsm__tr_t50, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(dsm__tr_t60, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(dsm__be_t10, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(dsm__be_t20, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(dsm__be_t30, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(dsm__be_t50, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement(dsm__be_t60, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 60, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(dsm__su_t10, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(dsm__su_t20, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(dsm__su_t30, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(dsm__su_t50, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 50, 0.9).
narrative_ontology:measurement(dsm__su_t60, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 60, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
