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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: DSM Taxonomy as Biomedical Disease Entities (Biomedical Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint represents the 'biomedical reading' of the DSM
 *   (Diagnostic and Statistical Manual of Mental Disorders) taxonomy, which
 *   asserts that its categories correspond to objective neurobiological
 *   disease entities discoverable through empirical research. This reading
 *   underpins the medical model of mental illness, justifying pharmaceutical
 *   and other biological interventions. It is a Snare because it extracts
 *   from diagnosed individuals (through involuntary treatment, loss of
 *   autonomy, and financial burden) while benefiting the psychiatric
 *   establishment and pharmaceutical industry, with alternatives suppressed
 *   by the authority of medical science.
 *
 * KEY AGENTS:
 *   - psychiatric_establishment: Agenda setter (institutional/arbitrage) — defines categories, legitimizes interventions.
 *   - pharmaceutical_industry: Beneficiary (institutional/arbitrage) — profits from treatments aligned with categories.
 *   - institutions_requiring_conformity: Beneficiary (institutional/constrained) — uses diagnoses to manage non-conforming behavior.
 *   - diagnosed_individuals: Payer (powerless/trapped) — bears the direct costs of diagnosis and treatment, loss of autonomy.
 *   - families_of_diagnosed: Payer (moderate/constrained) — bears caregiving burden, financial costs, and social stigma.
 *   - neurodiversity_advocates: Excluded (organized/constrained) — challenge the pathologizing framework, advocate for alternative models.
 *   - critical_psychiatrists: Observer (analytical/analytical) — analyze the social and economic functions of the DSM, often from an external perspective.
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
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Taxonomy as Biomedical Disease Entities (Biomedical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '6c73d7bf-ff5a-417a-b532-881e09caa274').
narrative_ontology:cs_kernel_codification('6c73d7bf-ff5a-417a-b532-881e09caa274', formalized).
narrative_ontology:cs_authority_grounding('6c73d7bf-ff5a-417a-b532-881e09caa274', expertise).
narrative_ontology:cs_interpretation_layer_present('6c73d7bf-ff5a-417a-b532-881e09caa274').
narrative_ontology:cs_reading_relation('6c73d7bf-ff5a-417a-b532-881e09caa274', dsm_taxonomy_kernel__neurodiversity_reading, forecloses).
narrative_ontology:cs_reading_relation('6c73d7bf-ff5a-417a-b532-881e09caa274', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('6c73d7bf-ff5a-417a-b532-881e09caa274', foundational, mental_disorders_are_brain_diseases).
narrative_ontology:cs_axiom_status(mental_disorders_are_brain_diseases, holdable).
narrative_ontology:cs_axiom_grounding('6c73d7bf-ff5a-417a-b532-881e09caa274', mental_disorders_are_brain_diseases, empirically_contingent).
narrative_ontology:cs_axiom('6c73d7bf-ff5a-417a-b532-881e09caa274', foundational, dsm_categories_reflect_objective_reality).
narrative_ontology:cs_axiom_status(dsm_categories_reflect_objective_reality, holdable).
narrative_ontology:cs_axiom_grounding('6c73d7bf-ff5a-417a-b532-881e09caa274', dsm_categories_reflect_objective_reality, empirically_contingent).
narrative_ontology:cs_reference_frame('6c73d7bf-ff5a-417a-b532-881e09caa274', scientific_medical_model).
narrative_ontology:cs_drift_state('6c73d7bf-ff5a-417a-b532-881e09caa274', contemporary_empirical_challenges, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6c73d7bf-ff5a-417a-b532-881e09caa274', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_conformity).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, diagnosed_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, families_of_diagnosed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized nomenclature and diagnostic criteria for mental health professionals, facilitating communication, research, and treatment planning across different clinical settings.
% TRANSFER_FUNCTION: Transfers authority over defining and treating mental distress from individuals and communities to medical professionals, and financial resources from healthcare systems and individuals to the pharmaceutical industry and psychiatric services.
% ABSENT_VOICES: Individuals with lived experience of psychiatric diagnosis who reject the biomedical model, indigenous healing traditions, and alternative therapeutic approaches are largely excluded from the DSM's revision process; they would argue for de-medicalization and culturally sensitive, non-pathologizing frameworks.
% DISAPPEARANCE_RATIONALE: If the DSM taxonomy vanished overnight, the entire structure of psychiatric care, pharmaceutical development, insurance coverage for mental health, and legal frameworks for mental illness would collapse and reorganize. There would be immense confusion initially, followed by a proliferation of alternative diagnostic and therapeutic models, and a fundamental shift in how societies understand and respond to mental distress.
% FOUNDING_PROBLEM: The founding problem was the lack of a consistent, shared language for describing and classifying mental disorders, leading to diagnostic chaos and hindering research and effective treatment.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment attests the problem is still live, citing ongoing diagnostic challenges and the need for scientific rigor. Neurodiversity advocates and critical psychiatrists attest that while a common language is useful, the current taxonomy's biomedical framing has created new problems (pathologization, over-medicalization) that overshadow the original coordination benefit; independent sociological and historical analyses corroborate the shift in function.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).

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
 *   The high extractiveness (0.85) reflects the significant costs borne by diagnosed individuals, including financial, social, and autonomy costs, often without clear evidence of 'cure'. Suppression (0.90) is severe due to the power of medical authority, involuntary commitment laws, and the lack of recognized alternatives within the dominant paradigm. The low theater ratio (0.20) indicates that while there is some performative aspect to maintaining the 'disease' narrative, the core function of diagnosis and treatment is actively pursued and enforced, rather than merely theatrical. The rising extractiveness and suppression over time reflect the increasing medicalization of distress and the expansion of diagnostic categories.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the psychiatric establishment and pharmaceutical industry, this constraint is a necessary framework for treating illness and improving public health (a claimed Rope or even Mountain). From the perspective of diagnosed individuals and their advocates, it is a system that pathologizes normal human variation, limits autonomy, and extracts resources (a Snare). The engine's classification as Snare reflects the structural reality of asymmetric extraction and suppression, regardless of the claimed coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   The psychiatric establishment and pharmaceutical industry are clear beneficiaries (d=0.0-0.1) as they define the categories and profit from the interventions. Institutions requiring conformity also benefit from a framework that labels non-conforming behavior as 'illness'. Diagnosed individuals are the primary targets (d=0.9-1.0) due to the direct impact on their lives, often with limited exit options (trapped/identity_locked). Families bear secondary costs. Neurodiversity advocates and critical psychiatrists are excluded or analytical observers, with high d values reflecting their opposition to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Snare because its primary function has shifted from genuinely coordinating care for severe mental illness to pathologizing a broader range of human experiences, creating markets for interventions, and enforcing social conformity. The 'coordination' story of providing a common language for clinicians serves as cover for the extractive and suppressive functions. The classification prevents mislabeling this as a Rope by highlighting the asymmetric costs and suppressed alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biomedical_vs_social_construction,
    'Is this constraint a reflection of objective neurobiological reality, or a social construct that benefits identifiable actors?',
    'Longitudinal empirical research demonstrating consistent, specific neurobiological markers for each DSM category, independent of cultural context or pharmaceutical intervention.',
    'If objective, the constraint moves towards a Mountain; if socially constructed, it remains a Snare, with higher effective extraction due to the false naturalization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biomedical_vs_social_construction, empirical, 'Ambiguity between natural law and social construct for DSM categories.').

omega_variable(
    dsm_reading_identification,
    'This constraint is the ''biomedical_reading'' of the ''dsm_taxonomy_kernel''. What would change if the ''neurodiversity_reading'' or ''critical_psychiatry_reading'' were adopted?',
    'Analysis of policy and clinical practice shifts in jurisdictions adopting alternative frameworks.',
    'The neurodiversity reading would shift the victim set from ''diagnosed_individuals'' to ''individuals_experiencing_distress_due_to_mismatch_with_norms'', and reduce extractiveness by reframing ''treatment'' as ''support''. The critical psychiatry reading would expose the pharmaceutical industry as the primary agenda_setter and increase measured extractiveness by revealing the market-creation function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dsm_reading_identification, conceptual, 'Impact of alternative readings of the DSM taxonomy kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (involuntary treatment, legal capacity loss) or internalized (self-pathologization, belief in inherent defect)?',
    'Post-diagnosis trajectory of individuals in supportive, non-coercive environments: if self-pathologization persists after structural coercion is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the diagnosed individual carries the suppression with them after formal exit or remission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in psychiatric diagnosis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dsm__tr_t10, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(dsm__tr_t20, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(dsm__be_t10, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(dsm__be_t20, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(dsm__su_t10, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(dsm__su_t20, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, information_standard).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, mental_health_insurance_coverage).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_research_funding).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, involuntary_commitment_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dsm_taxonomy_kernel'. Its extractiveness differs significantly from the 'neurodiversity_reading' and 'critical_psychiatry_reading' due to differing views on the ontological status and function of diagnostic categories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
