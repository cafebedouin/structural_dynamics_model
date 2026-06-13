% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Female Category Defined by Biological Sex
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint defines membership in the category 'woman' or 'female'
 *   based on chromosomal sex (XX), reproductive anatomy, and developmental
 *   biology. It is a reading of the 'woman_female_category' kernel,
 *   emphasizing biological sex as the primary determinant. This definition is
 *   often invoked to protect sex-based rights and single-sex spaces, but it
 *   excludes transgender women and can complicate the status of intersex
 *   individuals. The constraint is claimed as a Tangled Rope because it
 *   provides a coordination function (clear categories) but also involves
 *   significant, actively enforced extraction from those who do not fit the
 *   strict biological criteria.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.6).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.7).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Female Category Defined by Biological Sex").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, 'f31e75b2-0885-4ce3-b7f1-bdbfdfae6420').
narrative_ontology:cs_kernel_codification('f31e75b2-0885-4ce3-b7f1-bdbfdfae6420', formalized).
narrative_ontology:cs_authority_grounding('f31e75b2-0885-4ce3-b7f1-bdbfdfae6420', lineage).
narrative_ontology:cs_interpretation_layer_present('f31e75b2-0885-4ce3-b7f1-bdbfdfae6420').
narrative_ontology:cs_reading_relation('f31e75b2-0885-4ce3-b7f1-bdbfdfae6420', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('f31e75b2-0885-4ce3-b7f1-bdbfdfae6420', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('f31e75b2-0885-4ce3-b7f1-bdbfdfae6420', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('f31e75b2-0885-4ce3-b7f1-bdbfdfae6420', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('f31e75b2-0885-4ce3-b7f1-bdbfdfae6420', foundational, sex_based_rights_are_essential).
narrative_ontology:cs_axiom_status(sex_based_rights_are_essential, holdable).
narrative_ontology:cs_axiom_grounding('f31e75b2-0885-4ce3-b7f1-bdbfdfae6420', sex_based_rights_are_essential, deontological).
narrative_ontology:cs_reference_frame('f31e75b2-0885-4ce3-b7f1-bdbfdfae6420', biological_sex_as_foundational_category).
narrative_ontology:cs_drift_state('f31e75b2-0885-4ce3-b7f1-bdbfdfae6420', contemporary_gender_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f31e75b2-0885-4ce3-b7f1-bdbfdfae6420', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, intersex_individuals_not_fitting_binary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the clarity and protective scope of sex-based categories, particularly in contexts like sports, prisons, and shelters, where physical safety and fairness are paramount. They advocate for this definition to maintain distinct spaces and rights.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections, beneficiary,
    organized, generational, constrained, global).

% Are excluded from female-only spaces and categories based on this definition, leading to social marginalization, legal challenges, and potential safety risks. Their identity as women is denied by this framework, forcing them into categories that do not align with their self-perception.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, transgender_women, payer,
    powerless, biographical, identity_locked, global).

% Are often miscategorized or excluded by strict binary definitions of sex, facing medical and social challenges in a system that does not recognize their biological diversity. Their existence challenges the simplicity of the XX/XY framework.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, intersex_individuals_not_fitting_binary, payer,
    powerless, biographical, trapped, global).

% Are responsible for codifying and enforcing legal definitions of sex and gender. They face pressure from various advocacy groups and must navigate complex ethical and social considerations, often leading to contested legislation.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, legislators_and_policymakers, agenda_setter,
    institutional, generational, constrained, national).

% Provide scientific and clinical input on biological sex, reproductive anatomy, and developmental biology. Their expertise is foundational to this reading, but they also grapple with the complexities of intersex conditions and gender identity in practice.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, medical_professionals, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, biologically verifiable basis for categorizing individuals into 'male' and 'female', which is used to coordinate medical treatment, sports categories, and single-sex spaces based on reproductive capacity and physical differences.
% TRANSFER_FUNCTION: Transfers exclusive access to certain categories and protections (e.g., women's sports, female-only changing rooms, specific medical care) to natal females, while denying that access to transgender women and some intersex individuals.
% ABSENT_VOICES: Advocates for gender self-identification and intersex rights groups are often excluded from the legislative and policy-making processes that enshrine this definition, or their perspectives are marginalized. They would argue for broader, more inclusive definitions.
% DISAPPEARANCE_RATIONALE: If this definition vanished, the legal and social frameworks for sex-segregated spaces, sports, and medical classifications would collapse, requiring a complete re-evaluation and re-establishment of categories based on new criteria. This would lead to significant social and legal upheaval.
% FOUNDING_PROBLEM: The need for clear, objective criteria to distinguish between sexes for biological, reproductive, and social purposes, particularly in contexts where sex-linked differences are relevant (e.g., reproduction, physical strength, safety in vulnerable spaces).
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by natal females seeking sex-based protections, who cite ongoing concerns about fairness in sports, safety in single-sex spaces, and the erosion of sex-specific language. Medical professionals corroborate the biological basis for sex differences, though they also acknowledge the complexity of sex development. Critics (transgender advocates, intersex advocates) contest the scope and application of this problem, arguing it is used to justify exclusion rather than genuine protection.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.6) because the definition imposes significant costs on transgender women and intersex individuals by denying them access to categories and spaces aligned with their identity or lived experience. Suppression is also high (0.7) due to active legal and social enforcement mechanisms that uphold this definition, often through legislation or policy that explicitly excludes non-biologically female individuals from female categories. Theater ratio is low (0.1) as the constraint's function is largely direct and its enforcement is not primarily performative; it genuinely aims to define and protect a biological category.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of natal females, this constraint is a necessary Rope or even a Mountain, providing essential coordination and protection based on immutable biological facts. From the perspective of transgender women and intersex individuals, it operates as a Snare, enforcing exclusion and denying identity through a biologically reductionist framework. The engine's classification as Tangled Rope reflects this inherent tension between coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal females seeking sex-based protections are beneficiaries (d near 0.0) as they gain exclusive access and protections. Transgender women and intersex individuals are victims/targets (d near 1.0) as they bear the costs of exclusion and miscategorization. Legislators and policymakers act as agenda-setters, mediating between competing claims. Medical professionals provide the scientific grounding but are not direct beneficiaries or victims of the social application of the definition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_determinism_vs_identity,
    'Is biological sex an exhaustive and sufficient determinant for social and legal categories of ''woman''/''female'', or does gender identity offer a distinct and equally valid basis?',
    'Sociological and psychological research on the lived experience and social function of gender identity, alongside legal precedents that recognize gender identity as a protected characteristic.',
    'If gender identity is recognized as an equally valid basis, the extractiveness of this constraint would be re-evaluated downward, and its suppression mechanisms would be seen as unjust. If biological determinism is upheld, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_determinism_vs_identity, conceptual, 'The conceptual boundary between biological sex and gender identity in defining social categories.').

omega_variable(
    intersex_inclusion_ambiguity,
    'How does this binary biological definition accommodate the diversity of intersex conditions, and what are the implications for their inclusion in sex-based categories?',
    'Medical and legal frameworks that develop nuanced approaches to intersex inclusion, moving beyond strict XX/XY binaries for social categorization.',
    'If intersex conditions are systematically excluded or miscategorized, the constraint''s extractiveness and suppression would be amplified for this group. If inclusive frameworks emerge, the constraint''s rigidity would be challenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_inclusion_ambiguity, empirical, 'The capacity of a binary sex definition to accommodate intersex diversity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t1950, woman_female_category__sex_biology_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(woma_tr_t1970, woman_female_category__sex_biology_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(woma_tr_t1990, woman_female_category__sex_biology_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(woma_tr_t2010, woman_female_category__sex_biology_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(woma_tr_t2024, woman_female_category__sex_biology_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(woma_be_t1950, woman_female_category__sex_biology_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(woma_be_t1970, woman_female_category__sex_biology_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(woma_be_t1990, woman_female_category__sex_biology_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(woma_be_t2010, woman_female_category__sex_biology_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(woma_be_t2024, woman_female_category__sex_biology_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t1950, woman_female_category__sex_biology_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(woma_su_t1970, woman_female_category__sex_biology_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(woma_su_t1990, woman_female_category__sex_biology_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(woma_su_t2010, woman_female_category__sex_biology_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(woma_su_t2024, woman_female_category__sex_biology_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
