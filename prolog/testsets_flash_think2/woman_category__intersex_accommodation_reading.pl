% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Category of 'Woman' with Intersex Accommodation
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'intersex_accommodation_reading'
 *   of the 'woman_category' kernel. It describes the category of 'woman' as
 *   acknowledging biological sex as a non-binary spectrum, including typical
 *   female biology plus intersex variations that do not fit a male category.
 *   From this reading's perspective, the standing arrangement (the
 *   historically binary definition of 'woman') is a Tangled Rope: it serves a
 *   coordination function for social organization but imposes significant,
 *   asymmetric extraction on intersex individuals through non-accommodation
 *   and miscategorization. The metrics reflect the impact of this
 *   non-accommodation, particularly in high-stakes domains like elite sports.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.65).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.75).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Category of 'Woman' with Intersex Accommodation").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '6cf6abc8-a7a5-4d19-ba76-925d89a65bc1').
narrative_ontology:cs_kernel_codification('6cf6abc8-a7a5-4d19-ba76-925d89a65bc1', formalized).
narrative_ontology:cs_authority_grounding('6cf6abc8-a7a5-4d19-ba76-925d89a65bc1', practice).
narrative_ontology:cs_interpretation_layer_present('6cf6abc8-a7a5-4d19-ba76-925d89a65bc1').
narrative_ontology:cs_reading_relation('6cf6abc8-a7a5-4d19-ba76-925d89a65bc1', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cf6abc8-a7a5-4d19-ba76-925d89a65bc1', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('6cf6abc8-a7a5-4d19-ba76-925d89a65bc1', foundational, biological_sex_is_a_spectrum).
narrative_ontology:cs_axiom_status(biological_sex_is_a_spectrum, holdable).
narrative_ontology:cs_axiom_grounding('6cf6abc8-a7a5-4d19-ba76-925d89a65bc1', biological_sex_is_a_spectrum, empirically_contingent).
narrative_ontology:cs_axiom('6cf6abc8-a7a5-4d19-ba76-925d89a65bc1', foundational, categories_must_accommodate_biological_diversity).
narrative_ontology:cs_axiom_status(categories_must_accommodate_biological_diversity, holdable).
narrative_ontology:cs_axiom_grounding('6cf6abc8-a7a5-4d19-ba76-925d89a65bc1', categories_must_accommodate_biological_diversity, deontological).
narrative_ontology:cs_reference_frame('6cf6abc8-a7a5-4d19-ba76-925d89a65bc1', binary_sex_categories_with_exceptions).
narrative_ontology:cs_drift_state('6cf6abc8-a7a5-4d19-ba76-925d89a65bc1', contemporary_bioethical_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6cf6abc8-a7a5-4d19-ba76-925d89a65bc1', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, typical_females).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, binary_gender_system_maintainers).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_individuals).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, inclusive_feminism_doctrine).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, biological_diversity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose biological sex characteristics (chromosomes, gonads, hormones, anatomy) do not fit typical binary definitions of male or female. They bear the costs of non-accommodation, including miscategorization, medical pathologization, social exclusion, and denial of rights or access based on rigid binary categories. Their identity is intrinsically tied to their biological reality, making 'exit' from their intersex status impossible.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_individuals, payer,
    powerless, biographical, identity_locked, global).

% Individuals whose biological sex aligns with typical female characteristics. They benefit from the clarity and social recognition of the 'woman' category, even if the binary definition is imperfect. While not directly harmed by the binary nature, their social identity is deeply tied to the category, making a fundamental shift in its definition a complex prospect.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, typical_females, beneficiary,
    moderate, biographical, constrained, global).

% Institutions (medical, legal, governmental, social) and individuals who uphold and enforce binary sex/gender categories. They benefit from the simplicity and historical precedent of these categories. Changing these deeply entrenched systems involves significant institutional inertia and social resistance, constraining their options for rapid reform.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, binary_gender_system_maintainers, agenda_setter,
    institutional, generational, constrained, global).

% Organizations and individuals working to raise awareness about intersex variations, challenge binary sex/gender norms, and advocate for the rights and accommodation of intersex individuals. They analyze the structural harms of non-accommodation and propose policy and social changes.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_advocates, observer,
    organized, generational, analytical, global).

% Organizations responsible for setting rules for sex-segregated sports categories. They face pressure to ensure fair competition while also accommodating athletes with intersex variations. Their decisions in cases like Caster Semenya highlight the high stakes and significant extraction (e.g., exclusion from competition) that can occur when accommodation is not adequately provided.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, elite_sports_governing_bodies, agenda_setter,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To define and categorize human populations, specifically the category of 'woman', for social, legal, and biological purposes, aiming to include typical female biology alongside intersex variations that do not fit a male category.
% TRANSFER_FUNCTION: The constraint, as currently operating, transfers social recognition, access to sex-segregated spaces, and identity validation to those fitting binary definitions, while denying or complicating it for intersex individuals. This reading seeks to re-allocate these benefits more inclusively.
% ABSENT_VOICES: Historically, intersex individuals and their advocates have been marginalized or excluded from the medical, legal, and social processes that define sex and gender categories. Their perspectives on the lived experience of non-accommodation were largely absent from foundational discussions.
% DISAPPEARANCE_RATIONALE: The social and legal categories of 'woman' are foundational to many societal structures, including legal rights, healthcare, social spaces, and identity. If this understanding (even with intersex accommodation) vanished, legal frameworks, social norms, and individual identity structures would need fundamental re-evaluation and reorganization.
% FOUNDING_PROBLEM: To define and categorize human populations for social, legal, and reproductive purposes, historically based on observable sex characteristics, often simplifying complex biological realities into a binary.
% FOUNDING_PROBLEM_CORROBORATION: Biologists and medical professionals attest to the complexity and spectrum of human sex development, corroborating the inadequacy of rigid binary definitions. Social scientists and human rights advocates corroborate the historical exclusion and harm caused by such categories, supporting the need for accommodation. Binary system maintainers often contest the extent of the problem or the necessity of fundamental redefinition.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial, reflecting the social, psychological, and sometimes physical costs borne by intersex individuals due to the failure of binary categories to accommodate their reality. This is particularly acute in contexts like elite sports, where exclusion can be absolute. Suppression (0.75) is high due to the active enforcement of binary categories in medical, legal, and social systems, often pathologizing intersex variations rather than accommodating them. Theater ratio (0.40) indicates a degree of performative adherence to binary norms despite growing scientific understanding of sex as a spectrum. Accessibility collapse (0.70) is high as recognized non-binary categories are difficult to access, and resistance (0.60) is significant from intersex advocacy groups. The claimed type is Tangled Rope because the category of 'woman' serves a genuine coordination function, but its current (from this reading's perspective) operation involves substantial, asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'typical_females' and 'binary_gender_system_maintainers', the category of 'woman' might appear as a Rope or even a Mountain, providing stable social order with minimal perceived cost. However, from the 'intersex_individuals' seat, the same category operates as a Snare or Tangled Rope, imposing significant costs and requiring active suppression of their biological reality. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Intersex individuals are the primary targets (payers) of this constraint, bearing the costs of non-accommodation and miscategorization (high d). Typical females are beneficiaries, gaining from the clarity of the category, even if imperfect (low d). Binary gender system maintainers and elite sports governing bodies are agenda-setters, enforcing the existing categories and benefiting from their perceived stability (low d). Intersex advocates are observers, working to shift the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_vs_social_construction_of_sex,
    'To what extent is biological sex a purely objective, binary reality versus a concept influenced by social and cultural interpretations of biological variation?',
    'Further interdisciplinary research integrating biology, anthropology, and sociology to delineate the interplay of biological facts and social constructs in defining sex categories.',
    'If sex is found to be more socially constructed than currently acknowledged, the justification for rigid binary categories weakens, potentially leading to reclassification towards a Snare or Tangled Rope for the binary readings. If it''s found to be more objectively binary, the pressure for accommodation might be reframed as a social preference rather than a biological imperative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_vs_social_construction_of_sex, conceptual, 'Ambiguity regarding the foundational nature of sex categories.').

omega_variable(
    impact_of_accommodation_on_sex_segregated_spaces,
    'What are the empirical impacts of accommodating intersex individuals within sex-segregated categories (e.g., women''s sports, single-sex spaces) on the integrity, fairness, or safety of those spaces?',
    'Longitudinal studies and empirical data collection from jurisdictions or organizations that have implemented intersex-inclusive policies in sex-segregated contexts.',
    'Demonstrable negative impacts could lead to a re-evaluation of the scope or nature of accommodation, potentially increasing the perceived ''cost'' of this reading. Minimal or positive impacts would strengthen the case for broader accommodation and reduce perceived extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_of_accommodation_on_sex_segregated_spaces, empirical, 'Empirical consequences of intersex accommodation in sex-segregated contexts.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by intersex individuals primarily structural (e.g., legal non-recognition, medical protocols) or internalized (e.g., shame, self-misidentification due to societal norms)?',
    'Qualitative sociological research and post-policy-change studies: if suppression persists after structural barriers are removed, it indicates a significant internalized component.',
    'If internalized suppression is a major factor, the effective suppression is higher than structural measures suggest, and interventions would need to address both structural and psychological dimensions. If primarily structural, policy changes alone would be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for intersex individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t1950, woman_category__intersex_accommodation_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(woma_tr_t1965, woman_category__intersex_accommodation_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(woma_tr_t1980, woman_category__intersex_accommodation_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(woma_tr_t1995, woman_category__intersex_accommodation_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(woma_tr_t2010, woman_category__intersex_accommodation_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(woma_tr_t2025, woman_category__intersex_accommodation_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(woma_be_t1950, woman_category__intersex_accommodation_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(woma_be_t1965, woman_category__intersex_accommodation_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(woma_be_t1980, woman_category__intersex_accommodation_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(woma_be_t1995, woman_category__intersex_accommodation_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(woma_be_t2010, woman_category__intersex_accommodation_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(woma_be_t2025, woman_category__intersex_accommodation_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t1950, woman_category__intersex_accommodation_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(woma_su_t1965, woman_category__intersex_accommodation_reading, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(woma_su_t1980, woman_category__intersex_accommodation_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(woma_su_t1995, woman_category__intersex_accommodation_reading, suppression_requirement, 1995, 0.72).
narrative_ontology:measurement(woma_su_t2010, woman_category__intersex_accommodation_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(woma_su_t2025, woman_category__intersex_accommodation_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'woman_category' kernel, each offering a distinct structural interpretation of the category's definition and implications. This reading focuses on biological sex as a spectrum, accommodating intersex variations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
