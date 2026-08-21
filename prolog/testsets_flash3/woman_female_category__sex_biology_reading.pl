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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Female Category Defined by Biological Sex (Sex-Biology Reading)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint defines 'woman' or 'female' based on chromosomal sex,
 *   reproductive anatomy, and developmental biology (XX/XY, gamete production
 *   capacity). It is one reading of the 'woman_female_category' kernel. This
 *   reading aims to secure sex-based protections for natal females, leading
 *   to the exclusion of transgender women from categories and spaces defined
 *   by this biological standard. The metrics reflect the active enforcement
 *   required to maintain this definition against competing claims and the
 *   significant extraction experienced by those excluded.
 *
 * KEY AGENTS:
 *   - natal_females_seeking_sex_based_protections: Beneficiary (organized/constrained)
 *   - transgender_women_seeking_female_category_inclusion: Payer (powerless/identity_locked)
 *   - institutions_mandated_to_accommodate_gender_identity: Payer (institutional/constrained)
 *   - gender_identity_advocates: Excluded (organized/constrained)
 *   - legal_scholars_and_judiciary: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.65).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.7).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Female Category Defined by Biological Sex (Sex-Biology Reading)").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, 'd0bfeeff-bad3-4890-8640-1734182fcf07').
narrative_ontology:cs_kernel_codification('d0bfeeff-bad3-4890-8640-1734182fcf07', formalized).
narrative_ontology:cs_authority_grounding('d0bfeeff-bad3-4890-8640-1734182fcf07', lineage).
narrative_ontology:cs_interpretation_layer_present('d0bfeeff-bad3-4890-8640-1734182fcf07').
narrative_ontology:cs_reading_relation('d0bfeeff-bad3-4890-8640-1734182fcf07', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('d0bfeeff-bad3-4890-8640-1734182fcf07', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('d0bfeeff-bad3-4890-8640-1734182fcf07', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('d0bfeeff-bad3-4890-8640-1734182fcf07', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('d0bfeeff-bad3-4890-8640-1734182fcf07', foundational, sex_based_rights_are_essential_for_females).
narrative_ontology:cs_axiom_status(sex_based_rights_are_essential_for_females, holdable).
narrative_ontology:cs_axiom_grounding('d0bfeeff-bad3-4890-8640-1734182fcf07', sex_based_rights_are_essential_for_females, deontological).
narrative_ontology:cs_reference_frame('d0bfeeff-bad3-4890-8640-1734182fcf07', biological_sex_as_foundational_category).
narrative_ontology:cs_drift_state('d0bfeeff-bad3-4890-8640-1734182fcf07', contemporary_gender_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d0bfeeff-bad3-4890-8640-1734182fcf07', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, transgender_women_seeking_female_category_inclusion).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, institutions_mandated_to_accommodate_gender_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the clarity and protective function of sex-based categories in areas like sports, prisons, and women's shelters, which they argue are necessary for safety, fairness, and privacy. They bear the cost of social friction and being labeled as exclusionary.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections, beneficiary,
    organized, generational, constrained, national).

% Are excluded from female-only spaces and categories based on biological sex, which they experience as discrimination and invalidation of their gender identity. Their identity is locked into seeking inclusion, making exit from the contest difficult.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, transgender_women_seeking_female_category_inclusion, payer,
    powerless, biographical, identity_locked, national).

% Face legal and social pressure to accommodate gender identity, but this reading of the category definition creates conflict and legal ambiguity for them, especially in single-sex provisions. They bear the cost of navigating conflicting mandates and potential litigation.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, institutions_mandated_to_accommodate_gender_identity, payer,
    institutional, immediate, constrained, local).

% Are structurally excluded from the definitional framework of this reading, as their core premise of gender identity as the primary determinant of category membership is rejected. They would argue for a broader, identity-inclusive definition.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, gender_identity_advocates, excluded,
    organized, generational, constrained, global).

% Analyze and adjudicate the legal implications of defining 'woman' or 'female' by biological sex, particularly in relation to anti-discrimination laws and human rights. They observe the societal impacts and legal challenges arising from this definition.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, legal_scholars_and_judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous definition of 'female' based on immutable biological characteristics, intended to coordinate protections and resources for a specific sex class.
% TRANSFER_FUNCTION: Transfers the right to define and access sex-segregated spaces and resources to natal females, while transferring the burden of exclusion and non-recognition to transgender women.
% ABSENT_VOICES: Gender identity advocates and many transgender individuals are absent from the definitional process of this reading; they would argue for self-identification as the primary determinant of category membership.
% DISAPPEARANCE_RATIONALE: If this biological definition of 'female' vanished, the legal and social landscape for sex-segregated spaces, sports, and medical care would be fundamentally altered, leading to a re-evaluation of who qualifies for specific protections and resources. The concept of sex-based rights would need to be entirely re-articulated.
% FOUNDING_PROBLEM: The need to define and protect a distinct biological sex class (female) based on reproductive capacity and associated vulnerabilities, ensuring specific rights and resources are allocated to this group.
% FOUNDING_PROBLEM_CORROBORATION: Natal female advocacy groups and some bioethicists corroborate that the problem of protecting biological sex-based categories remains live, citing ongoing concerns about fairness in sports, safety in single-sex spaces, and accurate data collection. Opposing groups contest this, arguing the problem is reframed to exclude transgender individuals.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is substantial because the definition imposes significant costs on transgender women by denying them access to female categories and spaces. Suppression (0.70) is high due to the active legal and social enforcement required to maintain this definition against challenges, including legislative efforts to codify sex-based definitions. Theater ratio is low (0.10) as the constraint's function is direct and actively pursued, not performative. Accessibility collapse is moderate (0.40) as alternatives (e.g., creating new categories) exist but are highly constrained by social and legal frameworks. Resistance (0.75) is high, reflecting the intense and ongoing contestation from gender identity advocates.
 *
 * PERSPECTIVAL GAP:
 *   Natal females experience this as a protective coordination mechanism, ensuring their rights and safety. Transgender women experience it as a snare, actively excluding them and invalidating their identity. Institutions face a tangled rope, caught between conflicting legal and social mandates.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal females are beneficiaries (d low) as the constraint directly serves their interest in sex-based protections. Transgender women are targets (d high) as they bear the direct cost of exclusion. Institutions are also targets (d high) due to the costs of navigating conflicting mandates. Gender identity advocates are excluded, their perspective actively suppressed by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (to define and protect a biological sex class) is actively contested and enforced. The classification as a Tangled Rope reflects the genuine coordination function for natal females alongside the asymmetric extraction from transgender women and the active enforcement required to maintain this balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_definition_scope_ambiguity,
    'Is the biological definition of ''female'' universally applicable across all social and legal contexts, or are there specific contexts where it is more or less relevant?',
    'Empirical analysis of outcomes in different contexts (e.g., sports, prisons, medical care, social recognition) under varying definitional regimes. Legal rulings on context-specific applications.',
    'If the definition is found to be context-dependent, the constraint''s scope would narrow, potentially reducing its extractiveness in certain areas. If universally applicable, its current extractiveness and suppression would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_definition_scope_ambiguity, empirical, 'Ambiguity regarding the universal vs. context-specific application of the biological definition of ''female''.').

omega_variable(
    exclusion_as_protection_conceptual_ambiguity,
    'Is the exclusion of transgender women from female categories a necessary protection for natal females, or is it a form of discrimination that could be resolved through alternative means?',
    'Conceptual analysis of ''protection'' vs. ''discrimination'' in human rights frameworks, alongside policy experiments with alternative accommodation models that do not rely on exclusion.',
    'If deemed necessary protection, the constraint''s coordination function for natal females is strengthened. If deemed discrimination, its extractiveness is amplified, and its coordination function is re-evaluated as a cover for harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exclusion_as_protection_conceptual_ambiguity, conceptual, 'Conceptual ambiguity regarding whether exclusion constitutes protection or discrimination.').

omega_variable(
    founding_problem_status_corroboration_ambiguity,
    'Is the founding problem (protection of biological sex class) genuinely live and requiring this specific definition, or has it been reframed to justify exclusion?',
    'Independent sociological and legal analysis of the historical evolution of sex-based protections and the contemporary challenges they face, distinguishing genuine threats from rhetorical reframing.',
    'If the problem is found to be reframed, the constraint''s justification weakens, potentially shifting its classification towards a Snare. If genuinely live, its Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_corroboration_ambiguity, empirical, 'Ambiguity regarding the true status and justification of the founding problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__sex_biology_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__sex_biology_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(woma_tr_t15, woman_female_category__sex_biology_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__sex_biology_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(woma_be_t5, woman_female_category__sex_biology_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(woma_be_t10, woman_female_category__sex_biology_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(woma_be_t15, woman_female_category__sex_biology_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(woma_be_t20, woman_female_category__sex_biology_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(woma_su_t5, woman_female_category__sex_biology_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(woma_su_t10, woman_female_category__sex_biology_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(woma_su_t15, woman_female_category__sex_biology_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(woma_su_t20, woman_female_category__sex_biology_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'woman_female_category' kernel. This 'sex_biology_reading' defines female category membership by biological sex. It is linked to the 'gender_identity_reading' and 'hybrid_contextual_reading' as competing interpretations of the same core concept.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
