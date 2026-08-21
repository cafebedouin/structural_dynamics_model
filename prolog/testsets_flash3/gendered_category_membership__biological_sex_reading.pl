% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__biological_sex_reading, []).

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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Gendered Category Membership (Biological Sex Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint defines gendered category membership based on immutable
 *   biological markers (chromosomes, reproductive anatomy at birth). It is
 *   one reading of the broader 'gendered_category_membership' kernel. This
 *   reading structurally excludes transgender women and intersex individuals
 *   from categories aligned with their gender identity, while positioning
 *   cisgender women and biological essentialists as beneficiaries of category
 *   preservation. The constraint is actively enforced through social, legal,
 *   and political means, leading to high suppression and extractiveness for
 *   those who do not conform to the binary biological definition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.78).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.85).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, snare).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Gendered Category Membership (Biological Sex Reading)").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, '101467d2-ea62-4c83-84d1-f6269c4fcdc3').
narrative_ontology:cs_kernel_codification('101467d2-ea62-4c83-84d1-f6269c4fcdc3', implicit).
narrative_ontology:cs_authority_grounding('101467d2-ea62-4c83-84d1-f6269c4fcdc3', practice).
narrative_ontology:cs_interpretation_layer_present('101467d2-ea62-4c83-84d1-f6269c4fcdc3').
narrative_ontology:cs_reading_relation('101467d2-ea62-4c83-84d1-f6269c4fcdc3', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('101467d2-ea62-4c83-84d1-f6269c4fcdc3', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('101467d2-ea62-4c83-84d1-f6269c4fcdc3', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('101467d2-ea62-4c83-84d1-f6269c4fcdc3', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('101467d2-ea62-4c83-84d1-f6269c4fcdc3', foundational, gender_follows_sex).
narrative_ontology:cs_axiom_status(gender_follows_sex, holdable).
narrative_ontology:cs_axiom_grounding('101467d2-ea62-4c83-84d1-f6269c4fcdc3', gender_follows_sex, conventional).
narrative_ontology:cs_reference_frame('101467d2-ea62-4c83-84d1-f6269c4fcdc3', traditional_biological_binary).
narrative_ontology:cs_drift_state('101467d2-ea62-4c83-84d1-f6269c4fcdc3', contemporary_gender_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('101467d2-ea62-4c83-84d1-f6269c4fcdc3', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cisgender_women_advocates).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, biological_essentialists).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, transgender_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, intersex_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for the definition of 'woman' based exclusively on biological sex, aiming to preserve sex-segregated spaces and categories for cisgender women. They actively enforce this boundary through discourse, policy proposals, and social pressure, viewing it as essential for women's rights and safety.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cisgender_women_advocates, agenda_setter,
    organized, generational, constrained, national).

% Academics, policymakers, and cultural figures who benefit from a clear, immutable biological definition of sex and gender categories, as it aligns with their theoretical frameworks or social agendas. They provide intellectual justification and institutional support for the constraint.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, biological_essentialists, beneficiary,
    institutional, civilizational, analytical, global).

% Are excluded from categories and spaces aligned with their gender identity, facing social ostracization, legal discrimination, and psychological distress. Their identity is denied by the constraint, making exit from this structural position impossible without denying their selfhood.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, transgender_women, payer,
    powerless, biographical, identity_locked, global).

% Are often rendered invisible or forced into binary categories that do not reflect their biological reality, experiencing medical pathologization and social exclusion. Their existence challenges the very premise of immutable biological markers, yet they are forced to conform or be denied recognition.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, intersex_individuals, payer,
    powerless, biographical, identity_locked, global).

% Advocate for gender identity as the primary determinant of gendered category membership. They are actively resisted and often silenced within the discourse promoted by the biological sex reading, their arguments dismissed as undermining women's rights or biological reality.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_identity_advocates, excluded,
    organized, generational, constrained, global).

% Analyze the social construction of gender, the impact of biological essentialism, and the lived experiences of transgender and intersex individuals. They provide empirical data and theoretical frameworks that often challenge the immutability claims of the biological sex reading.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, social_scientists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social categories and sex-segregated spaces based on a clear, binary understanding of biological sex, providing a stable framework for legal and social distinctions.
% TRANSFER_FUNCTION: Transfers social recognition, access to specific spaces, and definitional power from transgender and intersex individuals to cisgender women and those who uphold biological essentialism.
% ABSENT_VOICES: Transgender individuals, intersex advocates, and gender identity theorists are actively excluded from the definitional process, their perspectives dismissed as invalid or threatening to the established biological binary. They would argue for inclusive definitions and self-determination.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal and social landscape around gender would undergo significant rearrangement. Sex-segregated spaces would need new criteria, legal definitions of gender would broaden, and the social recognition of transgender and intersex individuals would increase, leading to a more fluid and inclusive understanding of gender.
% FOUNDING_PROBLEM: To establish clear, immutable categories for sex and gender, believed to be essential for social order, reproductive understanding, and the protection of women's rights.
% FOUNDING_PROBLEM_CORROBORATION: Biological essentialists and some cisgender women's advocates attest the problem is live, citing perceived threats to women's safety and the erosion of biological reality. Gender identity advocates and social scientists attest the problem is largely a social construct, and the constraint now serves to suppress marginalized identities; their corroboration comes from sociological studies and human rights frameworks.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint imposes significant costs on individuals whose identities or biology do not align with the binary definition, denying them social recognition and access to spaces. Suppression is very high (0.85) due to the active social and institutional enforcement mechanisms that penalize non-conformity and suppress alternative definitions. Theater ratio is low (0.1) as the constraint's function is directly tied to its stated purpose of maintaining biological sex distinctions, with little performative overhead.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of cisgender women advocates, this constraint is a necessary protection for women's rights and safety, a 'rope' or even a 'mountain' reflecting biological reality. From the perspective of transgender women and intersex individuals, it is a 'snare' that denies their existence and extracts their social standing. The engine's classification will reflect the latter due to the high extractiveness and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Cisgender women advocates and biological essentialists are beneficiaries (d near 0.0) as they gain definitional power and category preservation. Transgender women and intersex individuals are clear victims (d near 1.0) as they bear the direct costs of exclusion and identity denial. Gender identity advocates are excluded, their perspectives actively suppressed by the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to establish clear biological categories is contested. While proponents argue it's still live for protecting women's rights, critics argue it's 'dead' in its original form and now functions primarily as a tool for identity suppression. The high extractiveness and suppression, coupled with the 'contested' founding problem status, prevent mislabeling this as a benign coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_immutability_ambiguity,
    'Is biological sex truly immutable and binary, or does intersex variation and scientific understanding of sex development challenge this premise?',
    'Ongoing scientific research into sex differentiation and the prevalence of intersex conditions; re-evaluation of historical and cross-cultural understandings of sex.',
    'If biological sex is found to be more fluid or non-binary than asserted, the foundational premise of this reading weakens, potentially reducing its perceived legitimacy and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_immutability_ambiguity, empirical, 'Ambiguity regarding the immutability and binary nature of biological sex.').

omega_variable(
    category_purpose_ambiguity,
    'Is the primary purpose of gendered categories to reflect biological reality, or to organize social roles and identities?',
    'Philosophical and sociological analysis of the function of gender in society; legal and policy debates on the scope and purpose of sex-segregated spaces.',
    'If the primary purpose is deemed social organization, the biological sex reading''s justification for exclusion weakens, potentially shifting the constraint towards a ''tangled_rope'' or ''piton'' if its original coordination function atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_purpose_ambiguity, conceptual, 'Ambiguity regarding the fundamental purpose of gendered categories.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, policy exclusion) or internalized (psychological impact of identity denial)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., internalized transphobia) after structural barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more insidious.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for transgender and intersex individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t1950, gendered_category_membership__biological_sex_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(gend_tr_t1970, gendered_category_membership__biological_sex_reading, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(gend_tr_t1990, gendered_category_membership__biological_sex_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(gend_tr_t2010, gendered_category_membership__biological_sex_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(gend_tr_t2024, gendered_category_membership__biological_sex_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gend_be_t1950, gendered_category_membership__biological_sex_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(gend_be_t1970, gendered_category_membership__biological_sex_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(gend_be_t1990, gendered_category_membership__biological_sex_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(gend_be_t2010, gendered_category_membership__biological_sex_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(gend_be_t2024, gendered_category_membership__biological_sex_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t1950, gendered_category_membership__biological_sex_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(gend_su_t1970, gendered_category_membership__biological_sex_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(gend_su_t1990, gendered_category_membership__biological_sex_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(gend_su_t2010, gendered_category_membership__biological_sex_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(gend_su_t2024, gendered_category_membership__biological_sex_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
