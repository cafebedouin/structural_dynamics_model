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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Female Category Defined by Biological Sex
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint instantiates the 'sex_biology_reading' of the
 *   'woman_female_category' kernel. It defines category membership based
 *   strictly on chromosomal sex (XX/XY), reproductive anatomy, and
 *   developmental biology. From this reading's perspective, the category of
 *   'woman' or 'female' is an immutable biological reality, foundational for
 *   legal, social, and medical distinctions. The constraint actively enforces
 *   boundaries to exclude individuals who do not meet these biological
 *   criteria, particularly trans women, from female-only spaces and
 *   protections, which is perceived as necessary for the safety and rights of
 *   natal females. The claimed type is 'tangled_rope' because it provides a
 *   coordination function for natal females while simultaneously extracting
 *   from trans women through active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.8).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.85).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Female Category Defined by Biological Sex").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '841e1a17-3228-4a1a-a11f-18840c2adb71').
narrative_ontology:cs_kernel_codification('841e1a17-3228-4a1a-a11f-18840c2adb71', formalized).
narrative_ontology:cs_authority_grounding('841e1a17-3228-4a1a-a11f-18840c2adb71', expertise).
narrative_ontology:cs_interpretation_layer_present('841e1a17-3228-4a1a-a11f-18840c2adb71').
narrative_ontology:cs_reading_relation('841e1a17-3228-4a1a-a11f-18840c2adb71', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('841e1a17-3228-4a1a-a11f-18840c2adb71', woman_female_category__hybrid_contextual_reading, forecloses).
narrative_ontology:cs_axiom('841e1a17-3228-4a1a-a11f-18840c2adb71', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('841e1a17-3228-4a1a-a11f-18840c2adb71', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('841e1a17-3228-4a1a-a11f-18840c2adb71', foundational, sex_segregation_is_necessary_for_female_safety).
narrative_ontology:cs_axiom_status(sex_segregation_is_necessary_for_female_safety, holdable).
narrative_ontology:cs_axiom_grounding('841e1a17-3228-4a1a-a11f-18840c2adb71', sex_segregation_is_necessary_for_female_safety, empirically_contingent).
narrative_ontology:cs_reference_frame('841e1a17-3228-4a1a-a11f-18840c2adb71', biological_sex_as_foundational_category).
narrative_ontology:cs_drift_state('841e1a17-3228-4a1a-a11f-18840c2adb71', contemporary_gender_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('841e1a17-3228-4a1a-a11f-18840c2adb71', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, gender_non_conforming_individuals).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, sex_based_rights_doctrine).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, biological_realism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the clarity and protective function of sex-segregated spaces and legal categories, particularly in contexts like sports, prisons, and shelters, which they perceive as safeguarding their physical safety, privacy, and fairness. They advocate for policies that reinforce this definition.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections, beneficiary,
    organized, generational, mobile, national).

% Are excluded from female-only spaces and legal categories based on this definition, leading to social marginalization, denial of recognition, and potential safety risks in spaces aligned with their birth-assigned sex. They experience this as a denial of their identity and human rights.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women, payer,
    powerless, biographical, identity_locked, local).

% May face scrutiny, misgendering, or exclusion from sex-segregated spaces if their appearance or presentation does not align with rigid biological sex stereotypes, even if they are natal females. They bear the cost of strict enforcement of sex-based categories.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, gender_non_conforming_individuals, payer,
    powerless, biographical, constrained, local).

% Actively promote and defend the definition of female based on biological sex, citing scientific evidence and historical legal precedent. They lobby policymakers and engage in public discourse to ensure this definition is upheld in law and policy, particularly concerning women's rights and spaces.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, sex_realist_advocates, agenda_setter,
    organized, generational, mobile, global).

% Are tasked with creating and enforcing laws and policies that define sex and gender categories. They face pressure from various advocacy groups and must navigate complex legal and social debates, often leading to inconsistent application or contested interpretations of sex-based categories.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, institutional_policy_makers, agenda_setter,
    institutional, immediate, constrained, national).

% Are actively working to establish gender identity as the primary determinant of category membership, challenging the biological sex definition. They are excluded from the framing of this constraint as a legitimate basis for social organization and are often framed as opposing women's rights.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, gender_identity_advocates, excluded,
    organized, generational, mobile, global).

% Academics, researchers, and ethicists who analyze the philosophical, biological, and social implications of different category definitions. They seek to understand the structural effects and ethical consequences of each reading without necessarily endorsing one.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:fixing_cost_class(woman_female_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a clear, objective, and historically consistent basis for social, legal, and medical categorization, particularly for the protection and recognition of natal females.
% TRANSFER_FUNCTION: Transfers social recognition, access to sex-segregated spaces, and specific legal protections to natal females, while denying these to trans women and other gender non-conforming individuals who do not meet the biological criteria.
% ABSENT_VOICES: Trans women and gender identity advocates are largely absent from the foundational framing of this constraint, as their perspectives on identity and self-determination are directly contradicted by its core premises. They would argue for self-identification as the basis for category membership.
% DISAPPEARANCE_RATIONALE: If the definition of female based solely on biological sex vanished overnight, legal frameworks, social norms, and institutional policies regarding sex-segregated spaces, sports, and medical care would undergo significant and immediate reorganization. The concept of 'woman' as a distinct biological class with specific rights and needs would be fundamentally altered, leading to widespread re-evaluation of policies and social structures.
% FOUNDING_PROBLEM: The need for clear, immutable, and universally applicable categories for human beings based on observable biological characteristics, particularly to ensure the protection and distinct recognition of the female sex in law and society.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading, including some feminist organizations and scientific bodies, attest that the founding problem of protecting and recognizing biological females remains live and is increasingly urgent due to challenges from alternative definitions. This is corroborated by ongoing legislative debates and scientific publications emphasizing sex differences.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.8) because the constraint denies fundamental social recognition and access to specific spaces for trans women, imposing significant costs on their well-being and identity. Suppression is also high (0.85) as it requires active legal and social enforcement to maintain these boundaries against competing definitions and advocacy. The theater ratio is low (0.1) because, from the perspective of this reading, the function of defining and protecting biological sex categories is considered very real and essential, not performative. Accessibility collapse is high (0.9) for trans women, as alternatives for category membership are structurally denied. Resistance is high (0.75) due to significant and organized opposition from gender identity advocates and trans communities.
 *
 * PERSPECTIVAL GAP:
 *   Natal females seeking sex-based protections experience this constraint as a necessary and beneficial coordination mechanism, ensuring their safety and rights. In contrast, trans women experience it as a highly extractive and suppressive force that denies their identity and access to appropriate spaces. Sex realist advocates view it as upholding scientific truth and fundamental rights, while gender identity advocates see it as discriminatory and harmful. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal females seeking sex-based protections are the primary beneficiaries, gaining exclusive access and legal recognition. Trans women and gender non-conforming individuals are the primary targets/payers, bearing the costs of exclusion and misrecognition. Sex realist advocates and institutional policymakers act as agenda-setters, actively shaping and enforcing the constraint. Gender identity advocates are structurally excluded from the framing of this constraint, as their core premises are directly contradicted.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_definition_ambiguity,
    'Is the category ''woman'' fundamentally a biological, social, or identity-based construct?',
    'Philosophical consensus on the nature of social categories, or a societal shift in the dominant understanding of gender.',
    'If ''woman'' is primarily a social or identity-based construct, the biological definition becomes a snare; if it is purely biological, the constraint is a rope or mountain for natal females.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_definition_ambiguity, conceptual, 'Ambiguity in the foundational definition of ''woman''.').

omega_variable(
    safety_efficacy_ambiguity,
    'Does strict sex-based segregation genuinely enhance physical safety and fairness for natal females in all contexts, or is it a pretext for exclusion?',
    'Empirical studies on safety outcomes in mixed-sex vs. sex-segregated spaces, and analysis of competitive fairness in sports with and without sex-based criteria.',
    'If safety/fairness benefits are negligible or can be achieved by less restrictive means, the high extractiveness and suppression become harder to justify, pushing the classification closer to a snare. If benefits are substantial and unique, it reinforces the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_efficacy_ambiguity, empirical, 'Whether the claimed benefits of sex-based segregation are empirically verifiable.').

omega_variable(
    identity_vs_biology_priority,
    'In cases of conflict, which should take precedence in legal and social contexts: biological sex or gender identity?',
    'Legislative action, judicial rulings, or evolving societal norms establishing a clear hierarchy or framework for reconciliation.',
    'The resolution of this preference question directly determines the victim and beneficiary sets, and thus the directionality and effective extraction for various groups. If identity takes precedence, this reading''s extractiveness increases dramatically; if biology, it is maintained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_vs_biology_priority, preference, 'The normative priority between biological sex and gender identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t6, woman_female_category__sex_biology_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__sex_biology_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(woma_tr_t18, woman_female_category__sex_biology_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(woma_tr_t24, woman_female_category__sex_biology_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(woma_tr_t30, woman_female_category__sex_biology_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(woma_be_t6, woman_female_category__sex_biology_reading, base_extractiveness, 6, 0.73).
narrative_ontology:measurement(woma_be_t12, woman_female_category__sex_biology_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(woma_be_t18, woman_female_category__sex_biology_reading, base_extractiveness, 18, 0.78).
narrative_ontology:measurement(woma_be_t24, woman_female_category__sex_biology_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(woma_be_t30, woman_female_category__sex_biology_reading, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(woma_su_t6, woman_female_category__sex_biology_reading, suppression_requirement, 6, 0.78).
narrative_ontology:measurement(woma_su_t12, woman_female_category__sex_biology_reading, suppression_requirement, 12, 0.81).
narrative_ontology:measurement(woma_su_t18, woman_female_category__sex_biology_reading, suppression_requirement, 18, 0.83).
narrative_ontology:measurement(woma_su_t24, woman_female_category__sex_biology_reading, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(woma_su_t30, woman_female_category__sex_biology_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'woman_female_category' kernel. This 'sex_biology_reading' defines female category membership by chromosomal sex and reproductive biology, directly influencing and being influenced by the 'gender_identity_reading' and 'hybrid_contextual_reading' which offer alternative definitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
