% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__diversity_reading, []).

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
 *   constraint_id: equal_protection_clause__diversity_reading
 *   human_readable: Equal Protection Clause: Diversity Interest Reading
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   This constraint represents the 'diversity interest' reading of the Equal
 *   Protection Clause, which permits race-conscious policies in higher
 *   education when narrowly tailored to serve a compelling interest in
 *   educational diversity. This reading views diversity as a benefit to all
 *   students, not solely as a remedy for past discrimination. It is a
 *   specific interpretation of a broader constitutional kernel, distinct from
 *   'colorblind' or 'remedial' readings. The constraint operates as a Tangled
 *   Rope because it genuinely coordinates educational goals (diversity
 *   benefits) but also involves asymmetric extraction (some applicants are
 *   disadvantaged for the benefit of others and the institution).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.45).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.3).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection Clause: Diversity Interest Reading").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, '65ba3a78-27a6-404f-b2bf-4a0a42da0d15').
narrative_ontology:cs_kernel_codification('65ba3a78-27a6-404f-b2bf-4a0a42da0d15', fixed_text).
narrative_ontology:cs_authority_grounding('65ba3a78-27a6-404f-b2bf-4a0a42da0d15', lineage).
narrative_ontology:cs_interpretation_layer_present('65ba3a78-27a6-404f-b2bf-4a0a42da0d15').
narrative_ontology:cs_reading_relation('65ba3a78-27a6-404f-b2bf-4a0a42da0d15', equal_protection_clause__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('65ba3a78-27a6-404f-b2bf-4a0a42da0d15', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('65ba3a78-27a6-404f-b2bf-4a0a42da0d15', foundational, educational_diversity_is_compelling_interest).
narrative_ontology:cs_axiom_status(educational_diversity_is_compelling_interest, overridden).
narrative_ontology:cs_axiom_grounding('65ba3a78-27a6-404f-b2bf-4a0a42da0d15', educational_diversity_is_compelling_interest, empirically_contingent).
narrative_ontology:cs_axiom('65ba3a78-27a6-404f-b2bf-4a0a42da0d15', secondary, race_as_plus_factor_is_narrowly_tailored).
narrative_ontology:cs_axiom_status(race_as_plus_factor_is_narrowly_tailored, overridden).
narrative_ontology:cs_axiom_grounding('65ba3a78-27a6-404f-b2bf-4a0a42da0d15', race_as_plus_factor_is_narrowly_tailored, conventional).
narrative_ontology:cs_reference_frame('65ba3a78-27a6-404f-b2bf-4a0a42da0d15', bakke_plurality_framework).
narrative_ontology:cs_drift_state('65ba3a78-27a6-404f-b2bf-4a0a42da0d15', sfaf_v_harvard_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('65ba3a78-27a6-404f-b2bf-4a0a42da0d15', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, educational_institutions).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, minority_applicants_denied_admission).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, majority_applicants_denied_admission).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, minority_applicants_admitted).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, civil_rights_advocates).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, educational_diversity_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, holistic_review_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions implement race-conscious admissions policies to achieve educational diversity, believing it benefits their student body and mission. They bear the legal and administrative costs of defending these policies against challenges.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% All students, including those from majority groups, are considered beneficiaries of a diverse learning environment, which enriches their educational experience and prepares them for a diverse society. This benefit is diffuse and long-term.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, all_students, beneficiary,
    moderate, biographical, mobile, local).

% Minority applicants whose race is considered as a 'plus factor' in admissions, leading to their acceptance into institutions they might not otherwise have attended. They benefit from increased access and the educational opportunities provided.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_applicants_admitted, beneficiary,
    moderate, immediate, constrained, local).

% Minority applicants who, despite race-conscious policies, are denied admission. While the policy aims to benefit their group, individual applicants may still experience the cost of rejection, potentially feeling tokenized or that their merit was secondary.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_applicants_denied_admission, payer,
    powerless, immediate, constrained, local).

% Applicants from majority groups who are denied admission, potentially perceiving that race-conscious policies disadvantaged them. They bear the direct cost of rejection and may feel unfairly treated, leading to legal challenges.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, majority_applicants_denied_admission, payer,
    moderate, immediate, constrained, local).

% The ultimate arbiter of Equal Protection Clause interpretations. Its rulings shape the permissible scope of race-conscious policies, balancing various constitutional principles and societal interests. Its role is to adjudicate, not to directly implement or benefit.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% Advocate for policies that promote diversity and inclusion, viewing race-conscious admissions as a legitimate tool to achieve these goals. They benefit from the legal and social validation of their advocacy efforts.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the efforts of educational institutions to create diverse learning environments, aiming to produce graduates better equipped for a diverse society and global economy. It provides a legal framework for institutions to pursue this goal.
% TRANSFER_FUNCTION: Transfers educational opportunities and social capital by adjusting admissions criteria, potentially from some applicants (both minority and majority) to others, in service of a broader institutional goal of diversity. It also transfers legal and administrative burden to institutions.
% ABSENT_VOICES: Applicants who feel unfairly disadvantaged by race-conscious policies, particularly those from majority groups who believe they are being penalized for historical injustices they did not commit. Their voices are often heard in litigation rather than policy formation.
% DISAPPEARANCE_RATIONALE: If this reading vanished, educational institutions would likely revert to purely race-neutral admissions, potentially leading to less diverse student bodies. This would trigger significant shifts in educational outcomes, institutional missions, and societal debates about equity.
% FOUNDING_PROBLEM: The problem of achieving a diverse student body and educational environment, which was seen as beneficial for all students and for preparing them for a diverse society, while navigating the constitutional prohibition against racial discrimination.
% FOUNDING_PROBLEM_CORROBORATION: Educational leaders, social scientists, and many civil rights organizations attest that the problem of achieving meaningful diversity in education remains live and that race-conscious policies are a necessary tool. This is corroborated by ongoing demographic disparities and research on the benefits of diversity.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__diversity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__diversity_reading_tests).
:- end_tests(equal_protection_clause__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because while the policy aims for a collective good, it does so by making individual admissions decisions race-conscious, which can disadvantage some applicants. Suppression (0.3) is relatively low because the constraint is actively debated and challenged in courts, and institutions must adhere to strict 'narrow tailoring' requirements, limiting the extent of racial preferences. Theater ratio (0.1) is low, as the stated goal of educational diversity is genuinely pursued by institutions, though the effectiveness and fairness are contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of educational institutions and civil rights advocates, this reading is a legitimate and necessary tool for achieving important educational goals. From the perspective of applicants denied admission, particularly those from majority groups, it can be perceived as an unfair form of reverse discrimination. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational institutions and 'all students' are beneficiaries, as they gain from the diverse environment. Minority applicants admitted also benefit directly. However, both minority and majority applicants denied admission bear costs, making them payers. Civil rights advocates are beneficiaries of the policy's existence. The Supreme Court acts as an observer, adjudicating the constraint's boundaries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_compelling_interest_empirical_basis,
    'Is the claim that educational diversity serves a ''compelling interest'' empirically robust, demonstrating clear, measurable benefits for all students?',
    'Longitudinal studies tracking educational and post-graduate outcomes for students in diverse vs. non-diverse environments, controlling for other variables. Consensus among educational researchers.',
    'If the empirical basis is weak, the ''compelling interest'' justification for race-conscious policies would erode, potentially shifting the constraint towards a Snare or Piton if the policies persist without clear benefit. If strong, it reinforces the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_compelling_interest_empirical_basis, empirical, 'Empirical validity of the ''compelling interest'' in educational diversity.').

omega_variable(
    narrow_tailoring_effectiveness,
    'Are ''narrow tailoring'' requirements (e.g., individualized review, no quotas) genuinely effective at minimizing harm to individual applicants, or are they largely performative?',
    'Audits of admissions processes, analysis of applicant pools and outcomes under different policy regimes, and legal challenges testing the limits of ''narrow tailoring''.',
    'If narrow tailoring is found to be performative, the constraint''s theater_ratio would increase, and its extractiveness would be perceived as higher, potentially pushing it towards a Snare. If effective, it supports the Tangled Rope classification by demonstrating a genuine attempt to balance interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_effectiveness, empirical, 'Effectiveness of ''narrow tailoring'' in mitigating individual harm.').

omega_variable(
    instrumentalization_of_minority_students,
    'Does the ''diversity interest'' reading instrumentalize minority students by valuing them primarily for their contribution to the educational experience of others, rather than for their inherent worth?',
    'Qualitative studies of minority student experiences, philosophical analysis of the ethical implications of ''diversity'' as a compelling interest, and shifts in legal doctrine regarding the purpose of affirmative action.',
    'If instrumentalization is found to be a significant ethical cost, it would increase the perceived extractiveness and suppression for minority students, potentially shifting the constraint towards a Snare from their perspective. It would also challenge the moral legitimacy of the ''beneficiary'' role for ''all students''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(instrumentalization_of_minority_students, conceptual, 'Ethical implications of instrumentalizing minority students for diversity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__diversity_reading, theater_ratio, 1978, 0.05).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_clause__diversity_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__diversity_reading, theater_ratio, 2003, 0.1).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_clause__diversity_reading, theater_ratio, 2016, 0.1).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__diversity_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement(equa_be_t1990, equal_protection_clause__diversity_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__diversity_reading, base_extractiveness, 2003, 0.42).
narrative_ontology:measurement(equa_be_t2016, equal_protection_clause__diversity_reading, base_extractiveness, 2016, 0.45).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__diversity_reading, base_extractiveness, 2023, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.2).
narrative_ontology:measurement(equa_su_t1990, equal_protection_clause__diversity_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__diversity_reading, suppression_requirement, 2003, 0.28).
narrative_ontology:measurement(equa_su_t2016, equal_protection_clause__diversity_reading, suppression_requirement, 2016, 0.3).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__diversity_reading, suppression_requirement, 2023, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Equal Protection Clause kernel, alongside the 'colorblind_reading' and 'remedial_reading'. Each reading instantiates a distinct constraint with different beneficiaries, victims, and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
