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
 *   constraint_id: equal_protection_clause__diversity_reading
 *   human_readable: Equal Protection Clause: Diversity Interest Reading
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   This constraint represents the 'diversity reading' of the Equal
 *   Protection Clause, which permits race-conscious policies in higher
 *   education when narrowly tailored to serve a compelling interest in
 *   educational diversity. This reading views diversity as a benefit to all
 *   students, not solely as a remedy for past discrimination. It is a
 *   contested interpretation, particularly in light of other readings that
 *   emphasize colorblindness or remedial justice. The metrics reflect a
 *   system that, while aiming for coordination (diversity benefits), involves
 *   some extraction (applicants denied admission) and requires active
 *   enforcement to maintain its legal standing.
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
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, 'a2a63c54-23ae-4530-8db1-1d3be0c71361').
narrative_ontology:cs_kernel_codification('a2a63c54-23ae-4530-8db1-1d3be0c71361', fixed_text).
narrative_ontology:cs_authority_grounding('a2a63c54-23ae-4530-8db1-1d3be0c71361', lineage).
narrative_ontology:cs_interpretation_layer_present('a2a63c54-23ae-4530-8db1-1d3be0c71361').
narrative_ontology:cs_reading_relation('a2a63c54-23ae-4530-8db1-1d3be0c71361', equal_protection_clause__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2a63c54-23ae-4530-8db1-1d3be0c71361', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('a2a63c54-23ae-4530-8db1-1d3be0c71361', foundational, educational_diversity_is_compelling_interest).
narrative_ontology:cs_axiom_status(educational_diversity_is_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('a2a63c54-23ae-4530-8db1-1d3be0c71361', educational_diversity_is_compelling_interest, empirically_contingent).
narrative_ontology:cs_axiom('a2a63c54-23ae-4530-8db1-1d3be0c71361', foundational, race_as_one_factor_in_holistic_review_is_narrowly_tailored).
narrative_ontology:cs_axiom_status(race_as_one_factor_in_holistic_review_is_narrowly_tailored, holdable).
narrative_ontology:cs_axiom_grounding('a2a63c54-23ae-4530-8db1-1d3be0c71361', race_as_one_factor_in_holistic_review_is_narrowly_tailored, conventional).
narrative_ontology:cs_reference_frame('a2a63c54-23ae-4530-8db1-1d3be0c71361', bakke_grutter_precedent).
narrative_ontology:cs_drift_state('a2a63c54-23ae-4530-8db1-1d3be0c71361', sfafa_v_harvard_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a2a63c54-23ae-4530-8db1-1d3be0c71361', '').
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

% Minority applicants whose race is considered as a 'plus factor' in admissions, contributing to their enrollment in institutions they might not otherwise access. They benefit from the policy's intent to create a diverse class.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_applicants_admitted, beneficiary,
    moderate, biographical, constrained, local).

% Minority applicants who, despite the policy's intent, are denied admission to a specific institution. Their race is considered, but the holistic review may still favor other candidates, leading to a sense of instrumentalization without guaranteed benefit.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_applicants_denied_admission, payer,
    powerless, immediate, trapped, local).

% Applicants from majority groups who are denied admission, potentially perceiving themselves as disadvantaged by race-conscious policies that favor minority candidates for diversity purposes. They bear the cost of a more competitive admissions process.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, majority_applicants_denied_admission, payer,
    powerless, immediate, trapped, local).

% The ultimate arbiter of Equal Protection Clause interpretation. Its rulings shape the permissible scope of race-conscious policies, balancing diversity interests against claims of individual discrimination. Its decisions are binding on all other stakeholders.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% Advocate for policies that promote racial diversity and inclusion in education, viewing this reading as a legitimate means to achieve broader societal equity goals. They provide legal and public support for educational institutions.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Argue against any race-conscious policies, asserting that the Equal Protection Clause demands strict colorblindness. They are excluded from the direct implementation of diversity policies but actively challenge them in court and public discourse.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, colorblind_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the admissions practices of educational institutions to achieve a diverse student body, which is believed to enhance the learning environment and prepare students for a diverse society. It provides a legal framework for institutions to pursue this goal.
% TRANSFER_FUNCTION: Transfers educational opportunities and benefits of a diverse environment to all students, while potentially shifting admission slots among applicants based on race as one factor in a holistic review.
% ABSENT_VOICES: Advocates for a strictly colorblind interpretation of the Equal Protection Clause are structurally excluded from the policy-making process that implements diversity policies, though they actively challenge them in the courts. Their arguments for individual meritocracy over group-based considerations are not directly incorporated into the diversity rationale.
% DISAPPEARANCE_RATIONALE: If this reading vanished, educational institutions would likely revert to strictly race-neutral admissions, potentially leading to less diverse student bodies. This would trigger new legal challenges, policy debates, and a re-evaluation of educational goals, fundamentally altering the landscape of higher education.
% FOUNDING_PROBLEM: The problem of achieving a robust and educationally beneficial diversity in higher education after the dismantling of de jure segregation, without resorting to quotas or explicit discrimination.
% FOUNDING_PROBLEM_CORROBORATION: Educational leaders, social scientists, and civil rights organizations consistently attest to the ongoing importance of diversity for educational outcomes and societal preparation. This is corroborated by numerous studies on the benefits of diverse learning environments, from outside the direct beneficiaries of the policy.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate because while some applicants are denied admission due to race-conscious policies, the policies are not designed for pure extraction but for a broader educational benefit. Suppression (0.3) is relatively low, as the constraint is actively debated and challenged in courts, indicating that alternatives (race-neutral policies) are not fully suppressed. Theater ratio (0.1) is low, as the stated goal of educational diversity is genuinely pursued by institutions, though the effectiveness and fairness of the means are contested. The claimed type is 'tangled_rope' because it serves a coordination function (diversity benefits) but involves asymmetric extraction (some applicants bear costs) and requires active enforcement to persist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of educational institutions and many civil rights advocates, this reading is a legitimate and necessary tool for achieving educational excellence and societal preparation. From the perspective of applicants denied admission (especially majority applicants) and colorblind advocates, it is an unfair and discriminatory practice that violates individual rights. The engine's classification will reflect this tension between coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational institutions and all students are beneficiaries, as they gain from the diverse learning environment. Minority students admitted under these policies are also beneficiaries. However, both minority and majority applicants who are denied admission can be considered victims, as their individual aspirations are impacted by the race-conscious aspect of the policy. The Supreme Court acts as an observer, adjudicating the legality and scope of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_benefit_quantification,
    'To what extent are the claimed educational benefits of diversity empirically quantifiable and attributable to race-conscious policies, as opposed to other factors?',
    'Longitudinal studies tracking educational outcomes, civic engagement, and professional success of students from diverse vs. non-diverse institutions, controlling for other variables. Expert testimony from social scientists and educators.',
    'Strong empirical evidence would bolster the ''compelling interest'' argument, potentially reducing perceived extractiveness for those denied admission. Weak or contested evidence would undermine the justification, increasing perceived extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_benefit_quantification, empirical, 'Empirical basis for the claimed benefits of educational diversity.').

omega_variable(
    instrumentalization_of_minority_students,
    'Does the ''diversity reading'' instrumentalize minority students by valuing their presence primarily for the benefit of the majority, rather than for their inherent worth or to remedy their own group''s disadvantage?',
    'Qualitative research on the experiences of minority students in institutions with diversity policies, analysis of institutional rhetoric, and legal scholarship on the intent vs. effect of such policies.',
    'If instrumentalization is found to be a significant effect, it would increase the perceived extractiveness and suppression for minority students, potentially shifting the classification towards a snare for that seat. If not, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalization_of_minority_students, conceptual, 'Whether minority students are instrumentalized by diversity policies.').

omega_variable(
    narrow_tailoring_effectiveness,
    'Are the ''narrowly tailored'' requirements (e.g., no quotas, individualized review) genuinely effective in minimizing the burden on individual applicants, or do they function as legal theater?',
    'Audits of admissions processes, analysis of admissions data for patterns resembling quotas, and legal challenges testing the limits of ''narrow tailoring'' in practice.',
    'If narrow tailoring is found to be ineffective or performative, the constraint''s theater_ratio would increase, and its extractiveness for denied applicants would be perceived as higher and less justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_effectiveness, empirical, 'Effectiveness of narrow tailoring in mitigating individual burden.').


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
narrative_ontology:measurement(equa_tr_t2000, equal_protection_clause__diversity_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_clause__diversity_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__diversity_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.35).
narrative_ontology:measurement(equa_be_t1990, equal_protection_clause__diversity_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(equa_be_t2000, equal_protection_clause__diversity_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(equa_be_t2010, equal_protection_clause__diversity_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__diversity_reading, base_extractiveness, 2023, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.2).
narrative_ontology:measurement(equa_su_t1990, equal_protection_clause__diversity_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(equa_su_t2000, equal_protection_clause__diversity_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(equa_su_t2010, equal_protection_clause__diversity_reading, suppression_requirement, 2010, 0.29).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__diversity_reading, suppression_requirement, 2023, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, identity_coordination).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Equal Protection Clause kernel. The 'diversity_reading' focuses on educational benefits for all students, distinct from the 'colorblind_reading' (individual rights, no racial classification) and the 'remedial_reading' (addressing historical subordination). Each reading represents a different structural claim about the clause's function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
