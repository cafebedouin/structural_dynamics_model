% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__diversity_reading, []).

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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection: Diversity as Compelling State Interest Reading
 *   domain: constitutional_law/social_policy
 *
 * SUMMARY:
 *   This constraint represents the legal interpretation of the Equal
 *   Protection Clause that permits race to be considered as one factor among
 *   many in university admissions to achieve educational diversity, provided
 *   it is narrowly tailored and serves a compelling state interest. This
 *   reading, established in cases like Regents of the University of
 *   California v. Bakke (1978) and Grutter v. Bollinger (2003), allows for
 *   holistic review processes but strictly forbids quotas or set-asides. It
 *   is one reading of the broader 'equal_protection_commitment' kernel,
 *   distinct from 'colorblind' or 'remedial' interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.6).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection: Diversity as Compelling State Interest Reading").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, 'fb7cb20f-fd70-45e6-b177-108ec782de48').
narrative_ontology:cs_kernel_codification('fb7cb20f-fd70-45e6-b177-108ec782de48', fixed_text).
narrative_ontology:cs_authority_grounding('fb7cb20f-fd70-45e6-b177-108ec782de48', lineage).
narrative_ontology:cs_interpretation_layer_present('fb7cb20f-fd70-45e6-b177-108ec782de48').
narrative_ontology:cs_reading_relation('fb7cb20f-fd70-45e6-b177-108ec782de48', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('fb7cb20f-fd70-45e6-b177-108ec782de48', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('fb7cb20f-fd70-45e6-b177-108ec782de48', foundational, educational_diversity_is_compelling_state_interest).
narrative_ontology:cs_axiom_status(educational_diversity_is_compelling_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('fb7cb20f-fd70-45e6-b177-108ec782de48', educational_diversity_is_compelling_state_interest, conventional).
narrative_ontology:cs_axiom('fb7cb20f-fd70-45e6-b177-108ec782de48', foundational, race_as_one_factor_is_narrowly_tailored).
narrative_ontology:cs_axiom_status(race_as_one_factor_is_narrowly_tailored, holdable).
narrative_ontology:cs_axiom_grounding('fb7cb20f-fd70-45e6-b177-108ec782de48', race_as_one_factor_is_narrowly_tailored, conventional).
narrative_ontology:cs_reference_frame('fb7cb20f-fd70-45e6-b177-108ec782de48', grutter_v_bollinger_framework).
narrative_ontology:cs_drift_state('fb7cb20f-fd70-45e6-b177-108ec782de48', contemporary_legal_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fb7cb20f-fd70-45e6-b177-108ec782de48', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, universities_seeking_diversity).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, civil_rights_advocates).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, all_applicants_to_selective_universities).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, equal_opportunity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer admissions policies that consider race as one factor among many to achieve educational diversity. They benefit from the discretion to shape their student body and educational environment, but operate under strict judicial scrutiny.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, universities_seeking_diversity, agenda_setter,
    institutional, generational, constrained, national).

% Navigate a complex, holistic admissions process where individual claims of merit may be weighed against diversity goals. They bear the cost of reduced transparency and the potential perception of unfairness, even if not directly discriminated against.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, all_applicants_to_selective_universities, payer,
    powerless, biographical, constrained, national).

% Establishes and refines the legal framework for equal protection, defining what constitutes a compelling state interest and narrowly tailored means. Its precedents shape the operational boundaries for universities.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% Support policies that promote racial diversity in education, viewing it as essential for social equity and educational quality. They benefit from the legal framework that permits race-conscious admissions.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Advocate for strict colorblindness or purely merit-based admissions, believing that any consideration of race is discriminatory. They bear the cost of policies they view as undermining individual equality.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, equal_opportunity_advocates, payer,
    organized, generational, mobile, national).

% May pass laws affecting university admissions within their states, often responding to public pressure or judicial rulings. Their actions are constrained by the federal equal protection framework.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, state_legislatures, observer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows universities to pursue the educational benefits of diversity while adhering to the constitutional mandate of equal protection, balancing institutional goals with individual rights.
% TRANSFER_FUNCTION: Transfers some individual claim certainty from applicants to universities, granting universities discretion in admissions to achieve diversity goals, while requiring a holistic review process.
% ABSENT_VOICES: Those who believe in strict colorblindness or pure merit-based admissions, who feel their claims are obscured by holistic review and that the Constitution forbids any consideration of race.
% DISAPPEARANCE_RATIONALE: If this legal framework vanished, universities would either revert to purely numerical criteria (losing diversity) or face immediate legal challenges for any race-conscious policies, fundamentally altering admissions practices and educational outcomes across the nation.
% FOUNDING_PROBLEM: How to reconcile the constitutional mandate of equal protection with the perceived educational benefits of a diverse student body, particularly after the explicit racial segregation of the past, without resorting to quotas.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, educational researchers, and university administrators (including those not directly benefiting from the policy) attest to the ongoing challenge of achieving diversity without explicit quotas, and the need for a framework to navigate this complex issue.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.28, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__diversity_reading_tests).
:- end_tests(equal_protection_commitment__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) because the constraint is primarily procedural, granting discretion rather than mandating a specific outcome, but it does impose costs on applicants through reduced transparency and the potential for individual claims to be obscured. Suppression is moderate (0.60) due to the active enforcement by courts and the administrative burden on universities to defend their policies. Theater ratio is low (0.10) as the function of balancing diversity and equal protection is genuinely performed, not merely theatrical. Resistance is moderate (0.50) due to ongoing legal and political challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of universities and civil rights advocates, this constraint is a necessary and legitimate tool for achieving important educational and societal goals. From the perspective of some applicants and equal opportunity advocates, it represents an unfair imposition that complicates admissions and potentially undermines individual merit. The engine computes these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities seeking diversity are beneficiaries (d near 0.0) as they gain discretion to pursue their educational missions. Civil rights advocates also benefit from the framework's support for diversity. All applicants to selective universities are potential targets (d near 1.0) as their individual applications are subject to a complex, less transparent process. Equal opportunity advocates, who oppose race-conscious policies, also bear costs as their preferred framework is not fully realized.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately classified as the ''diversity_reading'' of the ''equal_protection_commitment'' kernel?',
    'Analysis of judicial opinions and university policies to confirm alignment with the ''one factor among many'' and ''compelling state interest'' doctrines, distinguishing it from other interpretations.',
    'If misclassified, the analysis of its relationship to sibling readings and its internal axioms would be flawed, leading to incorrect predictions about its stability and drift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific interpretation of the Equal Protection Clause.').

omega_variable(
    diversity_definition_ambiguity,
    'What constitutes ''educational diversity'' as a compelling state interest, and how is its achievement measured and verified by universities?',
    'Judicial clarification on the permissible scope and metrics of diversity, or empirical studies demonstrating the educational benefits attributed to specific diversity initiatives.',
    'A clearer definition would reduce the ambiguity and administrative burden on universities, potentially lowering extractiveness. An overly broad or unmeasurable definition could increase theater and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_definition_ambiguity, empirical, 'Ambiguity in the definition and measurement of educational diversity.').

omega_variable(
    narrow_tailoring_efficacy,
    'Are the ''narrowly tailored'' means (e.g., holistic review) genuinely effective in achieving diversity without becoming a de facto quota system or unduly burdening non-minority applicants?',
    'Empirical analysis of admissions data and outcomes from universities employing holistic review, compared against stated diversity goals and impact on all applicant groups.',
    'If found ineffective or overly burdensome, the constraint''s legitimacy would be undermined, potentially leading to its reclassification as more extractive or even a snare. If effective, it reinforces the rope/tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_efficacy, empirical, 'Effectiveness of ''narrowly tailored'' means in achieving diversity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__diversity_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(equa_tr_t1992, equal_protection_commitment__diversity_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(equa_tr_t2006, equal_protection_commitment__diversity_reading, theater_ratio, 2006, 0.1).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__diversity_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__diversity_reading, base_extractiveness, 1978, 0.2).
narrative_ontology:measurement(equa_be_t1992, equal_protection_commitment__diversity_reading, base_extractiveness, 1992, 0.23).
narrative_ontology:measurement(equa_be_t2006, equal_protection_commitment__diversity_reading, base_extractiveness, 2006, 0.26).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__diversity_reading, base_extractiveness, 2023, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__diversity_reading, suppression_requirement, 1978, 0.55).
narrative_ontology:measurement(equa_su_t1992, equal_protection_commitment__diversity_reading, suppression_requirement, 1992, 0.58).
narrative_ontology:measurement(equa_su_t2006, equal_protection_commitment__diversity_reading, suppression_requirement, 2006, 0.6).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__diversity_reading, suppression_requirement, 2023, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
