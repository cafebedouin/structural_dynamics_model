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
 *   human_readable: Equal Protection: Diversity as Compelling Interest
 *   domain: constitutional_law/education_policy/political_philosophy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.45).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.55).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection: Diversity as Compelling Interest").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy/political_philosophy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, 'b900387d-bf07-4b86-86bb-9c9d479ff5e2').
narrative_ontology:cs_kernel_codification('b900387d-bf07-4b86-86bb-9c9d479ff5e2', fixed_text).
narrative_ontology:cs_authority_grounding('b900387d-bf07-4b86-86bb-9c9d479ff5e2', lineage).
narrative_ontology:cs_interpretation_layer_present('b900387d-bf07-4b86-86bb-9c9d479ff5e2').
narrative_ontology:cs_reading_relation('b900387d-bf07-4b86-86bb-9c9d479ff5e2', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('b900387d-bf07-4b86-86bb-9c9d479ff5e2', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('b900387d-bf07-4b86-86bb-9c9d479ff5e2', foundational, diversity_is_compelling_interest).
narrative_ontology:cs_axiom_status(diversity_is_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('b900387d-bf07-4b86-86bb-9c9d479ff5e2', diversity_is_compelling_interest, empirically_contingent).
narrative_ontology:cs_axiom('b900387d-bf07-4b86-86bb-9c9d479ff5e2', foundational, narrow_tailoring_is_possible).
narrative_ontology:cs_axiom_status(narrow_tailoring_is_possible, holdable).
narrative_ontology:cs_axiom_grounding('b900387d-bf07-4b86-86bb-9c9d479ff5e2', narrow_tailoring_is_possible, conventional).
narrative_ontology:cs_reference_frame('b900387d-bf07-4b86-86bb-9c9d479ff5e2', post_bakke_framework).
narrative_ontology:cs_drift_state('b900387d-bf07-4b86-86bb-9c9d479ff5e2', post_sffa_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('b900387d-bf07-4b86-86bb-9c9d479ff5e2', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, diversity_policy_proponents).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, applicants_denied_admission).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, colorblind_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, minority_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of the Equal Protection Clause, establishing and refining the legal framework that permits (or forbids) race-conscious policies for diversity. Its precedents define the boundaries of this constraint.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% Implement and defend race-conscious admissions policies, believing they are necessary to achieve educational diversity and enhance the learning environment for all students. They bear the administrative and legal costs of maintaining these policies.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Are considered the primary beneficiaries of the diverse learning environment fostered by these policies, which is argued to improve critical thinking, cross-cultural understanding, and preparation for a diverse workforce and society.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, all_students, beneficiary,
    moderate, biographical, mobile, national).

% Benefit from increased representation and access to educational opportunities that might otherwise be limited. However, they are also instrumentalized as a means to achieve the broader diversity interest for the benefit of all students.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_students, beneficiary,
    moderate, biographical, mobile, national).

% Individuals who might have been admitted under purely race-neutral criteria but are denied admission due to race-conscious policies. They bear the direct cost of the policy, often feeling unfairly disadvantaged.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, applicants_denied_admission, payer,
    powerless, immediate, constrained, national).

% Legal and political groups who argue that the Equal Protection Clause forbids all racial classifications, regardless of intent. They bear the cost of their preferred interpretation being rejected and actively challenge these policies in court and public discourse.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, colorblind_advocates, payer,
    organized, biographical, constrained, national).

% Advocacy groups, educators, and policymakers who actively support and defend race-conscious policies, benefiting from the ability to implement their vision of inclusive and diverse educational environments.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, diversity_policy_proponents, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the efforts of educational institutions to achieve a diverse student body, which is believed to enhance the educational experience and prepare students for a diverse society.
% TRANSFER_FUNCTION: Transfers opportunities (e.g., admission slots) from some applicants to others based on race, as one factor among many, to achieve a broader educational benefit for the entire student body. It also transfers administrative and legal burdens to educational institutions.
% ABSENT_VOICES: While 'colorblind advocates' are present in the debate, their fundamental premise (that all racial classifications are forbidden) is rejected by this reading. Individual 'applicants denied admission' often have their voices amplified by advocacy groups, but their direct input into policy formation is limited.
% DISAPPEARANCE_RATIONALE: If this reading of the Equal Protection Clause vanished overnight, educational institutions would immediately cease race-conscious policies. This would likely lead to significantly less diverse student bodies in many institutions, particularly in higher education, and a reorganization of admissions practices around purely race-neutral criteria, altering educational outcomes and experiences.
% FOUNDING_PROBLEM: To address the educational benefits of diversity and ensure a robust learning environment, particularly in the context of historical segregation and the need for a broadly educated citizenry in a diverse democracy.
% FOUNDING_PROBLEM_CORROBORATION: Educational researchers, university administrators, and some legal scholars continue to attest to the ongoing and compelling educational benefits of diversity. This is supported by numerous studies on student outcomes and institutional effectiveness, providing corroboration from outside the immediate beneficiaries of specific policies.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_compelling_interest_empirical_validity,
    'Is ''educational diversity'' truly a compelling governmental interest, and do race-conscious policies effectively achieve it without undue burden, as empirical evidence suggests?',
    'Further longitudinal studies on the educational and societal outcomes of diverse student bodies, and rigorous analysis of the efficacy and unintended consequences of race-conscious policies versus race-neutral alternatives.',
    'If the empirical basis for diversity as a compelling interest weakens, or if race-neutral alternatives are shown to be equally effective, the justification for this reading of the Equal Protection Clause would erode, potentially shifting its classification towards a Snare or Piton for those bearing its costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_compelling_interest_empirical_validity, empirical, 'Empirical validity of diversity as a compelling interest and efficacy of race-conscious policies.').

omega_variable(
    instrumentalization_dignity_reconciliation,
    'Is the instrumentalization of minority students (as a means to achieve diversity for the benefit of all students) consistent with the dignity principle of equal protection, or does it create a new form of racial harm?',
    'Philosophical and legal analysis of the concept of dignity in constitutional law, and qualitative studies on the lived experiences of minority students in institutions with diversity policies.',
    'If the instrumentalization is found to violate the dignity principle, the ethical foundation of this reading would be severely undermined, increasing its perceived extractiveness and potentially shifting its classification towards a Snare from the perspective of instrumentalized groups.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumentalization_dignity_reconciliation, conceptual, 'Reconciliation of instrumentalization with the dignity principle of equal protection.').

omega_variable(
    kernel_reading_contest_location,
    'This constraint is the ''diversity_reading'' of the ''equal_protection_clause'' kernel. Where is the core disagreement located between this reading and its siblings (''colorblind_reading'' and ''remedial_reading'')?',
    'Analysis of judicial opinions, legal scholarship, and public discourse surrounding the Equal Protection Clause, identifying the specific constitutional principles and interpretive methodologies that differentiate each reading.',
    'The location of the disagreement (e.g., whether the clause is about individual rights or group outcomes, or whether race can ever be a factor) determines the structural stability of each reading and its potential to foreclose or coexist with others. This reading''s core premise directly contradicts the colorblind reading''s absolute prohibition on racial classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Core disagreement between diversity, colorblind, and remedial readings of the Equal Protection Clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__diversity_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(equa_tr_t1987, equal_protection_clause__diversity_reading, theater_ratio, 1987, 0.12).
narrative_ontology:measurement(equa_tr_t1996, equal_protection_clause__diversity_reading, theater_ratio, 1996, 0.13).
narrative_ontology:measurement(equa_tr_t2005, equal_protection_clause__diversity_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(equa_tr_t2014, equal_protection_clause__diversity_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__diversity_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.35).
narrative_ontology:measurement(equa_be_t1987, equal_protection_clause__diversity_reading, base_extractiveness, 1987, 0.38).
narrative_ontology:measurement(equa_be_t1996, equal_protection_clause__diversity_reading, base_extractiveness, 1996, 0.41).
narrative_ontology:measurement(equa_be_t2005, equal_protection_clause__diversity_reading, base_extractiveness, 2005, 0.43).
narrative_ontology:measurement(equa_be_t2014, equal_protection_clause__diversity_reading, base_extractiveness, 2014, 0.44).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__diversity_reading, base_extractiveness, 2023, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.45).
narrative_ontology:measurement(equa_su_t1987, equal_protection_clause__diversity_reading, suppression_requirement, 1987, 0.48).
narrative_ontology:measurement(equa_su_t1996, equal_protection_clause__diversity_reading, suppression_requirement, 1996, 0.51).
narrative_ontology:measurement(equa_su_t2005, equal_protection_clause__diversity_reading, suppression_requirement, 2005, 0.53).
narrative_ontology:measurement(equa_su_t2014, equal_protection_clause__diversity_reading, suppression_requirement, 2014, 0.54).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__diversity_reading, suppression_requirement, 2023, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'equal_protection_clause' kernel. Its ε value and structural properties differ significantly from the 'colorblind_reading' and 'remedial_reading', necessitating separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
