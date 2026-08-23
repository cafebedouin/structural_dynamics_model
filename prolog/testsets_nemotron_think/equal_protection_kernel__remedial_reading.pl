% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__remedial_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Clause — Remedial Reading (Race-Conscious Remediation Permitted)
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   The remedial reading of the Equal Protection Clause (derived from Justice
 *   Powell's opinion in Bakke and refined in Grutter) permits state
 *   universities to use race as a plus factor in admissions when narrowly
 *   tailored to serve a compelling interest in diversity or remediation of
 *   historical exclusion. This reading structures the admissions constraint
 *   for selective public universities nationwide. It creates a coordination
 *   function (diverse educational environments) and an extraction function
 *   (displacement of some non-minority applicants). The constraint is
 *   actively enforced by federal courts through strict scrutiny review.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.6).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.8).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Clause — Remedial Reading (Race-Conscious Remediation Permitted)").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, '6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298').
narrative_ontology:cs_kernel_codification('6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298', fixed_text).
narrative_ontology:cs_authority_grounding('6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298', lineage).
narrative_ontology:cs_interpretation_layer_present('6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298').
narrative_ontology:cs_reading_relation('6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298', foundational, race_conscious_remedial_action_permitted).
narrative_ontology:cs_axiom_status(race_conscious_remedial_action_permitted, holdable).
narrative_ontology:cs_axiom_grounding('6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298', race_conscious_remedial_action_permitted, conventional).
narrative_ontology:cs_axiom('6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298', secondary, narrow_tailoring_requirement).
narrative_ontology:cs_axiom_status(narrow_tailoring_requirement, holdable).
narrative_ontology:cs_axiom_grounding('6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298', narrow_tailoring_requirement, conventional).
narrative_ontology:cs_reference_frame('6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298', bakke_remedial_framework).
narrative_ontology:cs_drift_state('6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298', contemporary_affirmative_action_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6c1b3cfb-cd8b-4bcc-b0e3-d109c9cf5298', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_racial_minorities).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, non_minority_applicants_displaced_by_race_conscious_policy).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, diversity_compelling_state_interest).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, remedial_justification_for_race_conscious_action).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Public universities design and implement admissions policies that consider race as a plus factor. They must document remedial purpose and narrow tailoring to survive strict scrutiny. They bear the administrative burden of compliance and risk litigation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, state_universities, agenda_setter,
    institutional, generational, constrained, national).

% Applicants from groups historically excluded from higher education receive a contextual advantage in admissions. The constraint enables their access to selective institutions, but they remain subject to the same competitive pool and cannot individually opt out of the racial classification.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_racial_minorities, beneficiary,
    organized, biographical, constrained, national).

% Applicants who would have been admitted under a race-blind process but are rejected because seats are allocated to historically excluded groups. They bear the cost of the constraint directly. Their exit options are limited: they can attend less selective institutions, forgo higher education, or litigate.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, non_minority_applicants_displaced_by_race_conscious_policy, payer,
    moderate, biographical, constrained, national).

% Advocates and litigants who argue the Constitution categorically forbids racial classifications. They are structurally excluded from the remedial framework because the reading treats colorblindness as an impermissible baseline. They pursue judicial overturning of the reading.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, colorblind_advocates, excluded,
    organized, generational, trapped, national).

% Courts adjudicate whether a given race-conscious policy satisfies strict scrutiny. They do not directly benefit or pay but their doctrinal choices shape the constraint's operation. They can narrow, expand, or eliminate the reading.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% Scholars and activists who view the Equal Protection Clause as targeting caste-like subordination. They broadly support race-conscious remediation but criticize the remedial reading's narrow diversity rationale as insufficiently transformative. They participate in discourse but are not direct beneficiaries or payers of the admissions constraint.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, antisubordination_advocates, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the pursuit of diverse student bodies and remedial justice in higher education by permitting institutions to consider race as one factor among many, thereby addressing historical exclusion without imposing rigid quotas.
% TRANSFER_FUNCTION: Moves admission offers from non-minority applicants who would have been admitted under a race-blind process to historically excluded minority applicants, using race as a contextual plus factor within a holistic review.
% ABSENT_VOICES: Colorblind constitutionalists who argue any racial classification is forbidden; prospective applicants who would benefit from a purely meritocratic (race-blind) process but have no standing in the current framework; institutions in states with bans on affirmative action that are forced into race-blind alternatives.
% DISAPPEARANCE_RATIONALE: If the remedial reading vanished overnight, public universities would be forced into race-blind admissions (or prohibited from considering race), leading to immediate drops in underrepresented minority enrollment at selective institutions, shifts in institutional diversity strategies, and a wave of litigation over what substitutes are permissible.
% FOUNDING_PROBLEM: Persistent racial stratification in access to elite public higher education resulting from centuries of slavery, segregation, and discriminatory policies that excluded Black, Latino, and Native American students.
% FOUNDING_PROBLEM_CORROBORATION: Historical records (e.g., university segregation policies, Brown v. Board) and sociological studies (e.g., Bowen & Bok, 'The Shape of the River') corroborate the founding problem's existence. However, the colorblind reading and some empirical studies contest whether the problem persists in a form that justifies current race-conscious remedies, arguing that class-based alternatives could achieve similar diversity without racial classification.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.6, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6) reflects that a measurable but minority share of admission slots are reallocated by race-conscious policy. Suppression (0.8) is high because the constraint's persistence depends on continuous judicial enforcement and the structural exclusion of race-blind alternatives as legally impermissible for institutions seeking diversity. Theater ratio (0.3) indicates the diversity rationale is genuinely operational but increasingly performative as courts narrow the permissible scope. Accessibility collapse (0.7) is high because once the remedial framework is accepted, race-blind alternatives are legally foreclosed for institutions that wish to pursue diversity. Resistance (0.7) is high due to sustained litigation, state bans, and political opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the university's seat, the constraint is a coordination mechanism they administer; from the displaced applicant's seat, it is an extractive barrier; from the beneficiary's seat, it is a corrective opportunity. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State universities are agenda setters (they design policy but operate under judicial oversight). Historically excluded minorities are beneficiaries (they receive admission advantages). Non-minority applicants displaced by the policy are payers (they lose seats they would otherwise have obtained). Colorblind advocates are excluded (their preferred regime is legally unavailable). Courts are observers (they adjudicate but do not directly gain or lose). Antisubordination advocates are observers (they influence discourse but are not direct parties).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (historical exclusion) is contested as to whether it persists in a form that justifies the current remedy. If the problem is dead but the constraint persists, mandatrophy may be unresolved. The constraint's continuation relies on the diversity rationale, which has shifted from remediation to educational benefits — a potential mandate drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_vs_colorblind_boundary,
    'Does the remedial reading logically foreclose the colorblind reading within a single constitutional framework, or can a jurisdiction adopt a hybrid approach?',
    'Supreme Court precedent analysis: if a majority opinion explicitly holds that the two readings are mutually exclusive, foreclosure is confirmed. If dissenting opinions or lower courts articulate workable hybrids, coexistence is possible.',
    'If foreclosure holds, the remedial reading''s legitimacy depends entirely on defeating the colorblind reading; if coexistence is possible, the constraint''s extraction may be modulated by jurisdictional choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_vs_colorblind_boundary, conceptual, 'Whether the remedial and colorblind readings are structurally incompatible in a single legal system.').

omega_variable(
    narrow_tailoring_measurement,
    'How should ''narrow tailoring'' be measured to distinguish genuine coordination from pretextual extraction?',
    'Empirical study of admissions outcomes under different tailoring regimes; comparison of racial composition changes when race-conscious policies are banned vs. when they are permitted with varying tailoring strictness.',
    'If narrow tailoring cannot be operationally defined, the constraint''s coordination function is undermined and extraction dominates. If measurable, the constraint''s tangled_rope character is empirically grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrow_tailoring_measurement, empirical, 'Operationalizing the constitutional requirement that race-conscious policies be narrowly tailored.').

omega_variable(
    committer_structure_kernel_reading,
    'How does the remedial reading''s structural relationship to the equal_protection_kernel affect its classification stability across sibling readings?',
    'Cross-reading comparison of ε, beneficiary/victim sets, and directionality profiles. If sibling readings produce divergent classifications for the same kernel, the kernel''s structural ambiguity is confirmed.',
    'If the kernel admits multiple stable constraint types, the classification of any single reading is inherently perspectival and the kernel itself becomes an object of study.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment-system framing of the equal protection kernel and its readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__remedial_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_kernel__remedial_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__remedial_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_kernel__remedial_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_kernel__remedial_reading, theater_ratio, 2016, 0.32).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__remedial_reading, theater_ratio, 2023, 0.3).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.35).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__remedial_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__remedial_reading, base_extractiveness, 2003, 0.55).
narrative_ontology:measurement(equa_be_t2010, equal_protection_kernel__remedial_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(equa_be_t2016, equal_protection_kernel__remedial_reading, base_extractiveness, 2016, 0.62).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__remedial_reading, base_extractiveness, 2023, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.5).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__remedial_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__remedial_reading, suppression_requirement, 2003, 0.75).
narrative_ontology:measurement(equa_su_t2010, equal_protection_kernel__remedial_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(equa_su_t2016, equal_protection_kernel__remedial_reading, suppression_requirement, 2016, 0.82).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__remedial_reading, suppression_requirement, 2023, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__remedial_reading, 0.08).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, university_admissions_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the equal_protection_kernel. The remedial reading permits race-conscious remediation; the colorblind reading forbids all racial classification; the antisubordination reading permits race-conscious action to dismantle hierarchy. They form a constraint family linked by the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__remedial_reading, institutional, 0.2).
constraint_indexing:directionality_override(equal_protection_kernel__remedial_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
