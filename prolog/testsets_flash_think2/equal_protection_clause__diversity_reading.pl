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
 *   human_readable: Equal Protection: Diversity Interest Reading
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'diversity reading' of the Equal
 *   Protection Clause, which permits race-conscious policies in higher
 *   education when narrowly tailored to serve a compelling interest in
 *   educational diversity. This reading, established in Regents of the
 *   University of California v. Bakke (1978) and reaffirmed in Grutter v.
 *   Bollinger (2003), framed diversity as a benefit to all students, distinct
 *   from remedial justifications. The metrics reflect the operation of this
 *   reading during its active period, prior to its effective repudiation by
 *   the Supreme Court in Students for Fair Admissions v. Harvard/UNC (2023).
 *
 * KEY AGENTS:
 *   - educational_institutions: Primary agenda_setter (institutional/constrained) — implement and defend policies.
 *   - minority_applicants: Primary payer (powerless/constrained) — instrumentalized for diversity goals.
 *   - white_applicants: Payer (moderate/constrained) — may perceive disadvantage.
 *   - all_students: Primary beneficiary (moderate/constrained) — claimed to benefit from diverse environment.
 *   - supreme_court: Ultimate agenda_setter (institutional/analytical) — defines legal boundaries.
 *   - civil_rights_advocates: Beneficiary (organized/mobile) — support and defend the reading.
 *   - colorblind_advocates: Excluded (organized/mobile) — actively oppose the reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.55).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.6).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection: Diversity Interest Reading").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, '4d9b1144-6132-427d-a7bb-43885fd593f0').
narrative_ontology:cs_kernel_codification('4d9b1144-6132-427d-a7bb-43885fd593f0', fixed_text).
narrative_ontology:cs_authority_grounding('4d9b1144-6132-427d-a7bb-43885fd593f0', lineage).
narrative_ontology:cs_interpretation_layer_present('4d9b1144-6132-427d-a7bb-43885fd593f0').
narrative_ontology:cs_reading_relation('4d9b1144-6132-427d-a7bb-43885fd593f0', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('4d9b1144-6132-427d-a7bb-43885fd593f0', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('4d9b1144-6132-427d-a7bb-43885fd593f0', foundational, educational_diversity_compelling_interest).
narrative_ontology:cs_axiom_status(educational_diversity_compelling_interest, overridden).
narrative_ontology:cs_axiom_grounding('4d9b1144-6132-427d-a7bb-43885fd593f0', educational_diversity_compelling_interest, conventional).
narrative_ontology:cs_axiom('4d9b1144-6132-427d-a7bb-43885fd593f0', secondary, race_as_plus_factor_narrowly_tailored).
narrative_ontology:cs_axiom_status(race_as_plus_factor_narrowly_tailored, overridden).
narrative_ontology:cs_axiom_grounding('4d9b1144-6132-427d-a7bb-43885fd593f0', race_as_plus_factor_narrowly_tailored, conventional).
narrative_ontology:cs_reference_frame('4d9b1144-6132-427d-a7bb-43885fd593f0', bakke_plurality_framework).
narrative_ontology:cs_drift_state('4d9b1144-6132-427d-a7bb-43885fd593f0', post_sffa_v_harvard_unc, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('4d9b1144-6132-427d-a7bb-43885fd593f0', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, educational_institutions).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, minority_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, white_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, civil_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement and defend race-conscious admissions policies, navigating complex legal requirements to achieve educational diversity. They benefit from the flexibility this reading provides but bear significant legal and administrative costs.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Are considered for admission based on race as a 'plus factor' to achieve diversity. While potentially benefiting from admission, they are instrumentalized as means to an institutional end, which can carry social and psychological costs.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_applicants, payer,
    powerless, biographical, constrained, national).

% May perceive themselves as disadvantaged by race-conscious policies, even though the diversity rationale claims to benefit all students. Their applications are evaluated within a framework that considers race, potentially leading to higher thresholds for admission.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, white_applicants, payer,
    moderate, biographical, constrained, national).

% Are claimed to benefit from a richer, more robust learning environment fostered by a diverse student body, preparing them for a diverse society and workforce. This benefit is diffuse and long-term.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, all_students, beneficiary,
    moderate, biographical, constrained, national).

% The ultimate arbiter of the Equal Protection Clause, defining the legal boundaries within which race-conscious policies can operate. Its interpretations shape the constraint's existence and enforcement.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% Support this reading as a means to promote racial equality and educational opportunity, even if imperfect. They advocate for its continued application and defend it against legal challenges.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Oppose this reading, arguing that any racial classification by the government is unconstitutional and that individuals should be treated without regard to race. They actively litigate against policies based on this reading.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, colorblind_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows educational institutions to pursue a diverse student body, which is believed to enhance the learning environment for all students, fostering cross-cultural understanding and preparing students for a diverse workforce and society.
% TRANSFER_FUNCTION: Transfers the burden of achieving diversity onto individual applicants (minority students are instrumentalized, white students may face perceived disadvantage) in exchange for the perceived benefits of a diverse educational environment for the broader student body and society. It also transfers legal and administrative costs to educational institutions for defending these policies.
% ABSENT_VOICES: Colorblind advocates (who argue all racial classifications are unconstitutional) and some minority groups (who argue diversity policies instrumentalize them or don't go far enough to address systemic inequality) are often marginalized in the legal framing of this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, educational institutions would lose a key tool for achieving diversity, leading to less diverse student bodies. The legal landscape for affirmative action would shift dramatically, likely towards a stricter colorblind standard, forcing institutions to find entirely race-neutral means or abandon diversity goals.
% FOUNDING_PROBLEM: To reconcile the Equal Protection Clause's prohibition on racial discrimination with the perceived educational benefits of a diverse student body, particularly in higher education, after earlier remedial justifications faced legal challenges.
% FOUNDING_PROBLEM_CORROBORATION: Educational institutions and many social scientists corroborate the ongoing benefits of diversity. Legal scholars and civil rights organizations also attest to the problem of achieving diversity without race-conscious means.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__diversity_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__diversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (fostering educational diversity) but involves asymmetric extraction. Extraction is moderate (0.60 at end of interval) due to the instrumentalization of applicants and the administrative burden on institutions. Suppression is high (0.70) because the legal framework imposes strict scrutiny, requiring institutions to actively defend their policies against challenges and exhaust race-neutral alternatives. Theater ratio is moderate (0.30) as institutions engage in performative 'narrow tailoring' to meet legal requirements, even as the underlying diversity goal is genuine. Resistance is high (0.70) due to persistent legal and political challenges.
 *
 * PERSPECTIVAL GAP:
 *   Educational institutions, as agenda-setters, view this reading as a necessary and legitimate tool for achieving educational goals. However, applicants, particularly minority applicants, may experience it as a form of instrumentalization, where their racial identity is valued more than their individual merit. Colorblind advocates perceive it as an unconstitutional racial preference.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational institutions and civil rights advocates are beneficiaries, gaining flexibility or progress towards equity. Minority and white applicants are payers, bearing the direct and indirect costs of the policy's implementation. The Supreme Court sets the agenda, and colorblind advocates are excluded from the policy's direct operation, actively seeking its dismantling.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint did not suffer from mandatrophy in the traditional sense during its active period. Its mandate (achieving educational diversity) remained live and actively pursued by institutions. However, the legal justification for that mandate faced increasing scrutiny and was ultimately repudiated, leading to a different form of functional obsolescence, captured by the drift_state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compelling_interest_validity,
    'Is the ''compelling interest in educational diversity'' a genuine, measurable educational benefit, or primarily a legal construct to permit race-conscious policies?',
    'Longitudinal empirical studies on the educational and societal outcomes of diverse student bodies, controlling for other factors, and comparative analysis with institutions lacking such policies.',
    'If primarily a legal construct, the coordination function is weaker, increasing effective extraction and pushing the classification closer to a Snare. If robustly empirical, it strengthens the Rope aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelling_interest_validity, empirical, 'Assesses the empirical grounding of the diversity rationale.').

omega_variable(
    instrumentalization_ethics,
    'Does the instrumentalization of minority students to achieve diversity for the benefit of the broader student body constitute an ethical harm or a necessary component of a just society?',
    'Philosophical and ethical analysis of individual rights versus collective goods in the context of historical injustice, and qualitative studies on the lived experiences of instrumentalized students.',
    'If deemed an ethical harm, the extraction component of the constraint is higher, and the classification leans more towards Snare. If deemed a necessary component, the extraction is mitigated by the broader societal benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumentalization_ethics, conceptual, 'Examines the ethical implications of instrumentalizing individuals for diversity.').

omega_variable(
    race_neutral_alternatives_sufficiency,
    'Could race-neutral policies (e.g., socioeconomic affirmative action, targeted outreach) achieve comparable levels of educational diversity without race-conscious means?',
    'Empirical studies comparing diversity outcomes in jurisdictions or institutions that have implemented robust race-neutral alternatives versus those using race-conscious policies.',
    'If race-neutral alternatives are sufficient, the necessity of the race-conscious constraint diminishes, increasing its effective extraction and suppression, as less coercive alternatives exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(race_neutral_alternatives_sufficiency, empirical, 'Evaluates the efficacy of race-neutral alternatives for achieving diversity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__diversity_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_clause__diversity_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__diversity_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_clause__diversity_reading, theater_ratio, 2013, 0.25).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__diversity_reading, theater_ratio, 2023, 0.3).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.45).
narrative_ontology:measurement(equa_be_t1990, equal_protection_clause__diversity_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__diversity_reading, base_extractiveness, 2003, 0.55).
narrative_ontology:measurement(equa_be_t2013, equal_protection_clause__diversity_reading, base_extractiveness, 2013, 0.58).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__diversity_reading, base_extractiveness, 2023, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.5).
narrative_ontology:measurement(equa_su_t1990, equal_protection_clause__diversity_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__diversity_reading, suppression_requirement, 2003, 0.6).
narrative_ontology:measurement(equa_su_t2013, equal_protection_clause__diversity_reading, suppression_requirement, 2013, 0.65).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__diversity_reading, suppression_requirement, 2023, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, identity_coordination).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, affirmative_action_policies).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Equal Protection Clause kernel, each with different structural properties and implications for race-conscious policies. This 'diversity_reading' focuses on educational benefits, while the 'colorblind_reading' forbids all racial classifications, and the 'remedial_reading' focuses on historical redress.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
