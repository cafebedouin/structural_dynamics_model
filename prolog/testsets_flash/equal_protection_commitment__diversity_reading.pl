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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection: Diversity as Compelling State Interest (Diversity Reading)
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'diversity reading' of the Equal
 *   Protection Clause, which permits race to be considered as one factor
 *   among many in university admissions to achieve educational diversity as a
 *   compelling state interest. This reading, established in Regents of the
 *   University of California v. Bakke (1978) and reaffirmed in Grutter v.
 *   Bollinger (2003), grants universities discretion in crafting holistic
 *   review processes. The constraint is procedural, focusing on how race can
 *   be used, rather than mandating specific outcomes. Its status was
 *   significantly altered by Students for Fair Admissions v. Harvard/UNC
 *   (2023), which largely foreclosed this reading, but for the purpose of
 *   this story, we analyze its operational period up to that point.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.35).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection: Diversity as Compelling State Interest (Diversity Reading)").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, '37ded735-a532-4ba6-b38d-d13de0a9d6c4').
narrative_ontology:cs_kernel_codification('37ded735-a532-4ba6-b38d-d13de0a9d6c4', fixed_text).
narrative_ontology:cs_authority_grounding('37ded735-a532-4ba6-b38d-d13de0a9d6c4', lineage).
narrative_ontology:cs_interpretation_layer_present('37ded735-a532-4ba6-b38d-d13de0a9d6c4').
narrative_ontology:cs_reading_relation('37ded735-a532-4ba6-b38d-d13de0a9d6c4', equal_protection_commitment__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('37ded735-a532-4ba6-b38d-d13de0a9d6c4', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('37ded735-a532-4ba6-b38d-d13de0a9d6c4', foundational, diversity_is_compelling_state_interest).
narrative_ontology:cs_axiom_status(diversity_is_compelling_state_interest, overridden).
narrative_ontology:cs_axiom_grounding('37ded735-a532-4ba6-b38d-d13de0a9d6c4', diversity_is_compelling_state_interest, instrumental).
narrative_ontology:cs_axiom('37ded735-a532-4ba6-b38d-d13de0a9d6c4', foundational, race_as_one_factor_among_many_is_permissible).
narrative_ontology:cs_axiom_status(race_as_one_factor_among_many_is_permissible, overridden).
narrative_ontology:cs_axiom_grounding('37ded735-a532-4ba6-b38d-d13de0a9d6c4', race_as_one_factor_among_many_is_permissible, conventional).
narrative_ontology:cs_reference_frame('37ded735-a532-4ba6-b38d-d13de0a9d6c4', bakke_grutter_precedent).
narrative_ontology:cs_drift_state('37ded735-a532-4ba6-b38d-d13de0a9d6c4', sfaf_harvard_unc_ruling, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('37ded735-a532-4ba6-b38d-d13de0a9d6c4', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, universities_seeking_diversity).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, students_from_underrepresented_groups).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, all_applicants_to_universities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, civil_rights_advocates).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, holistic_review_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, academic_freedom_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions interpret the Equal Protection Clause to allow consideration of race as one factor in admissions to achieve a diverse student body, which they deem essential to their educational mission. They actively design and defend admissions policies that implement this reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, universities_seeking_diversity, agenda_setter,
    institutional, generational, constrained, national).

% All individuals applying to universities are subject to admissions processes that may consider race as one factor among many. While no individual is explicitly excluded based on race, the holistic review process can obscure the specific impact of race on individual applications, leading to a diffuse sense of bearing costs for some.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, all_applicants_to_universities, payer,
    powerless, biographical, constrained, national).

% Students from racial or ethnic groups historically underrepresented in higher education may benefit from policies that consider race as a positive factor in admissions, increasing their access to diverse educational environments. Their benefit is contingent on the university's commitment to diversity.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, students_from_underrepresented_groups, beneficiary,
    moderate, biographical, constrained, national).

% The ultimate arbiter of the Equal Protection Clause, whose rulings define the permissible scope of race-conscious admissions. Its decisions shape the legal landscape within which universities operate, and its interpretations are binding.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Advocate for policies that promote racial diversity in education, viewing it as a means to achieve broader societal equality and dismantle historical disadvantages. They support the diversity reading as a tool for progress.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Argue that any consideration of race in admissions violates the Equal Protection Clause, advocating for a strictly 'colorblind' approach. They are structurally excluded from the diversity reading's framework, as their core premise directly contradicts it.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, colorblind_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the admissions practices of universities with the constitutional mandate of equal protection, allowing for the pursuit of educational diversity while adhering to legal limits on racial classification.
% TRANSFER_FUNCTION: Transfers discretion to universities to consider race as one factor in admissions, potentially shifting opportunities among applicants based on their racial background as part of a holistic review process.
% ABSENT_VOICES: Advocates for a strictly colorblind interpretation of the Equal Protection Clause are absent from the internal logic of this reading, as their foundational premise (no racial classification ever) is incompatible with the diversity rationale. They would argue that any race-conscious policy is inherently discriminatory.
% DISAPPEARANCE_RATIONALE: If this reading of equal protection vanished, universities would lose the legal basis for race-conscious admissions, forcing a complete overhaul of their diversity strategies. The composition of student bodies would likely shift, and the legal landscape for affirmative action would be fundamentally altered.
% FOUNDING_PROBLEM: The problem of achieving a diverse student body and educational environment, deemed essential for robust learning and preparing students for a diverse society, while navigating the constitutional prohibition against racial discrimination.
% FOUNDING_PROBLEM_CORROBORATION: Universities, educational researchers, and many civil rights organizations attest that the problem of achieving meaningful diversity in higher education remains live, citing ongoing societal inequalities and the educational benefits of diverse learning environments. This is corroborated by extensive academic literature and institutional statements.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).

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
 *   Extractiveness is low-moderate (0.28) because the constraint is primarily procedural, granting discretion rather than imposing heavy costs, and the 'victim' set (all applicants) experiences diffuse rather than concentrated extraction. Suppression is moderate (0.35) as it requires active legal defense against challenges but doesn't overtly coerce. Theater ratio is low (0.15) as universities genuinely pursue diversity, though the legal justifications can sometimes be performative. The temporal measurements reflect the period of its active legal life, showing relatively stable metrics until its effective foreclosure in 2023.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of universities and civil rights advocates, this reading is a necessary tool for achieving educational equity and societal benefit. From the perspective of colorblind advocates, it is an illegitimate racial classification that violates the core principle of equal protection. Applicants experience it as a complex, opaque process where individual merit is balanced against institutional goals.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities seeking diversity are beneficiaries (gain discretion, fulfill mission). Students from underrepresented groups are also beneficiaries (gain access). All applicants are diffuse payers (subject to a complex process where race is a factor, potentially disadvantaging some non-minority applicants). The Supreme Court acts as an agenda-setter, defining the boundaries of this reading. Civil rights advocates are beneficiaries, while colorblind advocates are excluded.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_vs_colorblind_legitimacy,
    'Is educational diversity a sufficiently compelling state interest to justify any race-conscious admissions policy, or does the Equal Protection Clause demand strict colorblindness?',
    'Further Supreme Court rulings or constitutional amendment clarifying the scope of equal protection regarding race.',
    'If colorblindness is strictly enforced, this reading is foreclosed, and universities lose a tool for diversity. If diversity is affirmed as compelling, this reading''s legitimacy is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diversity_vs_colorblind_legitimacy, conceptual, 'The fundamental legal and philosophical disagreement over the interpretation of the Equal Protection Clause.').

omega_variable(
    holistic_review_opacity,
    'To what extent does ''holistic review'' genuinely integrate race as ''one factor among many'' versus allowing it to function as a determinative factor for certain applicants?',
    'Empirical analysis of admissions data, disaggregated by race and other factors, to determine the statistical weight of race in admissions decisions.',
    'If race is found to be a determinative factor, the constraint''s extractiveness and suppression would be higher, potentially reclassifying it as a Tangled Rope or Snare due to its disproportionate impact on certain groups. If it truly functions as one factor among many, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holistic_review_opacity, empirical, 'The practical implementation of race-conscious admissions policies.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine reading of the Equal Protection Clause, or a policy choice dressed in constitutional language to achieve social goals?',
    'Legal scholarship and judicial opinions that rigorously trace the interpretive lineage of the clause, distinguishing between constitutional interpretation and policy advocacy.',
    'If it is primarily a policy choice, its ''mountain-like'' claim to constitutional necessity is weakened, and its classification would shift towards a more constructed type (e.g., Rope or Tangled Rope) with higher effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between constitutional interpretation and policy choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__diversity_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(equa_tr_t1992, equal_protection_commitment__diversity_reading, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(equa_tr_t2006, equal_protection_commitment__diversity_reading, theater_ratio, 2006, 0.15).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__diversity_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__diversity_reading, base_extractiveness, 1978, 0.2).
narrative_ontology:measurement(equa_be_t1992, equal_protection_commitment__diversity_reading, base_extractiveness, 1992, 0.25).
narrative_ontology:measurement(equa_be_t2006, equal_protection_commitment__diversity_reading, base_extractiveness, 2006, 0.3).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__diversity_reading, base_extractiveness, 2023, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__diversity_reading, suppression_requirement, 1978, 0.3).
narrative_ontology:measurement(equa_su_t1992, equal_protection_commitment__diversity_reading, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(equa_su_t2006, equal_protection_commitment__diversity_reading, suppression_requirement, 2006, 0.4).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__diversity_reading, suppression_requirement, 2023, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
