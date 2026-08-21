% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [OVERRIDDEN]
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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection: Diversity as Compelling State Interest (Diversity Reading)
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'diversity reading' of the Equal
 *   Protection Clause, primarily established by Regents of the University of
 *   California v. Bakke (1978) and affirmed in Grutter v. Bollinger (2003).
 *   It permitted universities to consider race as one factor among many in a
 *   holistic admissions review to achieve the compelling state interest of
 *   educational diversity. This reading provided a legal pathway for
 *   affirmative action policies focused on diversity, distinct from those
 *   aimed at remedying past discrimination. The metrics reflect its operation
 *   as a procedural constraint with low-moderate extraction, which faced
 *   increasing legal challenges over its lifespan, culminating in its
 *   effective overturning by Students for Fair Admissions v. Harvard/UNC
 *   (2023).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.45).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection: Diversity as Compelling State Interest (Diversity Reading)").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, '47831389-f3df-4530-9767-6454339ce57a').
narrative_ontology:cs_kernel_codification('47831389-f3df-4530-9767-6454339ce57a', fixed_text).
narrative_ontology:cs_authority_grounding('47831389-f3df-4530-9767-6454339ce57a', lineage).
narrative_ontology:cs_interpretation_layer_present('47831389-f3df-4530-9767-6454339ce57a').
narrative_ontology:cs_reading_relation('47831389-f3df-4530-9767-6454339ce57a', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('47831389-f3df-4530-9767-6454339ce57a', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('47831389-f3df-4530-9767-6454339ce57a', foundational, educational_diversity_is_compelling_state_interest).
narrative_ontology:cs_axiom_status(educational_diversity_is_compelling_state_interest, overridden).
narrative_ontology:cs_axiom_grounding('47831389-f3df-4530-9767-6454339ce57a', educational_diversity_is_compelling_state_interest, conventional).
narrative_ontology:cs_axiom('47831389-f3df-4530-9767-6454339ce57a', secondary, race_as_one_factor_in_holistic_review_is_permissible).
narrative_ontology:cs_axiom_status(race_as_one_factor_in_holistic_review_is_permissible, overridden).
narrative_ontology:cs_axiom_grounding('47831389-f3df-4530-9767-6454339ce57a', race_as_one_factor_in_holistic_review_is_permissible, conventional).
narrative_ontology:cs_reference_frame('47831389-f3df-4530-9767-6454339ce57a', holistic_review_framework).
narrative_ontology:cs_drift_state('47831389-f3df-4530-9767-6454339ce57a', post_sfaf_harvard_unc_ruling, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('47831389-f3df-4530-9767-6454339ce57a', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, universities_seeking_diversity).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, all_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, civil_rights_advocates).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, educational_diversity_as_compelling_state_interest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions implement admissions policies that consider race as one factor among many in a holistic review process to achieve a diverse student body. They benefit from the discretion to shape their student populations according to their educational mission, but operate under strict legal scrutiny.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, universities_seeking_diversity, agenda_setter,
    institutional, generational, constrained, national).

% Individuals applying to universities that employ race-conscious admissions. While the process is holistic, the consideration of race as a factor means that individual claims of merit may be obscured or weighed differently, leading to a perception of bearing costs through a less transparent process.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, all_applicants, payer,
    moderate, biographical, constrained, national).

% The ultimate arbiter of the Equal Protection Clause, whose rulings define the permissible scope of race-conscious policies. Its decisions establish the legal framework that universities must follow.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Organizations and individuals who support the use of race-conscious measures to achieve educational diversity, viewing it as essential for social equity and educational quality. They benefit from the legal framework that permits such policies.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Organizations and individuals who argue that the Constitution forbids any consideration of race in state action, including university admissions. They are structurally excluded from the legitimacy claims of this reading, though they actively challenge it in litigation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, colorblind_advocates, excluded,
    organized, generational, mobile, national).

% Academics who analyze the legal doctrines, historical context, and societal impact of equal protection jurisprudence. They provide critical commentary and theoretical frameworks but do not directly implement or enforce the constraint.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a legal framework for universities to pursue the compelling state interest of educational diversity through race-conscious admissions, while adhering to the Equal Protection Clause.
% TRANSFER_FUNCTION: Transfers discretion in admissions policy from a purely individual-merit basis to universities, allowing them to consider race as one factor among many. It also transfers the social benefits of diversity to the student body and society at large, while potentially obscuring individual applicants' claims.
% ABSENT_VOICES: Advocates for a strictly colorblind interpretation of the Equal Protection Clause are absent from the internal logic of this reading, as their core premise directly contradicts the permissibility of race-conscious admissions for diversity. While present in litigation, their perspective is not accommodated within this reading's framework.
% DISAPPEARANCE_RATIONALE: If this legal interpretation vanished overnight, universities would lose the legal basis for considering race in admissions for diversity purposes. This would necessitate a fundamental reorganization of admissions policies, likely leading to significant shifts in student demographics and institutional missions, particularly for institutions committed to diversity.
% FOUNDING_PROBLEM: To reconcile the constitutional mandate of equal protection with the societal goal of achieving educational diversity in higher education, particularly after the dismantling of de jure segregation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, educational administrators, and civil rights organizations attest that the underlying tension between equal protection and diversity goals remains a live and complex challenge, even after the legal status of this specific reading has changed. This is corroborated by ongoing public debate and policy efforts.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.28) is low-moderate because the constraint is procedural, guiding how diversity can be pursued rather than imposing a direct, heavy burden. However, it still involves a transfer of discretion from individual applicants to institutions, which can be perceived as extractive by some. Suppression (0.45) is moderate, reflecting the active legal enforcement required to maintain the delicate balance of 'one factor among many' and the ongoing litigation it faced. Theater ratio (0.10) is low, as the legal standard was genuinely applied and litigated, not merely performative. Accessibility collapse (0.40) is moderate, as alternatives to race-conscious admissions (e.g., race-neutral policies) were always present but constrained by the desire to achieve diversity. Resistance (0.50) was significant and persistent, leading to numerous legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   Universities seeking diversity experienced this as a necessary, albeit complex, coordination mechanism to fulfill their educational missions. Applicants, particularly those who felt disadvantaged by race-conscious policies, experienced it as a form of extraction or unfairness. Civil rights advocates saw it as a vital tool for equity, while colorblind advocates viewed it as a violation of constitutional principles. The engine's per-seat classification would capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities seeking diversity are the primary beneficiaries, gaining the legal discretion to implement policies aligned with their mission. All applicants are the payers, as the holistic review process, while aiming for a broader good, can obscure individual merit claims and create perceived costs. The Supreme Court acts as the agenda-setter, defining the boundaries of permissible action. Civil rights advocates are beneficiaries, supporting the policy, while colorblind advocates are excluded from the legitimacy of this specific reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_factor_vs_determinative_factor,
    'To what extent was ''race as one factor among many'' genuinely applied as a non-determinative element, versus functioning as a de facto determinative factor in admissions decisions?',
    'Detailed empirical analysis of admissions data, including statistical modeling to isolate the weight of racial factors, and internal university audits of admissions committee deliberations.',
    'If race was often a determinative factor, the constraint''s effective extractiveness and suppression would be higher, potentially reclassifying it closer to a Snare or Tangled Rope, as the procedural cover would be weaker than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(one_factor_vs_determinative_factor, empirical, 'Ambiguity in the practical application of the ''one factor among many'' standard.').

omega_variable(
    empirical_efficacy_of_diversity,
    'What is the demonstrable empirical impact of educational diversity on learning outcomes, civic engagement, and professional success, as claimed by this reading?',
    'Longitudinal studies tracking student cohorts from diverse and non-diverse institutions, controlling for other variables, to assess the claimed benefits.',
    'If the empirical benefits are weak or non-existent, the ''compelling state interest'' justification would be undermined, shifting the constraint''s classification towards a Snare (if extraction persists without a genuine coordination function) or Piton (if maintained by inertia/theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_efficacy_of_diversity, empirical, 'The empirical grounding of the ''compelling state interest'' in diversity.').

omega_variable(
    conceptual_boundary_of_discrimination,
    'Where is the precise conceptual boundary between permissible race-conscious policy (as defined by this reading) and impermissible racial discrimination?',
    'Further jurisprudential clarification from the Supreme Court or legislative action defining specific permissible and impermissible practices. This is a conceptual rather than empirical resolution.',
    'A clearer boundary would reduce the procedural burden and legal uncertainty for universities, potentially lowering extractiveness and suppression. An inability to define a clear boundary contributed to the eventual overturning of this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conceptual_boundary_of_discrimination, conceptual, 'The conceptual difficulty in distinguishing permissible race-conscious policy from discrimination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__diversity_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(equa_tr_t1988, equal_protection_commitment__diversity_reading, theater_ratio, 1988, 0.11).
narrative_ontology:measurement(equa_tr_t1998, equal_protection_commitment__diversity_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(equa_tr_t2008, equal_protection_commitment__diversity_reading, theater_ratio, 2008, 0.13).
narrative_ontology:measurement(equa_tr_t2018, equal_protection_commitment__diversity_reading, theater_ratio, 2018, 0.14).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__diversity_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__diversity_reading, base_extractiveness, 1978, 0.2).
narrative_ontology:measurement(equa_be_t1988, equal_protection_commitment__diversity_reading, base_extractiveness, 1988, 0.24).
narrative_ontology:measurement(equa_be_t1998, equal_protection_commitment__diversity_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement(equa_be_t2008, equal_protection_commitment__diversity_reading, base_extractiveness, 2008, 0.31).
narrative_ontology:measurement(equa_be_t2018, equal_protection_commitment__diversity_reading, base_extractiveness, 2018, 0.33).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__diversity_reading, base_extractiveness, 2023, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__diversity_reading, suppression_requirement, 1978, 0.4).
narrative_ontology:measurement(equa_su_t1988, equal_protection_commitment__diversity_reading, suppression_requirement, 1988, 0.43).
narrative_ontology:measurement(equa_su_t1998, equal_protection_commitment__diversity_reading, suppression_requirement, 1998, 0.47).
narrative_ontology:measurement(equa_su_t2008, equal_protection_commitment__diversity_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement(equa_su_t2018, equal_protection_commitment__diversity_reading, suppression_requirement, 2018, 0.53).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__diversity_reading, suppression_requirement, 2023, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'equal_protection_commitment' kernel. Each reading instantiates a different constraint with its own structural properties and classification, reflecting different interpretations of the Equal Protection Clause.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
