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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   obtaining the educational benefits that flow from a diverse student body.
 *   This reading frames diversity as benefiting all students, including
 *   non-minorities, and views minority students as instrumental to achieving
 *   this broader educational goal. It is a contested interpretation within
 *   constitutional law.
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
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection Clause: Diversity Interest Reading").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, '60b6ad90-74e0-4006-9685-28ec5402fcc0').
narrative_ontology:cs_kernel_codification('60b6ad90-74e0-4006-9685-28ec5402fcc0', fixed_text).
narrative_ontology:cs_authority_grounding('60b6ad90-74e0-4006-9685-28ec5402fcc0', lineage).
narrative_ontology:cs_interpretation_layer_present('60b6ad90-74e0-4006-9685-28ec5402fcc0').
narrative_ontology:cs_reading_relation('60b6ad90-74e0-4006-9685-28ec5402fcc0', equal_protection_clause__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('60b6ad90-74e0-4006-9685-28ec5402fcc0', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('60b6ad90-74e0-4006-9685-28ec5402fcc0', foundational, diversity_is_compelling_educational_interest).
narrative_ontology:cs_axiom_status(diversity_is_compelling_educational_interest, holdable).
narrative_ontology:cs_axiom_grounding('60b6ad90-74e0-4006-9685-28ec5402fcc0', diversity_is_compelling_educational_interest, empirically_contingent).
narrative_ontology:cs_axiom('60b6ad90-74e0-4006-9685-28ec5402fcc0', foundational, race_can_be_one_factor_among_many).
narrative_ontology:cs_axiom_status(race_can_be_one_factor_among_many, holdable).
narrative_ontology:cs_axiom_grounding('60b6ad90-74e0-4006-9685-28ec5402fcc0', race_can_be_one_factor_among_many, conventional).
narrative_ontology:cs_reference_frame('60b6ad90-74e0-4006-9685-28ec5402fcc0', grutter_v_bollinger_precedent).
narrative_ontology:cs_drift_state('60b6ad90-74e0-4006-9685-28ec5402fcc0', sfafa_v_harvard_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('60b6ad90-74e0-4006-9685-28ec5402fcc0', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, educational_institutions).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, non_minority_applicants_denied_admission).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, minority_applicants_instrumentalized).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, minority_applicants_instrumentalized).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, civil_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement race-conscious admissions policies to achieve educational diversity, navigating legal challenges and public opinion. They benefit from the perceived educational quality and social legitimacy that diversity brings, but bear the costs of litigation and administrative complexity.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Are theorized to benefit from a richer learning environment, broader perspectives, and better preparation for a diverse workforce and society. This benefit is diffuse and often indirect.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, all_students, beneficiary,
    organized, biographical, mobile, national).

% Are denied admission to preferred institutions where they might otherwise have been admitted, due to race-conscious policies. They bear a direct, concentrated cost in lost opportunity.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, non_minority_applicants_denied_admission, payer,
    moderate, immediate, constrained, local).

% Gain admission to institutions they might not otherwise have, but may experience the cost of being viewed primarily as a means to achieve institutional diversity goals, rather than as individuals. This can lead to feelings of tokenism or pressure to represent their racial group.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_applicants_instrumentalized, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, minority_applicants_instrumentalized, payer).

% Interprets the Equal Protection Clause, setting the legal boundaries for race-conscious policies. Its rulings shape the constraint's operation and legitimacy, acting as the ultimate enforcer and arbiter of its scope.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% Support race-conscious policies as a means to achieve greater equity and opportunity, aligning with the diversity rationale. They benefit from the continued legal viability of such policies.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Oppose any governmental racial classifications, arguing they violate the principle of individual equality. They are structurally excluded from the diversity reading's framework, as their core premise directly contradicts it.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, colorblind_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__diversity_reading, educational_institutions).
narrative_ontology:fixing_cost_class(equal_protection_clause__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable educational institutions to assemble diverse student bodies, which is believed to enhance the learning environment and prepare students for a diverse society, by permitting limited consideration of race in admissions.
% TRANSFER_FUNCTION: Transfers educational opportunities (admission slots) from some non-minority applicants to some minority applicants, in service of the broader goal of educational diversity. It also transfers the 'benefit' of a diverse learning environment to all students.
% ABSENT_VOICES: Advocates for a strictly 'colorblind' interpretation of the Equal Protection Clause are structurally absent from the diversity reading's internal logic, as their premise (no racial classifications) directly forecloses the diversity rationale. They would argue that any race-conscious policy is inherently discriminatory.
% DISAPPEARANCE_RATIONALE: If this reading of the Equal Protection Clause disappeared, educational institutions would likely cease race-conscious admissions policies, leading to less diverse student bodies. This would alter the educational experience for many students and shift the demographics of higher education, particularly at elite institutions.
% FOUNDING_PROBLEM: The problem of achieving educational benefits from a diverse student body in a society with persistent racial stratification, while adhering to constitutional principles of equal protection.
% FOUNDING_PROBLEM_CORROBORATION: Educational institutions and many social scientists attest that the problem of achieving educational diversity and its benefits remains live. Critics (e.g., colorblind advocates) contest the necessity of race-conscious means, but generally acknowledge the value of diversity itself, corroborating the problem's existence, if not the solution's constitutionality.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely aims to coordinate educational benefits (diversity for all students) but does so through a mechanism (race-conscious admissions) that imposes costs on identifiable groups (non-minority applicants denied admission, minority applicants potentially instrumentalized). The 'compelling interest' and 'narrow tailoring' requirements introduce complexity and enforcement overhead. Extractiveness is moderate (0.45) due to the denial of admission to some applicants based on race, while suppression (0.30) reflects the active judicial and institutional enforcement required to maintain these policies against legal challenges and public resistance. Theater ratio is low (0.10) as the policies are genuinely implemented, though their justification is often debated.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of educational institutions and 'all students' (as beneficiaries of diversity), this constraint functions as a Rope, coordinating a valuable educational outcome. However, from the perspective of non-minority applicants denied admission, it operates as a Snare, extracting opportunities based on race. Minority applicants may experience it as a complex Tangled Rope, benefiting from admission but potentially bearing the cost of instrumentalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational institutions and 'all students' are beneficiaries (d near 0.0-0.2) as they gain from the diverse environment. Non-minority applicants denied admission are clear victims (d near 0.8-1.0). Minority applicants are complex: beneficiaries of admission but potential victims of instrumentalization (d near 0.5-0.7). The Supreme Court and lower courts act as agenda-setters and enforcers, mediating the tension.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the extraction from denied applicants) or a pure Snare (ignoring the genuine coordination function of educational diversity). The 'diversity interest' is presented as an ongoing, permanent educational value, not a temporary remedial measure, thus avoiding a Scaffold classification. The ongoing contestation and active enforcement prevent it from becoming a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_interest_compelling_ambiguity,
    'Is the educational diversity interest truly ''compelling'' and does it genuinely benefit all students, or is this a post-hoc rationalization for other goals?',
    'Empirical studies on the long-term educational and societal outcomes for students from diverse vs. non-diverse institutions, controlling for other factors. Judicial review of ''narrow tailoring'' requirements.',
    'If the interest is not compelling or benefits are not universal, the constraint shifts towards a Snare, as the coordination story (universal benefit) collapses, revealing pure extraction from those denied admission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_interest_compelling_ambiguity, empirical, 'Ambiguity regarding the ''compelling'' nature and universal benefit of diversity interests.').

omega_variable(
    instrumentalization_of_minority_students,
    'Does the diversity reading of the Equal Protection Clause instrumentalize minority students as means to an end (benefiting the majority), rather than treating them as ends in themselves?',
    'Analysis of institutional practices and outcomes: do institutions invest equally in the success of all students, or are minority students disproportionately burdened with ''diversity work'' or tokenism?',
    'If instrumentalization is pervasive, the constraint''s extractiveness from minority students is higher than currently measured, and the ''beneficiary'' status of ''all students'' becomes more tenuous, pushing the constraint towards a Snare for minority students.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(instrumentalization_of_minority_students, conceptual, 'Whether minority students are instrumentalized under the diversity rationale.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''diversity_reading'' of the ''equal_protection_clause'' kernel. How would the classification change if a ''colorblind_reading'' or ''remedial_reading'' were adopted?',
    'Conceptual analysis of the alternative legal frameworks and their implications for race-conscious policies.',
    'A ''colorblind_reading'' would likely classify any race-conscious policy as a Snare due to its inherent extraction from individuals based on race. A ''remedial_reading'' would classify policies aimed at historical redress as a Rope or Scaffold, with different beneficiaries and a potential sunset clause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative readings of the Equal Protection Clause kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__diversity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(equa_tr_t10, equal_protection_clause__diversity_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(equa_tr_t20, equal_protection_clause__diversity_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__diversity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(equa_be_t10, equal_protection_clause__diversity_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(equa_be_t20, equal_protection_clause__diversity_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__diversity_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(equa_su_t10, equal_protection_clause__diversity_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(equa_su_t20, equal_protection_clause__diversity_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, identity_coordination).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, affirmative_action_policies__higher_education).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Equal Protection Clause kernel, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
