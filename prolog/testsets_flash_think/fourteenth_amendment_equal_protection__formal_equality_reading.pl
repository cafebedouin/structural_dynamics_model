% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: Formal Equality Reading of 14th Amendment Equal Protection
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'formal equality' reading of the 14th
 *   Amendment's Equal Protection Clause, which prohibits explicit state
 *   racial or status classifications unless justified by a compelling
 *   government interest. This reading treats structural inequality as a
 *   pre-constitutional background condition and views state corrective action
 *   (e.g., affirmative action) as potentially entering the victim set by
 *   creating new classifications. It is one interpretation of a contested
 *   kernel, emphasizing colorblindness and equal treatment over equal
 *   outcomes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.2).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.3).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Formal Equality Reading of 14th Amendment Equal Protection").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, '7f35c92d-5c84-463b-9037-94031a95bd12').
narrative_ontology:cs_kernel_codification('7f35c92d-5c84-463b-9037-94031a95bd12', fixed_text).
narrative_ontology:cs_authority_grounding('7f35c92d-5c84-463b-9037-94031a95bd12', lineage).
narrative_ontology:cs_interpretation_layer_present('7f35c92d-5c84-463b-9037-94031a95bd12').
narrative_ontology:cs_reading_relation('7f35c92d-5c84-463b-9037-94031a95bd12', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('7f35c92d-5c84-463b-9037-94031a95bd12', foundational, state_colorblindness_mandate).
narrative_ontology:cs_axiom_status(state_colorblindness_mandate, holdable).
narrative_ontology:cs_axiom_grounding('7f35c92d-5c84-463b-9037-94031a95bd12', state_colorblindness_mandate, deontological).
narrative_ontology:cs_reference_frame('7f35c92d-5c84-463b-9037-94031a95bd12', post_civil_war_neutrality).
narrative_ontology:cs_drift_state('7f35c92d-5c84-463b-9037-94031a95bd12', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7f35c92d-5c84-463b-9037-94031a95bd12', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, individuals_seeking_race_neutral_treatment).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, those_opposed_to_affirmative_action).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_actors_implementing_race_conscious_remedies).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, beneficiaries_of_affirmative_action_programs).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of the Equal Protection Clause, interpreting its meaning and applying it to state and federal actions. This reading emphasizes judicial restraint and a focus on explicit classifications.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from the legal principle that the state should not classify them based on race or other protected characteristics, ensuring equal treatment under the law regardless of group identity. They rely on the courts to enforce this neutrality.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, individuals_seeking_race_neutral_treatment, beneficiary,
    moderate, biographical, mobile, national).

% Advocate for and benefit from the constraint's application to policies like affirmative action, viewing such policies as 'reverse discrimination' that violates the principle of formal equality. They actively litigate to enforce this interpretation.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, those_opposed_to_affirmative_action, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of being prohibited from using explicit racial classifications, even when attempting to address historical or structural inequalities. They must find race-neutral means to achieve diversity or equity goals, which are often less effective or more complex.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_actors_implementing_race_conscious_remedies, payer,
    institutional, biographical, constrained, national).

% Experience the constraint as a barrier to policies designed to counteract systemic disadvantages. They lose access to programs or opportunities that would explicitly consider race to promote diversity or address past discrimination, making upward mobility more difficult.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, beneficiaries_of_affirmative_action_programs, payer,
    powerless, biographical, constrained, national).

% Argue that the formal equality reading fails to address structural racism and perpetuates existing hierarchies. Their calls for an 'anti-caste' interpretation, which would permit or require race-conscious remedies, are largely excluded from the dominant legal framework of this reading.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, civil_rights_advocates_anti_caste, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__formal_equality_reading, diffuse).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__formal_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action by establishing a clear legal standard: explicit racial or status classifications by the state are presumptively unconstitutional and require compelling justification, thereby guiding legislative and executive policy-making towards race-neutrality.
% TRANSFER_FUNCTION: Transfers the burden of achieving substantive equality from the state (by limiting race-conscious remedies) to individuals and civil society. It transfers the benefit of formal legal neutrality to all individuals, while implicitly transferring the cost of persistent structural inequality to historically marginalized groups.
% ABSENT_VOICES: Advocates for an anti-caste reading of Equal Protection, who argue that formal equality alone perpetuates structural inequality, are often marginalized in legal discourse dominated by formalist interpretations. Their perspective, which would prioritize dismantling hierarchy over formal neutrality, is not given equal weight.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, state and federal courts would lack a clear, widely accepted framework for evaluating racial classifications. This would likely lead to a proliferation of race-conscious policies, legal uncertainty, and a fundamental reorganization of civil rights law and policy, altering how government interacts with issues of race and status.
% FOUNDING_PROBLEM: The 14th Amendment was ratified to ensure legal equality for newly freed slaves post-Civil War, preventing states from enacting discriminatory laws based on race and guaranteeing 'equal protection of the laws' for all citizens.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of formal equality argue the founding problem of explicit state-sponsored discrimination is largely solved, and the amendment's purpose is now to prevent *any* racial classification. Critics (e.g., anti-caste scholars, civil rights organizations) attest that the founding problem of racial hierarchy persists in structural forms, and formal equality is insufficient to address it; historical analysis and sociological data from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).
:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.2) from the perspective of this reading's proponents, as it is seen as a neutral rule preventing 'reverse discrimination' and ensuring equal treatment. However, it is highly extractive from the perspective of those seeking race-conscious remedies. Suppression is low (0.3) as it's a legal principle, not direct physical coercion, but it does suppress certain policy options. Accessibility collapse is high (0.8) because it significantly limits the range of state actions regarding racial classification. Resistance is moderate (0.5) due to ongoing legal and philosophical debates. Theater ratio is low (0.1) as it's a fundamental legal principle, not primarily performative. The claimed type is 'rope' because it coordinates state action towards a specific legal standard of formal equality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals seeking race-neutral treatment, this constraint is a protective 'rope' ensuring fairness. From the perspective of state actors attempting to remedy historical discrimination or beneficiaries of affirmative action, it can function as a 'snare' or 'tangled rope' that prevents effective action and perpetuates existing inequalities. The Supreme Court, as the agenda-setter, largely aligns with the 'rope' framing, while civil rights advocates often experience it as extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court, as the institutional agenda-setter, benefits from the clarity and perceived neutrality of this interpretation. Individuals seeking race-neutral treatment and those opposed to affirmative action are direct beneficiaries, as the constraint aligns with their legal and philosophical positions. State actors implementing race-conscious remedies and beneficiaries of such programs are victims, as their policy options and opportunities are constrained. Civil rights advocates for an anti-caste reading are excluded, as their alternative framework is not accommodated.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was to prevent explicit state discrimination. While that specific form of discrimination is largely outlawed, the 'contested' status of the founding problem reflects a debate: proponents argue the problem is solved and the constraint now prevents any racial classification; critics argue the problem of racial hierarchy persists in structural forms, and the constraint's current application has outlived its original, narrower mandate, becoming an obstacle to substantive equality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, universally applicable principle of legal equality, or one specific interpretation of the 14th Amendment''s Equal Protection Clause?',
    'Analysis of judicial dissents, academic critiques, and alternative constitutional theories (e.g., the anti-caste reading) that offer structurally different interpretations of the same text.',
    'If it is merely one reading, its classification as a ''rope'' is contingent on the interpretive framework, and its perceived neutrality is challenged by the existence of equally coherent, but structurally different, sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''formal_equality_reading'' of the ''fourteenth_amendment_equal_protection'' kernel.').

omega_variable(
    structural_delta_of_anti_caste_reading,
    'What would be the structural impact on beneficiaries and victims if the ''anti_caste_reading'' of Equal Protection were adopted as the dominant interpretation?',
    'Legal analysis of proposed anti-caste jurisprudence, comparative study of jurisdictions with similar legal frameworks, and modeling of policy outcomes under such an interpretation.',
    'The ''state corrective action'' and ''affirmative enforcement programs'' would likely shift from the victim set to the beneficiary set, while ''individuals seeking race-neutral treatment'' and ''those opposed to affirmative action'' would likely shift to the victim set, fundamentally altering the constraint''s directionality and effective extraction for many agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_of_anti_caste_reading, conceptual, 'Impact of adopting the ''anti_caste_reading'' sibling interpretation.').

omega_variable(
    scope_of_equality_definition,
    'Does ''equal protection'' mandate merely formal equality (equal treatment under the law) or does it require substantive equality (equal outcomes or dismantling of structural hierarchy)?',
    'Ongoing legal and philosophical debate, judicial decisions, and legislative action that explicitly define the scope of equality required by the 14th Amendment.',
    'If substantive equality is mandated, the current constraint''s low extractiveness and ''rope'' classification would be challenged, as its failure to address structural issues would become a source of extraction. If formal equality remains the sole mandate, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_equality_definition, conceptual, 'Ambiguity in the definition of ''equal protection'' (formal vs. substantive equality).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1868, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1868, 0.1).
narrative_ontology:measurement(four_tr_t1900, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(four_tr_t1950, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(four_tr_t1980, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(four_tr_t2000, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(four_tr_t2024, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(four_be_t1868, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1868, 0.2).
narrative_ontology:measurement(four_be_t1900, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement(four_be_t1950, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(four_be_t1980, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1980, 0.18).
narrative_ontology:measurement(four_be_t2000, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1868, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1868, 0.3).
narrative_ontology:measurement(four_su_t1900, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1900, 0.32).
narrative_ontology:measurement(four_su_t1950, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(four_su_t1980, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1980, 0.28).
narrative_ontology:measurement(four_su_t2000, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection__anti_caste_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of two primary readings of the 14th Amendment's Equal Protection Clause. The 'formal equality' reading emphasizes colorblindness and equal treatment, while the 'anti-caste' reading emphasizes dismantling structural hierarchy. They are modeled as separate constraints due to their divergent ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
