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
 *   human_readable: Equal Protection: Formal Equality Reading
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'formal equality' reading of the
 *   Fourteenth Amendment's Equal Protection Clause, which prohibits explicit
 *   state racial or status classifications unless justified by a compelling
 *   government interest. This reading treats structural inequality as a
 *   pre-constitutional background condition, focusing on individual rights
 *   and race-neutral treatment. It tends to constrain state corrective action
 *   and affirmative enforcement programs, often placing them in the 'victim'
 *   category. The low extractiveness reflects that this reading primarily
 *   prevents certain state actions rather than actively extracting from
 *   specific groups, though it does impose costs on those seeking
 *   race-conscious remedies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.25).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.4).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Equal Protection: Formal Equality Reading").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, 'bf24b618-3eff-425f-a7e6-18a6c7e4cf07').
narrative_ontology:cs_kernel_codification('bf24b618-3eff-425f-a7e6-18a6c7e4cf07', fixed_text).
narrative_ontology:cs_authority_grounding('bf24b618-3eff-425f-a7e6-18a6c7e4cf07', lineage).
narrative_ontology:cs_interpretation_layer_present('bf24b618-3eff-425f-a7e6-18a6c7e4cf07').
narrative_ontology:cs_reading_relation('bf24b618-3eff-425f-a7e6-18a6c7e4cf07', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('bf24b618-3eff-425f-a7e6-18a6c7e4cf07', foundational, colorblind_state_action).
narrative_ontology:cs_axiom_status(colorblind_state_action, holdable).
narrative_ontology:cs_axiom_grounding('bf24b618-3eff-425f-a7e6-18a6c7e4cf07', colorblind_state_action, deontological).
narrative_ontology:cs_axiom('bf24b618-3eff-425f-a7e6-18a6c7e4cf07', foundational, individual_rights_supremacy).
narrative_ontology:cs_axiom_status(individual_rights_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('bf24b618-3eff-425f-a7e6-18a6c7e4cf07', individual_rights_supremacy, deontological).
narrative_ontology:cs_reference_frame('bf24b618-3eff-425f-a7e6-18a6c7e4cf07', post_civil_war_formal_equality).
narrative_ontology:cs_drift_state('bf24b618-3eff-425f-a7e6-18a6c7e4cf07', contemporary_judicial_interpretations, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('bf24b618-3eff-425f-a7e6-18a6c7e4cf07', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, individuals_seeking_race_neutral_treatment).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, institutions_avoiding_race_conscious_remedies).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_affirmative_action_programs).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minority_groups_seeking_remedies).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitution_doctrine).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, individual_rights_over_group_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of Equal Protection, interpreting its meaning and applying it to specific cases. Its rulings define the scope of permissible state action regarding race and status classifications. This reading emphasizes judicial restraint in mandating race-conscious remedies.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from state policies that treat all individuals equally regardless of race, and from the invalidation of race-conscious programs that might disadvantage them. They advocate for a 'colorblind' application of the law.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, individuals_seeking_race_neutral_treatment, beneficiary,
    moderate, biographical, mobile, national).

% Public and private entities that prefer to avoid implementing race-conscious programs, often citing administrative burden or legal risk. This reading provides a legal basis for resisting such mandates.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, institutions_avoiding_race_conscious_remedies, beneficiary,
    organized, biographical, constrained, national).

% These programs, designed to address historical and structural inequality, are often challenged and constrained under this reading. They bear the cost of legal defense and potential invalidation, limiting their ability to achieve their goals.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_affirmative_action_programs, payer,
    institutional, biographical, trapped, local).

% Advocate for state action to dismantle systemic inequality. Under this reading, their efforts to secure race-conscious remedies are often frustrated, as such remedies are viewed with suspicion and subjected to strict scrutiny.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minority_groups_seeking_remedies, payer,
    organized, generational, constrained, national).

% Argue that formal equality is insufficient to address structural racism and that a 'colorblind' approach perpetuates existing hierarchies. Their perspectives are often marginalized in judicial interpretations that favor this reading.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, legal_scholars_critical_race_theory, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, consistent standard for state action regarding racial classifications, ensuring that government does not explicitly discriminate and that individuals are treated equally under the law.
% TRANSFER_FUNCTION: Transfers the burden of proof to the state to justify any racial classification with a compelling interest, and transfers the benefit of race-neutral policies to individuals and institutions that prefer them.
% ABSENT_VOICES: Legal scholars and advocates for an anti-caste reading of Equal Protection are often excluded from the dominant judicial discourse, as their arguments for race-conscious remedies are deemed inconsistent with this formal equality framework.
% DISAPPEARANCE_RATIONALE: If this reading of Equal Protection vanished, the legal landscape for civil rights would fundamentally shift. State actors would have greater latitude for race-conscious policies, and the debate over affirmative action would be entirely reframed, leading to significant legal and social reorganization.
% FOUNDING_PROBLEM: The 14th Amendment was ratified to ensure legal equality for formerly enslaved people, prohibiting states from denying 'equal protection of the laws' to any person.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars generally agree on the founding problem of ensuring legal equality post-slavery. The contest lies in whether 'equality' means formal race-neutrality or active dismantling of systemic hierarchy; both sides attest to the problem's historical origin, but diverge on its contemporary status and required remedies.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.25) because this reading primarily acts as a negative constraint on state power, rather than a mechanism for direct resource transfer. However, it does impose costs on groups and programs seeking race-conscious remedies. Suppression (0.4) is moderate, reflecting the active judicial enforcement required to strike down race-conscious policies. Theater ratio is low (0.1) as the judicial function is largely genuine, though some critics argue the 'compelling interest' test can be performative. The temporal measurements show a slight increase in extractiveness and suppression over time, reflecting the increasing judicial scrutiny of race-conscious policies since the mid-20th century.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals seeking race-neutral treatment, this constraint is a pure rope, ensuring fairness. From the perspective of racial minority groups seeking remedies, it can feel like a snare, preventing necessary corrective action. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court acts as the agenda-setter, defining the scope of this reading. Individuals seeking race-neutral treatment and institutions avoiding race-conscious remedies are beneficiaries, as this reading aligns with their preferences. State affirmative action programs and racial minority groups seeking remedies are payers/victims, as their efforts are constrained or invalidated. Legal scholars advocating for an anti-caste reading are excluded, as their framework is fundamentally at odds with this formal equality approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_formal_equality,
    'Is the Equal Protection Clause solely concerned with formal, race-neutral treatment, or does it also mandate addressing structural inequalities that persist despite race-neutral laws?',
    'A shift in judicial precedent explicitly acknowledging and requiring remedies for structural inequality, or a constitutional amendment clarifying the scope of Equal Protection.',
    'If structural inequality is recognized as within the scope, this reading''s extractiveness on race-conscious remedies would be re-evaluated, potentially shifting its classification towards a snare for those seeking such remedies, or a piton if its original purpose is deemed unfulfilled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_formal_equality, conceptual, 'Ambiguity regarding the scope of ''equality'' under the Equal Protection Clause.').

omega_variable(
    compelling_interest_test_efficacy,
    'Is the ''compelling interest'' test, used to scrutinize racial classifications, genuinely effective at distinguishing legitimate state interests from discriminatory ones, or does it primarily serve to invalidate race-conscious remedies?',
    'Empirical analysis of judicial outcomes over time, comparing the success rate of different types of state interests under strict scrutiny, or a re-evaluation of the test''s application by the Supreme Court.',
    'If the test is found to be primarily a tool for invalidation, the theater_ratio of this constraint would increase, and its suppression of race-conscious remedies would be seen as more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelling_interest_test_efficacy, empirical, 'Effectiveness of the ''compelling interest'' test in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1868, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1868, 0.05).
narrative_ontology:measurement(four_tr_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1954, 0.08).
narrative_ontology:measurement(four_tr_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1978, 0.09).
narrative_ontology:measurement(four_tr_t2024, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(four_be_t1868, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1868, 0.1).
narrative_ontology:measurement(four_be_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(four_be_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1978, 0.2).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1868, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1868, 0.2).
narrative_ontology:measurement(four_su_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(four_su_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, affirmative_action_policies).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, voting_rights_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Fourteenth Amendment's Equal Protection Clause. Its sibling, 'anti_caste_reading', offers a contrasting interpretation focused on dismantling systemic hierarchy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
