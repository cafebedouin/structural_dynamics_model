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
 *   Fourteenth Amendment's Equal Protection Clause, which primarily prohibits
 *   explicit state racial classifications unless they meet strict scrutiny.
 *   This reading views structural inequality as a pre-constitutional
 *   background condition, not a target for state-mandated race-conscious
 *   remedies. It is one of several competing interpretations of the Equal
 *   Protection Clause.
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
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, 'db6d8f0a-0e93-468f-ba23-bfc7bf27f8e8').
narrative_ontology:cs_kernel_codification('db6d8f0a-0e93-468f-ba23-bfc7bf27f8e8', fixed_text).
narrative_ontology:cs_authority_grounding('db6d8f0a-0e93-468f-ba23-bfc7bf27f8e8', lineage).
narrative_ontology:cs_interpretation_layer_present('db6d8f0a-0e93-468f-ba23-bfc7bf27f8e8').
narrative_ontology:cs_reading_relation('db6d8f0a-0e93-468f-ba23-bfc7bf27f8e8', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('db6d8f0a-0e93-468f-ba23-bfc7bf27f8e8', foundational, state_colorblindness_is_justice).
narrative_ontology:cs_axiom_status(state_colorblindness_is_justice, holdable).
narrative_ontology:cs_axiom_grounding('db6d8f0a-0e93-468f-ba23-bfc7bf27f8e8', state_colorblindness_is_justice, deontological).
narrative_ontology:cs_axiom('db6d8f0a-0e93-468f-ba23-bfc7bf27f8e8', foundational, individual_rights_transcend_group_identity).
narrative_ontology:cs_axiom_status(individual_rights_transcend_group_identity, holdable).
narrative_ontology:cs_axiom_grounding('db6d8f0a-0e93-468f-ba23-bfc7bf27f8e8', individual_rights_transcend_group_identity, deontological).
narrative_ontology:cs_reference_frame('db6d8f0a-0e93-468f-ba23-bfc7bf27f8e8', post_civil_rights_era_colorblindness).
narrative_ontology:cs_drift_state('db6d8f0a-0e93-468f-ba23-bfc7bf27f8e8', contemporary_racial_justice_movements, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('db6d8f0a-0e93-468f-ba23-bfc7bf27f8e8', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, individuals_seeking_race_neutral_treatment).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, state_actors_avoiding_race_conscious_remedies).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minorities_seeking_affirmative_action).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_actors_implementing_race_conscious_remedies).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitution_doctrine).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, individual_rights_over_group_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of Equal Protection, interpreting the Fourteenth Amendment to prohibit explicit racial classifications unless they serve a compelling government interest and are narrowly tailored. Its rulings shape the application of this reading.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from a legal framework that treats all individuals equally regardless of race, preventing state-sponsored discrimination against them, including in affirmative action programs. They can challenge race-conscious policies.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, individuals_seeking_race_neutral_treatment, beneficiary,
    moderate, biographical, mobile, national).

% Benefit from a clear prohibition on explicit racial classifications, simplifying policy design by not requiring them to consider race in most contexts. They are protected from legal challenges when implementing race-neutral policies.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_actors_avoiding_race_conscious_remedies, beneficiary,
    institutional, biographical, constrained, national).

% Bear the cost of this reading as it limits the state's ability to implement race-conscious programs designed to address historical and systemic inequalities. They must pursue race-neutral alternatives or face legal challenges.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minorities_seeking_affirmative_action, payer,
    powerless, generational, constrained, national).

% Face legal challenges and restrictions when attempting to implement policies that explicitly consider race to address disparities. They must navigate strict scrutiny and often abandon such policies.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_actors_implementing_race_conscious_remedies, payer,
    institutional, biographical, constrained, national).

% Monitor and litigate cases related to Equal Protection, often challenging policies that they believe perpetuate or fail to address racial inequality. They analyze the impact of this reading on civil rights progress.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, civil_rights_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, judicially enforceable standard for state action regarding race, ensuring that government classifications are subject to strict scrutiny and are presumptively unconstitutional, thereby coordinating state behavior towards race-neutrality.
% TRANSFER_FUNCTION: Transfers the burden of proof to the state to justify any racial classification, effectively limiting the state's ability to implement race-conscious policies and transferring the benefit of race-neutral treatment to all individuals, regardless of their racial background.
% ABSENT_VOICES: Advocates for substantive equality and anti-caste principles, who argue that a purely formal equality approach ignores historical and systemic oppression, are often marginalized in legal discourse dominated by this reading. They would argue for state intervention to dismantle structural hierarchies.
% DISAPPEARANCE_RATIONALE: If this reading of Equal Protection vanished, state and federal governments would have significantly more latitude to implement race-conscious policies, potentially leading to a rapid shift in affirmative action programs, redistributive justice initiatives, and other remedies aimed at addressing systemic inequality. The legal landscape of civil rights would be fundamentally altered.
% FOUNDING_PROBLEM: The Fourteenth Amendment was ratified to abolish slavery, establish birthright citizenship, and ensure equal protection of the laws, primarily to protect newly freed slaves from discriminatory state laws.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars widely corroborate the founding problem's historical context. However, its 'status' is contested: proponents of formal equality argue the problem is 'live' in preventing any state-sponsored racial discrimination, while critics argue the original problem of systemic racial hierarchy is 'live' but this reading fails to address it, making the problem 'dead' for this specific interpretation's efficacy.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.25) is relatively low because this reading primarily prevents state action, rather than actively extracting resources. However, it extracts from those seeking race-conscious remedies by denying them. Suppression (0.4) is moderate, reflecting the judicial enforcement required to strike down race-conscious policies. Theater ratio is low (0.1) as the judicial process is largely functional in applying this standard. Accessibility collapse (0.7) is high because it significantly limits the types of remedies available for racial inequality. Resistance (0.3) is moderate, as there is ongoing legal and political debate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals seeking race-neutral treatment, this constraint is a protective 'rope' ensuring fairness. From the perspective of racial minorities seeking affirmative action, it operates as a 'snare' that prevents necessary corrective measures. The Supreme Court, as the agenda-setter, largely views it as a 'rope' upholding constitutional principles.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals seeking race-neutral treatment and state actors avoiding race-conscious remedies are beneficiaries (low d) as the constraint aligns with their interests. Racial minorities seeking affirmative action and state actors implementing race-conscious remedies are victims (high d) as the constraint directly limits their desired actions. The Supreme Court's directionality is complex, acting as both enforcer and interpreter, but generally aligns with the beneficiary side of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as pure extraction by focusing on the state's role in creating classifications. However, it risks mislabeling the absence of state-created racial classifications as the absence of racial inequality, thereby obscuring structural extraction that predates or operates outside explicit state action. The 'contested' status of the founding problem highlights this tension: is the problem solved by formal equality, or does it persist in a form this reading cannot address?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_explicit_discrimination,
    'Does the Equal Protection Clause, under this reading, adequately address structural racial inequality that does not arise from explicit state classification?',
    'Empirical studies on the persistence of racial disparities in areas like wealth, education, and housing, even in the absence of explicit state-sponsored racial classifications.',
    'If structural inequality persists and is demonstrably unaddressed by this reading, it would highlight a significant gap in the constraint''s efficacy, potentially shifting its classification towards a ''snare'' for those experiencing such inequality, as it provides a legal ''cover'' for inaction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_explicit_discrimination, empirical, 'Ambiguity regarding the scope of Equal Protection''s reach beyond explicit state action.').

omega_variable(
    reading_impact_on_remedies,
    'To what extent does this formal equality reading constrain the development and implementation of effective remedies for historical and ongoing racial disparities?',
    'Comparative legal analysis of jurisdictions with different Equal Protection interpretations, examining the range and effectiveness of their racial justice policies.',
    'If this reading is found to severely limit effective remedies, it would increase its ''extractiveness'' and ''suppression'' for affected groups, potentially pushing its classification towards a ''tangled_rope'' or ''snare'' for those seeking substantive equality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_impact_on_remedies, conceptual, 'The practical impact of formal equality on the pursuit of substantive racial justice.').

omega_variable(
    founding_intent_ambiguity,
    'Was the original intent of the Fourteenth Amendment solely to prohibit explicit racial classifications, or did it also encompass a broader mandate to dismantle racial hierarchy?',
    'Further historical and legal scholarship on the legislative history and framers'' intent of the Fourteenth Amendment, including debates surrounding its ratification.',
    'If a broader anti-caste intent is definitively established, it would challenge the foundational premise of this formal equality reading, potentially leading to a re-evaluation of its legitimacy and a shift in its perceived ''claimed_type'' towards a ''snare'' for those whose interests are not served by a narrow interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_intent_ambiguity, empirical, 'Contestation over the original intent of the Equal Protection Clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(four_be_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1978, 0.2).
narrative_ontology:measurement(four_be_t1990, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(four_be_t2000, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(four_be_t2010, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1978, 0.3).
narrative_ontology:measurement(four_su_t1990, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(four_su_t2000, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(four_su_t2010, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, affirmative_action_prohibition_constraint).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, voting_rights_act_enforcement_constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
