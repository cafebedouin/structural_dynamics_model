% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__dependency_trap_reading, []).

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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story analyzes unconditional income support from the
 *   'dependency trap' reading, where such programs, despite their stated goal
 *   of poverty alleviation, are seen as creating disincentives to work,
 *   leading to skill atrophy and increased reliance on the state. The
 *   constraint is framed as a Tangled Rope, as it provides a coordination
 *   function (basic income) but simultaneously extracts from working
 *   taxpayers and from the human capital of recipients who become dependent.
 *   The claimed type reflects the operator's (state's) initial framing, while
 *   the metrics reflect the observed outcomes from the dependency trap
 *   perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.68).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.75).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '243e9010-4017-46a5-9f9d-0beb7c5bf8de').
narrative_ontology:cs_kernel_codification('243e9010-4017-46a5-9f9d-0beb7c5bf8de', formalized).
narrative_ontology:cs_authority_grounding('243e9010-4017-46a5-9f9d-0beb7c5bf8de', lineage).
narrative_ontology:cs_interpretation_layer_present('243e9010-4017-46a5-9f9d-0beb7c5bf8de').
narrative_ontology:cs_reading_relation('243e9010-4017-46a5-9f9d-0beb7c5bf8de', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('243e9010-4017-46a5-9f9d-0beb7c5bf8de', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('243e9010-4017-46a5-9f9d-0beb7c5bf8de', foundational, work_ethic_is_foundational).
narrative_ontology:cs_axiom_status(work_ethic_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('243e9010-4017-46a5-9f9d-0beb7c5bf8de', work_ethic_is_foundational, deontological).
narrative_ontology:cs_axiom('243e9010-4017-46a5-9f9d-0beb7c5bf8de', foundational, human_capital_is_productive_asset).
narrative_ontology:cs_axiom_status(human_capital_is_productive_asset, holdable).
narrative_ontology:cs_axiom_grounding('243e9010-4017-46a5-9f9d-0beb7c5bf8de', human_capital_is_productive_asset, empirically_contingent).
narrative_ontology:cs_reference_frame('243e9010-4017-46a5-9f9d-0beb7c5bf8de', productive_citizenry_framework).
narrative_ontology:cs_drift_state('243e9010-4017-46a5-9f9d-0beb7c5bf8de', contemporary_welfare_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('243e9010-4017-46a5-9f9d-0beb7c5bf8de', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, income_support_recipients_exiting_labor).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, individuals_with_atrophied_skills).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the unconditional income support program, collecting taxes and distributing funds. Its mandate is to alleviate poverty, but from this reading, it inadvertently fosters dependence and manages a population that has exited the labor market.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, state_welfare_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Receive regular income without work requirements, which provides basic needs. However, over time, their skills atrophy, social networks outside the welfare system diminish, and their identity becomes fused with their recipient status, making exit from state dependence extremely difficult.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, income_support_recipients_exiting_labor, beneficiary,
    powerless, biographical, identity_locked, local).

% Bear the financial burden of the unconditional income support through taxes. From this reading, they perceive themselves as subsidizing non-participation in the labor force, leading to resentment and questions about fairness and productivity.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    organized, biographical, constrained, national).

% These are the same individuals as the 'income_support_recipients_exiting_labor', but this role highlights the cost they bear in terms of lost human capital and reduced future earning potential due to prolonged detachment from the labor market. Their ability to re-enter work is severely compromised.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, individuals_with_atrophied_skills, payer,
    powerless, biographical, identity_locked, local).

% Argue that income support should be conditional on work or training, or that the system should actively promote labor market participation. Their policy preferences are excluded by the 'unconditional' nature of the support, and they are often marginalized in the policy debate by proponents of universal basic income.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, advocates_for_work_requirements, excluded,
    powerful, generational, constrained, national).

% Analyze the long-term effects of unconditional income support on labor supply, skill development, and state budgets. They provide empirical data and theoretical frameworks that either support or refute the dependency trap hypothesis.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, economists_social_policy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__dependency_trap_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_commitment__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate basic needs provision and poverty alleviation, ensuring a minimum standard of living for all citizens, thereby reducing social instability and health crises associated with destitution.
% TRANSFER_FUNCTION: Transfers tax revenue collected from the working population to individuals, providing a regular, unconditional income stream.
% ABSENT_VOICES: Advocates for strong work incentives and those who emphasize individual responsibility for livelihood are often excluded from the core design of unconditional income programs, as their perspectives challenge the foundational premise of 'unconditionality'.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, a significant portion of the population, particularly those who have exited the labor market and atrophied skills, would face immediate and severe destitution, leading to widespread social and economic upheaval. The state would face a massive humanitarian crisis.
% FOUNDING_PROBLEM: The original problem was widespread poverty, destitution, and economic insecurity, particularly for those unable to work or facing structural unemployment.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of unconditional income support (e.g., some social justice advocates, certain economists) argue the founding problem of economic insecurity remains live. Critics (e.g., some conservative think tanks, labor market economists) argue that while poverty persists, the unconditional nature of the support creates new problems of dependency and skill atrophy, suggesting the original problem is being addressed in a counterproductive way. Legislative debates and academic studies from outside the direct beneficiaries corroborate this contestation.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high because a significant portion of the working population's taxes is transferred to non-working recipients, and the long-term cost of lost human capital from skill atrophy is substantial. Suppression (0.75) is also high, as the availability of unconditional income reduces the incentive and necessity to seek employment, effectively suppressing alternatives to state dependence. This suppression is both structural (reduced labor market demand for atrophied skills) and internalized (learned helplessness, identity fusion with recipient status). The theater ratio (0.15) is low because the program is actively functional in distributing funds and is a subject of intense policy debate, not mere performance.
 *
 * PERSPECTIVAL GAP:
 *   The state welfare agencies, as agenda-setters, would likely perceive the constraint as a Rope or Scaffold, fulfilling a vital coordination function for social welfare. However, working taxpayers and individuals experiencing skill atrophy would perceive it as a Snare or Tangled Rope, extracting resources and opportunities. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Income support recipients exiting the labor market are beneficiaries of the direct transfer, but also targets of the dependency trap, leading to a complex directionality. Working taxpayers are clear targets, bearing the financial cost. Individuals with atrophied skills are also targets, bearing the cost of lost human capital and constrained future options. The 'identity_locked' exit option for recipients reflects the deep structural and psychological barriers to re-entering the labor market once dependence sets in.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_causality_ambiguity,
    'What is the actual causal link between unconditional income support and observed skill atrophy/state dependence, distinguishing it from pre-existing structural unemployment or individual factors?',
    'Longitudinal studies with robust control groups, comparing labor market outcomes and skill development trajectories of unconditional income recipients against similar populations without such support, controlling for other socioeconomic variables.',
    'If a strong causal link is established, it reinforces the dependency trap reading and supports policy adjustments towards conditional support or active labor market policies. If the link is weak, it weakens this reading and strengthens alternative framings (e.g., freedom floor, where non-participation is a choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_causality_ambiguity, empirical, 'Empirical evidence for the causal mechanism of dependency.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of labor market alternatives for recipients primarily structural (lack of suitable jobs, discrimination) or internalized (learned helplessness, loss of work ethic)?',
    'Post-exit trajectory analysis: if recipients struggle to re-enter the labor market even when job opportunities are available and support is removed, it suggests a stronger internalized component. Qualitative studies on recipient motivations and self-perceptions.',
    'If primarily structural, the constraint''s extractiveness is more a symptom of broader economic failures. If primarily internalized, the constraint itself actively creates the dependency, making its extractiveness more direct and policy-actionable within the welfare system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for labor market exit.').

omega_variable(
    framing_of_non_participation,
    'Is non-participation in the labor market by income support recipients a ''dependency trap'' (negative outcome) or a ''freedom floor'' (positive exercise of autonomy)?',
    'This is a conceptual and preference-based question, not empirically resolvable. Resolution depends on the normative framework adopted (e.g., utilitarian, deontological, capabilities approach) and the values prioritized (e.g., productivity vs. autonomy).',
    'The classification of the constraint (e.g., Tangled Rope vs. Rope) and the evaluation of its social utility fundamentally shift based on this framing. This omega highlights the irreducible normative disagreement at the heart of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_non_participation, conceptual, 'Normative framing of labor market non-participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__dependency_trap_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__dependency_trap_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__dependency_trap_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(inco_tr_t50, income_support_commitment__dependency_trap_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__dependency_trap_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__dependency_trap_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__dependency_trap_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(inco_be_t50, income_support_commitment__dependency_trap_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__dependency_trap_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__dependency_trap_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__dependency_trap_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(inco_su_t50, income_support_commitment__dependency_trap_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, labor_market_participation_norms).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, tax_burden_on_workers).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, social_cohesion_dynamics).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'income_support_commitment' kernel, focusing on the negative consequences of dependency. It is linked to the 'freedom_floor_reading' and 'targeting_efficiency_reading' which offer alternative interpretations of the same policy commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
