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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story models the 'dependency trap' reading of
 *   unconditional income support, where the system, while providing a safety
 *   net, is seen as a work-disincentive that atrophies skills and increases
 *   state dependence. Key agents include recipients who may become dependent,
 *   taxpayers who fund the system, and policy advocates who highlight these
 *   negative outcomes. This reading emphasizes the extractive nature of the
 *   system on both taxpayers and the human capital of recipients.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.65).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.7).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '6169e45b-97ab-493a-9b5c-50c9ee0311f6').
narrative_ontology:cs_kernel_codification('6169e45b-97ab-493a-9b5c-50c9ee0311f6', formalized).
narrative_ontology:cs_authority_grounding('6169e45b-97ab-493a-9b5c-50c9ee0311f6', lineage).
narrative_ontology:cs_interpretation_layer_present('6169e45b-97ab-493a-9b5c-50c9ee0311f6').
narrative_ontology:cs_reading_relation('6169e45b-97ab-493a-9b5c-50c9ee0311f6', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('6169e45b-97ab-493a-9b5c-50c9ee0311f6', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('6169e45b-97ab-493a-9b5c-50c9ee0311f6', foundational, work_is_a_moral_obligation).
narrative_ontology:cs_axiom_status(work_is_a_moral_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6169e45b-97ab-493a-9b5c-50c9ee0311f6', work_is_a_moral_obligation, deontological).
narrative_ontology:cs_axiom('6169e45b-97ab-493a-9b5c-50c9ee0311f6', foundational, unearned_income_erodes_human_capital).
narrative_ontology:cs_axiom_status(unearned_income_erodes_human_capital, holdable).
narrative_ontology:cs_axiom_grounding('6169e45b-97ab-493a-9b5c-50c9ee0311f6', unearned_income_erodes_human_capital, empirically_contingent).
narrative_ontology:cs_reference_frame('6169e45b-97ab-493a-9b5c-50c9ee0311f6', productive_citizenship_model).
narrative_ontology:cs_drift_state('6169e45b-97ab-493a-9b5c-50c9ee0311f6', contemporary_welfare_state_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6169e45b-97ab-493a-9b5c-50c9ee0311f6', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, ubi_recipients_exiting_labor_market).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, government_agencies_administering_support).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, individuals_with_atrophied_skills).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, ubi_recipients_exiting_labor_market).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income, which provides a basic standard of living. However, for some, this leads to reduced labor market participation, skill atrophy, and increased reliance on state support, making exit from the system difficult.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, ubi_recipients_exiting_labor_market, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, ubi_recipients_exiting_labor_market, payer).

% Fund the unconditional income support system through taxes. They bear the financial cost and often perceive a moral cost in subsidizing non-participation in the labor force, leading to resentment and political pressure for reform.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    organized, biographical, constrained, national).

% Actively advocate for policies that emphasize work requirements and skill development, arguing that unconditional income creates disincentives and fosters long-term dependence. They shape public discourse and policy proposals from this perspective.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, social_policy_advocates_dependency_trap, agenda_setter,
    institutional, generational, analytical, national).

% Are tasked with implementing and managing the unconditional income support programs. They benefit from a clear mandate and budget, but also face the operational challenges of managing a large recipient base and public scrutiny regarding program outcomes.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, government_agencies_administering_support, agenda_setter,
    institutional, immediate, constrained, national).

% Are individuals whose skills have diminished due to prolonged absence from the labor market, making it increasingly difficult to find employment even if they desire to. They bear the cost of lost human capital and reduced future earning potential.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, individuals_with_atrophied_skills, payer,
    powerless, biographical, trapped, local).

% Would argue that unconditional income support primarily enables autonomy, dignity, and provides a necessary 'freedom floor' for all citizens, rather than creating dependence. Their perspective is excluded from the 'dependency trap' framing.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, freedom_floor_advocates, excluded,
    organized, generational, analytical, national).

% Would argue that income support should be concentrated on demonstrated need through means-testing, rather than universally distributed, to maximize efficiency and impact. Their focus on targeting is excluded from the 'dependency trap' framing, which critiques the unconditional nature itself.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, targeting_efficiency_advocates, excluded,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a basic safety net to prevent destitution and manage social instability, ensuring a minimal standard of living for all citizens, thereby coordinating social welfare.
% TRANSFER_FUNCTION: Transfers tax revenue collected from working individuals and businesses to all citizens, regardless of employment status, to provide a baseline income.
% ABSENT_VOICES: Advocates for the 'freedom floor' reading would emphasize the autonomy and dignity enabled by unconditional income, while 'targeting efficiency' advocates would argue for means-tested support to maximize impact on the most vulnerable. Both are excluded from the 'dependency trap' framing, which focuses on negative behavioral incentives.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, a significant portion of the population, particularly those who have exited the labor market or whose skills have atrophied, would face immediate destitution, leading to widespread social unrest and economic collapse for many households. The state would need to rapidly implement alternative, likely more coercive, welfare programs.
% FOUNDING_PROBLEM: To address poverty, economic insecurity, and the potential for social unrest arising from unemployment and insufficient wages, ensuring a basic standard of living for all citizens.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the 'dependency trap' reading, including conservative think tanks and some economists, argue the original problem of destitution is largely solved for many, and the current system creates new problems. Advocates for the 'freedom floor' reading and some social scientists argue the problem of economic insecurity remains live, but the solution is mischaracterized. Independent social surveys and labor market analyses provide mixed evidence, corroborating both persistent insecurity and potential disincentive effects.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate-high (0.65) because the system transfers resources from productive labor to non-productive consumption, and also extracts from recipients by diminishing their skills and future earning potential. Suppression is high (0.70) as it suppresses labor market participation for some and limits the alternatives for those whose skills atrophy, effectively trapping them in dependence. The accessibility collapse (0.75) reflects the difficulty of re-entering the labor market with diminished skills. Theater ratio is low (0.20) as the disincentive and dependence are seen as direct, rather than performative, effects of the policy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of working taxpayers, the system is extractive, forcing them to subsidize perceived idleness. From the perspective of recipients who become dependent, it is a trap that limits their autonomy and future prospects, even while providing basic needs. Government agencies, while administering the system, may also perceive the tension between welfare provision and labor market incentives.
 *
 * DIRECTIONALITY LOGIC:
 *   UBI recipients exiting the labor market are beneficiaries in terms of income received, but victims in terms of human capital atrophy and reduced autonomy, leading to a complex directionality. Working taxpayers are clear payers. Social policy advocates (dependency trap) and government agencies are agenda-setters, benefiting from their role in shaping/administering the policy. Individuals with atrophied skills are victims, bearing the cost of lost potential. Advocates for alternative readings are excluded from this framing.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the system as a pure Rope (pure coordination) by explicitly identifying the asymmetric extraction from taxpayers and the human capital of recipients. It also avoids mislabeling it as a pure Snare by acknowledging the genuine coordination function of providing a basic safety net, even if that function is seen as having negative side effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Is this constraint primarily about preventing destitution (freedom_floor_reading) or about the behavioral consequences of unconditional support (dependency_trap_reading)?',
    'Analysis of policy intent documents, public discourse framing, and empirical outcomes weighted by different normative frameworks.',
    'If framed as primarily preventing destitution, the classification might shift towards a Rope or Scaffold, with lower perceived extraction. If framed as a dependency trap, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the primary normative framing of income support.').

omega_variable(
    empirical_disincentive_magnitude,
    'What is the actual, empirically measured magnitude of work disincentive and skill atrophy caused by unconditional income support, across different demographics and economic conditions?',
    'Longitudinal studies and randomized controlled trials of unconditional income programs, disaggregated by recipient demographics, local labor market conditions, and program design.',
    'Strong empirical evidence of significant disincentive and atrophy would reinforce the high extractiveness and suppression. Weak or negligible evidence would challenge this reading, potentially shifting the classification towards a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_disincentive_magnitude, empirical, 'Uncertainty regarding the empirical extent of work disincentive and skill atrophy.').

omega_variable(
    state_dependence_mechanism,
    'Is state dependence primarily a result of individual choice and skill atrophy, or a structural outcome of labor market conditions that make re-entry difficult even with skills?',
    'Comparative studies of labor market dynamics in regions with and without unconditional income, controlling for skill levels and job availability, to isolate the causal factors of dependence.',
    'If dependence is primarily structural, the ''dependency trap'' framing is less about individual failing and more about systemic issues, potentially re-framing the extraction as a cost of systemic failure rather than individual choice. If individual choice dominates, the current framing is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_dependence_mechanism, empirical, 'Ambiguity in the causal mechanism of state dependence (individual vs. structural).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__dependency_trap_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__dependency_trap_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__dependency_trap_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(inco_tr_t50, income_support_commitment__dependency_trap_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__dependency_trap_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__dependency_trap_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__dependency_trap_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(inco_be_t50, income_support_commitment__dependency_trap_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__dependency_trap_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__dependency_trap_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__dependency_trap_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(inco_su_t50, income_support_commitment__dependency_trap_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, labor_market_flexibility_norms).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, social_safety_net_design_principles).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'income_support_commitment' kernel, focusing on the work-disincentive and dependency aspects. It is linked to sibling readings that emphasize autonomy ('freedom_floor_reading') and targeting efficiency ('targeting_efficiency_reading').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
