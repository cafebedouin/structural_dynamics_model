% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__dependency_trap_reading, []).

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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story represents the 'dependency trap' reading of
 *   unconditional income support. It argues that providing income without
 *   work requirements leads to long-term dependency, skill atrophy, and
 *   disincentives to work, effectively trapping recipients in a state of
 *   idleness and extracting resources from taxpayers. The constraint is
 *   framed as a snare, as its purported coordination function (poverty
 *   reduction) is undermined by its extractive effects (dependency and
 *   taxpayer burden).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.85).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.7).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, '0968d0ca-70f4-46a9-b96c-dce722267d17').
narrative_ontology:cs_kernel_codification('0968d0ca-70f4-46a9-b96c-dce722267d17', formalized).
narrative_ontology:cs_authority_grounding('0968d0ca-70f4-46a9-b96c-dce722267d17', extraction).
narrative_ontology:cs_interpretation_layer_present('0968d0ca-70f4-46a9-b96c-dce722267d17').
narrative_ontology:cs_reading_relation('0968d0ca-70f4-46a9-b96c-dce722267d17', income_support_conditionality__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('0968d0ca-70f4-46a9-b96c-dce722267d17', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('0968d0ca-70f4-46a9-b96c-dce722267d17', foundational, work_as_moral_imperative).
narrative_ontology:cs_axiom_status(work_as_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('0968d0ca-70f4-46a9-b96c-dce722267d17', work_as_moral_imperative, deontological).
narrative_ontology:cs_axiom('0968d0ca-70f4-46a9-b96c-dce722267d17', foundational, incentives_drive_productivity).
narrative_ontology:cs_axiom_status(incentives_drive_productivity, holdable).
narrative_ontology:cs_axiom_grounding('0968d0ca-70f4-46a9-b96c-dce722267d17', incentives_drive_productivity, empirically_contingent).
narrative_ontology:cs_reference_frame('0968d0ca-70f4-46a9-b96c-dce722267d17', conditional_welfare_state).
narrative_ontology:cs_drift_state('0968d0ca-70f4-46a9-b96c-dce722267d17', contemporary_ubi_proposals, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0968d0ca-70f4-46a9-b96c-dce722267d17', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, bureaucratic_administrators).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals receiving unconditional income support. From this reading's perspective, they are trapped in a cycle of idleness, losing skills and motivation, making it harder to re-enter the workforce. The 'benefit' of income is outweighed by the long-term cost of dependency.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, biographical, identity_locked, national).

% Citizens funding the unconditional income support programs through taxes. They bear the financial burden of supporting a non-productive population, leading to resentment and perceived economic inefficiency.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers, payer,
    moderate, generational, constrained, national).

% Government agencies and personnel responsible for implementing and managing unconditional income support programs. They benefit from the expansion of their mandate, budget, and institutional power, regardless of the program's long-term societal effects.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, bureaucratic_administrators, beneficiary,
    institutional, generational, mobile, national).

% Researchers and economists who study the effects of unconditional income support on employment, wages, and economic growth. They provide data and theoretical frameworks that either support or refute the dependency trap hypothesis.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_market_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a basic safety net, ensuring a minimum standard of living for all citizens, ostensibly to reduce poverty and social instability.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base to individuals, without requiring work or job-seeking activities.
% ABSENT_VOICES: Advocates for work-first policies and proponents of traditional welfare-to-work programs, who would argue for stricter conditionality and active labor market policies to prevent dependency.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, many recipients would face immediate financial hardship, potentially leading to increased poverty, homelessness, and social unrest. The labor market would also experience a sudden influx of individuals seeking employment, potentially driving down wages in some sectors. The administrative apparatus built around these programs would also need to be dismantled or repurposed.
% FOUNDING_PROBLEM: The problem of poverty and economic insecurity, particularly for those unable to participate in the labor market due to disability, caregiving responsibilities, or lack of opportunities.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of unconditional income support argue the problem is live and requires a universal solution. Critics (including some labor economists and social policy experts outside the administrative beneficiaries) argue that while poverty is real, unconditional support exacerbates other problems like dependency, suggesting the founding problem is being addressed in a counterproductive way.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the perceived cost to recipients (lost skills, reduced agency) and taxpayers (funding non-productive activity). Suppression (0.70) is due to the 'trap' mechanism: once dependent, exiting the system and re-entering the workforce becomes increasingly difficult due to skill atrophy and loss of motivation. Theater ratio is low (0.10) because the system is seen as genuinely, albeit negatively, effective in its operation, rather than performative. Accessibility collapse (0.60) indicates that while theoretical alternatives (work) exist, the practical barriers to accessing them become significant over time. Resistance (0.40) is moderate, coming from taxpayers and policy critics.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of UBI recipients, the constraint might initially appear as a benefit, but over time, this reading suggests it transforms into a trap. Taxpayers consistently view it as an extractive burden. Bureaucratic administrators, however, perceive it as a legitimate and necessary function, expanding their institutional scope.
 *
 * DIRECTIONALITY LOGIC:
 *   UBI recipients are victims (high d) as they are seen to lose agency and skills, becoming dependent. Taxpayers are also victims (high d) as they fund the system without perceived productive returns. Bureaucratic administrators are beneficiaries (low d) due to expanded institutional power and budgets. The 'dependency trap' mechanism itself acts as a form of identity-lock for recipients, making exit difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a 'Rope' (pure coordination) by highlighting the significant, asymmetric extraction from recipients and taxpayers. It emphasizes that while a coordination story (poverty reduction) exists, the actual operation, from this reading's perspective, creates a welfare trap, aligning it with a 'Snare'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_causality,
    'Is the observed long-term idleness and skill atrophy among UBI recipients primarily caused by the unconditional nature of the income support, or by pre-existing labor market conditions and individual circumstances?',
    'Longitudinal studies comparing UBI recipients with control groups in different labor market contexts, controlling for pre-existing conditions and alternative support structures.',
    'If primarily caused by UBI, the ''snare'' classification is strengthened. If primarily by external factors, the extractiveness attributed to UBI itself would decrease, potentially shifting the classification towards a ''Tangled Rope'' or even ''Rope'' if the coordination function is re-emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_causality, empirical, 'Distinguishing the causal drivers of dependency and skill atrophy.').

omega_variable(
    taxpayer_burden_perception,
    'To what extent is the ''burden'' perceived by taxpayers a direct economic cost versus a normative objection to non-conditional support?',
    'Public opinion surveys disaggregating economic concerns from moral/ideological objections to welfare, combined with economic modeling of fiscal impacts.',
    'If the burden is primarily normative, the ''extractiveness'' from taxpayers might be re-evaluated as a ''preference'' rather than a structural economic cost, potentially altering the overall extractiveness score and classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taxpayer_burden_perception, conceptual, 'Decomposing the nature of taxpayer ''extraction''.').

omega_variable(
    reading_framing_legitimacy,
    'Is the ''dependency trap'' framing a legitimate interpretation of the effects of unconditional income support, or a politically motivated narrative designed to justify conditionality?',
    'Analysis of the historical and political context of the ''dependency trap'' discourse, its proponents, and its empirical basis, compared against alternative framings.',
    'If the framing is found to be primarily political rather than empirically robust, the legitimacy of this reading''s high extractiveness and snare classification would be challenged, potentially favoring a different reading of the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_legitimacy, conceptual, 'Assessing the political vs. empirical grounding of the dependency trap narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__dependency_trap_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__dependency_trap_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__dependency_trap_reading, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.83).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__dependency_trap_reading, base_extractiveness, 15, 0.84).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__dependency_trap_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__dependency_trap_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__dependency_trap_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, labor_market_flexibility).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, social_safety_net_design).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
