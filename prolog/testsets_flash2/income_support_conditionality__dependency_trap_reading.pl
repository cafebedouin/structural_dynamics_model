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
 *   unconditional income support. From this perspective, providing income
 *   without conditions, while seemingly beneficial, creates a snare where
 *   recipients become dependent on the state, lose work incentives, and
 *   suffer skill atrophy. The constraint is not the income support itself,
 *   but the structural outcome of long-term dependency and reduced labor
 *   force participation that this reading attributes to it. The claimed type
 *   is 'snare' because it is seen as trapping individuals in a state that
 *   extracts their productive potential and imposes costs on society.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.85).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.75).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, 'e96e6455-9d8c-4b72-bcf1-81f00ecd4482').
narrative_ontology:cs_kernel_codification('e96e6455-9d8c-4b72-bcf1-81f00ecd4482', formalized).
narrative_ontology:cs_authority_grounding('e96e6455-9d8c-4b72-bcf1-81f00ecd4482', practice).
narrative_ontology:cs_interpretation_layer_present('e96e6455-9d8c-4b72-bcf1-81f00ecd4482').
narrative_ontology:cs_reading_relation('e96e6455-9d8c-4b72-bcf1-81f00ecd4482', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('e96e6455-9d8c-4b72-bcf1-81f00ecd4482', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('e96e6455-9d8c-4b72-bcf1-81f00ecd4482', foundational, work_as_moral_imperative).
narrative_ontology:cs_axiom_status(work_as_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('e96e6455-9d8c-4b72-bcf1-81f00ecd4482', work_as_moral_imperative, deontological).
narrative_ontology:cs_axiom('e96e6455-9d8c-4b72-bcf1-81f00ecd4482', foundational, unconditional_income_erodes_human_capital).
narrative_ontology:cs_axiom_status(unconditional_income_erodes_human_capital, holdable).
narrative_ontology:cs_axiom_grounding('e96e6455-9d8c-4b72-bcf1-81f00ecd4482', unconditional_income_erodes_human_capital, empirically_contingent).
narrative_ontology:cs_reference_frame('e96e6455-9d8c-4b72-bcf1-81f00ecd4482', conditional_welfare_state).
narrative_ontology:cs_drift_state('e96e6455-9d8c-4b72-bcf1-81f00ecd4482', contemporary_ubi_experiments, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e96e6455-9d8c-4b72-bcf1-81f00ecd4482', '').
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

% Individuals receiving unconditional income support, who, from this reading's perspective, become trapped in idleness, experience skill degradation, and lose the incentive to seek employment, effectively paying with their human capital and autonomy.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, biographical, identity_locked, local).

% Citizens who fund the unconditional income support programs through taxes. From this reading, they bear the cost of supporting non-productive individuals and the broader societal cost of reduced labor force participation.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers, payer,
    organized, generational, constrained, national).

% Government agencies and personnel responsible for distributing and managing unconditional income support programs. They benefit from the expansion of their mandate, budget, and administrative power, even if the program's stated goals are not met.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, bureaucratic_administrators, beneficiary,
    institutional, biographical, mobile, national).

% Groups and individuals who advocate for policies that emphasize work incentives and conditionality in social welfare programs. They actively shape the narrative around dependency and push for reforms that would dismantle unconditional support.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, policy_advocates_for_work_incentives, agenda_setter,
    powerful, generational, analytical, national).

% Businesses seeking to fill entry-level or low-wage positions, who find it harder to attract workers when unconditional income support is available. They are excluded from the policy-making process that creates the 'dependency trap' but bear its consequences.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, employers_seeking_labor, excluded,
    powerful, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a basic safety net for individuals, ensuring minimal subsistence without the administrative overhead of means-testing or work requirements, ostensibly to reduce poverty and administrative burden.
% TRANSFER_FUNCTION: Transfers financial resources from taxpayers to UBI recipients, with the implicit transfer of labor market participation and skill development from recipients to the state/society.
% ABSENT_VOICES: Future generations who will inherit a society with potentially lower productivity and higher tax burdens due to long-term dependency are absent from the current policy debate. Employers struggling to find labor are also largely excluded from the framing of UBI as a dependency trap.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, the labor market would see an immediate influx of individuals seeking work, potentially depressing wages but increasing labor supply. The administrative apparatus for UBI would dissolve, and social welfare systems would revert to conditional models. The 'dependency trap' would cease to exist, but new forms of poverty and precarity might emerge.
% FOUNDING_PROBLEM: The problem of poverty and economic insecurity, coupled with the administrative complexity and stigmatization associated with traditional conditional welfare programs.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for unconditional income support attest that poverty and insecurity remain live problems, and that UBI is a solution. Critics (from this reading) argue that while poverty is real, UBI exacerbates other problems like dependency, and that the original problem is now 'dead' in the sense that UBI creates a worse one; economic studies on labor force participation and skill development from independent research institutions corroborate the dependency concern.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the system is seen as extracting productive capacity and self-reliance from recipients, and financial resources from taxpayers without commensurate societal benefit. Suppression (0.75) is also high, as recipients are 'suppressed' from the labor market by the disincentive effect, and taxpayers are suppressed from opting out of funding the system. Theater ratio is low (0.1) because, from this reading, the system is genuinely (and negatively) functional in creating dependency, not merely performative. Accessibility collapse (0.65) is moderate, as the 'trap' is not absolute but makes exit from dependency difficult. Resistance (0.4) is present from policy advocates for work incentives and taxpayers, but not strong enough to dismantle the system.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between those who see unconditional income support as a liberatory 'freedom floor' (freedom_floor_reading) and those who see it as a 'dependency trap' (this reading). The former emphasizes the positive freedom from coercive labor, while the latter emphasizes the negative consequences for individual agency and societal productivity. The engine's classification will highlight how these different framings lead to vastly different structural assessments of the same policy.
 *
 * DIRECTIONALITY LOGIC:
 *   UBI recipients are the primary victims (d=1.0) as they are seen as losing their agency and skills. Taxpayers are also victims (d=0.8) as they fund a system that, in this reading, yields negative societal outcomes. Bureaucratic administrators are beneficiaries (d=0.1) due to expanded roles and budgets. Policy advocates for work incentives are agenda-setters (d=0.5) as they actively contest the constraint, while employers seeking labor are excluded (d=0.9) as they bear the costs without direct influence.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the original mandate of alleviating poverty is undermined by the creation of dependency. The constraint's persistence is not due to its original function, but because it creates a new, undesirable steady state of idleness and skill atrophy. The classification as a snare prevents mislabeling it as a benign coordination mechanism, highlighting the extractive and suppressive aspects of the 'trap'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_causality_ambiguity,
    'Is the observed dependency and skill atrophy a direct causal consequence of unconditional income support, or are there confounding socioeconomic factors at play?',
    'Longitudinal studies with robust control groups, comparing UBI recipients to similar populations without UBI, controlling for pre-existing conditions, local labor market dynamics, and educational opportunities.',
    'If causality is weak, the extractiveness and suppression metrics for UBI recipients would be lower, potentially reclassifying the constraint away from a snare. If causality is strong, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_causality_ambiguity, empirical, 'Uncertainty regarding the direct causal link between UBI and dependency/skill atrophy.').

omega_variable(
    social_value_of_non_market_activity,
    'Does ''dependency'' accurately capture the full range of activities (e.g., caregiving, community work, artistic pursuits) that UBI recipients might engage in, which are not recognized by market-centric definitions of ''work''?',
    'Qualitative sociological studies and time-use surveys of UBI recipients, coupled with a re-evaluation of ''productive'' activity in national accounts and social policy frameworks.',
    'If non-market activities are recognized as valuable, the ''extraction'' of productive potential would be re-evaluated downward, as would the ''suppression'' from the labor market, potentially weakening the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_value_of_non_market_activity, conceptual, 'Ambiguity in defining ''productive'' activity and its impact on dependency metrics.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''dependency trap'' as described, or is it an alternative reading of the ''income_support_conditionality'' kernel that emphasizes negative outcomes?',
    'Analysis of the policy''s long-term effects across diverse economic contexts and comparison with outcomes predicted by other readings (e.g., freedom_floor_reading, wage_subsidy_reading).',
    'If this reading is found to be a mischaracterization, the constraint would be reclassified to align with a more accurate structural description, potentially shifting to a different type or having significantly altered metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one specific reading of the ''income_support_conditionality'' kernel, emphasizing the dependency trap.').


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
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__dependency_trap_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__dependency_trap_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__dependency_trap_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__dependency_trap_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__dependency_trap_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__dependency_trap_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'income_support_conditionality' kernel. Other readings include 'freedom_floor_reading' and 'wage_subsidy_reading', which offer alternative structural analyses of the same policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
