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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'dependency trap' reading of
 *   unconditional income support. From this perspective, providing income
 *   without work requirements undermines individual work incentives, leading
 *   to long-term dependency on state support and the atrophy of valuable
 *   labor market skills. It is framed as a snare because it traps recipients
 *   in a state of idleness and extracts resources from taxpayers for what is
 *   perceived as unproductive transfers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.85).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.78).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, 'e9304a5b-0205-455c-99e6-1d827b02964c').
narrative_ontology:cs_kernel_codification('e9304a5b-0205-455c-99e6-1d827b02964c', formalized).
narrative_ontology:cs_authority_grounding('e9304a5b-0205-455c-99e6-1d827b02964c', lineage).
narrative_ontology:cs_interpretation_layer_present('e9304a5b-0205-455c-99e6-1d827b02964c').
narrative_ontology:cs_reading_relation('e9304a5b-0205-455c-99e6-1d827b02964c', income_support_conditionality__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('e9304a5b-0205-455c-99e6-1d827b02964c', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('e9304a5b-0205-455c-99e6-1d827b02964c', foundational, work_incentives_are_primary).
narrative_ontology:cs_axiom_status(work_incentives_are_primary, holdable).
narrative_ontology:cs_axiom_grounding('e9304a5b-0205-455c-99e6-1d827b02964c', work_incentives_are_primary, empirically_contingent).
narrative_ontology:cs_axiom('e9304a5b-0205-455c-99e6-1d827b02964c', foundational, unconditional_support_creates_moral_hazard).
narrative_ontology:cs_axiom_status(unconditional_support_creates_moral_hazard, holdable).
narrative_ontology:cs_axiom_grounding('e9304a5b-0205-455c-99e6-1d827b02964c', unconditional_support_creates_moral_hazard, empirically_contingent).
narrative_ontology:cs_reference_frame('e9304a5b-0205-455c-99e6-1d827b02964c', conditional_welfare_state).
narrative_ontology:cs_drift_state('e9304a5b-0205-455c-99e6-1d827b02964c', contemporary_policy_debate, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e9304a5b-0205-455c-99e6-1d827b02964c', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers).
narrative_ontology:constraint_vindicates(income_support_conditionality__dependency_trap_reading, work_ethic_principle).
narrative_ontology:constraint_vindicates(income_support_conditionality__dependency_trap_reading, moral_hazard_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for conditional welfare policies, believing unconditional support leads to dependency and skill atrophy. They shape public discourse and legislative efforts to restrict or condition income support, seeing themselves as protecting societal productivity.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, policy_advocates_dependency_trap, agenda_setter,
    institutional, generational, analytical, national).

% Individuals receiving unconditional income support, who, from this reading's perspective, become trapped in long-term idleness, experience skill degradation, and face high barriers to re-entering the labor market, leading to a cycle of dependency.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, biographical, trapped, national).

% Citizens who fund unconditional income support through taxes. From this reading's perspective, they bear the cost of non-productive transfers and the societal burden of dependency, feeling their contributions are misused.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Researchers and economists who study the effects of income support policies on labor supply, skill development, and economic participation. Their analyses often provide evidence for or against the dependency trap hypothesis, influencing policy debates.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_market_analysts, observer,
    analytical, biographical, analytical, global).

% Government agencies responsible for designing and administering social welfare programs. They implement policies that may reflect or counter the dependency trap concerns, balancing social support with fiscal responsibility and labor market goals.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, social_policy_bureaucracy, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the system primarily functions to distribute income without requiring labor, which is seen as a misallocation rather than a coordination function.
% TRANSFER_FUNCTION: Transfers income from taxpayers to non-working individuals, ostensibly to alleviate poverty, but in this reading, it creates a disincentive to work and fosters long-term reliance.
% ABSENT_VOICES: Advocates for unconditional income as a human right or freedom floor are structurally excluded from this reading's core premise; they would argue against the dependency framing and for the positive societal benefits of decommodified labor.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, individuals would be forced back into the labor market, potentially increasing labor supply and reducing dependency, though with significant social disruption and increased poverty for some. The labor market and social safety net would reorganize around conditional support or other mechanisms.
% FOUNDING_PROBLEM: Poverty and economic insecurity, addressed by providing a basic safety net to ensure subsistence for all citizens.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of conditional welfare attest to the problem's persistence and the need for work incentives. Critics (from other readings) argue the founding problem is misdiagnosed or that the unconditional solution creates new, more severe problems like dependency and skill atrophy. Independent economic analyses often present conflicting evidence on the actual impact on labor supply.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.85) because the system is seen as extracting productive capacity from recipients and financial resources from taxpayers without generating commensurate value. Suppression is high (0.78) as recipients are viewed as 'trapped' by the disincentive to work, with the 'exit cost' being the loss of guaranteed income and the effort required to re-skill and find employment. Theater ratio is low (0.15) because, from this reading, the system is actively and effectively producing its negative outcomes, not merely performing a function. Accessibility collapse is high (0.70) as the availability of unconditional income is seen to reduce the perceived necessity and attractiveness of alternative (work-based) income streams.
 *
 * PERSPECTIVAL GAP:
 *   The 'dependency trap' reading fundamentally diverges from 'freedom floor' or 'wage subsidy' readings. While other readings might see the same income transfer as empowering or as an employer subsidy, this reading focuses exclusively on the negative individual and societal consequences of reduced work incentives. The engine's per-seat classification will highlight how different stakeholders experience this constraint as a snare, while other readings would classify it differently.
 *
 * DIRECTIONALITY LOGIC:
 *   UBI recipients are the primary targets (victims) of this constraint, as they are seen to suffer dependency and skill atrophy. Taxpayers are also targets, bearing the financial cost of the system. There are no direct beneficiaries in this reading, as the system's outcomes are framed as detrimental. Policy advocates for conditional support are agenda-setters, seeking to 'fix' the perceived trap.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a 'rope' or 'scaffold' by emphasizing the extractive and suppressive aspects of the 'dependency trap'. The high extractiveness and suppression, coupled with the identification of clear victims, firmly place it as a snare, despite its stated intention to alleviate poverty. The 'contested' status of the founding problem further supports this, indicating that the original mandate is no longer universally accepted as being addressed effectively or without severe side effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_measurement_ambiguity,
    'How is ''dependency'' and ''skill atrophy'' precisely measured, and are these measures robust against alternative interpretations (e.g., preference for leisure, care work, or structural unemployment)?',
    'Longitudinal studies tracking skill levels, labor market participation, and self-reported well-being of UBI recipients versus control groups, disaggregated by demographic and regional factors.',
    'If dependency and skill atrophy are not robustly demonstrated, the extractiveness and suppression metrics for UBI recipients would decrease, potentially shifting the constraint''s classification away from a snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_measurement_ambiguity, empirical, 'Ambiguity in measuring the core negative outcomes of the dependency trap.').

omega_variable(
    causality_of_idleness,
    'Is idleness (non-participation in formal labor) primarily caused by the unconditional income support, or by other structural factors such as lack of available jobs, disability, caregiving responsibilities, or discrimination?',
    'Comparative studies across regions with varying labor market conditions and social support structures, controlling for individual circumstances and pre-existing barriers to employment.',
    'If idleness is largely attributable to structural factors rather than the income support itself, the suppression metric would decrease, as the constraint would not be the primary ''trap'' mechanism, potentially reclassifying it for recipients.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_of_idleness, empirical, 'Distinguishing the causal role of income support versus other factors in labor market non-participation.').

omega_variable(
    alternative_framing_impact,
    'If the ''freedom_floor_reading'' were adopted, how would the structural classification of unconditional income support change for recipients?',
    'Conceptual analysis of the ''freedom_floor_reading'' to identify its beneficiary/victim structure and metric profile, then compare the resulting classification to this ''dependency_trap_reading''.',
    'The ''freedom_floor_reading'' would likely classify recipients as beneficiaries with high exit options (positive freedom), leading to a rope or scaffold classification for that seat, directly contradicting the snare classification of this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_framing_impact, conceptual, 'Impact of adopting a ''freedom floor'' framing on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__dependency_trap_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__dependency_trap_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(inco_tr_t50, income_support_conditionality__dependency_trap_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__dependency_trap_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__dependency_trap_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(inco_be_t50, income_support_conditionality__dependency_trap_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__dependency_trap_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__dependency_trap_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(inco_su_t40, income_support_conditionality__dependency_trap_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(inco_su_t50, income_support_conditionality__dependency_trap_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'income_support_conditionality' kernel. This 'dependency_trap_reading' focuses on the negative consequences of unconditional support, while the 'freedom_floor_reading' emphasizes positive liberty, and the 'wage_subsidy_reading' highlights employer benefits. Each reading yields a structurally distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
