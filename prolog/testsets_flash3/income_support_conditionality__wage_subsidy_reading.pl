% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__wage_subsidy_reading, []).

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
 *   constraint_id: income_support_conditionality__wage_subsidy_reading
 *   human_readable: Unconditional Income Support as Employer Wage Subsidy
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story analyzes unconditional income support from the
 *   'wage subsidy' reading, where it functions as an employer subsidy,
 *   enabling wage suppression while maintaining worker subsistence. The
 *   policy, often framed as a social safety net, is interpreted here as a
 *   mechanism that institutionalizes low-wage labor rather than empowering
 *   workers to exit it. This reading highlights the transfer of public funds
 *   to employers via reduced wage pressure, with low-wage workers remaining
 *   in a victimized position due to captured benefits.
 *
 * KEY AGENTS:
 *   - low_wage_employers: Primary beneficiary (organized/arbitrage) — benefits from reduced wage pressure.
 *   - low_wage_workers: Primary victim/payer (powerless/constrained) — receives subsistence but loses bargaining power.
 *   - taxpayers: Payer (moderate/constrained) — funds the subsidy.
 *   - government_agencies: Agenda-setter (institutional/constrained) — administers the policy, potentially with unintended consequences.
 *   - labor_unions: Excluded (organized/constrained) — would object to wage suppression effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.78).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.65).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, 'ab1b48d9-9c62-4279-8390-664752e3c242').
narrative_ontology:cs_kernel_codification('ab1b48d9-9c62-4279-8390-664752e3c242', formalized).
narrative_ontology:cs_authority_grounding('ab1b48d9-9c62-4279-8390-664752e3c242', lineage).
narrative_ontology:cs_interpretation_layer_present('ab1b48d9-9c62-4279-8390-664752e3c242').
narrative_ontology:cs_reading_relation('ab1b48d9-9c62-4279-8390-664752e3c242', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab1b48d9-9c62-4279-8390-664752e3c242', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('ab1b48d9-9c62-4279-8390-664752e3c242', foundational, labor_market_power_imbalance).
narrative_ontology:cs_axiom_status(labor_market_power_imbalance, holdable).
narrative_ontology:cs_axiom_grounding('ab1b48d9-9c62-4279-8390-664752e3c242', labor_market_power_imbalance, empirically_contingent).
narrative_ontology:cs_axiom('ab1b48d9-9c62-4279-8390-664752e3c242', foundational, subsidy_capture_by_employers).
narrative_ontology:cs_axiom_status(subsidy_capture_by_employers, holdable).
narrative_ontology:cs_axiom_grounding('ab1b48d9-9c62-4279-8390-664752e3c242', subsidy_capture_by_employers, empirically_contingent).
narrative_ontology:cs_reference_frame('ab1b48d9-9c62-4279-8390-664752e3c242', unconditional_income_as_pure_safety_net).
narrative_ontology:cs_drift_state('ab1b48d9-9c62-4279-8390-664752e3c242', contemporary_labor_market_analysis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab1b48d9-9c62-4279-8390-664752e3c242', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a reduced need to pay living wages, as the state provides a baseline income. This allows them to maintain lower labor costs and higher profit margins, effectively externalizing a portion of their wage bill onto taxpayers. They have strong lobbying power to maintain such policies.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    organized, biographical, arbitrage, national).

% Receive a baseline income, which prevents absolute destitution but simultaneously reduces their bargaining power for higher wages. The income support is captured by employers through wage adjustments, leaving workers dependent on both their low wages and the state subsidy, rather than achieving true economic freedom. Their options for exit from low-wage work are limited by the overall labor market structure.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, immediate, constrained, local).

% Fund the unconditional income support through taxes. They bear the cost of the subsidy, which in this reading, primarily benefits employers by suppressing wages rather than directly improving worker welfare or enabling labor market exit. Their ability to influence policy is diffuse.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Administer the income support programs, often with the stated goal of poverty reduction or social safety. In this reading, their policies inadvertently (or knowingly) create a mechanism for wage suppression, requiring ongoing administrative effort to manage the system.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, government_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Would argue that unconditional income support, without strong labor protections, undermines collective bargaining power and contributes to a race to the bottom in wages. Their voice is often marginalized in policy debates framed around 'social safety nets' rather than 'labor market power'.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a baseline income floor, ensuring subsistence for all citizens and stabilizing aggregate demand, which can prevent economic collapse during downturns and reduce administrative overhead compared to conditional welfare programs.
% TRANSFER_FUNCTION: Transfers public funds (from taxpayers) to individuals, which then indirectly flows to low-wage employers by allowing them to pay below-subsistence wages, effectively subsidizing their labor costs.
% ABSENT_VOICES: Labor unions and advocates for higher minimum wages are often sidelined, as the income support is presented as a solution to poverty, obscuring its potential role in wage suppression. They would argue for direct wage increases and stronger worker protections.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, low-wage employers would face immediate pressure to raise wages to subsistence levels, or risk severe labor shortages. This would trigger significant labor market adjustments, potentially leading to business closures for those unable to adapt, and a reorganization of the low-wage sector.
% FOUNDING_PROBLEM: The founding problem was to address poverty, reduce administrative burden of welfare, and provide a safety net against economic shocks and automation-driven job displacement.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (often government agencies and some economists) argue the problem is live, citing persistent poverty and economic precarity. Critics (labor economists, some social policy researchers, and labor unions) argue that while poverty remains, the 'solution' has shifted to subsidize employers, making the original problem's status 'dead' in terms of the constraint's actual effect. Independent academic studies and labor market analyses from outside the benefiting parties corroborate the wage-subsidy effect.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__wage_subsidy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__wage_subsidy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) is high because a significant portion of the income support's value is captured by employers through wage adjustments, rather than accruing to workers as increased disposable income or freedom. Suppression (0.65) is moderate, as workers are not directly coerced but face structural disincentives to demand higher wages due to the baseline income. The theater ratio (0.20) is low, indicating that while the policy has a genuine coordination function (subsistence), a growing part of its operation serves an unstated, extractive purpose (wage suppression). The metrics show a rising trend in extractiveness and suppression over time, suggesting an increasing capture of the policy's benefits by employers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of low-wage employers, the income support is a beneficial policy that stabilizes the labor market and reduces business costs. From the perspective of low-wage workers, it is a mixed blessing that provides subsistence but traps them in a cycle of low wages and dependency. Government agencies may view it as a successful social policy, while labor unions see it as a structural impediment to fair wages. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage employers are clear beneficiaries (d near 0.0) as the policy reduces their wage burden. Low-wage workers are targets (d near 1.0) because the subsidy is captured, reducing their bargaining power and keeping wages low. Taxpayers are also targets (d near 1.0) as they fund the system without receiving direct benefits. Government agencies are agenda-setters (d near 0.5) as they administer the policy, balancing various objectives. Labor unions are excluded, experiencing the constraint as a structural barrier to their goals.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading suggests a form of mandatrophy where a policy designed for social welfare (reducing poverty) has drifted to serve an unintended, extractive function (employer subsidy). The classification as a Tangled Rope prevents mislabeling it as a pure Rope (genuine coordination) or a Snare (pure extraction), acknowledging its dual nature: it provides a safety net while simultaneously enabling wage suppression. The rising extractiveness over time indicates this drift is accumulating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_capture_quantification,
    'What precise percentage of unconditional income support is captured by employers through wage adjustments, versus accruing to workers as net benefit?',
    'Econometric studies analyzing wage elasticity and labor market responses to income support implementation, controlling for other economic factors.',
    'A high capture rate would strongly support the ''wage subsidy'' reading and classify the constraint closer to a Snare; a low rate would shift it towards a Rope or Scaffold, indicating more direct worker benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_capture_quantification, empirical, 'Quantifying the extent of wage capture by employers.').

omega_variable(
    policy_intent_vs_outcome,
    'Was the wage-suppression effect an intended consequence of the policy, or an emergent, unintended outcome?',
    'Analysis of legislative history, policy documents, and statements from policymakers and lobbyists during the policy''s formulation. Interviews with key stakeholders.',
    'If intended, the constraint''s ''claimed_type'' as a Rope would be a deliberate cover story, pushing it towards a Snare. If unintended, it remains a Tangled Rope, highlighting a design flaw or emergent property.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_intent_vs_outcome, conceptual, 'Distinguishing between intentional design and emergent outcomes of the policy.').

omega_variable(
    alternative_framing_impact,
    'How would the classification change if the ''freedom_floor_reading'' or ''dependency_trap_reading'' were adopted as the primary frame?',
    'Generate separate constraint stories for each sibling reading, with their own metrics and stakeholder analyses, then compare the resulting classifications and effective extraction values.',
    'Each reading would yield a different classification and effective extraction profile, demonstrating the perspectival dependence of the constraint''s perceived nature. This highlights the contestability of the ''income_support_conditionality'' kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_framing_impact, conceptual, 'This constraint is one reading of a contested kernel (''income_support_conditionality''). Sibling readings (''freedom_floor_reading'', ''dependency_trap_reading'') would alter the beneficiary/victim sets and the perceived coordination/extraction balance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__wage_subsidy_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__wage_subsidy_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__wage_subsidy_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 10, 0.73).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the 'income_support_conditionality' kernel. It focuses on the wage-subsidy effect, distinct from the 'freedom_floor_reading' (which emphasizes worker empowerment) and the 'dependency_trap_reading' (which emphasizes disincentives to work). All three are structurally distinct claims from the same policy kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
