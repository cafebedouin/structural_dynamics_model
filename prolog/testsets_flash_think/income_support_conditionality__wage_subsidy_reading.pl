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
 *   constraint_id: income_support_conditionality__wage_subsidy_reading
 *   human_readable: Income Support as Employer Wage Subsidy
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'wage_subsidy_reading' of the
 *   'income_support_conditionality' kernel. It posits that unconditional
 *   income support, while ostensibly a social safety net, primarily functions
 *   as an indirect subsidy to employers of low-wage labor. By ensuring worker
 *   subsistence, it removes the market pressure for employers to offer living
 *   wages, thereby suppressing overall wage levels and entrenching precarious
 *   work. The constraint is classified as a Tangled Rope because it provides
 *   a genuine coordination function (social stability, poverty reduction) but
 *   simultaneously extracts value (via wage suppression) from low-wage
 *   workers for the benefit of employers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.8).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.8).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, 'a59d6066-1af5-4108-9ab9-4fd1a2fceba5').
narrative_ontology:cs_kernel_codification('a59d6066-1af5-4108-9ab9-4fd1a2fceba5', formalized).
narrative_ontology:cs_authority_grounding('a59d6066-1af5-4108-9ab9-4fd1a2fceba5', practice).
narrative_ontology:cs_interpretation_layer_present('a59d6066-1af5-4108-9ab9-4fd1a2fceba5').
narrative_ontology:cs_reading_relation('a59d6066-1af5-4108-9ab9-4fd1a2fceba5', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('a59d6066-1af5-4108-9ab9-4fd1a2fceba5', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('a59d6066-1af5-4108-9ab9-4fd1a2fceba5', foundational, income_support_subsidizes_low_wages).
narrative_ontology:cs_axiom_status(income_support_subsidizes_low_wages, holdable).
narrative_ontology:cs_axiom_grounding('a59d6066-1af5-4108-9ab9-4fd1a2fceba5', income_support_subsidizes_low_wages, empirically_contingent).
narrative_ontology:cs_reference_frame('a59d6066-1af5-4108-9ab9-4fd1a2fceba5', worker_subsistence_safety_net).
narrative_ontology:cs_drift_state('a59d6066-1af5-4108-9ab9-4fd1a2fceba5', contemporary_labor_market, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a59d6066-1af5-4108-9ab9-4fd1a2fceba5', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, employers_of_low_wage_labor).
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

% Benefit from a stable supply of labor at suppressed wages, as income support reduces the pressure to offer competitive, living wages. They can maintain lower labor costs without workers facing absolute destitution.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, employers_of_low_wage_labor, beneficiary,
    institutional, biographical, mobile, national).

% Receive income support, which prevents destitution but also reduces their bargaining power. They are effectively locked into low-wage jobs, as the support system makes these jobs economically viable for employers, rather than enabling workers to exit for better opportunities.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, immediate, constrained, national).

% Administer the income support programs, aiming to reduce poverty and ensure social stability. From this reading, they inadvertently facilitate wage suppression by employers, becoming an enforcer of the tangled rope.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, government_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Fund the income support programs through taxes. They bear the cost of subsidizing low-wage labor, often without realizing the extent to which these funds flow to employers via wage suppression.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, taxpayers, payer,
    moderate, biographical, mobile, national).

% Critique the income support system for its role in depressing wages and undermining worker power, arguing it institutionalizes precarious labor rather than empowering workers.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_unions_advocates, observer,
    organized, generational, analytical, national).

% Propose income support as a means to decommodify labor and increase worker freedom. From this reading, their vision is foreclosed by the actual operation of the system, which is captured by employers.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, freedom_floor_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, employers_of_low_wage_labor).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a basic safety net for individuals, ensuring subsistence and maintaining a stable labor supply for low-wage sectors, preventing widespread social unrest or labor shortages.
% TRANSFER_FUNCTION: Transfers public funds (from taxpayers) to low-wage workers, which, through market mechanisms, effectively transfers a portion of labor costs from employers to the public purse.
% ABSENT_VOICES: Advocates for income support as a genuine 'freedom floor' or 'decommodification' mechanism would object, arguing that the current structure subverts these goals by subsidizing employers and entrenching low wages.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, low-wage workers would face immediate and severe hardship, leading to widespread poverty, potential labor market disruptions (as workers could no longer afford to take low-paying jobs), and increased social instability. Employers would face pressure to raise wages significantly or automate.
% FOUNDING_PROBLEM: To alleviate poverty, reduce economic insecurity, and provide a basic standard of living for all citizens, particularly those in precarious employment or unable to work.
% FOUNDING_PROBLEM_CORROBORATION: Government reports and some social policy experts corroborate the founding problem of poverty and insecurity. However, labor economists and unions, from outside the direct beneficiaries, corroborate that the problem's *solution* has been captured, leading to wage suppression.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because a significant portion of the public funds intended for workers is effectively captured by employers through lower wage costs. Suppression is also high, as the system reduces workers' bargaining power and exit options from low-wage employment. Theater ratio is low, as the system's function, even if captured, is direct and not primarily performative. Accessibility collapse is moderate: while it makes low-wage work 'accessible' by subsidizing it, it doesn't entirely collapse alternatives, though it makes them less attractive. Resistance is moderate from labor groups and some economists who highlight the subsidy effect.
 *
 * PERSPECTIVAL GAP:
 *   The government and some proponents of income support view it as a pure social safety net (closer to a Rope or Scaffold). However, from the perspective of low-wage workers and labor advocates, the same system operates as a mechanism for wage suppression and employer subsidy (a Snare or Tangled Rope). The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Employers of low-wage labor are the primary beneficiaries (low d) as they gain from reduced wage pressure. Low-wage workers are the primary targets (high d), as they receive support but at the cost of suppressed wages and reduced autonomy. Taxpayers are also targets, as they fund the subsidy. Government agencies are agenda-setters, administering the system. Labor unions and advocates are observers, while freedom-floor advocates are excluded, as their alternative framing is not realized by the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_elasticity_ambiguity,
    'To what extent do wages for low-income workers actually adjust downward in response to unconditional income support, and what is the magnitude of this effect?',
    'Empirical studies on labor market responses to UBI or similar programs, comparing wage trends in regions with and without such policies, controlling for other economic factors.',
    'If wage elasticity is low, the employer subsidy effect is minor, shifting the constraint closer to a Rope. If elasticity is high, the subsidy effect is substantial, reinforcing the Tangled Rope classification and increasing measured extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_elasticity_ambiguity, empirical, 'Uncertainty regarding the empirical magnitude of wage suppression due to income support.').

omega_variable(
    subsidy_capture_mechanism,
    'Is the employer subsidy a direct, intentional capture of the income support system, or an emergent, unintended market outcome?',
    'Analysis of policy design documents, lobbying efforts by employer groups, and economic modeling of market equilibrium shifts. Intentional capture would point to a more direct Snare-like mechanism.',
    'If intentional capture, the suppression and extractiveness metrics are more robustly tied to the constraint''s design. If emergent, the constraint''s classification might lean more towards a degraded Rope or a less malicious Tangled Rope, though the outcome for workers remains the same.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidy_capture_mechanism, conceptual, 'Ambiguity regarding the intentionality and mechanism of employer capture of income support benefits.').

omega_variable(
    framing_ambiguity_income_support,
    'Is unconditional income support primarily a ''freedom floor'' (decommodifying labor), a ''dependency trap'' (undermining work incentives), or an ''employer wage subsidy'' (suppressing wages)?',
    'This is a conceptual omega for the kernel itself. Resolution depends on which structural effects are prioritized and how normative goals are framed. Empirical evidence on labor market outcomes (wage elasticity, employment rates, worker autonomy) would inform the relative strength of each reading.',
    'The classification of the income_support_conditionality kernel depends on which reading is adopted. Each reading (freedom_floor, dependency_trap, wage_subsidy) yields a different constraint type and beneficiary/victim structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_ambiguity_income_support, conceptual, 'The core ambiguity of the income_support_conditionality kernel, with three competing readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__wage_subsidy_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__wage_subsidy_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__wage_subsidy_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(inco_tr_t25, income_support_conditionality__wage_subsidy_reading, theater_ratio, 25, 0.17).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__wage_subsidy_reading, theater_ratio, 30, 0.18).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(inco_be_t25, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(inco_su_t25, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 30, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
