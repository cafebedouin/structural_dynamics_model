% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support (Freedom Floor Reading)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story represents the 'freedom floor' reading of
 *   unconditional income support, where the policy is understood as a
 *   mechanism to enhance individual autonomy, reduce labor market coercion,
 *   and eliminate welfare stigma. It is framed as a Pareto improvement, with
 *   minimal extraction and high coordination benefits, particularly for
 *   vulnerable populations. This reading emphasizes the positive
 *   externalities of a secure baseline income on social well-being and
 *   economic dynamism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.25).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.1).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support (Freedom Floor Reading)").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '3bb85c41-516d-4bba-b6af-7506ea352dd1').
narrative_ontology:cs_kernel_codification('3bb85c41-516d-4bba-b6af-7506ea352dd1', formalized).
narrative_ontology:cs_authority_grounding('3bb85c41-516d-4bba-b6af-7506ea352dd1', lineage).
narrative_ontology:cs_interpretation_layer_present('3bb85c41-516d-4bba-b6af-7506ea352dd1').
narrative_ontology:cs_reading_relation('3bb85c41-516d-4bba-b6af-7506ea352dd1', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('3bb85c41-516d-4bba-b6af-7506ea352dd1', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('3bb85c41-516d-4bba-b6af-7506ea352dd1', foundational, autonomy_is_foundational_good).
narrative_ontology:cs_axiom_status(autonomy_is_foundational_good, holdable).
narrative_ontology:cs_axiom_grounding('3bb85c41-516d-4bba-b6af-7506ea352dd1', autonomy_is_foundational_good, deontological).
narrative_ontology:cs_axiom('3bb85c41-516d-4bba-b6af-7506ea352dd1', foundational, economic_precarity_is_coercive).
narrative_ontology:cs_axiom_status(economic_precarity_is_coercive, holdable).
narrative_ontology:cs_axiom_grounding('3bb85c41-516d-4bba-b6af-7506ea352dd1', economic_precarity_is_coercive, empirically_contingent).
narrative_ontology:cs_reference_frame('3bb85c41-516d-4bba-b6af-7506ea352dd1', liberal_autonomy_framework).
narrative_ontology:cs_drift_state('3bb85c41-516d-4bba-b6af-7506ea352dd1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3bb85c41-516d-4bba-b6af-7506ea352dd1', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_victims).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, all_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, taxpayers).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, labor_market_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives a baseline income, enabling them to refuse exploitative labor, pursue better opportunities, or invest in education without immediate financial precarity. This reduces their vulnerability to coercive labor practices.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, national).

% Receives income that values their unpaid labor, providing financial stability and recognition for essential social contributions, reducing the pressure to enter the formal labor market out of necessity.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, constrained, national).

% Gains financial security to pursue creative work without needing to monetize it immediately or compromise artistic integrity for survival, fostering cultural production and innovation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists, beneficiary,
    moderate, biographical, constrained, national).

% Receives an independent income stream, providing a critical resource to leave abusive relationships or situations where financial dependency is a tool of control. This enhances their autonomy and safety.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_victims, beneficiary,
    powerless, immediate, identity_locked, local).

% Contributes through taxes to fund the universal income program. While bearing a financial cost, they benefit from a more stable society, reduced social welfare administration, and a more dynamic labor market.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, taxpayers, payer,
    organized, generational, mobile, national).

% Faces a labor market where workers have greater bargaining power and are less compelled to accept low wages or poor conditions. This may necessitate higher wages or improved working conditions, increasing labor costs.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, labor_market_employers, payer,
    powerful, biographical, mobile, national).

% Evaluates the program's impact on poverty, labor participation, public health, and social cohesion, using empirical data to assess its effectiveness as an autonomy-enabling floor.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, social_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, unconditional income floor that coordinates individual economic security, enabling greater freedom in labor market participation and reducing the administrative overhead of means-tested welfare programs.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base to all citizens, ensuring a baseline income regardless of employment status or other conditions.
% ABSENT_VOICES: Those who believe that income should be strictly tied to labor contribution or that universal programs are inherently inefficient would object, arguing for targeted welfare or market-based solutions. Their voices are present in political discourse but are not the primary focus of this reading's justification.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, many precarious workers, caregivers, artists, and abuse victims would immediately lose their financial floor, forcing them back into coercive labor or dependent situations. The labor market would revert to its prior state of imbalanced power, and social safety nets would become more complex and stigmatizing.
% FOUNDING_PROBLEM: The problem of pervasive economic precarity, welfare stigma, and the coercive nature of a labor market where individuals must accept any work to survive, leading to poor working conditions and limited autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Social policy researchers, labor economists, and human rights organizations consistently corroborate the ongoing problems of precarity and labor market coercion. Pilot program participants and advocacy groups for vulnerable populations also attest to the problem's persistence and the efficacy of UBI in addressing it.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.25) because the primary goal is to provide a floor, not to extract rents; any 'cost' is seen as an investment in social capital and individual freedom. Suppression is low (0.1) as the system aims to reduce, not impose, coercion. Theater ratio is minimal (0.05) because the program's stated function (providing an unconditional floor) aligns directly with its operation. The metrics reflect the ideal operation of the 'freedom floor' reading, where the system is genuinely a Rope.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, the constraint is a pure Rope, enabling freedom and reducing precarity. From the perspective of some payers (e.g., certain employers), it might be perceived as a cost without sufficient benefit, or even as an interference with market mechanisms. However, this 'freedom floor' reading argues that the societal benefits (reduced crime, improved health, increased entrepreneurship) ultimately benefit all, including payers, making it a net positive coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious workers, caregivers, artists, and abuse victims are clear beneficiaries (d near 0.0) as the income directly addresses their precarity and enhances their autonomy. Taxpayers and labor market employers are payers (d near 1.0) due to the financial contributions and potential shifts in labor market dynamics, though the reading emphasizes broader societal benefits that mitigate these costs. All citizens are indirect beneficiaries of a more stable and equitable society.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_effects,
    'What are the actual long-term effects of unconditional income support on labor supply and participation rates, particularly for different demographic groups?',
    'Longitudinal studies and large-scale randomized control trials (RCTs) over several years, tracking employment, hours worked, and educational attainment.',
    'If labor supply significantly decreases, it would challenge the ''autonomy-enabling'' aspect by suggesting a disincentive to work, potentially shifting the classification towards a more extractive or less efficient coordination mechanism. If effects are minimal or positive (e.g., increased entrepreneurship), it reinforces the ''freedom floor'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_effects, empirical, 'Empirical impact on labor market participation.').

omega_variable(
    welfare_stigma_reduction,
    'To what extent does unconditional income support genuinely reduce welfare stigma compared to means-tested programs, and how is this measured across diverse cultural contexts?',
    'Sociological studies, qualitative interviews, and psychological assessments of recipients'' self-perception and social integration, comparing UBI recipients to those in traditional welfare systems.',
    'If stigma persists or new forms emerge, it would weaken the claim of eliminating welfare stigma, suggesting a gap between the intended and actual social coordination function. If stigma is significantly reduced, it strengthens the ''freedom floor'' reading''s claim of a more dignified social safety net.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_stigma_reduction, empirical, 'Effectiveness in reducing welfare stigma.').

omega_variable(
    coercion_definition_ambiguity,
    'Is the ''coercion'' removed from the labor market by UBI a genuine structural coercion, or is it a natural consequence of economic scarcity that UBI merely subsidizes?',
    'Philosophical and economic analysis of the concept of ''structural coercion'' in labor markets, distinguishing between market-driven necessity and exploitative power dynamics. This is a conceptual distinction that empirical data can inform but not fully resolve.',
    'If the ''coercion'' is primarily a natural scarcity, the ''freedom floor'' reading''s claim of removing coercion is weakened, potentially re-framing the benefit as a subsidy rather than a liberation. If it is clearly structural exploitation, the reading is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_definition_ambiguity, conceptual, 'Conceptual distinction between structural coercion and natural scarcity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__freedom_floor_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__freedom_floor_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(unco_tr_t15, unconditional_income_support__freedom_floor_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__freedom_floor_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__freedom_floor_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__freedom_floor_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(unco_be_t15, unconditional_income_support__freedom_floor_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__freedom_floor_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__freedom_floor_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__freedom_floor_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__freedom_floor_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(unco_su_t15, unconditional_income_support__freedom_floor_reading, suppression_requirement, 15, 0.1).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__freedom_floor_reading, suppression_requirement, 20, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'unconditional_income_support' kernel: 'freedom_floor_reading', 'dependency_trap_reading', and 'universality_paradox_reading'. Each represents a distinct structural claim about the policy's function and effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
