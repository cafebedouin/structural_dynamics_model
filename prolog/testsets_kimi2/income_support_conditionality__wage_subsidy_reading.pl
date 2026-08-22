% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-27
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
 *   human_readable: Unconditional Income Support as Employer Wage Subsidy
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint is the wage_subsidy_reading of the contested kernel
 *   income_support_conditionality. It treats unconditional income support not
 *   as a decommodifying freedom floor but as a structural subsidy to low-wage
 *   employers. In this reading, the transfer is captured via downward wage
 *   adjustment: employers pay below-subsistence wages because the state makes
 *   up the difference, institutionalizing a low-wage equilibrium that workers
 *   cannot exit without losing the wage component entirely. The genuine
 *   coordination function (preventing destitution) and the asymmetric
 *   extraction (employer capture via suppressed wages) coexist, making the
 *   constraint a tangled rope.
 *
 * KEY AGENTS:
 *   - low_wage_employers (organized/arbitrage): structural beneficiaries who capture the transfer through suppressed payroll costs
 *   - low_wage_workers (powerless/constrained): payers who receive the transfer but remain trapped in a low-wage equilibrium
 *   - state_welfare_apparatus (institutional/constrained): agenda-setter that administers the program and frames it as social protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.72).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.65).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, '20f748fe-0169-4ce5-b202-732caefde553').
narrative_ontology:cs_kernel_codification('20f748fe-0169-4ce5-b202-732caefde553', formalized).
narrative_ontology:cs_authority_grounding('20f748fe-0169-4ce5-b202-732caefde553', lineage).
narrative_ontology:cs_interpretation_layer_present('20f748fe-0169-4ce5-b202-732caefde553').
narrative_ontology:cs_reading_relation('20f748fe-0169-4ce5-b202-732caefde553', income_support_conditionality__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('20f748fe-0169-4ce5-b202-732caefde553', income_support_conditionality__dependency_trap_reading, influences).
narrative_ontology:cs_axiom('20f748fe-0169-4ce5-b202-732caefde553', foundational, transfer_captured_by_employers).
narrative_ontology:cs_axiom_status(transfer_captured_by_employers, holdable).
narrative_ontology:cs_axiom_grounding('20f748fe-0169-4ce5-b202-732caefde553', transfer_captured_by_employers, empirically_contingent).
narrative_ontology:cs_axiom('20f748fe-0169-4ce5-b202-732caefde553', foundational, low_wage_labor_institutionalized).
narrative_ontology:cs_axiom_status(low_wage_labor_institutionalized, holdable).
narrative_ontology:cs_axiom_grounding('20f748fe-0169-4ce5-b202-732caefde553', low_wage_labor_institutionalized, empirically_contingent).
narrative_ontology:cs_reference_frame('20f748fe-0169-4ce5-b202-732caefde553', social_protection_floor).
narrative_ontology:cs_drift_state('20f748fe-0169-4ce5-b202-732caefde553', post_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('20f748fe-0169-4ce5-b202-732caefde553', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive a labor force willing to accept below-subsistence nominal wages because unconditional income support closes the gap to subsistence. Their payroll costs are structurally lower than they would be in a market without the transfer. They organize to maintain the program and resist minimum-wage hikes that would eliminate the wedge.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    organized, biographical, arbitrage, national).

% Receive unconditional transfers that bring total household income to subsistence, but face a labor market where nominal wages are suppressed precisely because the subsidy exists. Their effective bargaining power is undermined; exiting low-wage work means relying solely on the politically fragile transfer component, while staying means permanent low-wage attachment.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, immediate, constrained, national).

% Administers the unconditional transfer program, disbursing payments and enforcing eligibility and tax rules. Frames the program as poverty alleviation and social protection. It does not directly capture the subsidy but maintains political legitimacy through the program's scale and the labor-market stability it underwrites.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, state_welfare_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a subsistence floor for working-age adults without means-tested administration, stabilizing household income and reducing absolute destitution.
% TRANSFER_FUNCTION: Moves public funds from the general tax base to low-wage households, with a structural wedge allowing employers to pay wages below subsistence levels because the transfer closes the gap.
% ABSENT_VOICES: Workers organized around genuine wage-bargaining power and anti-poverty advocates who would challenge the employer-capture mechanism; rival policy frameworks that would mandate living-wage laws or sectoral bargaining alongside transfers.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, low-wage employers would face labor shortages or be forced to raise wages to subsistence levels, collapsing the current low-wage equilibrium; low-wage workers would face immediate destitution or be driven into underground economies.
% FOUNDING_PROBLEM: Absolute poverty and destitution among working-age adults, and the administrative complexity and stigma of means-tested welfare.
% FOUNDING_PROBLEM_CORROBORATION: Social policy historians attest the poverty problem was live at founding. Labor economists outside the employer beneficiary set attest the arrangement now functions as a labor-market subsidy; worker advocacy organizations corroborate that the founding problem is not addressed by a mechanism that institutionalizes low-wage labor.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.72) is high because a substantial share of the transfer is captured by employers through wage compression rather than retained by workers as net income. Suppression (0.65) reflects the active enforcement required to maintain the eligibility rules and the closure of alternative labor-market organization (e.g., sectoral bargaining that would reclaim the wedge). Theater ratio (0.40) captures the growing gap between the rhetoric of unconditional dignity and the reality of employer-subsidized low-wage labor. Accessibility collapse (0.58) measures how thoroughly the subsidy closes off the alternative of a genuine subsistence-wage labor market. Resistance (0.48) is moderate: worker advocates and some labor economists resist the framing, but the diffuse nature of the capture keeps resistance fragmented.
 *
 * PERSPECTIVAL GAP:
 *   The employer seat experiences the constraint as benign coordination (stable labor supply, lower payroll costs, reduced turnover), while the worker seat experiences it as extraction (wages held below subsistence because the state closes the gap, leaving no bargaining leverage). The state apparatus sits nearer the middle: it administers a coordination mechanism but does not directly collect the extraction, instead trading political legitimacy for labor-market stability. The engine will compute divergent per-seat types from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Employers are declared beneficiaries and receive a low directionality (subsidy flows to them via suppressed wages). Workers are declared victims/payers and receive a high directionality (the constraint extracts from them through institutionalized low-wage attachment). The state agenda-setter is not a beneficiary in the receipt sense but enables the structure; its directionality is structurally derived as moderate because it neither collects the extraction nor bears the direct cost. No override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was poverty and destitution. The wage_subsidy_reading sees the current arrangement as having drifted (or been designed) into a labor-market subsidy function that persists beyond the original anti-poverty mandate. Classifying it as tangled_rope rather than snare prevents mislabeling the genuine coordination function (worker subsistence) as pure extraction, while still capturing the asymmetric employer capture that makes the arrangement substantially extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is the wage_subsidy_reading of kernel income_support_conditionality; do the sibling readings (freedom_floor_reading, dependency_trap_reading) describe the same policy under different empirical premises, or do they describe structurally different policies?',
    'Comparative policy analysis across jurisdictions with varying income support levels, labor market institutions, and wage-setting mechanisms; if the same formal program produces divergent structural outcomes depending on context, the readings are context-dependent facets rather than mutually exclusive constraints.',
    'If outcomes are context-dependent, the kernel decomposes into situation-specific constraints rather than a single contested kernel; if mutually exclusive, one reading''s empirical core must be falsified by evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the three kernel readings are context-sensitive descriptions or falsifying alternatives.').

omega_variable(
    employer_capture_magnitude,
    'To what degree do low-wage employers empirically capture unconditional income support through wage suppression versus workers retaining the transfer as a net income gain?',
    'Natural experiments from policy variations in transfer levels and minimum-wage regimes; econometric analysis of wage pass-through and reservation-wage effects in low-wage labor markets.',
    'High capture validates the tangled_rope classification and the employer beneficiary structure; low capture would shift the constraint toward a rope or freedom-floor reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_capture_magnitude, empirical, 'Empirical degree of employer wage capture from unconditional transfers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(income_support_wage_subsidy_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(income_support_wage_subsidy_tr_t4, income_support_conditionality__wage_subsidy_reading, theater_ratio, 4, 0.23).
narrative_ontology:measurement(income_support_wage_subsidy_tr_t8, income_support_conditionality__wage_subsidy_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(income_support_wage_subsidy_tr_t12, income_support_conditionality__wage_subsidy_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(income_support_wage_subsidy_tr_t16, income_support_conditionality__wage_subsidy_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(income_support_wage_subsidy_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(income_support_wage_subsidy_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(income_support_wage_subsidy_be_t4, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(income_support_wage_subsidy_be_t8, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(income_support_wage_subsidy_be_t12, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(income_support_wage_subsidy_be_t16, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(income_support_wage_subsidy_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(income_support_wage_subsidy_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(income_support_wage_subsidy_su_t4, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(income_support_wage_subsidy_su_t8, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(income_support_wage_subsidy_su_t12, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 12, 0.61).
narrative_ontology:measurement(income_support_wage_subsidy_su_t16, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(income_support_wage_subsidy_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
