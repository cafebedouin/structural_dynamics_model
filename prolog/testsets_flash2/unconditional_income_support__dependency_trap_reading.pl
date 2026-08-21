% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__dependency_trap_reading, []).

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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support (Dependency Trap Reading)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story represents the 'dependency trap' reading of
 *   unconditional income support. It argues that such programs, while
 *   ostensibly universal, function as an incentive-distorting subsidy that
 *   rewards idleness, crowds out more effective targeted aid for the truly
 *   needy, and results in an upward redistribution of wealth to non-needy
 *   recipients. The constraint is framed as a snare, actively extracting from
 *   the working poor and taxpayers, while benefiting the middle/upper classes
 *   and UBI advocates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.85).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.7).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support (Dependency Trap Reading)").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, '9d7cb119-d773-4a5b-b014-3ce000c288fa').
narrative_ontology:cs_kernel_codification('9d7cb119-d773-4a5b-b014-3ce000c288fa', formalized).
narrative_ontology:cs_authority_grounding('9d7cb119-d773-4a5b-b014-3ce000c288fa', extraction).
narrative_ontology:cs_interpretation_layer_present('9d7cb119-d773-4a5b-b014-3ce000c288fa').
narrative_ontology:cs_reading_relation('9d7cb119-d773-4a5b-b014-3ce000c288fa', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d7cb119-d773-4a5b-b014-3ce000c288fa', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('9d7cb119-d773-4a5b-b014-3ce000c288fa', foundational, incentives_drive_labor_supply).
narrative_ontology:cs_axiom_status(incentives_drive_labor_supply, holdable).
narrative_ontology:cs_axiom_grounding('9d7cb119-d773-4a5b-b014-3ce000c288fa', incentives_drive_labor_supply, empirically_contingent).
narrative_ontology:cs_axiom('9d7cb119-d773-4a5b-b014-3ce000c288fa', foundational, targeted_aid_is_more_efficient).
narrative_ontology:cs_axiom_status(targeted_aid_is_more_efficient, holdable).
narrative_ontology:cs_axiom_grounding('9d7cb119-d773-4a5b-b014-3ce000c288fa', targeted_aid_is_more_efficient, empirically_contingent).
narrative_ontology:cs_reference_frame('9d7cb119-d773-4a5b-b014-3ce000c288fa', traditional_welfare_incentive_framework).
narrative_ontology:cs_drift_state('9d7cb119-d773-4a5b-b014-3ce000c288fa', contemporary_ubi_advocacy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9d7cb119-d773-4a5b-b014-3ce000c288fa', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocates).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals lose access to targeted welfare programs (e.g., housing assistance, food stamps) that provided greater net benefit than the unconditional income amount. They are effectively made worse off by the 'universal' program, trapped by the replacement of more valuable aid.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor, payer,
    powerless, immediate, trapped, national).

% Bear the net fiscal cost of the program, estimated at $1.4 trillion after offsets. They perceive the program as an inefficient redistribution of their earnings, funding idleness and non-needy recipients, leading to reduced economic output.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Receive unconditional income transfers despite not having a demonstrated need, effectively a net subsidy. They benefit from the universality of the program, often without a corresponding reduction in other benefits or a significant increase in their tax burden relative to the transfer received.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients, beneficiary,
    powerful, biographical, mobile, national).

% Gain political capital and validation for their policy agenda. The implementation of unconditional income, even if flawed, represents a step towards their long-term goals, reinforcing their ideological position and influence in policy debates.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocates, beneficiary,
    organized, generational, analytical, global).

% Administers the unconditional income program, managing its distribution and the phasing out of targeted aid. While some parts of the bureaucracy may shrink, new administrative functions emerge, maintaining institutional power and control over social policy. They enforce the replacement of existing programs.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, welfare_state_bureaucracy, agenda_setter,
    institutional, generational, constrained, national).

% Conduct meta-analyses and studies (e.g., AEI) on the economic impacts of unconditional income, highlighting negative effects like reduced employment (-3.2% in large pilots) and fiscal unsustainability. Their analysis provides the empirical basis for the dependency trap reading.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, economic_analysts_aei, observer,
    analytical, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simplifies welfare administration by replacing complex, means-tested programs with a single, universal transfer, theoretically reducing bureaucratic overhead and stigma.
% TRANSFER_FUNCTION: Moves a fixed income amount from the general tax base to all citizens, while simultaneously shifting resources away from targeted aid programs for the poor.
% ABSENT_VOICES: Advocates for targeted, means-tested welfare programs, who would argue for the efficiency and necessity of aid tailored to specific needs, are marginalized by the universalist discourse. The voices of those who lose more in targeted aid than they gain in UBI are often drowned out by the promise of 'universality'.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, the fiscal burden on taxpayers would decrease, and there would be immediate pressure to reinstate or expand targeted welfare programs to address specific needs of the poor. The labor market might see a slight increase in participation, and the political landscape around welfare would shift dramatically.
% FOUNDING_PROBLEM: The perceived problems of existing welfare systems: administrative complexity, high overhead, and the 'welfare trap' where earning more leads to losing benefits, disincentivizing work.
% FOUNDING_PROBLEM_CORROBORATION: The welfare state bureaucracy and UBI advocates attest that the founding problems of complexity and disincentives are still live. However, economic analysts (e.g., AEI) and working poor advocates argue that the unconditional income solution exacerbates new problems (dependency, upward redistribution) while failing to solve the core issues for the truly needy, making the 'solution' worse than the original problem.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_income_support__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the net negative impact on the working poor (losing more valuable targeted aid) and the significant fiscal burden on taxpayers. Suppression (0.70) is present through the active replacement of existing welfare structures, limiting alternatives for the poor. The theater ratio (0.20) is relatively low, as the program's stated goals of simplicity and dignity are genuinely pursued by some, but the underlying economic effects are seen as perverse by this reading. The increasing extractiveness over time reflects the growing evidence of negative employment impacts and fiscal costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the working poor, the constraint is a snare, actively making them worse off. From the perspective of middle/upper-class recipients, it's a net benefit. UBI advocates see it as a step towards a more just society, despite the negative economic data highlighted by observers like AEI. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The working poor and taxpayers are clear targets (high d), bearing the costs of reduced aid and increased taxes, respectively. Middle/upper-class recipients are beneficiaries (low d), receiving transfers without need. UBI advocates also benefit (low d) from the political validation of their agenda. The welfare state bureaucracy acts as an agenda-setter, enforcing the program's structure and managing the transition from targeted aid.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_employment_impact,
    'What is the long-term, causal impact of unconditional income support on labor force participation and economic productivity, disentangled from confounding factors?',
    'Large-scale, multi-decade randomized controlled trials across diverse economic contexts, with robust controls for macroeconomic shifts and demographic changes.',
    'If employment impacts are negligible or positive, the ''idleness'' claim weakens, potentially reclassifying the constraint away from a snare. If negative impacts are confirmed, the snare classification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(true_employment_impact, empirical, 'Uncertainty regarding the precise and causal long-term effects of UBI on employment and productivity.').

omega_variable(
    redistribution_direction_ambiguity,
    'Does unconditional income support, when accounting for all fiscal effects (taxes, lost benefits, transfers), result in net upward or downward redistribution of wealth?',
    'Comprehensive, independent fiscal modeling that integrates all direct and indirect economic effects across income quintiles, with transparent assumptions.',
    'If net redistribution is found to be downward (pro-poor), the ''redistributes upward to non-needy'' claim is challenged, weakening the snare classification. If upward redistribution is confirmed, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redistribution_direction_ambiguity, empirical, 'Ambiguity regarding the true direction of wealth redistribution under unconditional income support.').

omega_variable(
    coordination_extraction_boundary_welfare,
    'Is the simplification of welfare administration (coordination) inherently tied to the replacement of targeted aid (extraction), or can administrative efficiency be achieved without harming the working poor?',
    'Policy experiments that implement administrative simplification for targeted aid programs without universal replacement, comparing outcomes for efficiency and recipient well-being.',
    'If efficiency can be achieved without replacement, the ''crowds out targeted aid'' claim is strengthened as a distinct extractive mechanism, reinforcing the snare. If simplification necessitates replacement, the coordination and extraction functions are more tightly coupled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_welfare, conceptual, 'Whether administrative efficiency in welfare requires the replacement of targeted aid, or if these functions are separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__dependency_trap_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__dependency_trap_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__dependency_trap_reading, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__dependency_trap_reading, base_extractiveness, 10, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__dependency_trap_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__dependency_trap_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
