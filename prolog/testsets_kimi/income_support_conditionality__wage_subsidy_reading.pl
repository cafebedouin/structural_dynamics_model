% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   This constraint instantiates the wage_subsidy_reading of the contested
 *   income_support_conditionality kernel. Under this reading, unconditional
 *   income supportâpresented as a worker benefit or poverty-alleviation
 *   measureâstructurally functions as an employer subsidy. Low-wage
 *   employers capture the transfer through downward wage adjustment,
 *   institutionalizing a labor market in which public funds cover the gap
 *   between market wages and subsistence. The constraint is claimed as
 *   tangled_rope because it simultaneously coordinates a genuine
 *   social-stability function (preventing destitution) and extracts
 *   asymmetrically from workers by suppressing their bargaining power and
 *   wages.
 *
 * KEY AGENTS:
 *   - low_wage_employers: Primary beneficiary (powerful/constrained) â captures transfer via wage suppression
 *   - low_wage_workers: Primary target (powerless/trapped) â receives transfer but suffers wage offset and blocked exit
 *   - state_transfer_administrator: Agenda setter (institutional/constrained) â administers policy within fiscal and political limits
 *   - labor_advocates: Analytical observer (organized/analytical) â documents employer capture and advocates wage-floor coupling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.72).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.68).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, 'ffca58ae-db50-41d8-a6d8-dd95432de7a1').
narrative_ontology:cs_kernel_codification('ffca58ae-db50-41d8-a6d8-dd95432de7a1', formalized).
narrative_ontology:cs_authority_grounding('ffca58ae-db50-41d8-a6d8-dd95432de7a1', lineage).
narrative_ontology:cs_interpretation_layer_present('ffca58ae-db50-41d8-a6d8-dd95432de7a1').
narrative_ontology:cs_reading_relation('ffca58ae-db50-41d8-a6d8-dd95432de7a1', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffca58ae-db50-41d8-a6d8-dd95432de7a1', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('ffca58ae-db50-41d8-a6d8-dd95432de7a1', foundational, transfer_subsidizes_employer_payroll).
narrative_ontology:cs_axiom_status(transfer_subsidizes_employer_payroll, holdable).
narrative_ontology:cs_axiom_grounding('ffca58ae-db50-41d8-a6d8-dd95432de7a1', transfer_subsidizes_employer_payroll, empirically_contingent).
narrative_ontology:cs_reference_frame('ffca58ae-db50-41d8-a6d8-dd95432de7a1', welfare_state_subsistence_guarantee).
narrative_ontology:cs_drift_state('ffca58ae-db50-41d8-a6d8-dd95432de7a1', contemporary_neoliberal_austerity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ffca58ae-db50-41d8-a6d8-dd95432de7a1', '').
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

% Operate in sectors such as retail, logistics, and agriculture where profit margins depend on abundant low-cost labor. They adjust nominal wages downward in response to unconditional transfer programs, capturing the public subsidy as reduced payroll costs while maintaining workforce subsistence. Competitive pressure prevents unilateral exit from the subsidy-dependent wage structure.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    powerful, biographical, constrained, national).

% Receive nominally unconditional income transfers intended to supplement wages, but face suppressed hourly pay as employers internalize the transfer as a wage substitute. They cannot refuse low-wage work because the transfer alone is insufficient for subsistence and often carries implicit work requirements or benefit cliffs; their bargaining power is structurally undercut by the employer's knowledge that the transfer covers the gap.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, immediate, trapped, national).

% Designs benefit levels, eligibility rules, and labor-market integration measures for the income support scheme. They publicly frame the policy as poverty relief and social investment, while calibrating transfer generosity against fiscal constraints and official employment targets. Institutional mandates and political economy block redesign toward genuine decommodification or full living-wage replacement.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, state_transfer_administrator, agenda_setter,
    institutional, generational, constrained, national).

% Document the wedge between nominal transfer generosity and net worker income after employer wage adjustment. They argue that unconditional support without robust wage floors functions as an indirect employer subsidy, and advocate for policy bundles that combine transfers with collective bargaining or statutory minimum wages.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_advocates, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a minimum consumption floor that prevents acute destitution and social instability in economies where market-clearing wages for low-skill labor fall below subsistence levels, preserving a continuous supply of available labor for non-tradable sectors.
% TRANSFER_FUNCTION: Moves public tax revenue to low-wage workers as cash or in-kind transfers, which employers partially capture through downward wage adjustment, effectively converting a worker-benefit program into a public subsidy of private payroll costs.
% ABSENT_VOICES: Workers who would prefer living wages over transfer-mediated subsistence, and small employers competing on fair wages who are undercut by larger firms exploiting the subsidy, are largely absent from policy design forums.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared overnight, low-wage employers paying below-subsistence wages would face immediate labor shortages or forced wage increases; the low-wage labor market would reorganize toward full subsistence wages, automation, or sectoral contraction.
% FOUNDING_PROBLEM: Structural inability of low-productivity sectors to pay subsistence wages in advanced economies, combined with political resistance to direct wage regulation or full decommodification of labor power.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists and left-policy institutes outside the employer beneficiary set attest that the productivity-wage gap is real but contest that transfer-mediated wage suppression is the appropriate response; employer associations and neoliberal think tanks corroborate the need for labor-cost support.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.72) because a substantial share of the transfer is captured by employers through wage offsets rather than retained by workers. Suppression (0.68) reflects the structural suppression of wage-bargaining power and the policy-design features that block genuine labor-market exit. Theater ratio (0.45) captures the growing gap between the public poverty-relief narrative and the subsidy function. Accessibility collapse (0.60) indicates that alternatives such as union bargaining or living-wage statutes are partially but not fully foreclosed. Resistance (0.55) is moderate, reflecting ongoing advocacy and some policy contestation. The measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The employer seat experiences the constraint as a beneficial reduction in labor costs that enables continued operation of low-margin business models. The worker seat experiences the same arrangement as a trap: a nominally unconditional transfer that is offset by lower wages and accompanied by implicit work requirements, leaving them no better off in net terms and with weakened bargaining position. The agenda-setter seat experiences it as a fiscally constrained compromise between poverty relief and labor-market activation. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage employers are declared beneficiaries with constrained exit (they benefit from and are locked into the subsidy-dependent wage structure), pushing their directionality toward the beneficiary pole. Low-wage workers are declared victims/payers with trapped exit, pushing their directionality toward the full-target pole. The state administrator sits at intermediate directionality: they enforce the constraint but do not personally extract from it, operating as an institutional intermediary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâsubsistence gaps in low-wage employmentâis contested but not dead, so the constraint is not a pure piton. However, the mechanism has drifted: the transfer was ostensibly designed to relieve poverty, but its calibration and design have become entangled with employer demands for affordable labor. If the problem were genuinely solved through productivity gains or wage floors, the subsidy mechanism would be unnecessary. Its persistence despite (contested) obsolescence of the original poverty-justification signals mandatrophy risk, but the active coordination function and live political contestation keep it in the tangled_rope category rather than piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employer_capture_fraction,
    'What fraction of unconditional transfers is captured by employers through wage suppression versus retained by workers as net income gain?',
    'Econometric analysis of wage and employment trajectories in jurisdictions before and after the introduction of unconditional transfers, controlling for macroeconomic trends and sectoral composition.',
    'If employer capture exceeds fifty percent, the constraint is primarily extractive; if capture is minimal, the wage_subsidy_reading collapses and the freedom_floor_reading gains structural support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_capture_fraction, empirical, 'Empirical ambiguity about the degree of employer wage capture.').

omega_variable(
    kernel_framing_contest,
    'Is unconditional income support structurally a wage subsidy to employers, a freedom floor for workers, or a dependency trap?',
    'Comparative institutional analysis across jurisdictions with varying labor market regulations, union density, and transfer designs to observe which reading''s predicted outcomes dominate.',
    'Resolution determines which of the three kernel readings is the structurally dominant constraint in a given institutional context; the same policy may instantiate different constraints in different labor regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_contest, conceptual, 'Conceptual ambiguity about which reading of the income support kernel is valid.').

omega_variable(
    suppression_design_or_market,
    'Is the wage suppression effect a spontaneous labor-market equilibrium, or actively maintained by policy features such as benefit cliffs, implicit work requirements, and deliberate calibration below living wage?',
    'Policy archaeology comparing transfer designs that yield suppression versus those that enable genuine exit; natural experiments from benefit-level adjustments and work-requirement removals.',
    'If actively maintained, suppression is higher and the constraint edges toward snare; if a spontaneous equilibrium, it remains a tangled rope arising from mixed coordination and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_design_or_market, empirical, 'Ambiguity about whether suppression is institutionally designed or market-spontaneous.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__wage_subsidy_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__wage_subsidy_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__wage_subsidy_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(inco_tr_t32, income_support_conditionality__wage_subsidy_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__wage_subsidy_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(inco_be_t32, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(inco_su_t8, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(inco_su_t16, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(inco_su_t24, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(inco_su_t32, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(inco_su_t40, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
