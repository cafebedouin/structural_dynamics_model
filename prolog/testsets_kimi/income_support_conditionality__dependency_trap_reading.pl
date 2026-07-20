% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Unconditional Income Support Dependency Trap
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the dependency_trap_reading of the
 *   income_support_conditionality kernel. Unconditional income support is
 *   read not as a safety net but as a structural trap: recipients lose labor
 *   market attachment and human capital over time, while taxpayers bear the
 *   fiscal burden without reciprocal obligation. The arrangement is enforced
 *   through state taxation and disbursement, with exit blocked by skill
 *   atrophy on the recipient side and legal compulsion on the taxpayer side.
 *   Welfare administrators act as agenda-setters and institutional
 *   beneficiaries, sustaining the program through poverty-reduction rhetoric
 *   that obscures the extraction it performs.
 *
 * KEY AGENTS:
 *   - ubi_recipients: Primary targets (powerless/trapped) â bear dependency and skill atrophy costs.
 *   - taxpayers: Secondary targets (moderate/constrained) â fund transfers through coerced taxation.
 *   - welfare_administrators: Agenda-setters and beneficiaries (institutional/mobile) â administer and justify program expansion.
 *   - labor_economists: Analytical observers (analytical/analytical) â evaluate employment effects from outside.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.78).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.72).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, 'c6be91a6-f2b1-4e38-90ae-934a4ba899ae').
narrative_ontology:cs_kernel_codification('c6be91a6-f2b1-4e38-90ae-934a4ba899ae', distributed).
narrative_ontology:cs_authority_grounding('c6be91a6-f2b1-4e38-90ae-934a4ba899ae', expertise).
narrative_ontology:cs_interpretation_layer_present('c6be91a6-f2b1-4e38-90ae-934a4ba899ae').
narrative_ontology:cs_reading_relation('c6be91a6-f2b1-4e38-90ae-934a4ba899ae', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6be91a6-f2b1-4e38-90ae-934a4ba899ae', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('c6be91a6-f2b1-4e38-90ae-934a4ba899ae', foundational, unconditional_support_causes_dependency).
narrative_ontology:cs_axiom_status(unconditional_support_causes_dependency, holdable).
narrative_ontology:cs_axiom_grounding('c6be91a6-f2b1-4e38-90ae-934a4ba899ae', unconditional_support_causes_dependency, empirically_contingent).
narrative_ontology:cs_axiom('c6be91a6-f2b1-4e38-90ae-934a4ba899ae', foundational, labor_attachment_as_welfare_criterion).
narrative_ontology:cs_axiom_status(labor_attachment_as_welfare_criterion, holdable).
narrative_ontology:cs_axiom_grounding('c6be91a6-f2b1-4e38-90ae-934a4ba899ae', labor_attachment_as_welfare_criterion, instrumental).
narrative_ontology:cs_reference_frame('c6be91a6-f2b1-4e38-90ae-934a4ba899ae', market_participation_as_baseline).
narrative_ontology:cs_drift_state('c6be91a6-f2b1-4e38-90ae-934a4ba899ae', post_ubi_pilot_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c6be91a6-f2b1-4e38-90ae-934a4ba899ae', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, welfare_administrators).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income transfers sufficient for subsistence. Over time, labor market skills depreciate and re-entry becomes increasingly difficult. The cash transfer is offset by eroded human capital and a reduced lifetime earnings trajectory, constituting an extraction masquerading as aid.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, biographical, trapped, national).

% Compelled by law to fund unconditional transfer programs through taxation. Experience the arrangement as a persistent fiscal extraction with no individual opt-out, while observing recipients outside the labor market.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Design and administer unconditional transfer programs. Justify expansion based on poverty-reduction metrics while capturing institutional budget, staffing, and authority. Career trajectories and departmental prestige are tied to program scale.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, welfare_administrators, agenda_setter,
    institutional, generational, mobile, national).

% Study employment effects of transfer programs. Produce evidence on work disincentives and human capital depreciation. Neither collect transfers nor fund them; influence operates through research findings and policy advising.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unconditional income support is presented as a solution to poverty and to the bureaucratic complexity of means-tested welfare systems.
% TRANSFER_FUNCTION: Moves income from taxpayers to non-working recipients, extracting human capital and labor-market attachment from recipients in the process.
% ABSENT_VOICES: Employers facing labor shortages; recipients future selves who bear the skill atrophy; advocates of reciprocal obligation and work-conditioned support.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared, recipients currently outside the labor market would face immediate survival pressure, likely reorganizing into employment, informal work, or family support networks; taxpayers would retain withheld income; the administrative apparatus would contract and civil society alternatives would partially revive.
% FOUNDING_PROBLEM: Industrial poverty and the inadequacy of patchwork, means-tested social assistance.
% FOUNDING_PROBLEM_CORROBORATION: Historians of social policy and labor economists outside the administrative beneficiary set attest that absolute poverty was the original problem; however, these same external observers dispute whether that problem remains acute enough to justify the current unconditional form, citing post-industrial growth and the emergence of dependency itself as a new pathology.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint moves substantial resources from taxpayers to recipients while imposing severe human capital depreciation on recipients, constituting a double extraction. Suppression (0.72) reflects the states monopoly on coercive taxation and the collapse of alternative support structures once unconditional provision becomes dominant. Theater ratio (0.45) captures the performative compassion narrative that sustains political support despite mounting evidence of labor-market detachment. Accessibility collapse (0.65) registers the atrophy of private charity and family support networks where state provision is universal, and the impossibility of individual tax opt-out. Resistance (0.58) reflects persistent political opposition and taxpayer resentment, moderated by the diffuse nature of the fiscal burden.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as a legitimate welfare institution delivering social insurance; the recipient seat experiences it as a comfortable trap with invisible walls of skill decay; the taxpayer seat experiences it as a persistent fiscal drain. These divergences are structurally encoded through the beneficiary and victim declarations and distinct exit options (mobile for administrators, trapped and constrained for victims).
 *
 * DIRECTIONALITY LOGIC:
 *   Recipients are declared victims despite receiving cash, because the constraint extracts human capital and future earnings; their directionality sits near the full-target end. Taxpayers are victims of coerced extraction, also high directionality. Welfare administrators are the named beneficiary, with low directionality reflecting their institutional subsidy and agenda control. The engine will compute seat divergence: the administrator seat sees coordination (poverty relief), while recipient and taxpayer seats see extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as snare rather than rope or scaffold prevents the error of accepting the poverty-relief coordination story at face value. A scaffold would require a sunset clause and transitional intent; none is present. A rope would require net beneficiaries among the governed and minimal coercion; here the governed (recipients and taxpayers) are both victim classes, and coercion is high. The mandate has not outlived its function because the claimed function (poverty relief) is still asserted, but the classification captures that the extraction is the operative structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which causal mechanism of unconditional income support is structurally dominant: dependency trap, freedom floor, or employer subsidy?',
    'Comparative natural experiments across jurisdictions with varying benefit levels and labor market institutions; longitudinal tracking of recipient work hours, wages, and skill acquisition.',
    'If the dependency trap mechanism dominates, classification as snare is reinforced; if freedom floor dominates, the constraint recasts as rope or scaffold; if employer subsidy dominates, it recasts as tangled_rope with employers as hidden beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Contested kernel reading ambiguity for income support conditionality.').

omega_variable(
    dependency_structural_vs_behavioral,
    'Is the observed dependency a structural trap (high effective marginal tax rates, skill depreciation) or an internalized behavioral norm (learned helplessness, identity fusion with beneficiary status)?',
    'Longitudinal studies of recipient trajectories post-benefit cessation; comparison with conditional transfer outcomes controlling for labor market conditions.',
    'If structural, exit options are constrained and directionality toward target is high; if internalized, effective suppression exceeds structural measures and the trap operates as cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_structural_vs_behavioral, empirical, 'Whether welfare dependency is structurally enforced or cognitively internalized.').

omega_variable(
    beneficiary_concentration_ambiguity,
    'Does the unconditional income support constraint concentrate benefits in a specific agent (administrative class, political coalition), or is the extraction purely diffuse?',
    'Analysis of budget allocation, administrative cost ratios, and political economy of program expansion.',
    'If concentrated beneficiaries exist, the snare classification is sharpened; if extraction is diffuse with no capturer, the constraint drifts toward piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_concentration_ambiguity, conceptual, 'Whether there is a concentrated beneficiary or diffuse extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__dependency_trap_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__dependency_trap_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__dependency_trap_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__dependency_trap_reading, base_extractiveness, 40, 0.78).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_conditionality__dependency_trap_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the income_support_conditionality kernel. Each reading assigns different beneficiary and victim structures to the same policy instrument, producing distinct epsilon values and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
