% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This is the dependency-trap reading of the contested
 *   income-support-conditionality kernel. The constraint instantiates
 *   unconditional income support as a snare: it presents itself as
 *   dignity-affirming and anti-bureaucratic, but operates as a mechanism that
 *   traps recipients in long-term non-participation and skill degradation.
 *   Recipients face high exit costs (identity lock-in, resume gaps, employer
 *   stigma); taxpayers fund transfers to non-productive population segments
 *   (in this reading's frame); program administrators maintain eligibility
 *   structures that enable dependency rather than transition. The core
 *   empirical claim is that unconditional support produces measurably higher
 *   dependency rates, lower skill acquisition, and lower labor-force re-entry
 *   than conditional or wage-integrated alternatives. The measurement series
 *   tracks extraction accumulation over 25 time units, with theater ratio
 *   rising as the program's initial anti-stigma function is displaced by
 *   observed idleness maintenance.
 *
 * KEY AGENTS:
 *   - ubi_recipients_entrapped: powerless, identity-locked, facing skill atrophy over generational horizon
 *   - taxpayers_subsidizing_idleness: organized, constrained, funding non-productive transfers
 *   - program_administrators: institutional, mobile, maintaining the system
 *   - political_coalitions_supporting_ubi: organized, beneficiary from ideological vindication
 *   - employers_benefiting_from_labor_surplus: organized, beneficiary from constrained worker exit
 *   - critical_policy_voices: excluded, moderate power, sidelined from implementation
 *   - comparative_jurisdictions: observer, institutional, generating empirical evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.68).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.72).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, 'cb0dbc56-eee7-46ec-8403-f7c17367777e').
narrative_ontology:cs_kernel_codification('cb0dbc56-eee7-46ec-8403-f7c17367777e', distributed).
narrative_ontology:cs_authority_grounding('cb0dbc56-eee7-46ec-8403-f7c17367777e', distributed).
narrative_ontology:cs_reading_relation('cb0dbc56-eee7-46ec-8403-f7c17367777e', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb0dbc56-eee7-46ec-8403-f7c17367777e', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('cb0dbc56-eee7-46ec-8403-f7c17367777e', foundational, unconditional_support_produces_measurable_dependency).
narrative_ontology:cs_axiom_status(unconditional_support_produces_measurable_dependency, holdable).
narrative_ontology:cs_axiom_grounding('cb0dbc56-eee7-46ec-8403-f7c17367777e', unconditional_support_produces_measurable_dependency, empirically_contingent).
narrative_ontology:cs_axiom('cb0dbc56-eee7-46ec-8403-f7c17367777e', foundational, dependency_harms_recipient_autonomy_and_skill).
narrative_ontology:cs_axiom_status(dependency_harms_recipient_autonomy_and_skill, holdable).
narrative_ontology:cs_axiom_grounding('cb0dbc56-eee7-46ec-8403-f7c17367777e', dependency_harms_recipient_autonomy_and_skill, deontological).
narrative_ontology:cs_reference_frame('cb0dbc56-eee7-46ec-8403-f7c17367777e', conditional_support_with_work_requirements).
narrative_ontology:cs_drift_state('cb0dbc56-eee7-46ec-8403-f7c17367777e', unconditional_support_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('cb0dbc56-eee7-46ec-8403-f7c17367777e', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients_entrapped).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers_subsidizing_idleness).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).

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
 *   Extractiveness rises from 0.52 to 0.68 over the interval as dependency deepens and skill atrophy accumulates — what begins as a modest income transfer becomes a binding constraint on recipients' future options. Suppression is consistently high (0.58–0.72) because the constraint's persistence depends on suppressing alternative framings (conditional support, wage integration, time-limited programs) and suppressing recipients' own agency toward re-employment and skill development. Theater ratio rises from 0.28 to 0.41 as the program's initial function (protecting dignity) becomes increasingly secondary to its actual operation (maintaining dependency). The measurement grid is aligned: every metric is authored at t=0,5,10,15,20,25 so temporal analysis has complete coverage. Accessibility collapse is moderate (0.58): alternatives exist (conditional support, wage subsidies, employer training) but are politically suppressed in favor of the unconditional model. Resistance is moderate (0.54): political and scholarly criticism exists but faces organized advocacy coalitions.
 *
 * PERSPECTIVAL GAP:
 *   From the program-administrator and political-coalition seats, the constraint is an anti-stigma coordination mechanism that provides dignity and freedom from bureaucratic intrusion. From the recipient and taxpayer seats, the same structure operates as a dependency trap: it provides consumption security but forecloses the pathway to autonomy and productive participation. The engine computes divergent types per seat from the structural data — the agenda-setter and beneficiary seats may compute toward rope or coordination, while the victim seats compute toward snare. This divergence is the measurement the constraint story exists to record.
 *
 * DIRECTIONALITY LOGIC:
 *   Recipients are trapped (identity_locked exit): the unconditional support provides just enough to survive without work, but not enough to accumulate capital or retrain; re-entry requires overcoming identity fusion (self-concept as benefit recipient), competing against employed peers, and enduring employer stigma for resume gaps. This drives d toward 1.0 (full target). Taxpayers are constrained victims: they fund the transfer but see no corresponding return in productivity or skill development; their exit requires political action to change the program, which is actively suppressed by organized coalitions. This drives d toward 0.8–0.9 (high target). Program administrators and supporting coalitions are beneficiaries (d toward 0.1–0.2): they maintain a system that vindicates their ideological commitments without bearing the direct cost of recipient dependency or taxpayer burden. Employers are indirect beneficiaries (d toward 0.15–0.25): they benefit from constrained labor supply and muted wage pressure, but are not formal stakeholders in the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The dependency-trap reading presents a classic mandatrophy scenario: the founding problem (destitution in the absence of work or support) was real when the program was designed, but has shifted as the program matured. The mandate was to prevent material deprivation; the operation has become maintenance of long-term non-participation. This reading declares that the founding problem has atrophied from 'live' into 'contested' — the deprivation it was designed to prevent is now manageable through conditional or wage-integrated support that would not produce the same dependency lock-in. The theater ratio's rise (0.28 to 0.41) documents this mandatrophy: increasing share of the program's activity is devoted to justifying its existence and suppressing alternative framings rather than performing its original anti-deprivation function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_causation_vs_selection,
    'Does unconditional income support CAUSE dependency and skill atrophy, or does it merely ATTRACT individuals already predisposed to non-participation?',
    'Quasi-experimental evidence from the introduction or removal of unconditional support programs, controlling for cohort selection effects. Longitudinal tracking of labor-force participation and skill acquisition before/after program exposure.',
    'If causal, the program induces dependency as a mechanism — supporting the snare classification. If selection dominates, the program may be targeting population segments already at risk, shifting responsibility to baseline economic conditions rather than the program design. Classification could shift from snare toward rope (necessary support for a trapped population) if selection effects dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_causation_vs_selection, empirical, 'The causal vs. selection boundary for program-induced dependency').

omega_variable(
    suppression_mechanism_structural_vs_ideological,
    'Is the suppression of alternative framings (conditional, wage-integrated, time-limited) structurally enforced (legal/institutional barriers) or ideologically maintained (advocacy coalitions'' discursive dominance)?',
    'Historical analysis of policy debate and legislative records. Comparative study of jurisdictions where conditional or wage-subsidized models are prominent vs. those dominated by unconditional approaches.',
    'If structural, the suppression is exogenous and the constraint''s persistence is enforced against genuine alternatives. If ideological, the suppression is endogenous and the constraint persists because of organized commitment rather than lack of viable alternatives. This affects whether suppression can be reduced through counter-advocacy or requires institutional/legal change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_ideological, conceptual, 'The locus of suppression: structural barriers vs. ideological alignment').

omega_variable(
    identity_lock_reversibility,
    'Is the identity lock-in produced by long-term unconditional support reversible through re-employment, or does it persist even after recipients exit into work?',
    'Post-exit longitudinal studies of former benefit recipients tracking self-concept, earnings trajectory, and labor-market stability 3–5 years after employment re-entry.',
    'If reversible, the exit cost is high but not permanent, supporting the snare classification (trapped, but escape is possible). If persistent, the identity fusion is more deeply rooted and the constraint''s effective extraction extends beyond the period of benefit receipt — the snare is more severe and the long-term damage to human capital is greater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity lock-in from long-term benefit receipt is reversible post-exit').

omega_variable(
    kernel_reading_contest,
    'Is the dependency-trap reading a genuine empirical claim about causal mechanisms, or is it a contested normative position among equally defensible readings of the same kernel?',
    'This is the core committer-axis question: the three sibling readings (dependency-trap, freedom-floor, wage-subsidy) rest on different empirical premises about unconditional support''s effects, but also on different normative frames (what counts as dependency vs. autonomy, what counts as harm vs. benefit). Resolving requires separating empirical falsifiability (does unconditional support increase measured dependency?) from normative framings (is that dependency a harm or a feature?).',
    'If the dependency-trap reading is empirically falsifiable and the freedom-floor reading makes opposite empirical claims, one of them is wrong and classification should converge on the true mechanism. If both are empirically true but normatively framed differently (unconditional support increases measured non-participation AND increases recipient autonomy from coercive work), then the readings coexist and the constraint''s type depends on which frame is privileged. This omega documents the reading''s contestation within the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The contestation between empirical and normative framings of the income-support kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__dependency_trap_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__dependency_trap_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(inco_tr_t25, income_support_conditionality__dependency_trap_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__dependency_trap_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__dependency_trap_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(inco_be_t25, income_support_conditionality__dependency_trap_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__dependency_trap_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__dependency_trap_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__dependency_trap_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(inco_su_t25, income_support_conditionality__dependency_trap_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested 'income_support_conditionality' kernel. The three readings (dependency-trap, freedom-floor, wage-subsidy) have different victim sets, different extractiveness profiles, and different structural framings of the same policy mechanism. The dependency-trap reading posits that unconditional income support produces dependency traps and skill atrophy; the freedom-floor reading posits that it decommodifies labor and creates positive freedom; the wage-subsidy reading posits that it functions as hidden employer subsidy. These are structurally distinct constraints with different ε values because they measure different outputs from the same policy input. The ε-invariance principle requires separate constraint stories. All three are linked via network.affects_constraints to document the reading contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__dependency_trap_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
