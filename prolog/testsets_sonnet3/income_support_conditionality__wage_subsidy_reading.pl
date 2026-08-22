% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   domain: political_economy/labor
 *
 * SUMMARY:
 *   This story instantiates the wage-subsidy reading of the
 *   income-support-conditionality kernel: unconditional transfers are read as
 *   functionally subsidizing low-wage employers, who capture part of the
 *   transfer's value through downward wage adjustment while the program's
 *   official metrics (subsistence-threshold clearance) mask the capture. This
 *   is a distinct constraint from the freedom-floor reading (which treats the
 *   same transfer as decommodifying labor power and enabling exit from
 *   coercive work) and the dependency-trap reading (which treats it as
 *   undermining work incentive). All three readings share the same underlying
 *   transfer mechanism but authored ε, beneficiary/victim sets, and
 *   classification differ structurally — this is not one constraint measured
 *   three ways, but three constructs the same policy instrument instantiates
 *   depending on which causal story about employer and worker behavior is
 *   accepted.
 *
 * KEY AGENTS:
 *   - low_wage_employers: primary beneficiary (organized/mobile) — captures transfer value via wage suppression
 *   - labor_intensive_industry_associations: agenda-setting beneficiary (institutional/arbitrage) — shapes program design to preserve capture
 *   - low_wage_workers: primary victim (powerless/constrained) — subsistence maintained but wage stagnation absorbed
 *   - general_taxpayers: secondary payer (moderate/trapped) — funds the effective subsidy
 *   - policy_researchers: analytical observer — adjudicates the empirical wage pass-through question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.66).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.42).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/labor").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, '9435f36a-33ed-49b8-97df-b97c7ba49645').
narrative_ontology:cs_kernel_codification('9435f36a-33ed-49b8-97df-b97c7ba49645', distributed).
narrative_ontology:cs_authority_grounding('9435f36a-33ed-49b8-97df-b97c7ba49645', distributed).
narrative_ontology:cs_reading_relation('9435f36a-33ed-49b8-97df-b97c7ba49645', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('9435f36a-33ed-49b8-97df-b97c7ba49645', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('9435f36a-33ed-49b8-97df-b97c7ba49645', foundational, unconditional_transfers_are_captured_by_wage_setters).
narrative_ontology:cs_axiom_status(unconditional_transfers_are_captured_by_wage_setters, holdable).
narrative_ontology:cs_axiom_grounding('9435f36a-33ed-49b8-97df-b97c7ba49645', unconditional_transfers_are_captured_by_wage_setters, empirically_contingent).
narrative_ontology:cs_axiom('9435f36a-33ed-49b8-97df-b97c7ba49645', secondary, poverty_metrics_conflate_income_floor_with_wage_adequacy).
narrative_ontology:cs_axiom_status(poverty_metrics_conflate_income_floor_with_wage_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('9435f36a-33ed-49b8-97df-b97c7ba49645', poverty_metrics_conflate_income_floor_with_wage_adequacy, empirically_contingent).
narrative_ontology:cs_reference_frame('9435f36a-33ed-49b8-97df-b97c7ba49645', labor_market_wage_determination_by_supply_and_demand).
narrative_ontology:cs_drift_state('9435f36a-33ed-49b8-97df-b97c7ba49645', post_program_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9435f36a-33ed-49b8-97df-b97c7ba49645', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, labor_intensive_industry_associations).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, general_taxpayers).
narrative_ontology:constraint_vindicates(income_support_conditionality__wage_subsidy_reading, poverty_alleviation_via_transfer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates in sectors (retail, hospitality, agriculture, gig platforms) where labor costs are the dominant expense. With a guaranteed income floor in place, can post wages below subsistence level because workers no longer face starvation-level urgency; the transfer program absorbs the gap the employer would otherwise have to pay. Lobbies to keep the transfer unconditional and universal (rather than tied to job search or wage floors) because conditionality on the worker side would restore some bargaining pressure back onto wages.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    organized, biographical, mobile, national).

% Advocates for the income-support program's design parameters in legislative and regulatory processes, framing it as poverty policy while shaping benefit levels and phase-out rates to minimize any pressure toward statutory minimum wage increases. Can relocate advocacy resources across jurisdictions; captures the policy conversation by presenting itself as an ally of low-wage workers.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_intensive_industry_associations, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__wage_subsidy_reading, labor_intensive_industry_associations, beneficiary).

% Receives the unconditional transfer, which prevents destitution, but observes real wages stagnate or fall in the sectors where they work as employers adjust pay downward to the point where transfer-plus-wage still clears subsistence. Cannot easily leave low-wage sectors for better-paid work without retraining, relocation, or credentials they lack; the transfer removes the most acute crisis but does not translate into improved market wages or bargaining leverage, because the employer captures the difference through the wage-setting process.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, biographical, constrained, national).

% Funds the transfer program through general taxation. Bears the fiscal cost of a program that, on this reading, is substituting for wages employers would otherwise have to pay directly — effectively subsidizing private payrolls through the public purse. Has no direct say in whether the program design closes this leakage; exit from the tax base is not realistically available.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, general_taxpayers, payer,
    moderate, generational, trapped, national).

% Designs and operates the disbursement mechanism, sets eligibility and phase-out parameters, and reports program outcomes (poverty-rate reduction, employment effects) to legislators. Measures success primarily by whether recipients clear a subsistence income threshold, which is consistent with the wage-subsidy dynamic going undetected in official metrics — a worker whose wage fell but whose total income (wage + transfer) held steady registers as a policy success.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, program_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Would object that unconditional transfers, absent wage floors or bargaining-power measures, let employers offload subsistence costs onto the public program while wages stagnate — undermining the union's core lever (the threat of labor scarcity). Rarely seated in the design process for the transfer program itself, which is typically negotiated between social-policy ministries and industry, not labor-relations bodies.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_unions, excluded,
    organized, biographical, constrained, national).

% Studies wage pass-through effects following transfer program rollouts — whether employer-side wage-setting responds to the existence of an income floor. Produces the empirical basis for adjudicating this reading versus the freedom-floor and dependency-trap readings, but its findings are contested and slow to shift program design.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, policy_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of preventing destitution among people without stable employment or adequate wages, providing a floor income regardless of work status.
% TRANSFER_FUNCTION: Moves general tax revenue to individuals as an income floor; on this reading, a portion of that transfer is effectively recaptured by low-wage employers through downward wage adjustment, so the net transfer runs from taxpayers through workers to employers.
% ABSENT_VOICES: Labor unions and worker bargaining coalitions are largely absent from the technical design tables where benefit levels, phase-out rates, and eligibility rules are set; those design choices determine how much of the transfer's value employers can capture through wage adjustment, but the design conversation is dominated by social-policy administrators and industry associations.
% DISAPPEARANCE_RATIONALE: If the unconditional transfer vanished overnight, low-wage employers would face immediate pressure to raise wages to retain subsistence-level workers or face turnover and labor shortages — the world would rearrange for them. Workers already near destitution would face acute crisis in the short run. Program administrators and industry associations dispute whether removal would raise wages (their preferred framing: wages are set by market conditions unrelated to the transfer) or whether the transfer removal would simply return the workforce to pre-program wage-and-poverty conditions without employer wage response at all.
% FOUNDING_PROBLEM: Workers in precarious or low-wage sectors faced destitution during unemployment, underemployment, or between jobs, with no adequate safety net tied to formal employment status.
% FOUNDING_PROBLEM_CORROBORATION: Program administrators and industry associations attest the founding problem (poverty prevention) remains live and the program is succeeding by that measure. Independent wage pass-through research and labor economists studying comparable transfer rollouts (outside both the administering agencies and the employer beneficiary set) attest to measurable downward wage drift in low-wage sectors following program introduction, consistent with partial employer capture — corroboration exists but is contested by the administering and beneficiary parties themselves.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, contested).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.66, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness starts moderate (0.35) reflecting genuine early-stage poverty-alleviation function and rises to 0.66 as wage pass-through accumulates — the longer the transfer exists as an unconditional floor without accompanying wage-floor policy, the more employer wage-setting adjusts to it. Suppression is moderate (0.42): there is no direct coercion forcing workers to accept lower wages, but structural dependence on the transfer combined with limited job mobility in low-wage sectors creates de facto pressure. Theater ratio rises modestly (0.12 to 0.31) as program administrators increasingly report subsistence-threshold outcomes as success metrics that do not capture the underlying wage stagnation — a mild Goodhart drift where the proxy (income floor cleared) substitutes for the real function (wage adequacy).
 *
 * PERSPECTIVAL GAP:
 *   From the employer seat, this looks like ordinary labor market equilibrium — wages reflect supply and demand, and the transfer is simply background welfare policy unrelated to hiring decisions. From the worker seat, the same arrangement is experienced as a program that keeps them alive but never lets their wages rise, effectively locking in low-wage employment as viable for employers who would otherwise have to raise pay to attract workers. From the taxpayer seat, the arrangement looks like paying twice: once for the transfer, once (invisibly) for the corporate wage bill it partially covers. The engine should compute these divergent seat classifications from the structural power/exit/scope data, not from any single narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage employers and their industry associations are declared beneficiaries because they can pay wages below subsistence and rely on the public transfer to make up the difference — this is textbook subsidy capture, and their organized/institutional power combined with mobile/arbitrage exit options puts their derived directionality near the full-beneficiary end. Low-wage workers are declared victims: their income floor is real, but the wage-suppression dynamic means the transfer's value is partially recaptured by employers rather than fully redounding to the worker, and their powerless/constrained position means little bargaining leverage to resist the wage adjustment. General taxpayers pay for a program whose stated purpose (poverty relief) is partially diverted to private payroll subsidy — a moderate-power, trapped-exit payer who cannot audit or resist the diversion easily.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (preventing destitution) is genuine and should not be discarded — that is why this is tangled_rope, not pure snare. The mandate is not obsolete (poverty prevention remains a live problem), but its current unconditional design allows a second, unaddressed function (wage subsidization) to ride along undetected. Resolving the mandatrophy risk requires either indexing transfer levels/phase-outs to wage floors, or pairing the transfer with binding minimum-wage enforcement, so poverty relief and wage-suppression capture can be structurally decoupled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_passthrough_magnitude,
    'How much of the transfer''s value is actually captured by employers through downward wage adjustment, versus retained by workers as net income gain?',
    'Natural experiments comparing wage trajectories in sectors/regions with and without unconditional transfer rollout, controlling for labor market tightness and minimum wage law changes.',
    'High pass-through (large employer capture) strongly supports tangled_rope classification with substantial victim-side extraction; near-zero pass-through would undermine this reading entirely and support the freedom-floor reading instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_passthrough_magnitude, empirical, 'Empirical magnitude of employer wage capture following transfer introduction.').

omega_variable(
    reading_selection_evidentiary_basis,
    'Which of the three kernel readings (wage-subsidy, freedom-floor, dependency-trap) best fits the actual behavioral response to unconditional transfers, and could more than one be simultaneously true for different worker segments?',
    'Disaggregated longitudinal studies segmenting workers by sector, bargaining power, and skill mobility — the wage-subsidy dynamic may dominate in low-mobility sectors while freedom-floor effects dominate in high-mobility sectors.',
    'If effects are segment-dependent, no single kernel reading is universally correct — the appropriate policy response (wage floor pairing vs. no intervention vs. work requirements) differs by segment, and treating one reading as canonical would misdiagnose the problem for the other segments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, conceptual, 'Whether the three sibling readings are mutually exclusive claims or coexisting segment-specific dynamics.').

omega_variable(
    counterfactual_wage_baseline,
    'What would low-wage employer pay have been absent the transfer program, given confounding trends like automation, sectoral shifts, and minimum wage law changes occurring over the same period?',
    'Structural econometric modeling isolating the transfer''s specific contribution to wage trajectories from concurrent labor market trends.',
    'Without a clean counterfactual, the measured extraction figure risks attributing to the transfer program wage stagnation that has other causes — this bears directly on whether the rising extractiveness measurements reflect genuine capture or coincidental trend correlation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_wage_baseline, empirical, 'Difficulty isolating transfer-program wage effects from confounding labor market trends.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(inco_tr_t4, income_support_conditionality__wage_subsidy_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__wage_subsidy_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__wage_subsidy_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__wage_subsidy_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__wage_subsidy_reading, theater_ratio, 24, 0.31).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inco_be_t4, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 24, 0.66).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_conditionality__wage_subsidy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, minimum_wage_enforcement_regime).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'unconditional income support' concept per the ε-invariance principle. The wage-subsidy reading (this file) claims tangled_rope with substantial employer-side extraction; the freedom-floor reading claims rope or scaffold with worker empowerment as the primary function; the dependency-trap reading claims a different extraction structure centered on incentive erosion rather than employer capture. All three share the same transfer mechanism as their subject but author independent ε, beneficiary/victim sets, and classifications because they encode incompatible causal claims about employer and worker behavioral response — this is the BGS-style decomposition pattern, not observer-relative measurement of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
