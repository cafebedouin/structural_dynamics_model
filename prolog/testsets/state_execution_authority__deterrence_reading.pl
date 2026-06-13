% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__deterrence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the DETERRENCE READING of the contested
 *   kernel 'state execution authority.' The reading grounds the practice in
 *   consequentialist prevention: execution reduces future homicides by
 *   increasing the cost of capital crime to potential offenders above their
 *   perceived benefit, deterring at the margin those who would otherwise
 *   commit murder. The constraint creates a tangled-rope structure: genuine
 *   coordination problem (deterring rational actors from murder), asymmetric
 *   extraction (offenders pay death; beneficiaries are future-prevented
 *   victims), active enforcement (appellate review, exclusion of rival
 *   interpretations). The empirical foundation is hotly
 *   contested—meta-analyses show weak or null deterrent effect—making the
 *   constraint's classification depend on assumptions about deterrence
 *   efficacy that the constraint's own operation cannot vindicate. This
 *   reading stands alongside two sibling readings (retributive_reading,
 *   abolition_reading) that contest the same kernel from different normative
 *   premises.
 *
 * KEY AGENTS:
 *   - state_execution_authority: institutional agenda-setter, administers capital punishment, claims deterrence justification (d ≈ 0.15 beneficiary pole — benefits indirectly through legitimacy and crime control)
 *   - executed_offenders: powerless victims, pay ultimate cost, trapped exit, immediate time-horizon (d ≈ 0.95 full target)
 *   - potential_future_homicide_victims: powerless beneficiaries, prevented by deterrent effect, trapped (counterfactual), biographical horizon (d ≈ 0.1 beneficiary pole)
 *   - wrongfully_convicted: powerless victims, pay death without deterrent justification, trapped, immediate (d ≈ 0.98 full target — worse than guilty offenders who at least arguably deserve something)
 *   - empirical_criminologists: institutional observers, generate contested evidence about deterrence efficacy (d ≈ 0.5 analytical)
 *   - appellate_judiciary: institutional dual role, administer constraint and review its legitimacy (d ≈ 0.35–0.45 moderate)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.62).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.71).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority (Deterrence Reading)").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, 'ada3509d-d4b6-417a-a85d-a1712b176db3').
narrative_ontology:cs_kernel_codification('ada3509d-d4b6-417a-a85d-a1712b176db3', formalized).
narrative_ontology:cs_authority_grounding('ada3509d-d4b6-417a-a85d-a1712b176db3', lineage).
narrative_ontology:cs_interpretation_layer_present('ada3509d-d4b6-417a-a85d-a1712b176db3').
narrative_ontology:cs_reading_relation('ada3509d-d4b6-417a-a85d-a1712b176db3', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('ada3509d-d4b6-417a-a85d-a1712b176db3', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('ada3509d-d4b6-417a-a85d-a1712b176db3', foundational, execution_consequentialist_prevention).
narrative_ontology:cs_axiom_status(execution_consequentialist_prevention, holdable).
narrative_ontology:cs_axiom_grounding('ada3509d-d4b6-417a-a85d-a1712b176db3', execution_consequentialist_prevention, empirically_contingent).
narrative_ontology:cs_axiom('ada3509d-d4b6-417a-a85d-a1712b176db3', secondary, rational_actor_crime_model).
narrative_ontology:cs_axiom_status(rational_actor_crime_model, holdable).
narrative_ontology:cs_axiom_grounding('ada3509d-d4b6-417a-a85d-a1712b176db3', rational_actor_crime_model, empirically_contingent).
narrative_ontology:cs_reference_frame('ada3509d-d4b6-417a-a85d-a1712b176db3', state_sovereign_penalty_authority).
narrative_ontology:cs_drift_state('ada3509d-d4b6-417a-a85d-a1712b176db3', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ada3509d-d4b6-417a-a85d-a1712b176db3', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, potential_future_homicide_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, general_public_safety).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, offenders_wrongfully_convicted).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.62 reflects the constraint's fundamentally extractive character masked by a coordination narrative: offenders die; the state claims prevented-victim benefit, but that benefit is conditional on unproven deterrence. If deterrence fails, extraction becomes indefensible and pure. Suppression at 0.71 is high because the constraint's persistence depends on actively excluding rival readings (abolition advocates) and constraining procedural review that might expose error. Theater_ratio at 0.28 is moderate: appellate review, constitutional claims, and deterrence rhetoric occupy substantial procedural space, but the machinery is primarily configured to overcome legal and moral objections rather than to serve deterrent function. Accessibility_collapse at 0.72 is high: once condemned, exit is nearly complete—no arbitrage, no mobility, only identity_locked (the offender remains bound by the state's sovereign claim). Resistance at 0.79 reflects active opposition: abolition movements, empirical criminologists contesting deterrence, international human-rights bodies all mount resistance. The measurement series shows extractiveness rising slightly to midpoint (extractive function clarifying as deterrence justification ages), theater plateauing (procedural expansion stabilizes), and suppression leveling off (enforcement machinery reaches steady state). The shared time grid ensures all three metrics are evaluated at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the state's institutional seat, the constraint is genuine coordination: it solves the problem of deterring rational offenders from capital murder. From the condemned offender's seat, it is pure extraction defended by appeal to a benefit (deterrence) they cannot verify and cannot contest before death. From the wrongfully convicted offender's seat, it is catastrophic extraction with zero corresponding benefit—their death contributes nothing to deterrence because they committed no crime. From the empirical criminologist's seat, the constraint's foundational mechanism is unproven at best and false at worst, making the extraction indefensible. The appellate judiciary sits between: they administer the constraint while reviewing its legitimacy, creating an internal double-bind (are they implementing deterrence or executing the powerless to satisfy retributive or political demands?). The engine computes these divergent classifications from the structural data: the institutional seat will classify the constraint as a functioning rope/coordination; the condemned seat will classify it as snare; the empiricist seat will classify it as incoherent (coordination claimed but mechanism absent).
 *
 * DIRECTIONALITY LOGIC:
 *   The state execution authority, as institutional agenda-setter, sits at d ≈ 0.15 (beneficiary pole): it controls the constraint, justifies it as deterrence, and claims legitimacy from crime reduction. Executed offenders sit at d ≈ 0.95 (full target): they pay the ultimate cost and have zero structural benefit. Potential future victims sit at d ≈ 0.1 (near-beneficiary): they benefit from deterrent effect but do not participate and cannot negotiate—their benefit is conditional on deterrence being real. The wrongfully convicted sit at d ≈ 0.98 (full target, worse than guilty offenders): they absorb the cost without any corresponding deterrent justification. This asymmetry is the constraint's core: enormous extraction from powerless offenders, claimed benefit for powerless future victims, legitimacy maintained by institutional agenda-setter. The directional structure is NOT symmetric—it is radically asymmetric extraction defended by appeal to consequentialist benefit that is empirically contested.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading faces a classical mandatrophy pattern: the founding problem (rational actors committing murder despite legal prohibition) was real at the constraint's origins, but the solution (raising expected cost via execution) is empirically unproven and increasingly contested. If empirical evidence accumulates showing zero or negative deterrent effect, the constraint's mandate has outlived its function—execution persists not because deterrence works but because: (1) retributive legitimacy has become the hidden justification, or (2) institutional inertia maintains the machinery despite failed function. The measurement series shows extractiveness plateauing at t=32–50, suggesting the constraint has reached a steady state of enforcement rather than continuing to sharpen its deterrent signal. Theater rising through t=24 then stabilizing suggests procedural expansion (appellate review, constitutional claims) is theatrical—it occupies substantive space without changing execution probability for the condemned. This is the diagnostic signature of mandatrophy: the founding function (deterrence) is no longer the primary engine of persistence; the constraint persists because the state's sovereign authority and institutional machinery have become self-justifying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_efficacy,
    'Does execution actually reduce future homicide rates at the margin? What is the true causal effect?',
    'Systematic meta-analysis of quasi-experimental designs (execution moratoria, state variation, instrumental variables); longitudinal homicide data controlling for confounds (policing intensity, incapacitation, sentence certainty, swiftness). The Donohue & Wolfers (2005) and National Research Council (2012) reviews exemplify the empirical frontier.',
    'If execution has zero or negative deterrent effect (risk of false deterrence), the constraint''s entire coordination justification collapses and it reclassifies as pure extraction (snare) rather than tangled rope. If substantial positive effect is established, the deterrent reading is vindicated and extraction becomes acceptable as cost of the coordination benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_empirical_efficacy, empirical, 'The empirical ground truth of deterrence: does execution prevent future homicides?').

omega_variable(
    error_rate_substitutability,
    'Can life without parole substitute for execution as a deterrent at equivalent strength? Is the constraint''s specific form (death penalty) necessary or merely sufficient?',
    'Comparative criminological analysis of deterrent effects of capital vs. non-capital sanctions; survey evidence on offender decision-making regarding execution vs. life sentence; natural experiments from jurisdictions shifting between sanction regimes.',
    'If life-without-parole deters equally, the deterrence reading''s justification for execution specifically dissolves — a lower-cost alternative achieves the same deterrent outcome, collapsing the extraction into pure surplus rather than necessary-cost coordination. The constraint would shift toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(error_rate_substitutability, empirical, 'Whether execution''s deterrent efficacy is unique or can be achieved by lesser sanction.').

omega_variable(
    wrongful_conviction_error_threshold,
    'At what error rate does the utilitarian cost of wrongful executions exceed the deterrent benefit in lives saved? What is the morally tolerable error rate?',
    'Estimation of true error rate in capital convictions (Gross et al. 2014 estimate ~4.1% for death sentences; refine with further empirical work); calculation of deterrent lives saved per execution; comparison of error cost to deterrent benefit; philosophical analysis of whether a threshold exists below which the practice becomes acceptable.',
    'If the true error rate and deterrent benefit produce a negative net-lives calculation (more innocent people wrongfully executed than guilty people deterred from murder), the constraint fails the deterrence reading''s own consequentialist ground. Reclassification to snare if error cost is knowingly accepted despite negative utilitarian calculus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_error_threshold, empirical, 'Error rate in capital convictions and its utilitarian cost relative to deterrent benefit.').

omega_variable(
    kernel_reading_contest,
    'Is the deterrence reading logically coherent with the retributive and abolition readings of the same kernel (state execution authority), or do they foreclose one another?',
    'Analysis of whether a single normative framework can hold multiple readings simultaneously. The retributive reading grounds execution in proportionate punishment (backward-looking); the deterrence reading grounds it in prevention of future crimes (forward-looking); the abolition reading rejects both on categorical deontological grounds. Can these coexist in a single legal system or do they foreclose?',
    'If the readings foreclose one another, the authority structure cannot rationally hold all three simultaneously and must choose one. If they coexist, the actual system may be incoherent by design (holding retributive justification in some cases, deterrence in others, satisfying neither fully). The reading_relations in cs_structure are derived from this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the deterrence reading''s core premise forecloses or coexists with rival readings of the execution-authority kernel.').

omega_variable(
    rational_actor_assumption_validity,
    'Do potential capital offenders respond to execution risk as rational expected-value maximizers, or is the assumption of rational-actor deterrence false?',
    'Criminological evidence on offender decision-making (survey evidence, prison interviews, case studies); analysis of whether murder is typically a calculated decision or an impulsive/emotional act; evidence on whether offenders actually assess and respond to penalty severity.',
    'If the rational-actor model is empirically false (most murders are non-deliberative or driven by passion/ideology rather than cost-benefit), the deterrence reading''s entire mechanism fails. The constraint would be maintained by enforcement inertia rather than by genuine deterrent function, shifting toward piton classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rational_actor_assumption_validity, empirical, 'Do potential offenders actually reason about execution risk as rational actors?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__deterrence_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__deterrence_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(stat_tr_t24, state_execution_authority__deterrence_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(stat_tr_t32, state_execution_authority__deterrence_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__deterrence_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__deterrence_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__deterrence_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(stat_be_t24, state_execution_authority__deterrence_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(stat_be_t32, state_execution_authority__deterrence_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__deterrence_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__deterrence_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__deterrence_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(stat_su_t24, state_execution_authority__deterrence_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(stat_su_t32, state_execution_authority__deterrence_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__deterrence_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__deterrence_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_execution_authority__deterrence_reading, 0.12).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, criminal_justice_legitimacy).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, offender_rational_actor_assumption).

% DUAL FORMULATION NOTE:
% This constraint is part of the state_execution_authority kernel family. The deterrence reading, retributive reading, and abolition reading are three distinct constraints sharing the same institutional kernel (state power to execute). Each reading produces a different ε, different beneficiary/victim structure, and different classification. They are linked by affects_constraints to enable contamination analysis: if the deterrence reading's empirical ground (that execution deters) is invalidated, the retributive reading's legitimacy is unaffected (it grounds execution in desert, not deterrence), while the abolition reading's claim strengthens (no consequentialist justification remains). Decomposition follows the ε-invariance principle: the observable 'deterrence efficacy' is specific to the deterrence reading and produces an ε distinct from the retributive reading's ε (which depends on desert, not deterrence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
