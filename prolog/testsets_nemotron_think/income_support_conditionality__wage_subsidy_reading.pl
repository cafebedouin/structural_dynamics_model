% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   This constraint story instantiates the wage_subsidy_reading of the
 *   income_support_conditionality kernel. The kernel is the policy concept of
 *   unconditional income support (UBI, negative income tax, guaranteed
 *   minimum income). Three readings contest its structural operation: the
 *   freedom_floor_reading sees emancipation, the dependency_trap_reading sees
 *   work disincentive, and this reading — wage_subsidy_reading — sees
 *   employer capture. The constraint is the standing arrangement: a public
 *   income floor that, under this reading, functions as a subsidy to
 *   employers who can now pay wages below what workers would accept without
 *   the floor. The constraint type is tangled_rope because it has a genuine
 *   coordination function (preventing destitution, maintaining labor supply)
 *   AND asymmetric extraction (employers capture the subsidy via wage
 *   suppression, workers bear the cost through stagnant wages). Active
 *   enforcement is required: the state must administer the transfer, set
 *   clawback rates, and maintain the legal framework that makes the income
 *   floor unconditional while labor markets remain monopsonistic.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.68).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.55).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, 'c47e9dd9-6a7a-44dd-9946-8c11f46e7eac').
narrative_ontology:cs_kernel_codification('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', formalized).
narrative_ontology:cs_authority_grounding('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', lineage).
narrative_ontology:cs_interpretation_layer_present('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac').
narrative_ontology:cs_reading_relation('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', foundational, public_floor_enables_wage_suppression).
narrative_ontology:cs_axiom_status(public_floor_enables_wage_suppression, holdable).
narrative_ontology:cs_axiom_grounding('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', public_floor_enables_wage_suppression, empirically_contingent).
narrative_ontology:cs_axiom('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', foundational, labor_supply_inelastic_at_subsistence_margin).
narrative_ontology:cs_axiom_status(labor_supply_inelastic_at_subsistence_margin, holdable).
narrative_ontology:cs_axiom_grounding('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', labor_supply_inelastic_at_subsistence_margin, empirically_contingent).
narrative_ontology:cs_axiom('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', secondary, emancipatory_framing_legitimizes_capture).
narrative_ontology:cs_axiom_status(emancipatory_framing_legitimizes_capture, holdable).
narrative_ontology:cs_axiom_grounding('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', emancipatory_framing_legitimizes_capture, conventional).
narrative_ontology:cs_reference_frame('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', post_fordist_welfare_settlement).
narrative_ontology:cs_drift_state('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', platform_economy_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c47e9dd9-6a7a-44dd-9946-8c11f46e7eac', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, platform_intermediaries).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, policy_advocates_freedom_floor).
narrative_ontology:constraint_vindicates(income_support_conditionality__wage_subsidy_reading, wage_subsidy_thesis).
narrative_ontology:constraint_vindicates(income_support_conditionality__wage_subsidy_reading, reserve_wage_suppression_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set wage floors in low-wage sectors. When unconditional income support establishes a public subsistence floor, employers can offer wages below what workers would otherwise accept, capturing the difference between the market-clearing wage and the new reservation wage. They lobby for UBI designs that maintain labor supply elasticity while suppressing wage costs.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, employers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__wage_subsidy_reading, employers, agenda_setter).

% Digital labor platforms (gig economy, crowdwork) treat UBI as a de facto wage floor that lets them pay piece rates below subsistence. Workers accept platform terms because the public income floor covers basic needs; the platform captures the surplus. Platforms advocate for portable benefits and UBI pilots that externalize their labor reproduction costs.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, platform_intermediaries, beneficiary,
    powerful, biographical, mobile, global).

% Receive unconditional income support but face wage offers that adjust downward by approximately the transfer amount. Their reservation wage falls because the public floor covers survival; employers compete on the margin above the floor. Exit from low-wage work is constrained by skill specificity, geographic immobility, and the UBI itself being calibrated at subsistence — not empowerment — level.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    moderate, biographical, constrained, national).

% Advocate UBI as decommodification of labor — the freedom to refuse coercive work. They benefit intellectually and politically from the emancipatory framing. In the wage_subsidy reading, their advocacy inadvertently legitimizes a policy that employers capture; they are not the wage-suppressors but their framing provides cover for the subsidy effect.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, policy_advocates_freedom_floor, beneficiary,
    organized, generational, mobile, global).

% Warn that unconditional income undermines work incentives and creates long-term dependency. Their voice is excluded from the wage_subsidy reading's coalition because it predicts a different behavioral response (labor supply reduction) than what the wage_subsidy mechanism requires (labor supply maintenance at lower wages). They are structurally excluded by the reading's assumption that workers cannot refuse work.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, policy_advocates_dependency_trap, excluded,
    organized, generational, constrained, national).

% Design and administer the income support program. They choose conditionality levels, clawback rates, and integration with tax systems. Their design choices determine whether the transfer functions as a wage floor (high clawback = employer subsidy) or as genuine decompression (low clawback = exit option). They face political pressure from both employer groups and labor advocates.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, state_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Study incidence of income support on wage setting, labor supply, and poverty dynamics. They provide the empirical evidence on whether UBI is captured by employers (wage_subsidy reading), reduces labor supply (dependency_trap reading), or expands refusal capacity (freedom_floor reading). Their analyses are cited by all three readings selectively.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a public income floor that prevents destitution while maintaining labor supply for low-wage service, care, and gig sectors that cannot automate easily and would otherwise face wage pressure from subsistence needs.
% TRANSFER_FUNCTION: Moves public funds (tax revenue) to employers and platform intermediaries via suppressed wage offers. Workers receive the transfer but their net gain is offset by lower wage offers; the incidence falls on capital through reduced labor costs. Taxpayers fund the subsidy; employers capture it.
% ABSENT_VOICES: Workers who would genuinely exit coercive or degrading low-wage jobs if UBI provided a true freedom floor — precarious migrants, care workers, gig drivers in monopsonistic markets. They are absent because the wage_subsidy reading assumes labor supply is inelastic at the subsistence margin; their potential refusal is what the reading's structure suppresses.
% DISAPPEARANCE_RATIONALE: If the unconditional income support vanished overnight, low-wage employers would face immediate labor shortages or be forced to raise wages to subsistence levels. Sectors dependent on below-subsistence wage offers (gig platforms, certain care work, seasonal agriculture) would restructure — automate, consolidate, or shrink. The labor market would reorganize around a higher wage floor.
% FOUNDING_PROBLEM: Post-1980 labor markets failed to deliver subsistence wages for expanding low-wage service and care sectors. Traditional welfare created high marginal tax rates that trapped recipients. UBI was proposed as a universal floor that would simplify administration and avoid poverty traps — but the wage_subsidy reading argues it became a subsidy for the very employers who suppressed wages.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists (Guy Standing, Philippe Van Parijs, Karl Widerquist) and historical analyses of welfare reform (EITC incidence studies, negative income tax experiments) corroborate that public income floors can be captured by employers via wage adjustment. Employer groups' strategic silence on UBI when it subsidizes their labor costs — contrasted with opposition to minimum wage hikes — is corroborating evidence from outside the benefiting parties.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is substantial: the transfer from taxpayers to low-wage workers is largely intercepted by employers via wage adjustment. Studies of EITC and negative income tax experiments show 20-50% incidence on wages; in monopsonistic gig platforms, capture approaches 100%. Suppression (0.55) is moderate: workers cannot easily refuse work because UBI is calibrated at subsistence, not empowerment, and exit options are constrained by skill/geography. Theater ratio (0.35) reflects the emancipatory framing that legitimizes the policy while the wage_subsidy mechanism operates beneath. Accessibility collapse (0.50) is moderate: alternatives (unionization, minimum wage, sectoral bargaining) exist but are politically weakened. Resistance (0.45) is moderate: labor organizing pushes for higher floors and conditionality, but the universalist framing fractures coalition potential.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (low_wage_workers) and beneficiary seats (employers, platforms) compute differently: from the employer seat, the constraint is a coordination mechanism that stabilizes labor supply at lower cost; from the worker seat, it is a trap where the public floor becomes the ceiling. The freedom_floor advocate seat experiences it as emancipation; the dependency_trap seat (excluded) predicts collapse. The engine computes this divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Employers and platforms are structural beneficiaries (d near 0.1-0.2): they collect the subsidy via wage suppression, control job design, and have arbitrage-grade exit (capital mobility, automation). Low-wage workers are structural targets (d near 0.8-0.9): they bear the incidence through stagnant wages, have constrained exit (skills, geography, subsistence-level UBI), and face monopsonistic employers. Policy advocates for freedom_floor are incidental beneficiaries (d ~0.3): they gain political capital but their framing enables the capture. Dependency_trap advocates are excluded (d not applicable): their prediction of labor supply reduction contradicts the wage_subsidy mechanism's requirement of maintained supply. State administrators are agenda_setters (d ~0.4): they design the clawback rates that determine incidence but face cross-pressures. Labor economists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (subsistence wage failure in low-wage sectors) remains contested: some argue UBI solves it, others that it institutionalizes the failure. The mandate has not atrophied — the coordination function (destitution prevention) is live — but the extraction function (employer capture) has grown as gig platforms and monopsonistic labor markets expanded. The theater ratio rise reflects this: more performative 'freedom' framing as the subsidy effect becomes more visible. The constraint is not a piton — it is actively maintained and expanded (pilots, legislation) — but it is not a pure rope either, because the coordination is real and the extraction is structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_adjustment_mechanism,
    'Is the wage suppression effect driven by structural market power (monopsony, platform control) or by behavioral reservation wage shifts (workers accepting less because survival is guaranteed)?',
    'Natural experiments from UBI pilots with varying clawback rates and labor market tightness; structural estimation of labor supply elasticities at the intensive vs. extensive margin; platform-level data on wage offers before/after income floor introduction.',
    'If structural market power dominates, the subsidy incidence is a rent transfer requiring antitrust/sectoral bargaining remedies. If behavioral reservation wage shift dominates, the effect is a feature of the policy design (clawback rates, universality) amenable to calibration. The tangled_rope classification holds either way but the fixing_cost and gain_flow differ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_adjustment_mechanism, empirical, 'Structural vs. behavioral driver of employer capture.').

omega_variable(
    kernel_reading_disagreement,
    'The three readings of the income_support_conditionality kernel (wage_subsidy, freedom_floor, dependency_trap) make contradictory predictions about labor supply response at the reservation wage. Which prediction holds under what conditions?',
    'Meta-analysis of UBI/negative income tax experiments stratified by labor market structure (monopsony vs. competitive), clawback design, and demographic group. Cross-reading adversarial collaboration on a shared empirical protocol.',
    'If wage_subsidy prediction holds in monopsonistic sectors but freedom_floor holds in competitive sectors, the constraint is domain-specific — a tangled_rope in gig/care, a rope in manufacturing. This would require decomposing the kernel into sector-specific constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement, conceptual, 'Irreducible disagreement across readings of the same kernel on the core behavioral mechanism.').

omega_variable(
    subsidy_incidence_fraction,
    'What fraction of the unconditional income transfer is captured by employers via wage suppression versus retained by workers as net income gain?',
    'Incidence estimation from EITC expansions (20-30% employer capture), negative income tax experiments (10-50%), and gig platform studies (up to 80% in algorithmic monopsony). Need unified framework for cross-sector comparison.',
    'If incidence >50% employer capture, the constraint is strongly extractive (snare-adjacent tangled_rope). If <20%, it is coordination-dominant (rope-adjacent). The 0.68 extractiveness score assumes ~40-50% incidence based on current evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_incidence_fraction, empirical, 'Quantitative incidence of the public transfer on private wage setting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__wage_subsidy_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement_basis(inco_tr_t5, observed).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__wage_subsidy_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(inco_tr_t10, observed).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__wage_subsidy_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement_basis(inco_tr_t15, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t25, income_support_conditionality__wage_subsidy_reading, theater_ratio, 25, 0.34).
narrative_ontology:measurement_basis(inco_tr_t25, projected).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__wage_subsidy_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(inco_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(inco_be_t5, observed).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(inco_be_t10, observed).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(inco_be_t15, observed).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t25, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(inco_be_t25, projected).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(inco_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement_basis(inco_su_t5, observed).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(inco_su_t10, observed).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(inco_su_t15, observed).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement_basis(inco_su_t20, observed).
narrative_ontology:measurement(inco_su_t25, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement_basis(inco_su_t25, projected).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(inco_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__wage_subsidy_reading, 0.15).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, labor_market_flexibilization).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, gig_platform_algorithmic_management).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, sectoral_bargaining_coverage).

% DUAL FORMULATION NOTE:
% This constraint (wage_subsidy_reading) and its two siblings (freedom_floor_reading, dependency_trap_reading) form a constraint family decomposing the income_support_conditionality kernel. Each reading instantiates a different constraint with different beneficiary/victim structures and different ε values. The wage_subsidy_reading has employers as beneficiaries and workers as victims (ε=0.68). The freedom_floor_reading has workers as beneficiaries and no clear victims (ε≈0.15). The dependency_trap_reading has future taxpayers as victims and current non-workers as beneficiaries (ε≈0.45). They are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__wage_subsidy_reading, institutional, 0.15).
constraint_indexing:directionality_override(income_support_conditionality__wage_subsidy_reading, powerful, 0.2).
constraint_indexing:directionality_override(income_support_conditionality__wage_subsidy_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
