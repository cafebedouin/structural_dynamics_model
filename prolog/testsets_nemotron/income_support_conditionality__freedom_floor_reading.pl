% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Floor as Labor Decommodification
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story models the 'freedom floor' reading of unconditional
 *   income support: a universal, unconditional cash transfer that
 *   decommodifies labor power by giving every person a material basis to
 *   refuse coercive work. The constraint is the income floor itself — its
 *   operation as a structural arrangement that alters the power geometry of
 *   labor markets. The claimed type is rope: a genuine coordination mechanism
 *   that solves the collective-action problem of survival without extracting
 *   from the coordinated (workers) and without suppressing alternatives (work
 *   remains an option, not a mandate). The beneficiaries are workers who gain
 *   exit power; the payers are employers who lose coercive leverage and
 *   fiscal authorities who fund the transfer. This reading stands in contest
 *   with two sibling readings of the same kernel
 *   (income_support_conditionality): the dependency_trap_reading (which sees
 *   the constraint as snare creating work disincentives) and the
 *   wage_subsidy_reading (which sees it as tangled_rope subsidizing low-wage
 *   employers).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.12).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.08).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Floor as Labor Decommodification").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/social_policy/labor_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, 'f168cec4-e57f-4157-a142-a137893a976c').
narrative_ontology:cs_kernel_codification('f168cec4-e57f-4157-a142-a137893a976c', distributed).
narrative_ontology:cs_authority_grounding('f168cec4-e57f-4157-a142-a137893a976c', practice).
narrative_ontology:cs_interpretation_layer_present('f168cec4-e57f-4157-a142-a137893a976c').
narrative_ontology:cs_reading_relation('f168cec4-e57f-4157-a142-a137893a976c', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('f168cec4-e57f-4157-a142-a137893a976c', income_support_conditionality__wage_subsidy_reading, influences).
narrative_ontology:cs_axiom('f168cec4-e57f-4157-a142-a137893a976c', foundational, labor_decommodification_via_unconditional_exit).
narrative_ontology:cs_axiom_status(labor_decommodification_via_unconditional_exit, holdable).
narrative_ontology:cs_axiom_grounding('f168cec4-e57f-4157-a142-a137893a976c', labor_decommodification_via_unconditional_exit, deontological).
narrative_ontology:cs_axiom('f168cec4-e57f-4157-a142-a137893a976c', foundational, survival_not_contingent_on_labor_market_attachment).
narrative_ontology:cs_axiom_status(survival_not_contingent_on_labor_market_attachment, holdable).
narrative_ontology:cs_axiom_grounding('f168cec4-e57f-4157-a142-a137893a976c', survival_not_contingent_on_labor_market_attachment, deontological).
narrative_ontology:cs_axiom('f168cec4-e57f-4157-a142-a137893a976c', secondary, positive_freedom_requires_material_exit_option).
narrative_ontology:cs_axiom_status(positive_freedom_requires_material_exit_option, holdable).
narrative_ontology:cs_axiom_grounding('f168cec4-e57f-4157-a142-a137893a976c', positive_freedom_requires_material_exit_option, deontological).
narrative_ontology:cs_reference_frame('f168cec4-e57f-4157-a142-a137893a976c', welfare_state_conditional_survival).
narrative_ontology:cs_drift_state('f168cec4-e57f-4157-a142-a137893a976c', post_pandemic_basic_income_momentum, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('f168cec4-e57f-4157-a142-a137893a976c', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, informal_sector_workers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, coercive_employers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, low_wage_employers_reliant_on_exit_threat).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, fiscal_authorities).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, labor_decommodification_thesis).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, positive_freedom_of_exit).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, universal_basic_security_as_coordination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income that covers basic needs regardless of employment status. Gain genuine capacity to refuse exploitative, unsafe, or degrading work without facing destitution. Can negotiate from a position of basic security rather than desperation. Exit from coercive employment becomes a real option.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    organized, biographical, mobile, national).

% Gig, contract, and informal workers who previously had no income floor. The unconditional support stabilizes income volatility and enables selective participation in precarious labor markets. Can refuse gigs that violate safety or dignity standards without immediate financial catastrophe.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, biographical, mobile, national).

% Unpaid care workers (childcare, eldercare, disability care) gain independent income recognition. No longer forced into market labor to survive while performing socially essential care work. The income floor coordinates recognition of care as work without commodifying it.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, constrained, national).

% Workers outside formal labor protections who face extreme exploitation. Unconditional income provides survival floor that makes informal work genuinely voluntary. Can refuse the most predatory informal arrangements.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, informal_sector_workers, beneficiary,
    powerless, immediate, constrained, national).

% Employers who previously relied on the threat of destitution to discipline workers (sweatshops, abusive warehouses, predatory gig platforms, exploitative agricultural operations). Lose coercive firing power — workers can walk away. Must compete on wages, conditions, and dignity to attract labor. Experience this as extraction (higher labor costs, lost disciplinary leverage).
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, coercive_employers, payer,
    powerful, biographical, constrained, national).

% Employers in low-margin sectors (hospitality, retail, personal services, agriculture) whose business models depended on workers having no exit option. Face rising wage floor and improved conditions pressure. Not inherently abusive but structurally reliant on labor's lack of alternatives. Experience the constraint as cost increase without offsetting productivity gain.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_employers_reliant_on_exit_threat, payer,
    organized, biographical, constrained, national).

% Design and administer the unconditional income system. Determine eligibility (universal vs. targeted), payment levels, delivery mechanisms, and integration with existing welfare bureaucracies. Their design choices shape whether the constraint functions as genuine decommodification or drifts into means-tested conditionalities.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, social_policy_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Finance the income floor through taxation, debt, or monetary operations. Bear the fiscal cost which they experience as extraction from the public purse. Their revenue choices (progressive vs. regressive taxation, deficit tolerance) determine the constraint's distributional profile and political sustainability.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, fiscal_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__freedom_floor_reading, fiscal_authorities, payer).

% Study the constraint's effects on labor supply, wages, productivity, inflation, and power dynamics. Produce the evidence base that policy actors cite. Their analytical frameworks (neoclassical vs. institutional vs. Marxian) shape how the constraint's effects are interpreted and whether it is read as rope, snare, or scaffold.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, labor_market_economists, observer,
    analytical, civilizational, analytical, global).

% Those currently outside the labor market who would be direct beneficiaries but are often absent from policy design conversations. Their voices would emphasize survival needs over work-incentive frameworks. Structural exclusion from legislative and media discourse about 'deservingness' and 'work requirements.'
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, unemployed_and_underemployed, excluded,
    powerless, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of guaranteeing every person a material exit option from coercive labor relations without requiring complex means-testing, behavioral conditions, or administrative surveillance. Creates a universal floor that makes 'refusal' a live option, coordinating labor market participation on genuinely voluntary terms rather than desperation.
% TRANSFER_FUNCTION: Moves fiscal resources from the public purse (funded by progressive taxation, resource rents, or monetary issuance) directly to individuals as unconditional cash. The transfer is not contingent on labor market status, job search, training compliance, or behavioral conditions. Employers indirectly 'pay' through higher equilibrium wages and improved conditions required to attract workers who now have an exit option.
% ABSENT_VOICES: The unemployed and underemployed who would be primary beneficiaries are structurally excluded from policy design — their absence enables the 'work incentive' framing that dominates legislative debates. Migrants without regularized status are often explicitly excluded from universal schemes despite performing essential labor. Disabled people whose capacity for 'work' is contested are frequently marginalized in the 'ability to work' discourse.
% DISAPPEARANCE_RATIONALE: If the unconditional income floor vanished overnight, millions of workers would lose their only material basis for refusing coercive work. Labor markets would revert to desperation-driven allocation. Employers would regain unilateral disciplinary power. Precarious workers would face immediate income collapse. Care work would be further devalued. The social contract around 'work as condition of survival' would reassert itself violently.
% FOUNDING_PROBLEM: Industrial capitalism created a structural dependency: workers must sell labor power to survive, giving employers coercive leverage. The welfare state's conditional benefits (unemployment insurance, means-tested assistance) maintained this dependency by making survival contingent on labor market attachment or bureaucratic compliance. The founding problem is how to secure survival without subordinating it to the labor market or the administrative state.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record of labor movements demanding 'the right to refuse work' (Autonomist Marxism, Wages for Housework, Basic Income Earth Network), by philosophical arguments for decommodification (Polanyi, Gorz, Standing), and by empirical evidence from pilot programs (Manitoba Mincome, Finland, Kenya, Stockton) showing increased refusal of exploitative work without labor supply collapse. No corroboration comes from employers or fiscal conservatives — they contest the problem's framing, not its existence.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).
:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.12) because the constraint's primary operation is a transfer FROM the public purse TO individuals — the 'extraction' experienced by employers is indirect (higher wage floor) and is the price of genuine coordination, not a transfer captured by a third party. Suppression is very low (0.08) because the constraint expands rather than contracts the option set — no one is forced to accept the income, no conditions are attached, and labor market participation remains voluntary. Theater ratio is low (0.15) and slowly rising as administrative complexity creeps in (universal delivery systems, integration with tax bureaucracies), but the core mechanism remains straightforward cash transfer. Accessibility collapse is low (0.25) because alternatives (conditional welfare, workfare, targeted transfers) remain fully available and politically live — the unconditional floor does not foreclose other policy designs. Resistance is moderate (0.35) from employer organizations, fiscal conservatives, and 'work ethic' cultural narratives, but this resistance is political contestation, not suppression of exit.
 *
 * PERSPECTIVAL GAP:
 *   From the worker seats, this constraint is experienced as pure rope: a coordination mechanism that secures survival and enables refusal. From the coercive employer seat, it is experienced as extraction: a forced transfer of disciplinary power and margin. From the low-wage employer seat, it is a cost pressure that may feel like snare if they cannot pass costs forward. The engine computes these seat-specific classifications from the structural data; the divergence IS the measurement. The claimed_type (rope) reflects the constraint's primary structural logic from the coordination seat; the payer seats will compute differently, and that divergence is analytically correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers, precarious workers, caregivers, and informal sector workers are structural beneficiaries: they receive the transfer, gain exit power, and face no conditions. Their directionality d is near 0.0 (full beneficiary). Coercive employers and low-wage employers reliant on exit threats are structural payers: they lose disciplinary power and face higher labor costs. Their d is near 1.0 (full target). Social policy administrators and fiscal authorities are agenda_setters who also bear costs (administration, funding) — their d is intermediate (~0.4-0.5). Labor market economists are analytical observers (d = 0.5 by definition). The unemployed and underemployed are excluded — they would be beneficiaries but are structurally absent from the constraint's design and defense.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows no mandatrophy — its founding problem (labor's structural dependency on employers for survival) remains live and intensifying with platform labor, gigification, and climate displacement. The arrangement is not a decayed remnant of a solved problem but a proposed solution to a worsening problem. The theater_ratio drift (0.05→0.15) reflects administrative accretion, not functional atrophy. If implemented, the constraint would require active defense against conditionalities (work requirements, means-testing, behavioral nudges) that would convert it from rope to scaffold or snare. The mandatrophy risk is forward-looking: the constraint could be captured and conditionalized, not that it has outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employer_capture_of_transfer,
    'Do low-wage employers capture the unconditional transfer through wage suppression (wage_subsidy_reading), or does the transfer genuinely raise the reservation wage and improve worker bargaining power (freedom_floor_reading)?',
    'Empirical evidence from universal basic income pilots and negative income tax experiments on reservation wages, job acceptance rates, and wage floors in low-wage sectors. Comparison of labor market tightness and wage growth in jurisdictions with vs. without income floors.',
    'If employers capture the transfer, the constraint is tangled_rope (extraction from workers/taxpayers to employers) not rope. The freedom_floor_reading''s claimed_type would be falsified. If workers'' reservation wage rises, the reading is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_capture_of_transfer, empirical, 'Whether the income floor''s transfer is captured by employers via wage suppression.').

omega_variable(
    work_disincentive_magnitude,
    'Does the income floor produce significant labor supply reduction (dependency_trap_reading) or primarily enable refusal of coercive/degrading work while maintaining overall participation (freedom_floor_reading)?',
    'Longitudinal labor supply analysis from pilots: distinguish between (a) reduction in hours/work intensity, (b) refusal of specific coercive jobs, (c) exit from labor force entirely. Track job quality improvements, entrepreneurial activity, care work, and education uptake among recipients.',
    'If labor supply collapses broadly, the constraint fails its coordination function (rope) and becomes snare (extracting from future productivity). If refusal is selective and targeted at coercive work, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(work_disincentive_magnitude, empirical, 'Whether unconditional income reduces labor supply broadly or selectively enables refusal of coercive work.').

omega_variable(
    fiscal_sustainability_and_progressive_funding,
    'Can the income floor be funded at a meaningful level through progressive taxation/resource rents without generating inflationary pressure or political backlash that triggers conditionalities?',
    'Macroeconomic modeling of funding mechanisms (wealth tax, carbon dividend, land value tax, monetary financing) at scale. Political economy analysis of coalition stability for universal vs. targeted designs.',
    'If funding requires regressive taxation or generates inflation that erodes the floor''s real value, the constraint drifts toward scaffold (transitional) or snare (conditionalities imposed to control cost). If progressive funding is sustainable, the rope structure holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_sustainability_and_progressive_funding, conceptual, 'Whether the fiscal foundations of the income floor can sustain its unconditional character at scale.').

omega_variable(
    reading_relations_structure,
    'What is the structural relationship between the freedom_floor_reading and its sibling readings — do they foreclose each other, coexist, or influence?',
    'Analyze the logical structure of each reading''s core axioms: does the freedom_floor_reading''s axiom ''labor_decommodification_via_unconditional_exit'' logically contradict the dependency_trap_reading''s axiom ''work_requirement_as_social_cohesion'' within a single policy framework? Does the wage_subsidy_reading''s axiom ''employer_capture_of_transfer'' structurally influence the freedom_floor_reading''s operating conditions?',
    'Determines cs_structure.reading_relations: forecloses means the readings cannot coexist in one legislative framework; coexists_with means different political coalitions hold them simultaneously; influences means one reading''s adoption changes the legitimacy conditions for the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_structure, conceptual, 'Structural relationship between freedom_floor_reading and sibling readings of the income_support_conditionality kernel.').

omega_variable(
    migrant_exclusion_as_structural_feature,
    'Is the exclusion of irregular migrants from universal income floors a contingent political compromise or a structural necessity of the nation-state welfare form?',
    'Comparative analysis of universal vs. residence-based vs. citizenship-based designs. Historical analysis of welfare state expansion and migrant inclusion/exclusion. Legal analysis of social rights frameworks.',
    'If exclusion is structural, the freedom_floor_reading''s universalism is inherently partial — the constraint coordinates freedom for some by excluding others. This would shift the constraint toward tangled_rope (coordination for citizens, extraction from/exclusion of migrants). If exclusion is contingent, the reading''s universalism is expandable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(migrant_exclusion_as_structural_feature, conceptual, 'Whether migrant exclusion from income floors is a structural feature or contingent boundary of the freedom_floor_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isf_floor_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(isf_floor_tr_t5, income_support_conditionality__freedom_floor_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement(isf_floor_tr_t10, income_support_conditionality__freedom_floor_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(isf_floor_tr_t15, income_support_conditionality__freedom_floor_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement(isf_floor_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(isf_floor_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(isf_floor_be_t5, income_support_conditionality__freedom_floor_reading, base_extractiveness, 5, 0.07).
narrative_ontology:measurement(isf_floor_be_t10, income_support_conditionality__freedom_floor_reading, base_extractiveness, 10, 0.09).
narrative_ontology:measurement(isf_floor_be_t15, income_support_conditionality__freedom_floor_reading, base_extractiveness, 15, 0.11).
narrative_ontology:measurement(isf_floor_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(isf_floor_su_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(isf_floor_su_t5, income_support_conditionality__freedom_floor_reading, suppression_requirement, 5, 0.04).
narrative_ontology:measurement(isf_floor_su_t10, income_support_conditionality__freedom_floor_reading, suppression_requirement, 10, 0.06).
narrative_ontology:measurement(isf_floor_su_t15, income_support_conditionality__freedom_floor_reading, suppression_requirement, 15, 0.07).
narrative_ontology:measurement(isf_floor_su_t20, income_support_conditionality__freedom_floor_reading, suppression_requirement, 20, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__freedom_floor_reading, 0.1).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__wage_subsidy_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, labor_market_regulation).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, housing_cost_constraint).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, healthcare_access_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the income_support_conditionality kernel. The freedom_floor_reading claims rope classification (genuine coordination on exit option). The dependency_trap_reading claims snare (work disincentive extraction). The wage_subsidy_reading claims tangled_rope (employer capture of transfer). All three share the same referent (unconditional income support) but author different ε, different victim/beneficiary structures, and different types. They are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__freedom_floor_reading, powerful, 0.85).
constraint_indexing:directionality_override(income_support_conditionality__freedom_floor_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
