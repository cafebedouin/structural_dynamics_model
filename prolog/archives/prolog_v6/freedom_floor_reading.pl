% ============================================================================
% CONSTRAINT STORY: freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_freedom_floor_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: freedom_floor_reading
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor (Freedom Floor Reading)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   The freedom-floor reading of unconditional income support asserts that an
 *   unconditional income floor enables autonomy by removing coercion from the
 *   labor market and eliminating welfare stigma. This reading frames UCS as a
 *   pure coordination mechanism: matching workers' preferences to available
 *   labor, recognizing unpaid care work, enabling artistic and
 *   entrepreneurial risk-taking, and allowing abuse victims to escape
 *   economic traps. The reading explicitly rejects the premise that UCS
 *   creates dependence — it claims dependence is the problem that UCS solves.
 *   This constraint story instantiates ONLY this reading and routes the
 *   contest with sibling readings (dependency-trap and universality-paradox)
 *   to omega variables and cs_structure declarations. The core empirical
 *   claim is that labor supply is inelastic at moderate UCS levels (per
 *   Alaska Permanent Fund, Kenya GiveDirectly trials, Finland pilot), meaning
 *   the coordination function operates without the extractive side effects
 *   the dependency-trap reading emphasizes. The core normative claim is that
 *   autonomy should be measured by choice freedom from coercion, not by
 *   self-sufficiency from state transfers.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary beneficiary (powerless/mobile) — currently coerced by subsistence need; UCS enables exit from exploitative arrangements without destitution
 *   - Caregivers and Unpaid Laborers: Primary beneficiary (moderate/mobile) — currently excluded from income or forced to choose between care work and market employment; UCS coordinates recognition of care value
 *   - Artists and Entrepreneurs: Primary beneficiary (moderate/mobile) — currently forced into risk-averse employment; UCS enables high-variance career paths
 *   - Abuse Victims: Primary beneficiary (moderate/mobile) — currently trapped by economic dependency on abusers; UCS enables escape to safety
 *   - Labor Market Collective / Union: Secondary beneficiary (organized/arbitrage) — gains bargaining power when workers can exit low-wage arrangements
 *   - Tax-Funded UCS Institution: Institutional implementer (institutional/arbitrage) — provides coordination mechanism; has arbitrage exit options for floor level and scope
 *   - Dependency-Trap Reading (Sibling Constraint): Coexists with this reading; differs on empirical labor-supply response and normative concept of dependence
 *   - Universality-Paradox Reading (Sibling Constraint): Influences this reading; creates structural pressure on fiscal scope and targeting mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(freedom_floor_reading, 0.18).
domain_priors:suppression_score(freedom_floor_reading, 0.12).
domain_priors:theater_ratio(freedom_floor_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(freedom_floor_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(freedom_floor_reading, rope).
narrative_ontology:human_readable(freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor (Freedom Floor Reading)").
narrative_ontology:topic_domain(freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(freedom_floor_reading, '677a9dec-638b-4a7e-9cdf-46aa3b975fa1').
narrative_ontology:cs_created_at('677a9dec-638b-4a7e-9cdf-46aa3b975fa1', '').
narrative_ontology:cs_kernel_codification('677a9dec-638b-4a7e-9cdf-46aa3b975fa1', formalized).
narrative_ontology:cs_authority_grounding('677a9dec-638b-4a7e-9cdf-46aa3b975fa1', distributed).
narrative_ontology:cs_kernel_id(freedom_floor_reading, unconditional_income_support).
narrative_ontology:cs_reading_relation('677a9dec-638b-4a7e-9cdf-46aa3b975fa1', dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('677a9dec-638b-4a7e-9cdf-46aa3b975fa1', universality_paradox_reading, influences).
narrative_ontology:cs_axiom('677a9dec-638b-4a7e-9cdf-46aa3b975fa1', foundational, labor_coercion_via_subsistence_is_primary_constraint).
narrative_ontology:cs_axiom_status(labor_coercion_via_subsistence_is_primary_constraint, holdable).
narrative_ontology:cs_axiom_grounding('677a9dec-638b-4a7e-9cdf-46aa3b975fa1', labor_coercion_via_subsistence_is_primary_constraint, empirically_contingent).
narrative_ontology:cs_axiom('677a9dec-638b-4a7e-9cdf-46aa3b975fa1', foundational, autonomy_measures_exit_freedom_from_coercion_not_self_sufficiency).
narrative_ontology:cs_axiom_status(autonomy_measures_exit_freedom_from_coercion_not_self_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('677a9dec-638b-4a7e-9cdf-46aa3b975fa1', autonomy_measures_exit_freedom_from_coercion_not_self_sufficiency, deontological).
narrative_ontology:cs_reference_frame('677a9dec-638b-4a7e-9cdf-46aa3b975fa1', voluntary_labor_participation).
narrative_ontology:cs_drift_state('677a9dec-638b-4a7e-9cdf-46aa3b975fa1', contemporary_labor_market_era, gap(stable, minor, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(freedom_floor_reading, artists).
narrative_ontology:constraint_beneficiary(freedom_floor_reading, abuse_victims).
narrative_ontology:constraint_beneficiary(freedom_floor_reading, labor_market_entrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (ROPE) — UCS enables exit from exploitative labor arrangements. Unconditional income removes the coercive force of subsistence need, converting forced labor into genuine choice. The constraint solves a coordination problem: matching worker preferences to available labor rather than matching subsistence need to any available wage. Extractiveness is low because the mechanism is purely coordinative — no asymmetric benefit accrues to the income supporter; the worker and the fund both benefit from voluntary participation.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREGIVER / UNPAID LABORER (ROPE) — UCS coordinates recognition of care work (childcare, eldercare, community support) that markets systematically underprice or exclude entirely. The constraint enables exit from forced market participation without abandoning valued work. The income floor removes the coercion that currently forces caregivers into paid employment or destitution. Low extractiveness — the mechanism is pure coordination (recognizing and paying for value previously externalized), not extraction from caregivers or toward them.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: ARTIST / ENTREPRENEUR (ROPE) — UCS enables high-variance career paths (creative work, startup formation, skill development) by decoupling subsistence from employment. Removes the constraint that forces risk-averse choice of stable employment over riskier, higher-value activities. Coordination mechanism: matching economic security to activity preference rather than forcing activity to match earnings requirements. Temporal dimension critical — at generational horizon, the accumulation of artistic and entrepreneurial output from a UCS-enabled population produces net positive externalities (cultural goods, technological innovation). Low extractiveness across this horizon.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ABUSE VICTIM (ROPE) — UCS removes the economic trap that binds abuse victims to abusers. Housing, food, and subsistence security become unconditional; leaving abusive situations no longer entails destitution or homelessness. Coordination function: matching safety preferences to resource availability rather than forcing victims to choose between physical safety and economic survival. Exit option shifts from trapped to mobile. Extractiveness is low because the income floor creates no asymmetry — both victim and the funding source benefit from the victim's exit to safety and autonomy.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR MARKET COLLECTIVE / UNION (ROPE) — UCS coordinates labor supply and demand without coercive wage floors or unemployment. Workers retain bargaining power because they can exit low-wage work without starvation; employers must compete on job quality, not subsistence desperation. The constraint removes the race-to-the-bottom dynamic where workers undercut each other out of survival pressure. Institutional perspective sees a coordination mechanism enabling voluntary sector participation at higher equilibrium quality. Low extractiveness from the organized perspective — the mechanism strengthens the collective's position without extracting from other agents.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: UCS FUNDING INSTITUTION (ROPE) — The institution that distributes the income floor (government, tax authority, sovereign wealth fund) coordinates redistribution by decoupling resource allocation from labor market outcomes. The mechanism is pure coordination: matching purchasing power to population need rather than forcing all income through employment or means-tested bureaucracy. Exit option for the institution is arbitrage — they can adjust the UCS floor, expand/contract beneficiary scope, or modify funding sources. Extractiveness is low because the funding institution has no incentive to extract; the coordination mechanism works when resources flow efficiently to target population.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some form of subsistence guarantee appears as an immutable requirement of any sustainable economic order: populations cannot function when basic survival is contingent on employment. This perspective risks naturalizing what is actually a contestable policy choice. The freedom-floor reading explicitly rejects the mountain classification — it claims UCS is a human-designed coordination mechanism, not a law of nature. The engine's false-summit detector should flag this: if this perspective naturalizes contingent institutional design, it obscures the policy debate.
constraint_indexing:constraint_classification(freedom_floor_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(freedom_floor_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(freedom_floor_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(freedom_floor_reading, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The freedom-floor reading models UCS as a pure coordination mechanism with minimal asymmetric extraction. The income floor benefits beneficiaries (removed from labor coercion) and the funding institution (achieves efficient redistribution) with no third party bearing costs. The low value reflects the absence of predatory beneficiaries or captive victims. Suppression (0.12): Very low. The mechanism is unconditional — no means-testing, no behavioral requirements, no bureaucratic discretion. Low administrative suppression compared to welfare-state alternatives (which impose work requirements, asset limits, stigmatizing eligibility screens). Theater ratio (0.25): Low. Direct cash transfer has minimal performative content — the mechanism is transparent (cash flow), the outcomes are measurable (income level, labor supply), and there is little room for theatrical compliance. The modest rise over the interval (0.20→0.25) reflects increasing administrative overhead as populations scale, but theater remains functionally marginal compared to welfare-bureaucracy alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between this reading and the dependency-trap reading is significant but not foreclosing. Both readings agree on the mechanism (cash transfer) and the beneficiaries (precarious workers, caregivers, artists, abuse victims). They diverge on: (1) empirical labor-supply response — freedom-floor claims inelasticity; dependency-trap claims substantial disincentive effects; (2) normative framing of dependence — freedom-floor claims dependence on employment is the problem; dependency-trap claims dependence on state transfers is worse; (3) autonomy concept — freedom-floor measures autonomy as exit options from coercion; dependency-trap measures autonomy as self-sufficiency from transfers. The universality-paradox reading coexists with both, adding a third axis: the targeting vs. universality tension. Each reading remains live in the policy debate without logically foreclosing the others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the beneficiary-victim structure and exit options. All declared beneficiaries occupy positions where UCS removes a binding constraint: subsistence coercion, unpaid labor externalization, risk aversion in employment, economic dependence on abusers. None of these beneficiaries are extractors — they gain autonomy without creating victims. The absence of declared victims is structurally significant: the freedom-floor reading asserts the UCS is a Pareto improvement (or near-Pareto, with only fiscal redistribution as the cost to general taxpayers, not a specific victim group). Institutional and organized perspectives have arbitrage exit — they can adjust UCS parameters or redirect funding. Powerless and moderate perspectives have mobile exit — they can shift labor market participation patterns in response to UCS. This exit-option distribution produces low d values (beneficiary-skewed) and therefore low f(d) values, keeping chi low and extractiveness minimal.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by declaring a single clear beneficiary class (precarious workers and those constrained by labor coercion) and no victim group. The coordination function is transparent (income floor enabling voluntary participation) and the extraction is explicitly modeled as zero or near-zero. The measurement trajectory shows theater ratio and extractiveness stable and low, confirming the rope classification across the interval. No mandatrophy emerges because the reading does not claim both high coordination and high extraction — it claims high coordination and low extraction, a coherent position. The omega variables document the empirical uncertainties (labor-supply response, inflation capture, macroeconomic scale) that could shift the classification toward tangled-rope if resolved unfavorably, but these are tracked as contingencies, not inherent contradictions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_response_empirical,
    'What is the actual labor supply elasticity in response to unconditional income support? Do empirical findings from Alaska, Kenya, Finland, and Ontario refute the ''work disincentive'' hypothesis or validate it under certain conditions?',
    'Meta-analysis of randomized controlled trials and natural experiments with UCS or UBI pilots. Disaggregation by work type (subsistence vs. discretionary), duration (short-term vs. long-term), and population (working-age vs. mixed). Primary outcome: labor supply response relative to income floor level.',
    'If labor supply inelastic at low-to-moderate UCS levels: ε estimate confirmed at 0.18, rope classification robust. If labor supply highly elastic: ε might rise to 0.35-0.40, shifting toward tangled-rope or snare in some contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_supply_response_empirical, empirical, 'Labor supply response to unconditional income floor').

omega_variable(
    inflation_and_rent_capture,
    'Does universal cash transfer get partially captured by landlords and rent-seekers through price inflation, reducing net autonomy gain for beneficiaries?',
    'Time-series analysis of rental markets, food prices, and service costs in UCS-pilot regions compared to control regions. Measurement of real purchasing power and housing accessibility pre/post UCS. Landlord-behavior studies examining rent-setting responses to known UCS payments.',
    'If captured: effective extraction mechanism emerges (beneficiaries pay in rent what they gain in cash) — ε might rise to 0.35-0.45, type shifts toward tangled_rope. If not captured: extraction is minimal, rope classification and low ε confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_and_rent_capture, empirical, 'Rent capture and inflation response to UCS').

omega_variable(
    welfare_stigma_elimination_structural,
    'Is welfare stigma a contingent cultural artifact of means-tested systems, or does it persist even under unconditional universal distribution? Does the stigma-removal claim depend on universal scope?',
    'Comparative analysis of UCS designs: universal (all citizens) vs. targeted (low-income only). Measurement of self-reported stigma, labor market discrimination, and social integration for beneficiaries under each design. Longitudinal tracking of psychological outcomes and employment patterns.',
    'If stigma persists under targeted UCS: autonomy gain is partial — classification might shift toward tangled_rope (coordination + residual extraction through stigma). If eliminated under universal design: autonomy gain confirmed, rope classification robust. Scope dimension critical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_stigma_elimination_structural, empirical, 'Whether stigma elimination requires universal scope').

omega_variable(
    macroeconomic_stability_unknown,
    'At what population-wide funding scale does UCS become macroeconomically destabilizing? Is there a fiscal or inflation ceiling where the coordination mechanism breaks down?',
    'Agent-based modeling of UCS at multiple funding levels (5%, 15%, 30% of GDP). Real-world case studies from countries with high social spending (Nordic nations at 25-30% of GDP social transfers). Measurement of fiscal sustainability, inflation, and productivity growth.',
    'If stable at high scale: classification holds at national scope. If ceiling exists below universal coverage: effective extractiveness rises due to implicit rationing — ε might rise toward 0.35-0.45, type shifts toward tangled_rope (coordination + resource constraint extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(macroeconomic_stability_unknown, empirical, 'Fiscal and macroeconomic ceiling for UCS expansion').

omega_variable(
    reading_constitutive_empirical_premise,
    'Does this freedom-floor reading depend on the empirical premise that labor coercion via subsistence need is the PRIMARY mechanism constraining worker autonomy, or are non-economic coercive mechanisms (family authority, social norms, legal restriction) equally or more constraining?',
    'Comparative analysis of agent exit decisions: measure relative importance of subsistence need vs. other constraints on choice to exit employment, leave abusive situations, or pursue non-market work. Survey and interview studies of precarious workers, caregivers, and abuse victims about binding constraints on their choices.',
    'If economic coercion is primary: the reading''s core premise is sound, ε and classification confirmed. If non-economic constraints are equally/more binding: UCS is necessary but insufficient for autonomy; classification might shift toward tangled_rope (UCS coordinates subsistence but not other coercions) or piton (theater of autonomy without removing binding constraints).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_constitutive_empirical_premise, empirical, 'Whether economic coercion is the primary autonomy-constraining mechanism').

omega_variable(
    kernel_reading_constitutive,
    'This is the freedom-floor reading of the unconditional-income-support kernel. What structural features distinguish this reading from the dependency-trap reading and the universality-paradox reading?',
    'Logical analysis of each reading''s core axioms and counterfactual: freedom-floor asserts UCS expands autonomy by removing coercion; dependency-trap asserts it increases dependence on the state; universality-paradox asserts it faces a structural contradiction between targeting (effective but stigmatizing) and universality (equitable but fiscally overextended). Identify where these readings agree and diverge on facts and values.',
    'This omega documents the kernel reading itself as an irreducible ambiguity in the political economy of UCS. No empirical data alone resolves which reading is ''correct'' — they disagree on value-laden concepts (autonomy vs. dependence) and on how to weigh competing axioms (liberty vs. community, individual vs. collective benefit). The constraint story instantiates only this one reading per Rule 1.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_constitutive, conceptual, 'Kernel reading identity and axiom divergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(freedom_floor_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(free_tr_t0, freedom_floor_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(free_tr_t5, freedom_floor_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(free_tr_t10, freedom_floor_reading, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(free_be_t0, freedom_floor_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(free_be_t5, freedom_floor_reading, base_extractiveness, 5, 0.17).
narrative_ontology:measurement(free_be_t10, freedom_floor_reading, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(freedom_floor_reading, dependency_trap_reading).
narrative_ontology:affects_constraint(freedom_floor_reading, universality_paradox_reading).

% DUAL FORMULATION NOTE:
% The unconditional_income_support kernel has three constraint stories corresponding to three readings: freedom_floor_reading (UCS as autonomy enabler), dependency_trap_reading (UCS as state dependence), and universality_paradox_reading (UCS facing targeting-universality contradiction). Each story has its own epsilon, its own beneficiary/victim structure, and its own classification. They are linked as siblings via network.affects_constraints to indicate they are competing interpretations of the same policy kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
