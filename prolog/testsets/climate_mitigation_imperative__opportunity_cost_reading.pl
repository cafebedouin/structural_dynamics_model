% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_opportunity_cost, []).

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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Climate Mitigation Opportunity Cost: Capital Velocity Imperative
 *   domain: energy_policy/climate/technology_governance
 *
 * SUMMARY:
 *   This is ONE READING of the contested kernel
 *   'climate_mitigation_imperative': the opportunity-cost reading, which
 *   argues that climate mitigation requires the fastest possible carbon
 *   displacement per dollar spent, and therefore that nuclear's capital
 *   intensity and 10-15 year construction timeline make it net-harmful to
 *   climate goals despite its low-carbon output. Capital spent on nuclear is
 *   capital NOT spent on solar and wind, which deploy faster and achieve
 *   carbon reduction sooner. The constraint operationalizes this reading by
 *   making carbon-per-dollar-per-year the binding investment metric. This
 *   reading competes with the portfolio-optimization reading (maximize all
 *   low-carbon sources; nuclear is necessary for baseload) and the
 *   systems-transition reading (mitigation requires decentralizing energy
 *   systems; nuclear perpetuates centralized extractive control). The three
 *   readings decompose a single natural-language claim ('climate mitigation
 *   imperative') into three structurally distinct constraints with different
 *   ε values, different beneficiary/victim sets, and different temporal
 *   dynamics. This file instantiates ONLY the opportunity-cost reading as a
 *   clean ε-invariant constraint; the siblings are separate constraint
 *   stories.
 *
 * KEY AGENTS:
 *   - climate_policy_setter: institutional agenda-setter; sets carbon-per-dollar-per-year metrics and allocation rules
 *   - distributed_renewable_developers: beneficiary; capture capital when speed-per-dollar dominates
 *   - fast_deployment_manufacturing_chains: beneficiary; expand market share under speed optimization
 *   - nuclear_industry: victim (payer); capital diverted to renewables; projects cancelled/deprioritized
 *   - large_capital_project_financiers: victim (payer); reallocation pressure; portfolio metrics shift against long-lead-time projects
 *   - grid_infrastructure_operators: payer+beneficiary; faster emissions reduction but higher near-term integration costs
 *   - energy_modelers_and_climate_economists: excluded; their multidimensional portfolio analyses are not the binding constraint
 *   - energy_security_strategists: excluded; resilience arguments do not move allocation under speed-optimization frame
 *   - carbon_accounting_framework_keeper: observer; defines what metrics count as climate progress
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.67).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Climate Mitigation Opportunity Cost: Capital Velocity Imperative").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '07073eb8-f96c-44ee-affb-f7729a71e8f1').
narrative_ontology:cs_kernel_codification('07073eb8-f96c-44ee-affb-f7729a71e8f1', distributed).
narrative_ontology:cs_authority_grounding('07073eb8-f96c-44ee-affb-f7729a71e8f1', extraction).
narrative_ontology:cs_reading_relation('07073eb8-f96c-44ee-affb-f7729a71e8f1', climate_mitigation_imperative__portfolio_optimization_reading, influences).
narrative_ontology:cs_reading_relation('07073eb8-f96c-44ee-affb-f7729a71e8f1', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('07073eb8-f96c-44ee-affb-f7729a71e8f1', foundational, speed_per_dollar_is_binding_metric).
narrative_ontology:cs_axiom_status(speed_per_dollar_is_binding_metric, holdable).
narrative_ontology:cs_axiom_grounding('07073eb8-f96c-44ee-affb-f7729a71e8f1', speed_per_dollar_is_binding_metric, empirically_contingent).
narrative_ontology:cs_axiom('07073eb8-f96c-44ee-affb-f7729a71e8f1', secondary, temporal_urgency_overrides_portfolio_diversity).
narrative_ontology:cs_axiom_status(temporal_urgency_overrides_portfolio_diversity, holdable).
narrative_ontology:cs_axiom_grounding('07073eb8-f96c-44ee-affb-f7729a71e8f1', temporal_urgency_overrides_portfolio_diversity, deontological).
narrative_ontology:cs_reference_frame('07073eb8-f96c-44ee-affb-f7729a71e8f1', speed_optimized_decarbonization_allocation).
narrative_ontology:cs_drift_state('07073eb8-f96c-44ee-affb-f7729a71e8f1', contemporary_2023_2030, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('07073eb8-f96c-44ee-affb-f7729a71e8f1', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, distributed_renewable_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, fast_deployment_manufacturing_chains).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, large_capital_project_financiers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.67 at interval end) and rising through 2030 because the constraint's operation systematically diverts capital from nuclear and long-lead-time projects toward renewables, measured by a metric that privileges speed over total carbon payoff or portfolio diversity. Suppression is moderate-high (0.58) because the constraint must actively enforce the speed metric against competing allocation logics (portfolio optimization, resilience, total cost). The rise in suppression from 2020 to 2030 (0.35→0.61) reflects increasing institutional effort to enforce the opportunity-cost reading as competing readings gain traction from grid-integration costs and nuclear advocates. Theater is moderate (0.42) and peaks around 2030: carbon-per-dollar-per-year is a real metric with genuine analytical support, but increasingly serves as justification for capital flows that beneficiaries prefer for other reasons (profit margin, market expansion, extractive capture of 'climate urgency'). By 2035, theater stabilizes as portfolio pressure mounts and the constraint's limitations become visible. Accessibility collapse is high (0.71) because once the metric is adopted as the binding constraint, alternatives (nuclear, large hydro, mixed portfolios) collapse as feasible options within the allocation framework—not because they are physically impossible, but because they fail the speed-per-dollar test. Resistance is substantial (0.64) because nuclear advocates, grid operators, and energy security strategists actively contest the reading, producing modeling studies, regulatory challenges, and international advocacy for portfolio approaches.
 *
 * PERSPECTIVAL GAP:
 *   From the climate policy setter's position, the opportunity cost constraint solves an urgent allocation problem: given finite capital and a 1.5°C carbon budget, speed is the binding constraint. From the renewable developers' position, the constraint is excellent policy that happens to align with their commercial interests. From the nuclear industry's position, the constraint is a political attack dressed in technical language—a metric that privileges speed over reliability, carbon-intensity, or portfolio resilience, and that treats their superior long-term carbon performance as irrelevant. From the energy modeler's position, the constraint is analytically incomplete: it optimizes one dimension (speed) while ignoring others (cost, reliability, diversity) that their models show are jointly important. From the grid operator's position, the constraint creates near-term operational burden (high renewable penetration complexity) in exchange for long-term decarbonization benefit they support in principle but experience as cost. The engine should compute these divergences from the structural data; no single seat's experience is the 'true' classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The climate policy setter is the structural agenda-setter (d near 0.0, beneficiary): sets the rules, administers enforcement, collects political credit for rapid decarbonization. Renewable developers and manufacturing chains are strong beneficiaries (d ~0.2): capital flows toward them, their market expands, profit margins improve—the constraint makes them winners without requiring them to maintain it. Nuclear industry is a clear victim (d ~0.95): capital diverts away, projects are cancelled, their competitive position erodes independent of their carbon performance. Large capital financiers are victims (d ~0.75): their portfolio allocation strategies shift against long-lead-time projects, increasing perceived risk and return thresholds. Grid operators sit near symmetric (d ~0.5): genuine benefit from faster emissions reduction, but concentrated cost from integration burden. Energy modelers are analytical excluded (d is not applicable, they do not collect or pay; their analysis is consumed but does not move allocation). The directionality for nuclear is particularly sharp: they are victims not because they harm anyone but because the metric makes their temporal profile disqualifying despite technical excellence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: decarbonization urgency and capital constraints are real. The opportunity cost reading does not suffer from mandatrophy (dead founding problem persisting as theater). However, the analysis reveals rising complexity in how the constraint meets its mandate: as renewable penetration deepens (2025-2030), grid integration costs rise, and energy security concerns mount, the constraint's single-metric approach increasingly misses the multidimensional nature of the allocation problem. The theater ratio rises to 0.45 by 2030 because the speed-per-dollar metric becomes less fully explanatory—it continues to drive allocation, but increasingly serves as cover for other interests (manufacturing profit margins, centralized renewable deployment models) that are orthogonal to the founding mitigation problem. By 2035, theater stabilizes at 0.42 as the constraint's limitations become explicit and portfolio pressure forces recalibration. This is not mandatrophy (the founding problem remains live) but rather a tension between a live problem and a constraint whose structure becomes increasingly inadequate to the problem's full dimensionality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speed_vs_total_carbon_payoff_boundary,
    'Is deployment speed (carbon per dollar per year) or total lifecycle carbon intensity (grams per joule) the appropriate metric for capital allocation under decarbonization urgency?',
    'Systems modeling comparing net-zero pathways optimized on speed vs. total-carbon metrics: do they converge to similar technology portfolios and timelines, or does metric choice produce structurally different outcomes? If convergence, the constraint''s speed metric is a proxy for an underlying total-carbon optimization; if divergence, the metric choice is a substantive value judgment, not a technical conclusion.',
    'If metrics converge, the opportunity cost reading is equivalent to portfolio optimization under time pressure (no real disagreement). If they diverge, the reading is a deliberate choice to sacrifice portfolio diversity and resilience for speed—a value judgment, not a natural law. Impacts treatment of nuclear: convergent = nuclear is analytically marginal even in diversified portfolios (constraint is justified); divergent = nuclear is excluded by metric choice, not by carbon analysis (constraint is extractive cover).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(speed_vs_total_carbon_payoff_boundary, empirical, 'Whether speed-per-dollar is a proxy for total-carbon optimization or a distinct metric choice.').

omega_variable(
    grid_integration_cost_feedback,
    'As renewable penetration deepens toward 80%+ of generation, do integration costs (balancing, storage, transmission upgrade, grid hardening) grow to offset the deployment-speed advantage in carbon-per-dollar terms?',
    'Grid-integration cost studies from high-renewable-penetration regions (Denmark, California, South Australia) tracking real integration costs as a function of penetration level. If costs grow faster than renewable costs decline, the speed-per-dollar metric becomes locally invalid at high penetration—a constraint that works in 2020-2030 but breaks down in 2030-2040.',
    'A positive feedback (integration costs eroding speed advantage at high penetration) would show the constraint is time-horizon dependent: it optimizes for 2020-2030 but may be suboptimal for 2030-2050 systems. The theater ratio rises as this becomes visible (performance justification declines). If true, the constraint''s persistence beyond 2035 becomes extractive: it continues to direct capital toward renewables not because they remain fastest per dollar, but because beneficiaries'' interests are now decoupled from the underlying metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_integration_cost_feedback, empirical, 'Whether integration costs erode the speed advantage at high renewable penetration.').

omega_variable(
    small_modular_reactor_disruption,
    'If small modular reactors (SMRs) achieve cost and timeline projections (5-7 year deployment, $1-2B per unit), does the opportunity cost constraint''s exclusion of nuclear remain analytically justified?',
    'Tracking SMR development timeline and cost curves; if deployments begin and hit cost targets, re-running opportunity-cost analysis with SMR timeline and capital cost as inputs. If SMRs restore nuclear to speed-competitive range, the constraint''s structure changes: the victim set narrows (large-reactor projects excluded but SMRs included) and the constraint''s justification shifts from ''nuclear is too slow'' to ''only small-scale nuclear is acceptable.''',
    'SMR viability would not dissolve the constraint but would reframe it: the reading would become ''speed AND small-scale decentralization'' rather than ''pure speed optimization.'' This would partially align the opportunity-cost reading with the systems-transition reading (decentralization), narrowing the kernel contest. Alternatively, if SMRs fail to materialize or exceed cost targets, the constraint''s exclusion of nuclear hardens into a structural victim assignment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(small_modular_reactor_disruption, empirical, 'Whether SMR development disrupts nuclear''s classification as a speed-constrained victim.').

omega_variable(
    reading_kernel_distinction,
    'Is this constraint instantiating ONE reading of a single commitment kernel (climate mitigation imperative), or is it describing a fundamental incompatibility in what decarbonization requires?',
    'If the three sibling readings (opportunity_cost, portfolio_optimization, systems_transition) can be jointly held within a single decarbonization framework under different institutional or temporal contexts, then they are alternative readings of a unified kernel. If one reading''s core premise logically forecloses another''s (e.g., systems_transition''s ''nuclear perpetuates centralization'' directly contradicts portfolio_optimization''s ''nuclear is necessary and socially acceptable''), then the kernel houses incompatible commitments, not alternative readings of one commitment.',
    'This is a conceptual/framing question, not an empirical one. If readings coexist, the constraint story is one reading among several; if they foreclose each other, the ''kernel'' label is misleading and the contest is a fight over what decarbonization fundamentally means. The impact is on how to interpret the narrative: is this a policy choice (readings coexist) or a foundational value conflict (readings foreclose)? The engine computes type-divergence per seat; this omega documents whether the divergence is a measurement problem or a framing problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Whether the three readings decompose one kernel or express incompatible foundational commitments.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression (0.58) required to maintain the opportunity cost reading structural (regulatory enforcement, market-access barriers, subsidy conditioning) or internalized (energy analysts and policymakers have adopted speed-per-dollar as THE legitimate metric and no longer countenance other framings)?',
    'Post-regulatory relaxation trajectory: if nuclear advocates gain political power and relax the speed-metric enforcement, does capital immediately flow toward long-lead-time projects, or has the speed-per-dollar metric become so institutionalized in investment frameworks that it persists despite enforcement relaxation? If it persists despite enforcement change, suppression is partially internalized.',
    'If suppression is purely structural, relaxing regulations would dissolve the constraint and restore portfolio competition. If suppression is partially internalized, the constraint would persist as institutional culture even if formal enforcement were removed, making it more durable but also more difficult to dislodge without explicit deprogramming of investment frameworks. This affects the constraint''s type durability: a snare with internalized suppression is harder to exit than one with purely structural suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 2020, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(clim_tr_t2023, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2023, 0.35).
narrative_ontology:measurement(clim_tr_t2026, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2030, 0.45).
narrative_ontology:measurement(clim_tr_t2035, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2035, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement(clim_be_t2023, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2023, 0.52).
narrative_ontology:measurement(clim_be_t2026, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(clim_be_t2035, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2035, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2020, 0.35).
narrative_ontology:measurement(clim_su_t2023, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2023, 0.48).
narrative_ontology:measurement(clim_su_t2026, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2026, 0.56).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2030, 0.61).
narrative_ontology:measurement(clim_su_t2035, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2035, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__opportunity_cost_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE reading of the contested kernel climate_mitigation_imperative. It decomposes from a single natural-language claim ('mitigation requires fastest deployment per dollar; nuclear is net-harmful') into a specific ε-invariant constraint with beneficiaries (fast-deployment renewables) and victims (nuclear industry, long-lead-time project financiers). The sibling readings—portfolio_optimization_reading (maximize all low-carbon; nuclear is necessary) and systems_transition_reading (decentralize; reject nuclear's centralization)—are SEPARATE constraint stories with different ε values, different beneficiary/victim structures, and different temporal dynamics. All three stories link via network.affects_constraints to document the kernel decomposition. The three constraints are not views of one constraint; they are three distinct constraints that instantiate different interpretations of the same contested commitment. Each story carries its own claim, metrics, stakeholders, and six-questions analysis independent of the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__opportunity_cost_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
