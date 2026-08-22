% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__velocity_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Deployment-Velocity Test for Climate Technology Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested 'technology
 *   legitimacy kernel' governing climate mitigation policy: legitimacy is
 *   defined by whether a technology can be deployed at gigawatt scale within
 *   the remaining carbon-budget timeline (2030/2050 milestones). Under this
 *   reading, solar, wind, and battery storage qualify as legitimate because
 *   they can be permitted and built within a few years; nuclear power is
 *   marginalized or disqualified because its construction and licensing
 *   timelines routinely exceed a decade, regardless of its per-unit emissions
 *   profile or dispatchability. The reading functions as a genuine
 *   coordination device — it prevents slow technologies from being used to
 *   justify delaying near-term action — while simultaneously extracting real
 *   costs from grid operators, ratepayers, and communities that must live
 *   with the intermittency and system-reliability consequences of a resource
 *   mix optimized for build speed rather than total system performance.
 *   Sibling readings of the same kernel (reliability_primacy_reading,
 *   precautionary_reading) evaluate the identical technologies against
 *   different criteria and would produce different beneficiary/victim sets
 *   and different classifications entirely; this story does not average
 *   across them or describe their contest — it authors the velocity-primacy
 *   reading alone, at its own single, stable epsilon.
 *
 * KEY AGENTS:
 *   - solar_and_wind_developers: Primary beneficiary (organized/mobile) — favored by the deployment-speed test
 *   - battery_storage_manufacturers: Secondary beneficiary (organized/mobile) — demand created by the intermittency the test tolerates
 *   - renewable_finance_investors: Beneficiary (institutional/arbitrage) — captures returns from the legitimacy label
 *   - climate_advocacy_organizations_favoring_fast_rollout: Agenda-setter (organized/analytical) — authors and promotes the velocity criterion
 *   - grid_operators_managing_intermittency: Primary target (institutional/trapped) — bears real-time balancing burden
 *   - nuclear_developers_and_supply_chains: Primary target (powerful/constrained) — disqualified by timeline regardless of output
 *   - ratepayers_in_grids_with_high_curtailment_and_backup_costs: Diffuse target (powerless/trapped) — pays embedded system costs
 *   - communities_dependent_on_displaced_baseload_plants: Excluded (powerless/trapped) — no voice in the criterion itself
 *   - national_climate_regulators: Analytical observer (institutional/analytical) — operationalizes some reading through rulemaking
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.58).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.62).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Deployment-Velocity Test for Climate Technology Legitimacy").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, '5ab2949d-e846-42aa-87fc-f10c713c3233').
narrative_ontology:cs_kernel_codification('5ab2949d-e846-42aa-87fc-f10c713c3233', distributed).
narrative_ontology:cs_authority_grounding('5ab2949d-e846-42aa-87fc-f10c713c3233', distributed).
narrative_ontology:cs_reading_relation('5ab2949d-e846-42aa-87fc-f10c713c3233', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ab2949d-e846-42aa-87fc-f10c713c3233', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('5ab2949d-e846-42aa-87fc-f10c713c3233', foundational, deployment_speed_is_the_binding_constraint).
narrative_ontology:cs_axiom_status(deployment_speed_is_the_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('5ab2949d-e846-42aa-87fc-f10c713c3233', deployment_speed_is_the_binding_constraint, empirically_contingent).
narrative_ontology:cs_axiom('5ab2949d-e846-42aa-87fc-f10c713c3233', secondary, near_term_emissions_avoided_dominates_long_term_technology_merit).
narrative_ontology:cs_axiom_status(near_term_emissions_avoided_dominates_long_term_technology_merit, holdable).
narrative_ontology:cs_axiom_grounding('5ab2949d-e846-42aa-87fc-f10c713c3233', near_term_emissions_avoided_dominates_long_term_technology_merit, instrumental).
narrative_ontology:cs_reference_frame('5ab2949d-e846-42aa-87fc-f10c713c3233', carbon_budget_timeline_primacy).
narrative_ontology:cs_drift_state('5ab2949d-e846-42aa-87fc-f10c713c3233', post_2015_paris_ratchet_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5ab2949d-e846-42aa-87fc-f10c713c3233', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, solar_and_wind_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewable_finance_investors).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, climate_advocacy_organizations_favoring_fast_rollout).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators_managing_intermittency).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_developers_and_supply_chains).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, ratepayers_in_grids_with_high_curtailment_and_backup_costs).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, communities_dependent_on_displaced_baseload_plants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build projects that can be permitted, financed, and interconnected within a few years, which the velocity test treats as decisive proof of legitimacy. They win subsidy priority, favorable interconnection queue treatment in jurisdictions that adopt the test, and public legitimacy framing over slower-build alternatives regardless of levelized system cost once intermittency is priced in.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, solar_and_wind_developers, beneficiary,
    organized, biographical, mobile, global).

% Sell the mitigation good the velocity test implicitly requires to make fast-deployed variable generation function on real grids. Demand for their product rises in direct proportion to how aggressively the test disqualifies dispatchable alternatives that would otherwise reduce storage need.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Allocate capital toward projects the policy and rating apparatus recognizes as 'legitimate,' capturing returns from a legitimacy label they did not need to earn through system-level performance. Can reallocate globally to wherever the velocity test is adopted or relaxed, insulating them from local grid consequences.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewable_finance_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Author and promote the velocity-primacy framing in IPCC-adjacent advocacy, NGO scorecards, and legislative testimony, treating deployment speed against 2030/2050 milestones as the primary legitimacy test. Benefit reputationally and organizationally from a bright-line test that is easy to communicate and easy to score technologies against.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_advocacy_organizations_favoring_fast_rollout, agenda_setter,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, climate_advocacy_organizations_favoring_fast_rollout, beneficiary).

% Must balance supply and demand in real time on grids increasingly saturated with variable generation admitted under the velocity test. Absorb curtailment costs, procure backup capacity and ancillary services the test does not price, and are blamed for reliability events the underlying legitimacy criterion did not account for. Cannot decline the mix of resources policy and markets hand them.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators_managing_intermittency, payer,
    institutional, immediate, trapped, regional).

% Operate on multi-decade construction and licensing timelines that the velocity test structurally disqualifies regardless of the technology's near-zero operating emissions or dispatchability. Face capital flight and policy marginalization because the test's timeline threshold is calibrated to solar/wind build speed, not to total decarbonization delivered by mid-century.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_developers_and_supply_chains, payer,
    powerful, generational, constrained, national).

% Pay electricity bills that embed the cost of curtailment, backup gas capacity, and grid reinforcement needed to accommodate fast-deployed variable generation. Have no voice in the legitimacy test itself and cannot opt out of the resource mix chosen under it.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, ratepayers_in_grids_with_high_curtailment_and_backup_costs, payer,
    powerless, biographical, trapped, regional).

% Depend economically on coal, gas, or nuclear plants displaced by policy favoring fast-deployable alternatives. Their employment and tax-base concerns are not part of the velocity criterion's accounting and are treated as externalities to the legitimacy question rather than inputs to it.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, communities_dependent_on_displaced_baseload_plants, excluded,
    powerless, biographical, trapped, local).

% Design portfolio standards, auction mechanisms, and interconnection rules that operationalize some reading of technology legitimacy. Take testimony from advocacy groups, utilities, and industry, and can shift the test's weighting between speed, reliability, and precaution through rulemaking.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, national_climate_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__velocity_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__velocity_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legible, bright-line criterion — can this technology physically be built at gigawatt scale before 2030/2050 carbon budget deadlines close — that lets policymakers, financiers, and advocates coordinate investment and permitting priority around technologies capable of near-term emissions reduction, rather than technologies whose payoff arrives too late to matter for the stated targets.
% TRANSFER_FUNCTION: Moves policy priority, subsidy allocation, interconnection queue position, and public legitimacy framing away from technologies with long build timelines (chiefly nuclear) and toward technologies with short build timelines (chiefly solar, wind, storage), while shifting the operational and cost burden of managing the resulting intermittent generation mix onto grid operators and ratepayers.
% ABSENT_VOICES: Nuclear supply chains and long-construction-timeline technologies are structurally disqualified by the criterion's own terms before any life-cycle or total-system-cost comparison occurs; communities dependent on displaced baseload capacity have no forum in which the legitimacy test itself is contested, only in downstream siting or closure proceedings.
% DISAPPEARANCE_RATIONALE: If the velocity-primacy test vanished as the dominant legitimacy criterion, capital and permitting priority would reopen to dispatchable low-carbon technologies on longer timelines, procurement and grid-planning rules would need to re-weight reliability and total system cost rather than deployment speed alone, and current beneficiaries (renewable developers, storage manufacturers, fast-rollout advocacy organizations) would lose a policy lever that currently favors them independent of system-level performance.
% FOUNDING_PROBLEM: The remaining carbon budget for 1.5C/2C pathways is shrinking on a fixed clock (roughly 2030 and 2050 milestones), and technologies whose deployment timelines exceed that clock cannot contribute to meeting it no matter how clean their eventual output — the reading was built to prevent slow-build technologies from being used to justify delay ('we'll decarbonize later with X') while the window to act is closing.
% FOUNDING_PROBLEM_CORROBORATION: IPCC pathway modelers and climate-timeline scientists outside the renewable industry corroborate that deployment speed genuinely matters given a fixed carbon budget. However, grid engineering bodies (e.g. system operators' associations) and independent energy-systems modelers outside both the renewable and nuclear industries dispute that speed-to-deploy is a sufficient legitimacy criterion on its own, arguing that total decarbonization delivered by 2050 depends on reliability-adjusted system cost, not construction speed in isolation — corroboration for the founding problem itself is strong, but corroboration for velocity as the correct SOLE test is contested even among climate scientists.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the velocity criterion does perform a real coordination function (preventing slow technologies from justifying delay under a fixed carbon budget) while also transferring real system costs onto grid operators and ratepayers who have no say in the criterion's design — the tangled_rope profile, not a pure snare, because the coordination function is genuine and not merely cover. Suppression (0.62) captures that alternatives to the velocity framing (life-cycle system-cost comparison, reliability-weighted criteria) are actively marginalized in policy and financing discourse, not merely disfavored on the merits. Theater ratio (0.40) reflects a moderate and rising share of enforcement activity — subsidy design, portfolio standards, scorecard methodology — that functions to defend the velocity framing against reliability-based critique rather than to verify actual decarbonization delivered. All three temporal series share one time grid (0,4,8,12,16,20,24) as required; no metric is asserted at a point another metric skips.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (fast-rollout advocacy organizations) and the beneficiary seats (renewable developers, storage manufacturers, investors), this reading is a rope: a bright-line coordination device that mobilizes capital fast enough to matter against a closing carbon budget. From the grid-operator and nuclear-developer seats, the same structure computes as extractive — a criterion whose threshold was calibrated to one technology family's build speed, imposed as if it were a neutral physical constraint, with reliability costs pushed onto operators and ratepayers who did not write the rule. The engine computes this divergence from the declared power/exit/beneficiary structure; the claimed_type of tangled_rope is authored because both the coordination function and the asymmetric extraction are structurally genuine, not because the seats disagree about it.
 *
 * DIRECTIONALITY LOGIC:
 *   Solar/wind developers, storage manufacturers, and renewable investors are beneficiaries: the criterion directly favors their product category and their exit options (mobile, arbitrage) let them relocate capital wherever the reading is adopted, insulating them from local consequences. Grid operators are targets with trapped exit: they cannot decline the resource mix that policy and markets deliver under this reading and must absorb balancing costs in real time. Nuclear developers are targets despite considerable institutional power, because the criterion's timeline threshold structurally excludes them regardless of bargaining position — power does not buy exit from a bright-line temporal disqualification. Ratepayers and displaced-baseload communities are powerless targets with trapped exit and no voice in the criterion's construction, which is why they appear respectively as payer and excluded rather than as parties to any negotiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a shrinking, fixed carbon budget makes deployment speed genuinely decisive) remains partially live — the timeline math is real — which is why founding_problem_status is authored as contested rather than dead. Classifying this as tangled_rope rather than snare prevents mislabeling a reading that solves a genuine, still-partly-live coordination problem as pure extraction; classifying it as tangled_rope rather than rope prevents treating the real costs imposed on grid operators, ratepayers, and disqualified technology sectors as costless byproducts of legitimate coordination. The corroboration split (climate scientists affirm the timeline problem; grid engineers and independent systems modelers dispute velocity-alone as sufficient) is exactly the evidence that keeps this from resolving cleanly to either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    velocity_vs_total_decarbonization_tradeoff,
    'Does prioritizing deployment velocity over reliability actually maximize cumulative emissions avoided by 2050, or does it produce a resource mix whose backup-gas and curtailment overhead partially offsets the near-term deployment-speed advantage?',
    'Integrated system-cost and emissions modeling comparing cumulative 2024-2050 emissions trajectories under velocity-primacy-driven portfolios versus reliability-weighted portfolios, accounting for backup generation, storage buildout, and curtailment losses.',
    'If velocity-primacy portfolios produce higher cumulative system emissions once backup and curtailment are counted, the reading''s coordination claim weakens substantially and its classification would move toward snare; if velocity-primacy genuinely dominates on cumulative emissions despite the overhead, the coordination function is more strongly vindicated and the reading sits closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(velocity_vs_total_decarbonization_tradeoff, empirical, 'Whether velocity-primacy actually minimizes cumulative system emissions once intermittency costs are counted.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Is the disagreement between velocity_primacy_reading, reliability_primacy_reading, and precautionary_reading a genuine dispute about physical/engineering facts (achievable build rates, grid stability thresholds, failure-mode reversibility), or a dispute about which values (speed vs. stability vs. caution) should govern under irreducible uncertainty about all three?',
    'Structured elicitation distinguishing technical disagreements (which could in principle be resolved by better engineering data) from value disagreements (which cannot be resolved by data alone) among the expert communities backing each reading.',
    'If primarily technical, convergence on a single unified legitimacy criterion becomes possible as data accumulates and the three readings could eventually merge; if primarily a value dispute, the three readings will persist as coexisting, irreducibly plural legitimacy criteria indefinitely, and policy will need to arbitrate between them rather than resolve them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel''s reading-plurality is technical (resolvable) or value-based (irreducible).').

omega_variable(
    grid_operator_capture_of_criterion_design,
    'Could grid operators and reliability engineers who currently bear the costs of the velocity-primacy reading gain enough influence over future portfolio-standard rulemaking to shift the operative legitimacy criterion toward a reliability-weighted or blended standard?',
    'Track regulatory proceedings and portfolio-standard revisions over the next several years for evidence of reliability-cost internalization (e.g., capacity-value discounting for variable resources, explicit system-cost accounting in procurement rules).',
    'If grid operators successfully internalize reliability costs into procurement rules, the velocity-primacy reading''s dominance would erode and its effective extraction on grid operators would fall; if not, the asymmetry this story documents will likely persist or intensify as decarbonization accelerates toward 2030.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_operator_capture_of_criterion_design, empirical, 'Whether grid operators can shift future rulemaking toward internalizing the costs this reading currently externalizes onto them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(tech_tr_t24, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(tech_be_t24, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(tech_su_t24, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__velocity_primacy_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of technology_legitimacy_kernel, decomposed per the ε-invariance principle because 'technology legitimacy for climate mitigation' resolves to structurally distinct claims depending on which criterion (speed, reliability, or precaution) adjudicates legitimacy — each with a different beneficiary/victim set and a different epsilon. velocity_primacy_reading favors fast-build renewables and storage and marginalizes nuclear on timeline grounds; reliability_primacy_reading (sibling story) favors dispatchable/baseload technologies including nuclear and marginalizes unfirmed variable renewables on stability grounds; precautionary_reading (sibling story) favors technologies with bounded, reversible worst-case failure modes, which cuts differently against both fast-build renewables (land/material/mining externalities) and nuclear (long-tail waste and accident risk) depending on how reversibility is operationalized. All three are linked via affects_constraints because policy adoption of one reading structurally changes the legitimacy conditions and resource availability facing technologies evaluated under the others (e.g., capital diverted to renewables under velocity-primacy is capital unavailable to nuclear under reliability-primacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
