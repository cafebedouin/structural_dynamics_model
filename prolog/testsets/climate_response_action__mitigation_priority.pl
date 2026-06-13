% ============================================================================
% CONSTRAINT STORY: climate_response_action__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__mitigation_priority, []).

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
 *   constraint_id: climate_response_action__mitigation_priority
 *   human_readable: Climate Mitigation via Emissions Reduction, Carbon Markets, and Technological Innovation
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of climate response frames the solution
 *   as limiting temperature rise to 2°C through emissions reductions enabled
 *   by technological innovation and carbon markets, while maintaining GDP
 *   growth in high-income nations. This constraint is ONE reading of a
 *   contested kernel: climate_response_action has three active readings
 *   (mitigation_priority, adaptation_priority, degrowth_transformation) that
 *   instantiate fundamentally different problem definitions, beneficiary
 *   structures, and victim allocations. The mitigation reading is
 *   institutionalized at the UNFCCC, dominates global climate finance
 *   allocation, and benefits high-income nations with innovation capacity. It
 *   simultaneously extracts from climate-vulnerable regions (who bear
 *   deferred adaptation costs and residual warming), future generations (who
 *   inherit atmospheric CO2 lock-in), and incumbent energy workers (who face
 *   rapid job displacement). The measurement trajectory shows extraction
 *   rising steeply through year 10 (as carbon markets scale, technology rents
 *   accumulate, and incumbent industries capture transition subsidies), then
 *   plateauing at year 15+ (suggesting the extraction ceiling is reached and
 *   further growth in rents requires axiom-level shifts that the framework
 *   resists). The theater ratio rises throughout, indicating an increasing
 *   proportion of enforcement activity (suppression of degrowth and
 *   adaptation-priority alternatives) relative to functional coordination
 *   (the real abatement work). This is diagnostically consistent with Piton
 *   emergence: the mitigation framework may be transitioning from functional
 *   Tangled Rope (coordination + extraction) to theatrical Piton (extraction
 *   + institutional inertia) as empirical feasibility assumptions are
 *   challenged.
 *
 * KEY AGENTS:
 *   - High-income nations with innovation capacity (EU, UK, US, Japan): agenda-setters, set targets and mechanisms, benefit from technology rents and carbon market operation.
 *   - Carbon-intensive incumbent industries (fossil fuels, cement, steel): payers (compliance costs) and secondary beneficiaries (carbon market access, transition subsidies).
 *   - Technology vendors and carbon market operators: beneficiaries, capture rents on every abatement transaction globally.
 *   - Climate-vulnerable low-income nations (SIDS, sub-Saharan Africa, South Asia): victims, trapped in accepting residual warming + adaptation underfunding.
 *   - Future generations: victims, inherit CO2 lock-in and delayed transformation costs.
 *   - Energy workers and incumbent sector communities: victims, face rapid displacement; identity-locked by professional and regional dependence.
 *   - Degrowth and transformation advocates: excluded, structurally marginalized from UNFCCC; their objection to the growth axiom is not represented in target-setting.
 *   - Adaptation-priority advocates from vulnerable regions: formally present but systematically underfunded and subordinated in policy hierarchy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_action__mitigation_priority, 0.71).
domain_priors:theater_ratio(climate_response_action__mitigation_priority, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__mitigation_priority, "Climate Mitigation via Emissions Reduction, Carbon Markets, and Technological Innovation").
narrative_ontology:topic_domain(climate_response_action__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__mitigation_priority, 'c5b47288-73b0-4221-ae67-b65561ee6eb0').
narrative_ontology:cs_kernel_codification('c5b47288-73b0-4221-ae67-b65561ee6eb0', formalized).
narrative_ontology:cs_authority_grounding('c5b47288-73b0-4221-ae67-b65561ee6eb0', extraction).
narrative_ontology:cs_interpretation_layer_present('c5b47288-73b0-4221-ae67-b65561ee6eb0').
narrative_ontology:cs_reading_relation('c5b47288-73b0-4221-ae67-b65561ee6eb0', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('c5b47288-73b0-4221-ae67-b65561ee6eb0', climate_response_action__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('c5b47288-73b0-4221-ae67-b65561ee6eb0', foundational, technological_substitution_feasibility).
narrative_ontology:cs_axiom_status(technological_substitution_feasibility, holdable).
narrative_ontology:cs_axiom_grounding('c5b47288-73b0-4221-ae67-b65561ee6eb0', technological_substitution_feasibility, empirically_contingent).
narrative_ontology:cs_axiom('c5b47288-73b0-4221-ae67-b65561ee6eb0', foundational, emissions_growth_decoupling_viability).
narrative_ontology:cs_axiom_status(emissions_growth_decoupling_viability, holdable).
narrative_ontology:cs_axiom_grounding('c5b47288-73b0-4221-ae67-b65561ee6eb0', emissions_growth_decoupling_viability, empirically_contingent).
narrative_ontology:cs_axiom('c5b47288-73b0-4221-ae67-b65561ee6eb0', secondary, market_mechanism_efficiency_in_carbon_pricing).
narrative_ontology:cs_axiom_status(market_mechanism_efficiency_in_carbon_pricing, holdable).
narrative_ontology:cs_axiom_grounding('c5b47288-73b0-4221-ae67-b65561ee6eb0', market_mechanism_efficiency_in_carbon_pricing, instrumental).
narrative_ontology:cs_reference_frame('c5b47288-73b0-4221-ae67-b65561ee6eb0', two_degree_temperature_stabilization_pathway).
narrative_ontology:cs_drift_state('c5b47288-73b0-4221-ae67-b65561ee6eb0', contemporary_2024_2030, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c5b47288-73b0-4221-ae67-b65561ee6eb0', '').
narrative_ontology:cs_kernel_id(climate_response_action__mitigation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, high_income_nations_with_innovation_capacity).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_intensive_incumbent_industries).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, technology_vendors_and_carbon_market_operators).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, climate_vulnerable_low_income_nations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, communities_dependent_on_climate_sensitive_sectors).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, workers_in_incumbent_energy_sectors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__mitigation_priority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__mitigation_priority, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.38 (early 2000s: nascent carbon markets, minimal enforcement of mitigation targets, incumbent industries largely unregulated) and rises to 0.68 by year 30 (mature carbon markets, decarbonization mandates in force, rents consolidated in technology and finance sectors). The measurement series reflects the empirical history: Kyoto Protocol (1997) was a weak instrument (low extractiveness); Paris Agreement (2015) established the 2°C framework and incentivized carbon markets (rising extractiveness t=5–10); subsequent COPs (Glasgow 2021, Dubai 2023) have formalized Article 6 (carbon trading mechanisms) and scaled green finance (high extractiveness, rising theater). Suppression rises from 0.52 (degrowth and adaptation alternatives still present in policy conversation) to 0.71 (institutional exclusion and resource subordination now systematic). Theater plateaus at 0.44, indicating that while initial carbon markets pursued real emissions reduction, the constraint increasingly relies on performative carbon accounting (additionality inflation, baseline creep, leakage to non-regulated regions) rather than net global abatement. The convergence of extraction + theater + suppression around year 15–20 signals the constraint's shift from coordination-plus-extraction (Tangled Rope) toward maintenance-through-inertia (Piton trajectory).
 *
 * PERSPECTIVAL GAP:
 *   Seats compute radically differently from this constraint's structure. High-income nations and technology vendors see Tangled Rope: genuine coordination (internalized abatement targets, transparent carbon pricing) plus legitimate extraction (technology rents, market operator fees — the price of efficient abatement). Vulnerable regions and future generations see Snare: no coordination function (they did not set the target, did not consent to the 2°C residual risk), only extraction (deferred adaptation costs, inherited lock-in). Energy workers see identity-locked trap: their professional identity is incompatible with exit from incumbent sectors, but the framework mandates rapid industry closure. Degrowth advocates see masked Snare: the growth axiom is incompatible with the emissions-reduction axiom at scale, but the framework suppresses this contradiction through institutional hierarchy (mitigation finance overweighting adaptation by 3:1, degrowth proposals excluded from UNFCCC). The engine computes these divergent classifications from the structural data: beneficiary/victim declarations + power atoms + exit options produce different d values for each seat, which yield different type assignments. This divergence is precisely the measurement the corpus takes — it is not an error but the point.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income agenda-setters have d near 0.0 (beneficiaries: set the rules, control technology markets, collect rents, exit is easy — arbitrage to other investment vehicles if climate markets collapse). Carbon-intensive incumbent industries have d = 0.50–0.65 (asymmetric: initial compliance costs (CapEx redeployment), but transition subsidies and carbon market access create offsetting benefits; exit is constrained by their productive base being tied to fossil fuels, but they can gradually shift within the incumbent economic structures). Technology vendors have d near 0.0 (beneficiaries: scale globally, face no compliance cost, can exit to non-climate sectors). Vulnerable regions have d near 1.0 (targets: locked in geographically, climate impacts unavoidable, no exit; deferred adaptation costs are external impositions). Future generations have d = 1.0 (pure targets: no participation in the agreement, no exit, trapped by atmospheric CO2 irreversibility). Energy workers have d = 0.80 (high target status: compliance costs borne locally, identity-locked exit prevents arbitrage, region-dependent). Degrowth advocates have d = 0.75 (suppressed: included in discourse but institutionally subordinated, marginal power, constrained by resource allocation rules that favor mitigation-first). The overrides field is not needed — structural derivation from beneficiary/victim + power + exit cleanly produces these values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate ('stabilize global climate at 2°C via emissions reductions, technology, and carbon markets while maintaining growth') was born from real coordination problem recognition (early 2000s: climate change science established, no mechanism to coordinate global abatement). The coordination function was genuine: internalize climate externality, create incentive structures for abatement, provide transfer mechanism to make burden-sharing feasible. However, the founding problem's relationship to the solution is contested (six_questions.founding_problem_status = contested). High-income nations attest the founding problem is live and being solved: cumulative emissions are decoupling from growth in some sectors, renewable capacity is scaling. Vulnerable regions, degrowth advocates, and post-hoc analysis (Kate Raworth, IPCC WG2 on impacts, Hickel et al.) attest the problem is not solved: absolute global emissions continue rising; the solution is deferring rather than eliminating the damages; adaptation is underfunded; the growth axiom prevents reaching net-zero at the speed needed. The measurement trajectory shows extraction rising while coordination function (measured by actual net global emissions reductions) remains ambiguous and contested. Theater rises to 0.44, indicating increasing reliance on carbon accounting manipulations (baseline creep, leakage arbitrage, removal credits that may not be additional) rather than real abatement. The convergence of rising extraction + ambiguous coordination function + rising theater suggests mandatrophy: the constraint's original justification (solve collective-action problem in emissions abatement) has been partially supplanted by a secondary function (distribute rents from the transition to high-income actors and technology vendors). The constraint persists because the beneficiary seats (high-income nations, technology vendors) have accumulated power to maintain it, not because the coordination problem that justified it remains solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_carbon_removal_feasibility,
    'Can carbon removal technology scale to the gigatons per year needed to meet the 2°C target while remaining energy-neutral and economically viable?',
    'Real-world deployment data by 2030: if direct air capture, enhanced weathering, and biochar reach 100+ Mt/year at <$150/ton with energy EROI >5, feasibility is corroborated; if deployment stalls below 10 Mt/year or EROI remains <2, the assumption fails.',
    'This is a foundational axiom of the mitigation_priority reading. If feasibility fails, the 2°C target becomes unreachable without demand reduction, forcing alignment with degrowth_transformation. Classification would shift toward snare (extraction without coordination function). Beneficiary structure collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_carbon_removal_feasibility, empirical, 'Whether carbon removal scales as the mitigation pathway assumes.').

omega_variable(
    decoupling_emissions_from_growth_sustainability,
    'Can high-income economies decouple absolute emissions from GDP growth permanently, or does decoupling reach a boundary beyond which growth requires absolute emissions increase?',
    'National greenhouse gas accounting 2015–2030: if absolute emissions in high-income nations remain stable or decline while GDP grows, decoupling is sustained; if absolute emissions turn upward, decoupling is temporary (Jevons paradox). Consumption-based accounting (embodied emissions in imports) reveals geographic arbitrage.',
    'If decoupling is unsustainable, the growth axiom contradicts the emissions-reduction axiom — the mitigation_priority reading forecloses growth-compatible decarbonization. The reading becomes logically incoherent, forcing reclassification to degrowth_transformation or snare (extraction without coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decoupling_emissions_from_growth_sustainability, empirical, 'Whether growth-emissions decoupling is permanent or temporary.').

omega_variable(
    carbon_market_additionality_and_real_reduction,
    'Do carbon credits represent real, additional emissions reductions, or do they primarily monetize reductions that would have occurred anyway (baseline creep) or displace emissions to non-regulated regions (leakage)?',
    'Meta-analysis of carbon credit projects (forest, methane, renewable energy): comparison of avoided emissions to counterfactual baseline; tracking of investment flows. Audits of Kyoto Protocol mechanisms and Article 6 implementation 2020–2030.',
    'If additionality is low, the constraint''s measured extractiveness (0.68) understates the real extraction — rents are transferred to market operators while net global emissions reduction is minimal. Coordination function collapses; classification shifts to snare. The beneficiary seats (technology vendors, high-income nations) maintain extraction while coordination becomes illusory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_market_additionality_and_real_reduction, empirical, 'Whether carbon markets achieve real or illusory emissions reductions.').

omega_variable(
    intergenerational_residual_risk_distribution,
    'What is the magnitude of climate damage that occurs even under a successful 2°C pathway, and to which regions and populations? Is this residual risk distribution just, and who decided?',
    'IPCC impact assessments for 1.5°C and 2°C scenarios; vulnerability indices for climate-exposed regions; economic loss projections. Ethical frameworks (utilitarian, egalitarian, capability-based) applied to residual impacts. Reconstruction of who participated in deciding 2°C was acceptable.',
    'The mitigation_priority reading axiomatically accepts residual warming. If residual damage is catastrophic for specific populations (island nations, Sahel, South Asia monsoon zones) while minimal for others, the constraint distributes intergenerational and spatial injustice. This omega flags whether the framework launders harm into the future without consent from future generations or vulnerable present populations — raising the question of whether the coordination function is real or a cover story for extractive distribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_residual_risk_distribution, preference, 'Ethical status of distributing climate risk across generations and regions.').

omega_variable(
    mitigation_adaptation_structural_subordination,
    'Is the mitigation-priority framework structurally compatible with simultaneous adaptation prioritization (coexists_with adaptation_priority), or does it subordinate adaptation (influences/forecloses adaptation_priority)?',
    'Global climate finance ratios 2020–2030: if adaptation stabilizes at ≥40% of total climate finance, coexistence is achieved; if adaptation remains at 20–25%, subordination is structural. Track UNFCCC decisions: are adaptation investments conditional on mitigation alignment?',
    'If adaptation is systematically subordinated (as current finance ratios suggest), the mitigation_priority and adaptation_priority readings do not coexist — mitigation forecloses adequate adaptation. The adaptation_priority reading would be more accurately described as ''influenced_by'' (structurally pressured downward) rather than coexistent. This suggests mitigation_priority is using adaptation as a sacrificial victim set, revealing the constraint to be more snare-like than tangled-rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mitigation_adaptation_structural_subordination, empirical, 'Whether mitigation and adaptation can be genuinely co-prioritized.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression (0.71) of degrowth and transformation alternatives structural (institutional barriers, exclusion, fund gatekeeping) or internalized (advocates internalize ''growth is necessary'' even when it contradicts their values)?',
    'Ethnographic analysis of climate advocate communities; survey of private beliefs vs. public statements; post-exit trajectory of advocates who leave mitigation-first institutions. If suppression persists after barriers are removed, internalization is confirmed.',
    'If suppression is primarily structural, removing barriers (reseating degrowth voices in UNFCCC, reallocating finance, changing target-setting rules) could shift constraint persistence. If internalized, advocates carry the suppression cognitively — raising effective suppression above 0.71 and suggesting the constraint is psychological lock-in, making it more snare-like (cognitive closure) than tangled-rope (institutional coordination with distributed extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternatives is externally enforced or self-reinforced.').

omega_variable(
    framework_coherence_under_empirical_drift,
    'As empirical realities depart from the framework''s founding assumptions (emissions-growth decoupling stalls, carbon removal fails to scale, adaptation remains underfunded), does the mitigation-priority framework maintain coherence through internal adjustment, or does it require axiom-level revision?',
    'Compare UNFCCC climate targets across successive COPs (Paris 2015, Glasgow 2021, Dubai 2023): if targets are consistently extended when initial ones are missed, and if carbon budgets are recalculated to accommodate slower abatement, assess whether these are internal adjustments (holding axioms fixed) or axiom-level changes. Track whether the growth axiom, decoupling axiom, or technological feasibility axiom has been formally revised.',
    'If coherence requires perpetual re-calibration without axiom-level change, the constraint may be transitioning from Tangled Rope (functional coordination + extraction) to Piton (extraction maintained by institutional inertia). The mandate (stabilize at 2°C via markets + growth) may have outlived its epistemic foundations, leaving only the extraction machinery (carbon markets, green finance, technology rents) running on momentum. This is the Piton emergence pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_coherence_under_empirical_drift, conceptual, 'Whether the framework maintains coherence or requires continuous patching.').

omega_variable(
    knowledge_committer_axis_kernel_contest,
    'Is the mitigation_priority reading of the climate_response_action kernel a stable, institutionally embedded interpretation, or is it contested at the epistemic level by alternative readings (adaptation_priority, degrowth_transformation) that claim different understandings of what ''climate response'' means?',
    'Document the kernel: the shared commitment to ''respond to climate change'' and the three readings that interpret it differently. Assess whether the mitigation_priority reading is one interpretation among contested alternatives (coexists_with) or whether it has foreclosed the others (forecloses) through institutional power and resource control.',
    'The mitigation_priority reading claims to be THE solution to the climate crisis, but operationally it is one reading that defers, subordinates, and suppresses the others. If the sibling readings are genuinely coexistent (held by different coalitions), the constraint''s persistence depends on the mitigation coalition''s power to maintain hegemony in UNFCCC, finance institutions, and technology markets — revealing the constraint to be more about power and extraction than about solving the founding problem. If the sibling readings are coexistent, they remain alternative moral frameworks, not subordinate errors — making the constraint''s suppression of degrowth and adaptation-priority advocates a form of epistemic injustice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_committer_axis_kernel_contest, conceptual, 'Whether mitigation_priority is a stable reading or one contested by alternative frames.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__mitigation_priority, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t5, climate_response_action__mitigation_priority, theater_ratio, 5, 0.2).
narrative_ontology:measurement(clim_tr_t10, climate_response_action__mitigation_priority, theater_ratio, 10, 0.28).
narrative_ontology:measurement(clim_tr_t15, climate_response_action__mitigation_priority, theater_ratio, 15, 0.35).
narrative_ontology:measurement(clim_tr_t20, climate_response_action__mitigation_priority, theater_ratio, 20, 0.4).
narrative_ontology:measurement(clim_tr_t25, climate_response_action__mitigation_priority, theater_ratio, 25, 0.43).
narrative_ontology:measurement(clim_tr_t30, climate_response_action__mitigation_priority, theater_ratio, 30, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__mitigation_priority, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_be_t5, climate_response_action__mitigation_priority, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(clim_be_t10, climate_response_action__mitigation_priority, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(clim_be_t15, climate_response_action__mitigation_priority, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(clim_be_t20, climate_response_action__mitigation_priority, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(clim_be_t25, climate_response_action__mitigation_priority, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(clim_be_t30, climate_response_action__mitigation_priority, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__mitigation_priority, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(clim_su_t5, climate_response_action__mitigation_priority, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(clim_su_t10, climate_response_action__mitigation_priority, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(clim_su_t15, climate_response_action__mitigation_priority, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(clim_su_t20, climate_response_action__mitigation_priority, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(clim_su_t25, climate_response_action__mitigation_priority, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(clim_su_t30, climate_response_action__mitigation_priority, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__mitigation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% The climate_response_action kernel has three structurally distinct readings: mitigation_priority (this story), adaptation_priority, and degrowth_transformation. They share the founding problem ('climate change requires response') but instantiate different ε values, different victim/beneficiary structures, and different persistence mechanisms because they assume different solutions. Each reading is a separate constraint story with its own classification. The network links document that these three are not independent constraints but readings of a contested kernel — their epsilon values, axioms, and foreclosure relationships are interdependent. Do not collapse these three into one constraint with 'measurement basis parameters'; they are three separate constraints with three separate ε values that differ because they measure different things (mitigation path vs. adaptation capacity vs. transformation feasibility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__mitigation_priority, powerless, 0.88).
constraint_indexing:directionality_override(climate_response_action__mitigation_priority, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
