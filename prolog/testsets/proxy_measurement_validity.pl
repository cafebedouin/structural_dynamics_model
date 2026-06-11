% ============================================================================
% CONSTRAINT STORY: proxy_measurement_validity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proxy_measurement_validity, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: proxy_measurement_validity
 *   human_readable: Proxy Measurement Validity in World3 Model Validation
 *   domain: system_dynamics/industrial_ecology/sustainability_science
 *
 * SUMMARY:
 *   The World3 model, developed for The Limits to Growth (1972), uses
 *   aggregated theoretical constructs (pollution units, resource units,
 *   capital units) to represent global system dynamics. These constructs are
 *   not directly observable — 'pollution' is defined functionally as anything
 *   that degrades environmental quality, 'resources' as anything that enables
 *   production. Empirical validation requires mapping these constructs to
 *   measurable proxies: atmospheric CO2 concentration for pollution, fossil
 *   fuel consumption for resources, industrial production indices for
 *   capital. The proxy-construct gap — the degree to which available
 *   empirical proxies structurally represent the World3 sector aggregates —
 *   is the constraint under analysis. This gap is claimed to be a mountain
 *   (an irreducible measurement problem inherent to system dynamics modeling)
 *   but carries identifiable beneficiaries (fossil fuel policy frameworks
 *   that benefit from CO2-centric climate discourse, carbon accounting
 *   industries that monetize CO2 measurement). The presence of beneficiaries
 *   triggers the false summit detector: is this a genuine natural law of
 *   measurement, or a contingent institutional arrangement that benefits
 *   specific actors?
 *
 * KEY AGENTS:
 *   - Empirical Researcher: Primary target (powerless/trapped) — must use available proxies; no alternative observables exist at global scale with sufficient temporal coverage
 *   - National Statistical Agency: Secondary target (moderate/constrained) — could develop alternatives but faces prohibitive coordination costs
 *   - IPCC Assessment Process: Institutional actor (institutional/mobile) — could commission new measurement programs but experiences the gap as structural
 *   - Fossil Fuel Policy Frameworks: Primary beneficiary (institutional/arbitrage) — benefit from CO2-centric discourse that backgrounds other pollution flows and resource depletion dynamics
 *   - Carbon Accounting Industry: Secondary beneficiary (institutional/arbitrage) — monetizes CO2 measurement infrastructure; expansion to other pollutants would require new methodologies
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the gap as an irreducible feature of system dynamics modeling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proxy_measurement_validity, 0.08).
domain_priors:suppression_score(proxy_measurement_validity, 0.12).
domain_priors:theater_ratio(proxy_measurement_validity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proxy_measurement_validity, extractiveness, 0.08).
narrative_ontology:constraint_metric(proxy_measurement_validity, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(proxy_measurement_validity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(proxy_measurement_validity, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(proxy_measurement_validity, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proxy_measurement_validity, mountain).
narrative_ontology:human_readable(proxy_measurement_validity, "Proxy Measurement Validity in World3 Model Validation").
narrative_ontology:topic_domain(proxy_measurement_validity, "system_dynamics/industrial_ecology/sustainability_science").

domain_priors:emerges_naturally(proxy_measurement_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proxy_measurement_validity, fossil_fuel_focused_policy_frameworks).
narrative_ontology:constraint_beneficiary(proxy_measurement_validity, carbon_accounting_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(proxy_measurement_validity, ipcc_assessment_process).
narrative_ontology:constraint_beneficiary(proxy_measurement_validity, fossil_fuel_policy_frameworks).
narrative_ontology:constraint_victim(proxy_measurement_validity, empirical_researcher).
narrative_ontology:constraint_victim(proxy_measurement_validity, national_statistical_agency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces the structural constraint that World3 sector aggregates have no direct empirical referents. Must use available proxies (CO2, fossil fuels, IIP) despite knowing they capture only a fraction of the theoretical constructs. Cannot develop alternatives within a single research project — the data infrastructure required for comprehensive environmental accounts would take decades and international coordination. Bears the epistemic cost of the proxy-construct gap: validation studies are ambiguous because the coverage fraction is unknown.
narrative_ontology:constraint_stakeholder(proxy_measurement_validity, empirical_researcher, payer,
    powerless, immediate, trapped, global).

% Constrained by international reporting standards (UNFCCC for CO2, IEA for fossil fuels, UN for industrial output) and historical data infrastructure. Could in principle develop alternative indicators, but the cost and coordination burden are prohibitive. The proxy-construct gap is experienced as a fixed feature of the measurement landscape within a career timescale. Bears the coordination cost of maintaining legacy measurement systems that may not align with contemporary policy needs.
narrative_ontology:constraint_stakeholder(proxy_measurement_validity, national_statistical_agency, payer,
    moderate, biographical, constrained, national).

% Mobile in principle — could commission new measurement programs through UNFCCC or WMO — but experiences the proxy-construct gap as a structural feature of Earth system science. CO2 is not chosen as a pollution proxy because it benefits the IPCC; it is chosen because it is measurable, well-mixed in the atmosphere, and causally linked to radiative forcing. Benefits incidentally from the availability of high-quality CO2 data that enables climate assessment, but does not defend the proxy infrastructure against alternatives.
narrative_ontology:constraint_stakeholder(proxy_measurement_validity, ipcc_assessment_process, beneficiary,
    institutional, generational, mobile, global).

% Benefits structurally from a measurement infrastructure that privileges CO2 (a fossil fuel combustion product) over other pollution flows (toxics, nutrient loading, habitat destruction) and fossil fuel consumption over other resource depletion dynamics (water, minerals, soil). The CO2-centric climate discourse backgrounds non-fossil environmental degradation, which reduces regulatory pressure on non-carbon impacts of fossil fuel extraction and combustion. This benefit is not actively extracted — it accrues passively from the measurement landscape — but it is a real structural advantage. Would likely resist comprehensive environmental accounts that expand the measurement frame beyond carbon.
narrative_ontology:constraint_stakeholder(proxy_measurement_validity, fossil_fuel_policy_frameworks, beneficiary,
    institutional, biographical, arbitrage, global).

% Monetizes CO2 measurement infrastructure through carbon credits, offsets, and compliance markets. Benefits from the proxy-construct gap because CO2 is the only pollution flow with a mature global accounting system — expansion to other pollutants would require new methodologies, standards, and verification systems, which would dilute the carbon accounting industry's expertise advantage. This benefit is structural rather than actively extracted, but the industry has an interest in defending CO2-centric measurement against comprehensive environmental accounts.
narrative_ontology:constraint_stakeholder(proxy_measurement_validity, carbon_accounting_industry, beneficiary,
    institutional, biographical, arbitrage, global).

% Sees the proxy-construct gap as a structural feature of system dynamics modeling. Aggregated theoretical constructs (pollution, resources, capital) are not directly observable because they are defined functionally rather than physically. Proxies are necessary because the constructs are designed to capture system behavior, not to map one-to-one with observables. The gap is irreducible in the sense that any system dynamics model with aggregated sectors will face the same measurement problem. The question is whether the gap is a genuine natural law (no one benefits from its existence) or a false summit (beneficiaries defend it against alternatives).
narrative_ontology:constraint_stakeholder(proxy_measurement_validity, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enable empirical validation of World3 model by mapping theoretical constructs (pollution units, resource units, capital units) to measurable proxies (CO2 concentration, fossil fuel consumption, industrial production indices). The coordination problem is that World3 aggregates are defined functionally (pollution = environmental degradation, resources = production enablers) and have no direct physical referents, so validation requires agreed-upon proxies.
% TRANSFER_FUNCTION: Epistemic cost flows from empirical researchers (who bear the ambiguity of unknown coverage fractions) to the measurement infrastructure (which privileges certain flows — CO2, fossil fuels — over others). Structural advantage flows to fossil fuel policy frameworks and carbon accounting industries, which benefit from a measurement landscape that privileges their domain. The transfer is not a direct extraction (no one is paying rents to the beneficiaries) but a passive accumulation of advantage from the proxy infrastructure.
% ABSENT_VOICES: Researchers working on non-carbon environmental flows (toxics, biodiversity, nutrient cycles) and non-fossil resource depletion (water, soil, minerals) are underrepresented in World3 validation discourse because their domains lack the measurement infrastructure that CO2 and fossil fuels have. These researchers would object that the proxy-construct gap is not a natural law but a contingent feature of measurement investment priorities — CO2 has high-quality global data because climate policy drove measurement investment, not because it is inherently more measurable than other pollutants.
% DISAPPEARANCE_RATIONALE: If the proxy-construct gap disappeared overnight (i.e., if comprehensive environmental accounts with full sectoral coverage became available at zero cost), the world would rearrange only minimally. Researchers would use the better data, but the underlying system dynamics would not change. The gap is a measurement constraint, not a coordination mechanism that organizes behavior. The structural advantage to fossil fuel policy frameworks and carbon accounting industries would disappear, but this is a second-order effect — the primary function of the constraint is epistemic (enable validation), not extractive (transfer advantage).
% FOUNDING_PROBLEM: Enable empirical validation of system dynamics models with aggregated theoretical constructs. World3 was developed in the early 1970s when global environmental data was sparse. The founding problem was: how do we test a model whose variables (pollution units, resource units) have no direct empirical referents? The solution was to use the best available proxies (CO2 for pollution, fossil fuels for resources) and acknowledge the coverage ambiguity. This problem is still live — system dynamics models still use aggregated constructs, and proxies are still necessary.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by the system dynamics modeling community (Meadows et al. 1972, 2004; Randers 2012) and by critics of World3 aggregation (Nordhaus 1992; Pielke 2010). Both proponents and critics agree that the proxy-construct gap is a real measurement challenge. The disagreement is about whether the gap is irreducible (proponents: aggregation is necessary for tractability) or avoidable (critics: disaggregation would reduce ambiguity). The problem's liveness is corroborated by ongoing World3 validation studies (Turner 2008, 2014; Herrington 2020) that continue to use CO2 and fossil fuel proxies because no better alternatives exist at global scale.
narrative_ontology:disappearance_verdict(proxy_measurement_validity, world_unchanged).
narrative_ontology:founding_problem_status(proxy_measurement_validity, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPIRICAL RESEARCHER (MOUNTAIN) — Faces the structural constraint that World3 sector aggregates (pollution units, resource units, capital units) are theoretical constructs with no direct empirical referents. Available proxies (CO2 for pollution, fossil fuels for resources, IIP for industrial output) are the only measurable quantities that map to these aggregates. The gap between construct and proxy is an irreducible measurement problem, not a policy choice. Trapped by data availability; no alternative observables exist at global scale with sufficient temporal coverage.
constraint_indexing:constraint_classification(proxy_measurement_validity, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NATIONAL STATISTICAL AGENCY (MOUNTAIN) — Constrained by international reporting standards and historical data infrastructure. Could in principle develop alternative indicators, but the cost and coordination burden are prohibitive. The proxy-construct gap is experienced as a fixed feature of the measurement landscape. Biographical time horizon: within a career, the measurement infrastructure is unchangeable.
constraint_indexing:constraint_classification(proxy_measurement_validity, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IPCC ASSESSMENT PROCESS (MOUNTAIN) — Mobile in principle (could commission new measurement programs) but experiences the proxy-construct gap as a structural feature of Earth system science. CO2 is not chosen as a pollution proxy because it benefits anyone; it is chosen because it is measurable, well-mixed in the atmosphere, and causally linked to radiative forcing. The gap between 'pollution units' (a World3 aggregate) and CO2 concentration (an observable) reflects the difference between a theoretical construct and a physical quantity. Generational time horizon: even with decades of effort, some aggregates remain unmeasurable.
constraint_indexing:constraint_classification(proxy_measurement_validity, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — The proxy-construct gap is a structural feature of system dynamics modeling: aggregated theoretical constructs (pollution, resources, capital) are not directly observable. Proxies are necessary because the constructs are defined functionally (pollution = anything that degrades environmental quality; resources = anything that enables production) rather than physically. The gap is irreducible because the constructs are designed to capture system behavior, not to map one-to-one with observables. This is not a false summit — no identifiable agent benefits from the gap's existence, and the gap would persist regardless of institutional arrangements.
constraint_indexing:constraint_classification(proxy_measurement_validity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proxy_measurement_validity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(proxy_measurement_validity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(proxy_measurement_validity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(proxy_measurement_validity, ExtMetricName, E),
    domain_priors:suppression_score(proxy_measurement_validity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(proxy_measurement_validity),
    narrative_ontology:constraint_metric(proxy_measurement_validity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(proxy_measurement_validity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(proxy_measurement_validity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The proxy-construct gap imposes minimal direct costs on researchers or policymakers. The 'extraction' is subtle: fossil fuel policy frameworks benefit from a measurement infrastructure that privileges CO2 over other environmental flows, but this benefit is not actively extracted from victims — it is a structural advantage that accrues from the measurement landscape. The low value reflects that most actors experience the gap as a constraint, not as a transfer. Suppression (0.12): Very low. No active enforcement prevents researchers from developing alternative proxies. The barriers are technical (data availability, measurement cost) and coordinative (international reporting standards), not coercive. Researchers are free to propose alternatives; the constraint is that alternatives are expensive and difficult, not that they are forbidden. Theater ratio (0.15): Very low. Proxy measurement is functional, not performative. CO2 monitoring networks, fossil fuel production statistics, and industrial output indices serve real epistemic purposes. The theater component is the ritualistic citation of these proxies in World3 validation studies without acknowledging the coverage fraction ambiguity — but this is a minor performative element in an otherwise functional measurement system. Accessibility collapse (0.88): Very high. Once the measurement problem is understood, alternatives do not become more accessible — the gap between theoretical constructs and observables is a structural feature of the modeling approach. A researcher who understands that 'pollution units' are not directly measurable does not thereby gain access to better proxies. Resistance (0.05): Very low. The constraint meets almost no active resistance because it is widely perceived as a natural feature of measurement, not as a constructed arrangement. The minimal resistance comes from critics who argue that World3 aggregation obscures critical dynamics, but this is methodological critique, not organized opposition.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all four perspectives classify as mountain. The empirical researcher (powerless/trapped/immediate) sees an unchangeable measurement constraint. The national statistical agency (moderate/constrained/biographical) sees an unchangeable infrastructure within a career timescale. The IPCC (institutional/mobile/generational) sees a structural feature of Earth system science even at generational timescales. The analytical observer (analytical/analytical/civilizational) sees an irreducible gap between theoretical constructs and observables. The uniformity is diagnostic: either this is a genuine natural law (all perspectives correctly identify it as mountain), or it is a false summit (all perspectives have been captured by the same naturalization). The false summit detector evaluates this by checking whether the declared beneficiaries would resist alternatives if they became available. If fossil fuel policy frameworks and carbon accounting industries would defend the current proxy infrastructure against comprehensive environmental accounts, the mountain classification is a false summit. If they are indifferent, the mountain is genuine.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chain produces low d values for all agents because the constraint is experienced as a structural feature of measurement rather than as an extractive arrangement. The empirical researcher (powerless/trapped) would normally have high d (victim + trapped exit → high extraction), but this constraint has no declared victims — the researcher is constrained by data availability, not extracted from by an institutional actor. The IPCC (institutional/mobile) is declared as a beneficiary, but the benefit is incidental (they use the available proxies because they are the best available, not because they profit from the gap). The fossil fuel policy frameworks and carbon accounting industry are the only agents with clear structural benefits, but even they do not actively extract from the gap — they passively benefit from a measurement infrastructure that privileges their domain. The low extractiveness and suppression values, combined with the absence of declared victims, produce low effective extraction across all perspectives. The mountain classification is uniform because the constraint is genuinely experienced as immutable by all agents at their respective time horizons.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy because it is not a coordination mechanism that has outlived its function. The proxy-construct gap is not a solution to a problem that no longer exists — it is a structural feature of the relationship between theoretical constructs and empirical observables. The gap persists because World3 aggregates are defined functionally (pollution = environmental degradation, resources = production enablers) rather than physically, and functional definitions do not map one-to-one with measurable quantities. The constraint's mandate (enable empirical validation of World3) is still live, and the proxies still serve that mandate as well as any available alternative. The question is whether the mandate itself is legitimate (does World3 aggregation obscure critical dynamics?) and whether the beneficiaries defend the current proxy infrastructure against better alternatives (false summit test).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sectoral_coverage_ambiguity,
    'What fraction of World3 ''pollution units'' does atmospheric CO2 concentration actually represent? Is CO2 10% of total pollution load, 50%, or 90%?',
    'Decomposition of World3 pollution aggregate into constituent flows (greenhouse gases, particulates, toxics, nutrient loading, etc.) with independent measurement of each; comparison of CO2 radiative forcing contribution to total environmental degradation metrics',
    'If CO2 represents <30% of pollution units: World3 validation studies using CO2 as a proxy are measuring a minority component and may miss majority dynamics. If >70%: CO2 is a reasonable proxy and the gap is less severe than critics claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sectoral_coverage_ambiguity, empirical, 'Fraction of World3 pollution units captured by CO2 proxy').

omega_variable(
    resource_aggregation_validity,
    'Does the World3 ''resource'' aggregate meaningfully unify fossil fuels, minerals, water, and land, or does aggregation obscure critical substitution and depletion dynamics?',
    'Disaggregated World3 runs with separate resource stocks; comparison of aggregated vs disaggregated model behavior; empirical analysis of historical substitution patterns',
    'If aggregation is valid: fossil fuel consumption is a reasonable proxy for total resource depletion. If invalid: the World3 resource sector conflates non-substitutable stocks and the proxy measurement problem is compounded by a modeling problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_aggregation_validity, conceptual, 'Validity of World3 resource sector aggregation').

omega_variable(
    false_summit_beneficiary_test,
    'Do the declared beneficiaries (fossil fuel policy frameworks, carbon accounting industry) actually benefit from the proxy-construct gap, or are they incidental to a genuine measurement constraint?',
    'Counterfactual analysis: if alternative proxies (e.g., comprehensive environmental accounts, material flow analysis) were available at equivalent cost and coverage, would fossil fuel policy frameworks resist adoption? Historical analysis of resistance to expanded environmental accounting.',
    'If beneficiaries would resist alternatives: the mountain classification is a false summit and the constraint is actually a tangled rope (coordination + extraction). If beneficiaries are indifferent: the mountain classification is correct and the gap is a genuine natural law of measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_test, empirical, 'Test of whether beneficiaries defend the proxy-construct gap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proxy_measurement_validity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proxy_meas_tr_t0, proxy_measurement_validity, theater_ratio, 0, 0.1).
narrative_ontology:measurement(proxy_meas_tr_t15, proxy_measurement_validity, theater_ratio, 15, 0.12).
narrative_ontology:measurement(proxy_meas_tr_t30, proxy_measurement_validity, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(proxy_meas_be_t0, proxy_measurement_validity, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(proxy_meas_be_t15, proxy_measurement_validity, base_extractiveness, 15, 0.07).
narrative_ontology:measurement(proxy_meas_be_t30, proxy_measurement_validity, base_extractiveness, 30, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(proxy_measurement_validity, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proxy_measurement_validity, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is not part of a decomposed family. The proxy-construct gap is a single structural phenomenon with a single extractiveness value. Alternative observables (comprehensive environmental accounts, material flow analysis, disaggregated resource stocks) would be different constraints with their own stories, not alternative measurements of this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
