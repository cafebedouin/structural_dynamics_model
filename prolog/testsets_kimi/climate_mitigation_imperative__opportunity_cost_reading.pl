% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__opportunity_cost_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Climate Mitigation Imperative â Opportunity Cost Reading
 *   domain: energy policy / climate mitigation / technology governance
 *
 * SUMMARY:
 *   This constraint is the opportunity_cost_reading of the contested kernel
 *   climate_mitigation_imperative. It models the imperative as a
 *   capital-efficiency decision rule: under a tight carbon budget, every
 *   mitigation dollar must seek the fastest achievable emissions reduction,
 *   measured as carbon displaced per dollar per year. Nuclear energy, with
 *   its decade-long construction timelines and high upfront capital
 *   intensity, fails this screen and is structurally excluded from green
 *   taxonomies, climate finance, and procurement portfolios. Renewable energy
 *   developers benefit from the resulting capital flow. The constraint
 *   carries genuine coordination valueâdirecting limited funds toward rapid
 *   deploymentâbut asymmetrically extracts from nuclear by treating its
 *   timeline and capital structure as disqualifying rather than as properties
 *   to be priced within a portfolio. The structural delta from sibling
 *   readings is that nuclear sits in the victim set rather than the
 *   beneficiary or neutral set, and the governing metric is speed-per-dollar
 *   rather than portfolio completeness or political-economic transformation.
 *
 * KEY AGENTS:
 *   - fast_renewables_sector: Primary beneficiary (powerful/mobile/global) â captures capital diverted by the speed-per-dollar screen
 *   - nuclear_industry: Primary target (powerful/constrained/global) â bears extraction through exclusion from finance and taxonomies
 *   - climate_finance_gatekeepers: Agenda setter (institutional/analytical/global) â administers the screening criteria
 *   - electric_utilities: Secondary payer (organized/constrained/national) â bears integration and procurement restriction costs
 *   - grid_reliability_planners: Excluded voice (moderate/constrained/national) â would argue for system value but is overridden by finance logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.62).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Climate Mitigation Imperative â Opportunity Cost Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy policy / climate mitigation / technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, 'd96d5129-c04d-4399-97a3-dd4266da9a56').
narrative_ontology:cs_kernel_codification('d96d5129-c04d-4399-97a3-dd4266da9a56', distributed).
narrative_ontology:cs_authority_grounding('d96d5129-c04d-4399-97a3-dd4266da9a56', expertise).
narrative_ontology:cs_interpretation_layer_present('d96d5129-c04d-4399-97a3-dd4266da9a56').
narrative_ontology:cs_reading_relation('d96d5129-c04d-4399-97a3-dd4266da9a56', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('d96d5129-c04d-4399-97a3-dd4266da9a56', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('d96d5129-c04d-4399-97a3-dd4266da9a56', foundational, speed_priority_over_technological_neutrality).
narrative_ontology:cs_axiom_status(speed_priority_over_technological_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('d96d5129-c04d-4399-97a3-dd4266da9a56', speed_priority_over_technological_neutrality, instrumental).
narrative_ontology:cs_axiom('d96d5129-c04d-4399-97a3-dd4266da9a56', foundational, nuclear_opportunity_cost_net_harmful).
narrative_ontology:cs_axiom_status(nuclear_opportunity_cost_net_harmful, holdable).
narrative_ontology:cs_axiom_grounding('d96d5129-c04d-4399-97a3-dd4266da9a56', nuclear_opportunity_cost_net_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('d96d5129-c04d-4399-97a3-dd4266da9a56', capital_efficiency_climate_governance).
narrative_ontology:cs_drift_state('d96d5129-c04d-4399-97a3-dd4266da9a56', renewable_dominance_and_security_return_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d96d5129-c04d-4399-97a3-dd4266da9a56', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, fast_renewables_sector).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, electric_utilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and deploys solar, wind, and battery projects that rank highly on speed-per-dollar metrics. Benefits from capital flows, policy priority, and taxonomy eligibility that the opportunity-cost screening provides. Can shift technology focus or geographic markets if conditions change.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, fast_renewables_sector, beneficiary,
    powerful, biographical, mobile, global).

% Owns and develops large-scale nuclear generation assets with decade-long construction timelines and high upfront capital requirements. Excluded from green taxonomies, climate finance, and procurement screens because it fails the fast-deployment-per-dollar test. Exit means abandoning sunk expertise and regulatory certifications for a sector with few alternative applications.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry, payer,
    powerful, generational, constrained, global).

% Set the screening criteria for green bonds, development finance, and climate portfolios. They translate carbon-budget science into deployability heuristics and exclude nuclear on speed and cost grounds. They do not directly collect the diverted capital but administer the constraint's logic.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_finance_gatekeepers, agenda_setter,
    institutional, biographical, analytical, global).

% Procure generation capacity under regulatory mandates and carbon targets. The opportunity-cost imperative restricts their procurement options, forcing reliance on variable renewable portfolios with associated integration and backup costs. They bear the system-integration costs that the speed-per-dollar screen externalizes.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, electric_utilities, payer,
    organized, biographical, constrained, national).

% Model long-term system reliability and resource adequacy. They would argue that levelized cost and deployment speed omit capacity value and multi-decade carbon lock-in, but their input is overridden by finance-screening logic that prioritizes near-term spend efficiency.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, grid_reliability_planners, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__opportunity_cost_reading, fast_renewables_sector).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of allocating scarce mitigation capital among competing low-carbon technologies under a tight carbon budget and time constraint by prioritizing those that reduce emissions fastest per dollar.
% TRANSFER_FUNCTION: Moves capital, policy support, and taxonomy eligibility away from nuclear energy toward solar, wind, and other fast-deploying alternatives; transfers credibility and regulatory bandwidth to short-cycle technologies.
% ABSENT_VOICES: Nuclear engineers, grid reliability planners, and long-term systems modelers who argue that levelized cost and deployment speed omit capacity value and multi-decade carbon lock-in; they are present in technical discourse but excluded from finance-screening logic.
% DISAPPEARANCE_RATIONALE: If the imperative disappeared, green taxonomies and climate finance screens would no longer exclude nuclear on speed grounds; capital would reallocate across the full low-carbon portfolio, project pipelines and national energy plans would shift, and the renewable sector's preferential access to finance would erode.
% FOUNDING_PROBLEM: The atmosphere has a finite remaining carbon budget and the window to avoid catastrophic warming is narrow; mitigation capital is scarce and must be deployed where it reduces carbon most rapidly.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and the IPCC attest to the carbon budget and timeline from outside the renewable finance sector; independent energy economists corroborate capital scarcity. However, these same sources often dispute that speed-per-dollar is the sole or best metric for allocation.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the constraint diverts large-scale capital away from a proven low-carbon source on a single metric, imposing concentrated losses on the nuclear industry. Suppression (0.58) reflects active enforcement through green taxonomies, ESG screens, and development-finance exclusion; it is not total because some national budgets and export credit agencies still fund nuclear. Theater ratio (0.30) is moderate-low: the coordination function is genuine and the speed metric is sincerely held, but a portion of the enforcement activity serves to protect the renewable investment pipeline rather than purely to optimize carbon reduction. Accessibility collapse (0.45) is moderate: nuclear alternatives are not legally barred but are made economically inaccessible by the finance architecture. Resistance (0.55) is significant: nuclear states, utilities, and some system planners actively contest the constraint. Measurements track rising extraction and suppression as the finance criteria hardened from 2005â2025.
 *
 * PERSPECTIVAL GAP:
 *   From the renewable developer seat, the constraint is legitimate coordination that solves a capital-rationing problem under climate urgency. From the nuclear industry seat, the same structure is actively enforced exclusion that ignores multi-decade carbon displacement and system reliability. The finance-gatekeeper seat experiences it as neutral technocratic optimization. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The fast_renewables_sector is the structural beneficiary: low directionality because the constraint subsidizes its capital access. The nuclear_industry is the structural target: high directionality because the constraint extracts by denying capital. Electric_utilities sit near symmetric: they bear system-integration costs but also receive a clearer procurement mandate. Climate_finance_gatekeepers are near the agenda-setter middle: they do not collect the extraction directly but benefit from institutional relevance and analytical authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling this constraint as a snare (it does coordinate genuine mitigation investment) and prevents mislabeling it as a rope (it actively suppresses a legitimate low-carbon alternative). The founding problemâscarce capital and a tight carbon budgetâremains live, but the solution mechanism has atrophied into a single-metric screen that may reduce total mitigation potential. The temporal measurements show extraction accumulating over the interval as the speed metric became institutionalized, suggesting a drift toward greater extraction without a corresponding increase in coordination value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    system_value_undercounting,
    'Does the carbon-per-dollar-per-year metric systematically undercount system reliability, capacity value, and long-term carbon lock-in?',
    'Integrated resource planning studies comparing full-system portfolios with and without nuclear under identical carbon budgets and reliability constraints.',
    'If system value is materially undercounted, the constraint over-extracts from nuclear and may reduce total mitigation achieved per dollar; this would push classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(system_value_undercounting, empirical, 'Whether speed-per-dollar omits system value').

omega_variable(
    reading_location_uncertainty,
    'Is the disagreement between this reading and its siblings located in empirical facts (deployment speed and cost) or in normative weighting (speed vs reliability vs democratic control)?',
    'Sensitivity analysis of optimal portfolios under varying empirical and ethical weight assumptions; convergence would indicate empirical disagreement, divergence would indicate normative pluralism.',
    'If empirical, one reading will dominate as data improves; if normative, the kernel remains permanently contested and the constraint family is irreducible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_location_uncertainty, conceptual, 'Location of disagreement between kernel readings').

omega_variable(
    finance_criteria_neutrality,
    'Does the opportunity-cost reading reflect an objective economic optimization or an ideological preference against large-scale centralized infrastructure?',
    'Cross-jurisdictional comparison of opportunity-cost rankings; stability across political cultures supports optimization, tracking by ideology supports preference.',
    'If preference, the constraint''s extraction component is larger than its coordination component and the effective suppression is higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(finance_criteria_neutrality, preference, 'Whether the constraint is optimization or ideology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, systems_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel climate_mitigation_imperative. The opportunity_cost_reading models the imperative as a capital-efficiency screen that victimizes nuclear; the portfolio_optimization_reading models it as an all-source maximization problem; the systems_transition_reading models it as a political-economic transformation. They form a constraint family linked by institutional coupling in climate finance and energy policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
