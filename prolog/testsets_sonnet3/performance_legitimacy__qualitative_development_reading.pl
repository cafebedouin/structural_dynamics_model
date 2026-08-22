% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: Performance Legitimacy — 'High-Quality Development' (Innovation/Sustainability/Efficiency) Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint captures one reading of a contested legitimacy kernel:
 *   the claim that state authority should be judged by its success at
 *   structural transformation toward 'high-quality development' — innovation,
 *   sustainability, efficiency — rather than by raw growth rates, employment
 *   gains, technological self-sufficiency, or livelihood delivery. Under this
 *   reading, the legitimacy test shifts from 'is GDP still expanding' or 'is
 *   daily life improving' to 'is the economy moving up the value chain.' This
 *   reframing has real coordination content — an economy facing diminishing
 *   returns from extensive investment does benefit from redirected capital
 *   and attention — but it also reassigns who counts as productive and who
 *   counts as obsolete, generating a distinct beneficiary/victim structure
 *   from the sibling readings of the same kernel.
 *
 * KEY AGENTS:
 *   - central_planning_technocrats: sets the KPI framework (institutional/analytical) — administers the metric shift without bearing transition costs
 *   - high_tech_national_champions: primary beneficiary (organized/arbitrage) — captures preferential capital and can relocate across competing innovation zones
 *   - state_backed_venture_capital_funds: beneficiary (institutional/mobile) — collects fees and equity regardless of downstream employment outcomes
 *   - traditional_manufacturing_firms and property_dependent_local_governments: primary payers (moderate/constrained-trapped) — reclassified as obsolete, lose access to credit and legitimacy
 *   - displaced_manufacturing_workers and smaller_industrial_cities: diffuse victims (powerless/trapped) — bear the employment and fiscal costs of the reclassification with no fast route into the new legitimacy metric
 *   - comparative_political_economists: analytical observer — evaluates whether the metric shift reflects genuine transformation or opportunistic redefinition timed to declining growth rates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.62).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.58).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Performance Legitimacy — 'High-Quality Development' (Innovation/Sustainability/Efficiency) Reading").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, 'b5090b79-74ea-4163-823d-a1a2f606a417').
narrative_ontology:cs_kernel_codification('b5090b79-74ea-4163-823d-a1a2f606a417', distributed).
narrative_ontology:cs_authority_grounding('b5090b79-74ea-4163-823d-a1a2f606a417', practice).
narrative_ontology:cs_interpretation_layer_present('b5090b79-74ea-4163-823d-a1a2f606a417').
narrative_ontology:cs_reading_relation('b5090b79-74ea-4163-823d-a1a2f606a417', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5090b79-74ea-4163-823d-a1a2f606a417', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('b5090b79-74ea-4163-823d-a1a2f606a417', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('b5090b79-74ea-4163-823d-a1a2f606a417', foundational, value_chain_position_over_output_volume).
narrative_ontology:cs_axiom_status(value_chain_position_over_output_volume, holdable).
narrative_ontology:cs_axiom_grounding('b5090b79-74ea-4163-823d-a1a2f606a417', value_chain_position_over_output_volume, instrumental).
narrative_ontology:cs_axiom('b5090b79-74ea-4163-823d-a1a2f606a417', foundational, tolerable_growth_deceleration_for_structural_upgrading).
narrative_ontology:cs_axiom_status(tolerable_growth_deceleration_for_structural_upgrading, holdable).
narrative_ontology:cs_axiom_grounding('b5090b79-74ea-4163-823d-a1a2f606a417', tolerable_growth_deceleration_for_structural_upgrading, empirically_contingent).
narrative_ontology:cs_reference_frame('b5090b79-74ea-4163-823d-a1a2f606a417', extensive_investment_led_growth_model).
narrative_ontology:cs_drift_state('b5090b79-74ea-4163-823d-a1a2f606a417', post_overcapacity_recognition_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b5090b79-74ea-4163-823d-a1a2f606a417', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_national_champions).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_venture_capital_funds).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, innovation_zone_administrators).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, central_planning_technocrats).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_firms).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, displaced_manufacturing_workers).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, smaller_industrial_cities).
narrative_ontology:constraint_vindicates(performance_legitimacy__qualitative_development_reading, innovation_driven_growth_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy__qualitative_development_reading, structural_upgrading_necessity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the KPI framework by which provincial and municipal officials are evaluated, replacing raw GDP targets with indicators for patent filings, R&D intensity, energy efficiency, and 'new productive forces.' Administers promotion and resource allocation according to this framework; can revise the metrics but bears none of the transition costs directly.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_planning_technocrats, agenda_setter,
    institutional, generational, analytical, national).

% Receive preferential financing, tax breaks, land grants, and procurement guarantees because their sector now defines what counts as legitimate growth. Can relocate between innovation zones competing to host them and play jurisdictions against each other for better terms.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_national_champions, beneficiary,
    organized, generational, arbitrage, national).

% Government guidance funds and state-linked VC vehicles are massively expanded to channel capital toward semiconductor, AI, and green-tech startups. They collect management fees and equity stakes regardless of downstream employment outcomes, and can redeploy capital across regions as the innovation narrative shifts.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_venture_capital_funds, beneficiary,
    institutional, generational, mobile, national).

% Low-margin, labor-intensive producers now classified as 'outdated production capacity' subject to closure quotas, tightened credit, and environmental compliance costs designed to force exit or consolidation. Cannot easily pivot to high-tech classification and lack the political access enjoyed by innovation-sector firms.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_firms, payer,
    moderate, biographical, constrained, regional).

% Municipalities whose fiscal base rested on land sales and construction-linked GDP now find that growth model politically discredited, while national transfer formulas increasingly weight innovation metrics they structurally cannot produce. Locked into existing debt obligations incurred under the old growth model with no fast route to the new legitimacy metric.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    moderate, biographical, trapped, regional).

% Workers in shuttered or downsized traditional manufacturing plants face retraining programs pitched toward high-tech employment that absorbs only a fraction of displaced labor. Geographic and skill immobility leaves many without a realistic route into the innovation-sector jobs the new legitimacy narrative celebrates.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, displaced_manufacturing_workers, payer,
    powerless, biographical, trapped, local).

% Second- and third-tier cities built around single traditional industries lack the university base, talent pool, or capital access to credibly compete for innovation-zone designation. Their local economies contract as investment concentrates in a handful of favored high-tech hubs.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, smaller_industrial_cities, payer,
    powerless, generational, trapped, local).

% Would argue that the 'sustainability' component of the legitimacy claim is often subordinated to industrial-policy imperatives (semiconductor fabs, battery plants) with their own significant environmental footprint. Rarely consulted in setting the qualitative-development KPI weights, which are dominated by industrial-upgrading rather than ecological metrics.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, environmental_and_sustainability_advocates, excluded,
    powerless, generational, constrained, national).

% Study whether the shift from growth-rate to quality-of-growth metrics represents genuine structural transformation or a redefinition of legitimacy criteria timed to a period when raw growth rates were becoming harder to sustain. Have access to regional GDP, patent, and employment data across the transition period.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, comparative_political_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__qualitative_development_reading, diffuse).
narrative_ontology:fixing_cost_class(performance_legitimacy__qualitative_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Redirects capital, land, credit, and administrative attention away from commoditized, low-margin production toward higher value-added innovation sectors, in principle solving a genuine problem: an economy that has exhausted the easy gains of factor-accumulation growth needs a coordinated push into productivity-driven, innovation-led sectors to avoid stagnation.
% TRANSFER_FUNCTION: Moves financing, land, subsidized credit, and political preferment from traditional manufacturing and property-dependent local governments toward high-tech firms, state-backed venture capital vehicles, and the localities that host them; moves employment risk from the innovation ecosystem onto displaced manufacturing workers and shrinking industrial cities.
% ABSENT_VOICES: Displaced manufacturing workers and residents of non-favored industrial cities have no direct channel into how the qualitative-development KPI weights are set; environmental advocates are formally invoked in the 'sustainability' language but structurally marginal to the metrics that actually govern promotion and investment decisions.
% DISAPPEARANCE_RATIONALE: Central planners and high-tech beneficiaries would say the world rearranges catastrophically — capital flows back toward unproductive construction and low-value manufacturing, forfeiting the structural transformation. Traditional manufacturing regions and displaced workers would say very little rearranges for them either way: the legitimacy narrative changed, but their exposure to closures, credit tightening, and unfavorable KPI weighting does not depend on which growth theory motivates it, only on the underlying capital reallocation continuing.
% FOUNDING_PROBLEM: Extensive-growth strategies (fixed-asset investment, real estate, low-value-added export manufacturing) were producing diminishing returns, overcapacity, unsustainable local government debt tied to land sales, and growing recognition that raw GDP growth rate was becoming an unreliable and increasingly costly legitimacy metric.
% FOUNDING_PROBLEM_CORROBORATION: Central planning technocrats and high-tech sector representatives attest the structural-transformation problem remains live and urgent, citing productivity gaps with frontier economies. Independent economists studying regional fiscal data and comparative political economists outside the beneficiary set corroborate that diminishing returns to extensive growth were real, but note the qualitative-development framing conveniently arrived just as growth-rate targets were becoming politically embarrassing to sustain — suggesting the legitimacy criterion shifted partly because the old one was failing, not solely because the new one was independently more valid.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, contested).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 (rising from 0.38) because the qualitative-development framework does real coordination work early (redirecting genuinely misallocated capital) but increasingly functions to justify concentrated benefit flows to favored sectors and zones as the framework matures and its metrics become the sole channel for political and fiscal legitimacy. Suppression at 0.58 reflects the coercive apparatus behind reclassification — closure quotas, credit tightening, environmental enforcement selectively applied to 'outdated capacity' — which is a real structural lever, not mere rhetoric. Theater ratio at 0.40 captures that a meaningful share of 'sustainability' and 'innovation' reporting has become compliance performance (patent-filing quotas, R&D-spend box-checking) disconnected from genuine technological upgrading. Accessibility collapse is moderate (0.50): traditional-sector actors are not fully foreclosed from adaptation, but the path is narrow and capital-intensive. Resistance is moderate-high (0.55): affected local governments and displaced-worker constituencies do generate real political friction, visible in local protests and provincial pushback against closure quotas.
 *
 * DIRECTIONALITY LOGIC:
 *   High-tech national champions and state-backed VC funds sit near the full-beneficiary end: they did not design the legitimacy shift but they are its structural collectors, with mobile or arbitrage-grade exit letting them play jurisdictions against each other. Central planning technocrats administer the framework without bearing its costs, placing them at the agenda-setter position rather than a payer or beneficiary proper. Traditional manufacturing firms and property-dependent local governments are structural targets: their prior legitimacy basis has been redefined as obsolete, and their exit options are constrained or trapped by existing debt, capital stock, and workforce composition. Displaced workers and smaller industrial cities are the most trapped: powerless, local-scope, biographical-to-generational time horizon, with no meaningful arbitrage against the new metric regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — diminishing returns to extensive growth and unsustainable local-government land-sale dependency — is corroborated as genuinely live by economists outside the beneficiary set, which argues against pure mandatrophy: this is not simply an old mandate persisting past its function. However, the founding-problem status is authored as 'contested' rather than clearly 'live' because independent observers note the qualitative-development framing arrived suspiciously in step with the old growth-rate metric's political embarrassment, suggesting partial motivated reframing layered onto a real underlying problem. The tangled_rope classification (rather than pure snare) reflects this: there is a genuine coordination function (redirecting capital from overcapacity sectors) operating through the same structure that extracts from traditional manufacturing and its dependent localities — both must be true simultaneously for this to be a tangled rope rather than either a rope or a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_transformation_vs_relabeled_capital_allocation,
    'Is the qualitative-development metric shift a genuine response to diminishing returns from extensive growth, or a relabeling exercise that redirects the same capital-allocation authority toward a new set of politically favored beneficiaries once the old growth-rate metric became harder to hit?',
    'Track total factor productivity and genuine R&D output (not just spending or patent-filing volume) in favored sectors against a counterfactual where growth-rate targets had continued; compare against the timing of growth-rate slowdown to see if the metric shift precedes or follows the point where old-metric legitimacy became unsustainable.',
    'If TFP gains are substantial and precede growth-rate difficulty, this supports a genuine rope-like coordination reading; if the shift closely tracks growth-rate failure and TFP gains are modest relative to capital deployed, this supports treating the reading as closer to a snare wearing coordination language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_transformation_vs_relabeled_capital_allocation, empirical, 'Whether the innovation-legitimacy shift reflects real structural transformation or opportunistic metric substitution.').

omega_variable(
    sustainability_component_substantiveness,
    'Does the ''sustainability'' element of high-quality development carry independent weight in resource allocation decisions, or is it subordinated entirely to industrial-upgrading and innovation metrics with their own environmental footprint (e.g. semiconductor fabs, battery gigafactories)?',
    'Compare environmental-impact-adjusted resource allocation against nominal ''sustainability'' KPI weighting in provincial evaluation formulas; audit whether high-tech projects receive environmental compliance exemptions unavailable to traditional manufacturing.',
    'If sustainability is substantively weighted, the coordination function is broader than industrial policy alone; if it is nominal, the ''sustainability'' language functions primarily as legitimation cover for industrial-policy capital reallocation, strengthening the tangled_rope/snare-adjacent reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_component_substantiveness, empirical, 'Whether sustainability functions as a real constraint or as legitimating rhetoric for industrial upgrading.').

omega_variable(
    kernel_reading_selection_stability,
    'Is the choice of qualitative_development_reading over the sibling readings (quantitative_growth, techno_nationalist, livelihood_security) a stable structural commitment, or does the central authority selectively invoke whichever reading is most favorable at a given moment (e.g., reverting to growth-rate emphasis during downturns, techno-nationalist emphasis during geopolitical tension)?',
    'Track official rhetoric and resource-allocation formulas across multiple economic cycles and geopolitical episodes to see whether the qualitative-development framing persists or is selectively suspended.',
    'If the reading is stably held even through downturns, this supports treating it as a genuine, durable legitimacy commitment; if it is opportunistically swapped for other readings under pressure, this suggests all four readings function as a menu of interchangeable legitimating narratives rather than a genuine structural transformation commitment — which would reclassify the entire kernel as closer to pure legitimation theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_stability, conceptual, 'Whether this reading represents a durable structural commitment or one of several interchangeable legitimating narratives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__qualitative_development_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__qualitative_development_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__qualitative_development_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__qualitative_development_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__qualitative_development_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__qualitative_development_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__qualitative_development_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__qualitative_development_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__qualitative_development_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__qualitative_development_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__qualitative_development_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__qualitative_development_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__qualitative_development_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__qualitative_development_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__qualitative_development_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__qualitative_development_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__qualitative_development_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__qualitative_development_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__qualitative_development_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the performance_legitimacy kernel, each authored as a separate ε-invariant constraint per the decomposition principle. quantitative_growth_reading measures legitimacy against GDP growth rate and job creation (different beneficiary/victim structure: favors labor-intensive and export manufacturing over the high-tech sector this reading favors). techno_nationalist_reading measures legitimacy against strategic-industry self-sufficiency (overlapping beneficiary set with this reading's high-tech champions, but justified by national-security logic rather than efficiency logic, with different tolerance for cost-inefficiency in strategic sectors). livelihood_security_reading measures legitimacy against direct household-welfare delivery (structurally in tension with this reading's tolerance for manufacturing-sector displacement). All four readings compete for which metric governs actual resource allocation and cadre evaluation at any given moment; this story's omega on kernel_reading_selection_stability documents that the same authority may invoke different siblings opportunistically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
