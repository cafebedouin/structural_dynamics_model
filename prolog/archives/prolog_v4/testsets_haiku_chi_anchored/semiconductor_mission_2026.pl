% ============================================================================
% CONSTRAINT STORY: semiconductor_mission_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semiconductor_mission_2026, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: semiconductor_mission_2026
 *   human_readable: Indian Semiconductor Mission 2.0
 *   domain: economic/industrial_policy
 *
 * SUMMARY:
 *   The Indian Semiconductor Mission 2.0 represents a complex industrial
 *   policy constraint that combines legitimate coordination (building
 *   domestic manufacturing capability) with asymmetric extraction
 *   (protectionist restrictions that harm consumers and global competitors).
 *   Launched in 2021 and accelerated in 2024-2025, the mission provides
 *   subsidies, tax breaks, tariff protection, and preferential procurement to
 *   domestic semiconductor manufacturers. The constraint exhibits different
 *   classifications from different perspectives: domestic manufacturers see
 *   enabling coordination (rope); consumers and SMEs see extraction through
 *   restricted access (snare); global suppliers see constrained market access
 *   (tangled rope); government policy sees a temporary bootstrap program with
 *   sunset logic (scaffold); the export commitment apparatus shows
 *   degradation into theater (piton); and the civilizational view risks
 *   naturalizing what is a contingent policy choice as inevitable
 *   technological necessity (false mountain). The escalating theater_ratio
 *   (0.35→0.61) reflects increasing performative reporting around export
 *   targets and capacity deployment metrics that may not reflect genuine
 *   technological independence or global competitiveness. The rising
 *   extractiveness (0.28→0.52) indicates that protectionist measures are
 *   accumulating faster than functional capabilities are being transferred.
 *
 * KEY AGENTS:
 *   - Domestic Semiconductor Manufacturers (Institutional/Arbitrage): Primary beneficiaries—receive subsidies, tax breaks, tariff protection, preferential procurement; can arbitrage between state support and export markets
 *   - Indian Consumers & SMEs (Powerless/Trapped): Primary victims—trapped in domestic market with higher-cost, lower-capability components; cannot exit without relocating business operations
 *   - Global Semiconductor Suppliers (Organized/Constrained): Secondary victims—face tariff barriers and preferential procurement rules; also constrained by Indian market access opportunities and potential partnership channels
 *   - Government Industrial Policy Agencies (Organized/Constrained): Architects of coordination mechanism; see mission as temporary bootstrap with generational sunset; constrained by fiscal sustainability and technology transfer efficacy
 *   - Export Performance Apparatus (Institutional/Constrained): Responsible for reporting mission success via export volumes; implements performative metrics that may not reflect genuine global competitiveness
 *   - Analytical Observer (Analytical/Analytical): Civilizational perspective that risks naturalizing contingent industrial policy as inherent technology learning curve
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semiconductor_mission_2026, 0.52).
domain_priors:suppression_score(semiconductor_mission_2026, 0.48).
domain_priors:theater_ratio(semiconductor_mission_2026, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semiconductor_mission_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(semiconductor_mission_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(semiconductor_mission_2026, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semiconductor_mission_2026, tangled_rope).
narrative_ontology:human_readable(semiconductor_mission_2026, "Indian Semiconductor Mission 2.0").
narrative_ontology:topic_domain(semiconductor_mission_2026, "economic/industrial_policy").

domain_priors:requires_active_enforcement(semiconductor_mission_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semiconductor_mission_2026, domestic_semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(semiconductor_mission_2026, government_industrial_policy_agencies).
narrative_ontology:constraint_victim(semiconductor_mission_2026, competing_global_semiconductor_suppliers).
narrative_ontology:constraint_victim(semiconductor_mission_2026, end_users_via_technology_lock_in).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIAN CONSUMERS & SMEs (SNARE) — Trapped by protectionist policy that restricts access to globally competitive components. Must source from domestically subsidized suppliers with higher costs and potentially inferior specifications. Cannot exit domestic market without relocating business. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(semiconductor_mission_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC SEMICONDUCTOR MANUFACTURERS (ROPE) — Primary beneficiaries of subsidies, tax breaks, and tariff protection. Can arbitrage between domestic support mechanisms and global supply chains. Experience mission as enabling coordination: access to capital, infrastructure, and preferential procurement. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(semiconductor_mission_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: GLOBAL SEMICONDUCTOR SUPPLIERS (TANGLED ROPE) — Constrained by Indian tariff barriers and preferential procurement rules, but also benefit from market access and potential partnerships with subsidized Indian manufacturers. Experience mission as both coordination (access to Indian market) and extraction (margin compression due to domestic competition artificially boosted by subsidies). d≈0.58, f(d)≈0.73, σ=1.2 → χ≈0.39.
constraint_indexing:constraint_classification(semiconductor_mission_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: GOVERNMENT INDUSTRIAL POLICY (SCAFFOLD) — Orchestrates the mission as a temporary coordination mechanism to bootstrap domestic capabilities. Explicit goal: reduce dependency on foreign suppliers within 10-15 years via technology transfer and skills development. Sees mission with sunset logic: as domestic industry matures and achieves global competitiveness, protectionist supports can be removed. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.18.
constraint_indexing:constraint_classification(semiconductor_mission_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTRONICS EXPORT COMMITMENTS (PITON) — The mission includes contradictory export targets while maintaining domestic protection. Government commits to exporting semiconductors globally while restricting foreign competition domestically. This creates theater: compliance reporting on export volumes inflates the functional success of the mission, but actual export competitiveness depends on global-scale manufacturing (which the domestic protectionist regime suppresses). theater_ratio=0.61 reflects this performative reporting layer. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.32.
constraint_indexing:constraint_classification(semiconductor_mission_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LEARNING CURVE VIEW (MOUNTAIN) — From a civilizational perspective, semiconductor manufacturing has a steep learning curve and economies of scale that require sustained capital investment. Any emerging nation attempting to build independent capabilities must solve this structural problem; the mission is reframing a natural constraint of the technology domain. However, structural data (ε=0.52, suppression=0.48) contradicts the mountain classification — the constraint is contingent on policy choices, not technological immutability.
constraint_indexing:constraint_classification(semiconductor_mission_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semiconductor_mission_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semiconductor_mission_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semiconductor_mission_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semiconductor_mission_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(semiconductor_mission_2026, TR),
    TR >= 0.70.

:- end_tests(semiconductor_mission_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The mission provides real subsidies (estimated 15-20% cost advantage for domestic manufacturers) and tariff protection (15-20% tariff on imported semiconductors), creating measurable extraction from consumers and global competitors. However, extractiveness is not at snare levels (0.70+) because the mission explicitly targets capability building, not pure rent extraction. The structure is hybrid: subsidies fund R&D and infrastructure, which can generate genuine learning, but also support lower-efficiency producers and suppress competitive pressure that would accelerate improvement. Suppression (0.48): Moderate. Barriers to exit include tariffs, local content requirements, and preferential procurement rules—but these are explicitly policy-codified, not inherent to the technology. Global suppliers can lobby, form joint ventures with domestic partners, or wait for policy changes. Domestic suppliers face lower suppression because they benefit from the rules. Theater ratio (0.61): Moderate-high. The mission is tracked via export volume targets and capacity metrics, but actual global competitiveness remains unproven. Government reports emphasize approved projects and committed capacity, but many projects face delays and technology transfer stalls. The export targets are ambitious relative to manufacturing ramp-up timelines, creating theater: reported progress on capacity deployment may not translate to actual export volumes or technological autonomy.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates conflicting readings from different structural positions. Domestic manufacturers see the mission as pure coordination—it solves the real collective action problem of expensive foundry construction and technology risk. Global suppliers see partial extraction but also market opportunity. The key gap emerges between beneficiaries and victims: Indian consumers see extraction (higher costs, restricted choice), while the government sees temporary coordination with sunset (the subsidies fund capability that will eventually make them unnecessary). The piton perspective reveals that even government agencies may be losing sight of the sunset logic as they accumulate new export commitments and expand subsidy programs—what was intended as temporary support shows signs of becoming permanent institutional apparatus justified by performative metrics. The analytical observer's 'mountain' framing risks legitimizing what is actually a policy choice by calling it 'inherent to semiconductor learning curves.' This is a false summit if the alternative (open competition + targeted R&D) could achieve comparable capability faster.
 *
 * DIRECTIONALITY LOGIC:
 *   Domestic manufacturers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; negative effective extraction means they subsidize the system. Indian consumers/SMEs: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit options from domestic market restrictions. Global suppliers: Victim + constrained → d≈0.58, f(d)≈0.73. Significant extraction (tariff barriers, preferential procurement) but not maximal because they retain arbitrage paths (joint ventures, lobbying, waiting for policy change). Government agencies: Beneficiary + constrained → d≈0.35, f(d)≈0.35. Low-to-moderate effective extraction because government is the architect of the constraint and can modify it; experiences the mission as agency-enabling rather than extractive. Export apparatus: Split between beneficiary (institutional, arbitrage for success metrics) and victim (constrained by actual performance requirements); piton classification derives from theater gate, not directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival; the false summit detector should flag this.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is NOT fully resolved. The core ambiguity: Is this a legitimate temporary industrial bootstrap (scaffold with sunset) or a permanent protectionist extraction (snare masquerading as policy)? The mandatrophy resolves partially via time horizon: from a 10-15 year generational perspective (government view), the mission is scaffold—the sunset is explicit. From a consumer or global-supplier view at biographical timescale, it is snare—the extraction is immediate and persistent. From the analytical view, the mountain framing is false—semiconductors are not inherently subject to immutable technological constraints that require permanent protection. What IS immutable is the time cost of learning: building manufacturing capability takes years. What IS NOT immutable is the choice to use protectionism vs. open competition + targeted R&D. The escalating theater_ratio and extractiveness suggest that the mission is drifting from scaffold toward piton (performance metrics becoming decoupled from functional outcomes) or toward snare (sunset never arriving). Resolution depends on whether forced technology transfer actually produces autonomous manufacturers or remains perpetually dependent on foreign IP.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_transfer_efficacy,
    'Will forced technology transfer agreements in subsidy conditions actually result in domestic technological capability, or will domestic manufacturers remain dependent on foreign IP and supply chain inputs?',
    'Longitudinal tracking of patent filings, design autonomy assessments, and supply chain self-sufficiency metrics for subsidized manufacturers over 5-10 year horizon',
    'If efficacy is high: mission succeeds as temporary scaffold, classification remains valid. If efficacy is low: mission becomes permanent extractive snare (subsidies fund perpetual dependency rather than independence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_efficacy, empirical, 'Whether forced technology transfer produces genuine domestic capability').

omega_variable(
    consumer_cost_externality,
    'What is the aggregate economic harm to Indian consumers and SMEs from higher component costs and technology lock-in, relative to the industrial development gains?',
    'Cost-benefit analysis: cumulative subsidy expenditure vs. consumer surplus loss; comparison to counterfactual scenario of open competition with targeted R&D investment instead of protectionism',
    'If harm >> gains: victim classification (snare) is vindicated, mission is regressive wealth transfer. If gains justify harm: tangled_rope classification holds, mission is legitimate industrial policy trade-off.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_cost_externality, empirical, 'Net economic impact on end users relative to development gains').

omega_variable(
    global_competitiveness_trajectory,
    'Will subsidized Indian manufacturers achieve global cost and quality parity with established foundries (TSMC, Samsung, Intel), or will they remain niche players dependent on domestic procurement preferences?',
    'Track manufacturing cost per wafer, defect rates, process node capabilities, and actual non-preferential export volumes. Compare with TSMC/Samsung benchmarks at equivalent subsidy maturity stages.',
    'If trajectory to parity: scaffold sunset is credible (protections can be removed as competitiveness emerges). If stuck at niche: sunset never arrives, permanent tangled rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_competitiveness_trajectory, empirical, 'Whether subsidized manufacturers can compete globally without protectionism').

omega_variable(
    hidden_extraction_metrics,
    'Are government agencies using mission performance metrics (capacity targets, export volumes) to justify subsidy continuation even as underlying extraction mechanisms persist?',
    'Audit of mission performance reporting vs. actual functional outcomes (technology transfer completeness, manufacturing process autonomy, export competitiveness without preferences). Identify metric gaming in theater_ratio.',
    'If significant gaming detected: theater_ratio rises above 0.70 (piton threshold), mission degrades from scaffold to piton. Extraction continues but justification becomes performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_extraction_metrics, empirical, 'Whether mission metrics are inflated relative to functional outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semiconductor_mission_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sem_tr_t0, semiconductor_mission_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sem_tr_t3, semiconductor_mission_2026, theater_ratio, 3, 0.48).
narrative_ontology:measurement(sem_tr_t6, semiconductor_mission_2026, theater_ratio, 6, 0.61).

% Extraction over time
narrative_ontology:measurement(sem_be_t0, semiconductor_mission_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sem_be_t3, semiconductor_mission_2026, base_extractiveness, 3, 0.41).
narrative_ontology:measurement(sem_be_t6, semiconductor_mission_2026, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semiconductor_mission_2026, resource_allocation).
narrative_ontology:affects_constraint(semiconductor_mission_2026, rare_earth_supply_dependency).
narrative_ontology:affects_constraint(semiconductor_mission_2026, semiconductor_trade_war_dynamics).
narrative_ontology:affects_constraint(semiconductor_mission_2026, technology_transfer_coercion).

% DUAL FORMULATION NOTE:
% The Indian Semiconductor Mission 2.0 operates at the nexus of three distinct structural constraints: (1) Rare earth element sourcing and refinement (upstream supply chain), (2) geopolitical semiconductor competition (bilateral trade dynamics), and (3) institutional forced technology transfer (IP extraction via subsidy conditionality). This story focuses on the direct internal constraint (extractiveness via protectionism and preferential procurement). Upstream supply constraints have different ε values; downstream trade war dynamics are separate stories linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(semiconductor_mission_2026, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
