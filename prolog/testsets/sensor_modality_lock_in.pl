% ============================================================================
% CONSTRAINT STORY: sensor_modality_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sensor_modality_lock_in, []).

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
 *   constraint_id: sensor_modality_lock_in
 *   human_readable: Camera-Only Autonomy Architecture Lock-In
 *   domain: technology_governance/autonomous_vehicles/platform_economics
 *
 * SUMMARY:
 *   The camera-only autonomy architecture represents a structural commitment
 *   with asymmetric consequences: Tesla captures $11,600 cost differential
 *   per vehicle (enabling mass-market pricing and superior margins) while
 *   fleet customers and operational reliability bear the cost of operational
 *   design domain restrictions. The constraint exhibits genuine coordination
 *   function (cost reduction enables broader autonomy deployment) coupled
 *   with asymmetric extraction (architectural lock-in prevents adaptation as
 *   operational limits become clear). Camera-only systems cannot operate in
 *   snow, fog, or heavy rain — conditions where lidar-based sensor fusion
 *   maintains capability. This is not a temporary software problem but an
 *   architectural ceiling: cameras lack depth perception and fail in
 *   low-visibility conditions that lidar penetrates. The $400 vs $12k cost
 *   differential creates a coordination incentive (standardize on cheaper
 *   sensors) that becomes extractive when the architectural commitment
 *   prevents pivoting as operational constraints become undeniable. Theater
 *   ratio (0.58) reflects that autonomy capability claims (Full Self-Driving,
 *   Autopilot) systematically exceed deliverable performance within
 *   camera-only architecture, maintained through definitional flexibility and
 *   temporal deferral.
 *
 * KEY AGENTS:
 *   - Fleet Customer: Primary victim (powerless/trapped) — purchased vehicles based on autonomy promises; cannot exit without total capital loss; operational design domain restrictions discovered post-purchase
 *   - Tesla Margin Structure: Primary beneficiary (institutional/arbitrage) — captures $11,600 cost differential per vehicle; architectural commitment enables vertical integration and data moat
 *   - Safety Regulator: Mixed position (moderate/constrained) — benefits from standardization pressure but bears cost of edge-case failures that camera-only cannot handle
 *   - Lidar Industry Coalition: Organized agents (organized/mobile) — see camera-only as temporary market distortion with sunset as lidar costs decline to $500 by 2028
 *   - Competing Automaker: Secondary victim (moderate/constrained) — forced to match Tesla price point while maintaining sensor fusion, creating margin compression
 *   - Autonomy Milestone Narrative: Institutional actor (institutional/arbitrage) — maintains theatrical capability claims that exceed camera-only deliverable performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sensor_modality_lock_in, 0.48).
domain_priors:suppression_score(sensor_modality_lock_in, 0.62).
domain_priors:theater_ratio(sensor_modality_lock_in, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sensor_modality_lock_in, extractiveness, 0.48).
narrative_ontology:constraint_metric(sensor_modality_lock_in, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sensor_modality_lock_in, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sensor_modality_lock_in, tangled_rope).
narrative_ontology:human_readable(sensor_modality_lock_in, "Camera-Only Autonomy Architecture Lock-In").
narrative_ontology:topic_domain(sensor_modality_lock_in, "technology_governance/autonomous_vehicles/platform_economics").

domain_priors:requires_active_enforcement(sensor_modality_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sensor_modality_lock_in, tesla_margin_structure).
narrative_ontology:constraint_beneficiary(sensor_modality_lock_in, camera_supply_chain).
narrative_ontology:constraint_beneficiary(sensor_modality_lock_in, software_first_narrative).
narrative_ontology:constraint_victim(sensor_modality_lock_in, operational_reliability).
narrative_ontology:constraint_victim(sensor_modality_lock_in, fleet_customers).
narrative_ontology:constraint_victim(sensor_modality_lock_in, edge_case_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FLEET CUSTOMER (SNARE) — Purchased vehicles based on autonomy promises; cannot exit without total capital loss. Operational design domain restrictions (no snow/fog/heavy rain capability) discovered post-purchase. Vehicle depreciation accelerates as competitors demonstrate superior capabilities. Maximum extraction: paid premium for capability that cannot be delivered within chosen architecture.
constraint_indexing:constraint_classification(sensor_modality_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAFETY REGULATOR (TANGLED ROPE) — Constrained by need to balance innovation incentives against public safety. Benefits from standardization pressure (camera-only creates regulatory precedent) but bears cost of edge-case failures that camera-only architecture cannot handle. Mixed extraction: coordination function exists (establishing sensor requirements) but asymmetric risk (regulator blamed for both over-regulation and under-regulation).
constraint_indexing:constraint_classification(sensor_modality_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLATFORM ARCHITECT (ROPE) — Tesla's margin structure benefits from $400 camera-only vs $12k lidar sensor fusion. Architectural commitment enables vertical integration, data moat (vision-only training pipeline), and narrative control (software will solve it). Net beneficiary: extraction flows toward this agent through cost differential capture and competitive moat construction.
constraint_indexing:constraint_classification(sensor_modality_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LIDAR INDUSTRY COALITION (SCAFFOLD) — Organized suppliers (Luminar, Innoviz, Velodyne) see camera-only as temporary market distortion with sunset logic: as lidar costs decline ($500 projected by 2028) and camera-only operational limits become undeniable, sensor fusion becomes economically viable. Coalition has exit options (pivot to ADAS, industrial automation) and sees architectural lock-in dissolving as cost curves converge.
constraint_indexing:constraint_classification(sensor_modality_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPETING AUTOMAKER (TANGLED ROPE) — Constrained by need to match Tesla's price point while maintaining sensor fusion architecture. Benefits from coordination (industry converges on sensor requirements, supply chain matures) but bears extraction (Tesla's cost advantage forces margin compression or feature reduction). Mixed position: coordination function real but asymmetric cost structure creates competitive disadvantage.
constraint_indexing:constraint_classification(sensor_modality_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: AUTONOMY MILESTONE NARRATIVE (PITON) — Public demonstrations and capability claims (Full Self-Driving Beta, Autopilot) are substantially theatrical: operational design domain restrictions mean claimed autonomy level cannot be achieved within camera-only architecture, but narrative persists through marketing and incremental feature releases. Theater ratio high: performance claims exceed deliverable capability, maintained through definitional flexibility (what counts as 'self-driving') and temporal deferral (next software update will fix it).
constraint_indexing:constraint_classification(sensor_modality_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Camera-only architecture represents genuine coordination function (cost reduction enables mass-market autonomy) coupled with asymmetric extraction (operational reliability sacrificed for margin structure). The constraint is not a false summit (physics permits camera-only autonomy in constrained domains) but also not pure coordination (architectural commitment creates lock-in that prevents adaptation as operational limits become clear). Tangled rope: both functions coexist and neither dominates.
constraint_indexing:constraint_classification(sensor_modality_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sensor_modality_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sensor_modality_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sensor_modality_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sensor_modality_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sensor_modality_lock_in, TR),
    TR >= 0.70.

:- end_tests(sensor_modality_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The $11,600 cost differential per vehicle flows to Tesla's margin structure while operational reliability degradation (no snow/fog/rain capability) flows to fleet customers. Extraction is substantial but not maximal because coordination function is real: camera-only does enable mass-market autonomy in constrained operational design domains (highway, clear weather, well-marked roads). The architectural commitment becomes extractive when it prevents adaptation — fleet customers cannot retrofit lidar, and Tesla's vertical integration (vision-only training pipeline, data moat) creates switching costs that lock in the architecture even as operational limits become clear. Suppression (0.62): Moderate-high. Fleet customers face capital loss (vehicle depreciation), sunk cost (paid premium for autonomy capability), and switching cost (no retrofit path). Competing automakers face margin compression (must match Tesla price or lose market share). Safety regulators face political cost (blamed for both over-regulation if they mandate sensors and under-regulation if edge cases cause fatalities). Suppression is not total — some agents can exit (lidar coalition pivots to other markets, customers can sell vehicles) — but barriers are significant. Theater ratio (0.58): Moderate-high. Autonomy capability claims systematically exceed camera-only deliverable performance. Full Self-Driving Beta operates only in constrained ODD; Autopilot requires constant supervision; marketing materials imply unrestricted autonomy while fine print specifies restrictions. Theater has increased over interval as gap between claimed capability and architectural ceiling becomes more apparent, requiring more definitional flexibility to maintain narrative.
 *
 * PERSPECTIVAL GAP:
 *   The platform architect (Tesla) sees pure coordination (Rope) — camera-only solves the legitimate problem of making autonomy economically viable at mass-market scale. The lidar industry coalition sees temporary market distortion with sunset (Scaffold) — as lidar costs decline, sensor fusion becomes viable and camera-only lock-in dissolves. The autonomy narrative sees degraded capability claims (Piton) — marketing maintains promises that architecture cannot deliver. Fleet customers see pure extraction (Snare) — paid premium for capability that cannot be achieved within chosen architecture, cannot exit without capital loss. Safety regulators and competing automakers see mixed coordination-extraction (Tangled Rope) — standardization benefits exist but asymmetric costs create extraction. The analytical observer sees tangled rope as the structural reality: both coordination function (cost reduction) and extraction mechanism (architectural lock-in preventing adaptation) coexist, and neither dominates. The perspectival gap reveals that 'camera-only vs sensor fusion' is not a technical question with a single answer but a structural position question: your classification depends on whether you capture the cost differential or bear the operational restrictions.
 *
 * DIRECTIONALITY LOGIC:
 *   Tesla's margin structure is the primary beneficiary: institutional power with arbitrage exit options (can pivot architecture in future vehicle generations) yields low directionality value and negative effective extraction — the constraint subsidizes this agent. Fleet customers are primary victims: powerless with trapped exit options (cannot exit without capital loss) yields high directionality value and maximum effective extraction. Safety regulators occupy mixed position: moderate power with constrained exit (cannot ignore autonomy regulation but face political cost either way) yields mid-range directionality and moderate extraction. Lidar coalition has organized power with mobile exit options (can pivot to other markets) yielding low-moderate directionality — they experience the constraint as temporary market distortion rather than permanent extraction. Competing automakers have moderate power with constrained exit (must compete in autonomy market but architectural choice creates cost disadvantage) yielding moderate directionality and mixed extraction-coordination experience. The analytical observer sees both functions coexist: genuine cost-reduction coordination coupled with asymmetric architectural lock-in extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that tangled rope classification requires BOTH genuine coordination function AND asymmetric extraction, and that both must be structurally persistent rather than transitional. Camera-only architecture provides real coordination: $11,600 cost reduction per vehicle enables mass-market autonomy deployment that would be economically unviable with $12k lidar systems. This is not theatrical — the cost differential is real and the coordination function (making autonomy accessible) is genuine. Simultaneously, the architectural commitment creates asymmetric extraction: fleet customers bear operational design domain restrictions (no snow/fog/rain capability) that they cannot escape without capital loss, while Tesla captures margin advantage and competitive moat. The extraction is not incidental to coordination but structurally coupled: the same architectural choice that enables cost reduction also creates operational ceiling and lock-in. Neither function is transitional: coordination persists (cost advantage remains) and extraction persists (architectural lock-in prevents retrofit). The constraint is not 'coordination that will become extraction' (scaffold degrading) or 'extraction disguised as coordination' (snare with FNL cover story) but both simultaneously. Tangled rope is the only type that captures this structural duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vision_sufficiency_threshold,
    'Is camera-only perception fundamentally sufficient for L4/L5 autonomy, or does it hit an architectural ceiling below full autonomy?',
    'Longitudinal tracking of operational design domain expansion: if camera-only systems achieve unrestricted operation within 10 years, architecture is sufficient; if ODD restrictions persist or widen, architectural ceiling confirmed.',
    'If sufficient: lock-in is temporary coordination problem (Scaffold from more perspectives). If insufficient: lock-in is extraction mechanism preventing architectural pivot (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vision_sufficiency_threshold, empirical, 'Whether camera-only perception can achieve unrestricted autonomy').

omega_variable(
    lidar_cost_trajectory,
    'Will lidar costs decline to camera-competitive levels ($500-1000/vehicle) within the architectural lock-in window (5-7 years)?',
    'Lidar manufacturing cost curves; solid-state lidar adoption rates; automotive-grade qualification timelines.',
    'If costs converge rapidly: sensor fusion becomes economically viable and camera-only lock-in dissolves (Scaffold confirmed). If costs remain high: camera-only maintains cost advantage and lock-in persists (Snare from fleet customer perspective intensifies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lidar_cost_trajectory, empirical, 'Lidar cost decline timeline relative to architectural commitment window').

omega_variable(
    regulatory_sensor_mandate,
    'Will regulators mandate sensor redundancy (lidar or radar) for L4/L5 certification, or accept camera-only with ODD restrictions?',
    'NHTSA, UNECE, and EU regulatory framework evolution; precedent from aviation (redundant sensor requirements) vs consumer electronics (performance-based standards).',
    'If mandate: camera-only architecture cannot achieve full autonomy certification regardless of software advances (Snare confirmed for fleet customers). If ODD-restricted certification allowed: camera-only remains viable in constrained domains (Tangled Rope maintained).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_sensor_mandate, preference, 'Whether regulators will require sensor redundancy for autonomy certification').

omega_variable(
    fleet_depreciation_acceleration,
    'Does camera-only architectural limitation cause accelerated depreciation relative to sensor-fusion vehicles as operational limits become market-visible?',
    'Used vehicle market pricing; fleet resale values; insurance actuarial data on operational design domain restriction impact.',
    'If depreciation accelerates: extraction from fleet customers intensifies as capital loss compounds operational restrictions (Snare perspective strengthens). If depreciation matches sensor-fusion vehicles: market does not price architectural risk (extraction less severe than structural analysis suggests).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fleet_depreciation_acceleration, empirical, 'Whether architectural lock-in causes measurable depreciation acceleration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sensor_modality_lock_in, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sensor_lock_theater_t0, sensor_modality_lock_in, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sensor_lock_theater_t3, sensor_modality_lock_in, theater_ratio, 3, 0.48).
narrative_ontology:measurement(sensor_lock_theater_t6, sensor_modality_lock_in, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(sensor_lock_extract_t0, sensor_modality_lock_in, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sensor_lock_extract_t3, sensor_modality_lock_in, base_extractiveness, 3, 0.41).
narrative_ontology:measurement(sensor_lock_extract_t6, sensor_modality_lock_in, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sensor_modality_lock_in, resource_allocation).
narrative_ontology:affects_constraint(sensor_modality_lock_in, hardware_software_inversion).

% DUAL FORMULATION NOTE:
% Sensor modality lock-in is downstream of hardware-software inversion (the broader architectural commitment to solve autonomy in software rather than hardware). The upstream constraint (hardware_software_inversion, claimed_type: snare) represents the strategic choice to minimize hardware cost and maximize software leverage. Sensor modality lock-in is the specific instantiation of that choice in the perception stack: camera-only is the hardware-minimization strategy applied to sensing. The two constraints have different epsilon values because they operate at different architectural layers: hardware-software inversion affects the entire vehicle platform (compute, sensors, actuators, update mechanism), while sensor modality lock-in affects only the perception subsystem. Both are part of the same architectural family and should be analyzed together for contamination propagation: if hardware-software inversion's purity degrades (software-first strategy fails to deliver promised capability), sensor modality lock-in's extraction intensifies (architectural lock-in becomes more costly as pivot becomes necessary but impossible).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sensor_modality_lock_in, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
