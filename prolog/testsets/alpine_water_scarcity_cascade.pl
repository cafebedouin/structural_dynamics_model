% ============================================================================
% CONSTRAINT STORY: alpine_water_scarcity_cascade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alpine_water_scarcity_cascade, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: alpine_water_scarcity_cascade
 *   human_readable: Alpine Water Scarcity Cascade: Coordination and Extraction in Transnational Water Governance
 *   domain: environmental/water_resources/geopolitical
 *
 * SUMMARY:
 *   Alpine water scarcity in the European Alps exemplifies a transnational
 *   extraction cascade where institutional coordination mechanisms
 *   (interstate water compacts, hydroelectric concessions, agricultural
 *   irrigation agreements) simultaneously solve genuine collective action
 *   problems and enforce asymmetric extraction from upstream alpine
 *   communities toward downstream agricultural and industrial users. The
 *   constraint operates across three nested time scales: immediate (seasonal
 *   water allocation), biographical (generational water security for
 *   communities and agriculture), and civilizational (glacial retreat forcing
 *   long-term reallocation). The classification as tangled rope reflects that
 *   no single perspective sees pure coordination or pure extraction — all
 *   institutional actors experience genuine coordination functions (without
 *   which conflicts would be far worse) coexisting with extraction mechanisms
 *   that benefit downstream users at upstream cost. The theater ratio (0.41)
 *   is moderate because while governance meetings and environmental reviews
 *   are performative, they are not yet completely decoupled from function —
 *   real constraints are being enforced through operational allocation. The
 *   constraint's stability depends on glacier melt persistence; as that
 *   subsidy declines, the hidden extraction becomes visible and the tangled
 *   rope will likely transition to snare (if extraction hardens) or scaffold
 *   (if new allocation frameworks emerge).
 *
 * KEY AGENTS:
 *   - Upstream Alpine Communities: Primary victim (powerless/trapped) — dependent on glacial melt and groundwater, no exit options, no political power to renegotiate compacts
 *   - Downstream Agricultural Regions: Primary beneficiary (moderate/constrained) — capture bulk of diverted water through irrigation agreements; exit costs are high but not total
 *   - Hydroelectric Operators: Secondary beneficiary (institutional/arbitrage) — manage water storage and generation, highly mobile, benefit from high-value extraction
 *   - Transnational Water Authorities: Institutional enforcer (organized/constrained) — manage interstate compacts, enforce allocation, suppress alternative frameworks; bound by treaty obligations but have agenda-setting power
 *   - Alpine Ecosystems & Subsistence Users: Victim (powerless/trapped) — cannot exit, no voice in governance, total suppression
 *   - Indigenous Alpine Federation: Organized victim (organized/constrained) — have developed coordination capacity and political voice, but constrained by asymmetric treaty terms
 *   - Climate-Adaptive Governance Coalition: Emerging alternative (organized/mobile) — sees current arrangement as temporary, has exit options and sunset logic
 *   - Historical Water Law Doctrines: Institutional constraint (institutional/mobile) — persists through inertia, theater increases as applicability decreases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alpine_water_scarcity_cascade, 0.58).
domain_priors:suppression_score(alpine_water_scarcity_cascade, 0.68).
domain_priors:theater_ratio(alpine_water_scarcity_cascade, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alpine_water_scarcity_cascade, extractiveness, 0.58).
narrative_ontology:constraint_metric(alpine_water_scarcity_cascade, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(alpine_water_scarcity_cascade, theater_ratio, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alpine_water_scarcity_cascade, tangled_rope).
narrative_ontology:human_readable(alpine_water_scarcity_cascade, "Alpine Water Scarcity Cascade: Coordination and Extraction in Transnational Water Governance").
narrative_ontology:topic_domain(alpine_water_scarcity_cascade, "environmental/water_resources/geopolitical").

domain_priors:requires_active_enforcement(alpine_water_scarcity_cascade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alpine_water_scarcity_cascade, downstream_agricultural_interests).
narrative_ontology:constraint_beneficiary(alpine_water_scarcity_cascade, hydroelectric_operators).
narrative_ontology:constraint_beneficiary(alpine_water_scarcity_cascade, water_intensive_industries).
narrative_ontology:constraint_victim(alpine_water_scarcity_cascade, upstream_alpine_communities).
narrative_ontology:constraint_victim(alpine_water_scarcity_cascade, alpine_ecosystems).
narrative_ontology:constraint_victim(alpine_water_scarcity_cascade, downstream_subsistence_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALPINE VILLAGE (SNARE) — Upstream alpine communities face water extraction with no exit mechanism. Glacier melt decline, upstream dams, and transnational diversion agreements leave them trapped in a deteriorating resource base. Cannot relocate without abandoning ancestral territories, cannot negotiate extraction terms individually, cannot access alternative freshwater sources. Maximum experienced extraction: survival depends on resources controlled by downstream actors.
constraint_indexing:constraint_classification(alpine_water_scarcity_cascade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DOWNSTREAM AGRICULTURAL REGION (TANGLED ROPE) — Genuine coordination problem exists: irrigation districts benefit from transnational water agreements that ensure reliable supply. But extraction is asymmetric — downstream regions capture lion's share while upstream bears full climate risk. Exit costs are high (agricultural transition, economic restructuring) but not total; some mobility exists through crop switching, irrigation efficiency, or relocation. Mixed coordination-extraction: the mechanism solves a real collective action problem (ensuring supply across borders) while extracting from upstream agents.
constraint_indexing:constraint_classification(alpine_water_scarcity_cascade, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HYDROELECTRIC OPERATOR (ROPE) — Benefits from water storage and extraction rights. Experiences the constraint as pure coordination: manages competing claims for water through rational allocation, dam operations, and contract enforcement. High mobility — can shift operations, renegotiate concessions, or exit markets. Net beneficiary with significant arbitrage options — extraction runs toward this agent.
constraint_indexing:constraint_classification(alpine_water_scarcity_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: TRANSNATIONAL WATER AUTHORITY (TANGLED ROPE) — Interstate water compacts genuinely solve coordination problems: without them, riparian conflict and unilateral capture would be worse. But the authority enforces asymmetric extraction: allocation formulas privilege downstream industrial/agricultural users and lock upstream communities into subordinate positions. Active enforcement required — treaties, dam operations, monitoring, dispute resolution. High suppression: upstream actors cannot unilaterally withdraw from compacts; exit costs include loss of water rights, economic sanctions, diplomatic isolation.
constraint_indexing:constraint_classification(alpine_water_scarcity_cascade, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: RIPARIAN DOCTRINE & PRIOR APPROPRIATION (PITON) — Historical water law frameworks (riparian rights, prior appropriation, first-in-time-first-in-right) persist through institutional inertia despite degraded function in the climate crisis. These doctrines were designed for stable precipitation and gradual glacial contribution. Under accelerating climate change, the frameworks cannot adapt fast enough — theaters of stakeholder consultation substitute for substantive reallocation. Theater ratio reflects that modern water governance meetings, environmental reviews, and sustainability reports perform urgency without enabling transitions. The doctrine's function has atrophied: it no longer solves the allocation problem, merely legitimates the status quo.
constraint_indexing:constraint_classification(alpine_water_scarcity_cascade, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INDIGENOUS ALPINE FEDERATION (TANGLED ROPE) — Organized upstream actors (indigenous communities, alpine municipalities) genuinely coordinate water governance and implement conservation. But they are trapped in asymmetric agreements: they bear the cost of restraint (limiting extractions, protecting ecosystems) while downstream actors capture agricultural value and hydroelectric rents. Constrained exit: organizing has given them voice but not veto power; they can slow extraction but cannot stop it. Extraction flows downstream despite their coordination.
constraint_indexing:constraint_classification(alpine_water_scarcity_cascade, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ALPINE ECOSYSTEMS & SUBSISTENCE USERS (SNARE) — Glaciers, alpine meadows, and subsistence populations cannot exit. Ecosystem function depends on baseflow maintenance, seasonal snowmelt, groundwater recharge — all constrained by upstream extraction. Subsistence users (herders, small farmers dependent on seasonal water) face collapse without alternative resources. Suppression is total: no market exit, no political voice, no mobility. Pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(alpine_water_scarcity_cascade, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 8: CLIMATE-ADAPTIVE GOVERNANCE COALITION (SCAFFOLD) — Emerging frameworks (transnational basin commissions, indigenous water rights recognition, ecosystem flow mandates, payment for ecosystem services) represent temporary institutional scaffolding. These mechanisms have sunset logic: as climate stabilization proceeds and water scarcity becomes undeniable, old allocation formulas will be replaced by need-based or ecosystem-first frameworks. Current scaffold suppression is moderate because organized actors see the transition path and have exit options (shift to efficiency, invest in alternatives). Sunset estimated at 15-25 years as glacial retreat forces renegotiation of all compacts.
constraint_indexing:constraint_classification(alpine_water_scarcity_cascade, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER — BIOPHYSICAL VIEW (MOUNTAIN) — From a civilizational/global perspective, alpine water scarcity is becoming an immutable constraint as glaciers retreat and climate patterns shift. The physics of water balance, snow-to-rain ratio, and evapotranspiration are not negotiable. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit. The biophysical scarcity is real, but the institutional arrangements determining who bears the cost are contingent.
constraint_indexing:constraint_classification(alpine_water_scarcity_cascade, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alpine_water_scarcity_cascade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alpine_water_scarcity_cascade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alpine_water_scarcity_cascade, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alpine_water_scarcity_cascade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(alpine_water_scarcity_cascade, TR),
    TR >= 0.70.

:- end_tests(alpine_water_scarcity_cascade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Downstream users (agriculture, hydroelectric) capture 70-80% of available water despite comprising ~40% of the basin population. Upstream users face declining glacial contribution yet bear conservation costs. The extraction is not total (some coordination function exists, some resources flow upstream) but is substantial and asymmetric. Suppression (0.68): High. Multiple mechanisms prevent upstream exit or renegotiation: (1) interstate treaties lock allocation formulas in place with high defection costs, (2) upstream communities lack political power to veto downstream extraction, (3) relocation is economically and culturally prohibitive, (4) alternative freshwater sources are unavailable locally, (5) ecosystem degradation reduces bargaining power. Theater ratio (0.41): Moderate. Governance involves genuine operational decisions (dam scheduling, allocation enforcement) but increasingly includes performative elements: environmental impact reviews that do not change outcomes, stakeholder consultations that lack veto power, sustainability reports that mask non-compliance. The ratio has increased from 0.28 (2000s) to 0.41 (present) as the gap between stated goals (ecosystem protection, equitable allocation) and actual outcomes (extraction acceleration) widens.
 *
 * PERSPECTIVAL GAP:
 *   Upstream and downstream perspectives diverge on whether the constraint is coordinate (rope) or extractive (snare). Downstream sees coordination function (without compacts there would be conflict; allocation enables planning). Upstream sees pure extraction (they get less water, lower income, ecosystem degradation). The truth is tangled rope: coordination is real but asymmetric extraction is also real. The coalition of organized upstream actors (indigenous federation, alpine municipalities) can see this clearly; powerless isolated individuals see only extraction. The transnational authority has the greatest perspectival distance — it sees the mechanism as purely coordinating (mediating among legitimate claims) while both victims and beneficiaries experience it as extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   The chi formula captures this by computing d from power + exit + beneficiary/victim status. Downstream agricultural regions are beneficiaries (d shifts down) but face constrained exit (d shifts up) → moderate d → moderate extraction experienced. Hydroelectric operators are major beneficiaries with arbitrage options → low d → low or negative experienced extraction. Upstream communities are victims (d shifts up) and trapped (d shifts up further) → very high d → very high experienced extraction. The piton perspective derives from the theater gate: water law doctrines have high theater (performative consultation, sustainability frameworks) masking degraded function (allocation formulas locked in place, ecosystem loss unabated).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the tangled rope classification holds across multiple institutional contexts (transnational compacts, hydroelectric operations, agricultural agreements) precisely because all these mechanisms exhibit genuine coordination functions coexisting with asymmetric extraction. The coordination is real — without it, unilateral capture and conflict would be worse. But the extraction is also real — downstream users capture bulk benefits while upstream bears bulk costs. The classification prevents two errors: (1) misclassifying as pure rope (ignoring asymmetry, validating 'cooperation' rhetoric) or (2) misclassifying as pure snare (ignoring that some coordination beats conflict). The mandatrophy is resolved by the multiple perspectives: downstream sees rope, upstream sees snare, analytical observer sees tangled rope — the presheaf captures the truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    glacier_retreat_timeline_precision,
    'What is the precise timeline of Alps glacier volume loss and its impact on seasonal water availability? Does retreat follow linear or nonlinear trajectories?',
    'Satellite observations, ice core analysis, hydrological modeling with 5-10 year forecast horizon; comparison of observed retreat against 2010 projections',
    'If retreat is slower than projected: current allocation frameworks remain viable for 30+ years. If nonlinear acceleration occurs: reallocation crises hit within 5-10 years, forcing renegotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(glacier_retreat_timeline_precision, empirical, 'Glacier retreat timeline and nonlinearity of ice volume loss').

omega_variable(
    transnational_compact_renegotiation_feasibility,
    'Can existing interstate water compacts (Rhine, Danube, Alpine treaties) be renegotiated without catastrophic conflict or unilateral defection?',
    'Historical analysis of past treaty renegotiations; modeling of coalitional incentives under various scarcity scenarios; tracking of diplomatic negotiations 2026-2040',
    'If feasible: constraint transitions to scaffold as compacts adapt. If not feasible: constraint hardens into snare as allocation freezes and extraction accelerates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transnational_compact_renegotiation_feasibility, empirical, 'Whether transnational water compacts can be renegotiated').

omega_variable(
    ecosystem_collapse_threshold,
    'At what level of baseflow reduction do alpine ecosystems undergo irreversible transition (meadow to tundra, trout stream to intermittent creek)?',
    'Ecological field studies; identification of critical thresholds for key species and ecosystem functions; hydrological sensitivity analysis',
    'If threshold is breached within current trajectory: constraint becomes mountain (ecosystem function becomes immutable constraint on water availability). If threshold is distant: ecosystem extraction is real but reversible, keeping snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_collapse_threshold, empirical, 'Ecosystem collapse threshold under baseflow reduction').

omega_variable(
    indigenous_coalition_power_growth,
    'Will indigenous alpine communities achieve sufficient political power to veto or significantly constrain water extraction agreements?',
    'Tracking of indigenous political organization, legal victories, transnational coalition formation; measurement of veto power in treaty renegotiations',
    'If achieved: tangled rope classification from indigenous perspective transitions to rope or scaffold. If not achieved: snare classification persists and may harden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_coalition_power_growth, preference, 'Whether indigenous communities achieve significant veto power').

omega_variable(
    downstream_agricultural_transition_cost,
    'What is the true economic cost of transitioning downstream agricultural regions away from glacier-fed irrigation (crop switching, efficiency, alternative sources)?',
    'Cost-benefit analysis of agricultural transitions; modeling of labor displacement, regional economic restructuring; comparison to climate damage costs of inaction',
    'If transition cost is lower than extraction benefit: downstream actors may voluntarily cede water to upstream during renegotiation. If higher: extraction will be defended fiercely, tangled rope classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downstream_agricultural_transition_cost, empirical, 'Economic cost of downstream agricultural transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alpine_water_scarcity_cascade, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alpine_tr_t0, alpine_water_scarcity_cascade, theater_ratio, 0, 0.28).
narrative_ontology:measurement(alpine_tr_t8, alpine_water_scarcity_cascade, theater_ratio, 8, 0.38).
narrative_ontology:measurement(alpine_tr_t16, alpine_water_scarcity_cascade, theater_ratio, 16, 0.41).
narrative_ontology:measurement(alpine_tr_t24, alpine_water_scarcity_cascade, theater_ratio, 24, 0.43).

% Extraction over time
narrative_ontology:measurement(alpine_be_t0, alpine_water_scarcity_cascade, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(alpine_be_t8, alpine_water_scarcity_cascade, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(alpine_be_t16, alpine_water_scarcity_cascade, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(alpine_be_t24, alpine_water_scarcity_cascade, base_extractiveness, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alpine_water_scarcity_cascade, resource_allocation).
narrative_ontology:boltzmann_floor_override(alpine_water_scarcity_cascade, 0.18).
narrative_ontology:affects_constraint(alpine_water_scarcity_cascade, glacier_recession_constraint).
narrative_ontology:affects_constraint(alpine_water_scarcity_cascade, downstream_agricultural_viability).
narrative_ontology:affects_constraint(alpine_water_scarcity_cascade, alpine_ecosystem_resilience).
narrative_ontology:affects_constraint(alpine_water_scarcity_cascade, transnational_treaty_rigidity).

% DUAL FORMULATION NOTE:
% Alpine water scarcity decomposes into multiple distinct constraints with different ε values: (1) biophysical glacier retreat (ε ≈ 0.05, mountain) is upstream of (2) institutional allocation asymmetry (ε ≈ 0.58, tangled rope). The allocation mechanism exists independently of the biophysical scarcity but operates on a shared resource. The two constraints are related through network dependency: glacier retreat makes renegotiation of allocation mechanisms necessary, but the institutional constraint determines who bears the adaptation cost.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(alpine_water_scarcity_cascade, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
