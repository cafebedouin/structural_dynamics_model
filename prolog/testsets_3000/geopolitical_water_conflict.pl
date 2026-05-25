% ============================================================================
% CONSTRAINT STORY: geopolitical_water_conflict
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geopolitical_water_conflict, []).

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
 *   constraint_id: geopolitical_water_conflict
 *   human_readable: Geopolitical Water Conflict as Coordination-Extraction Hybrid
 *   domain: geopolitics/resource_governance/environmental
 *
 * SUMMARY:
 *   Geopolitical water conflict represents a constraint that coordinates
 *   legitimate shared interests (seasonal flow management, flood control,
 *   joint infrastructure) while extracting asymmetric benefits toward
 *   upstream riparian states and away from downstream populations and
 *   ecosystems. The constraint exhibits genuine coordination value —
 *   unilateral water management is more dangerous and less productive than
 *   negotiated treaties — yet operates through power asymmetry rooted in
 *   geography and history. The theater ratio (0.55) reflects significant
 *   performative content: diplomatic rituals (basin committees, cooperation
 *   frameworks) maintain the appearance of joint governance while actual
 *   allocation remains locked into historically determined patterns.
 *   Extractiveness has increased over the interval (0.35 → 0.58) as climate
 *   variability and population growth tighten the scarcity margin, making the
 *   asymmetry more severe. The constraint is downstream of upstream national
 *   interests (energy production, industrial development) but feeds into
 *   geopolitical stability concerns across the region.
 *
 * KEY AGENTS:
 *   - Downstream Subsistence Communities: Primary victim (powerless/trapped) — Geographic dependence, non-substitutable water, no alternative sources. Bears full extraction cost through seasonal drought and ecosystem degradation.
 *   - Downstream Riparian State: Moderate victim (moderate/constrained) — Diplomatic and trade dependencies constrain response options despite state-level power. Benefits from coordination (flood warning, scheduled releases) but net victim of allocation asymmetry.
 *   - Upstream Riparian State: Primary beneficiary (institutional/constrained) — Controls water supply and extraction flow. Constrained by international law, downstream military capacity, and regional coalition pressure. Benefits from both extraction rents and coordination benefits.
 *   - Upstream Industrial & Hydroelectric Users: Secondary beneficiary (powerful/mobile) — Capture below-market water prices and reliable supply. Mobile but invested in current regime.
 *   - International Water Treaty Institutions: Arbitrage beneficiary (institutional/arbitrage) — Gain legitimacy and enforcement mandates through dispute resolution. Experience constraint as pure coordination.
 *   - Climate Adaptation Coalition: Organized agents (organized/mobile) — See constraint as temporary with technology sunset. Building alternative pathways (desalination, efficiency) that could dissolve the zero-sum allocation problem.
 *   - Colonial Water Law Legacy: Institutional persistence (institutional/arbitrage) — Historical treaties persist through inertia despite suboptimal allocation terms. Theater-driven maintenance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geopolitical_water_conflict, 0.58).
domain_priors:suppression_score(geopolitical_water_conflict, 0.68).
domain_priors:theater_ratio(geopolitical_water_conflict, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geopolitical_water_conflict, extractiveness, 0.58).
narrative_ontology:constraint_metric(geopolitical_water_conflict, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(geopolitical_water_conflict, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geopolitical_water_conflict, tangled_rope).
narrative_ontology:human_readable(geopolitical_water_conflict, "Geopolitical Water Conflict as Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(geopolitical_water_conflict, "geopolitics/resource_governance/environmental").

domain_priors:requires_active_enforcement(geopolitical_water_conflict).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geopolitical_water_conflict, upstream_riparian_state).
narrative_ontology:constraint_beneficiary(geopolitical_water_conflict, industrial_water_users).
narrative_ontology:constraint_beneficiary(geopolitical_water_conflict, hydroelectric_power_operators).
narrative_ontology:constraint_victim(geopolitical_water_conflict, downstream_riparian_state).
narrative_ontology:constraint_victim(geopolitical_water_conflict, subsistence_farmers).
narrative_ontology:constraint_victim(geopolitical_water_conflict, fishing_communities).
narrative_ontology:constraint_victim(geopolitical_water_conflict, regional_ecosystem_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM SUBSISTENCE COMMUNITIES (SNARE) — Trapped by geographic dependence on river flow. No alternative water sources, no exit option. Bears full extraction cost through reduced irrigation, seasonal droughts, ecosystem collapse. Powerless to negotiate or exit. Suppression is structural: water is non-substitutable, geography is fixed, alternatives require massive capital investment. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(geopolitical_water_conflict, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DOWNSTREAM RIPARIAN STATE (TANGLED ROPE) — Constrained by diplomatic costs of conflict escalation and economic dependency on trade with upstream state. Also benefits from coordination: joint water management treaties enable predictable planning, flood control benefits, and irrigation scheduling. Genuine coordination function (joint dams, information sharing) exists alongside asymmetric extraction (upstream state controls flow). Significant agency but high exit costs (military conflict, economic sanctions, resource warfare). Moderate experienced extraction.
constraint_indexing:constraint_classification(geopolitical_water_conflict, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERNATIONAL WATER TREATIES & ARBITRAGE (ROPE) — Treaty bodies (UN Watercourses Convention, bilateral frameworks) experience the constraint as pure coordination: sharing hydrological data, establishing dispute resolution, managing seasonal variation. These actors have arbitrage options (switch between mediation frameworks, invoke international law). Net beneficiary position: treaty regimes gain legitimacy and enforcement mandates. Extracted value flows toward institutional maintenance. Low experienced extraction because arbitrage options reduce lock-in.
constraint_indexing:constraint_classification(geopolitical_water_conflict, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UPSTREAM RIPARIAN STATE (TANGLED ROPE) — Primary beneficiary but also constrained by international law, downstream state's potential military response, and long-term coalition pressure (other upstream states with shared aquifers). Genuine coordination function: seasonal flow agreements, flood warning systems, joint development of water resources. Active enforcement: dam operations, allocation protocols, dispute resolution mechanisms. Mixed position: net extraction but structured through treaties that provide coordination benefits to both sides. Constrained exit (cannot unilaterally breach treaties without regional instability).
constraint_indexing:constraint_classification(geopolitical_water_conflict, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CLIMATE ADAPTATION & DESALINATION COALITION (SCAFFOLD) — Organized technical actors (water technology firms, climate adaptation funds, development banks) see the conflict as a temporary coordination failure with a structural sunset: desalination, water recycling, agricultural efficiency improvements, and managed aquifer recharge offer exit pathways. These technologies have scaling costs and energy requirements, but they are improving. Sunset logic: as alternatives mature (10-30 year horizon), the zero-sum allocation constraint loses force. Low theater (technical solutions are measurable). Exit is mobile for organized actors — they can pivot to alternative technologies.
constraint_indexing:constraint_classification(geopolitical_water_conflict, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COLONIAL WATER LAW LEGACY (PITON) — The constraint is partly maintained by historical treaties (Nile Waters Agreement 1959, Indus Waters Treaty 1960) that were imposed during colonialism and persist through institutional inertia. These treaties persist not because they remain functionally optimal but because renegotiating them risks conflict escalation. Theater is high: diplomatic rituals around 'consultation,' 'cooperation frameworks,' and 'basin committees' perform cooperation while actual allocation remains locked in legacy terms. The institutional structure persists despite low functional maintenance. Piton classification: high theater (0.55), low functional coordination relative to the institutional theater.
constraint_indexing:constraint_classification(geopolitical_water_conflict, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SCARCITY NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, freshwater scarcity appears as a natural constraint: finite supply, inelastic demand, fixed geography. Climate change and population growth amplify the appearance of immutability. This perspective risks naturalizing the constraint as an unavoidable law of physics rather than a product of allocation institutions, technological choices, and historical power distribution. The engine's false summit detector should flag this: the 'immutable scarcity' framing obscures contingent institutional arrangements.
constraint_indexing:constraint_classification(geopolitical_water_conflict, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: UPSTREAM INDUSTRIAL WATER USERS (TANGLED ROPE) — Powerful actors (hydroelectric operators, industrial manufacturers) are primary beneficiaries but also embedded in supply chain networks. Exit options are mobile: they can invest in water efficiency, recycling, or relocation (but not without cost). Genuine coordination: industrial ecosystems benefit from stable water supply and predictable seasonal variation (for power generation). Extraction: they capture economic rents from water access at below-market prices. Constrained but mobile: they have resources to adapt but strong incentives to maintain the current allocation regime.
constraint_indexing:constraint_classification(geopolitical_water_conflict, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geopolitical_water_conflict_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geopolitical_water_conflict, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geopolitical_water_conflict, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geopolitical_water_conflict, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geopolitical_water_conflict, TR),
    TR >= 0.70.

:- end_tests(geopolitical_water_conflict_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The upstream state captures significant economic and strategic rents through water control, but the extraction is not total — downstream state retains some agency through treaty negotiation and implicit threat of conflict escalation. The value reflects growth from 0.35 to 0.58 over the interval as climate variability tightens the margin and makes allocation decisions higher-stakes. Suppression (0.68): High. Structural barriers to exit include: geography is fixed, water is non-substitutable (biological requirement), alternative sources require massive capital investment, and military conflict risks escalate costs beyond negotiation. Cognitive suppression includes climate variability attribution (upstream state can obscure deliberate cuts by blaming drought) and naturalization of scarcity as immutable. Theater ratio (0.55): Moderate. The constraint has genuine coordination content (joint dams, flood management, seasonal planning) but also significant performative content (basin committees that lack enforcement power, cooperation declarations that precede unilateral action). Theater has increased over the interval as gap between declared cooperation and actual allocation has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon (upstream state controls allocation, downstream state has no alternative) produces radically different classifications depending on observer position. The downstream victim sees extraction with no coordination benefit (snare from their perspective). The upstream beneficiary sees coordination that enables more efficient water management (tangled rope from their perspective — genuine joint benefit exists, but extraction is asymmetric). The international institution sees pure coordination (rope perspective). The technology coalition sees the constraint as temporarily necessary but obsolete in 20-30 years (scaffold). The colonial law legacy sees institutional ritual that maintains legitimacy (piton). The analytical observer risks naturalizing allocation asymmetry as immutable scarcity (false mountain). The perspectival gaps reveal that classification depends critically on exit options and structural position, not just on the nominal 'fact' of water allocation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness depends on their structural relationship to the allocation flow. Upstream state (beneficiary + constrained exit by treaties) derives d ≈ 0.35, producing f(d) ≈ 0.25 → chi ≈ 0.58 × 0.25 × 1.0 (regional scope) → χ ≈ 0.15 (perceived as rope or low tangled rope). Downstream state (victim + constrained exit by geography/diplomacy) derives d ≈ 0.72, producing f(d) ≈ 1.15 → chi ≈ 0.58 × 1.15 × 1.0 → χ ≈ 0.67 (perceived as snare or high tangled rope). Subsistence communities (victim + trapped exit) derive d ≈ 0.95, producing f(d) ≈ 1.42 → chi ≈ 0.58 × 1.42 × 1.0 → χ ≈ 0.82 (pure snare). International institutions (beneficiary + arbitrage exit) derive d ≈ 0.15, producing f(d) ≈ -0.01 → chi ≈ 0.58 × (-0.01) × 1.0 → χ ≈ -0.006 (rope, negative extraction). The directionality computation explains why the same constraint produces different classifications: not because observers are biased, but because they occupy genuinely different structural positions with different exit costs and different extraction flows.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival decomposition: the constraint is Tangled Rope from the institutional/international treaty perspective (genuine coordination function + asymmetric extraction + active enforcement). From victim perspectives it appears as Snare (no coordination benefit perceived, pure extraction). From beneficiary perspectives it appears as Rope (coordination benefit dominates perception, extraction is seen as fair reward for management). From technology coalition it appears as Scaffold (coordination is temporary, sunset via technology). The mandatrophy question is not 'which type is correct?' but 'how do different structural positions experience this single constraint?' The tangled rope classification at the institutional power level (international treaties perspective) is the canonical type because it captures both the genuine coordination function and the asymmetric extraction simultaneously. The snare and rope classifications are perspectival reductions based on whether the observer experiences more extraction or more coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hydropolitical_coalition_threshold,
    'At what scale of water scarcity do downstream states coalesce into counter-coalition powerful enough to force renegotiation?',
    'Historical analysis of water conflict escalation trajectories; comparison of cases where downstream coalition formation succeeded vs failed; modeling of critical mass threshold for military/economic retaliation capacity',
    'If threshold is low (early coalition): snare perspective transitions to tangled_rope (victims gain agency). If threshold is high (delayed coalition): snare classification persists longer, enabling extraction to accumulate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hydropolitical_coalition_threshold, empirical, 'Threshold for downstream coalition formation and renegotiation power').

omega_variable(
    technology_substitution_feasibility,
    'Are desalination and water recycling genuinely capable of replacing upstream allocation control, or do energy costs and environmental effects create new extraction mechanisms?',
    'Long-term cost curves for desalination; energy source decarbonization requirements; brine disposal and environmental impact assessment; comparison of lifecycle water costs via technology vs via traditional allocation',
    'If feasible: scaffold sunset is real, extraction declines as alternatives scale. If infeasible: scaffold perspective is aspirational theater masking continued extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_substitution_feasibility, empirical, 'Whether technology can substitute for geopolitical allocation').

omega_variable(
    treaty_enforcement_mechanism,
    'Does the international water treaty framework actually enforce equal treatment between riparian states, or does it embed upstream advantage through procedural channels?',
    'Analysis of dispute resolution outcomes (arbitration awards, ICJ cases); comparison of water allocation changes before/after treaty invocation; assessment of which party typically prevails in disputes',
    'If enforced equally: rope perspective is accurate, international institutions provide genuine neutral coordination. If embedded asymmetry: rope perspective is naive, treaty framework is capture mechanism for institutional power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_enforcement_mechanism, empirical, 'Whether water treaties enforce equal treatment or embed upstream advantage').

omega_variable(
    climate_variability_masking,
    'Does upstream state attribute water flow reductions to climate variability and thereby obscure deliberate allocation cutbacks?',
    'Cross-year analysis of dam release patterns vs precipitation data; identification of discretionary allocation periods; comparison of upstream/downstream climate impact narratives',
    'If masking occurs: suppression metric underestimates cognitive component; theater ratio increases; extractiveness classification shifts upward. Constraint becomes less tractable because victim accountability is obscured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_variability_masking, empirical, 'Whether climate variability attribution masks deliberate allocation reductions').

omega_variable(
    regional_power_dynamics_shift,
    'Does shifts in regional military balance (weapons capability, alliance formation) alter the extractiveness experienced by downstream state?',
    'Historical correlation between military balance shifts and water allocation changes; analysis of negotiation outcomes after regional power transitions; modeling of deterrence credibility against upstream state',
    'If military balance is decisive: downstream coalition formation becomes viable, tangled_rope classification becomes stable. If military balance is irrelevant: downstream state remains trapped regardless of military capability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_power_dynamics_shift, empirical, 'Whether regional military balance shifts alter water negotiation outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geopolitical_water_conflict, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geowater_tr_t0, geopolitical_water_conflict, theater_ratio, 0, 0.38).
narrative_ontology:measurement(geowater_tr_t10, geopolitical_water_conflict, theater_ratio, 10, 0.48).
narrative_ontology:measurement(geowater_tr_t20, geopolitical_water_conflict, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(geowater_be_t0, geopolitical_water_conflict, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(geowater_be_t10, geopolitical_water_conflict, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(geowater_be_t20, geopolitical_water_conflict, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geopolitical_water_conflict, resource_allocation).
narrative_ontology:boltzmann_floor_override(geopolitical_water_conflict, 0.18).
narrative_ontology:affects_constraint(geopolitical_water_conflict, transboundary_aquifer_depletion).
narrative_ontology:affects_constraint(geopolitical_water_conflict, hydroelectric_dam_deployment).
narrative_ontology:affects_constraint(geopolitical_water_conflict, regional_security_alliance_cohesion).

% DUAL FORMULATION NOTE:
% Geopolitical water conflict is the institutional-level constraint that aggregates multiple downstream effects: specific bilateral allocation disputes (Nile, Indus, Mekong, Jordan) have their own constraint stories with narrower scope and sometimes different ε values. This story represents the general pattern across all transboundary water systems. The network links indicate that changes in this general constraint affect specific bilateral disputes and also feed into broader regional security dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geopolitical_water_conflict, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
