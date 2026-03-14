% ============================================================================
% CONSTRAINT STORY: water_scarcity_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_water_scarcity_governance, []).

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
 *   constraint_id: water_scarcity_governance
 *   human_readable: Water Scarcity Governance and Allocation Asymmetry
 *   domain: environmental_resource_governance
 *
 * SUMMARY:
 *   Water scarcity governance represents a canonical Tangled Rope constraint
 *   operating at multiple scales simultaneously — from household water access
 *   to interstate river agreements to global food security. The constraint
 *   exhibits genuine coordination function (allocation rules, dispute
 *   resolution, infrastructure planning) alongside asymmetric extraction
 *   (benefits flow disproportionately to large-scale agricultural and
 *   industrial users while costs fall on subsistence farmers and future
 *   generations through groundwater depletion). The 30-year observation
 *   window captures a gradual intensification of extraction (ε rising from
 *   0.35 to 0.58) driven by population growth, agricultural expansion, and
 *   declining precipitation, while theater ratio (performative activity in
 *   bureaucratic governance) increases modestly (0.42 to 0.55) as
 *   institutions add monitoring and compliance mechanisms without
 *   fundamentally addressing allocation asymmetry. The constraint spans from
 *   local/regional (subsistence farmer trapped in geographic location with no
 *   alternative) through national (rural communities constrained by
 *   infrastructure dependence and legal frameworks) to international
 *   (downstream nations negotiating with upstream states over river
 *   allocation) to civilizational (deep time perspective where physical water
 *   limits appear immutable). The perspectival gap reveals how the same
 *   structural constraint appears as natural law from hydrology, as
 *   negotiable from climate adaptation coalitions, as degraded bureaucracy
 *   from institutional analysts, and as catastrophic extraction from
 *   powerless agents.
 *
 * KEY AGENTS:
 *   - Subsistence Farmers: Primary victims (powerless/trapped) — geographic location, lack of capital, legal junior status in water rights create insurmountable barriers to alternative water sources
 *   - Rural Poor Communities: Secondary victims (moderate/constrained) — depend on formal allocations for drinking water and small-scale agriculture; constrained by infrastructure costs and legal status but some agency through collective action
 *   - Large Agribusiness: Primary beneficiaries (institutional/arbitrage) — senior water rights, capital for irrigation infrastructure, market access for alternative crops, political influence on allocation policy
 *   - Industrial Water Users: Secondary beneficiaries (powerful/arbitrage) — can relocate, access alternative supplies, lobby for preferential pricing and reliable allocation guarantees
 *   - Urban Wealthy Districts: Tertiary beneficiaries (powerful/mobile) — municipal water priority, economic capacity to pay for conservation or alternative supplies, ability to externalize water scarcity to rural regions
 *   - Downstream Ecosystems: Victims without representation (powerless/trapped) — depend on minimum flow requirements that are consistently violated; face extinction if extraction continues at current trajectory
 *   - Future Generations: Structural victims (powerless/trapped) — bear costs of aquifer depletion, ecosystem collapse, and climate instability driven by current extraction patterns; no exit option from inherited water scarcity
 *   - Downstream Nation: Institutional victim (powerful/constrained) — negotiates within international treaty frameworks that lock in asymmetric allocation; can use political leverage but cannot unilaterally exit riparian agreements
 *   - Water Authority Bureaucracy: Institutional actor maintaining coordination mechanism but with degraded function (institutional/mobile but resistant to reform)
 *   - Climate Adaptation Coalition: Organized agents seeing temporary constraint with technological sunset (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(water_scarcity_governance, 0.58).
domain_priors:suppression_score(water_scarcity_governance, 0.68).
domain_priors:theater_ratio(water_scarcity_governance, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(water_scarcity_governance, extractiveness, 0.58).
narrative_ontology:constraint_metric(water_scarcity_governance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(water_scarcity_governance, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(water_scarcity_governance, tangled_rope).
narrative_ontology:human_readable(water_scarcity_governance, "Water Scarcity Governance and Allocation Asymmetry").
narrative_ontology:topic_domain(water_scarcity_governance, "environmental_resource_governance").

domain_priors:requires_active_enforcement(water_scarcity_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(water_scarcity_governance, large_agribusiness).
narrative_ontology:constraint_beneficiary(water_scarcity_governance, industrial_users).
narrative_ontology:constraint_beneficiary(water_scarcity_governance, urban_wealthy_districts).
narrative_ontology:constraint_victim(water_scarcity_governance, subsistence_farmers).
narrative_ontology:constraint_victim(water_scarcity_governance, rural_poor_communities).
narrative_ontology:constraint_victim(water_scarcity_governance, downstream_ecosystems).
narrative_ontology:constraint_victim(water_scarcity_governance, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSISTENCE FARMER (SNARE) — Trapped by geographic location, lack of capital for alternative water sources, and legal frameworks that prioritize senior water rights held by agribusiness. Bears extraction through reduced allocation, forced crop changes, and eventual agricultural abandonment. No exit path within the constraint's time horizon.
constraint_indexing:constraint_classification(water_scarcity_governance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL COMMUNITY (TANGLED ROPE) — Constrained by infrastructure dependence and limited capital for alternative water systems. Also benefits from governance institutions that provide some allocational rules and dispute resolution. Mixed extraction and coordination — governance enables some water security while enforcing asymmetric distribution favoring upstream industrial users.
constraint_indexing:constraint_classification(water_scarcity_governance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE AGRIBUSINESS (ROPE) — Primary beneficiary with institutional power and arbitrage options (can relocate, diversify crops, access alternative supplies through markets). Experiences the constraint as coordination of resource access; governance rules provide legal certainty and priority allocation. Genuine coordination benefit alongside extraction of value.
constraint_indexing:constraint_classification(water_scarcity_governance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOWNSTREAM NATION (TANGLED ROPE) — Constrained by international treaty frameworks and upstream states' prior allocation rights. Receives some benefit from treaty coordination mechanisms (predictable allocations, dispute resolution) but bears extraction through reduced availability and ecosystem degradation. Powerful enough to negotiate but unable to fully exit interstate water governance regimes.
constraint_indexing:constraint_classification(water_scarcity_governance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: WATER AUTHORITY BUREAUCRACY (PITON) — Maintains allocation mechanisms that were designed for different climate conditions and population distributions. Theater ratio reflects that bureaucratic processes (permitting, monitoring, enforcement) consume resources without proportionally improving allocation efficiency. The institution persists through inertia despite reduced functional fit. Paradoxically mobile (could reform itself) but resistant due to entrenched interests.
constraint_indexing:constraint_classification(water_scarcity_governance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE ADAPTATION COALITION (SCAFFOLD) — Organized agents (development banks, climate agencies, NGOs) see water scarcity governance as a temporary coordination failure resolvable through technology and policy reform. Sunset clause: as desalination, wastewater recycling, and demand-reduction technologies mature and as international water agreements are renegotiated with climate projections, the current allocation extraction mechanism loses force. Estimated sunset: 15-30 years depending on technology deployment and political will.
constraint_indexing:constraint_classification(water_scarcity_governance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PHYSICAL HYDROLOGY (MOUNTAIN) — From a deep temporal perspective, the constraint appears as a natural law: precipitation patterns and aquifer recharge rates are fixed physical facts. Any civilization exceeding sustainable extraction rates faces immutable scarcity. However, this perspective risks naturalizing what is actually contingent institutional choice: the distribution of scarcity is not physically determined but politically constructed through allocation rules, pricing mechanisms, and investment patterns.
constraint_indexing:constraint_classification(water_scarcity_governance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(water_scarcity_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(water_scarcity_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(water_scarcity_governance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(water_scarcity_governance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(water_scarcity_governance, TR),
    TR >= 0.70.

:- end_tests(water_scarcity_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Large-scale agricultural and industrial users capture disproportionate allocation under senior water rights frameworks while subsistence farmers and ecosystems bear scarcity costs. The extraction is not maximal (0.66+) because formal governance institutions provide some allocation rules and dispute resolution, and some coordination of cross-border water sharing does occur. Suppression (0.68): High. Powerless agents face multiple reinforcing barriers: geographic immobility (cannot relocate to water-abundant regions without abandoning livelihoods), economic dependency (lack capital for alternative water infrastructure), legal status (junior water rights), information asymmetries (unaware of their negotiating capacity), and institutional capture (water authorities reflect beneficiary preferences). Collectively these create strong suppression without requiring explicit coercion. Theater ratio (0.55): Moderate. Governance institutions maintain elaborate monitoring, permitting, and enforcement processes that consume resources but often fail to prevent illegal use or adjust allocations as climate projections change. The theater increased from 0.42 to 0.55 over the observation window as institutions added compliance mechanisms in response to scarcity intensification without fundamentally reforming allocation asymmetry. Beneficiaries (large agribusiness, industrial users, wealthy urban) coordinate around water access and benefit from legal frameworks that protect their allocations. Victims (subsistence farmers, rural poor, ecosystems, future generations) bear costs of scarcity intensification and institutional prioritization of high-value uses over subsistence and ecological needs.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence across power positions and time horizons. Subsistence farmers see pure extraction (Snare) with no exit — their livelihoods are destroyed while agribusiness expands irrigation. Rural communities see mixed coordination and extraction (Tangled Rope) — governance provides some water security but at costs that prevent accumulation. Large agribusiness sees coordination (Rope) — water governance provides legal certainty, infrastructure investment, and protection of senior rights. Downstream nations see coordination-extraction hybrid at international scale (Tangled Rope) — treaties provide predictability but encode upstream power asymmetries. Water bureaucracy sees its own degraded function (Piton) — institutional mechanisms persist through inertia despite declining effectiveness. Climate adaptation coalition sees temporary problem with technological solution and sunset (Scaffold) — desalination and recycling will eventually break the constraint. Deep hydrology perspective sees natural law (Mountain) — precipitation patterns and aquifer limits are physical facts. The perspectival gap reveals that the 'natural scarcity' framing naturalizes what is actually a political allocation problem: different distributions of scarcity could be constructed through different governance rules, but current institutions lock in the observed extraction pattern.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the extraction flow. Subsistence farmers have zero exit options within their time horizon (trapped) and are victims of allocation asymmetry (high d toward 1.0, experiencing maximum χ). Rural communities have constrained exit (high infrastructure/relocation costs) and bear significant costs (d ≈ 0.70, moderate-high χ). Large agribusiness has arbitrage-level exit (can adapt crops, move operations, access markets) and benefits from allocation (d ≈ 0.10, negative or low χ) — they perceive coordination rather than extraction. Downstream nations have power but constrained exit within treaty frameworks (d ≈ 0.65, moderate χ experienced as Tangled Rope). Water bureaucracy has mobile exit (could reform) but faces resistance from beneficiary interests; analytical observer derives d from the full structural picture. The piton classification does not come from high d but from high theater ratio — the institution maintains performative processes despite degraded function.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in water scarcity governance is resolved through temporal decomposition. At the immediate/biographical horizon, the constraint appears as pure extraction (Snare) for powerless agents and as coordination (Rope) for beneficiaries — these are both structurally accurate from their positions. At the generational horizon, the constraint begins to reveal itself as temporary (Scaffold) as technology deployment timelines and adaptation pathways become visible — organized agents can plan exits. At the civilizational horizon, the question becomes whether the constraint is an immutable physical law (Mountain) or a contingent institutional arrangement (Tangled Rope subject to renegotiation). The mandatrophy is resolved by recognizing that the classification depends on the agent's structural position AND time horizon. The constraint is a Snare for powerless agents at biographical horizon, a Rope for beneficiaries at immediate horizon, a Scaffold for organized coalitions at generational horizon, and a questioned Mountain (likely to be revealed as false summit) at civilizational horizon. All are structurally legitimate readings of the same data from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allocation_baseline_ambiguity,
    'What constitutes the ''fair'' or ''natural'' baseline allocation? Historic use, equal per-capita, ecosystem needs, or economic productivity?',
    'Comparative analysis of allocation frameworks across jurisdictions; historical reconstruction of pre-scarcity allocation patterns; ecosystem flow requirements from hydrological studies',
    'If historic use is baseline: current distribution is natural law (Mountain). If equal per-capita: current distribution is clear extraction (Snare). If ecosystem needs dominate: extraction levels would be classified as catastrophic (Snare from ecosystem perspective). Choice of baseline determines classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(allocation_baseline_ambiguity, conceptual, 'Definition of fair baseline allocation determines classification').

omega_variable(
    technology_substitution_feasibility,
    'Can desalination, recycling, and demand reduction actually scale to replace extracted freshwater within the policy horizon, or do thermodynamic/economic barriers impose permanent limits?',
    'Cost trajectory analysis for desalination and recycling; energy requirements relative to renewable capacity; deployment timelines in comparable jurisdictions; basin-level water balance projections with technology deployment',
    'If substitution is feasible: Scaffold sunset is credible, classification remains Tangled Rope. If substitution barriers are permanent: current extraction mechanism becomes endemic, pushing classification toward Snare for powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_substitution_feasibility, empirical, 'Whether technology can substitute for extracted freshwater').

omega_variable(
    interstate_power_asymmetry_lock,
    'Do upstream riparian rights embedded in international treaties represent immutable historical power asymmetries or renegotiable institutional arrangements?',
    'Analysis of treaty renegotiation precedents (Colorado River, Nile, Ganges); downstream state exit capacity (economic leverage, alternative supplies, military capability); feasibility of multilateral agreement revision',
    'If treaties are locked: downstream extraction is trapped within international law (Mountain-like immutability). If renegotiable: extraction is a Tangled Rope subject to power dynamics and negotiation. Classification affects downstream state''s exit_options assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interstate_power_asymmetry_lock, empirical, 'Immutability of riparian treaties and allocation baselines').

omega_variable(
    informal_extraction_mechanism,
    'How much of the extraction occurs through informal/illegal use (groundwater pumping, dam bypass, night-time diversion) rather than formal allocation?',
    'Satellite-based water use monitoring; aquifer depletion rate analysis; comparison of formal allocations to hydrological balance estimates; qualitative assessment of enforcement capacity',
    'If informal use is dominant: suppression may be understated (enforcement failures mean agents find workarounds, reducing experienced suppression). If formal allocation is accurate: suppression is as measured. High informal extraction suggests governance institutions are degraded (Piton element higher than estimated).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(informal_extraction_mechanism, empirical, 'Proportion of extraction through informal vs formal mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(water_scarcity_governance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(water_tr_t0, water_scarcity_governance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(water_tr_t15, water_scarcity_governance, theater_ratio, 15, 0.51).
narrative_ontology:measurement(water_tr_t30, water_scarcity_governance, theater_ratio, 30, 0.55).
narrative_ontology:measurement(water_tr_t8, water_scarcity_governance, theater_ratio, 8, 0.47).

% Extraction over time
narrative_ontology:measurement(water_be_t0, water_scarcity_governance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(water_be_t15, water_scarcity_governance, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(water_be_t30, water_scarcity_governance, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(water_be_t8, water_scarcity_governance, base_extractiveness, 8, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(water_scarcity_governance, resource_allocation).
narrative_ontology:boltzmann_floor_override(water_scarcity_governance, 0.18).
narrative_ontology:affects_constraint(water_scarcity_governance, groundwater_depletion_lock).
narrative_ontology:affects_constraint(water_scarcity_governance, agricultural_intensity_scaling).
narrative_ontology:affects_constraint(water_scarcity_governance, ecosystem_flow_requirement_negotiation).
narrative_ontology:affects_constraint(water_scarcity_governance, food_security_externality).
narrative_ontology:affects_constraint(water_scarcity_governance, international_water_treaty_lock).

% DUAL FORMULATION NOTE:
% Water scarcity governance decomposes into multiple structurally distinct constraints: groundwater depletion (ε ≈ 0.72, geological lock-in), agricultural scaling (ε ≈ 0.55, policy-driven), ecosystem flows (ε ≈ 0.68, ecological constraint), food security (ε ≈ 0.62, geopolitical extraction), and treaty lock (ε ≈ 0.58, institutional path dependence). This story addresses the governance allocation mechanism itself. Upstream constraints drive the scarcity conditions; downstream constraints inherit the allocation asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(water_scarcity_governance, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
