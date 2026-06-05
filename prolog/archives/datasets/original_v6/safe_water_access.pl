% ============================================================================
% CONSTRAINT STORY: safe_water_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_safe_water_access, []).

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
 *   constraint_id: safe_water_access
 *   human_readable: Safe Water Access as Infrastructure Constraint
 *   domain: public_health/infrastructure/political_economy
 *
 * SUMMARY:
 *   Access to safe water is framed globally as a human right and a public
 *   health imperative, yet over 2 billion people lack reliable access to
 *   safely managed water. The constraint exhibits the tangled structure of
 *   modern infrastructure: genuine coordination function (water systems solve
 *   collective action problems, prevent waterborne epidemics, enable economic
 *   activity) combined with systematic extraction from powerless agents. The
 *   extractiveness has increased over the measurement interval (0.42 to 0.58)
 *   as infrastructure systems have been constructed but maintenance has been
 *   systematically under-resourced, shifting from a capital-investment
 *   problem to an operational extraction mechanism. Theater ratio has risen
 *   modestly (0.38 to 0.48) as international commitments (SDG 6, water
 *   treaties) have proliferated without corresponding enforcement or resource
 *   allocation. The constraint is not a mountain of absolute scarcity — most
 *   regions with safe water access failures have sufficient water resources;
 *   the constraint is political-economic distribution. However, in genuinely
 *   arid zones, some component is immutable (pure water insufficiency),
 *   creating a mixed constraint family.
 *
 * KEY AGENTS:
 *   - Low-income urban households: Primary victims (powerless/trapped) — no material alternatives within economic reach; bear disease burden and time costs of collection/treatment
 *   - Rural agrarian populations: Primary victims (powerless/trapped) — geographically isolated, no infrastructure investment, intergenerational transmission of constraint
 *   - Middle-income neighborhoods: Secondary victims (moderate/constrained) — benefit from coordination (piped systems reduce collection burden) but bear extraction through unreliable service and high costs
 *   - Public water utilities: Primary beneficiary (institutional/arbitrage) — capture revenue streams, political legitimacy, and coordination rent; operate monopolies with limited accountability
 *   - Large industrial consumers: Secondary beneficiary (powerful/arbitrage) — preferential allocation during scarcity, negotiated rates, guaranteed supply; can exit via relocation or alternative sources
 *   - International governance apparatus: Institutional theater (institutional/arbitrage) — SDG 6, water treaties, UN agencies declare commitments with minimal enforcement or funding
 *   - Analytical observer: Civilizational risk (analytical/analytical) — naturalizes political distribution as immutable scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(safe_water_access, 0.58).
domain_priors:suppression_score(safe_water_access, 0.65).
domain_priors:theater_ratio(safe_water_access, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(safe_water_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(safe_water_access, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(safe_water_access, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(safe_water_access, tangled_rope).
narrative_ontology:human_readable(safe_water_access, "Safe Water Access as Infrastructure Constraint").
narrative_ontology:topic_domain(safe_water_access, "public_health/infrastructure/political_economy").

domain_priors:requires_active_enforcement(safe_water_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(safe_water_access, water_utilities).
narrative_ontology:constraint_beneficiary(safe_water_access, large_industrial_consumers).
narrative_ontology:constraint_beneficiary(safe_water_access, wealthy_municipalities).
narrative_ontology:constraint_victim(safe_water_access, low_income_households).
narrative_ontology:constraint_victim(safe_water_access, rural_populations).
narrative_ontology:constraint_victim(safe_water_access, informal_settlements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME HOUSEHOLD (SNARE) — Cannot exit dependency on unsafe water systems. Bears full cost of disease burden, medical expenses, and time spent collecting/boiling water. No structural alternative within economic constraints. Zero degrees of freedom.
constraint_indexing:constraint_classification(safe_water_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RURAL AGRARIAN POPULATION (SNARE) — Trapped by geographic isolation and lack of infrastructure investment. Limited capacity to dig wells or install treatment. Generational time horizon shows intergenerational transmission of constraint — children born into same unsafe systems as parents. Exit requires migration (itself costly and risky).
constraint_indexing:constraint_classification(safe_water_access, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: MIDDLE-INCOME NEIGHBORHOOD (TANGLED ROPE) — Has some access coordination benefit (piped systems reduce collection burden) but bears significant extraction through unreliable service, high water bills relative to income, and inferior water quality. Can organize (neighborhood associations) but exit to independent systems is costly. Mixed benefit and burden.
constraint_indexing:constraint_classification(safe_water_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC WATER UTILITY (ROPE) — Experiences safe water access constraint as coordination mechanism: organizing water supply prevents epidemics, enables economic activity, solves collective action problem of individual well-drilling. Benefits from revenue streams and political legitimacy. Has arbitrage options (operate more efficiently, expand services, adjust pricing). Net beneficiary.
constraint_indexing:constraint_classification(safe_water_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LARGE INDUSTRIAL CONSUMER (TANGLED ROPE) — Coordinates production and supply chain through reliable water access but extracts through preferential allocation, negotiated rates, and guaranteed supply during scarcity. Can arbitrage to alternative sources or relocate. Powerful negotiating position within the constraint system.
constraint_indexing:constraint_classification(safe_water_access, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL WATER GOVERNANCE (PITON) — UN Sustainable Development Goal 6 (Clean Water and Sanitation) and related water-cooperation treaties create theatrical coordination performance with minimal enforcement. Goals are declared but funding mechanisms are inadequate, compliance is unverified, and real extraction barriers remain unchanged. Maintains institutional theater of commitment without functional change.
constraint_indexing:constraint_classification(safe_water_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL SCARCITY (MOUNTAIN) — From a civilizational view, water scarcity in arid regions and limits on purification technology constitute an immutable constraint. Some regions lack sufficient water; treatment scales with energetic cost. However, structural data contradicts this: 2+ billion people lack safe water access in regions with sufficient water resources. The false summit reveals naturalization of a political-economic distribution problem as a natural-law scarcity.
constraint_indexing:constraint_classification(safe_water_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(safe_water_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(safe_water_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(safe_water_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(safe_water_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(safe_water_access, TR),
    TR >= 0.70.

:- end_tests(safe_water_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, and increasing over the measurement interval. Base extractiveness reflects systematic concentration of safe water access in wealthy/industrial populations while powerless agents are trapped in unsafe systems. The increase from 0.42 to 0.58 over 20 years indicates that as infrastructure has been constructed in wealthy areas, extraction mechanisms have solidified — the utility system now actively maintains inequality through pricing, service allocation, and maintenance prioritization. Suppression (0.65): High. Significant barriers include lack of capital for independent systems, geographic isolation, regulatory barriers to alternative water sources (groundwater extraction regulations that freeze current allocations), and political powerlessness. Suppression is structural (external barriers) rather than internalized. Theater ratio (0.48): Moderate. Water systems have genuine coordination function — they do prevent epidemics and enable economic activity — so theater is not dominant. But theater is rising: international commitments (SDG 6) generate reporting and conferences without enforcement, and utility performance metrics are often theatrical (percentage of population connected) rather than functional (water safety and reliability for those connected).
 *
 * PERSPECTIVAL GAP:
 *   The gap between victim and beneficiary perspectives is maximal here. Low-income households see Snare (pure extraction, no coordination benefit to them — the system traps them in unsafe water). Water utilities see Rope (coordination mechanism providing essential service). Industrial consumers see Rope (access to reliable supply for production). The middle-income neighborhood sees the true hybrid: Tangled Rope (benefits from piped system and coordination, but extracted through cost and unreliability). The international apparatus sees Piton (the performative theater of global commitments masking unchanged local reality). The analytical observer risks Mountain (water scarcity as natural limit) but the falsity is revealed: water scarcity is geographically and politically contingent, not universal. The perspectival gap is not perceptual (people misunderstanding the same reality) but structural (they experience radically different extraction flows from the same constraint system).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Low-income households are victims with trapped exit → maximum d (0.95) → maximum f(d) → experience maximum chi. Rural populations similarly trapped but with generational horizon → high d → high chi. Water utilities are beneficiaries with arbitrage options (can operate more or less efficiently, can expand or contract service areas) → low d → low or negative chi (experienced as coordination mechanism, not extraction). Industrial consumers are beneficiaries with powerful status and arbitrage options → very low d → negative chi (perceive utility as providing valuable service). The piton perspective (international governance) has institutional power with arbitrage (can fund or defund initiatives) but experiences the constraint as empty ritual — low chi from arbitrage position but high theater obscures the extraction happening at victim levels.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that safe water access is genuinely a mixed coordination-extraction system (Tangled Rope is the analytical position), while simultaneously appearing as pure extraction to trapped agents and pure coordination to beneficiaries. The mandatrophy question is 'Is this coordination or extraction?' — the answer is 'It is both, and which you experience depends on your structural position.' The constraint cannot be reduced to either pure type without loss of diagnostic power. The piton perspective reveals theatrical elements (international commitments without enforcement) that might otherwise be invisible. The mountain perspective reveals the risk of naturalizing political distribution as immutable scarcity — this risk is real in arid regions where absolute water insufficiency exists, but in most regions with safe water access failures, the scarcity is political, not physical. A future resolution would require either (a) universal access through reallocation (political choice), (b) acceptance of differential access (explicit inequality), or (c) decentralized systems that remove the utility monopoly (structural change).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_capital_vs_maintenance,
    'Is safe water access constrained by capital investment (building systems) or operational maintenance (keeping systems functional)?',
    'Comparative analysis of regions with infrastructure but service collapse vs regions lacking infrastructure entirely. Tracking maintenance budgets vs capital budgets in water utility allocations.',
    'If capital-constrained: constraint is a coordination problem (Rope/Tangled Rope). If maintenance-constrained: constraint is primarily extraction (Snare/Piton), since infrastructure exists but extraction takes maintenance resources from system reliability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_capital_vs_maintenance, empirical, 'Capital investment vs operational maintenance constraints').

omega_variable(
    willingness_to_pay_genuine_demand,
    'Low take-up of piped water in some regions: is this suppression (target cannot afford) or genuine preference (target does not value piped system)?',
    'Randomized pricing experiments, ethnographic research on value perception, willingness-to-pay studies that control for information asymmetry.',
    'If suppression: classification shifts toward Snare. If genuine low demand: constraint is over-specified (people have opted for alternative water sources with full information). Affects both ε and suppression metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(willingness_to_pay_genuine_demand, empirical, 'Whether low water uptake reflects suppression or genuine preference').

omega_variable(
    decentralized_vs_centralized_extraction,
    'Does distributed water access (boreholes, rainwater harvesting, small treatment systems) reduce extraction, or does centralized utility extraction get replaced by extraction by local gatekeepers (water committee capture, informal pricing)?',
    'Longitudinal studies of decentralized water access projects; documentation of gatekeeper capture patterns in community water committees; comparison of cost structures and equity outcomes.',
    'If decentralized reduces extraction: constraint can be reframed as coordination problem solvable at smaller scale. If replaced by local extraction: constraint is reframed rather than resolved — extraction mechanism relocates rather than disappears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_vs_centralized_extraction, empirical, 'Whether decentralized water systems eliminate or relocate extraction mechanisms').

omega_variable(
    climate_scarcity_irreducibility,
    'In regions where precipitation is genuinely insufficient (arid/semi-arid zones), is water scarcity a mountain (immutable) or is current distribution mechanism itself unjust extraction?',
    'Engineering analysis of minimum water requirements vs available renewable sources; documentation of non-water uses in the region; analysis of historical/alternative allocation mechanisms.',
    'If insufficient water: some extraction is immutable (mountain component). If sufficient but inequitably allocated: constraint is politically constructed Snare, not natural scarcity. Most regions show mixed: some absolute scarcity + significant extractive reallocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_scarcity_irreducibility, conceptual, 'Irreducibility of water scarcity vs political inequality in allocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(safe_water_access, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(water_tr_t0, safe_water_access, theater_ratio, 0, 0.38).
narrative_ontology:measurement(water_tr_t10, safe_water_access, theater_ratio, 10, 0.44).
narrative_ontology:measurement(water_tr_t20, safe_water_access, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(water_be_t0, safe_water_access, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(water_be_t10, safe_water_access, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(water_be_t20, safe_water_access, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(safe_water_access, resource_allocation).
narrative_ontology:boltzmann_floor_override(safe_water_access, 0.18).
narrative_ontology:affects_constraint(safe_water_access, sanitation_access).
narrative_ontology:affects_constraint(safe_water_access, hygiene_disease_transmission).
narrative_ontology:affects_constraint(safe_water_access, water_scarcity_agriculture).
narrative_ontology:affects_constraint(safe_water_access, utility_monopoly_pricing).

% DUAL FORMULATION NOTE:
% Safe water access decomposes into upstream constraints (water resource availability, scarcity in arid regions) and downstream constraints (utility monopoly extraction, pricing mechanisms, maintenance prioritization). This story captures the distribution and access mechanism; upstream scarcity constraints should be separate stories with their own ε values. Industrial water usage, agricultural demand, and urban consumption create competing extraction mechanisms — each could be a separate story linked via network. The constraint family includes: water_resource_scarcity (ε≈0.20, may be mountain in arid regions), water_utility_monopoly (ε≈0.58, this story), sanitation_access (ε≈0.55, parallel to water), and disease_transmission_via_unsafe_water (ε≈0.40, consequence story).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(safe_water_access, powerful, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
