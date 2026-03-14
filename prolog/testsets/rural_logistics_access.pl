% ============================================================================
% CONSTRAINT STORY: rural_logistics_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rural_logistics_access, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rural_logistics_access
 *   human_readable: Rural Logistics Access Constraint
 *   domain: economic/infrastructure
 *
 * SUMMARY:
 *   Rural logistics access represents a structural constraint where
 *   geographic distance combines with institutional consolidation to create
 *   asymmetric extraction from rural producers and merchants. The constraint
 *   operates at multiple scales — individual merchant transactions, regional
 *   supply chains, national distribution networks — and exhibits different
 *   classificatory properties depending on the observer's structural
 *   position. Urban logistics operators experience the constraint as a solved
 *   coordination problem (rope): consolidating dispersed rural demand into
 *   hub-and-spoke networks reduces transport inefficiency and enables scale
 *   economies. Rural merchants experience the constraint as inescapable
 *   extraction (snare): geographic isolation and lack of alternative
 *   suppliers force acceptance of distance-premium pricing. Intermediate
 *   actors like small farm operators and regional governments experience
 *   mixed coordination and extraction (tangled rope): the centralized system
 *   solves genuine aggregation problems while extracting asymmetric margins.
 *   Degraded rural cooperatives that once provided alternative logistics
 *   channels now serve primarily theatrical functions (piton), maintained
 *   through institutional inertia rather than utility. Digital logistics
 *   technologies and platform cooperatives represent emerging alternative
 *   pathways with genuine sunset logic (scaffold): automation, distributed
 *   fulfillment, and decentralized routing reduce the inherent cost penalty
 *   of distance over a 15-20 year horizon. The constraint demonstrates how
 *   seemingly natural (immutable geographic distance) and institutional
 *   (margin capture, infrastructure neglect, regulatory choices) components
 *   intertwine, risking false naturalization by analytical observers who
 *   treat the constraint as a law of physics rather than a contingent
 *   institutional arrangement.
 *
 * KEY AGENTS:
 *   - Rural Merchants: Primary victim (powerless/trapped) — bear full distance premium cost with no exit options or negotiating power
 *   - Small Farm Operators: Secondary victim (moderate/constrained) — face high transport costs but benefit from some aggregation coordination; can potentially exit through cooperative reformation or direct-to-consumer channels at significant cost
 *   - Urban Logistics Operators: Primary beneficiary (institutional/arbitrage) — capture margin through consolidation; experience constraint as solved coordination problem; arbitrage options enable escape if political pressure mounts
 *   - Regional Government: Secondary actor (institutional/constrained) — bears infrastructure maintenance costs and maintains rural service politically while also benefiting from efficient centralized networks; enforcement required to sustain subsidy mechanisms
 *   - Rural Cooperative System: Institutional actor (institutional/arbitrage) — formerly functional coordination mechanism, now largely theatrical; maintained through member identity attachment rather than logistics utility
 *   - Digital Logistics Coalition: Organized actors (organized/mobile) — startups, platform cooperatives, automation firms with exit pathways and technology-based sunset mechanisms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable geographic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rural_logistics_access, 0.52).
domain_priors:suppression_score(rural_logistics_access, 0.68).
domain_priors:theater_ratio(rural_logistics_access, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rural_logistics_access, extractiveness, 0.52).
narrative_ontology:constraint_metric(rural_logistics_access, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rural_logistics_access, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rural_logistics_access, tangled_rope).
narrative_ontology:human_readable(rural_logistics_access, "Rural Logistics Access Constraint").
narrative_ontology:topic_domain(rural_logistics_access, "economic/infrastructure").

domain_priors:requires_active_enforcement(rural_logistics_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rural_logistics_access, urban_logistics_operators).
narrative_ontology:constraint_beneficiary(rural_logistics_access, consolidated_retailers).
narrative_ontology:constraint_beneficiary(rural_logistics_access, centralized_distributors).
narrative_ontology:constraint_victim(rural_logistics_access, rural_merchants).
narrative_ontology:constraint_victim(rural_logistics_access, remote_communities).
narrative_ontology:constraint_victim(rural_logistics_access, small_farm_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL MERCHANT (SNARE) — Trapped by geographic isolation and lack of alternative suppliers. Cannot negotiate logistics costs; bears full burden of distance penalty. No exit: relocation economically impossible, alternative supply chains nonexistent. Maximum extraction experienced.
constraint_indexing:constraint_classification(rural_logistics_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALL FARM OPERATOR (TANGLED ROPE) — Constrained by transport cost burden and dependence on centralized distributors, but also benefits from coordinated supply chain access and bulk purchasing leverage they could not achieve independently. Extraction is asymmetric but coordination is genuine — the constraint solves a real collective action problem (aggregating dispersed demand) while extracting margins.
constraint_indexing:constraint_classification(rural_logistics_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN LOGISTICS OPERATOR (ROPE) — Primary beneficiary with arbitrage options. Experiences the constraint as coordination: consolidating rural demand and routing through hub-and-spoke networks solves genuine logistics optimization problems. Net beneficiary through margin capture, but the mechanism is coordinative — without the constraint, the logistics problem remains unsolved.
constraint_indexing:constraint_classification(rural_logistics_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL GOVERNMENT (TANGLED ROPE) — Constrained by infrastructure funding scarcity and political demand for rural service maintenance. Also benefits from centralized logistics networks reducing redundancy. Faces extraction (bearing cost of infrastructure maintenance) and coordination incentive (need to maintain supply access) simultaneously. Active enforcement required to maintain subsidy mechanisms.
constraint_indexing:constraint_classification(rural_logistics_access, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RURAL COOPERATIVE SYSTEM (PITON) — Historical coordination mechanism for aggregating rural demand, now largely theatrical. Maintains formal structure and member loyalty but has been functionally displaced by centralized logistics networks. Theater ratio high (0.65+): annual meetings, governance ceremonies, member newsletters persist while actual logistics functions have been outsourced to urban operators. Preserved through institutional inertia and identity attachment rather than functional necessity.
constraint_indexing:constraint_classification(rural_logistics_access, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: DIGITAL LOGISTICS COALITION (SCAFFOLD) — Organized agents (platform cooperatives, last-mile startups, autonomous delivery initiatives) view the constraint as a temporary market failure being solved through technology. Decentralized routing, rural drone delivery, micro-fulfillment centers create alternative pathways with lower transaction costs. High suppression tolerated because coalition sees clear sunset: technology deployment reducing distance penalty over 15-20 year horizon. Has_sunset_clause rationale: as automation and distributed fulfillment mature, the geographic distance penalty becomes a choice variable rather than a structural constraint.
constraint_indexing:constraint_classification(rural_logistics_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, geographic distance creates inherent transportation cost differentials: supplying a remote location will always cost more than supplying urban centers due to physics and spatial geometry. This perspective naturalizes the constraint as an immutable property of logistics. However, the base_properties contradicts the mountain classification: the extractiveness (0.52) and suppression (0.68) values indicate institutional choices, not physical laws. The analytical observer's mountain is a false summit, revealing that naturalizing distance as destiny obscures the contingent institutional arrangements (centralized distribution, margin capture, infrastructure neglect) that create the actual constraint.
constraint_indexing:constraint_classification(rural_logistics_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rural_logistics_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rural_logistics_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rural_logistics_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rural_logistics_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rural_logistics_access, TR),
    TR >= 0.70.

:- end_tests(rural_logistics_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The rural logistics cost premium comprises both genuine geographic cost (fuel, distance, time) and institutional markup (margin capture by consolidated operators, infrastructure underinvestment, regulatory barriers to competition). The 0.52 value reflects that institutional extraction is substantial but not total — some operators do serve rural markets at lower margins, indicating the market failure is not absolute. The value increased from 0.38 to 0.52 over the measurement interval as consolidation deepened and cooperative alternatives atrophied. Suppression (0.68): High. Significant barriers to exit include geographic isolation (cannot relocate suppliers), lack of alternatives (consolidation eliminated competitors), information asymmetry (rural agents unaware of real cost structure), and regulatory barriers (license/permit costs higher in rural contexts). Suppression is structural (external barriers) rather than internalized — rural merchants perceive the extraction clearly but cannot escape it. Theater ratio (0.35): Low. The constraint is functionally genuine: real logistics optimization problems exist, genuine coordination gaps exist, transport costs are real. Theater appears primarily in the rural cooperative system (maintained through ceremony and identity attachment rather than function) and in policy narratives that naturalize distance-based pricing as inevitable rather than contingent. The low theater ratio indicates the constraint's core mechanism is extractive logistics, not performative ritual.
 *
 * PERSPECTIVAL GAP:
 *   Gap between snare (rural merchant, trapped) and rope (urban operator, arbitrage) is maximum: both see the same constraint, but the powerless see extraction, the beneficiary sees coordination. Gap between tangled rope (farm operator, constrained) and rope (urban operator, arbitrage) reflects the exit option differential: constrained agents experience both extraction and coordination benefit; arbitrage agents experience pure coordination benefit. Gap between piton (cooperative system) and snare (merchant) reflects institutional obsolescence: the cooperative was once a rope (genuine coordination) but is now theatrical (abandoned function, maintained identity). Gap between scaffold (digital coalition, organized/mobile) and snare (merchant, powerless/trapped) reflects asymmetric awareness of exit pathways: organized agents see technology-based sunset routes; powerless agents see no exit. Gap between mountain (analytical, civilizational) and snare (merchant, biographical) reflects the false naturalization: the analytical observer risks treating contingent institutional arrangements (centralization, margin extraction, infrastructure neglect) as immutable geographic laws. This gap is the diagnostic signal for false summits — when the analytical observer's classification (mountain) contradicts the primary victim's (snare) and beneficiary's (rope) lived experience, the analytical frame has naturalized institutional choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from agent power level, exit options, and beneficiary/victim status. Urban logistics operators have institutional power, arbitrage exit options, and beneficiary status — derived d ≈ 0.08 (full beneficiary with escape routes), producing negative effective extraction (they benefit from the constraint). Rural merchants have powerless status, trapped exit options, and victim status — derived d ≈ 0.95 (full target with no escape), producing maximum experienced extraction. Small farm operators have moderate power, constrained exit (can relocate to urban supply chains at significant cost, can attempt cooperative reformation with high coordination barriers), and victim-beneficiary duality (benefit from supply aggregation, pay extraction premium) — derived d ≈ 0.55, producing moderate experienced extraction. Regional government has institutional power, constrained exit (cannot ignore rural supply policy politically but can offload costs to consolidation), and mixed victim-beneficiary status (benefits from efficient networks, bears subsidy costs) — derived d ≈ 0.50. The digital coalition has organized power, mobile exit options (can scale technology or exit market), and beneficiary status (capturing value from logistics efficiency gains) — derived d ≈ 0.35. These differentials are captured through the beneficiary/victim declarations and exit_options vectors; the directionality overrides are not needed (derivation chain produces accurate d values from structural data).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival decomposition. The constraint is genuinely tangled rope at the core (beneficiaries exist who experience coordination; victims exist who bear asymmetric extraction; active enforcement maintains the regime). The snare classification at the powerless perspective reflects accurate lived experience: trapped agents with no exit do experience the constraint as pure extraction, and for them the classification is correct. The rope classification at the institutional beneficiary perspective also reflects accurate lived experience: beneficiaries with arbitrage options do experience coordination benefit. The piton and scaffold perspectives reveal institutional evolution: the cooperatives have atrophied to theater (piton), and new technology-based alternatives are emerging (scaffold). The mountain perspective is a false summit: the analytical observer risks naturalizing the constraint as immutable distance-based cost when structural data (the tangled rope core with enforced asymmetry) reveals it as contingent institutional arrangement. The resolution prevents the mandatrophy trap where 'which is the true type?' paralysis occurs: all six types are legitimate readings of the same structure from different perspectives. The tangled rope is the analytical consensus (benefits and costs are both real, coordination and extraction are both present, enforcement is active). The snare and rope are the perspectival extremes (what the most exploited and most benefited agents actually experience). The piton and scaffold are historical/technological trajectories. The mountain is a cautionary example of how false naturalization obscures institutional contingency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distance_penalty_vs_institutional_markup,
    'What portion of rural logistics cost premium is inherent to distance (physical/fuel costs) versus institutional markup (margin extraction, neglected competition)?',
    'Cost decomposition analysis: fuel/time cost modeling vs actual retail price differential. Comparison to rural logistics costs in high-competition markets (e.g., Nordic countries with cooperative infrastructure) vs consolidated markets.',
    'If institutional markup is >40%: constraint is primarily extractive (snare/tangled_rope confirmed, mountain false). If markup is <20%: constraint has larger natural law component (mountain plausible). Threshold determines whether rural subsidy policy addresses market failure or institutional extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distance_penalty_vs_institutional_markup, empirical, 'Proportion of cost premium from distance vs institutional extraction').

omega_variable(
    cooperative_viability_threshold,
    'What minimum rural population density sustains independent logistics cooperatives without external subsidy?',
    'Comparative analysis of cooperative viability across density gradients in multiple regions. Identification of break-even density thresholds where cooperative overhead becomes unsustainable.',
    'If threshold > existing density: piton classification confirmed (cooperatives are inertial, not functional). If threshold < existing density: cooperatives are viable but captured (tangled_rope for regional government). Determines whether sunset requires technology or policy intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cooperative_viability_threshold, empirical, 'Minimum population density for viable independent cooperatives').

omega_variable(
    last_mile_automation_feasibility,
    'Can autonomous/drone-based last-mile logistics achieve cost parity with centralized hub-and-spoke networks in rural contexts within the proposed 15-20 year horizon?',
    'Comparative cost modeling: autonomous delivery infrastructure deployment (drones, micro-fulfillment) vs centralized logistics over technology maturation timeline. Regulatory and terrain feasibility analysis.',
    'If feasible: scaffold perspective confirmed (sunset is structural, not aspirational). If infeasible: scaffold is theatrical aspiration; constraint persists beyond sunset horizon. Affects confidence in digital coalition''s exit pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(last_mile_automation_feasibility, empirical, 'Whether autonomous last-mile logistics can achieve cost parity').

omega_variable(
    rural_agent_coalition_emergence,
    'Under what conditions do powerless rural merchants organize into coalitions capable of renegotiating logistics terms?',
    'Historical analysis of successful rural collective action (successful cooperative models, producer unions, platform cooperatives). Identification of prerequisites: communication infrastructure, charismatic leadership, external support, collective identity.',
    'If conditions rare/absent: snare classification sustained (powerless remain trapped). If conditions present/emerging: potential for classification shift from snare to organized tangled_rope. Affects mandatrophy resolution pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rural_agent_coalition_emergence, conceptual, 'Conditions for powerless agents to form renegotiating coalitions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rural_logistics_access, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rla_tr_t0, rural_logistics_access, theater_ratio, 0, 0.28).
narrative_ontology:measurement(rla_tr_t10, rural_logistics_access, theater_ratio, 10, 0.33).
narrative_ontology:measurement(rla_tr_t20, rural_logistics_access, theater_ratio, 20, 0.35).
narrative_ontology:measurement(rla_tr_t5, rural_logistics_access, theater_ratio, 5, 0.3).

% Extraction over time
narrative_ontology:measurement(rla_be_t0, rural_logistics_access, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(rla_be_t10, rural_logistics_access, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rla_be_t20, rural_logistics_access, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(rla_be_t5, rural_logistics_access, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rural_logistics_access, resource_allocation).
narrative_ontology:boltzmann_floor_override(rural_logistics_access, 0.12).
narrative_ontology:affects_constraint(rural_logistics_access, agricultural_supply_chain_consolidation).
narrative_ontology:affects_constraint(rural_logistics_access, rural_infrastructure_underinvestment).
narrative_ontology:affects_constraint(rural_logistics_access, small_business_margin_squeeze).

% DUAL FORMULATION NOTE:
% Rural logistics access decomposes into multiple structurally distinct constraints: (1) supply_chain_consolidation (ε≈0.45, tangled_rope) — institutional extraction through margin capture; (2) transport_cost_asymmetry (ε≈0.35, rope) — genuine geometric/physical coordination problem; (3) infrastructure_maintenance_burden (ε≈0.40, tangled_rope) — regional government bears subsidy cost while central operators capture efficiency gains. This story treats the unified constraint; decomposition into substrate constraints is available for higher granularity analysis. Links show how rural logistics access cascades into agricultural consolidation and small business margin compression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
