% ============================================================================
% CONSTRAINT STORY: canal_panama_influence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_canal_panama_influence, []).

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
 *   constraint_id: canal_panama_influence
 *   human_readable: Geopolitical Influence over Panama Canal
 *   domain: political/geopolitical
 *
 * SUMMARY:
 *   The Panama Canal represents a critical global chokepoint where
 *   geopolitical competition between the United States and China intersects
 *   with Panamanian sovereignty, Swiss commercial interests, and the shipping
 *   requirements of the global economy. The constraint exhibits tangled-rope
 *   structure: a genuine coordination function (efficient canal operations,
 *   global commerce enablement) exists alongside asymmetric extraction (US
 *   maintains strategic dominance, China seeks leverage, Panama bears
 *   sovereignty costs, developing economies face tariff exposure). Swiss port
 *   operators provide a crucial institutional buffer through operational
 *   neutrality and technical expertise, though their true allegiances remain
 *   contested. The constraint's extractiveness has increased from 0.35 to
 *   0.58 over the past 25 years as US-China competition has intensified,
 *   while theater ratio remains moderate (0.48), indicating that operational
 *   legitimacy persists even as geopolitical leverage accumulates. Panama
 *   remains trapped by geographic inevitability and economic dependency,
 *   unable to credibly threaten closure or renegotiation. The emergence of
 *   alternative maritime routes (Arctic shipping, expanded Suez capacity,
 *   overland corridors) represents a potential scaffold sunset mechanism that
 *   could reduce leverage concentration within 20-50 years.
 *
 * KEY AGENTS:
 *   - Panama Government: Primary victim (powerless/trapped) — geographic dependency, economic reliance on canal revenues (6-8% of GDP), constrained renegotiation capacity
 *   - United States Strategic Interest: Primary beneficiary/constrained extractor (organized/constrained) — maintains geopolitical dominance despite treaty constraints; must operate indirectly through military presence and diplomatic pressure
 *   - Chinese Economic Expansion: Secondary extractor (powerful/constrained) — pursues leverage through adjacent port investment and Belt and Road financing; faces US opposition and constrained access
 *   - Swiss Port Operators: Institutional beneficiary (institutional/arbitrage) — experiences canal as coordination problem; maintains neutrality to preserve operational access; high exit options (can relocate investments)
 *   - Panama Canal Authority: Institutional actor (institutional/arbitrage) — nominal Panamanian control with limited actual autonomy; theater-heavy role maintaining fiction of sovereignty
 *   - Developing Economies & Neutral Shipping Nations: Secondary victims (moderate/constrained) — benefit from efficient transit but face tariff exposure and vulnerability to geopolitical escalation
 *   - Alternative Routes Coalition: Organized agents (organized/mobile) — emerging actors exploring Arctic, Suez, and overland options; represent sunset mechanism for canal dependency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(canal_panama_influence, 0.58).
domain_priors:suppression_score(canal_panama_influence, 0.68).
domain_priors:theater_ratio(canal_panama_influence, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(canal_panama_influence, extractiveness, 0.58).
narrative_ontology:constraint_metric(canal_panama_influence, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(canal_panama_influence, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(canal_panama_influence, tangled_rope).
narrative_ontology:human_readable(canal_panama_influence, "Geopolitical Influence over Panama Canal").
narrative_ontology:topic_domain(canal_panama_influence, "political/geopolitical").

domain_priors:requires_active_enforcement(canal_panama_influence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(canal_panama_influence, united_states_strategic_interests).
narrative_ontology:constraint_beneficiary(canal_panama_influence, swiss_port_operators).
narrative_ontology:constraint_beneficiary(canal_panama_influence, global_shipping_efficiency).
narrative_ontology:constraint_victim(canal_panama_influence, panama_sovereignty).
narrative_ontology:constraint_victim(canal_panama_influence, developing_economies_tariff_exposure).
narrative_ontology:constraint_victim(canal_panama_influence, neutral_shipping_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PANAMA SOVEREIGNTY (SNARE) — Panama cannot exit its geographic position or renegotiate canal control meaningfully without economic catastrophe. The nation is trapped by dependency on canal revenues (6-8% of GDP) and cannot credibly threaten closure or renegotiation. Bears full extraction cost through restricted autonomy in canal governance and vulnerability to external pressure from US-China competition.
constraint_indexing:constraint_classification(canal_panama_influence, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING ECONOMIES TARIFF EXPOSURE (TANGLED ROPE) — Latin American and African nations benefit from low transit costs and efficient routing but are constrained by inability to negotiate alternative routes or reduce dependency. Experiences mixed extraction: canal efficiency benefits them, but geopolitical rent-seeking and leverage over shipping rates imposes asymmetric costs. Exit options are constrained (cannot easily shift to other routes); power is moderate (can coordinate but not credibly threaten closure).
constraint_indexing:constraint_classification(canal_panama_influence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SWISS PORT OPERATORS & GLOBAL EFFICIENCY (ROPE) — Swiss-owned concessionaires benefit from operational expertise, cost-efficiency improvements, and neutral positioning. Experience the constraint as pure coordination: managing canal flow, investing in modernization, and maintaining throughput benefits them directly. High exit options (can shift investments to other ports globally) and institutional power mean they experience low or negative extraction — the constraint solves their operational coordination problem.
constraint_indexing:constraint_classification(canal_panama_influence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UNITED STATES STRATEGIC INTEREST (SNARE) — Despite institutional power and historic dominance, US faces organized constraints from Chinese economic competition and Panamanian nationalism. Experiences high suppression: cannot explicitly control canal anymore (1977 treaty transfer), cannot prevent Chinese investment (Swiss intermediaries provide plausible deniability), cannot unilaterally raise tolls without triggering renegotiation. The extraction mechanism is maintenance of geopolitical dominance despite formal treaty constraints — high suppression required to sustain the influence.
constraint_indexing:constraint_classification(canal_panama_influence, snare,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CHINESE ECONOMIC EXPANSION (SNARE) — China pursues leverage through investment in adjacent port infrastructure (Colón, Balboa) and Belt and Road Initiative financing. Experiences extraction through US strategic opposition and cannot freely expand presence (constrained by US pressure on Panama). High suppression: must operate through intermediaries, Chinese firms face visa restrictions, cannot directly control canal operations despite financial investment. Extraction mechanism is maintenance of restricted access despite capital deployment.
constraint_indexing:constraint_classification(canal_panama_influence, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NEUTRAL SHIPPING NATIONS (TANGLED ROPE) — Nations with significant merchant fleets (Singapore, Norway, Greece) benefit from efficient transit but are constrained by geopolitical leverage. Coordination function: canal enables global commerce. Extraction function: vulnerability to toll increases, potential closure if geopolitical conflict escalates, inability to influence canal governance despite heavy usage. Suppression via US military presence in region and implicit threat of escalation.
constraint_indexing:constraint_classification(canal_panama_influence, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: PANAMA CANAL AUTHORITY (PITON) — The PCA maintains the fiction of Panamanian control while operating under significant external constraints (US security interests, international commerce law, geopolitical pressure). Theater ratio high: formal sovereignty over an asset that cannot be meaningfully modified. Decision-making power is constrained by treaty and geopolitical reality despite bureaucratic autonomy. The institution persists through inertia — Panamanian control is nominal, but no alternative governance structure has emerged to replace it.
constraint_indexing:constraint_classification(canal_panama_influence, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ALTERNATIVE ROUTES COALITION (SCAFFOLD) — Arctic shipping, expanded Suez capacity, and overland corridors (Nicaragua, other routes) represent emerging alternatives. These constraints are temporary: as climate change opens Arctic routes and alternative infrastructure develops, canal dependency decreases. Sunset mechanism: decentralization of global shipping routes reduces leverage over any single chokepoint. Coalition includes climate researchers, infrastructure planners, and logistics companies exploring path dependence reduction.
constraint_indexing:constraint_classification(canal_panama_influence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / GEOGRAPHY (MOUNTAIN) — From a civilizational perspective, the canal's geographic position as the shortest water route between Atlantic and Pacific is a natural constraint. No agent can change that fact. Any power can only extract from this fixed bottleneck position — the geography itself is the unmovable constraint. However, the structural data (high suppression, active enforcement, geopolitical competition) reveals this as a false summit: the extractive mechanism is geopolitical, not geographic.
constraint_indexing:constraint_classification(canal_panama_influence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canal_panama_influence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(canal_panama_influence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(canal_panama_influence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(canal_panama_influence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(canal_panama_influence, TR),
    TR >= 0.70.

:- end_tests(canal_panama_influence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant geopolitical leverage for the US and access denial for China, while Panama bears sovereignty costs and developing economies face tariff vulnerability. The value reflects that genuine coordination function (canal operation) exists but is increasingly subordinated to geopolitical competition. Suppression (0.68): High. Panama cannot credibly exit or renegotiate; both US and China must operate indirectly (military presence for US, intermediary investment for China); neutral shipping nations cannot effectively reduce dependency; Swiss operators maintain neutrality under pressure. Active enforcement required to maintain the constraint against countervailing interests. Theater ratio (0.48): Moderate. Canal operations maintain high legitimacy and technical professionalism (Swiss management), but geopolitical competition increasingly drives strategic decisions. The constraint has not degraded into pure performative theater (as in a Piton) because actual shipping does occur and efficiency is genuinely at stake. The theater reflects the gap between formal Panamanian sovereignty and actual geopolitical constraints.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Panama sees a Snare — trapped by geography and economic dependency, unable to modify its structural position. The US sees constrained extraction through diplomatic and military means (Snare from their position) despite formal treaty limitations. China sees blocked expansion (Snare) requiring indirect investment strategies. Swiss operators see pure coordination (Rope) — their exit options and institutional power mean they experience low effective extraction. Developing economies see mixed benefits and costs (Tangled Rope) — efficiency gain but tariff vulnerability. The alternative routes coalition sees a temporary problem with a sunset (Scaffold) — decentralized shipping routes emerging as viable alternatives. The analytical observer risks naturalizing geographic inevitability as immutable law (Mountain), but the structural data reveals geopolitical suppression as the critical extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Panama's directionality: powerless/trapped status + victim classification → high d (0.85-0.95) → high f(d) → high experienced χ. Panama cannot arbitrage or exit; bears full extraction cost from geopolitical competition. US directionality: institutional/constrained status + beneficiary classification → moderate d (0.40-0.50) → moderate f(d). US benefits from strategic dominance but is constrained by treaty and must use indirect enforcement mechanisms. China directionality: powerful/constrained status → high d (0.55-0.65) → high f(d). China seeks benefits but faces suppression and access denial. Swiss operators directionality: institutional/arbitrage status + beneficiary classification → low d (0.10-0.20) → negative f(d). Arbitrage options and technical expertise mean they experience minimal extraction cost; the constraint solves their operational coordination problem. Developing economies directionality: moderate/constrained status + victim classification → high d (0.65-0.75) → high f(d). Constrained exit and tariff exposure create moderate-to-high extraction experience.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that both genuine coordination and asymmetric extraction coexist structurally. The canal IS a coordination mechanism — efficient shipping, legitimate operations, technical professionalism. The canal IS an extraction mechanism — geopolitical leverage concentration, sovereignty constraints, tariff vulnerability, blocked access. The constraint is not misclassified as pure extraction; it genuinely contains both functions. The tangled-rope classification is correct because (1) beneficiaries exist and benefit from coordination (Swiss, global shipping efficiency), (2) victims exist and bear extraction costs (Panama, developing economies), and (3) active enforcement (diplomatic pressure, military presence, indirect investment controls) is required to maintain the asymmetry. The perspectival gaps (Snare from Panama, Rope from Swiss operators, Scaffold from alternative routes coalition) are legitimate — different agents experience different structural realities based on their exit options and power positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    swiss_intermediary_allegiance,
    'Do Swiss operators maintain genuine neutrality or tacitly favor US or Chinese interests through operational decisions?',
    'Detailed analysis of toll structures, priority allocation, infrastructure investment patterns, and decision-maker funding sources; tracking of operational decisions during US-China diplomatic incidents',
    'If genuinely neutral: Swiss intermediaries function as constraint-reducing agents. If captured by either power: constraint becomes asymmetric snare with hidden beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(swiss_intermediary_allegiance, empirical, 'Whether Swiss operators maintain neutrality or are captured by geopolitical interests').

omega_variable(
    panama_exit_credibility,
    'Could Panama credibly threaten canal closure or renegotiation to improve its bargaining position?',
    'Analysis of alternative revenue sources, economic resilience to closure, military capacity, and international support; simulation of renegotiation scenarios',
    'If exit credible: Snare classification degrades to Tangled Rope (Panama gains negotiating power). If exit not credible: Snare classification confirmed, Panama trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(panama_exit_credibility, empirical, 'Whether Panama has credible threat capacity for renegotiation').

omega_variable(
    geopolitical_competition_intensity,
    'Is US-China competition the primary driver of canal influence suppression, or are other factors (local corruption, maritime law, environmental constraints) equally important?',
    'Decomposition of suppression sources via governance analysis, stakeholder interviews, and counterfactual scenarios (US-China détente, unilateral US withdrawal)',
    'If geopolitical competition primary: current tangled_rope classification correct. If other factors dominant: constraint may be Scaffold (solvable) or Rope (coordination-only).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_competition_intensity, conceptual, 'Primary drivers of suppression and geopolitical influence').

omega_variable(
    alternative_routes_timeline,
    'At what efficiency threshold do alternative routes (Arctic, expanded Suez, overland) eliminate canal dependency and reduce geopolitical leverage?',
    'Infrastructure capacity analysis, cost modeling for Arctic vs Panama vs alternatives, climate projection integration, and logistics network optimization',
    'If threshold reachable in 20 years: Scaffold sunset is real, constraint will degrade to Rope. If threshold unreachable: constraint persists as Snare indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_routes_timeline, empirical, 'Feasibility and timeline for alternative route dominance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(canal_panama_influence, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(canal_tr_t0, canal_panama_influence, theater_ratio, 0, 0.32).
narrative_ontology:measurement(canal_tr_t25, canal_panama_influence, theater_ratio, 25, 0.48).
narrative_ontology:measurement(canal_tr_t50, canal_panama_influence, theater_ratio, 50, 0.51).

% Extraction over time
narrative_ontology:measurement(canal_be_t0, canal_panama_influence, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(canal_be_t25, canal_panama_influence, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(canal_be_t50, canal_panama_influence, base_extractiveness, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(canal_panama_influence, global_infrastructure).
narrative_ontology:affects_constraint(canal_panama_influence, suez_canal_chokepoint).
narrative_ontology:affects_constraint(canal_panama_influence, arctic_shipping_dependency).
narrative_ontology:affects_constraint(canal_panama_influence, us_china_technology_competition).
narrative_ontology:affects_constraint(canal_panama_influence, developing_economy_trade_asymmetry).

% DUAL FORMULATION NOTE:
% The canal influence constraint can be decomposed into three related claims: (1) Geographic bottleneck (ε ≈ 0.10, Mountain) — purely physical constraint; (2) Operational efficiency (ε ≈ 0.25, Rope) — neutral coordination mechanism; (3) Geopolitical leverage (ε ≈ 0.58, Tangled Rope) — the actual constraint family. The present story focuses on the geopolitical layer. Upstream constraints (US-China competition, Panamanian sovereignty) feed this story; downstream constraints (alternative routes coalition) represent sunset mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(canal_panama_influence, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
