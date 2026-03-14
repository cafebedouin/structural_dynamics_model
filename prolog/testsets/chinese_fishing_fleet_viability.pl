% ============================================================================
% CONSTRAINT STORY: chinese_fishing_fleet_viability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chinese_fishing_fleet_viability, []).

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
 *   constraint_id: chinese_fishing_fleet_viability
 *   human_readable: Chinese Fishing Fleet Economic Viability and Ocean Governance
 *   domain: economic_policy/maritime_governance/environmental_sustainability
 *
 * SUMMARY:
 *   The Chinese distant-water fishing fleet represents a structural
 *   constraint operating at the intersection of economic development, food
 *   security, maritime governance, and ecological carrying capacity.
 *   Approximately 3,500 Chinese fishing vessels operate across global waters,
 *   concentrated in Southeast Asian EEZs, the Atlantic, and Pacific regions.
 *   The constraint generates six distinct classifications depending on
 *   observer position: small-scale coastal fishers see a snare (trapped by
 *   depletion), marine ecosystems experience tangled coordination-extraction,
 *   the Chinese state perceives a rope (coordination tool for employment and
 *   food security), organized international bodies encounter a tangled rope
 *   (coordinating conflicting interests under weak enforcement), the legal
 *   framework appears as a piton (maintained through institutional inertia),
 *   and the civilizational analytical perspective approaches a mountain
 *   (ecological carrying capacity is a hard limit). The constraint's
 *   extractiveness (0.58) reflects that the fleet's continued viability
 *   depends substantially on exceeding sustainable catch levels — the
 *   contradiction between stated sustainability norms and actual practices.
 *   Theater ratio (0.48) reflects that enforcement mechanisms (port state
 *   control, RFMOs, bilateral agreements) produce visible compliance
 *   structures while actual overfishing persists.
 *
 * KEY AGENTS:
 *   - Chinese Fishing Industry: Primary beneficiary (institutional/arbitrage) — captures export revenue, employment, and state subsidies. Has agency to adjust fleet composition and fishing grounds.
 *   - Small-Scale Coastal Fishers (Southeast Asia): Primary victim (powerless/trapped) — lose access to traditional fishing grounds, cannot compete with industrial fleets, have no exit options without abandoning livelihoods and social identity.
 *   - Global Fish Stocks and Marine Ecosystems: Secondary victim (moderate/constrained) — bear suppression through overfishing and habitat destruction; cannot exit or organize; subject to biological limits on recovery.
 *   - Chinese State Apparatus: Beneficiary (institutional/arbitrage) — extracts revenue and maintains employment in politically important coastal communities; has policy alternatives but faces domestic pressure.
 *   - Regional Fishing States (Vietnam, Indonesia, Philippines): Organized actors (organized/constrained) — experience coordination burden of managing overlapping claims and enforcement gaps; constrained by geopolitical power asymmetries.
 *   - International Regulatory Bodies (UNCLOS, ASEAN, FAO, RFMOs): Institutional actors (institutional/constrained) — tasked with coordination but lack enforcement capacity; experience constraint as enforced cooperation with limited real power.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees approaching ecological boundary and risks naturalizing governance failure as immutable ecological limit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chinese_fishing_fleet_viability, 0.58).
domain_priors:suppression_score(chinese_fishing_fleet_viability, 0.65).
domain_priors:theater_ratio(chinese_fishing_fleet_viability, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chinese_fishing_fleet_viability, extractiveness, 0.58).
narrative_ontology:constraint_metric(chinese_fishing_fleet_viability, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(chinese_fishing_fleet_viability, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chinese_fishing_fleet_viability, tangled_rope).
narrative_ontology:human_readable(chinese_fishing_fleet_viability, "Chinese Fishing Fleet Economic Viability and Ocean Governance").
narrative_ontology:topic_domain(chinese_fishing_fleet_viability, "economic_policy/maritime_governance/environmental_sustainability").

domain_priors:requires_active_enforcement(chinese_fishing_fleet_viability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chinese_fishing_fleet_viability, chinese_fishing_industry).
narrative_ontology:constraint_beneficiary(chinese_fishing_fleet_viability, coastal_communities).
narrative_ontology:constraint_beneficiary(chinese_fishing_fleet_viability, state_revenue_systems).
narrative_ontology:constraint_victim(chinese_fishing_fleet_viability, global_fish_stocks).
narrative_ontology:constraint_victim(chinese_fishing_fleet_viability, small_scale_fishers).
narrative_ontology:constraint_victim(chinese_fishing_fleet_viability, coastal_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL-SCALE COASTAL FISHERS (SNARE) — Trapped by territorial depletion and inability to compete with industrial fleets. No exit options: migration to other regions means losing traditional fishing grounds and social identity; switching livelihoods requires capital and skills they lack. Bear full cost of the constraint through resource scarcity while having no voice in governance. Maximum experienced extraction.
constraint_indexing:constraint_classification(chinese_fishing_fleet_viability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GLOBAL MARINE ECOSYSTEM (TANGLED ROPE) — Experiences both coordination and extraction. The constraint coordinates resource extraction across multiple stakeholders and timeframes, but at asymmetric cost. Marine ecology cannot exit; it bears suppression through overfishing and habitat destruction. Genuine coordination function (sustainable yield targets, seasonal closures) exists alongside severe extraction (actual practices far exceed sustainable levels). Active enforcement required to maintain the facade of coordination.
constraint_indexing:constraint_classification(chinese_fishing_fleet_viability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CHINESE STATE REVENUE AND EMPLOYMENT (ROPE) — Primary beneficiary. Extracts value through subsidies, export revenue, and employment maintenance. Experiences the constraint as coordination: balancing domestic food security, coastal employment, and international relations. Has substantial arbitrage options (can redirect subsidies, adjust fleet size, invest in alternative sectors). Net beneficiary with agency.
constraint_indexing:constraint_classification(chinese_fishing_fleet_viability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIONAL FISHING STATES AND INTERNATIONAL BODIES (TANGLED ROPE) — Organized actors (UNCLOS framework, ASEAN, FAO, RFMOs) experience coordinating across conflicting interests while being constrained by enforcement capacity. Genuine coordination function (negotiated fishing rights, marine protected areas) exists alongside extraction through regulatory capture and weak enforcement. Can exit disputes through bilateral agreements or withdrawal, but constrained by geopolitical consequences.
constraint_indexing:constraint_classification(chinese_fishing_fleet_viability, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-UNCLOS LAW OF THE SEA THEATER (PITON) — The legal and regulatory framework (UNCLOS, EEZ enforcement, fishing licenses) persists largely through institutional inertia. Theater ratio is moderate (0.48) because some enforcement occurs, but compliance is theatrical: Chinese distant-water fleets operate across multiple jurisdictions with varying enforcement rigor, often flagging vessels in convenient registries to avoid accountability. The framework is maintained because alternatives haven't replaced it, not because it functions effectively.
constraint_indexing:constraint_classification(chinese_fishing_fleet_viability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ECOLOGICAL LIMIT VIEW (MOUNTAIN) — From a civilizational/universal perspective, the constraint approaches an irreducible ecological boundary: the biological carrying capacity of exploited fish stocks has hard limits independent of governance preferences. As fishing pressure exceeds recruitment rates, stock collapse becomes inevitable — a structural limit, not a policy choice. However, the structural data reveals this as a prospective mountain (future reality), not a present one. The current classification is Tangled Rope because enforcement of sustainable limits remains incomplete. The mountain emerges only if depletion continues unchecked.
constraint_indexing:constraint_classification(chinese_fishing_fleet_viability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chinese_fishing_fleet_viability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(chinese_fishing_fleet_viability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chinese_fishing_fleet_viability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(chinese_fishing_fleet_viability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(chinese_fishing_fleet_viability, TR),
    TR >= 0.70.

:- end_tests(chinese_fishing_fleet_viability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The fleet's profitability and viability depend on accessing fishing grounds beyond sustainable yield levels. Historical analysis shows catch per unit effort declining while fleet size and technology intensity increase — indicators that expansion is driven by subsidy and debt accumulation rather than genuine productivity. Extractiveness has risen from 0.35 to 0.58 over the interval (0-10 years) as stocks have declined and vessels have had to venture further. The rate of increase reflects accelerating depletion. Suppression (0.65): High. Multiple layers: small-scale fishers have limited geographic mobility, regulatory barriers to alternative sectors are high, social identity is fused with fishing occupations, and geopolitical consequences constrain state policy flexibility. China's involvement in territorial disputes (South China Sea) makes fishing fleet presence a sovereignty claim, reducing state flexibility to accept fishing restrictions. Theater ratio (0.48): Moderate. Enforcement mechanisms exist (port state control, RFMOs, bilateral agreements, AIS monitoring) and produce visible compliance structures. However, compliance is often achieved through flag state shopping, underreporting, and jurisdictional gaps. The theater is neither minimal (there is real enforcement infrastructure) nor maximal (it does affect behavior, albeit incompletely). The rising theater ratio over the interval reflects increasing sophistication in evasion tactics.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The Chinese state genuinely experiences the fleet as a coordination mechanism solving employment and food security — from their position, other constraints (coastal unemployment, geopolitical competition) create pressure to maintain fleet expansion. Small-scale fishers see pure extraction with no coordination benefit — they bear costs while receiving nothing. The global marine ecosystem bears extraction with embedded coordination theater: catch limits, seasonal closures, and protected areas exist as coordination structures, but actual enforcement is weak enough that extraction far exceeds coordination. Regional states experience a governance tragedy: they coordinate enforcement among themselves but cannot compel Chinese compliance without geopolitical costs. The journal/international body (piton perspective) sees a ritual framework — the Post-UNCLOS law of the sea persists through institutional maintenance and theater, but actual compliance patterns follow power asymmetries rather than norms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. The Chinese state (beneficiary + arbitrage) has low d (~0.15) — the constraint extracts value toward them. Small-scale coastal fishers (victim + trapped) have high d (~0.95) — the constraint extracts value away from them. Regional fishing states (organized actors + constrained exit) have moderate d (~0.50) — they experience mixed costs and benefits. Marine ecosystems (victim + no exit) have high d (~0.90). These d values feed the sigmoid f(d) to produce experienced extractiveness chi, which varies by perspective. The organized institutional perspective (Chinese state) sees low chi because their d is low (they benefit). The powerless perspective (coastal fishers) sees high chi because their d is high (they bear costs). The analytical perspective derives d from its position as an observer of the system (~0.72, slightly toward target because the system imposes costs on the analyst's presumed values of sustainability).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint avoids the mandatrophy trap by acknowledging that the 'viability' framing itself contains the extraction mechanism. 'Fleet viability' is defined as the state's capacity to maintain employment and geopolitical presence through fishing, which is only 'viable' by subsidizing beyond ecological carrying capacity. The coordinate system that makes the constraint visible is the one where costs are externalized to small-scale fishers and marine ecosystems while benefits concentrate in the Chinese state and industrial fleet operators. From the state's perspective, the constraint is a rope solving coordination problems (employment, food security, geopolitical leverage). From the ecosystem's perspective, it is a snare with embedding coordination theater. The mandatrophy dissolves when we recognize that 'viability of the fleet' and 'viability of coastal fishing communities' are not the same constraint — they have opposite extraction directions. The corpus should treat them as separate stories linked by network causality: fleet expansion undermines small-fisher viability, creating a zero-sum relationship. The current story models the fleet viability constraint (beneficiary: Chinese state; victim: small-scale fishers and marine ecosystems). A companion story should model small-scale fisher viability constraint (beneficiary: regional communities; victim: subsidy-dependent industrial fleet development pressure). The two stories together clarify the true structure: a state-enforced transfer mechanism from small-scale fishers and ecosystems to industrial fleet operators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_dependency_collapse,
    'What proportion of Chinese fishing fleet viability depends on direct government subsidies vs. market-based economics?',
    'Comparative analysis of fleet profitability with and without fuel subsidies, tax incentives, and vessel construction support. Historical data from subsidy reduction periods (if any) or cross-country comparison with less-subsidized fleets.',
    'If subsidy-dependent > 70%: the constraint is primarily a transfer mechanism maintained by state policy (Rope with beneficiary-side extraction), vulnerable to subsidy removal. If subsidy-dependent < 30%: genuine market viability exists independently (constraint is ecological scarcity, approaching Mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidy_dependency_collapse, empirical, 'Proportion of fleet viability dependent on government subsidies').

omega_variable(
    stock_recovery_feasibility,
    'Are currently depleted fish stocks capable of recovery under enforced catch reductions, or have they crossed irreversible tipping points?',
    'Stock modeling with recruitment dynamics, comparison to historical baseline populations, identification of ecosystem-level regime shifts. Specific focus on species under greatest pressure (Atlantic bluefin tuna, South China Sea demersal species).',
    'If recovery feasible: constraint is a temporal tragedy of the commons solvable by enforcement (Tangled Rope). If tipping points crossed: constraint is approaching Mountain (ecological limit has become immutable). Classification changes from Snare/Tangled Rope to Mountain depending on timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stock_recovery_feasibility, empirical, 'Whether depleted stocks can recover under enforced conservation').

omega_variable(
    enforcement_capacity_vs_fleet_scale,
    'What is the technical/political feasibility of enforcement mechanisms matching the scale of distant-water fishing fleets?',
    'Analysis of satellite monitoring (AIS, VMS), port state control capacity, flag state compliance rates. Comparison to enforcement costs relative to extraction value.',
    'If feasible at reasonable cost: enforcement gaps are political choices, not technical limits (supports Tangled Rope with extractive enforcement asymmetry). If infeasible: the constraint includes an unenforceable element (raises theater_ratio, supports Piton classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_fleet_scale, empirical, 'Feasibility of enforcement mechanisms at fleet scale').

omega_variable(
    alternative_livelihoods_transition_cost,
    'What is the economic and social cost of transitioning coastal communities away from fishing into alternative sectors?',
    'Cost-benefit analysis of retraining programs, capital requirements for alternative sectors, social cohesion impacts, regional economic dependency data.',
    'If transition cost < subsidies: constraint removal (enforcement) is economically preferable to subsidy continuation. If transition cost > subsidies: constraint perpetuation becomes economically rational (supports interpretation of Rope as coordination function for employment stability). Shifts victim analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_livelihoods_transition_cost, empirical, 'Cost of transitioning coastal communities to alternative livelihoods').

omega_variable(
    state_capacity_for_alternative_nutrition,
    'Can China secure equivalent nutrition/protein for its population through aquaculture, plant-based substitutes, or imports if wild-catch fisheries are constrained?',
    'Nutritional modeling, aquaculture capacity expansion data, trade dependency analysis for protein sources.',
    'If alternative sources are available: state rationale for fleet expansion becomes geopolitical/extraction-based rather than necessity-based (deepens Tangled Rope analysis). If alternatives are limited: constraint includes genuine coordination function around food security (shifts beneficiary_justification toward legitimate coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_capacity_for_alternative_nutrition, empirical, 'Alternative sources for protein/nutrition if wild fisheries constrained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chinese_fishing_fleet_viability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cff_tr_t0, chinese_fishing_fleet_viability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cff_tr_t5, chinese_fishing_fleet_viability, theater_ratio, 5, 0.42).
narrative_ontology:measurement(cff_tr_t10, chinese_fishing_fleet_viability, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(cff_be_t0, chinese_fishing_fleet_viability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cff_be_t5, chinese_fishing_fleet_viability, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cff_be_t10, chinese_fishing_fleet_viability, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chinese_fishing_fleet_viability, resource_allocation).
narrative_ontology:affects_constraint(chinese_fishing_fleet_viability, southeast_asia_maritime_sovereignty).
narrative_ontology:affects_constraint(chinese_fishing_fleet_viability, south_china_sea_territorial_disputes).
narrative_ontology:affects_constraint(chinese_fishing_fleet_viability, marine_biodiversity_loss).
narrative_ontology:affects_constraint(chinese_fishing_fleet_viability, small_scale_fisher_livelihoods).

% DUAL FORMULATION NOTE:
% This story models the Chinese fishing fleet viability constraint from the perspective of industrial fleet maintenance and state revenue extraction. A companion constraint story (small_scale_fisher_viability) should model the symmetric problem from coastal community perspectives. The two constraints are causally linked: fleet expansion directly undermines small-scale fisher viability through resource depletion and market exclusion. The network relationship should show upstream causality: this constraint (industrial fleet expansion) affects the companion constraint (small-scale fisher viability as victims of depletion). The ε values differ structurally: industrial fleet viability depends on subsidy and depletion (ε ≈ 0.58), while small-scale fisher viability depends on resource access and competitive capacity (ε different value reflecting different structural mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(chinese_fishing_fleet_viability, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
