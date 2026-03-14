% ============================================================================
% CONSTRAINT STORY: latin_american_supply_chain_segmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_american_supply_chain_segmentation, []).

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
 *   constraint_id: latin_american_supply_chain_segmentation
 *   human_readable: Latin American Supply Chain Segmentation
 *   domain: economic/geopolitical/trade
 *
 * SUMMARY:
 *   Latin American supply chain segmentation represents a structural
 *   constraint on regional industrial development, where multinational
 *   corporations organize production networks to concentrate high-value
 *   design, IP generation, and final assembly in developed economies while
 *   confining Latin American suppliers to low-margin component manufacturing
 *   and assembly. This constraint coordinates global supply networks while
 *   simultaneously extracting value from the region through suppressed wages,
 *   restricted technology access, and subordination to multinational control.
 *   The structure emerges from active enforcement mechanisms (IP regimes,
 *   trade agreement rules of origin, multinational supplier gatekeeping,
 *   technology transfer restrictions) rather than from natural economic
 *   advantages. The constraint is maintained through both formal
 *   institutional structures (USMCA, TRIPS, FDI screening) and informal
 *   practices (supplier contract terms, investment threat credibility,
 *   technology monopolization). Extractiveness has increased over the 15-year
 *   measurement interval as supply chain concentration has deepened and IP
 *   enforcement has strengthened. Theater ratio reflects that the policy
 *   narrative (supply chain integration as development opportunity) has
 *   increasingly decoupled from actual capacity-creation outcomes, with
 *   multinational and government rhetoric emphasizing 'competitiveness' and
 *   'integration' while regional value capture stagnates.
 *
 * KEY AGENTS:
 *   - Regional Suppliers: Primary victims (powerless/trapped) — locked in assembly roles with no capital or technology access paths to upgrade.
 *   - Multinational Lead Firms: Primary beneficiaries (institutional/arbitrage) — capture margin extraction, technology control, and supply chain optionality.
 *   - National Governments: Secondary actors (moderate/constrained) — face dual mandate: job creation from integration versus lost industrial capacity and policy autonomy.
 *   - Regional Integration Blocks (USMCA countries, MERCOSUR): Institutional negotiators (powerful/mobile) — balance market access gains against subordination in regional hierarchy.
 *   - Domestic Industrial Policy Coalitions: Organized challengers (organized/constrained) — labor unions, domestic manufacturers, policy reformers building exit path through nearshoring and capacity development.
 *   - Washington Consensus / Structural Adjustment Framework: Institutional narrative maintainer (institutional/arbitrage) — sustains segmentation through lending conditions, development bank policy, and technocrat alignment despite declining functional credibility.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent enforcement mechanisms as natural economic laws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_american_supply_chain_segmentation, 0.58).
domain_priors:suppression_score(latin_american_supply_chain_segmentation, 0.62).
domain_priors:theater_ratio(latin_american_supply_chain_segmentation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_american_supply_chain_segmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(latin_american_supply_chain_segmentation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(latin_american_supply_chain_segmentation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_american_supply_chain_segmentation, tangled_rope).
narrative_ontology:human_readable(latin_american_supply_chain_segmentation, "Latin American Supply Chain Segmentation").
narrative_ontology:topic_domain(latin_american_supply_chain_segmentation, "economic/geopolitical/trade").

domain_priors:requires_active_enforcement(latin_american_supply_chain_segmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_american_supply_chain_segmentation, multinational_lead_firms).
narrative_ontology:constraint_beneficiary(latin_american_supply_chain_segmentation, advanced_economy_final_consumers).
narrative_ontology:constraint_victim(latin_american_supply_chain_segmentation, regional_manufacturing_capacity).
narrative_ontology:constraint_victim(latin_american_supply_chain_segmentation, local_value_capture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL SUPPLIER (SNARE) — Locked into low-value assembly and component roles with no exit path. Suppression is structural: capital requirements to move upstream are prohibitive, technology access is restricted by IP regimes, and supply chain switching costs lock suppliers into contracts with lead firms. Bearing maximum extraction with no agency.
constraint_indexing:constraint_classification(latin_american_supply_chain_segmentation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NATIONAL GOVERNMENT (TANGLED ROPE) — Operates under conflicting coordination goals (employment via supply chain participation) and extraction costs (technology transfer refusal, profit repatriation, wage suppression to remain 'competitive'). Has constrained exit — can theoretically renegotiate terms but faces disinvestment threats. Both benefits from integration (manufacturing employment) and bears extraction (lost policy autonomy, suppressed industrial capacity).
constraint_indexing:constraint_classification(latin_american_supply_chain_segmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTINATIONAL LEAD FIRM (ROPE) — Experiences the constraint as pure coordination mechanism: managing global supply networks, risk diversification, cost optimization. Has arbitrage options (move production to other regions, shift supplier relationships). Net beneficiary from the segmentation structure — captures margin extraction and retains technology control. Sees constraint as efficient value distribution.
constraint_indexing:constraint_classification(latin_american_supply_chain_segmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL INTEGRATION BLOCK (TANGLED ROPE) — Trade agreements (USMCA, etc.) coordinate market access while enforcing extractive rules of origin and IP provisions that lock smaller economies into assembly roles. Has mobile options (renegotiate terms, form alternative blocs) but faces coordinated pressure from lead-firm countries. Benefits from regional market integration; bears costs of subordinate position within the regional hierarchy.
constraint_indexing:constraint_classification(latin_american_supply_chain_segmentation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: DOMESTIC INDUSTRIAL POLICY COALITION (SCAFFOLD) — Organized labor, domestic manufacturers, and policy reformers see the segmentation as a temporary constraint being dissolved by: local value chain development, nearshoring reshuffling post-pandemic, regional technology transfer initiatives, and trade union power building. Constrained by multinational political influence but organized enough to see the exit path. Sunset logic: as regional technical capacity and trade negotiating power mature over 15-25 years, the segmentation structure's grip weakens.
constraint_indexing:constraint_classification(latin_american_supply_chain_segmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: WASHINGTON CONSENSUS / STRUCTURAL ADJUSTMENT PROTOCOLS (PITON) — The institutional narrative framing (trade is development, supply chain integration is upgrading, multinational investment is growth) persists despite declining functional credibility. Theater ratio reflects that the policy discourse (opportunity narratives) has decoupled from actual capacity creation outcomes. The framework maintains itself through institutional inertia: multilateral lending conditions, development bank policies, and government technocrat alignment, even as empirical evidence of value capture dynamics accumulates.
constraint_indexing:constraint_classification(latin_american_supply_chain_segmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / RICARDIAN VIEW (MOUNTAIN) — From a civilizational perspective, comparative advantage naturally produces supply chain specialization. Economies with lower labor costs are structurally optimized for assembly; economies with capital and technology are optimized for design and IP. This perspective sees segmentation as a natural economic law flowing from factor endowments. However, the structural data reveals this as a false summit: the segmentation is maintained through active enforcement (IP regimes, technology transfer restrictions, tariff structures, FDI screening) rather than emerging naturally from economic fundamentals.
constraint_indexing:constraint_classification(latin_american_supply_chain_segmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_american_supply_chain_segmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(latin_american_supply_chain_segmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(latin_american_supply_chain_segmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_american_supply_chain_segmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(latin_american_supply_chain_segmentation, TR),
    TR >= 0.70.

:- end_tests(latin_american_supply_chain_segmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts significant value from the region through (1) suppressed wages maintained by competitive bidding between suppliers, (2) margin capture by multinationals on assembled goods, (3) blocked technology access that prevents indigenous capability development, (4) profit repatriation that reduces reinvestment in local capacity. The extraction is not at maximum (0.72) because genuine manufacturing employment does flow to the region and some coordination benefits exist (supply stability, access to global markets). The 15-year trajectory shows steady increase as supply chain concentration and IP enforcement have deepened. Suppression (0.62): High. Multiple coordinated barriers prevent exit: (a) Capital requirements for technology adoption far exceed regional firm capacity, (b) IP regimes legally restrict reverse engineering or indigenous development of essential technologies, (c) Multinational supplier contracts lock suppliers into exclusive relationships with termination penalties, (d) Rules of origin in trade agreements restrict suppliers' ability to source components regionally or upgrade locally, (e) Wage competition between suppliers prevents collective action on labor standards. Theater ratio (0.48): Moderate. The discourse emphasizes integration as development opportunity; the reality is constrained opportunity. The theater is not extreme (0.70+) because the constraint's coordination function is partially genuine — multinational supply networks do exist and do operate efficiently. But the narrative obscures the extraction mechanism (value concentration, technology gatekeeping) so theater has increased as the gap between opportunity rhetoric and actual capacity outcomes has widened.
 *
 * PERSPECTIVAL GAP:
 *   Perspectival disagreement is maximal. Multinational lead firms see rope (coordination of global supply networks with legitimate cost optimization). National governments see tangled_rope (genuine coordination benefits from manufacturing jobs offset against lost industrial autonomy and wage suppression costs). Regional suppliers see snare (maximum extraction with no exit). Regional integration blocks see tangled_rope at the regional level (some coordination within the bloc against subordination to multinational/developed-economy dominance). Organized coalitions see scaffold with sunset (the constraint is temporary; nearshoring and capacity development will dissolve it over 15-25 years). Institutional gatekeeping narratives (Washington Consensus) see piton (the framework persists through inertia; its functional credibility has eroded as outcomes have disappointed). The civilizational analytical view risks mountain (comparative advantage naturally produces this segmentation) — but the structural data reveals this as false: active enforcement mechanisms maintain segmentation against what would be natural diffusion of technology and capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position: (1) Regional suppliers are victims with trapped exit → d ≈ 0.92 → high f(d) → experience high chi. (2) Multinationals are beneficiaries with arbitrage exit → d ≈ 0.10 → low f(d) → experience low/negative chi (the constraint subsidizes them). (3) Governments are mixed (benefit from employment, bear extraction from lost autonomy) with constrained exit → d ≈ 0.50 → f(d) ≈ 0.65 → experience moderate chi. (4) Regional integration blocks are beneficiaries with mobile exit options → d ≈ 0.35 → moderate f(d). (5) Industrial policy coalitions are victims with constrained exit and organizing power → d ≈ 0.65 → f(d) ≈ 1.00. These d values reflect that the extraction flow is predominantly toward multinationals and away from regional suppliers, with governments navigating a mixed position. The chi scaling by scope (σ=0.9 for regional scope) dampens the effective extraction slightly — if the constraint operated at global scope (σ=1.2), chi would be amplified by 33%, reflecting that large-scope supply chains are harder to verify/challenge than small ones.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies mandatrophy resolution through structural analysis. The false summit at the analytical level (mountain/Ricardian view) appears only when enforcement mechanisms are invisible — treated as natural constraints rather than institutional choices. The tangled_rope classification at the government level captures the dual mandate dilemma: governments genuinely benefit from manufacturing employment (coordination function) while being systematically extracted from through lost industrial capacity development and wage suppression (asymmetric extraction). The snare classification at the supplier level is unambiguous — the combination of trapped exit and high extraction defines snare completely. The piton at the institutional narrative level (Washington Consensus framework) reflects that the policy discourse maintaining the structure has lost functional credibility — the theater ratio (0.48) shows the gap between rhetoric and outcome. The scaffold at the coalition level represents the exit path being constructed: this is not a false hope but a genuine structural feature (nearshoring, regional capacity initiatives, trade policy renegotiation) with measurable progress, though success is not yet assured (confidence: medium). The mandatrophy is resolved by recognizing that no single type is 'correct' — the perspectival presheaf over observation positions IS the answer. Each perspective captures a real structural position and real experienced classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_transfer_constraint_vs_market_efficiency,
    'Is technology access restriction a natural market outcome or an artificially maintained enforcement mechanism?',
    'Historical analysis of technology diffusion rates in periods with and without active IP enforcement; comparison to pre-TRIPS regimes; impact assessment of open-source and patent-pool initiatives on regional capacity.',
    'If natural: segmentation is mountain (structural economic law). If enforced: segmentation is tangled_rope/snare with contingent suppression. Classification hinges on whether tech barriers are exogenous or maintained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_transfer_constraint_vs_market_efficiency, empirical, 'Whether technology barriers are natural or actively enforced').

omega_variable(
    regional_capacity_vs_multinational_gatekeeping,
    'Do regional suppliers lack capability to upgrade to higher-value production, or are they systematically excluded by multinational gatekeeping of contracts?',
    'Case studies of successful regional suppliers who moved upstream; analysis of contract terms restricting supplier innovation; comparison of supplier capacity in regions with strong independent manufacturers vs supplier-dependent regions.',
    'If capability gap: segmentation is rope (efficient division). If gatekeeping: segmentation is snare (extraction mechanism). Directionality and exit_options depend critically on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_capacity_vs_multinational_gatekeeping, empirical, 'Whether segmentation reflects capability gap or systematic exclusion').

omega_variable(
    trade_agreement_coordination_vs_extraction_vehicle,
    'Do trade agreements (USMCA, etc.) primarily coordinate market access or primarily enforce supplier subordination?',
    'Text analysis of rules of origin, technology transfer provisions, and IP chapters; empirical measurement of value distribution before/after agreement implementation; comparison of negotiating power distribution.',
    'If primarily coordination: agreements are rope. If primarily enforcement of extraction: agreements are tangled_rope/snare. Classification of government perspective hinges on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trade_agreement_coordination_vs_extraction_vehicle, empirical, 'Whether trade agreements coordinate or enforce extraction').

omega_variable(
    nearshoring_sustainability_and_true_decoupling,
    'Does post-pandemic nearshoring represent structural decoupling from the segmentation constraint or temporary crisis response with reversion risk?',
    'Multi-year tracking of supply chain localization; analysis of whether manufacturing gains translate to local value capture or remain assembly-level; monitoring of multinational reshoring vs permanent relocation.',
    'If structural: scaffold sunset is real and constraint lifetime is 15-25 years. If temporary: segmentation persists and scaffold perspective is aspirational rather than structural. Affects measurement trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nearshoring_sustainability_and_true_decoupling, empirical, 'Whether nearshoring represents durable decoupling from segmentation').

omega_variable(
    regional_political_economy_coalition_viability,
    'Can organized labor and domestic manufacturers build sufficient political power to negotiate de-segmentation, or does multinational influence durably subordinate regional coalitions?',
    'Analysis of successful vs failed industrial policy initiatives; mapping of political economy coalitions and their leverage; assessment of union/manufacturer coordinating capacity vs multinational political spending.',
    'If viable: scaffold coalition has real exit path and sunset is credible. If durably subordinated: scaffold is aspirational theater, constraint persists indefinitely. Affects long-term classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_political_economy_coalition_viability, empirical, 'Whether regional coalitions can durably challenge segmentation structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_american_supply_chain_segmentation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lasc_tr_t0, latin_american_supply_chain_segmentation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lasc_tr_t5, latin_american_supply_chain_segmentation, theater_ratio, 5, 0.42).
narrative_ontology:measurement(lasc_tr_t10, latin_american_supply_chain_segmentation, theater_ratio, 10, 0.48).
narrative_ontology:measurement(lasc_tr_t15, latin_american_supply_chain_segmentation, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(lasc_be_t0, latin_american_supply_chain_segmentation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lasc_be_t5, latin_american_supply_chain_segmentation, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(lasc_be_t10, latin_american_supply_chain_segmentation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(lasc_be_t15, latin_american_supply_chain_segmentation, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_american_supply_chain_segmentation, resource_allocation).
narrative_ontology:affects_constraint(latin_american_supply_chain_segmentation, multinational_labor_arbitrage).
narrative_ontology:affects_constraint(latin_american_supply_chain_segmentation, intellectual_property_regime_enforcement).
narrative_ontology:affects_constraint(latin_american_supply_chain_segmentation, trade_agreement_rules_of_origin).
narrative_ontology:affects_constraint(latin_american_supply_chain_segmentation, regional_manufacturing_capacity_development).

% DUAL FORMULATION NOTE:
% Latin American supply chain segmentation is the aggregate outcome of multiple coordinated enforcement mechanisms: IP regimes restrict technology diffusion, trade agreements enforce rules of origin that lock suppliers into assembly roles, multinational gatekeeping controls supplier relationships, and wage competition prevents regional collective action. Each mechanism has its own ε value and story; the supply chain segmentation story treats the aggregate effect. Upstream constraints (IP enforcement, trade agreement design) have lower ε values reflecting their specialized function; downstream constraints (labor arbitrage, manufacturing capacity suppression) have higher ε values reflecting the cumulative effect of all upstream mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(latin_american_supply_chain_segmentation, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
