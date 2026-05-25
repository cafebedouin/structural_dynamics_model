% ============================================================================
% CONSTRAINT STORY: supply_chain_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supply_chain_concentration, []).

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
 *   constraint_id: supply_chain_concentration
 *   human_readable: Supply Chain Concentration and Extraction
 *   domain: economic/logistics/manufacturing
 *
 * SUMMARY:
 *   Supply chain concentration represents the structural consolidation of
 *   logistics, manufacturing, and distribution into a small number of
 *   dominant firms and platforms. This constraint exhibits a genuine hybrid
 *   structure: it solves real coordination problems (routing optimization,
 *   inventory management, economies of scale, risk pooling) while
 *   simultaneously enabling asymmetric extraction through market
 *   concentration, margin compression, and dependency lock-in. The
 *   constraint's evolution shows extractiveness rising from 0.35 to 0.58 as
 *   consolidation has accelerated, while theater ratio remains moderate
 *   (0.48) because the coordination function is genuinely valuable —
 *   manufacturers do get lower costs through concentrated logistics — even as
 *   extraction mechanisms intensify. The perspectival gap reveals why supply
 *   chain concentration persists despite its extractive character:
 *   beneficiaries (logistics firms, large manufacturers) experience it as
 *   pure coordination; trapped suppliers experience it as pure extraction;
 *   organized nation-states experience it as hybrid with geopolitical
 *   vulnerability; the analytical observer risks naturalizing it as an
 *   immutable economic law when it is actually contingent on legal frameworks
 *   enabling consolidation.
 *
 * KEY AGENTS:
 *   - Small Suppliers: Primary victims (powerless/trapped) — locked into single-customer relationships with no exit; contractual lock-in and capital requirements prevent alternative paths
 *   - Mid-Tier Manufacturers: Secondary victims (moderate/constrained) — benefit from access to concentrated supply networks but bear margin compression and just-in-time risk transfer
 *   - Logistics Incumbents: Primary beneficiaries (institutional/arbitrage) — capture economies of scale and network effects; can arbitrage between suppliers and customers
 *   - Dominant Manufacturers: Secondary beneficiaries (institutional/arbitrage) — access to consolidated logistics reduces supply chain costs; can pass extraction costs to smaller suppliers
 *   - Supply-Dependent Nations: Organized agents (organized/constrained) — benefit from integrated global supply but vulnerable to supply shocks and geopolitical leverage
 *   - Regulatory Bodies: Institutional observers (institutional/arbitrage) — antitrust frameworks persist but show degraded enforcement capacity relative to supply chain complexity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing consolidation as inevitable economic law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supply_chain_concentration, 0.58).
domain_priors:suppression_score(supply_chain_concentration, 0.65).
domain_priors:theater_ratio(supply_chain_concentration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supply_chain_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(supply_chain_concentration, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(supply_chain_concentration, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supply_chain_concentration, tangled_rope).
narrative_ontology:human_readable(supply_chain_concentration, "Supply Chain Concentration and Extraction").
narrative_ontology:topic_domain(supply_chain_concentration, "economic/logistics/manufacturing").

domain_priors:requires_active_enforcement(supply_chain_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supply_chain_concentration, dominant_logistics_firms).
narrative_ontology:constraint_beneficiary(supply_chain_concentration, consolidated_manufacturers).
narrative_ontology:constraint_victim(supply_chain_concentration, small_suppliers).
narrative_ontology:constraint_victim(supply_chain_concentration, dependent_nations).
narrative_ontology:constraint_victim(supply_chain_concentration, consumer_price_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL SUPPLIER (SNARE) — Trapped by capital requirements, contractual lock-in, and switching costs. Single large customer dependency creates maximum extraction. No viable alternative supply chains. Bears full cost of logistics monopoly.
constraint_indexing:constraint_classification(supply_chain_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER MANUFACTURER (TANGLED ROPE) — Constrained by scale requirements and logistics integration costs. Benefits from access to consolidated supply networks (genuine coordination function) but bears asymmetric extraction through margin compression and just-in-time inventory risk transfer.
constraint_indexing:constraint_classification(supply_chain_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LOGISTICS INCUMBENT (ROPE) — Experiences supply chain concentration as pure coordination: routing optimization, economies of scale, network effects. Can arbitrage between suppliers and customers. Net beneficiary with exit options.
constraint_indexing:constraint_classification(supply_chain_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SUPPLY-DEPENDENT NATION (TANGLED ROPE) — Organized agents (nation-states) experience supply concentration as both coordination and extraction. Infrastructure integration benefits exist alongside vulnerability to supply shocks, price spikes, and geopolitical leverage. Genuine coordination function (integrated supply reduces costs) combined with asymmetric extraction (supply denial as weapon).
constraint_indexing:constraint_classification(supply_chain_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY APPARATUS (PITON) — Antitrust frameworks and supply chain resilience mandates persist but show degraded function. Regulatory authority has arbitrage (can select enforcement targets) but enforcement capacity has atrophied relative to supply chain complexity. Theater ratio (0.48) reflects genuine regulation mixed with performative compliance rituals.
constraint_indexing:constraint_classification(supply_chain_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EFFICIENCY VIEW (MOUNTAIN) — From a civilizational perspective, supply chain concentration appears as an immutable consequence of capital efficiency, transportation physics, and economies of scale. The observer risks naturalizing what is a contingent institutional arrangement (legal frameworks enabling consolidation, regulatory choices about merger review, investment capital concentration) as inherent economic law.
constraint_indexing:constraint_classification(supply_chain_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supply_chain_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supply_chain_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supply_chain_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(supply_chain_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(supply_chain_concentration, TR),
    TR >= 0.70.

:- end_tests(supply_chain_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint exhibits strong extraction mechanisms (margin compression, dependency lock-in, price-setting power) but the extraction is bundled with genuine coordination value. Suppliers get access to integrated logistics that reduce their costs; manufacturers get supply reliability. The extraction is real (small suppliers pay a rent-seeking margin to logistics firms) but not maximal because the coordination function is authentic. The rise from 0.35 to 0.58 over the interval reflects increasing consolidation and declining small-supplier alternatives. Suppression (0.65): High. Barriers to exit include capital requirements for alternative supply networks, switching costs (supplier qualification, integration), contractual lock-in through long-term agreements, and information asymmetry (logistics firms control routing and pricing algorithms). Regulatory barriers (compliance with consolidated logistics standards) also suppress alternatives. Theater ratio (0.48): Moderate. The logistics industry uses both genuine optimization and performative efficiency language. Regulatory compliance and 'supply chain resilience' narratives add theater without proportional functional benefit. The moderate value reflects that the coordination function is real — improved routing and inventory management do reduce costs — but significant portions of the operational overhead are theatrical compliance with consolidated platforms.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is fundamental. The logistics incumbent sees a Rope constraint — they are solving the coordination problem of routing goods efficiently. The small supplier sees a Snare — they are locked into dependency with no exit. The mid-tier manufacturer sees a Tangled Rope — they benefit from consolidated logistics but pay extraction margins. The organized nation-state sees a Tangled Rope with geopolitical risk — genuine supply integration benefits alongside vulnerability to supply denial. The regulatory body sees a Piton — the antitrust framework persists but enforcement capacity has atrophied. The civilizational analytical observer risks seeing a Mountain — supply concentration appears as an immutable efficiency law — but the structural data reveals this as naturalization of contingent legal and institutional choices (merger regulations, investment capital concentration, intellectual property frameworks enabling platform lock-in).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values differ sharply across power atoms due to structural position relative to extraction flow. Logistics firms (institutional/arbitrage) have d ≈ 0.10 — they are net beneficiaries with high exit optionality and capture positive extracted rents. Small suppliers (powerless/trapped) have d ≈ 0.95 — they bear full extraction and have no exit. The engine derives d from beneficiary/victim declarations: logistics firms listed as beneficiaries receive low d → low/negative f(d); small suppliers listed as victims receive high d → high f(d). Mid-tier manufacturers (moderate/constrained) have d ≈ 0.60 — they experience both costs (margin compression) and benefits (access), producing moderate directionality. Organized nation-states (organized/constrained) have d ≈ 0.55 — they have some policy agency but face genuine supply vulnerability. The piton classification derives not from high directionality chi but from the theater gate: regulatory frameworks persist with moderate functional value (theater ratio 0.48) despite atrophied enforcement capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy between pure coordination and pure extraction by showing that both functions genuinely exist and are not resolvable into a single type. The coordination function is real: consolidated supply chains do reduce logistics costs, improve inventory efficiency, and enable economies of scale that benefit consumers through lower prices. The extraction function is also real: logistics incumbents capture rents through market concentration, small suppliers bear margin compression and dependency risk, and geopolitical vulnerability is weaponized (supply denial). The Tangled Rope classification accurately captures this hybrid. The false summit (mountain perspective) naturalizes consolidation as inevitable efficiency, when it is actually contingent on regulatory choices about merger review, antitrust enforcement, investment concentration, and platform lock-in mechanisms. The perspectival gap reveals that the constraint is not 'really' one type viewed from different angles — it genuinely instantiates different extraction/coordination ratios for different observers. For the logistics firm it is mostly coordination (Rope). For the small supplier it is mostly extraction (Snare). For organized nation-states it is hybrid with geopolitical amplification (Tangled Rope). The mandatrophy is resolved by accepting that supply chain concentration is a structurally legitimate tangled rope (both coordination and extraction) whose proportions vary across perspectives and whose legal/institutional contingency makes future decomposition possible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    concentration_versus_resilience_tradeoff,
    'Is supply chain concentration a necessary tradeoff for efficiency, or could resilient networks achieve comparable efficiency with distributed redundancy?',
    'Comparative analysis of supply chain cost structures pre/post concentration; simulation of resilient network architectures; pandemic supply data showing concentration failure costs',
    'If concentration necessary: extraction is legitimate coordination cost (Rope from more perspectives). If resilient alternatives exist: concentration is extractive choice (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concentration_versus_resilience_tradeoff, empirical, 'Whether supply concentration is inevitable or chosen for extraction').

omega_variable(
    regulatory_capture_depth,
    'To what extent does the logistics industry capture regulatory standards for supply chain ''efficiency'' that entrench concentration?',
    'Analysis of regulatory comment periods; comparison of proposed vs final supply chain standards; personnel flow between industry and regulatory bodies',
    'If high capture: regulatory framework is inertial (Piton classification confirmed, not mountain). If low: regulation represents genuine public interest (Rope classification strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Depth of industry capture in supply chain regulation').

omega_variable(
    small_supplier_coalition_formation,
    'Can small suppliers overcome collective action barriers to form alternative supply networks, or are the coordination costs prohibitive?',
    'Historical analysis of supplier cooperatives; cost analysis of alternative logistics platforms; game-theoretic modeling of coalition formation under extraction pressure',
    'If coalition formation viable: powerless agents can exit (Snare shifts toward Tangled Rope). If coordination costs prohibitive: trap is structural (Snare classification sustained).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(small_supplier_coalition_formation, empirical, 'Viability of small supplier coalitions as escape from concentration').

omega_variable(
    geopolitical_fragmentation_risk,
    'Will geopolitical fragmentation (trade wars, sanctions, regionalism) force deliberate supply chain decentralization, or will concentration increase as security mechanism?',
    'Scenario modeling; historical analysis of supply chain response to geopolitical shocks; investment flows into reshoring/decentralization vs consolidation',
    'If fragmentation forces decentralization: concentration extraction window is time-bounded. If concentration intensifies under threat: extractive mechanism strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_fragmentation_risk, conceptual, 'Geopolitical direction of future supply chain organization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supply_chain_concentration, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scc_tr_t0, supply_chain_concentration, theater_ratio, 0, 0.32).
narrative_ontology:measurement(scc_tr_t10, supply_chain_concentration, theater_ratio, 10, 0.4).
narrative_ontology:measurement(scc_tr_t20, supply_chain_concentration, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(scc_be_t0, supply_chain_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scc_be_t10, supply_chain_concentration, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(scc_be_t20, supply_chain_concentration, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supply_chain_concentration, resource_allocation).
narrative_ontology:affects_constraint(supply_chain_concentration, semiconductor_supply_dependency).
narrative_ontology:affects_constraint(supply_chain_concentration, rare_earth_concentration).
narrative_ontology:affects_constraint(supply_chain_concentration, just_in_time_fragility).

% DUAL FORMULATION NOTE:
% Supply chain concentration is upstream of specific commodity dependencies (semiconductors, rare earths) and just-in-time vulnerability. Each commodity constraint has its own ε reflecting the empirical concentration in that sector; the general supply chain concentration story models the structural mechanism enabling all sector-specific extractions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supply_chain_concentration, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
