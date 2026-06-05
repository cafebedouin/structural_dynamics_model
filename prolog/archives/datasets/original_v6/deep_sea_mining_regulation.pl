% ============================================================================
% CONSTRAINT STORY: deep_sea_mining_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deep_sea_mining_regulation, []).

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
 *   constraint_id: deep_sea_mining_regulation
 *   human_readable: Deep Sea Mining Regulation
 *   domain: environmental/maritime/economic
 *
 * SUMMARY:
 *   Deep sea mining regulation creates a structural tension between resource
 *   scarcity for energy transition (cobalt dependency for EV batteries),
 *   fiscal survival for vulnerable island states, and ecosystem preservation
 *   in planetary commons. The constraint operates across multiple
 *   institutional levels: mining corporations seeking cost-stable supply
 *   chains, island states seeking revenue alternatives to climate adaptation,
 *   tech manufacturers seeking supply security, environmental coalitions
 *   seeking ecosystem protection, and the International Seabed Authority
 *   maintaining regulatory legitimacy while lacking enforcement capacity. The
 *   constraint exhibits tangled_rope structure: there is genuine coordination
 *   (resource pricing security for manufacturers, fiscal bridge for island
 *   economies) alongside asymmetric extraction (ecosystem collapse
 *   concentrated on powerless agents, regulatory capture concentrated on
 *   corporations and complicit states). Theater ratio (0.68) reflects the
 *   ISA's performative environmental assessment process — rigorous in
 *   appearance, weak in enforcement, enabling licensing while preserving
 *   legitimacy. The measurement trajectory shows extractiveness increasing
 *   over time as more licenses are granted and ecosystem impacts accumulate,
 *   while theater ratio rises as the ISA's assessment process becomes more
 *   elaborate without improving actual protection.
 *
 * KEY AGENTS:
 *   - Mining Corporations: Beneficiary (institutional/arbitrage) — capture supply-chain security and regulatory predictability; experience constraint as enabling coordination
 *   - Island Nations: Victim (powerless/trapped) — face fiscal dependency on mining revenue despite ecosystem risks; trapped between economic collapse and environmental collapse
 *   - Tech Manufacturers: Mixed (moderate/constrained) — benefit from cobalt supply stability but locked into single supply source; constrained by both mining and alternative-source transition costs
 *   - Artisanal Fishing Communities: Victim (powerless/trapped) — face livelihood extinction from ecosystem damage; no formal negotiating power; localized in impact zone
 *   - Environmental Coalition: Organized opponent (organized/constrained) — have advocacy platforms and can mobilize opposition but require continuous resource investment and coalition maintenance
 *   - International Seabed Authority: Institutional maintainer (institutional/arbitrage) — preserves legitimacy through regulatory theater while lacking enforcement capacity to prevent mining
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (regulatory capture, island revenue dependency, cobalt-dependent technology) as immutable resource scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deep_sea_mining_regulation, 0.58).
domain_priors:suppression_score(deep_sea_mining_regulation, 0.65).
domain_priors:theater_ratio(deep_sea_mining_regulation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deep_sea_mining_regulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(deep_sea_mining_regulation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(deep_sea_mining_regulation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deep_sea_mining_regulation, tangled_rope).
narrative_ontology:human_readable(deep_sea_mining_regulation, "Deep Sea Mining Regulation").
narrative_ontology:topic_domain(deep_sea_mining_regulation, "environmental/maritime/economic").

domain_priors:requires_active_enforcement(deep_sea_mining_regulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deep_sea_mining_regulation, mining_corporations).
narrative_ontology:constraint_beneficiary(deep_sea_mining_regulation, cobalt_dependent_tech_manufacturers).
narrative_ontology:constraint_victim(deep_sea_mining_regulation, developing_island_states).
narrative_ontology:constraint_victim(deep_sea_mining_regulation, ocean_ecosystem_integrity).
narrative_ontology:constraint_victim(deep_sea_mining_regulation, artisanal_fishing_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISLAND NATION (SNARE) — Small island developing states face existential climate risk and economic collapse. Deep sea mining licensing offers short-term revenue but locks them into oceanic extraction dependency. Exit costs are catastrophic: refusing mining threatens their fiscal survival; accepting mining threatens their survival via ecosystem collapse. No genuine choice exists. Maximum experienced extraction.
constraint_indexing:constraint_classification(deep_sea_mining_regulation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ARTISANAL FISHING COMMUNITY (SNARE) — Localized fishing communities dependent on deep-sea ecosystems face livelihood extinction from mining activity. They have no formal voice in licensing negotiations, no exit option except relocation (impossible without skills transfer), and no compensation pathway that preserves their way of life. Pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(deep_sea_mining_regulation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: TECH MANUFACTURER (TANGLED ROPE) — Battery manufacturers genuinely benefit from cost-stable cobalt supply (coordination function: secure materials pricing reduces supply-chain risk). But the constraint also extracts from them: mining-dependent cobalt supply locks them into a single extraction source, creating vulnerability to price shocks and regulatory closure. They face high costs to develop alternative cobalt sources or battery chemistries. Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(deep_sea_mining_regulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MINING CORPORATION (ROPE) — Experiences the regulatory framework as pure coordination: licensing rules enable long-term planning, environmental standards are predictable compliance costs, and international agreements provide legitimacy for operations. The corporation can arbitrage between permissive and restrictive jurisdictions. Net beneficiary — extraction flows toward them through favorable licensing terms.
constraint_indexing:constraint_classification(deep_sea_mining_regulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ENVIRONMENTAL COALITION (TANGLED ROPE) — NGOs, environmental coalitions, and scientific bodies have organized advocacy platforms (coordination function: they can mobilize public opinion, generate scientific consensus, and block licensing). But the constraint also extracts from them: anti-mining advocacy requires continuous resource expenditure, and their ability to block mining depends on maintaining coalition cohesion against corporate lobbying. Moderate experienced extraction with real but constrained agency.
constraint_indexing:constraint_classification(deep_sea_mining_regulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL SEABED AUTHORITY (PITON) — The ISA was created to govern deep-sea mining but has become largely performative. The body produces technical standards, environmental impact assessments, and licensing frameworks that appear rigorous but lack enforcement mechanisms. Mining proceeds via regulatory theater: assessments are conducted, objections are formally recorded, and approvals proceed regardless. The ISA sees its own process as degraded — it maintains the ritual of environmental review despite knowing enforcement is weak. Piton classification: high theater (0.68), minimal functional governance.
constraint_indexing:constraint_classification(deep_sea_mining_regulation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From a civilizational perspective, the tension between resource scarcity and environmental limits appears immutable: humanity needs cobalt for renewable energy; cobalt exists in the deep sea; extraction inevitably causes ecosystem damage; developing nations inevitably depend on resource rents. This perspective naturalizes the constraint as an inescapable consequence of physics and economics. However, the structural data contradicts mountain classification — the constraint is contingent on specific institutional arrangements (regulatory capture, lack of alternative funding for island nations, technological choices favoring cobalt) not on immutable physical laws. False summit.
constraint_indexing:constraint_classification(deep_sea_mining_regulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deep_sea_mining_regulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deep_sea_mining_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deep_sea_mining_regulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deep_sea_mining_regulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(deep_sea_mining_regulation, TR),
    TR >= 0.70.

:- end_tests(deep_sea_mining_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint involves genuine resource coordination (stable cobalt supply enables renewable energy transition) but with asymmetric distribution of costs and benefits. Mining corporations capture supply-security gains while island states bear ecosystem risks. The extractiveness is not as severe as a pure snare (0.72+) because the coordination function is real — renewable energy transition does require cobalt — but it is high because the extraction mechanism (regulatory capture, island fiscal dependency) prevents cost-sharing or alternative pathways. Suppression (0.65): High. Powerless agents (island nations, fishing communities) face severe barriers to exit: island nations depend on mining revenue for fiscal survival; fishing communities depend on ocean ecosystems but have no formal negotiating power. Even organized agents (environmental coalitions) face high suppression in the form of resource costs and coalition fragility. Institutional actors (corporations, ISA) face low suppression — they can arbitrage between jurisdictions or abandon regulation. Theater ratio (0.68): Moderate-high and increasing. The ISA conducts environmental impact assessments, engages stakeholder consultation, and produces technical standards (0.42 baseline). But enforcement mechanisms are weak, and scientific recommendations are often disregarded. As more mining licenses are granted despite environmental objections, the theater becomes more elaborate (0.68 at current) — assessments become more sophisticated and objections more formally recorded while outcomes remain predetermined. This trajectory indicates degradation from rope (legitimate coordination with imperfect information) toward piton (performative ritual with known weakness).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates why indexical classification reveals what single-perspective analysis obscures. The mining corporation's perspective (rope: 'we coordinate resource supply and environmental standards') is their genuine structural experience — from their position, the regulation is enabling and fair. The island nation's perspective (snare: 'we are trapped between fiscal survival and ecosystem collapse') is equally genuine — from their position, the regulation is coercive and offers no real choice. Neither perspective is false; both are structurally accurate from their indexical positions. The gap reveals the asymmetry: the corporation's exit costs (finding alternative jurisdictions) are low; the island nation's exit costs (finding alternative revenue or refusing mining) are catastrophic. The ISA's perspective (piton: 'we conduct rigorous environmental review but enforcement is weak') reveals institutional degradation: the assessment process persists as legitimate theater despite everyone knowing it cannot prevent mining. The analytical perspective at civilizational time (mountain: 'resource scarcity makes mining inevitable') naturalizes what is actually a contingent institutional arrangement, revealing the oracle gap — the analytical observer's position itself is structured by the same institutional capture that makes mining appear inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from the structural position of each agent relative to extraction. Mining corporations occupy d ≈ 0.05 (beneficiaries with arbitrage exit — they can move operations or abandon mining) producing low effective extraction (f(d) ≈ -0.12). Island nations occupy d ≈ 0.90 (victims with trapped exit — no escape from either mining revenue dependency or ecosystem collapse) producing high effective extraction (f(d) ≈ 1.40). Tech manufacturers occupy d ≈ 0.55 (mixed position: constrained suppliers needing materials but unable to shift sources easily) producing moderate extraction (f(d) ≈ 0.75). Environmental coalitions occupy d ≈ 0.60 (victims with constrained exit: can organize opposition but face resource and coalition-fragility costs) producing moderate extraction (f(d) ≈ 0.85). ISA occupies institutional d ≈ 0.20 (nominal coordinator but captured by mining interests, similar to regulated utility) producing low effective extraction (f(d) ≈ 0.02). Scope amplification (σ(S) = 1.2 for global scope) scales the effective extraction, making power asymmetries more pronounced at large scale.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint is not purely extractive (snare) because genuine coordination exists — renewable energy transition requires cobalt supply stability, and regulated mining provides that. The constraint is not pure coordination (rope) because asymmetric extraction is enforced: island states bear ecosystem risks while corporations capture supply security. The constraint is not temporary (scaffold) because the sunset mechanism is absent — island states are expected to continue mining indefinitely despite ecosystem damage, and no alternative funding pathway is being systematized. The constraint is not degraded (piton) yet because the regulatory process, while theatrical, still produces nominal environmental constraints that mining companies must address (even if weakly). The constraint is TANGLED ROPE because: (1) beneficiaries (mining corporations, tech manufacturers) genuinely benefit from resource coordination and price stability; (2) victims (island states, fishing communities, ecosystem integrity) bear asymmetric costs; (3) active enforcement (ISA licensing, environmental standards, compliance monitoring) exists, even if weak; (4) no coordination function would exist without extraction, and no extraction would proceed without coordination cover. Mandatrophy resolves by recognizing that the classification is stable across all high-confidence perspectives (mining corp = rope, island nation = snare, tech mfg = tangled rope, coalition = tangled rope, ISA = piton) except for the analytical observer's false summit attempt (mountain). The false summit is itself diagnostic: it reveals how naturalizing rhetoric ('resource scarcity') masks institutional choices (regulatory capture, island fiscal dependency, technology lock-in). The constraint is not a law of nature; it is a tangled coordination-extraction hybrid that appears inevitable only from the perspective of agents who benefit from the current institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecosystem_recovery_timeline,
    'What is the true recovery timeline for deep-sea ecosystems after mining cessation? Does it support the ''temporary extraction'' narrative or reveal permanent ecosystem collapse?',
    'Long-term ecological monitoring data from experimental mining sites; comparison with baseline deep-sea biodiversity; modeling of microbial community recovery and chemosynthetic ecosystem re-establishment',
    'If recovery is plausible (50+ years): constraint appears temporary (scaffold). If recovery is implausible (centuries or never): constraint appears permanent (snare), and ''sustainable mining'' framing becomes theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecosystem_recovery_timeline, empirical, 'Ecosystem recovery timeline after mining cessation').

omega_variable(
    alternative_cobalt_sources,
    'Are economically viable terrestrial cobalt sources (improved recycling, laterite mining, deep laterite processing) genuinely available alternatives, or is deep-sea mining framing them as economically impossible to justify oceanic extraction?',
    'Cost-benefit analysis of alternative cobalt sources; comparison of lifecycle extraction costs (terrestrial vs deep-sea) including environmental externalities; technological roadmap for battery recycling scale-up',
    'If alternatives exist at comparable cost: mining is extraction (high d). If alternatives are genuinely more expensive: mining is coordination (lower d), and island-state revenue dependency becomes the primary extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_cobalt_sources, empirical, 'Viability of alternative cobalt sources relative to deep-sea mining').

omega_variable(
    island_state_revenue_fungibility,
    'Are licensing revenues from deep-sea mining genuinely necessary for island-state fiscal survival, or are they being prioritized over alternative development pathways (climate finance, blue economy non-extractive tourism, debt relief)?',
    'Comparative fiscal analysis of island economies; examination of alternative revenue sources and funding availability; political economy of IMF/World Bank development conditions that create mining dependency',
    'If alternative funding is available: mining appears as optional extraction (lower suppression). If alternatives are structurally blocked: mining appears as forced trap (higher suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(island_state_revenue_fungibility, empirical, 'Whether island-state revenue dependency on mining is structural or contingent').

omega_variable(
    regulatory_capture_extent,
    'How much of the ISA''s permissive regulatory posture reflects genuine scientific uncertainty about mining impacts versus active capture by mining-industry lobbying and island-state revenue interest?',
    'Analysis of ISA contractor appointments, funding sources, and voting patterns; comparison of ISA environmental standards with peer scientific recommendations; investigation of island-state voting shifts following mining-licensing discussions',
    'If capture is dominant: ISA classification is piton with thin legitimacy. If genuine uncertainty is dominant: ISA classification is rope with warranted caution, and stronger governance is appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Extent of regulatory capture in the International Seabed Authority').

omega_variable(
    extraction_direction_ambiguity,
    'Does mining extraction flow from corporations to island states (beneficiary relationship) or from island states to corporations (victim relationship)? Revenue to states masks structural dependency.',
    'Long-term fiscal accounting for island economies post-mining (sustainability of revenues, capacity to transition away from mining, fiscal autonomy); comparison with terrestrial mining-dependent economies (resource curse dynamics, institutional capture, revenue volatility)',
    'If island states accumulate fiscal capacity: extraction is bounded. If island states degrade institutional capacity (resource curse): extraction is severe and masked by nominal revenue flows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_direction_ambiguity, empirical, 'True direction of economic extraction in deep-sea mining licensing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deep_sea_mining_regulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_tr_t0, deep_sea_mining_regulation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dsm_tr_t5, deep_sea_mining_regulation, theater_ratio, 5, 0.58).
narrative_ontology:measurement(dsm_tr_t10, deep_sea_mining_regulation, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(dsm_be_t0, deep_sea_mining_regulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dsm_be_t5, deep_sea_mining_regulation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dsm_be_t10, deep_sea_mining_regulation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deep_sea_mining_regulation, resource_allocation).
narrative_ontology:boltzmann_floor_override(deep_sea_mining_regulation, 0.18).
narrative_ontology:affects_constraint(deep_sea_mining_regulation, cobalt_supply_chain_vulnerability).
narrative_ontology:affects_constraint(deep_sea_mining_regulation, island_state_climate_adaptation_funding).
narrative_ontology:affects_constraint(deep_sea_mining_regulation, ocean_ecosystem_governance).
narrative_ontology:affects_constraint(deep_sea_mining_regulation, international_seabed_authority_capture).

% DUAL FORMULATION NOTE:
% Deep sea mining regulation is downstream of specific material scarcity constraints (cobalt supply chain vulnerability) and island-state fiscal constraints (climate adaptation funding deficits). The regulation itself is a coordination-extraction hybrid that attempts to balance these upstream constraints but creates its own extractive effects. The network includes both material constraints (supply chain, ecosystem) and institutional constraints (regulatory capture, revenue dependency). Each upstream constraint has its own story; deep sea mining is their intersection point.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deep_sea_mining_regulation, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
