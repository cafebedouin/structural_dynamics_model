% ============================================================================
% CONSTRAINT STORY: deforestation_commons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deforestation_commons, []).

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
 *   constraint_id: deforestation_commons
 *   human_readable: Deforestation Commons Trap
 *   domain: environmental/economic/social
 *
 * SUMMARY:
 *   Deforestation in tropical and subtropical regions represents a structural
 *   trap where immediate economic incentives for land conversion are
 *   misaligned with long-term ecosystem stability and indigenous welfare. The
 *   constraint exhibits the defining properties of a snare: high
 *   extractiveness concentrated on powerless agents (indigenous communities,
 *   future generations) with minimal coordination benefit; suppression
 *   maintained through state power (resource concessions, property law),
 *   market structure (global commodity prices set elsewhere), and capital
 *   concentration (frontier investors with exit capacity); and growing
 *   theater (conservation certifications, REDD+ programs) that does not
 *   meaningfully reduce deforestation at scale. The constraint operates
 *   across multiple institutional levels — from local land-use decisions by
 *   farmers and logging operators to global commodity markets to
 *   international environmental governance — creating a multi-scale
 *   extraction mechanism where no single actor perceives sufficient incentive
 *   to coordinate conservation. Extractiveness has increased over the
 *   interval (0.42 → 0.72) as commodity prices have risen and frontier
 *   expansion has accelerated. Theater has also increased (0.35 → 0.58) as
 *   conservation narratives, certification programs, and
 *   payment-for-ecosystem-services mechanisms have proliferated without
 *   proportional deforestation reduction. This Goodhart drift (theater rising
 *   while actual deforestation persists) is diagnostic of snare dynamics:
 *   performative responses to extraction that do not alter the underlying
 *   power asymmetry.
 *
 * KEY AGENTS:
 *   - Indigenous forest communities: Primary victims (powerless/trapped) — structurally dependent on forest resources; cannot exit; face violent and legal suppression of land claims; bear full cost of land loss and ecosystem service degradation
 *   - Downstream agricultural and urban communities: Secondary victims (powerless/trapped) — depend on forest-mediated hydrology; face water loss, flooding, and soil degradation; no individual exit options
 *   - Global climate system and future generations: Aggregate victim (powerless/trapped) — cannot exit atmospheric carbon accumulation; bear distributed cost of emission from deforestation; have zero agency within the constraint
 *   - Industrial logging operators: Institutional beneficiary (institutional/arbitrage) — capture direct extraction value; can exit to new frontiers or substitute commodities; experience constraint as coordination opportunity
 *   - Global agricultural commodity markets: Institutional beneficiary (institutional/arbitrage) — coordinate production and pricing; capture value from cheap land conversion; have exit flexibility through geographic arbitrage
 *   - National governments and environmental regulators: Constrained institutional actor (organized/constrained) — tasked with both resource extraction revenue and environmental protection; face fiscal dependency and international pressure; have agency but face significant costs to enforcement
 *   - Financial capital and venture investors: Paradoxical actor (powerful/trapped) — possess structural power but experience snare-level extraction due to capital lock and regulatory risk; profit from land conversion but cannot easily exit positions as climate and regulatory risks rise
 *   - International conservation NGOs: Institutional performer (institutional/constrained) — maintain conservation programs and rhetoric; lack enforcement power; persist through funding flows despite limited impact; represent piton degradation
 *   - International environmental coalition and REDD+ mechanisms: Temporary support (organized/constrained) — coordinate payment-for-ecosystem-services and carbon markets; provide genuine but limited alternative incentives; represent scaffold with generational sunset horizon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deforestation_commons, 0.68).
domain_priors:suppression_score(deforestation_commons, 0.72).
domain_priors:theater_ratio(deforestation_commons, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deforestation_commons, extractiveness, 0.68).
narrative_ontology:constraint_metric(deforestation_commons, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(deforestation_commons, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deforestation_commons, snare).
narrative_ontology:human_readable(deforestation_commons, "Deforestation Commons Trap").
narrative_ontology:topic_domain(deforestation_commons, "environmental/economic/social").

domain_priors:requires_active_enforcement(deforestation_commons).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deforestation_commons, industrial_logging_operators).
narrative_ontology:constraint_beneficiary(deforestation_commons, agricultural_frontier_investors).
narrative_ontology:constraint_beneficiary(deforestation_commons, global_commodity_markets).
narrative_ontology:constraint_victim(deforestation_commons, indigenous_forest_communities).
narrative_ontology:constraint_victim(deforestation_commons, local_downstream_communities).
narrative_ontology:constraint_victim(deforestation_commons, global_climate_system).
narrative_ontology:constraint_victim(deforestation_commons, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS FOREST COMMUNITY (SNARE) — Structurally trapped. No realistic exit from forest-dependent livelihood; cannot resist logging operations; traditional land rights unenforceable against state-backed resource concessions; relocation offers no alternative (debt peonage, cultural dissolution). Bears maximum extraction through land loss, resource degradation, and cultural destruction. Zero degrees of freedom for this agent within the constraint.
constraint_indexing:constraint_classification(deforestation_commons, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DOWNSTREAM AGRICULTURAL COMMUNITIES (SNARE) — Hydrological regime collapse through watershed deforestation. Loss of dry-season water, increased flooding, soil degradation in previously productive agricultural land. No exit option — cannot relocate water source or reverse deforestation individually. Trapped by geographic dependence on forest-mediated hydrology. High extraction with no alternatives.
constraint_indexing:constraint_classification(deforestation_commons, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: GLOBAL CLIMATE SYSTEM / FUTURE GENERATIONS (SNARE) — Trapped by atmospheric carbon accumulation and carbon sink loss. No individual agent can exit the climatic consequences of distributed deforestation. Extraction manifests as climate instability, biodiversity loss, and resource scarcity. This perspective has zero agency; the constraint is unidirectional extraction with no coordination benefit.
constraint_indexing:constraint_classification(deforestation_commons, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: INDUSTRIAL LOGGING OPERATORS (ROPE) — High-value extraction, but also genuine coordination function: logistics networks, supply chains, market relationships, equipment and labor coordination. Experience the constraint as a coordination mechanism enabling resource access. Net beneficiary during immediate term; can exit via resource depletion or regulatory change (arbitrage options). Effective extraction is bounded by their ability to arbitrage to new frontiers or substitute commodities.
constraint_indexing:constraint_classification(deforestation_commons, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL COMMODITY MARKETS (ROPE) — Pure coordination: price signals, futures markets, production networks. Land clearance for soy, cattle, palm oil represents rational response to market signals. Experience constraint as efficient resource allocation. Can arbitrage between commodities, regions, and production methods. Net beneficiaries from cheap land conversion. Structural position offers maximum exit flexibility.
constraint_indexing:constraint_classification(deforestation_commons, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATIONAL GOVERNMENT / ENVIRONMENTAL REGULATORS (TANGLED ROPE) — Constrained by fiscal dependency on resource extraction revenues and agricultural export income. Also tasked with environmental protection (genuine coordination function). Experiences the constraint as mixed coordination-extraction: revenue from logging/land conversion vs. cost of ecosystem service loss and international environmental commitments. Moderate extraction with agency limited by fiscal constraints and international pressure. Can modify constraints but faces significant economic penalties for enforcement.
constraint_indexing:constraint_classification(deforestation_commons, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: FINANCIAL CAPITAL / VENTURE INVESTORS (SNARE paradox) — Despite powerful position and arbitrage options, experience snare-level extraction from the constraint because of investment commitments, portfolio risk concentration, and regulatory uncertainty. Extraction mechanism: capital locked into frontier land conversion at the moment when climate and regulatory risks are rising. High effective extraction (inability to exit position profitably) despite structural power. This perspective reveals mandatrophy: a powerful agent experiencing snare-level constraint due to capital lock, not structural powerlessness.
constraint_indexing:constraint_classification(deforestation_commons, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: INTERNATIONAL ENVIRONMENTAL COALITION / REDD+ (SCAFFOLD) — Temporary support structures: carbon credit markets, payment-for-ecosystem-services programs, forest conservation incentives. Experience constraint as solvable via economic incentive restructuring. Low theater (genuine mechanisms, not performative) but also low organizational enforcement power. Sunset logic: as carbon pricing matures and enforcement capacity strengthens, the extraction mechanism loses force. Extraction is not primary — coordination is primary. Estimated sunset: 15-30 years as global carbon markets and enforcement systems mature.
constraint_indexing:constraint_classification(deforestation_commons, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: INTERNATIONAL CONSERVATION NGOs (PITON) — Institutional actors maintaining conservation narratives and programs without fundamental power to alter extraction mechanisms. High theater: impact reports, conservation certifications, sustainable forestry labels that do not prevent deforestation at scale. Persist through funding flows and institutional inertia despite low effectiveness. Theater ratio high because actual deforestation rates remain uncorrelated with certification programs. Piton classification reflects degraded conservation function maintained by institutional momentum.
constraint_indexing:constraint_classification(deforestation_commons, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN diagnosis) — Tempting but incorrect naturalization: 'Economic development requires land conversion; forests are fungible resources; tragedy of commons is inevitable.' Appears as immutable economic law. However, the constraint is NOT immutable — it reflects specific institutional arrangements (weak property rights, commodity price structures, regulatory capture, externality non-internalization). The engine will flag this as a false summit. The constraint is socially contingent, not a law of nature.
constraint_indexing:constraint_classification(deforestation_commons, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deforestation_commons_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deforestation_commons, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deforestation_commons, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deforestation_commons, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(deforestation_commons, TR),
    TR >= 0.70.

:- end_tests(deforestation_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting strong asymmetric extraction concentrated on indigenous and local communities who bear disproportionate cost of forest loss while capturing minimal benefit from commodity production. The 0.42 → 0.68 trajectory reflects acceleration of frontier expansion and commodity price appreciation over the measurement interval, making land conversion increasingly profitable at the cost of ecosystem stability. Suppression (0.72): High, reflecting multiple suppression mechanisms: state monopoly on resource concessions and enforcement, indigenous land rights unrecognized in law, economic dependency that prevents exit, and capital concentration among logging operators and agribusiness investors. The suppression is structural (property law, capital access) and geographic (trapped by water/land dependence) and cultural (identity loss). Theater ratio (0.58): Moderate-high, reflecting growth in conservation rhetoric and certification programs that have not reduced deforestation at scale. REDD+ payments remain too low relative to commodity returns. Sustainable forestry labels do not prevent conversion to pasture and cropland. International conservation NGO programs do not translate to territory protection against state-backed concessions. The theater has grown (0.35 → 0.58) as a Goodhart response to rising criticism, but actual deforestation rates remain uncorrelated with certification programs. Claimed type: Snare. The engine will verify through the beneficiary/victim structure and exit options: victims are trapped (no material exit), suppression is high (0.72), extraction is high (0.68), and the constraint offers minimal coordination benefit to the victims (it is pure extraction from their perspective). The only snare gate threshold in question is χ ≥ 0.66. With d approaching 1.0 for trapped indigenous agents, f(d) ≈ 1.42, and σ(S) at regional level (0.9), χ ≈ 0.68 × 1.42 × 0.9 ≈ 0.87. Exceeds snare threshold. Mandatrophy is resolved by recognizing that the snare classification is stable across most perspectives except for the analytical observer's false natural law attempt — that perspective will be flagged as a false summit.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates large perspectival divergence between trapped victims, beneficiary institutions, and attempts at international coordination. Indigenous communities see a pure snare: land loss, rights violation, cultural destruction with zero coordination benefit. Downstream communities see hydrological snare: forced adaptation to water loss with no exit option. Industrial operators see rope: supply chains, market coordination, profit opportunity. Commodity markets see rope: price signals, rational allocation. National governments see tangled rope: revenue dependency conflicting with environmental protection mandate. Financial capital sees snare paradox: trapped by sunk costs despite structural power. Conservation NGOs see piton: degraded programs maintained by institutional inertia. International environmental coalition sees scaffold: temporary mechanisms with sunset potential. The analytical observer risks false naturalization (mountain): 'development requires land conversion; this is inevitable.' The multiple classification types from a single set of base properties reveal that the same constraint is experienced radically differently depending on whether the observer is trapped, benefiting, or attempting coordination. No unified perspective exists — only a presheaf of incompatible experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation from beneficiary/victim status and exit options: Indigenous communities as trapped victims (d approaching 1.0) experience maximum extraction. Downstream communities as trapped victims (d ≈ 0.95) also experience near-maximum extraction. Global climate system as trapped aggregate (d = 1.0) experiences total extraction with zero agency. Industrial operators as institutional beneficiaries with arbitrage options (d ≈ 0.15) experience negative effective extraction (they are subsidized by the constraint). Commodity markets as institutional beneficiaries with arbitrage (d ≈ 0.10) experience minimal extraction. National governments with constrained exit (d ≈ 0.60) experience moderate-to-high extraction reflecting their conflict between revenue dependency and enforcement mandate. Financial capital with structural power but capital lock (d ≈ 0.75) experiences high extraction despite power — this is the paradox that reveals mandatrophy: a powerful agent trapped by commitment. Directionality overrides are not needed — the derived d values correctly reflect structural relationships. The constraint's snare classification depends fundamentally on the powerless/trapped agents bearing maximum extraction while institutional beneficiaries capture asymmetric returns.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (extractiveness > 0.70): The constraint exhibits snare classification across most victim perspectives and has legitimate extractive features — high suppression, minimal coordination benefit for victims, asymmetric distribution of costs and benefits. However, mandatrophy analysis requires verification that this is genuine snare and not false naturalization. The false natural law attempt (analytical/mountain perspective) claims land conversion is inherent to economic development. This is rejected: property rights systems, commodity pricing structures, and capital allocation mechanisms are all socially contingent, not natural laws. They could be reorganized (stronger indigenous land rights, carbon pricing internalization, redirected investment) to reduce or eliminate the extraction. The snare classification is therefore stable and not a misclassification of coordination as extraction. The piton perspective (international conservation NGOs) represents genuine institutional degradation — performative programs that do not achieve stated conservation goals — which supports the snare classification rather than undermining it (if conservation mechanisms actually worked, the snare would be partially resolved). Financial capital's paradoxical snare classification (powerful agent experiencing trap) represents genuine mandatrophy insight: a structurally powerful actor is locked into extractive positions through capital commitment and regulatory uncertainty, creating mutual extraction risk. This suggests that REDD+ mechanisms targeting financial de-risking (carbon price certainty, enforcement guarantees) could simultaneously reduce the snare and rescue stranded capital — aligning victim interests (forest conservation) with beneficiary interests (return on land investment) through mechanism redesign. The scaffold perspective supports this: international carbon markets and enforcement mechanisms create potential exit from the snare if implemented at sufficient price levels and scope. Mandatrophy is resolved by confirming snare classification, identifying specific institutional contingencies (property rights, pricing, regulation) that create it, and noting that the constraint could be reorganized through coordination mechanisms that are not currently deployed at sufficient scale or commitment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_rights_externality_architecture,
    'To what extent is deforestation a tragedy-of-commons problem (weak property rights) vs. an externality-pricing problem (carbon and ecosystem services priced at zero)?',
    'Counterfactual comparison: regions with strong indigenous property rights (e.g., Brazil''s Xingu Indigenous Land) vs. state-assigned concessions; correlation between carbon pricing schemes and deforestation rates; analysis of forest persistence under different property/pricing architectures',
    'If property-rights dominant: snare classification is correct (powerless agents trapped by lack of enforceable claims). If externality-pricing dominant: constraint degrades to tangled_rope (reorganizing price signals could solve most extraction). If both interact: constraint remains snare but with identified leverage points.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_rights_externality_architecture, empirical, 'Relative importance of property rights weakness vs. externality non-internalization').

omega_variable(
    indigenous_countervailing_power,
    'Do organized indigenous coalitions constitute a countervailing power that transforms the snare into tangled_rope, or does structural isolation prevent coalition formation?',
    'Historical analysis of indigenous organizing capacity (e.g., Brazil''s Coordination of Indigenous Organizations of the Amazon Basin); mapping of successfully defended territories vs. conquered territories; measurement of coalition size threshold needed to shift power dynamics',
    'If coalitions can form and scale: victims graduate from trapped to constrained, classification shifts toward tangled_rope with organized victim cohort. If isolation prevents scaling: snare classification confirmed, but organizing vector identified. This feeds mandatrophy: snare is stable only under isolation assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_countervailing_power, empirical, 'Whether indigenous organizing can create countervailing power').

omega_variable(
    capital_lock_financial_risk,
    'What percentage of frontier land investment is locked into sunk costs that cannot be recovered if deforestation is halted, and does this create mutual extraction between financial capital and logging operators?',
    'Analysis of land investment portfolios; tracking of stranded asset declarations; measurement of divestment costs during regulatory tightening (e.g., post-2020 Amazon monitoring); identification of whether financial capital is genuinely trapped or merely experiencing lower ROI',
    'If capital is truly locked (sunk costs > exit value): financial capital experiences snare, revealing mandatrophy. If capital can exit but chooses not to: financial capital is complicit beneficiary, not victim. Resolving this determines whether REDD+ mechanisms should target financial risk or operator incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_lock_financial_risk, empirical, 'Degree of financial capital lock-in and stranded asset risk').

omega_variable(
    carbon_pricing_equilibrium,
    'At what carbon price does forest conservation become economically competitive with land conversion for commodity production?',
    'Analysis of REDD+ payment rates vs. agricultural commodity returns; modeling of breakeven carbon price for different forest types and commodity crops; empirical measurement from carbon markets and conservation payments',
    'If breakeven price is achievable (e.g., $50-150/ton CO2): scaffold sunset is realistic, constraint can be reorganized. If breakeven price exceeds political/economic feasibility (e.g., $500+/ton): scaffold is aspirational, snare classification stands. This determines whether market-based mechanisms can resolve the extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_pricing_equilibrium, empirical, 'Carbon price required for forest conservation to be economically competitive').

omega_variable(
    supply_chain_enforcement_capacity,
    'Can commodity buyer enforcement (zero-deforestation commitments, supply chain verification) meaningfully reduce deforestation, or is enforcement cost too high relative to commodity value?',
    'Tracking of deforestation rates in regions with strong buyer commitments (e.g., soy moratorium in Mato Grosso) vs. unconstrained regions; measurement of enforcement cost as percentage of commodity value; assessment of monitoring technology effectiveness (satellite imagery, blockchain)',
    'If enforcement is cost-effective and scalable: constraint can be transformed via supply chain coordination (reduces to rope or tangled_rope with organized beneficiary pressure). If enforcement is prohibitively expensive: constraint remains snare at scale, though supply chain mechanisms can protect specific high-value products.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_enforcement_capacity, empirical, 'Feasibility and cost-effectiveness of supply chain enforcement mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deforestation_commons, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defcom_tr_t0, deforestation_commons, theater_ratio, 0, 0.35).
narrative_ontology:measurement(defcom_tr_t20, deforestation_commons, theater_ratio, 20, 0.48).
narrative_ontology:measurement(defcom_tr_t40, deforestation_commons, theater_ratio, 40, 0.58).
narrative_ontology:measurement(defcom_tr_t60, deforestation_commons, theater_ratio, 60, 0.62).

% Extraction over time
narrative_ontology:measurement(defcom_be_t0, deforestation_commons, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(defcom_be_t20, deforestation_commons, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(defcom_be_t40, deforestation_commons, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(defcom_be_t60, deforestation_commons, base_extractiveness, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deforestation_commons, resource_allocation).
narrative_ontology:affects_constraint(deforestation_commons, tropical_biodiversity_loss).
narrative_ontology:affects_constraint(deforestation_commons, indigenous_land_rights_recognition).
narrative_ontology:affects_constraint(deforestation_commons, global_carbon_emissions).
narrative_ontology:affects_constraint(deforestation_commons, agricultural_commodity_price_structure).

% DUAL FORMULATION NOTE:
% Deforestation commons decomposes into structurally distinct constraints with different ε values: (1) local_land_rights (ε=0.85, snare) — indigenous rights violation; (2) commodity_price_externality (ε=0.62, tangled_rope) — carbon/ecosystem services unpriced; (3) financial_capital_lock (ε=0.58, tangled_rope) — venture capital trapped in frontier land. This story represents the aggregate constraint. Upstream constraints include weak_property_rights_systems (ε=0.75) and commodity_market_price_discovery_failure (ε=0.60). Downstream constraints include tropical_biodiversity_collapse (ε=0.80) and indigenous_cultural_extinction (ε=0.82).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deforestation_commons, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
