% ============================================================================
% CONSTRAINT STORY: chinese_ev_manufacturing_fdi
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chinese_ev_manufacturing_fdi, []).

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
 *   constraint_id: chinese_ev_manufacturing_fdi
 *   human_readable: Chinese EV Manufacturing Foreign Direct Investment
 *   domain: economic_policy/trade/manufacturing
 *
 * SUMMARY:
 *   Chinese EV manufacturing foreign direct investment represents a
 *   structural transformation of global automotive production driven by
 *   technological advantage, labor cost differentials, and policy
 *   coordination (Belt and Road, trade arrangements, EV acceleration
 *   targets). The constraint exhibits tangled rope characteristics: genuine
 *   coordination function (accelerating global EV adoption, building
 *   manufacturing capacity, creating supply chain ecosystems) combined with
 *   asymmetric extraction (technology leverage, cost advantages, market
 *   dominance, supplier lock-in). Chinese manufacturers benefit from
 *   arbitrage across regulatory regimes, labor cost differentials, and
 *   accumulated technological lead in battery production and BEV design. Host
 *   countries experience immediate job creation and tax revenue but face
 *   long-term supply chain dependence, technology transfer on asymmetric
 *   terms, wage pressure on incumbent workers, and erosion of domestic
 *   competitive capacity. The constraint's theater ratio (0.48) reflects
 *   significant performative elements in regulatory negotiations and local
 *   content requirements that are often not enforced or are circumvented
 *   through subsidiary structures, while the genuine coordination function
 *   remains substantial. The constraint is generationally unstable—trade
 *   regulations and local capacity development are gradually embedding
 *   constraints on the extraction mechanism, but the process is slow (10-15
 *   year timescale) and unevenly distributed across host countries.
 *
 * KEY AGENTS:
 *   - Chinese EV Manufacturers: Primary beneficiary (powerful/mobile) — extract asymmetric rents through technology leverage, cost advantages, and supply chain dominance
 *   - Incumbent Auto Workers: Primary victim (powerless/trapped) — displaced from traditional ICE production; cannot exit regional labor markets; experience wage suppression
 *   - Host Country Governments: Secondary beneficiary (institutional/arbitrage) — gain job creation, tax revenue, manufacturing capacity; exercise optionality through investment terms
 *   - Domestic EV Competitors: Secondary victim (moderate/constrained) — face technology gaps and capital barriers; extracted through supply chain dependence and market share pressure
 *   - Legacy Automotive Supply Chains: Tertiary victim (institutional/arbitrage) — lose procurement relevance as Chinese manufacturers internalize supplies (piton dynamics)
 *   - International Trade Regulators: Organized actors (organized/constrained) — attempting to embed sunset constraints through local content requirements and trade rules
 *   - Technology-Dependent Host Sectors: Potential victim (moderate/constrained) — coerced technology transfer and IP asymmetries create long-term dependency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chinese_ev_manufacturing_fdi, 0.58).
domain_priors:suppression_score(chinese_ev_manufacturing_fdi, 0.65).
domain_priors:theater_ratio(chinese_ev_manufacturing_fdi, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chinese_ev_manufacturing_fdi, extractiveness, 0.58).
narrative_ontology:constraint_metric(chinese_ev_manufacturing_fdi, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(chinese_ev_manufacturing_fdi, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chinese_ev_manufacturing_fdi, tangled_rope).
narrative_ontology:human_readable(chinese_ev_manufacturing_fdi, "Chinese EV Manufacturing Foreign Direct Investment").
narrative_ontology:topic_domain(chinese_ev_manufacturing_fdi, "economic_policy/trade/manufacturing").

domain_priors:requires_active_enforcement(chinese_ev_manufacturing_fdi).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chinese_ev_manufacturing_fdi, chinese_ev_manufacturers).
narrative_ontology:constraint_beneficiary(chinese_ev_manufacturing_fdi, host_country_employment).
narrative_ontology:constraint_beneficiary(chinese_ev_manufacturing_fdi, local_supply_chains).
narrative_ontology:constraint_victim(chinese_ev_manufacturing_fdi, incumbent_automotive_sectors).
narrative_ontology:constraint_victim(chinese_ev_manufacturing_fdi, domestic_ev_competitors).
narrative_ontology:constraint_victim(chinese_ev_manufacturing_fdi, technology_transfer_constraint).
narrative_ontology:constraint_victim(chinese_ev_manufacturing_fdi, labor_standards_pressure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INCUMBENT AUTO WORKERS (SNARE) — Trapped by geographic and skill dependency. EV manufacturing FDI displaces skilled labor from traditional ICE automotive production. Workers cannot exit regional labor markets without economic devastation; retraining costs are high; wage suppression follows from oversupply of specialized labor. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(chinese_ev_manufacturing_fdi, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DOMESTIC EV COMPETITORS (TANGLED ROPE) — Constrained by capital requirements and technology gaps. Chinese FDI creates coordination benefits (supply chain ecosystem, consumer market expansion, manufacturing infrastructure development) but also extraction through IP capture, supply chain dominance, and market share concentration. Exit costs are high but not insurmountable—competitors can relocate operations or seek alternate supply chains, but at significant expense.
constraint_indexing:constraint_classification(chinese_ev_manufacturing_fdi, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HOST COUNTRY GOVERNMENT (ROPE) — Experiences the constraint as pure coordination. Chinese FDI solves immediate policy problems: job creation, tax revenue, technology access, transition to EV manufacturing infrastructure. Governments can exercise optionality (favor or disfavor specific investors, adjust subsidy structures, negotiate terms) and have exit paths (attracting competing FDI, diversifying investment sources). Net beneficiary from the constraint viewed at immediate time horizon.
constraint_indexing:constraint_classification(chinese_ev_manufacturing_fdi, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL TRADE REGULATORY COALITION (SCAFFOLD) — Organized actors (USMCA, EU trade bodies, RCEP signatories) view Chinese EV FDI as a temporary regulatory arbitrage problem with a sunset. Local content requirements, origin rules, and labor standards are being embedded into trade agreements to phase out the extraction mechanism. The constraint has an explicit time horizon: as regulations tighten and local capacities mature, the asymmetric advantage of Chinese manufacturers declines. Theater ratio reflects the performative framing of these negotiations—regulatory posturing precedes actual enforcement.
constraint_indexing:constraint_classification(chinese_ev_manufacturing_fdi, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY AUTOMOTIVE SUPPLY CHAINS (PITON) — Traditional automotive supplier networks (European, Japanese, Korean Tier-1 suppliers) maintain their institutional roles and procurement relationships despite eroding functional relevance. Chinese manufacturers are increasingly internalizing supply chains, reducing dependency on legacy suppliers. The legacy supplier ecosystem persists through contractual inertia and switching costs rather than genuine coordination function. Theater ratio reflects the performative maintenance of supply relationships as actual integration declines.
constraint_indexing:constraint_classification(chinese_ev_manufacturing_fdi, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CHINESE EV MANUFACTURERS (TANGLED ROPE) — Powerful institutional actors with high mobile exit options (can shift investment to other markets, source from alternative suppliers). FDI abroad is a coordination mechanism: building global manufacturing footprint, accessing markets, diversifying supply chains. Simultaneously, it enables extraction: IP leverage over host country sectors, preferential access to supply chains, technology-for-access terms that benefit Chinese firms asymmetrically. The constraint is genuinely mixed—these firms both coordinate global EV ecosystem and extract asymmetric rents.
constraint_indexing:constraint_classification(chinese_ev_manufacturing_fdi, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational and global perspective, Chinese EV manufacturing FDI represents a structural transformation of global automotive production with both genuine coordination function (accelerating EV adoption, building supply chains, creating manufacturing capacity) and persistent asymmetric extraction (IP capture, labor cost arbitrage, market dominance, technology-for-access dependencies). The constraint persists because the coordination benefits are real and necessary, but the extraction mechanism is embedded in the asymmetric power relationships between Chinese manufacturers and host countries.
constraint_indexing:constraint_classification(chinese_ev_manufacturing_fdi, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chinese_ev_manufacturing_fdi_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(chinese_ev_manufacturing_fdi, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chinese_ev_manufacturing_fdi, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(chinese_ev_manufacturing_fdi, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(chinese_ev_manufacturing_fdi, TR),
    TR >= 0.70.

:- end_tests(chinese_ev_manufacturing_fdi_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated but not extreme. Chinese FDI generates real coordination benefits (EV acceleration, supply chain infrastructure) that partially offset extraction. The extractiveness is driven by technology leverage, labor cost arbitrage, and supply chain dominance. The measure reflects that extraction is substantial but not pure—the coordination function is genuine and valued by host countries. Measurement trajectory from 0.35 to 0.58 shows accumulation of extraction as Chinese manufacturers build dominance and lock in supply dependencies. Suppression (0.65): High. Significant barriers to host country exit include capital requirements for alternative manufacturing, technology gaps, switching costs for established supply chains, labor market immobility for displaced workers, and regulatory capture by large investors. However, suppression is not absolute—some host countries (EU, US) are developing protective regulations and alternative sourcing strategies. Theater ratio (0.48): Moderate. Local content requirements, regulatory negotiations, and origin rules are partly performative (enforcement is selective, subsidiary structures circumvent requirements) but also partly functional (they do increase costs for manufacturers). The theater ratio does not trend high because the coordination function is substantive enough to maintain genuine operational requirements rather than pure ritual.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (Chinese EV manufacturing investment) produces radically different experienced types based on the observer's position. Incumbent auto workers see a snare because they are trapped: they cannot exit the regional labor market, cannot retrain at reasonable cost, and experience pure wage suppression with no coordination benefit to them. Host governments see rope because they exercise agency (attract competing investors, negotiate terms, adjust subsidies) and experience genuine coordination benefits (jobs, taxes, technological acceleration). Domestic competitors see tangled rope because they experience both—the supply ecosystem benefits them (coordination) while the technology gap and cost advantages of Chinese manufacturers extract from them (extraction). Organized trade regulators see scaffold because they are building exit paths through regulatory constraints and local capacity development with explicit time horizons (10-15 years). Legacy suppliers see piton—their institutional role persists through inertia even as their functional relevance erodes. The analytical observer at civilizational scope sees tangled rope because both mechanisms are structurally embedded: coordination is necessary for global EV transition; extraction is embedded in asymmetric power relationships. The constraint cannot be resolved by collapsing to a single type—the presheaf of perspectives IS the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from base extractiveness (ε=0.58), the agent's directionality value (d), and scope modifier σ(S). Incumbent workers have d ≈ 0.95 (trapped victim) at regional scope (σ=0.9), yielding χ ≈ 0.58 × 1.35 × 0.9 ≈ 0.70—they experience this as a severe snare. Host governments have d ≈ 0.15 (institutional beneficiary) at national scope (σ=1.0), yielding χ ≈ 0.58 × (-0.01) × 1.0 ≈ negative (they benefit). Chinese manufacturers have d ≈ 0.10 (powerful beneficiary) at global scope (σ=1.2), yielding χ ≈ 0.58 × (-0.08) × 1.2 ≈ negative (they capture value). Domestic competitors have d ≈ 0.65 (moderate victim-beneficiary mix) at national scope, yielding χ ≈ 0.58 × 1.00 × 1.0 ≈ 0.58 (they experience mixed extraction and benefit). The piton classification for legacy suppliers derives from theater ratio (0.48 is approaching the piton gate of 0.70) rather than from high χ—their institutional role persists performatively despite functional erosion.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope is not a fudge category for 'can't decide between rope and snare.' It is the legitimate classification when a constraint genuinely contains both coordination and extraction mechanisms at structurally distinct levels. Chinese EV manufacturing FDI coordinates the global automotive transition toward EVs (genuine coordination function requiring these investment flows) while simultaneously extracting rents through technology asymmetries, cost advantages, and supply chain dominance (genuine extraction function). Neither rope nor snare captures this: rope would miss the extraction; snare would miss the coordination that benefits host countries. The mandatrophy is resolved by showing that the perspectival variation is not noise—it reflects real structural differences in how different agents experience the constraint. The snare classification for incumbent workers is not a 'perspective error'; it is their actual structural reality. The rope classification for host governments is not a misreading; it is their actual structural reality. The constraint IS tangled rope from the analytical perspective because both mechanisms are necessary to understand the system. Local content regulations and trade negotiations are attempting to shift the constraint toward scaffold by embedding sunset logic, but this is a slow institutional process (10-15 years) that has not yet succeeded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_transfer_voluntariness,
    'Is technology transfer from Chinese EV manufacturers to host country sectors voluntary market exchange or coercive extraction?',
    'Analysis of IP licensing agreements, patent ownership patterns, subsidiary independence levels; comparison with non-Chinese FDI technology transfer rates; interview-based assessment of host country firm autonomy in tech-sharing decisions',
    'If voluntary: constraint reclassifies toward Rope from host country perspective. If coercive: suppression and extraction metrics increase; snare reclassification for technology-dependent sectors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_voluntariness, empirical, 'Whether technology transfer is voluntary market exchange or coerced extraction').

omega_variable(
    supply_chain_lock_in_duration,
    'What is the time horizon for supply chain lock-in to Chinese manufacturers? Can host countries build alternative sources, and at what cost?',
    'Historical analysis of supply chain transitions in automotive; cost-benefit modeling for alternative sourcing; capacity development timelines for competing suppliers; scenario analysis for retaliatory trade measures',
    'If short-term (3-5 years): suppliers can transition, scaffold perspective strengthened. If long-term (10+ years): lock-in is substantial, snare perspective for dependent suppliers strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_lock_in_duration, empirical, 'Duration and reversibility of supply chain dependencies on Chinese manufacturers').

omega_variable(
    labor_standards_extraction_mechanism,
    'Are labor suppression and wage pressure in host countries a direct mechanism of Chinese FDI (companies deliberately undercut local standards) or a market effect of increased manufacturing capacity?',
    'Comparative analysis of wage trends in host countries before/after Chinese FDI; labor inspection data; union negotiation records; interviews with host country manufacturers about wage-setting decisions',
    'If direct: suppression metrics increase, snare classification for workers strengthened, extraction mechanism explicit. If market effect: suppression is correlated but not directly caused, tangled rope interpretation remains stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_standards_extraction_mechanism, empirical, 'Whether labor suppression is deliberate mechanism or market side-effect of FDI').

omega_variable(
    local_content_regulation_effectiveness,
    'Do local content requirements and origin rules actually constrain Chinese manufacturer extraction, or are they performative regulatory theater?',
    'Enforcement data on local content violations; actual cost impact on Chinese manufacturers from compliance; substitution patterns (Chinese subsidiaries vs parent companies); comparison with regulatory intent vs actual outcomes',
    'If effective: scaffold sunset is real, theater ratio decreases. If performative: theater ratio increases, regulations are institutional inertia rather than functional constraint on extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_content_regulation_effectiveness, empirical, 'Whether local content regulations functionally constrain extraction or remain performative').

omega_variable(
    domestic_competitor_viability,
    'Can host country EV manufacturers achieve cost parity with Chinese manufacturers without protective trade barriers or subsidies?',
    'Comparative cost analysis; technology development timelines; capital requirements; economies of scale achievable at different production levels; scenario analysis for different subsidy/tariff regimes',
    'If yes: competitive threat forces Chinese manufacturers to improve terms, tangled rope equilibrium stabilizes. If no: domestic competitors become dependent on protection, shifting from tangled rope to snare for protected sectors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_competitor_viability, empirical, 'Whether domestic EV competitors can achieve cost parity without protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chinese_ev_manufacturing_fdi, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cn_ev_tr_t0, chinese_ev_manufacturing_fdi, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cn_ev_tr_t3, chinese_ev_manufacturing_fdi, theater_ratio, 3, 0.45).
narrative_ontology:measurement(cn_ev_tr_t6, chinese_ev_manufacturing_fdi, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(cn_ev_be_t0, chinese_ev_manufacturing_fdi, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cn_ev_be_t3, chinese_ev_manufacturing_fdi, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(cn_ev_be_t6, chinese_ev_manufacturing_fdi, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chinese_ev_manufacturing_fdi, resource_allocation).
narrative_ontology:affects_constraint(chinese_ev_manufacturing_fdi, automotive_labor_displacement).
narrative_ontology:affects_constraint(chinese_ev_manufacturing_fdi, battery_supply_chain_concentration).
narrative_ontology:affects_constraint(chinese_ev_manufacturing_fdi, technology_transfer_asymmetry).
narrative_ontology:affects_constraint(chinese_ev_manufacturing_fdi, trade_regulatory_fragmentation).

% DUAL FORMULATION NOTE:
% Chinese EV manufacturing FDI is upstream of multiple domain-specific constraints: labor displacement in incumbent sectors, supply chain concentration in battery production, technology transfer asymmetries in host country firms, and regulatory fragmentation in trade agreements. Each downstream constraint has its own extractiveness value reflecting domain-specific structures; the FDI constraint operates at the institutional/geopolitical level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(chinese_ev_manufacturing_fdi, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
