% ============================================================================
% CONSTRAINT STORY: cobalt_congo_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cobalt_congo_dependency, []).

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
 *   constraint_id: cobalt_congo_dependency
 *   human_readable: Global Cobalt Supply Dependency on Democratic Republic of Congo
 *   domain: economic/supply_chain/geopolitical
 *
 * SUMMARY:
 *   The global dependency on cobalt from the Democratic Republic of Congo for
 *   battery production and electronics manufacturing creates a multilayered
 *   extraction constraint operating across geopolitical, economic, and labor
 *   domains. Congo holds approximately 70% of the world's economically
 *   recoverable cobalt reserves and supplies 60%+ of global production. This
 *   geographic concentration, combined with weak state capacity, governance
 *   challenges, and the structural volatility of commodity markets, produces
 *   extractive dynamics that harm Congolese miners and the Congolese state
 *   while benefiting global battery manufacturers and consumer economies. The
 *   constraint exhibits properties of a pure snare from the perspective of
 *   artisanal miners (trapped in hazardous labor with no exit), a resource
 *   curse snare from the state perspective (structural lock-in via debt and
 *   development model), and a solved coordination problem (rope) from the
 *   perspective of battery manufacturers who have secured stable, low-cost
 *   supply. The theater ratio (0.55) reflects the proliferation of ESG,
 *   conflict minerals, and responsible sourcing frameworks that create
 *   substantial documentation and certification activity without
 *   proportionally reducing labor extraction or improving state revenue
 *   capture. The extractiveness trajectory (0.52→0.68 over 15 years) reflects
 *   intensifying demand for cobalt, increasing pressure on supply, and
 *   growing awareness of extraction without corresponding improvements in
 *   miner conditions or Congolese rents. The constraint demonstrates how a
 *   genuine technical problem (securing necessary material for energy
 *   transition) becomes weaponized into an extractive asymmetry.
 *
 * KEY AGENTS:
 *   - Artisanal Cobalt Miners: Primary victims (powerless/trapped) — face wage suppression, hazardous conditions, zero negotiating power; trapped by geography and economic dependency
 *   - Democratic Republic of Congo (State): Primary victim (powerless/trapped) — structurally locked in resource curse; minimal revenue capture; alternative development pathways blocked
 *   - Artisanal and Small-Scale Mining Communities: Extended victims (powerless/identity_locked) — occupational identity fused with mining; cultural practices disrupted by extraction pressure; communities trapped through identity as much as economics
 *   - Mining Companies (Glencore, Eurasian Resources, etc.): Secondary beneficiaries (moderate/constrained) — benefit from supply access and labor arbitrage; constrained by regulatory risk and reputational exposure; also trapped in supply chain lock-in
 *   - Battery Manufacturers and Tech Companies: Primary beneficiaries (institutional/arbitrage) — secure stable, low-cost cobalt supply; high arbitrage capacity; experience constraint as solved coordination problem
 *   - Developed Economies and Consumers: Indirect beneficiaries (institutional/arbitrage) — receive energy transition benefits; insulated from supply chain extraction through price mechanisms
 *   - International Monitoring/Certification Bodies: Institutional actors (institutional/arbitrage) — maintain ESG and conflict minerals frameworks; benefit from certification fees and compliance activity; produce theater without proportional extraction reduction
 *   - Analytical Observer: Global system view (analytical/analytical) — sees genuine coordination need (energy transition requires cobalt) layered with asymmetric extraction and geographic lock-in
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cobalt_congo_dependency, 0.68).
domain_priors:suppression_score(cobalt_congo_dependency, 0.72).
domain_priors:theater_ratio(cobalt_congo_dependency, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cobalt_congo_dependency, extractiveness, 0.68).
narrative_ontology:constraint_metric(cobalt_congo_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cobalt_congo_dependency, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cobalt_congo_dependency, snare).
narrative_ontology:human_readable(cobalt_congo_dependency, "Global Cobalt Supply Dependency on Democratic Republic of Congo").
narrative_ontology:topic_domain(cobalt_congo_dependency, "economic/supply_chain/geopolitical").

domain_priors:requires_active_enforcement(cobalt_congo_dependency).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cobalt_congo_dependency, battery_manufacturers).
narrative_ontology:constraint_beneficiary(cobalt_congo_dependency, technology_companies).
narrative_ontology:constraint_beneficiary(cobalt_congo_dependency, developed_economies).
narrative_ontology:constraint_victim(cobalt_congo_dependency, congolese_miners).
narrative_ontology:constraint_victim(cobalt_congo_dependency, congolese_state).
narrative_ontology:constraint_victim(cobalt_congo_dependency, global_supply_chain_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONGOLESE ARTISANAL MINERS (SNARE) — Trapped in extractive labor with minimal alternatives. Face hazardous working conditions, wage suppression, and zero negotiating power. Geographic immobility, economic dependency, and absence of formal employment protections create maximum experienced extraction. No viable exit short of abandoning livelihoods entirely.
constraint_indexing:constraint_classification(cobalt_congo_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGOLESE STATE (SNARE) — Structurally trapped in resource curse dynamics. Cobalt extraction generates minimal domestic value capture; revenues flow to foreign corporations and corrupt officials. State lacks capacity to regulate mining or capture rents; alternative development pathways blocked by debt servicing and aid conditionality. Extraction persists across generational timeframe — institutional trajectory locked in.
constraint_indexing:constraint_classification(cobalt_congo_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MINING COMPANIES (TANGLED ROPE) — Constrained by regulatory uncertainty and reputational risk, but also benefit from access to world's highest-grade cobalt reserves. Experience the constraint as coordination problem (securing supply through partnership) combined with extraction opportunity (capturing rents from labor and regulatory arbitrage). High constraints to exit due to sunk capital; also genuine efficiency benefits from consolidated supply.
constraint_indexing:constraint_classification(cobalt_congo_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BATTERY MANUFACTURERS AND TECH COMPANIES (ROPE) — Net beneficiaries with significant arbitrage options. Secure stable, low-cost cobalt supply essential to product competitiveness. Experience the constraint as solved coordination problem: Congo provides concentrated supply, predictable pricing, minimal labor costs. Low effective extraction because they control value chain and have alternative sourcing (though at higher cost). Can arbitrage to other suppliers under pressure.
constraint_indexing:constraint_classification(cobalt_congo_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MONITORING AND CERTIFICATION REGIMES (PITON) — Conflict minerals declarations, responsible sourcing protocols, and ESG reporting create substantial theater without reducing extraction. Compliance costs are passed to miners; audit trails obscure rather than prevent supply chain opacity. Regimes persist through institutional inertia and corporate risk management rather than functional verification. Theater ratio reflects gap between audit documentation and actual labor/environmental conditions in mines.
constraint_indexing:constraint_classification(cobalt_congo_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL BATTERY SUPPLY CHAIN (SNARE) — Organized but structurally constrained. The industry's decarbonization and electrification commitments create irreversible demand lock-in for cobalt. Cannot exit dependency without fundamental technology shifts (solid-state batteries, cobalt-free chemistries) requiring 10+ year development cycles. Trapped in current constraint despite high institutional power — the extraction mechanism is structural scarcity and geopolitical concentration, not coercion from weaker actors.
constraint_indexing:constraint_classification(cobalt_congo_dependency, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint exhibits genuine coordination function (cobalt supply enables global energy transition) layered with severe asymmetric extraction (benefits accrue to consuming economies; costs borne by Congo). The constraint is simultaneously solving a real technical problem (securing necessary material for batteries) and perpetuating resource curse dynamics. Classification as Tangled Rope reflects both genuine coordination need and undeniable extraction asymmetry.
constraint_indexing:constraint_classification(cobalt_congo_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cobalt_congo_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cobalt_congo_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cobalt_congo_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cobalt_congo_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cobalt_congo_dependency, TR),
    TR >= 0.70.

:- end_tests(cobalt_congo_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts substantial value from Congolese miners through wage suppression, from the Congolese state through minimal revenue capture and debt servicing, and from the global supply chain through concentrated geopolitical risk. The value of 0.68 reflects intense extraction not only from artisanal miners but from entire institutional layers (state capacity, regulatory infrastructure). The trajectory from 0.52→0.68 shows increasing extraction pressure as global cobalt demand accelerates (EV adoption, battery storage) without corresponding improvements in producer bargaining power. Suppression (0.72): Very high. Artisanal miners face multiple suppression mechanisms: geographic immobility (location of deposits), absence of alternative livelihoods, weak labor protections, informal sector status, informational asymmetry about global pricing, and state governance failure. The Congolese state faces suppression via debt constraints, aid conditionality, capital flight, and limited institutional capacity to enforce regulations or negotiate rents. The supply chain itself faces technological suppression (no near-term cobalt-free battery alternatives; substitution requires 10+ year development). Theater ratio (0.55): Moderate-high. Conflict minerals declarations, responsible cobalt sourcing initiatives, ESG reporting, and third-party audits create substantial documentation and compliance activity. However, these frameworks have not proportionally reduced labor extraction or improved state revenue capture. The theater persists because certification costs are passed through to miners, audit trails obscure supply chain opacity rather than eliminate it, and enforcement mechanisms lack teeth. Theater ratio increase from 0.38→0.55 reflects proliferation of compliance frameworks without corresponding functional improvements — classic Piton signature.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between artisanal miners and battery manufacturers reveals the full extraction asymmetry. Miners see a Snare: they bear suppressed wages, hazardous conditions, zero negotiating power, no alternatives. Maximum experienced extraction. Manufacturers see a Rope: they coordinate cobalt supply, receive stable pricing, have substitution options (albeit expensive), experience low effective extraction. The gap is not in the structural data (same extractiveness, suppression, theater values) but in each agent's position within the extraction flow: miners are at the high-extraction end (high d), manufacturers are at the low-extraction end (low d). The certification regimes (monitoring, ESG, conflict minerals) create a perspectival illusion: they generate activity and documentation that appear to address extraction, but they do not proportionally reduce extraction experienced by miners or increase revenue capture by Congo. This is the Piton signature — institutional theater without functional extraction reduction. The analytical observer at civilizational scope sees Tangled Rope: the constraint genuinely solves a coordination problem (securing cobalt for energy transition) AND exhibits severe asymmetric extraction (benefits concentrated, costs dispersed). Both properties are true; the classification reflects both.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by their structural position in the extraction flow. Artisanal miners (powerless/trapped) experience maximum extraction: they are victims with zero exit options and zero institutional power — d ≈ 0.95, f(d) ≈ 1.42, experienced chi is high. The Congolese state (powerless/trapped at generational scale) also experiences high extraction: state capacity is limited, alternatives to resource extraction are blocked by international financial constraints and debt servicing, d ≈ 0.90. Mining companies (moderate/constrained) experience moderate extraction: they face regulatory and reputational costs, but also benefit from supply access and wage arbitrage; d ≈ 0.55, creating moderate experienced extraction. Battery manufacturers (institutional/arbitrage) experience low or negative extraction: they are beneficiaries with significant arbitrage capacity (can source elsewhere at higher cost), d ≈ 0.15, creating low effective extraction from their perspective. The scope modifier σ(S) also affects chi: for artisanal miners operating at local/regional scope (σ ≈ 0.8), effective extraction is somewhat dampened by scope (but still high due to high d). For the global battery industry (σ ≈ 1.2), effective extraction is amplified by global scope (but low d makes absolute chi moderate). The key insight: measured from the miner's position, chi is maximized (high ε × high f(d) × local σ). Measured from the manufacturer's position, chi is minimized (same ε × low f(d) × global σ). This perspectival gap is diagnostic of the Snare type.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's high extractiveness (0.68) and high suppression (0.72) trigger the mandatrophy requirement (resolving potential mislabeling of extraction as coordination). The analysis confirms Snare classification is correct for trapped victims (miners, state at biographical/generational scales), not a misclassified Tangled Rope. The constraint exhibits genuine coordination function (solving supply security) but this coordination function benefits only the beneficiary tier (manufacturers, developed economies) not the victim tier (miners, Congo). The victims experience zero coordination benefit — they receive suppressed wages and institutional degradation regardless of whether cobalt supply is 'coordinated' or chaotic. The mandatrophy is resolved by recognizing that Tangled Rope requires BOTH genuine coordination AND victim participation in coordination benefits. Here, coordination exists for manufacturers; extraction is pure for miners. Classification choice: treat constraint as Snare from powerless/victim perspectives (appropriate) and analyze the rope-seeming aspects (supply coordination, certification theater) as mechanisms that perpetuate and mask the snare rather than contradicting it. The 'coordination' machinery (mining companies, trading networks, supply contracts) is what extracts; it is not an independent benefit. The Tangled Rope classification appears only at the analytical level where both extraction and coordination are visible together — reflecting the constraint's true duality: functional supply system + extractive labor system, operating as a single mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artisanal_vs_industrial_mechanism,
    'Is extraction primarily driven by artisanal sector informality and governance failure, or is it inherent to the industrial supply chain structure?',
    'Comparative analysis of labor conditions and value capture in Congo''s industrial mines (Glencore, Eurasian Resources) vs artisanal sector; tracking of pricing power and profit margins across supply chain tiers',
    'If artisanal-driven: constraint is fixable through formalization and regulation (governance problem). If industrial-driven: constraint reflects structural scarcity rent extraction regardless of formalization (snare persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artisanal_vs_industrial_mechanism, empirical, 'Whether extraction is driven by informal/governance gaps or industrial structure').

omega_variable(
    supply_substitution_feasibility,
    'Can battery technologies transition to cobalt-free or reduced-cobalt chemistries within 10 years, creating exit path for supply dependency?',
    'Technology maturity assessment of sodium-ion, LFP, solid-state batteries; cost curves and production readiness timelines; market adoption constraints independent of cobalt availability',
    'If feasible by 2035: constraint has natural sunset (Scaffold from developed economy perspective). If not feasible: cobalt dependency persists as structural (Snare extends beyond 20-year horizon).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_substitution_feasibility, empirical, 'Feasibility of cobalt-free battery technology substitution').

omega_variable(
    congolese_rents_capture_possibility,
    'Could Congo''s institutional capacity and governance improve enough to capture resource rents domestically, converting Snare to Tangled Rope?',
    'Analysis of state capacity trajectory, tax revenue optimization, sovereign wealth fund models, and historical precedent for resource curse reversal in comparable economies',
    'If possible: Congo moves from Snare (powerless/trapped) toward Tangled Rope (organized/constrained) through institutional development (15+ year horizon). If not: Snare classification is stable across generational timescale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(congolese_rents_capture_possibility, empirical, 'Feasibility of Congolese institutional capacity improvement and rent capture').

omega_variable(
    geographic_concentration_irreducibility,
    'Is DRC''s 70% cobalt reserve concentration a fundamental geological fact, or does it reflect historical extraction patterns and unexplored alternatives?',
    'Geological surveys of cobalt reserves in alternative jurisdictions; cost analysis of extraction from lower-grade deposits; mining technology constraints on economic recovery rates',
    'If fundamental: geographic concentration is a Natural Law (Mountain from analytical scope). If historical artifact: concentration is contingent institutional arrangement (Tangled Rope/Snare from analytical scope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_concentration_irreducibility, empirical, 'Whether cobalt geographic concentration is geological or institutional').

omega_variable(
    certification_regime_effectiveness,
    'Do conflict minerals and ESG certification regimes actually improve labor conditions and state revenue, or are they pure theater masking unchanged extraction?',
    'Longitudinal comparison of miner incomes, workplace safety metrics, and state revenue capture before and after certification implementation; tracking audit compliance vs observable conditions',
    'If effective: theater ratio should decline over time and certification justifies Piton classification. If theater: theater ratio stays high, confirming Piton as degraded institution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(certification_regime_effectiveness, empirical, 'Whether certification regimes reduce extraction or are performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cobalt_congo_dependency, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cobalt_tr_t0, cobalt_congo_dependency, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cobalt_tr_t5, cobalt_congo_dependency, theater_ratio, 5, 0.47).
narrative_ontology:measurement(cobalt_tr_t10, cobalt_congo_dependency, theater_ratio, 10, 0.55).
narrative_ontology:measurement(cobalt_tr_t15, cobalt_congo_dependency, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(cobalt_be_t0, cobalt_congo_dependency, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cobalt_be_t5, cobalt_congo_dependency, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(cobalt_be_t10, cobalt_congo_dependency, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(cobalt_be_t15, cobalt_congo_dependency, base_extractiveness, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cobalt_congo_dependency, resource_allocation).
narrative_ontology:boltzmann_floor_override(cobalt_congo_dependency, 0.18).
narrative_ontology:affects_constraint(cobalt_congo_dependency, lithium_supply_concentration).
narrative_ontology:affects_constraint(cobalt_congo_dependency, rare_earth_geopolitical_risk).
narrative_ontology:affects_constraint(cobalt_congo_dependency, mining_labor_exploitation_systems).

% DUAL FORMULATION NOTE:
% Cobalt supply dependency can be decomposed into three structurally distinct constraints: (1) Geographic supply concentration (cobalt_reserve_geography, ε=0.05, Mountain — geological fact), (2) Industrial supply chain extraction (cobalt_congo_dependency, ε=0.68, Snare — labor extraction), and (3) State resource curse (congo_resource_curse, ε=0.72, Snare — institutional lock-in). This story captures the industrial supply chain constraint. Upstream constraint (cobalt_reserve_geography) affects this; downstream constraints (lithium_supply_concentration, rare_earth_geopolitical_risk) follow similar patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cobalt_congo_dependency, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
