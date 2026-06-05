% ============================================================================
% CONSTRAINT STORY: port_strike_cascades
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_port_strike_cascades, []).

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
 *   constraint_id: port_strike_cascades
 *   human_readable: Port Strike Cascades and Global Supply Chain Extraction
 *   domain: economic/labor/logistics
 *
 * SUMMARY:
 *   Port strike cascades represent a structural extraction mechanism embedded
 *   in global supply chain dependencies. When port workers strike, extraction
 *   flows backward through the entire supply chain — from capital operators,
 *   through upstream suppliers, to end consumers — while workers themselves
 *   face dual pressure: union solidarity enforces strike participation, but
 *   the strike itself redistributes costs unevenly. Capital operators
 *   experience strikes as temporary coordination problems with arbitrage
 *   options (port diversification, automation, rerouting); workers experience
 *   them as trapped participation in a collective action enforced by
 *   institutional coercion; consumers experience them as passive cost
 *   absorption through inflation and scarcity. The constraint exhibits
 *   genuine coordination function (port strikes are the primary mechanism by
 *   which workers can negotiate collective wages against capital
 *   concentration), but this coordination is inseparable from asymmetric
 *   extraction — the supply chain bears costs that neither workers nor
 *   capital bear in full proportion. Theater ratio (0.48) reflects that
 *   modern port strikes increasingly rely on threat and institutional ritual
 *   rather than direct economic disruption, as automation and port
 *   diversification reduce structural leverage.
 *
 * KEY AGENTS:
 *   - Port Workers (Union): Primary beneficiary from wage perspective (trapped/organized) — captures rents during strike window; also primary loser from strike costs and automation threat
 *   - Port Operators & Shipping Lines: Primary institutional beneficiary (institutional/arbitrage) — rents captured through margins during disruption; can arbitrage to other ports or automation
 *   - Global Consumers: Primary victim (powerless/constrained) — absorb supply shock costs through price inflation, scarcity, delayed goods
 *   - Upstream Suppliers & Manufacturers: Secondary victim (moderate/constrained) — face inventory costs, production disruption, penalty clauses; some coordinate through supply agreements
 *   - Capital Holding Firms: Institutional beneficiary (institutional/arbitrage) — profit from supply chain disruption and reinvest in automation to reduce future labor leverage
 *   - Regulatory & Port Authority: Weak institutional actors (institutional/constrained) — can mediate but lack power to impose settlement; trapped between public interest and stakeholder pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(port_strike_cascades, 0.58).
domain_priors:suppression_score(port_strike_cascades, 0.62).
domain_priors:theater_ratio(port_strike_cascades, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(port_strike_cascades, extractiveness, 0.58).
narrative_ontology:constraint_metric(port_strike_cascades, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(port_strike_cascades, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(port_strike_cascades, tangled_rope).
narrative_ontology:human_readable(port_strike_cascades, "Port Strike Cascades and Global Supply Chain Extraction").
narrative_ontology:topic_domain(port_strike_cascades, "economic/labor/logistics").

domain_priors:requires_active_enforcement(port_strike_cascades).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(port_strike_cascades, port_operators).
narrative_ontology:constraint_beneficiary(port_strike_cascades, shipping_lines).
narrative_ontology:constraint_beneficiary(port_strike_cascades, capital_holding_firms).
narrative_ontology:constraint_victim(port_strike_cascades, port_workers).
narrative_ontology:constraint_victim(port_strike_cascades, global_consumers).
narrative_ontology:constraint_victim(port_strike_cascades, upstream_suppliers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PORT WORKER (SNARE) — Trapped by geographic location, skill specificity, union structure, and lack of alternative employment. Cannot exit strike participation without union penalty; cannot exit port labor without severe economic loss. Strike is weaponized cooperation enforced by collective coercion. Maximum experienced extraction.
constraint_indexing:constraint_classification(port_strike_cascades, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GLOBAL CONSUMER (SNARE) — Constrained by dependence on port-reliant goods (electronics, clothing, food, fuel). Cannot exit the supply chain; can only absorb costs through price inflation, scarcity, or delays. Strike extraction flows backward through the supply chain; consumers bear the cost without having participated in wage negotiation.
constraint_indexing:constraint_classification(port_strike_cascades, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PORT OPERATOR & SHIPPING LINES (ROPE) — Arbitrage options available (reroute through other ports, speed up turnaround, reduce wages through automation). Experience the strike as temporary coordination problem. Rents captured during disruption and reinvested in port infrastructure/automation reduce long-term wage pressures. Net beneficiary.
constraint_indexing:constraint_classification(port_strike_cascades, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UPSTREAM SUPPLIER (TANGLED ROPE) — Constrained by inventory costs, production scheduling, and buyer concentration. Port strike creates genuine coordination problem (when will goods ship?) but also extracts from suppliers through forced inventory carrying costs and penalty clauses. Both coordination function and asymmetric extraction present.
constraint_indexing:constraint_classification(port_strike_cascades, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PORT WORKERS AS ORGANIZED COALITION (TANGLED ROPE) — At generational horizon and organized power level, port workers see both coordination function (collective wage negotiation prevents race-to-bottom) and extraction mechanism (strike imposes costs on non-participating workers, consumers, suppliers). The constraint requires active enforcement (union discipline) to maintain solidarity and prevent free-riding. Extraction runs FROM the broader supply chain INTO union wage floors.
constraint_indexing:constraint_classification(port_strike_cascades, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PORT STRIKE INSTITUTIONAL THEATER (PITON) — Strike ritual persists through inertia despite containerization, automation, and port diversification reducing strike leverage. Modern port strikes are largely performative — the disruption threat carries weight through institutional habit rather than structural necessity. Theater ratio (0.48) reflects that disruption is still materially costly but increasingly optional. Automation trajectories are making the strike mechanism less functionally relevant while maintaining ritual enforcement.
constraint_indexing:constraint_classification(port_strike_cascades, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, port concentration and supply chain interdependence are treated as immutable economic laws — 'just-in-time logistics requires continuous flow' and 'globalization is inevitable.' This naturalizes the constraint rather than recognizing it as a contingent institutional arrangement (port monopolies, union structures, global trade agreements). The engine flags this as a false summit.
constraint_indexing:constraint_classification(port_strike_cascades, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(port_strike_cascades_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(port_strike_cascades, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(port_strike_cascades, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(port_strike_cascades, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(port_strike_cascades, TR),
    TR >= 0.70.

:- end_tests(port_strike_cascades_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Port strikes create genuine economic extraction through supply chain cascades. Workers extract wage concessions from capital, but capital extracts losses from consumers and suppliers, creating a cascade where the largest costs are borne by powerless agents (consumers) not party to the wage negotiation. The value increased from 0.45 to 0.58 over the interval as automation and port concentration increased worker leverage asymmetry. Suppression (0.62): Moderate-high. Workers are suppressed by union structure (mandatory participation), geographic immobility, and skill specificity. Consumers are suppressed by supply chain dependence and lack of alternatives. Capital is NOT suppressed — has arbitrage options. Asymmetric suppression is the key feature. Theater ratio (0.48): Moderate. Port strikes retain material disruptive capacity (blockade real goods) but increasingly rely on threat and ritual as automation spreads. The increase from 0.38 to 0.48 reflects growing gap between strike narrative (existential threat to supply chains) and structural reality (port diversification, automation, and rerouting options make strikes less universally disruptive).
 *
 * PERSPECTIVAL GAP:
 *   Port workers at biographical/trapped level see a snare (coercive participation). Port workers at generational/organized level see a rope/tangled_rope (genuine coordination mechanism for wages). Capital sees rope (arbitrage options). Consumers see snare (extraction without participation). The gap reveals that the constraint's function depends critically on time horizon: at biographical scale, strikes are experienced as coercive extraction; at generational scale, they are experienced as necessary coordination mechanism. The piton perspective flags that strike leverage is eroding (theater increasing) as automation and port diversification mature — the ritual is increasingly performative. The mountain perspective naturalizes supply chain interdependence as inevitable, but this obscures how port monopolies, union structures, and trade agreements are institutional contingencies, not natural laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Worker directionality (d) is high (≈0.85 at trapped level) because they are nominal beneficiaries (capture wage concessions) but actual victims (absorbed into coercive collective action with uncertain personal payoff). Union structure requires strike participation (suppression gate), but individual workers may lose income during strike. Capital directionality (d) is low (≈0.15) because arbitrage options are available — they can reroute, automate, or wait. Consumer directionality is very high (d ≈0.95) because they bear cascade costs with no participation or exit option. Supplier directionality is moderate (d ≈0.60) because they have some coordination benefit (supply timing) but face extraction through inventory and penalty mechanisms. The asymmetry is structural: the wage negotiation is between workers and capital, but the cost distribution extends to third parties (consumers, suppliers) who have no bargaining power.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURE: Port strikes resolve the mandatrophy by showing that the tangled_rope classification is precise — there is genuine coordination function (workers can only negotiate collectively; capital needs organized labor flow) AND asymmetric extraction (costs cascade backward to powerless consumers and suppliers). The constraint cannot be reduced to pure extraction (snare) because the coordination mechanism is real; it cannot be reduced to pure coordination (rope) because the extraction through supply chain cascades is real and asymmetric. IDENTITY DYNAMICS: There is incipient identity_locked dynamics — port workers' identity is increasingly fused with union membership and strike participation, creating psychological suppression beyond the material suppression of geographic immobility. As automation threatens the port labor profession itself, workers face identity collapse (no longer a 'port worker' if ports are automated). This intensifies the snare experience at biographical scale even as the generational rope perspective remains valid. PITON TRAJECTORY: Theater ratio rising to 0.48 suggests the constraint is degrading toward piton — strike threat carries weight through institutional habit (unions can still mobilize) rather than structural necessity (automation makes port disruption more avoidable). Within 10-15 years, the constraint may shift from tangled_rope (genuine coordination with asymmetric extraction) to piton (degraded ritual of union strike with minimal functional verification).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strike_leverage_trajectory,
    'Is port strike leverage increasing or decreasing as port automation, diversification, and supply chain resilience measures mature?',
    'Historical analysis of strike duration, economic impact, and wage gains across 1990-2030; correlation with automation investment and port diversification rates',
    'If decreasing: piton classification becomes primary (ritual replacement of function). If increasing: snare extraction mechanism is strengthening. If stable: tangled_rope classification is stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strike_leverage_trajectory, empirical, 'Port strike leverage trend over supply chain transformation').

omega_variable(
    supply_chain_relocation_feasibility,
    'Can supply chains genuinely relocate to non-union or automated ports, or are geographic, infrastructure, and customer-proximity factors immutable constraints?',
    'Case studies of relocation attempts (China to Vietnam, LA to Long Beach to Oakland); cost-benefit analysis including infrastructure development, customer distance, and tariff impacts',
    'If feasible: port operators have genuine arbitrage options and rope classification holds. If infeasible: port operators are constrained and extraction distribution shifts (less rope, more snare)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_relocation_feasibility, empirical, 'Port relocation feasibility and arbitrage optionality').

omega_variable(
    consumer_vs_worker_extraction_ratio,
    'What fraction of port strike economic costs are borne by workers (wage/employment loss during strike) vs consumers (price inflation, scarcity) vs capital (reduced margins)?',
    'Post-strike economic analysis: wage losses to workers, price impact on consumer baskets, capital loss calculations; international comparison of strike cost distribution',
    'If workers bear majority: snare dominates (workers extract from consumers but lose to capital). If capital bears majority: tangled_rope confirmed (capital subsidizes worker gains). If consumers bear majority: snare extraction flows through supply chain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_vs_worker_extraction_ratio, empirical, 'Distribution of port strike economic costs across stakeholders').

omega_variable(
    automation_substitute_adequacy,
    'Can automated port handling and autonomous cargo systems genuinely substitute for human labor at equivalent cost and reliability, or are there irreducible coordination/flexibility constraints that require human judgment?',
    'Technical analysis of current and projected automation (port robotics, autonomous vehicles, AI scheduling); cost comparison including maintenance, software licensing, and flexibility loss',
    'If substitutable: suppression increases (workers face automation threat) and piton mechanism accelerates. If non-substitutable: workers maintain leverage and snare extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_substitute_adequacy, empirical, 'Automation adequacy and labor substitutability in ports').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(port_strike_cascades, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(portstr_tr_t0, port_strike_cascades, theater_ratio, 0, 0.38).
narrative_ontology:measurement(portstr_tr_t5, port_strike_cascades, theater_ratio, 5, 0.42).
narrative_ontology:measurement(portstr_tr_t10, port_strike_cascades, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(portstr_be_t0, port_strike_cascades, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(portstr_be_t5, port_strike_cascades, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(portstr_be_t10, port_strike_cascades, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(port_strike_cascades, resource_allocation).
narrative_ontology:affects_constraint(port_strike_cascades, global_supply_chain_concentration).
narrative_ontology:affects_constraint(port_strike_cascades, union_labor_leverage_erosion).
narrative_ontology:affects_constraint(port_strike_cascades, port_automation_trajectory).
narrative_ontology:affects_constraint(port_strike_cascades, shipping_line_consolidation).

% DUAL FORMULATION NOTE:
% Port strike cascades is downstream of supply chain concentration and union structure but represents a distinct constraint. The upstream constraints (shipping line consolidation, port monopoly) have their own extractiveness values reflecting market power; port strike cascades has its own extractiveness reflecting wage-setting asymmetry and cost cascade.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(port_strike_cascades, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
