% ============================================================================
% CONSTRAINT STORY: supply_chain_monopoly_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supply_chain_monopoly_power, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: supply_chain_monopoly_power
 *   human_readable: Supply Chain Monopoly Power Extraction
 *   domain: economic/industrial_organization
 *
 * SUMMARY:
 *   Supply chain monopoly power arises when control of a critical input or
 *   production node concentrates in a single actor who uses that control to
 *   extract rents from dependent suppliers and downstream consumers. This
 *   constraint operates across multiple structural levels: direct supplier
 *   extraction through margin compression and unfair payment terms; consumer
 *   extraction through pricing power and reduced innovation incentives;
 *   competitive suppression through controlled supply rationing; and
 *   institutional extraction through regulatory capture that prevents
 *   antitrust enforcement. The constraint's theater_ratio (0.35) remains
 *   consistently low because monopoly extraction relies on structural
 *   barriers and direct economic coercion, not performative legitimacy.
 *   Unlike degraded institutions (pitons), supply chain monopoly power is a
 *   functioning extraction mechanism that does not require theatrical
 *   maintenance. The extractiveness trajectory (0.38 → 0.68 over the
 *   interval) reflects progressive margin accumulation and supply control
 *   intensification as the monopoly controller internalizes more of the
 *   supply chain and consolidates market position. Dependent suppliers face
 *   trapped exit (specific investments, customer concentration, switching
 *   costs), end consumers face constrained exit (product lock-in, high
 *   switching friction), and competitive participants face constrained exit
 *   (capital barriers to building alternative supplies). The regulatory
 *   authority retains genuine mobility and can reshape the constraint through
 *   enforcement and market restructuring, making the scaffold perspective
 *   viable. The global trade regime, by contrast, has become largely
 *   performative in preventing concentration—it legitimizes free-trade
 *   principles while those principles no longer functionally prevent monopoly
 *   formation.
 *
 * KEY AGENTS:
 *   - Monopoly Controller: Primary beneficiary (institutional/arbitrage) — captures margin extraction, supply control premiums, and first-mover advantages; can pivot or divest at low cost
 *   - Dependent Suppliers: Primary victim (powerless/trapped) — locked into buyer relationship with no viable alternatives; face margin compression, exclusivity demands, payment term manipulation
 *   - End Consumers: Secondary victim (powerless/constrained) — face reduced choice, elevated prices, reduced quality innovation; can theoretically exit but switching costs and product lock-in constrain mobility
 *   - Competitive Market Participants: Tertiary victim (organized/constrained) — face both coordination benefits (reduced supply variance through monopoly) and extraction costs (supply rationing, pricing markup); capital and regulatory barriers prevent building competing supply chains
 *   - Regulatory Authority: Institutionalized constraint modifier (powerful/mobile) — maintains antitrust frameworks and can reshape concentration through enforcement and mandates; retains genuine exit optionality
 *   - Global Trade Regime: Degraded constraint legitimizer (institutional/arbitrage) — maintains formal free-trade norms while those norms have degraded as prevention mechanisms; persists through institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supply_chain_monopoly_power, 0.68).
domain_priors:suppression_score(supply_chain_monopoly_power, 0.72).
domain_priors:theater_ratio(supply_chain_monopoly_power, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supply_chain_monopoly_power, extractiveness, 0.68).
narrative_ontology:constraint_metric(supply_chain_monopoly_power, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(supply_chain_monopoly_power, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supply_chain_monopoly_power, snare).
narrative_ontology:human_readable(supply_chain_monopoly_power, "Supply Chain Monopoly Power Extraction").
narrative_ontology:topic_domain(supply_chain_monopoly_power, "economic/industrial_organization").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supply_chain_monopoly_power, monopoly_controller).
narrative_ontology:constraint_victim(supply_chain_monopoly_power, dependent_suppliers).
narrative_ontology:constraint_victim(supply_chain_monopoly_power, downstream_consumers).
narrative_ontology:constraint_victim(supply_chain_monopoly_power, competitive_market_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT SUPPLIER (SNARE) — Supplier locked into monopoly buyer relationship with no viable alternatives. Cannot exit without bankruptcy or forced pivot to incompatible markets. Bears full extraction cost through margin compression, exclusivity demands, and payment term manipulation. Maximum suppression: customer concentration risk, switching costs, asset specificity, and information asymmetry create immobilizing barriers.
constraint_indexing:constraint_classification(supply_chain_monopoly_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: END CONSUMER (SNARE) — Faces reduced choice and elevated prices due to monopoly-driven supply constraints and reduced competitive pressure. Exit is theoretically possible but constrained by high switching costs, product differentiation lock-in, and geographic concentration of alternative sources. Bears extraction through pricing power and reduced quality innovation.
constraint_indexing:constraint_classification(supply_chain_monopoly_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MONOPOLY CONTROLLER (ROPE) — Experiences the constraint as a coordination mechanism: centralized control of critical inputs enables efficient production orchestration and reduces redundancy. Benefits from margin extraction, first-mover advantage, and network effects. Has maximal exit optionality through arbitrage (can divest, reposition, or dissolve without structural cost). Perceives constraint as natural outcome of superior efficiency and integration.
constraint_indexing:constraint_classification(supply_chain_monopoly_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPETING MARKET PARTICIPANTS (TANGLED ROPE) — Face both coordination barriers and extraction. Genuine coordination problem exists: monopoly control reduces unpredictable supply fluctuations and quality variance, benefiting downstream producers who compete with each other. Simultaneously, the monopoly extracts through price markup and supply rationing. Organizations constrained by regulatory, capital, and time barriers to building alternative supply chains. Experience mixed benefit (coordination) and cost (extraction).
constraint_indexing:constraint_classification(supply_chain_monopoly_power, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AUTHORITY (SCAFFOLD) — Maintains antitrust frameworks and supply chain resilience mandates as temporary coordination mechanisms with sunset logic. High mobility (can reshape enforcement, mandate divestitures, alter regulations). Perceives constraint as remediable through enforcement and market redesign. Theater remains low (enforcement is functional, not merely performative). Sunset implicit in regulatory powers: market concentration can be forced to decline through active intervention.
constraint_indexing:constraint_classification(supply_chain_monopoly_power, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: GLOBAL TRADE REGIME (PITON) — Maintains formal neutrality toward supply chain concentration through most-favored-nation principles and free-trade norms, but these norms have degraded as mechanisms for preventing monopoly power. Trade agreements no longer functionally prevent concentration; they mostly perform legitimacy. High theater ratio reflects the performative status of trade institutions as regulators of concentration. The primary function (preventing monopoly power) has atrophied; the institutional form persists through inertia.
constraint_indexing:constraint_classification(supply_chain_monopoly_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / EFFICIENCY NARRATIVE (MOUNTAIN) — From civilizational/universal scope, some supply chain concentration is inherent to economic efficiency: larger integrated systems can achieve lower transaction costs, faster innovation cycles, and higher reliability than fragmented alternatives. This perspective naturalizes monopoly as immutable law of industrial organization. However, the structural data contradicts mountain classification — the extracted-from agents have alternatives (though costly), suppression is maintained by enforcement and information control (not physical law), and the 'efficiency imperative' framing naturalizes what is actually a contingent power distribution.
constraint_indexing:constraint_classification(supply_chain_monopoly_power, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supply_chain_monopoly_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supply_chain_monopoly_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supply_chain_monopoly_power, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(supply_chain_monopoly_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(supply_chain_monopoly_power, TR),
    TR >= 0.70.

:- end_tests(supply_chain_monopoly_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The monopoly controller captures margin extraction through pricing power, supply control premiums, and reduced competitive pressure. Initial extractiveness (0.38) reflects pre-monopoly competitive conditions; final value (0.68) reflects full monopoly consolidation. The rising trajectory reflects progressive margin accumulation and supply control intensification as the monopoly internalizes more supply chain nodes. Suppression (0.72): High. Multiple suppression mechanisms operate: (1) structural barriers—specialized assets, customer concentration, switching costs create immobilizing investment; (2) information asymmetry—monopoly controller controls cost data, quality metrics, and sourcing alternatives; (3) regulatory capture—antitrust enforcement is weakened by institutional capture, reducing exit mechanism credibility; (4) network effects—the monopoly's scale creates benefits (supply reliability, quality consistency) that raise opportunity cost of exit. Theater ratio (0.35): Low and stable. Monopoly extraction functions through direct economic coercion and structural barriers, not performative legitimacy. No need for theatrical maintenance—the constraint is visibly extractive and suppresses alternatives directly. Low theater distinguishes this from piton (degraded) or rope (legitimized coordination), confirming snare classification.
 *
 * PERSPECTIVAL GAP:
 *   The dependent supplier sees pure snare (trapped, maximum extraction, no exit). The end consumer sees snare but with slightly higher perceived exit optionality (constrained rather than trapped). The monopoly controller sees rope (coordination mechanism, legitimate efficiency gains, arbitrage exit). Competing market participants see tangled rope (mixed coordination benefit from supply stability + extraction cost from pricing). The regulatory authority sees scaffold (remediable through enforcement, genuine sunset logic via antitrust mandates). The global trade regime sees itself as rope (free-trade coordination) but the analytical observer diagnoses it as piton (degraded, performative). The civilizational analytical observer risks falsely naturalizing this as mountain (efficiency imperative of industrial organization), but structural data reveals this as false summit: the monopoly is maintained by enforcement and institutional dynamics, not physical law. The perspectival gap reveals that the 'efficiency' framing is a naturalizing narrative deployed to justify extraction that would otherwise appear clearly unjust.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (monopoly_controller) receive low d values due to arbitrage exit optionality and net benefit flow. Victims (dependent_suppliers, downstream_consumers, competitive_market_participants) receive high d values due to trapped or constrained exit and net cost flow. The derived d feeds the sigmoid f(d), which scales extractiveness experienced by each agent based on their structural position relative to the constraint. A dependent supplier with d ≈ 0.92 (victim + trapped) experiences f(d) ≈ 1.38, amplifying the base extractiveness (0.68) to experienced χ ≈ 0.72–0.94 depending on scope. The monopoly controller with d ≈ 0.08 (beneficiary + arbitrage) experiences f(d) ≈ -0.12, producing negative experienced extraction (they benefit from the constraint). The competitive participant with d ≈ 0.58 (victim + constrained, with some coordination benefit) experiences f(d) ≈ 0.75, producing moderate experienced extraction. These directionality values are derived from structural data (exit costs, beneficiary/victim relationship) and require no override.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_vs_extraction_threshold,
    'At what margin/utilization ratio does centralized supply control transition from efficiency coordination to pure extraction?',
    'Comparative efficiency metrics (input costs, inventory turnover, innovation velocity) for monopoly-controlled vs competitive supply chains in same sector; correlation between margin expansion and consumer welfare changes',
    'If efficiency threshold is high (monopoly controller''s margins can reach 0.50+ before efficiency gains are outweighed): constraint reclassifies toward rope for more perspectives. If threshold is low (monopoly controller''s margins above 0.25-0.30 consistently exceed efficiency gains): constraint remains snare across all non-beneficiary perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_vs_extraction_threshold, empirical, 'Threshold separating efficiency gains from extraction').

omega_variable(
    substitution_cost_availability,
    'Do viable substitute supply sources exist at costs that would enable competitive market structure, or are barriers to entry inherently prohibitive?',
    'Cost analysis of new entrant supply chains; geographic/technical barriers to alternative sourcing; capital requirements for backward integration by dependent suppliers',
    'If substitutes available at modest cost premium (< 15%): dependent suppliers are constrained rather than trapped; reclassify to constrained exit, reducing d and experienced extraction. If substitutes unavailable or prohibitively expensive (> 40% cost premium): trap is structural, confirm powerless/trapped classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_cost_availability, empirical, 'Whether viable substitutes exist at accessible cost').

omega_variable(
    regulatory_capture_extent,
    'To what extent have monopoly controllers captured antitrust enforcement and trade regime institutions, converting scaffold/piton into snare?',
    'Analysis of regulatory enforcement patterns, revolving-door personnel flows, lobbying expenditure correlations with enforcement weakness; comparison of enforcement intensity across jurisdictions and historical periods',
    'If capture is high: regulatory authority perspective should shift from scaffold to constrained institutional view, reducing their agency. If capture is low: regulatory capacity is genuine and sunset mechanism is real, confirming scaffold classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Degree of regulatory capture by monopoly controllers').

omega_variable(
    information_asymmetry_persistence,
    'Can dependent suppliers or end consumers access sufficient cost/quality/sourcing information to evaluate alternatives, or is information control itself a suppression mechanism?',
    'Audit of information availability (supplier cost transparency, alternative source quality metrics, total-cost-of-ownership calculators); correlation between information access and supplier/consumer switching rates',
    'If information control is severe: suppression derives partly from epistemic barriers, increasing effective suppression beyond structural barriers alone. Reinforces snare classification for powerless perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_persistence, empirical, 'Information asymmetry as suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supply_chain_monopoly_power, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scmp_tr_t0, supply_chain_monopoly_power, theater_ratio, 0, 0.32).
narrative_ontology:measurement(scmp_tr_t5, supply_chain_monopoly_power, theater_ratio, 5, 0.33).
narrative_ontology:measurement(scmp_tr_t10, supply_chain_monopoly_power, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(scmp_be_t0, supply_chain_monopoly_power, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(scmp_be_t5, supply_chain_monopoly_power, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(scmp_be_t10, supply_chain_monopoly_power, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supply_chain_monopoly_power, resource_allocation).
narrative_ontology:affects_constraint(supply_chain_monopoly_power, industrial_vertical_integration).
narrative_ontology:affects_constraint(supply_chain_monopoly_power, buyer_power_asymmetry).
narrative_ontology:affects_constraint(supply_chain_monopoly_power, information_asymmetry_supply_chains).

% DUAL FORMULATION NOTE:
% Supply chain monopoly power is upstream of multiple sectoral constraints (semiconductor bottlenecks, rare earth dependency, pharmaceutical supply concentration). Each sector-specific monopoly should be modeled as a separate constraint story with its own ε and perspectives, linked via network.affects_constraints to this general supply chain mechanism. This story captures the structural mechanism; sector stories capture observable instances.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
