% ============================================================================
% CONSTRAINT STORY: silent_dependency_activation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silent_dependency_activation, []).

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
 *   constraint_id: silent_dependency_activation
 *   human_readable: The Invisible Supply Chain Trap
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The invisible supply chain trap emerges when systems achieve efficiency
 *   through consolidation and cost optimization that hides critical
 *   dependencies. During normal market conditions, the bottleneck remains
 *   latent — invisible because the upstream monopolist controls a component
 *   that appears commodity-like, with minimal apparent strategic value. No
 *   signal propagates upstream to reveal the true scarcity. This constraint
 *   exhibits a distinctive temporal structure: extractiveness increases
 *   sharply once market disruption (geopolitical event, demand shock, natural
 *   disaster) activates the hidden dependency as a binding constraint. Once
 *   activated, the downstream manufacturer faces sudden scarcity, price
 *   extraction, and supply control. The upstream monopolist shifts from
 *   cost-compression beneficiary to extraction mechanism. The constraint
 *   operates by maintaining this visibility gap — suppressing or obscuring
 *   information about concentration until activation forces recognition.
 *   Theater is moderate because supply chain risk management creates an
 *   appearance of monitoring without preventing activation. The constraint
 *   exemplifies how distributed rational optimization (each actor minimizing
 *   their own costs) can create a collective trap that extracts maximum value
 *   once conditions change.
 *
 * KEY AGENTS:
 *   - Upstream Monopolist: Primary beneficiary (institutional/arbitrage) — controls hidden dependency, benefits from cost optimization during invisibility window, captures extraction during activation
 *   - Downstream Manufacturers: Primary victim (powerless/trapped) — locked into dependency with no exit option; face sudden supply disruption and price extraction
 *   - Cost Optimizers (procurement, logistics firms): Secondary beneficiary (institutional/arbitrage) — benefit from efficiency gains during normal conditions; often complicit in creating visibility gap
 *   - Supply Chain Community: Secondary victim (moderate/constrained) — bears costs of systemic fragility when bottleneck activates; some agency through diversification but constrained by economics
 *   - Industrial Policy Coalition: Organized actor (organized/constrained) — state actors and consortiums attempting to build redundancy; face high enforcement costs of maintaining diverse supply
 *   - Supply Chain Risk Management Industry: Institutional actor (institutional/arbitrage) — maintains performative auditing and risk metrics with limited actual bottleneck prevention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silent_dependency_activation, 0.58).
domain_priors:suppression_score(silent_dependency_activation, 0.68).
domain_priors:theater_ratio(silent_dependency_activation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silent_dependency_activation, extractiveness, 0.58).
narrative_ontology:constraint_metric(silent_dependency_activation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(silent_dependency_activation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silent_dependency_activation, tangled_rope).
narrative_ontology:human_readable(silent_dependency_activation, "The Invisible Supply Chain Trap").
narrative_ontology:topic_domain(silent_dependency_activation, "technological/economic").

domain_priors:requires_active_enforcement(silent_dependency_activation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(silent_dependency_activation, upstream_monopolist).
narrative_ontology:constraint_beneficiary(silent_dependency_activation, cost_optimizers).
narrative_ontology:constraint_victim(silent_dependency_activation, downstream_manufacturers).
narrative_ontology:constraint_victim(silent_dependency_activation, end_consumers).
narrative_ontology:constraint_victim(silent_dependency_activation, supply_chain_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM MANUFACTURER (SNARE) — Locked into dependency on hidden component supplier with no viable exit. Faces sudden supply disruptions and price extraction once bottleneck is activated. Cannot reorganize supply chain in meaningful timeframe. Experiences maximum extraction with no alternative sources.
constraint_indexing:constraint_classification(silent_dependency_activation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUPPLY CHAIN COMMUNITY (TANGLED ROPE) — Benefits from cost optimization and efficiency gains from consolidated supply chains during normal conditions. But bears asymmetric costs when bottleneck activates. Has some agency through diversification efforts and strategic stockpiling, but reorganization is slow and expensive. Mixed coordination and extraction.
constraint_indexing:constraint_classification(silent_dependency_activation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: UPSTREAM MONOPOLIST (ROPE) — Controls hidden dependency and benefits from efficiency coordination that hides the true bottleneck. Gains from cost compression and market consolidation. Can arbitrage between transparent and opaque pricing regimes. Experiences constraint as pure coordination mechanism enabling extraction.
constraint_indexing:constraint_classification(silent_dependency_activation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDUSTRIAL POLICY COALITION (TANGLED ROPE) — Organized state actors and industry consortiums attempting to build redundancy and resilience. Benefit from recognizing the bottleneck (coordination function). But face high costs of restructuring and must actively enforce redundant supply capacity. Active enforcement against market consolidation logic.
constraint_indexing:constraint_classification(silent_dependency_activation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPPLY CHAIN RISK MANAGEMENT INDUSTRY (PITON) — Risk metrics, auditing protocols, and supply chain mapping tools are largely performative. They claim to identify hidden dependencies but rarely prevent activation because the incentive structure (cost minimization) overwhelms risk signals. Theater persists through institutional inertia despite low effectiveness at preventing bottleneck activation.
constraint_indexing:constraint_classification(silent_dependency_activation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some supply chain opacity is inherent to complex systems: perfect visibility is computationally intractable, and some dependencies will always be latent. However, the structural data contradicts this naturalization — the hidden dependency is actively maintained through cost optimization incentives, not inherent to complexity itself.
constraint_indexing:constraint_classification(silent_dependency_activation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silent_dependency_activation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(silent_dependency_activation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silent_dependency_activation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(silent_dependency_activation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(silent_dependency_activation, TR),
    TR >= 0.70.

:- end_tests(silent_dependency_activation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint is not pure extraction (ε ≥ 0.66) because significant efficiency gains flow downstream during invisibility window. Manufacturers benefit from cost reduction. But the structure creates acute extraction potential once activated. The 0.58 value reflects the average state over the interval: low during invisibility, high during and after activation. Suppression (0.68): High. Information barriers maintain the visibility gap. Upstream monopolist has incentive to suppress knowledge of concentration. Downstream actors lack transparency into supply concentration metrics. Standard procurement practices (competitive bidding, minimal long-term contracts) mask the true dependency structure. Alternatives exist in principle but are economically suppressed through cost differentials. Theater ratio (0.55): Moderate-high. Supply chain risk management creates an appearance of monitoring. Audits, certifications, and diversity scorecards circulate without preventing activation. The theater reflects that these tools are inherently backward-looking — they document past concentration but cannot prevent it if cost incentives drive consolidation. Claimed type: Tangled Rope. The constraint satisfies the gate: (1) base extraction ε=0.58 ≥ 0.30, (2) beneficiaries present (upstream monopolist, cost optimizers), (3) victims present (downstream manufacturers, supply chain resilience), (4) requires active enforcement (maintaining cost discipline despite visibility of risk). The constraint exhibits both genuine coordination (cost efficiency is real) and asymmetric extraction (concentrated upside capture).
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between invisibility and activation. During the invisibility phase (0-15 years in the measurement interval), the upstream monopolist sees pure Rope — efficient coordination with no apparent extraction. Cost optimizers see the same. Downstream manufacturers see minimal constraint because costs are low and supplies ample. Risk managers see a manageable problem with existing tools. But once activation occurs (t>15), the same structural constraint flips from Rope to Snare in multiple perspectives. The downstream manufacturer's invisible Rope suddenly becomes transparent Snare. The upstream monopolist's coordination becomes obvious extraction. The industrial policy coalition's Tangled Rope (mixed coordination and enforcement costs) becomes acute. The risk management industry's Piton (performative tools) is revealed. The analytical observer's Mountain (inherent complexity) is exposed as a naturalization of market structure. This perspectival collapse — where a single constraint simultaneously appears as coordination and extraction depending on activation state — reveals that the constraint's true structure is not determined by market fundamentals but by information asymmetry and temporal surprise.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives. Upstream monopolist: beneficiary status + arbitrage exit → low d (≈0.15) → negative f(d) → extraction flows toward them. Downstream manufacturer: victim status + trapped exit → high d (≈0.95) → f(d)≈1.42 → maximum experienced extractiveness. Cost optimizers: beneficiary status but constrained by regulations → moderate d (≈0.35) → moderate extraction capture. Industrial policy coalition: organized but constrained by market economics → moderate-high d (≈0.60) → moderate-high effective extraction against their interests. Risk management industry: institutional but with arbitrage (can exit via contracting) → low d (≈0.20) → mostly insulated from extraction. The perspectival gap reflects different structural positions relative to the visibility/activation timeline. Those benefiting from invisibility (upstream, cost optimizers) experience low chi. Those harmed by activation (downstream, resilience community) experience high chi. The constraint's dynamics are temporal — chi changes as the system transitions from invisible to activated state.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION MECHANISM: The constraint is classified as Tangled Rope at base analysis (not pure Snare) because the efficiency coordination is genuine during invisibility: downstream actors do benefit from lower costs. But the classification must account for the temporal dynamics. At t=0-15, multiple perspectives (beneficiary, cost optimizer, manufacturer) see Rope. At t=15-30, the same structural data produces Snare/Tangled Rope depending on perspective. The mandatrophy is resolved by explicitly modeling this as a constraint with activation dynamics, not as a static type. The ε value (0.58) is computed as a temporal average — low during invisibility, high during activation. This prevents the false conclusion that the constraint is either pure coordination (it isn't, given extraction potential) or pure extraction (it isn't, given real efficiency gains). The theater ratio (0.55) measures the performative content of risk management — high enough to mask the dependency until activation, but not so high as to be obviously theater. This middle ground is what makes the constraint extractive: the theater provides false assurance that allows cost optimization to continue despite risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    visibility_threshold_activation,
    'At what point does a supply chain dependency transition from economically invisible to operationally critical?',
    'Historical analysis of supply disruptions: correlation between pre-disruption supply concentration metrics and post-disruption price spikes and production halts',
    'If threshold is low (small concentration -> immediate impact): markets would price in risk earlier. If threshold is high (large concentration required): extraction persists longer before social/political response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(visibility_threshold_activation, empirical, 'Supply concentration threshold for bottleneck activation').

omega_variable(
    cost_compression_vs_resilience_tradeoff,
    'Is the cost optimization driving hidden dependencies a genuinely necessary tradeoff or a distributional choice that extracts from downstream actors?',
    'Comparative analysis of supply chains with different redundancy levels; accounting for total system costs including disruption externalities vs. pure procurement costs',
    'If tradeoff is genuine: constraint is Rope from system perspective (coordination problem). If distributional: constraint is Snare/Tangled Rope (extraction mechanism disguised as efficiency).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_compression_vs_resilience_tradeoff, empirical, 'Whether cost optimization reflects genuine system tradeoffs or distributional extraction').

omega_variable(
    actor_awareness_asymmetry,
    'Do upstream monopolists actively suppress knowledge of bottleneck dependencies, or do dependencies emerge from decentralized optimization without intentional hiding?',
    'Documentary analysis of internal communications, strategy documents, and supplier relationship management; comparison of disclosures to financial markets vs. operational partners',
    'If intentional suppression: establishes malicious enforcement (Snare/Tangled Rope). If emergent opacity: suggests coordination failure (Rope) rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actor_awareness_asymmetry, empirical, 'Whether bottleneck opacity reflects intentional suppression or emergent complexity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silent_dependency_activation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(silent_tr_t0, silent_dependency_activation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(silent_tr_t15, silent_dependency_activation, theater_ratio, 15, 0.48).
narrative_ontology:measurement(silent_tr_t30, silent_dependency_activation, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(silent_be_t0, silent_dependency_activation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(silent_be_t15, silent_dependency_activation, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(silent_be_t30, silent_dependency_activation, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(silent_dependency_activation, resource_allocation).
narrative_ontology:affects_constraint(silent_dependency_activation, rare_earth_supply_concentration).
narrative_ontology:affects_constraint(silent_dependency_activation, semiconductor_fabrication_bottleneck).
narrative_ontology:affects_constraint(silent_dependency_activation, pharmaceutical_active_ingredient_sourcing).

% DUAL FORMULATION NOTE:
% The invisible supply chain trap is a family of structurally similar constraints across different supply chains. Each specific supply chain (rare earths, semiconductors, pharmaceuticals) has its own constraint story with domain-specific extractiveness values. This story documents the generic structural pattern that activates across all members of the family. Upstream constraints (rare_earth_supply_concentration) have lower extractiveness (higher visibility). Downstream activation constraints have higher extractiveness once the bottleneck manifests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(silent_dependency_activation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
