% ============================================================================
% CONSTRAINT STORY: optimization_fragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_optimization_fragility, []).

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
 *   constraint_id: optimization_fragility
 *   human_readable: The Efficiency-Resilience Tradeoff in Just-in-Time Systems
 *   domain: economic/technological/infrastructural
 *
 * SUMMARY:
 *   Just-in-Time (JIT) manufacturing and logistics represent a historically
 *   successful coordination technology that has been progressively optimized
 *   to remove buffer capacity and slack. Over the past 20 years, this
 *   optimization has created structural fragility: the same system that
 *   efficiently moves goods during normal operation becomes a cascading
 *   failure mechanism during disruption. The constraint exhibits all six DR
 *   types depending on observer position. Cost-minimizing corporations
 *   experience JIT as pure coordination (Rope). Supply chain workers
 *   experience it as mixed coordination and extraction (Tangled Rope).
 *   Dependent consumers with no alternative supply sources experience it as
 *   pure extraction (Snare). Regulatory frameworks enforce optimization
 *   metrics through institutions designed for a different era (Piton). Open
 *   supply chain ecosystems see the coordination function plus systemic risk
 *   (Tangled Rope). The civilizational analyst risks naturalizing the
 *   efficiency-resilience tradeoff as an immutable law rather than a policy
 *   choice (false Mountain). The theater ratio has increased over the
 *   measurement interval as regulatory compliance, efficiency auditing, and
 *   optimization reporting have become performative — firms celebrate
 *   inventory turns without measuring resilience, masking the growing
 *   structural fragility. The extraction value (0.58) reflects that cost
 *   savings flow asymmetrically to capital while disruption costs flow
 *   asymmetrically to labor and consumers.
 *
 * KEY AGENTS:
 *   - Cost-Minimizing Corporation: Primary beneficiary (institutional/arbitrage) — captures profit from inventory elimination and accelerated capital turnover; can shift suppliers when disruption threatens
 *   - Dependent Consumer: Primary victim (powerless/trapped) — bears full cost of supply disruptions with no alternative sourcing; no buffer means no forgiveness for system failures
 *   - Supply Chain Worker: Secondary victim (moderate/constrained) — benefits from JIT employment creation but constrained by zero-slack work conditions and wage pressure from competitive optimization
 *   - Supply Chain Ecosystem: Organized agent (organized/constrained) — logistics firms and component manufacturers solve genuine coordination problems but face systemic collapse risk when cascades begin
 *   - Regulatory Framework: Institutional actor (institutional/arbitrage) — tax policy and trade rules enforce efficiency metrics; maintains performative compliance theater while missing resilience degradation
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing the efficiency-resilience tradeoff as immutable rather than a contingent policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(optimization_fragility, 0.58).
domain_priors:suppression_score(optimization_fragility, 0.68).
domain_priors:theater_ratio(optimization_fragility, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(optimization_fragility, extractiveness, 0.58).
narrative_ontology:constraint_metric(optimization_fragility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(optimization_fragility, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(optimization_fragility, tangled_rope).
narrative_ontology:human_readable(optimization_fragility, "The Efficiency-Resilience Tradeoff in Just-in-Time Systems").
narrative_ontology:topic_domain(optimization_fragility, "economic/technological/infrastructural").

domain_priors:requires_active_enforcement(optimization_fragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(optimization_fragility, cost_minimizers).
narrative_ontology:constraint_beneficiary(optimization_fragility, shareholders).
narrative_ontology:constraint_victim(optimization_fragility, system_users).
narrative_ontology:constraint_victim(optimization_fragility, supply_chain_workers).
narrative_ontology:constraint_victim(optimization_fragility, aggregate_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT CONSUMER (SNARE) — Trapped in an optimized system with no buffer capacity. When disruption occurs (supply shock, natural disaster, geopolitical event), the powerless consumer bears immediate cost: supply interruptions, price spikes, unavailable goods. No alternative supply chains. Exit is not an option. Maximum experienced extraction through vulnerability.
constraint_indexing:constraint_classification(optimization_fragility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUPPLY CHAIN WORKER (TANGLED ROPE) — Experiences both coordination benefits (JIT efficiency creates employment and optimization problems) and extraction (zero slack means zero forgiveness for errors or personal circumstance; wage stagnation from competitive pressure). Constrained exit due to geographic and skill specificity. The worker benefits from the system existing but bears the cost of its rigidity.
constraint_indexing:constraint_classification(optimization_fragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COST-MINIMIZING CORPORATION (ROPE) — Sees JIT optimization as pure coordination problem solved: eliminated inventory costs, accelerated capital turnover, reduced warehousing. Experiences the constraint as a beneficial coordination mechanism. Can arbitrage between suppliers; has exit option (shift supply chains, off-shore). Extraction flows toward this agent as profit. Benefits from the discipline imposed on others.
constraint_indexing:constraint_classification(optimization_fragility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SUPPLY CHAIN ECOSYSTEM (TANGLED ROPE) — Organized agents (logistics firms, component manufacturers, just-in-time coordinators) derive income from optimization enforcement but also bear the structural risk. When cascading failures occur, the ecosystem faces systemic collapse. Sees the constraint as hybrid: genuine coordination function (JIT logistics is real technical achievement) combined with asymmetric extraction (firms profit on normal operation, collapse on disruption, costs externalized).
constraint_indexing:constraint_classification(optimization_fragility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — National and international trade rules mandate efficiency and penalize 'wasteful' inventory holding through tax policy, tariff structure, and regulatory reporting. This enforcement mechanism persists through institutional inertia: rules designed for mid-20th-century manufacturing conditions still govern 21st-century supply chains. The framework is substantially performative — compliance is theater (annual audits, efficiency reporting) with little real resilience improvement. Theater ratio high because regulators celebrate optimization metrics while supply chains grow fragile.
constraint_indexing:constraint_classification(optimization_fragility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SYSTEM LIMITS (FALSE SUMMIT) — The civilizational observer risks classifying the efficiency-resilience tradeoff as an immutable law of complex systems: that optimization must sacrifice resilience, that you cannot have both speed and slack. This naturalizes what is actually a policy choice. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(optimization_fragility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(optimization_fragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(optimization_fragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(optimization_fragility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(optimization_fragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(optimization_fragility, TR),
    TR >= 0.70.

:- end_tests(optimization_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. JIT optimization has systematically removed buffer capacity, creating rents for cost minimizers and vulnerability for everyone else. The extraction is not maximal (0.90+) because genuine coordination function exists — JIT is a real technical achievement, not purely parasitic. But the asymmetry has grown as firms have optimized beyond the point where resilience remains. Suppression (0.68): Significant. Alternatives to JIT (strategic inventory, redundant suppliers, geographic diversification) are suppressed by financial incentive structures that penalize holding costs, by tax policy that discourages capital-intensive inventory, and by competitive pressure that forces margin compression. A firm that chooses to maintain resilient buffers faces cost disadvantage against optimized competitors. The suppression is real and structural, not absolute — some firms maintain higher buffers, but at competitive cost. Theater ratio (0.55): Moderate. JIT systems produce extensive efficiency metrics, optimization reports, and compliance documentation. Much of this theater is performative — it celebrates inventory turns and throughput without measuring resilience or failure recovery time. However, JIT also has genuine technical content (real scheduling, real logistics optimization), so theater does not dominate as it would in pure Piton. The measurement trajectory shows increasing theater as firms add more regulatory reporting and efficiency auditing while actual resilience degrades.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon produces radically different classifications depending on structural position. From the firm's viewpoint, JIT is a rope (coordination problem solved). From the consumer's viewpoint during disruption, it is a snare (trapped in unavailable supply). From the worker's viewpoint, it is tangled rope (coordination benefits mixed with extraction pressure). From the ecosystem viewpoint, it is also tangled rope but with different risk profile. The regulatory viewpoint is piton (enforcement theater). The civilizational observer risks mountain (immutable law). None of these is wrong — they are accurate descriptions of different structural relationships to the same constraint. The perspectival gap reveals that optimization fragility is not a technical problem with a single answer but a distribution problem with multiple legitimate stakes.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality depends critically on whether the observer benefits from optimization or bears its fragility costs. Cost-minimizing corporations benefit directly from JIT (d ≈ 0.0 in beneficiary frame, arbitrage exit → negative chi). Supply chain workers experience both benefits and costs (d ≈ 0.55, moderate power + constrained exit → moderate chi). Dependent consumers bear all fragility costs with no optimization benefits (d ≈ 0.95, powerless + trapped exit → maximum chi approaching snare). Regulatory frameworks maintain low extraction through institutional arbitrage (d ≈ 0.10, institution + arbitrage → low chi). The supply chain ecosystem sits between beneficiary and victim: it profits from normal operation but risks collapse (d ≈ 0.65, organized power + constrained exit → moderate-high chi). The engine's derivation chain computes these from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that both coordination and extraction elements are structurally present. JIT solves a genuine coordination problem (how to move goods without excessive inventory cost), so pure Snare classification is incomplete. But JIT also creates asymmetric extraction (cost minimizers capture gains; disruption victims bear costs), so pure Rope classification is also incomplete. Tangled Rope classification is justified because: (1) JIT provides genuine coordination function (real technical achievement in logistics), (2) extraction is significant and asymmetric (cost savings flow to capital, disruption costs to labor and consumers), (3) enforcement is active (tax policy, competitive pressure, regulatory framework). The mandatrophy is resolved by recognizing that JIT is legitimately both: a coordination mechanism that solved a real problem AND an extraction mechanism that has been pushed beyond resilience through competitive optimization. The false summit risk (Mountain) is critical: the constraint naturalizes the efficiency-resilience tradeoff as immutable law when it is actually a policy choice. Societies could require resilience buffers, mandate inventory holding, tax financial velocity that enables JIT, or enforce supply chain redundancy — these are policy moves, not natural laws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    buffer_adequacy_threshold,
    'What inventory/capacity buffer level constitutes resilience without wasting efficiency gains?',
    'Comparative analysis of systems with 5%, 15%, and 30% buffer capacity across disruption types (supply shock, demand spike, component failure); measurement of actual recovery time and system survival',
    'If buffer threshold low (5%): current systems already resilient (snare classification weakens). If threshold high (30%): current optimization is severe extraction (snare classification strengthens, tangled_rope requires active renegotiation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(buffer_adequacy_threshold, empirical, 'Optimal buffer level balancing efficiency and resilience').

omega_variable(
    cascading_failure_predictability,
    'Can supply chain cascade failures be predicted before they propagate, enabling preemptive slack allocation?',
    'Development and testing of early-warning systems (network pressure monitoring, supplier stress indicators); comparison of actual failure cascade duration with predicted cascade duration in optimized vs buffered systems',
    'If predictable: slack can be deployed dynamically (reduces snare extraction, enables scaffold dynamics). If unpredictable: slack must be static (maintains snare extraction, validates current optimization trap).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cascading_failure_predictability, empirical, 'Predictability of supply chain cascade failures').

omega_variable(
    extraction_beneficiary_identity,
    'Do cost savings from JIT optimization flow primarily to shareholders, consumers, or intermediate agents?',
    'Financial analysis of supply chain pricing, margin compression, and profit distribution across supply tiers; time-series comparison of input cost reduction vs end-price reduction vs operator profit changes',
    'If primarily to shareholders: tangled_rope with asymmetric extraction toward capital (current story). If primarily to consumers: rope with genuine coordination (reclassifies). If distributed: snare classification weakens as no single beneficiary emerges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_beneficiary_identity, empirical, 'Distribution of JIT efficiency gains across supply chain actors').

omega_variable(
    system_boundary_definitions,
    'Are supply chain failures truly ''systemic'' or are they bounded to specific sectors, enabling partial unraveling?',
    'Analysis of actual disruption events (semiconductor shortage 2021-2023, COVID-19 supply chain impacts, port congestion 2021-2022); measurement of failure propagation speed and sector isolation effectiveness',
    'If failures propagate globally: snare classification is robust (optimization trap is comprehensive). If failures remain sectoral: snare weakens to tangled_rope or rope (agents can selectively buffer specific supply chains).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(system_boundary_definitions, empirical, 'Scope and propagation of supply chain failures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(optimization_fragility, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(optfrag_tr_t0, optimization_fragility, theater_ratio, 0, 0.3).
narrative_ontology:measurement(optfrag_tr_t10, optimization_fragility, theater_ratio, 10, 0.42).
narrative_ontology:measurement(optfrag_tr_t20, optimization_fragility, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(optfrag_be_t0, optimization_fragility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(optfrag_be_t10, optimization_fragility, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(optfrag_be_t20, optimization_fragility, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(optimization_fragility, resource_allocation).
narrative_ontology:affects_constraint(optimization_fragility, semiconductor_supply_concentration).
narrative_ontology:affects_constraint(optimization_fragility, pharmaceutical_manufacturing_consolidation).
narrative_ontology:affects_constraint(optimization_fragility, agricultural_monoculture_fragility).

% DUAL FORMULATION NOTE:
% Optimization fragility is upstream of specific sector supply constraints (semiconductors, pharmaceuticals, agriculture). Each downstream constraint has its own extractiveness reflecting domain-specific factors. The upstream optimization_fragility constraint represents the general structural logic that appears across multiple domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(optimization_fragility, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
