% ============================================================================
% CONSTRAINT STORY: cascading_constraint_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cascading_constraint_failure, []).

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
 *   constraint_id: cascading_constraint_failure
 *   human_readable: The Dominos of Systemic Collapse
 *   domain: technological/infrastructural/economic
 *
 * SUMMARY:
 *   The dominos of systemic collapse represent a structural constraint that
 *   emerges when a system is composed of tightly coupled, interdependent
 *   sub-constraints. Rather than independent failures, each constraint
 *   failure in the network triggers cascading failures in dependent systems,
 *   extracting redundancy reserves and operational margins from downstream
 *   nodes. This constraint operates across technological (power grids,
 *   telecommunications, internet backbones), infrastructural (transportation,
 *   water, supply chains), and economic (banking, trade finance, market
 *   microstructure) domains. The extraction mechanism is not intentional
 *   monopoly but rather the structural consequence of optimization for
 *   efficiency at the cost of resilience. The tight coupling that enables
 *   fast response times and capital efficiency also eliminates buffers that
 *   would otherwise allow individual failures to remain contained. As
 *   coupling tightens over decades, extractiveness increases because the
 *   system's margin for error shrinks — any failure now ripples through the
 *   entire network. The constraint exhibits high suppression because actors
 *   cannot easily exit the tightly coupled regime without coordinating
 *   massive infrastructure investment.
 *
 * KEY AGENTS:
 *   - Dependent Infrastructure Nodes: Primary victims (powerless/trapped) — individual subsystems bear extraction of redundancy when cascade occurs
 *   - System Users: Collective victims (moderate/constrained) — experience cascade effects without causal visibility into infrastructure dependencies
 *   - Infrastructure Operators: Primary beneficiaries (institutional/arbitrage) — capture efficiency gains and rent from dependency through tight coupling
 *   - Regional Coordination Authorities: Secondary victims (moderate/constrained) — tasked with resilience but constrained by legacy architecture and budget limits
 *   - Resilience Coalition: Organized agents (organized/mobile) — building decoupling and redundancy with sunset trajectory toward safer architecture
 *   - Legacy Regulatory Framework: Institutional sustainer (institutional/arbitrage) — maintains coupling mandates through inertia despite changed risk environment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing architectural choice as immutable system dynamics law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cascading_constraint_failure, 0.68).
domain_priors:suppression_score(cascading_constraint_failure, 0.72).
domain_priors:theater_ratio(cascading_constraint_failure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cascading_constraint_failure, extractiveness, 0.68).
narrative_ontology:constraint_metric(cascading_constraint_failure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cascading_constraint_failure, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cascading_constraint_failure, snare).
narrative_ontology:human_readable(cascading_constraint_failure, "The Dominos of Systemic Collapse").
narrative_ontology:topic_domain(cascading_constraint_failure, "technological/infrastructural/economic").

% --- Structural relationships ---
narrative_ontology:constraint_victim(cascading_constraint_failure, dependent_systems).
narrative_ontology:constraint_victim(cascading_constraint_failure, redundancy_margins).
narrative_ontology:constraint_victim(cascading_constraint_failure, recovery_capacity).
narrative_ontology:constraint_victim(cascading_constraint_failure, system_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT INFRASTRUCTURE NODE (SNARE) — Individual subsystems cannot decouple from the cascade. Once one constraint fails, downstream systems face extraction of their redundancy reserves and operational margin. No exit option exists except catastrophic shutdown. The node bears maximum cost of systemic brittleness.
constraint_indexing:constraint_classification(cascading_constraint_failure, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL COORDINATION AUTHORITY (SNARE) — Tasked with maintaining system resilience but constrained by budget limits, legacy infrastructure, and political pressure to optimize for cost rather than redundancy. Faces extraction of authority when cascades occur; blamed for failures caused by systemic coupling. Limited ability to enforce decoupling.
constraint_indexing:constraint_classification(cascading_constraint_failure, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INFRASTRUCTURE OPERATOR (TANGLED ROPE) — Benefits from operational efficiency gains from tight coupling (lower capital costs, faster response times, unified control). Also extracts rent from dependency: competing operators cannot easily enter when substitution requires breaking couplings. Maintains the tight-coupling regime despite knowing the cascade risk. Active enforcement of coupling through standardization and interconnection mandates.
constraint_indexing:constraint_classification(cascading_constraint_failure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RESILIENCE COALITION (SCAFFOLD) — Organized agents (grid modernization initiatives, decentralized energy projects, mesh networking advocates) are building redundancy and decoupling mechanisms with explicit sunset logic: as redundancy matures and decentralized alternatives become cost-competitive, the tight-coupling regime loses its economic justification. Sunset estimated at 15-30 years depending on sector.
constraint_indexing:constraint_classification(cascading_constraint_failure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY REGULATORY FRAMEWORK (PITON) — Regulations mandating interconnection and standardization were designed for a different risk environment and cost structure. They now persist through institutional inertia despite creating the cascade vulnerability they originally aimed to prevent. The regulatory theater (compliance audits, interconnection agreements) substitutes for actual decoupling investment. Theater ratio high; functional risk-reduction low.
constraint_indexing:constraint_classification(cascading_constraint_failure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SYSTEM DYNAMICS VIEW (MOUNTAIN) — From first principles, tightly coupled systems with delayed feedback exhibit oscillatory instability and cascade failure as mathematical necessities. The constraint appears as an immutable property of complex systems — tight coupling + long propagation delays + incomplete information = cascade risk. However, this perspective risks naturalizing what is actually a choice of system architecture. The engine's false summit detector flags this as naturalization.
constraint_indexing:constraint_classification(cascading_constraint_failure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cascading_constraint_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cascading_constraint_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cascading_constraint_failure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cascading_constraint_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cascading_constraint_failure, TR),
    TR >= 0.70.

:- end_tests(cascading_constraint_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The baseline coupling creates an extraction mechanism where upstream nodes extract reliability margins from downstream nodes by propagating failures. As systems tighten coupling over time (interval shows progression from 0.35 to 0.68), the extraction increases because redundancy buffers disappear. Each optimization for efficiency is an extraction of future safety margin. Suppression (0.72): High. Exiting the tightly coupled regime requires coordinated infrastructure investment that no single actor can undertake unilaterally. Switching costs are prohibitively high; path dependency locks in the tight-coupling regime. Alternative decoupled architectures exist but require breaking standardization agreements and interconnection mandates. Theater ratio (0.58, rising): Moderate and increasing. Compliance theater (resilience audits, redundancy certifications, disaster recovery drills) substitutes for actual decoupling investment. The theater increases over time because the gap between certified resilience and actual cascade risk grows — the performative audits cannot address the structural coupling problem.
 *
 * PERSPECTIVAL GAP:
 *   The infrastructure operator sees tight coupling as coordination (Tangled Rope from their institutional perspective) — they benefit from efficiency and maintain the regime through standardization mandates. Dependent nodes see pure extraction (Snare) — they have no exit and bear cascade costs. The resilience coalition sees a solvable problem with a sunset (Scaffold) — decentralized alternatives and redundancy are becoming cost-competitive. The regulatory framework sees its own degradation (Piton) — interconnection mandates that once prevented monopoly now create cascade vulnerabilities. The analytical observer risks seeing this as an immutable law of complex systems (Mountain) — but the structural data reveals it as a choice of system architecture, not a law of nature. The mandatrophy is resolved: this is extraction (Snare) disguised as coordination (tight coupling), not coordination disguised as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The infrastructure operator holds institutional power and arbitrage options — they can decouple if margins compress, but benefit from tight coupling now. They are the primary beneficiary; d ≈ 0.15 (beneficiary + arbitrage exit). Dependent nodes are powerless with no exit — they cannot uncouple without breaking the system they depend on. They are victims; d ≈ 0.95 (powerless + trapped). Coordination authorities are constrained by legacy infrastructure and budget politics; they are secondary victims but not fully trapped. d ≈ 0.70 (moderate + constrained). The resilience coalition has organized power and mobile exit options — they can build alternatives. But they are currently suppressed by standardization and interconnection mandates. d ≈ 0.45 (organized + mobile + constrained by legacy regime). The cascade's effective extractiveness χ is scaled upward by global scope (σ(S) = 1.2) — failures propagate internationally, amplifying the impact. Suppression is unscaled (raw structural property) — the inability to exit is intrinsic to the coupling regime.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coupling_threshold_identification,
    'What degree of coupling density triggers cascade risk from acceptable to unacceptable?',
    'Empirical analysis of historical cascade events; network graph analysis of failure propagation; simulation of coupling reduction scenarios',
    'If threshold < 0.4 (coupling density): most current infrastructure exceeds safety margin; immediate decoupling investment required. If threshold > 0.8: current tight coupling is defensible; cascade risk is tail-event problem, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coupling_threshold_identification, empirical, 'Threshold coupling density at which cascade risk becomes unacceptable').

omega_variable(
    decoupling_cost_feasibility,
    'Is decoupling to safe coupling densities economically feasible, or does it require subsidy/mandate to overcome capital barriers?',
    'Cost-benefit analysis of decoupling options; comparison of capital requirements vs. amortized cost of cascade events; market test of decoupled alternatives in competitive sectors',
    'If feasible: market competition should drive decoupling naturally; current tight coupling is rent-seeking choice (Snare confirmed). If infeasible: tight coupling is coordination lock-in (reduces to Rope from institutional perspective); decoupling requires subsidy and mandate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decoupling_cost_feasibility, empirical, 'Economic feasibility of decoupling to safe architecture').

omega_variable(
    cascade_propagation_speed,
    'How quickly do cascade failures propagate relative to human decision-making and system mitigation timescales?',
    'Real-time monitoring of cascade events; measurement of failure propagation time vs. human response latency vs. automated mitigation latency',
    'If propagation << human response (seconds to minutes): decoupling or full automation required; no human-in-loop option. If propagation >> human response (hours): coordination and human intervention can limit cascade; systemic extraction is avoidable with good policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cascade_propagation_speed, empirical, 'Speed of cascade propagation relative to mitigation response').

omega_variable(
    information_asymmetry_distribution,
    'Do all nodes in the tightly coupled system have access to the state information needed to make decoupling or failsafe decisions independently?',
    'Audit of information distribution protocols; analysis of decision bottlenecks and information gatekeeping; simulation of node-level resilience if information were fully distributed',
    'If information is centralized: system cannot fail safely — all nodes trapped in collective-action problem (high extraction, Snare confirmed). If information is distributed: nodes can decouple unilaterally and the extraction is institutional choice, not structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_distribution, empirical, 'Distribution of information needed for decoupling decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cascading_constraint_failure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cascading_tr_t0, cascading_constraint_failure, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cascading_tr_t10, cascading_constraint_failure, theater_ratio, 10, 0.5).
narrative_ontology:measurement(cascading_tr_t20, cascading_constraint_failure, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(cascading_be_t0, cascading_constraint_failure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cascading_be_t10, cascading_constraint_failure, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(cascading_be_t20, cascading_constraint_failure, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cascading_constraint_failure, global_infrastructure).
narrative_ontology:affects_constraint(cascading_constraint_failure, supply_chain_concentration).
narrative_ontology:affects_constraint(cascading_constraint_failure, financial_system_interconnection).
narrative_ontology:affects_constraint(cascading_constraint_failure, critical_infrastructure_interdependence).

% DUAL FORMULATION NOTE:
% The cascading constraint failure should be decomposed into substrate-specific constraints: power grid cascade (electrical coupling), telecom cascade (protocol coupling), supply chain cascade (logistics coupling). Each has different ε values reflecting empirical coupling density. This story represents the unified pattern across all three. Upstream constraints (supply_chain_concentration, financial_system_interconnection) influence the cascade risk; downstream impact is measured through critical_infrastructure_interdependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cascading_constraint_failure, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
