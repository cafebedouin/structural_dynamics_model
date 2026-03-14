% ============================================================================
% CONSTRAINT STORY: critical_system_cascading_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_critical_system_cascading_failure, []).

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
 *   constraint_id: critical_system_cascading_failure
 *   human_readable: Critical System Cascading Failure Coordination and Extraction
 *   domain: infrastructure/systems_reliability/governance
 *
 * SUMMARY:
 *   Critical system cascading failures — where localized faults propagate
 *   through tightly coupled infrastructure to cause system-wide collapse —
 *   create a structural tension between the coordination requirements for
 *   preventing cascades and the extraction benefits accruing to centralized
 *   authorities who manage the coordination. The constraint exhibits the full
 *   spectrum of DR classifications: end users experience pure extraction
 *   (snare) with no exit; centralized authorities experience coordination
 *   benefits (rope); distributed operators experience mixed coordination and
 *   extraction (tangled_rope); organized decentralization movements see a
 *   temporary problem with a sunset (scaffold); legacy monitoring systems
 *   persist through institutional inertia (piton); and analytical observers
 *   risk naturalizing what is actually a contingent architectural choice
 *   (false mountain). The extractiveness has risen from 0.35 to 0.58 over the
 *   interval, indicating that the constraint has become increasingly
 *   extractive as centralization has consolidated. The theater ratio rising
 *   from 0.48 to 0.64 suggests that cascade prevention procedures have become
 *   increasingly performative — consuming operational effort while providing
 *   degraded actual protection as system complexity exceeds the coordination
 *   mechanism's capacity.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — cannot exit critical infrastructure; bear full cost of cascades with zero alternatives; suppressed from organizing distributed solutions
 *   - Centralized Infrastructure Authority: Primary beneficiary (institutional/arbitrage) — benefits from cascade coordination role; uses cascade risk to justify consolidation and regulatory leverage; has exit option to shift investments across domains
 *   - Regional Grid Operator: Secondary actor (moderate/constrained) — must coordinate locally but comply with centralized protocols; bears disproportionate compliance costs; structurally asymmetric relationship
 *   - Distributed Resilience Coalition: Organized agents (organized/mobile) — pushing alternative architectures (islanding, microgrids, autonomous control); see clear exit path as technology matures
 *   - Regulatory Authority: Institutional actor (organized/constrained) — coordinates safety standards; captured by centralization interests; constrains alternative architectures through standards
 *   - Legacy SCADA System: Institutional infrastructure (institutional/arbitrage) — maintains performative monitoring; persists through inertia despite degraded capacity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent architectural choice as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(critical_system_cascading_failure, 0.58).
domain_priors:suppression_score(critical_system_cascading_failure, 0.68).
domain_priors:theater_ratio(critical_system_cascading_failure, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(critical_system_cascading_failure, extractiveness, 0.58).
narrative_ontology:constraint_metric(critical_system_cascading_failure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(critical_system_cascading_failure, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(critical_system_cascading_failure, tangled_rope).
narrative_ontology:human_readable(critical_system_cascading_failure, "Critical System Cascading Failure Coordination and Extraction").
narrative_ontology:topic_domain(critical_system_cascading_failure, "infrastructure/systems_reliability/governance").

domain_priors:requires_active_enforcement(critical_system_cascading_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(critical_system_cascading_failure, redundancy_system_operators).
narrative_ontology:constraint_beneficiary(critical_system_cascading_failure, centralized_control_authorities).
narrative_ontology:constraint_victim(critical_system_cascading_failure, distributed_end_users).
narrative_ontology:constraint_victim(critical_system_cascading_failure, system_resilience_incentives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CASCADING VICTIM (SNARE) — End users trapped within critical infrastructure systems cannot exit or diversify. When cascades propagate, they bear full cost with zero alternatives. No coordination benefit accrues to them; maximum extraction with suppression of any organizational response or workaround. The constraint's suppression (0.68) reflects that distributed alternatives are systematically eliminated by consolidation incentives.
constraint_indexing:constraint_classification(critical_system_cascading_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INFRASTRUCTURE AUTHORITY (ROPE) — Centralized control systems benefit from the redundancy coordination: monitoring correlated failures, deploying backup capacity, and managing load-shedding protocols. The authority experiences the constraint as a legitimate coordination mechanism — they solve a genuine problem of preventing runaway cascades. Net beneficiary with arbitrage options (can shift infrastructure investments and regulatory leverage across domains).
constraint_indexing:constraint_classification(critical_system_cascading_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL GRID OPERATOR (TANGLED ROPE) — Operators benefit from the cascade coordination framework (early warning systems, automated disconnect procedures) but are extraction targets when centralized authorities use cascade risk to justify consolidation of control and elimination of local redundancy. Constrained by regulatory architecture and capital requirements; some agency but structurally asymmetric: must comply with centralized protocols while bearing disproportionate compliance costs.
constraint_indexing:constraint_classification(critical_system_cascading_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: DISTRIBUTED RESILIENCE COALITION (SCAFFOLD) — Organized agents (islanding protocols, microgrid standards, decentralized control research) see cascading failures as a temporary problem with a sunset: distributed energy resources, local storage, and autonomous load-shedding can create alternative architectures that replace centralized cascade prevention. Extraction is low because this perspective sees agency and an exit path. Sunset clause: as distributed generation and smart controls mature, the vulnerability to centralized cascade propagation diminishes.
constraint_indexing:constraint_classification(critical_system_cascading_failure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY SCADA SYSTEM (PITON) — Traditional supervisory control architectures persist through institutional inertia long after their functional capacity has been exceeded by system complexity. Theater ratio of 0.64 reflects that SCADA monitoring and automated responses consume substantial operational effort while providing increasingly degraded protection as system interdependencies proliferate. The system is maintained because alternatives haven't fully replaced it, not because it works — a classic piton: degraded function masked by procedural theater.
constraint_indexing:constraint_classification(critical_system_cascading_failure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY BOTTLENECK (TANGLED ROPE) — Regulatory bodies coordinate legitimate safety standards (reserve requirements, voltage limits, ramp rates) that prevent some cascades. But regulatory capture consolidates control in centralized authorities and suppresses alternative verification methods (microgrid testing, distributed autonomous protocols, open-source monitoring). Organized but constrained by legacy institutional structures; both coordination function and asymmetric extraction present.
constraint_indexing:constraint_classification(critical_system_cascading_failure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational timescale, cascading failure appears as an immutable consequence of system interdependency: any sufficiently complex, tightly coupled system exhibits cascade risk. This perspective risks naturalizing what is actually a contingent architectural choice. The engine's false summit detector identifies this as naturalization of a design decision (centralized architecture) rather than as a law of physics.
constraint_indexing:constraint_classification(critical_system_cascading_failure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(critical_system_cascading_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(critical_system_cascading_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(critical_system_cascading_failure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(critical_system_cascading_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(critical_system_cascading_failure, TR),
    TR >= 0.70.

:- end_tests(critical_system_cascading_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. The constraint exhibits genuine coordination benefits for preventing cascade propagation, but extraction accrues to centralized authorities through consolidation of control, suppression of distributed alternatives, and regulatory leverage. The upward trajectory reflects that as system complexity increases, the gap between the coordination problem's difficulty and the performance of the centralized solution widens — more operational effort is expended per cascade prevented, creating space for extraction alongside coordination. Suppression (0.68): High. Multiple barriers prevent adoption of distributed cascade prevention: regulatory standards favor centralized architectures, interconnection requirements suppress small-scale autonomous systems, proprietary SCADA systems prevent interoperability, and the political economy of infrastructure consolidation systematically eliminates alternative pathways. Theater ratio (0.64): Moderate-high and rising. Cascade prevention procedures (reserve requirements, automatic load-shedding, voltage support protocols) consume substantial operational overhead, but their actual effectiveness decreases as systems become more complex and more tightly coupled. Much of the monitoring and control activity is procedural compliance rather than functional prevention — classic theater drift. Claimed type (Tangled Rope): The constraint has both genuine coordination function (preventing some cascades) and asymmetric extraction (consolidation of authority, suppression of alternatives, exclusion of end users from decision-making). Both elements are structurally necessary; neither dominates.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the authority's rope (sees pure coordination benefit) and the end user's snare (sees pure extraction cost) is explained by directionality: the authority has arbitrage exit (low d, negative effective extraction) while the end user has trapped exit (high d, maximum effective extraction). The gap is not a difference in how they perceive the same extraction; it is a difference in the extraction they actually experience because they occupy different structural positions. The scaffold perspective's sunset clause (distributed alternatives will mature and replace centralization) creates a temporal perspectival gap: the coordination mechanism that seems natural and immutable now (mountain view) will become a legacy system maintained through inertia (piton view) as alternatives mature. The theater ratio rising from 0.48 to 0.64 provides a measurement signal of this transition: as actual coordination capacity saturates relative to system complexity, more effort is expended on procedural theater to maintain the appearance of control. The engine's false summit detector should flag the analytical observer's mountain classification: cascade vulnerability is not inherent to coupled systems universally, but rather a specific consequence of centralized architecture choices. Distributed, loosely coupled systems with autonomous local control show different cascade characteristics — they can cascade locally without global propagation. This reveals that the natural law framing naturalizes what is actually a design choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the extraction flow. Centralized authorities (beneficiaries with arbitrage exit) have low d values, experiencing negative effective extraction (they benefit). End users (victims with trapped exit) have high d values near 1.0, experiencing maximum extraction. Regional operators (secondary actors, constrained exit) have moderate d values around 0.65, experiencing moderate extraction. The pipeline computes f(d) from these values: low d → f(d) ≈ -0.12 to 0.02 (institutional/arbitrage beneficiaries experience coordination benefit), high d → f(d) ≈ 1.28 to 1.42 (trapped victims experience maximum extraction), moderate d → f(d) ≈ 0.65-1.0 (constrained actors experience mixed effects). The scope modifier σ(S) amplifies extraction at global scope (σ=1.2): cascade failures that propagate globally concentrate extraction more severely on powerless agents than regional failures would.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through explicit recognition that both coordination and extraction are real and co-present. The system genuinely coordinates cascade prevention — without centralized authorities monitoring and responding to failures, cascades would be worse. But the same coordination mechanism is also the instrument of extraction — consolidation of control, suppression of alternatives, and exclusion of end users from participation. The mandatrophy resolution requires declaring that BOTH observations are true: the architecture is optimally coordinate for preventing some cascades AND it concentrates extraction on powerless end users who have no exit. The false mountain perspective (naturalizing architecture as immutable law) is revealed as such by the structural data: cascade vulnerability is a consequence of design choices (tight coupling, centralized monitoring, suppressed distributed alternatives), not of physics or logic. The snare perspective (end user view) is not contradicted by the rope perspective (authority view) — they are both accurate descriptions of different positions within the same extraction flow. The scaffold perspective (decentralization coalition) points to how the constraint might transition: if distributed architectures mature (autonomous voltage support, islanding protocols, decentralized monitoring), the centralized extraction mechanism loses its functional necessity and the coordination function can be distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    centralization_necessity_claim,
    'Is centralized cascade prevention strictly necessary, or is it a design choice that naturalizes particular organizational interests?',
    'Comparative analysis of decentralized cascade-resistant architectures (islanding, autonomous local control, distributed monitoring) against centralized systems; identification of whether cascade vulnerability is inherent to system physics or emergent from architectural concentration',
    'If necessary: mountain classification confirmed, constraint is natural law. If design choice: centralization is contingent extraction mechanism, constraint reclassifies as snare at global scope. The regulatory capture that suppresses alternative architectures becomes visible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(centralization_necessity_claim, empirical, 'Whether centralized control is necessary or contingent architectural choice').

omega_variable(
    distributed_autonomous_feasibility,
    'Can distributed, autonomous cascade prevention (without centralized coordination) achieve equivalent or superior reliability at reasonable cost?',
    'Real-world pilot data from islanded microgrids, autonomous voltage/frequency support systems, and decentralized demand response; comparison of cascade incidence rates and recovery times across architectures',
    'If feasible: scaffold sunset clause is real, distributed resilience coalition will transition infrastructure away from centralized bottleneck. If infeasible: centralized control is the only coordination mechanism that works, and extraction is a side effect of necessary architecture rather than a primary mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_autonomous_feasibility, empirical, 'Feasibility of distributed autonomous cascade prevention').

omega_variable(
    suppression_mechanism_origin,
    'Is suppression of alternative architectures driven by technical barriers or by institutional interests of centralized authorities?',
    'Analysis of regulatory barriers to distributed generation, microgrid interconnection standards, and autonomous control deployment; identification of technical vs institutional obstacles',
    'If technical: suppression reflects genuine safety constraints (mountain). If institutional: suppression is extractive mechanism (snare). Most likely: mixed, with institutional suppression amplifying technical barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_origin, empirical, 'Whether suppression of alternatives is technical or institutional').

omega_variable(
    cascade_contagion_boundary,
    'What system characteristics determine whether cascades remain localized or propagate globally? Can these boundaries be designed rather than prevented through centralized control?',
    'Mathematical analysis of phase transitions in coupled oscillator networks; identification of control parameters that prevent global contagion; experimental verification in power grid simulators and renewable-heavy test systems',
    'If boundaries can be designed: decentralized architecture becomes viable, constraint transitions to scaffold with clear sunset. If boundaries require centralized monitoring: centralization is unavoidable, constraint remains tangled_rope with indefinite extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cascade_contagion_boundary, empirical, 'Whether cascade boundaries are designable vs require centralized control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(critical_system_cascading_failure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csf_tr_t0, critical_system_cascading_failure, theater_ratio, 0, 0.48).
narrative_ontology:measurement(csf_tr_t5, critical_system_cascading_failure, theater_ratio, 5, 0.57).
narrative_ontology:measurement(csf_tr_t10, critical_system_cascading_failure, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(csf_be_t0, critical_system_cascading_failure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(csf_be_t5, critical_system_cascading_failure, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(csf_be_t10, critical_system_cascading_failure, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(critical_system_cascading_failure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(critical_system_cascading_failure, 0.12).
narrative_ontology:affects_constraint(critical_system_cascading_failure, financial_contagion_systemic_risk).
narrative_ontology:affects_constraint(critical_system_cascading_failure, infrastructure_supply_chain_coupling).
narrative_ontology:affects_constraint(critical_system_cascading_failure, regulatory_monoculture_risk).

% DUAL FORMULATION NOTE:
% Cascading failure is a constraint family with distinct stories for different infrastructure domains (power grids, financial networks, supply chains). Each domain has its own extractiveness and architectural specifics, but all share the core structural pattern: centralization for coordination + extraction mechanism + suppression of distributed alternatives. The upstream constraint is system interdependency (mathematical/physical); this story is about how centralized coordination of cascade prevention creates extraction. Downstream constraints are domain-specific manifestations (financial contagion uses the same coordination architecture as power grids; supply chain cascades follow the same suppression pattern).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(critical_system_cascading_failure, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
