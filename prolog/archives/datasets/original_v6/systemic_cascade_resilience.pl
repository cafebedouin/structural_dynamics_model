% ============================================================================
% CONSTRAINT STORY: systemic_cascade_resilience
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_systemic_cascade_resilience, []).

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
 *   constraint_id: systemic_cascade_resilience
 *   human_readable: Systemic Cascade Resilience Coordination
 *   domain: infrastructure/complex_systems/risk_management
 *
 * SUMMARY:
 *   Systemic cascade resilience coordination represents a fundamental tension
 *   between the technical requirement for rapid, synchronized response to
 *   spreading failures and the institutional preference for centralized
 *   control over distributed adaptation. As interconnected systems (power
 *   grids, supply chains, financial networks, transportation infrastructure)
 *   grow more complex and tightly coupled, the capacity for small failures to
 *   cascade into systemic collapse increases. The standard response is
 *   centralized coordination: real-time monitoring, standardized protocols,
 *   command authority during cascade events, and mandatory information
 *   sharing. This creates a structural constraint that exhibits all six DR
 *   types from different perspectives. The same coordination mechanism
 *   appears as a genuine public good (rope), a temporary transition to
 *   decentralized resilience (scaffold), a degraded ritual enforced by
 *   institutional inertia (piton), a mixed coordination-extraction hybrid
 *   (tangled rope), pure extraction of local autonomy (snare), or a
 *   thermodynamic law of complex systems (mountain), depending on the
 *   observer's structural position. Extractiveness has risen from 0.32 to
 *   0.58 over the measurement interval, indicating that institutional
 *   authorities have increasingly demanded operator compliance at the cost of
 *   local resilience capacity. Theater ratio has similarly risen from 0.38 to
 *   0.58, suggesting that compliance focus has shifted from actual cascade
 *   prevention toward protocol adherence.
 *
 * KEY AGENTS:
 *   - Centralized System Integrators: Primary beneficiary (institutional/arbitrage) — captures authority to command all components during cascade events; has full optionality to maintain or abandon the system
 *   - Distributed Infrastructure Operators: Primary victim (powerless/trapped) — must participate in mandatory coordination while losing local autonomy and bearing disproportionate cascade costs
 *   - Regional Authorities: Secondary victim (moderate/constrained) — benefit from centralized data and prediction models but are constrained by mandatory protocols that supersede local knowledge
 *   - Edge System Resilience: Tertiary victim (powerless/trapped) — the capacity for distributed systems to adapt locally and contain cascades is suppressed by centralized control requirements
 *   - Decentralization Movement: Organized agents (organized/constrained) — building microgrids and autonomous local systems as alternative resilience pathways with generational sunset logic
 *   - Legacy Interconnection Standard: Institutional actor (institutional/arbitrage) — maintains performative protocols that enforce compliance rather than prevent cascades
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the efficiency-resilience tradeoff as immutable when it is contingent on system design and economic choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(systemic_cascade_resilience, 0.58).
domain_priors:suppression_score(systemic_cascade_resilience, 0.62).
domain_priors:theater_ratio(systemic_cascade_resilience, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(systemic_cascade_resilience, extractiveness, 0.58).
narrative_ontology:constraint_metric(systemic_cascade_resilience, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(systemic_cascade_resilience, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(systemic_cascade_resilience, tangled_rope).
narrative_ontology:human_readable(systemic_cascade_resilience, "Systemic Cascade Resilience Coordination").
narrative_ontology:topic_domain(systemic_cascade_resilience, "infrastructure/complex_systems/risk_management").

domain_priors:requires_active_enforcement(systemic_cascade_resilience).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(systemic_cascade_resilience, centralized_coordination_authorities).
narrative_ontology:constraint_beneficiary(systemic_cascade_resilience, system_integrators).
narrative_ontology:constraint_victim(systemic_cascade_resilience, distributed_infrastructure_operators).
narrative_ontology:constraint_victim(systemic_cascade_resilience, edge_system_resilience).
narrative_ontology:constraint_victim(systemic_cascade_resilience, local_adaptation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISTRIBUTED INFRASTRUCTURE OPERATOR (SNARE) — Local grid operators, regional power systems, and distributed networks cannot exit the cascade coordination regime without losing operational legitimacy. They face mandatory participation in centralized monitoring protocols, load-shedding commands, and interdependency reporting while bearing disproportionate cost of cascade failure. No alternative coordination pathway available. Maximum experienced extraction.
constraint_indexing:constraint_classification(systemic_cascade_resilience, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL AUTHORITY (TANGLED ROPE) — Regional system operators benefit from centralized real-time data on network state and cascade prediction models, enabling better local decisions. Simultaneously constrained by mandatory protocols that supersede local knowledge, force specific mitigation actions, and create liability for non-compliance. Mixed benefit and extraction — genuine coordination function embedded in asymmetric control.
constraint_indexing:constraint_classification(systemic_cascade_resilience, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRALIZED SYSTEM INTEGRATOR (ROPE) — Benefits from authority to command all system components during cascade events. Experiences the constraint as pure coordination: standardized protocols enable rapid information flow and synchronized response. Has full exit optionality (can abandon the system entirely) but chooses to maintain it. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(systemic_cascade_resilience, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZATION MOVEMENT (SCAFFOLD) — Organized agents (distributed energy advocates, mesh network builders, local resilience coalitions) view centralized cascade coordination as temporary — microgrids, redundant pathways, and local autonomous systems will sunset the dependency on global coordination. Sees itself building alternative resilience pathways with explicit time horizon (20-50 years as grid decentralizes). Low effective extraction because the coalition has agency and explicit sunset logic.
constraint_indexing:constraint_classification(systemic_cascade_resilience, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY INTERCONNECTION STANDARD (PITON) — The formal protocols and interconnection standards that coordinate cascade response are substantially performative. They mandate actions (load-shedding sequences, voltage thresholds, communication hierarchies) that were designed for lower-complexity, slower-failure-mode systems. Modern cascades often exceed design assumptions. The standard persists through institutional inertia and legal obligation despite reduced functional verification. Theater ratio high because enforcement focus is on protocol compliance rather than cascade prevention.
constraint_indexing:constraint_classification(systemic_cascade_resilience, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a civilizational/universal perspective, some degree of cascade coordination is inherent to complex interconnected systems: the fundamental tradeoff between system efficiency (tight coupling, high utilization) and resilience (loose coupling, redundancy) is a law of thermodynamic systems. From this view, the bottleneck in cascade resilience appears immutable. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the efficiency-resilience tradeoff is not inherent but contingent on system design choices and economic incentives.
constraint_indexing:constraint_classification(systemic_cascade_resilience, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(systemic_cascade_resilience_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(systemic_cascade_resilience, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(systemic_cascade_resilience, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(systemic_cascade_resilience, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(systemic_cascade_resilience, TR),
    TR >= 0.70.

:- end_tests(systemic_cascade_resilience_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Centralized cascade coordination captures significant benefits for system integrators (authority, information advantage, bailout protection) while imposing costs on distributed operators (loss of autonomy, mandatory participation, liability exposure). The extractiveness is not extreme because some genuine coordination function exists — cascades do require synchronized response — but the portion of suppression and authority that serves coordination vs institutional control is ambiguous. The rising trajectory (0.32 → 0.58) indicates that authorities have increasingly demanded operator compliance at increasing cost to local resilience capacity. Suppression (0.62): High. Distributed operators face structural barriers to alternative coordination: legal mandates, interconnection standards, interoperability requirements, and liability exposure for non-compliance. Suppression is enforced through regulatory framework rather than physical force, but equally binding. Theater ratio (0.58): Moderate-high and rising. The formal coordination protocols emphasize compliance measurement (load-shedding sequences executed, voltage thresholds maintained, communication logs filed) rather than cascade prevention outcomes. As systems have grown more complex beyond design assumptions, the protocols have become increasingly theatrical — enforced because they are mandatory, not because they prevent cascades.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Centralized authorities see rope — solving the genuine problem of coordinating response across heterogeneous systems. Distributed operators see snare — trapped in mandatory participation with asymmetric cost bearing. Regional authorities see tangled rope — genuine benefits from centralized data mixed with forced constraints on local decision-making. The decentralization movement sees scaffold — centralized coordination is temporary; microgrids and autonomous systems will sunset this dependency. The legacy standard sees itself as piton — performative protocols enforced by institutional obligation despite reduced functional fit. The civilizational analytical observer risks mountain — the efficiency-resilience tradeoff appears immutable. But the structural data reveals this as false summit: the tradeoff is contingent on how tightly systems are coupled and how costs are distributed, not on thermodynamic law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position: centralized system integrators with arbitrage exits experience low d (they can walk away), shifting extraction toward them as negative f(d). Distributed operators with trapped exits experience high d (no exit), maximizing experienced extraction through f(d). Regional authorities with constrained exits occupy the middle. The pipeline uses these context parameters to compute chi for each perspective. Beneficiary declarations (centralized authorities, system integrators) feed low directionality; victim declarations (distributed operators, edge resilience, local adaptation) feed high directionality. The perspective-dependent chi values produce the perspectival gap: rope at institutional level becomes snare at powerless level.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves mandatrophy by showing that the classification depends entirely on the observer's structural position relative to the coordination mechanism. The mandatrophy is not 'which type is correct?' but 'who controls the coordination choice?' If decentralized systems can provide equivalent resilience, centralized coordination is revealed as institutional preference disguised as technical necessity — shifting the classification sharply toward snare across all victim perspectives. If decentralization is technically infeasible, centralized coordination may be unavoidable — but even then, the tangled rope classification reflects that asymmetric extraction is happening alongside genuine coordination, not that the extraction is justified. The false summit (mountain perspective) naturalizes what is actually a contingent design choice. The scaffold perspective's decentralization timeline (20-50 years) is empirically testable: if microgrids and autonomous systems actually reduce cascade risk, the scaffold sunset is real and the constraint is temporary. If they don't, the constraint hardens into permanent necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cascade_definition_boundary,
    'Where is the boundary between acceptable interdependency spillover and extractive cascade risk transfer?',
    'Historical analysis of cascade events and their cost distribution; comparison of pre-event risk assessments vs actual failure modes; longitudinal tracking of whether centralized predictions actually prevent distributed operators'' losses.',
    'If boundary < 0.15: many legitimate coordinated responses misclassified as extraction. If boundary > 0.40: significant cascade risk remains uncoordinated and extraction persists hidden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cascade_definition_boundary, empirical, 'Boundary between acceptable interdependency and extractive risk transfer').

omega_variable(
    decentralization_technical_feasibility,
    'Can microgrids and distributed autonomous systems actually provide cascade resilience without centralized coordination, or does decentralization merely distribute rather than eliminate cascade risk?',
    'Simulation studies comparing cascade containment in fully distributed vs hybrid vs centralized topologies; real-world data from existing microgrid deployments under cascade-triggering events; analysis of whether decentralized systems exhibit their own cascade failure modes.',
    'If technically feasible: scaffold perspective is structural (decentralization sunset is real). If not: scaffold is aspirational and cascade coordination becomes a permanent natural constraint, shifting classification upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralization_technical_feasibility, empirical, 'Whether decentralization provides true cascade resilience').

omega_variable(
    information_asymmetry_intentionality,
    'Is suppression of local operators'' cascade response autonomy due to genuine technical necessity (cascades must be coordinated to be contained) or due to institutional preference for centralized control?',
    'Comparison of cascade containment effectiveness under different autonomy levels; analysis of whether suppression decreases as local prediction capability improves; audit of decisions where centralized authority overrode local expertise with worse outcomes.',
    'If necessity: suppression value justified; classification remains tangled_rope/snare. If preference: suppression is extractive overhead; classification shifts toward snare across more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_intentionality, empirical, 'Whether information suppression is technically necessary or institutionally preferred').

omega_variable(
    moral_hazard_in_centralized_guarantee,
    'Does centralized cascade coordination create moral hazard — do system operators take higher risks knowing centralized authorities will bail them out during cascades?',
    'Longitudinal analysis of operator risk-taking behavior before vs after centralized coordination adoption; comparison of cascade frequency in coordinated vs non-coordinated systems controlling for system size and complexity; audit trail of bailout decisions and their incentive effects.',
    'If significant moral hazard: the coordination function is partially illusory (doesn''t prevent cascades, just redistributes costs); suppression increases as authorities must constrain behavior to compensate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_hazard_in_centralized_guarantee, empirical, 'Whether centralized coordination creates moral hazard in operator behavior').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(systemic_cascade_resilience, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cascade_tr_t0, systemic_cascade_resilience, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cascade_tr_t10, systemic_cascade_resilience, theater_ratio, 10, 0.52).
narrative_ontology:measurement(cascade_tr_t20, systemic_cascade_resilience, theater_ratio, 20, 0.58).
narrative_ontology:measurement(cascade_tr_t5, systemic_cascade_resilience, theater_ratio, 5, 0.45).

% Extraction over time
narrative_ontology:measurement(cascade_be_t0, systemic_cascade_resilience, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cascade_be_t10, systemic_cascade_resilience, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cascade_be_t20, systemic_cascade_resilience, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cascade_be_t5, systemic_cascade_resilience, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(systemic_cascade_resilience, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(systemic_cascade_resilience, 0.18).
narrative_ontology:affects_constraint(systemic_cascade_resilience, supply_chain_single_point_failure).
narrative_ontology:affects_constraint(systemic_cascade_resilience, financial_contagion_threshold).
narrative_ontology:affects_constraint(systemic_cascade_resilience, interconnected_critical_infrastructure).

% DUAL FORMULATION NOTE:
% Systemic cascade resilience decomposes into constraint families by infrastructure domain: power grid cascades (ε ≈ 0.55), supply chain cascades (ε ≈ 0.48), financial contagion (ε ≈ 0.72). Each exhibits the same structural pattern (centralized coordination) but with different base extractiveness values. Network edges link to domain-specific cascade constraints that this mechanism affects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(systemic_cascade_resilience, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
