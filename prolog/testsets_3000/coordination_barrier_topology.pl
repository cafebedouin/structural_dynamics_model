% ============================================================================
% CONSTRAINT STORY: coordination_barrier_topology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coordination_barrier_topology, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: coordination_barrier_topology
 *   human_readable: Coordination Barrier Topology in Dispersed Labor Systems
 *   domain: organizational_systems/labor_economics/institutional_extraction
 *
 * SUMMARY:
 *   The coordination barrier topology is the structural constraint that
 *   organizing requires trust networks and sustained communication, while
 *   surveillance and dispersion prevent both. This is a mountain constraint
 *   because the barrier emerges from the mathematics of network formation
 *   under observation, not from any particular institutional arrangement. The
 *   constraint is scale-invariant: it applies to factory workers under
 *   panopticon surveillance, gig workers scattered across a city, remote
 *   workers in different time zones, or prisoners in solitary confinement.
 *   The topology is the constraint — the ratio of surveillance-to-privacy,
 *   the post-work dispersion rate, the detection-to-coordination speed ratio.
 *   These are measurable structural properties that determine organizing
 *   feasibility independent of the actors involved. The constraint is NOT the
 *   employer's decision to implement open-plan offices or algorithmic
 *   scheduling (those are upstream extractive constraints that exploit this
 *   mountain). The constraint is the graph-theoretic fact that dense trust
 *   networks cannot form under continuous observation and that sustained
 *   coordination cannot occur when participants are temporally and spatially
 *   dispersed faster than trust-building requires.
 *
 * KEY AGENTS:
 *   - Isolated Worker: Powerless/trapped — experiences the topology as an immutable barrier to organizing
 *   - Organizer: Moderate/constrained — has organizing skills but faces hard topological limits
 *   - Labor Movement: Organized/mobile — recognizes the structural shift from historical organizing conditions
 *   - Employer: Institutional/arbitrage — exploits the pre-existing topological constraint through workspace and schedule design
 *   - Regulator: Powerful/mobile — sees the topology as a constraint on what policy can achieve
 *   - Analytical Observer: Analytical/analytical — identifies the graph-theoretic properties that constitute the barrier
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coordination_barrier_topology, 0.08).
domain_priors:suppression_score(coordination_barrier_topology, 0.03).
domain_priors:theater_ratio(coordination_barrier_topology, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coordination_barrier_topology, extractiveness, 0.08).
narrative_ontology:constraint_metric(coordination_barrier_topology, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(coordination_barrier_topology, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coordination_barrier_topology, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(coordination_barrier_topology, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coordination_barrier_topology, mountain).
narrative_ontology:human_readable(coordination_barrier_topology, "Coordination Barrier Topology in Dispersed Labor Systems").
narrative_ontology:topic_domain(coordination_barrier_topology, "organizational_systems/labor_economics/institutional_extraction").

domain_priors:emerges_naturally(coordination_barrier_topology).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED WORKER (MOUNTAIN) — Experiences the coordination barrier as an immutable physical constraint. Cannot organize during work (surveillance), cannot organize after work (geographic dispersion), cannot organize over time (detection speed exceeds coordination speed). The topology itself is the constraint, independent of who designed it or why.
constraint_indexing:constraint_classification(coordination_barrier_topology, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ORGANIZER (MOUNTAIN) — Even with organizing experience and resources, the topology imposes hard limits. Trust-building requires repeated private interaction; the workspace provides none. Planning requires sustained coordination time; post-work dispersion prevents it. Detection algorithms operate faster than human coordination can complete. These are structural barriers, not strategic choices.
constraint_indexing:constraint_classification(coordination_barrier_topology, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LABOR MOVEMENT (MOUNTAIN) — Historical organizing succeeded when workers shared neighborhoods, taverns, churches — spatial and temporal overlap that enabled trust networks to form outside employer surveillance. Modern dispersion eliminates these preconditions. The movement sees this as a structural shift in the topology of organizing possibility, not as a defeatable tactic.
constraint_indexing:constraint_classification(coordination_barrier_topology, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: EMPLOYER (MOUNTAIN) — The employer who implements open-plan surveillance and shift dispersion experiences these as design choices, but the underlying coordination barrier is not their creation. Any sufficiently surveilled and dispersed population faces the same organizing difficulty, regardless of intent. The employer exploits a pre-existing topological constraint.
constraint_indexing:constraint_classification(coordination_barrier_topology, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — The coordination barrier is a graph-theoretic property: organizing requires dense trust networks (high clustering coefficient) and sustained communication channels (low latency, high bandwidth). Surveillance reduces edge density in the trust graph; geographic dispersion increases communication latency; temporal scatter reduces available bandwidth. These are structural properties of the coordination topology, invariant across institutional contexts. The constraint is a mountain because it derives from the mathematics of network formation under adversarial observation.
constraint_indexing:constraint_classification(coordination_barrier_topology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: REGULATOR (MOUNTAIN) — Labor law can mandate break rooms, limit surveillance, or require predictable schedules, but cannot repeal the underlying coordination mathematics. If workers are sufficiently dispersed in space and time, organizing remains structurally difficult even with legal protections. The regulator sees the topology as a constraint on what policy can achieve.
constraint_indexing:constraint_classification(coordination_barrier_topology, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coordination_barrier_topology_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(coordination_barrier_topology, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coordination_barrier_topology, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(coordination_barrier_topology, ExtMetricName, E),
    domain_priors:suppression_score(coordination_barrier_topology, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(coordination_barrier_topology),
    narrative_ontology:constraint_metric(coordination_barrier_topology, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(coordination_barrier_topology, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(coordination_barrier_topology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The coordination barrier itself extracts nothing — it is a structural property of network formation under observation. The extraction occurs in the upstream constraints (quota systems, debt traps) that the coordination barrier prevents workers from collectively resisting. The small non-zero value reflects the opportunity cost: in a counterfactual world where organizing were topologically easier, workers would capture more surplus. But this is not extraction by the constraint; it is foregone coordination benefit. Suppression (0.03): Very low. The topology does not suppress alternatives through coercion; it is a mathematical property of the coordination problem. Workers are not prevented from organizing by force; they are prevented by the structural impossibility of forming trust networks under the given spatial, temporal, and observational conditions. The small non-zero value reflects that the topology does make some coordination strategies unavailable (e.g., workplace organizing during shifts), but this is constraint, not suppression. Theater ratio (0.05): Very low. There is no performative component to the coordination barrier. The topology either permits organizing or it does not, based on measurable structural properties. No ritual or ceremony is involved. Accessibility collapse (0.92): Very high. Once the surveillance-to-privacy ratio exceeds a threshold and dispersion rate exceeds another threshold, organizing becomes structurally infeasible for all agents regardless of resources or strategy. The collapse is sharp and universal. Resistance (0.08): Very low. Attempts to organize under adverse topology fail predictably and universally. There are no edge cases where the barrier can be overcome through effort or cleverness when the structural parameters are sufficiently unfavorable. The small non-zero value reflects that marginal improvements in topology (slightly less surveillance, slightly less dispersion) do enable marginal increases in organizing success, so the barrier is not absolutely binary.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap in classification — all perspectives return mountain. The gap is in interpretation: the isolated worker experiences the barrier as a personal failure to organize; the organizer experiences it as a strategic challenge; the labor movement experiences it as a historical shift in organizing conditions; the employer experiences it as a design opportunity; the regulator experiences it as a policy constraint; the analytical observer experiences it as a graph-theoretic property. But all agree that the barrier, once instantiated, is immutable at the relevant time scale. The uniformity of classification is the diagnostic signal: this is a genuine natural law constraint (NL profile confirmed), not a naturalized extractive constraint masquerading as a mountain. The accessibility collapse is high (0.92), resistance is low (0.08), and the constraint emerges naturally from the mathematics of network formation. No agent can overcome the barrier through power, resources, or strategy when the topological parameters are sufficiently adverse.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no beneficiaries or victims in the structural sense. The coordination barrier is a graph-theoretic property that exists independent of any agent's relationship to it. All agents experience it as an immutable constraint, though they occupy different positions relative to the upstream extractive constraints that the barrier protects. The employer benefits from the barrier (it prevents organized resistance to extraction), but the employer does not benefit from the barrier itself — the employer benefits from the extraction that the barrier enables. The distinction is critical: the mountain is the topology; the extraction is upstream. No directionality overrides are needed because no agent has a structural relationship to the constraint that would produce a non-canonical d value. The barrier is equally immutable from all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN CONFIRMATION: This constraint resolves the mandatrophy by demonstrating that not all barriers to collective action are extractive institutional arrangements. Some are genuine structural constraints that emerge from the mathematics of coordination under observation. The coordination barrier topology is a mountain because: (1) it derives from graph theory and information theory, not from institutional design; (2) it exhibits high accessibility collapse — once the parameters cross thresholds, organizing becomes infeasible for all agents; (3) it exhibits low resistance — no strategy overcomes adverse topology; (4) it emerges naturally from the coordination problem itself, not from enforcement. The constraint is NOT a naturalized snare. The employer's decision to implement open-plan surveillance IS extractive (that decision is modeled in upstream constraints like quota_ratchet_asymmetry). But the graph-theoretic fact that surveillance prevents trust network formation is a mountain. The mandatrophy resolution is: distinguish the mountain (coordination topology) from the extractive constraints that exploit it (workspace design, algorithmic scheduling). The mountain enables extraction but is not itself extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coordination_barrier_topology, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coordination_barrier_topology, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of quota_ratchet_asymmetry and debt_trap_compounding. Those upstream constraints are extractive (tangled_rope and snare respectively); they exploit the coordination barrier mountain to prevent collective resistance. The mountain itself is not extractive — it is a structural property of network formation. The decomposition separates the graph-theoretic constraint (this story) from the institutional arrangements that exploit it (upstream stories).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
