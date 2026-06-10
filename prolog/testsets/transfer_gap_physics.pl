% ============================================================================
% CONSTRAINT STORY: transfer_gap_physics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transfer_gap_physics, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: transfer_gap_physics
 *   human_readable: Physical Transfer Gap in Grid-to-Battery Switching
 *   domain: electrical_engineering/power_systems/off_grid_infrastructure
 *
 * SUMMARY:
 *   The transfer gap constraint describes the physical time delay between
 *   grid power loss and battery power availability in off-grid or backup
 *   power systems. Mechanical relay-based transfer switches exhibit a 4-20ms
 *   gap due to contact motion time (limited by spring dynamics and contact
 *   bounce). Passive Schottky diode-based systems exhibit zero gap because
 *   forward-bias conduction begins instantaneously once junction voltage
 *   exceeds threshold (limited only by carrier mobility, effectively
 *   instantaneous at millisecond timescales). The constraint is claimed as
 *   mountain because the gap durations are direct consequences of physical
 *   mechanisms — inertia and contact dynamics for relays, semiconductor
 *   physics for diodes — not institutional arrangements or coordination
 *   mechanisms. However, the presence of identifiable beneficiaries (passive
 *   diode manufacturers who profit from selling zero-gap solutions,
 *   high-availability computing operators who avoid GPU crashes) raises the
 *   false summit question: is the constraint's salience constructed even if
 *   the underlying physics is real?
 *
 * KEY AGENTS:
 *   - GPU Workload: Primary 'victim' (powerless/trapped) — experiences the constraint as an immutable limit; 15ms power loss causes state loss regardless of software design
 *   - Off-Grid Operator: Moderate agent (moderate/constrained) — can choose between relay and diode mechanisms but cannot change the physics of either
 *   - Passive Diode Manufacturers: Primary beneficiary (institutional/arbitrage) — profit from selling zero-gap solutions, but the physical constraint is not constructed to generate that profit
 *   - High-Availability Computing Operators: Secondary beneficiary (institutional/arbitrage) — avoid downtime and data loss by deploying zero-gap systems
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as a direct consequence of Maxwell's equations and solid-state physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transfer_gap_physics, 0.02).
domain_priors:suppression_score(transfer_gap_physics, 0.01).
domain_priors:theater_ratio(transfer_gap_physics, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transfer_gap_physics, extractiveness, 0.02).
narrative_ontology:constraint_metric(transfer_gap_physics, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(transfer_gap_physics, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transfer_gap_physics, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(transfer_gap_physics, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transfer_gap_physics, mountain).
narrative_ontology:human_readable(transfer_gap_physics, "Physical Transfer Gap in Grid-to-Battery Switching").
narrative_ontology:topic_domain(transfer_gap_physics, "electrical_engineering/power_systems/off_grid_infrastructure").

domain_priors:emerges_naturally(transfer_gap_physics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transfer_gap_physics, passive_diode_manufacturers).
narrative_ontology:constraint_beneficiary(transfer_gap_physics, high_availability_computing_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(transfer_gap_physics, gpu_workload).
narrative_ontology:constraint_victim(transfer_gap_physics, off_grid_operator).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The computational process running on GPU hardware during a grid loss event. Experiences a 15ms power interruption as immediate state loss — register contents, cache state, and in-flight computations are lost. Has no agency over the switching mechanism and cannot exit the constraint. The cost is borne directly: crashed process, lost work, potential data corruption.
narrative_ontology:constraint_stakeholder(transfer_gap_physics, gpu_workload, payer,
    powerless, immediate, trapped, universal).

% The individual or organization operating off-grid or backup power infrastructure. Faces the choice between relay-based transfer (cheap, 4-20ms gap, GPU crashes likely) and diode-based transfer (expensive, zero gap, GPU survives). Pays in capital cost for zero-gap solutions or in operational cost (downtime, data loss) for gapped solutions. Constrained by budget and criticality requirements but has agency to choose between mechanisms.
narrative_ontology:constraint_stakeholder(transfer_gap_physics, off_grid_operator, payer,
    moderate, biographical, constrained, regional).

% Manufacturers of passive Schottky diode-based UPS systems and zero-transfer-time power electronics. Profit from selling solutions to the transfer gap problem. Can enter or exit the market based on profitability. The benefit is real (they collect revenue) but incidental to the physical constraint — they did not create the gap and cannot maintain it through suppression. Their profit derives from solving a genuine physical problem, not from constructing or enforcing the problem.
narrative_ontology:constraint_stakeholder(transfer_gap_physics, passive_diode_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Data centers, research computing facilities, and high-performance computing operators who deploy zero-gap power systems to avoid downtime. Benefit by avoiding GPU crashes, data loss, and service interruptions. Have the capital and technical sophistication to deploy passive diode solutions. Can choose between high-availability infrastructure (expensive, zero gap) and standard infrastructure (cheap, gap present) based on criticality and budget.
narrative_ontology:constraint_stakeholder(transfer_gap_physics, high_availability_computing_operators, beneficiary,
    institutional, biographical, arbitrage, global).

% The physicist or electrical engineer analyzing the constraint from a civilizational perspective. Sees the transfer gap as a direct consequence of Maxwell's equations (for electromagnetic relay dynamics) and solid-state physics (for diode forward-bias characteristics). Neither collects from nor pays into the constraint — observes it as a measurement of physical law.
narrative_ontology:constraint_stakeholder(transfer_gap_physics, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint does not coordinate — it is a physical limit. However, the market for zero-gap solutions coordinates around the constraint: manufacturers signal capability (zero transfer time), operators signal criticality (willingness to pay for zero gap), and the price mechanism allocates zero-gap solutions to high-criticality applications.
% TRANSFER_FUNCTION: Capital flows from high-availability operators to passive diode manufacturers in exchange for zero-gap power systems. Operational cost (downtime, data loss) is avoided by operators who pay the capital cost. The transfer is a market transaction solving a real physical problem, not rent extraction.
% ABSENT_VOICES: Low-criticality operators who cannot afford zero-gap solutions are present in the market but priced out. They are not excluded from the conversation — they choose relay-based systems because the capital cost of zero-gap exceeds the operational cost of occasional downtime. Their voice is present in the market as price sensitivity.
% DISAPPEARANCE_RATIONALE: If the transfer gap disappeared (e.g., if mechanical relays could switch instantaneously, or if passive diodes required finite switching time), the market for zero-gap solutions would collapse. High-availability operators would no longer pay premium prices for passive diode systems. Manufacturers would lose the product differentiation that justifies higher margins. The rearrangement is economic (market structure changes) but the underlying arrangements (who needs continuous power, who can pay for it) depend on the constraint's existence.
% FOUNDING_PROBLEM: The founding problem is the physical requirement for continuous power in critical applications (medical life support, military command and control, financial transaction processing, scientific computing) combined with the reality of grid instability and the mechanical limitations of early transfer switches. The problem predates commercial UPS markets — military and medical applications required power continuity in the 1950s-60s, before solid-state transfer switches were commercially available.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by: (1) IEEE standards for power quality (IEEE 1100, IEEE 446) that specify maximum tolerable interruption durations for different application classes, predating modern UPS markets; (2) military specifications (MIL-STD-704, MIL-STD-1399) for aircraft and shipboard power systems that required continuous power during generator switching, documented since the 1960s; (3) medical device standards (IEC 60601) that specify power continuity requirements for life-support equipment, independent of UPS vendor claims. The problem's status as 'live' is corroborated by ongoing grid instability (documented in utility reliability reports, NERC data) and increasing criticality of computing workloads (data center downtime cost studies, financial transaction processing requirements).
narrative_ontology:disappearance_verdict(transfer_gap_physics, world_rearranges).
narrative_ontology:founding_problem_status(transfer_gap_physics, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GPU WORKLOAD (MOUNTAIN) — The computational process has no agency over the physical switching mechanism. A 15ms power interruption causes state loss regardless of software design, institutional arrangements, or economic incentives. The constraint is experienced as an immutable physical limit.
constraint_indexing:constraint_classification(transfer_gap_physics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: OFF-GRID OPERATOR (MOUNTAIN) — The operator can choose between relay-based and diode-based transfer mechanisms, but cannot eliminate the physical constraint itself. The choice is between a 4-20ms gap (mechanical relay) and zero gap (passive diode forward-bias). The constraint that mechanical switching takes time is immutable; the constraint that diode forward-bias is instantaneous is immutable. The operator's agency is in selecting which physical mechanism to deploy, not in changing the physics.
constraint_indexing:constraint_classification(transfer_gap_physics, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UPS MANUFACTURER (MOUNTAIN) — Manufacturers benefit economically from selling zero-transfer-time solutions, but the physical constraint is not constructed to generate that benefit. The diode's zero switching time is a consequence of solid-state physics (no moving parts, forward-bias is instantaneous once threshold voltage is exceeded), not an institutional arrangement. The manufacturer's profit derives from solving a real physical problem, not from creating or maintaining the problem.
constraint_indexing:constraint_classification(transfer_gap_physics, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — The transfer gap is a direct consequence of Maxwell's equations and solid-state physics. Mechanical relay switching requires physical contact motion (limited by inertia and spring dynamics); passive diode forward-bias requires only that junction voltage exceed threshold (limited by carrier mobility, effectively instantaneous at human timescales). No institutional arrangement, economic incentive, or coordination mechanism can change these physical limits. The constraint emerges naturally from the structure of electromagnetism and materials science.
constraint_indexing:constraint_classification(transfer_gap_physics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transfer_gap_physics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(transfer_gap_physics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transfer_gap_physics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transfer_gap_physics, ExtMetricName, E),
    domain_priors:suppression_score(transfer_gap_physics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transfer_gap_physics),
    narrative_ontology:constraint_metric(transfer_gap_physics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transfer_gap_physics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transfer_gap_physics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.02): Near-zero. The constraint extracts negligibly from those it governs. The GPU workload 'pays' in the sense that state loss occurs during the gap, but this is not extraction in the DR sense — no agent collects from the gap's existence. The off-grid operator pays for zero-gap solutions, but this is payment for solving a real problem, not rent extraction. The manufacturers' profit is incidental to the physical constraint, not its purpose. Suppression (0.01): Near-zero. No coercion or lack of alternatives. The operator can choose relay-based (cheap, gap present) or diode-based (expensive, zero gap) systems. The choice is constrained by budget and criticality requirements, but alternatives are not suppressed. Theater ratio (0.03): Near-zero. Minimal performative activity. Oscilloscope measurements directly verify gap duration; GPU crash rates directly measure functional impact. No institutional ritual mediates the constraint's operation. Accessibility collapse (0.92): Very high. Once the physical mechanism is understood, alternatives collapse almost completely. You cannot make a mechanical relay switch faster than its spring constant and contact mass allow. You cannot make a passive diode slower than its carrier mobility allows (without adding external components, which would be a different topology). Resistance (0.04): Near-zero. The constraint meets almost no active resistance because it is recognized as a physical limit. No advocacy groups campaign against the transfer gap; no political movements seek to eliminate it through policy.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify the constraint as mountain, which is unusual and diagnostic. The uniformity arises because the constraint is a genuine physical limit with no institutional mediation. The powerless agent (GPU workload) experiences it as immutable. The moderate agent (off-grid operator) can choose between mechanisms but cannot change the physics. The institutional agent (UPS manufacturer) benefits economically but does not construct or maintain the constraint. The analytical observer sees it as a direct consequence of fundamental physics. The lack of perspectival gap is itself the signal: when all perspectives agree on mountain, the constraint is either a robust natural law or a successfully naturalized false summit. The omega variables document the irreducible uncertainty: does beneficiary presence indicate false summit, or is the benefit genuinely incidental?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. The GPU workload is a victim (bears the cost of state loss during the gap) with trapped exit (cannot avoid the constraint), yielding high d. However, the constraint's base extractiveness is so low (0.02) that even high d produces negligible effective extraction — the 'victim' experiences a physical limit, not extraction. The off-grid operator is neither clear beneficiary nor victim (pays for solutions but solves a real problem), yielding mid-range d. The passive diode manufacturers are beneficiaries (profit from selling zero-gap solutions) with arbitrage exit (can enter or exit the market), yielding low d and negative effective extraction (they collect from the constraint's solution, not from the constraint itself). The analytical observer has analytical exit and experiences the constraint as a measurement, not an extraction. No directionality overrides are needed because the structural derivation accurately reflects the physical relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that mountain classification can coexist with identifiable beneficiaries when the benefit is incidental to an immutable physical constraint rather than the purpose of a constructed arrangement. The manufacturers profit from solving the transfer gap problem, but they did not create the problem and cannot maintain it through suppression — the gap exists whether or not anyone sells solutions. The false summit detector will evaluate whether the constraint's salience is market-driven (in which case the 'problem' is constructed even if the physics is real) or whether the constraint was salient before commercial solutions existed (in which case the mountain classification is robust). The omega variables document this irreducible uncertainty. The constraint also demonstrates ε-invariance: the gap duration is the same whether measured by oscilloscope (voltage continuity), GPU crash rate (functional impact), or capacitor discharge curve (energy buffer depletion). If these observables disagreed, the constraint would decompose into multiple stories (one for the physical gap, one for computational resilience), but they agree, confirming that this is a single constraint with a single ε.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_naturalness_ambiguity,
    'Does the presence of identifiable beneficiaries (passive diode manufacturers, high-availability operators) indicate that the ''mountain'' classification naturalizes a contingent market structure, or is the benefit genuinely incidental to an immutable physical constraint?',
    'Historical analysis: Did passive diode technology develop in response to the transfer gap problem, or did the transfer gap problem become salient only after passive diode technology created a market for zero-gap solutions? Counterfactual test: In a world without commercial UPS markets, would the physical constraint still be described identically?',
    'If the constraint''s salience is market-driven, the mountain classification may be a false summit — the ''problem'' is constructed even if the underlying physics is real. If the constraint''s salience predates the commercial solution (e.g., military/medical applications where power continuity was critical before UPS markets existed), the mountain classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_naturalness_ambiguity, conceptual, 'Whether beneficiary presence indicates false summit or incidental benefit from genuine natural law').

omega_variable(
    alternative_topology_sufficiency,
    'Do alternative power system topologies (rotary UPS, flywheel energy storage, supercapacitor buffering) constitute exits from the transfer gap constraint, or merely alternative implementations of the same physical limits?',
    'Engineering analysis of each topology''s transfer characteristics. Rotary UPS: mechanical inertia provides continuous power during grid loss, but startup time from cold state still limited by motor acceleration. Flywheel: similar to rotary but with higher energy density. Supercapacitor: charge/discharge time limited by ESR and capacitance, effectively zero gap if pre-charged. If all topologies face the same fundamental limit (energy storage must be pre-charged or pre-spinning), the constraint is mountain. If some topology eliminates the gap through a different physical mechanism, the constraint is contingent on topology choice.',
    'If alternative topologies eliminate the gap, the ''mountain'' is actually a design choice (tangled_rope or scaffold). If all topologies face equivalent physical limits, the mountain classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_topology_sufficiency, empirical, 'Whether alternative power topologies constitute genuine exits or equivalent physical limits').

omega_variable(
    measurement_methodology_invariance,
    'Is the transfer gap constraint invariant across measurement methodologies (oscilloscope bus voltage, GPU crash rate, capacitor discharge curve), or does the observable choice change the constraint''s apparent extractiveness?',
    'Cross-methodology comparison: oscilloscope measures voltage continuity directly (physical); GPU crash rate measures functional impact (computational); capacitor discharge measures energy buffer depletion (electrical). If all three observables produce the same gap duration for a given switching mechanism, the constraint is ε-invariant (genuine mountain). If observables disagree (e.g., voltage gap is 15ms but GPU survives due to on-die capacitance), the constraint decomposes into multiple stories.',
    'If observables disagree, this story must be split: one for the physical switching gap (mountain), one for the computational resilience to brief interruptions (rope or scaffold). If observables agree, the mountain classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_methodology_invariance, empirical, 'Whether the constraint is invariant across measurement methodologies or decomposes by observable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transfer_gap_physics, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(xfer_gap_theater_t0, transfer_gap_physics, theater_ratio, 0, 0.03).
narrative_ontology:measurement(xfer_gap_theater_t25, transfer_gap_physics, theater_ratio, 25, 0.03).
narrative_ontology:measurement(xfer_gap_theater_t50, transfer_gap_physics, theater_ratio, 50, 0.03).

% Extraction over time
narrative_ontology:measurement(xfer_gap_extract_t0, transfer_gap_physics, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(xfer_gap_extract_t25, transfer_gap_physics, base_extractiveness, 25, 0.02).
narrative_ontology:measurement(xfer_gap_extract_t50, transfer_gap_physics, base_extractiveness, 50, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transfer_gap_physics, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is ε-invariant across measurement methodologies (oscilloscope voltage, GPU crash rate, capacitor discharge). If future analysis reveals that computational resilience to brief interruptions is a structurally distinct constraint with different ε, decompose into: (1) transfer_gap_physics (this story, mountain), (2) computational_resilience_to_interruption (new story, likely rope or scaffold).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
