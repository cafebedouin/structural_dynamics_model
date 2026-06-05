% ============================================================================
% CONSTRAINT STORY: quantum_error_correction_overhead
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_error_correction_overhead, []).

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
 *   constraint_id: quantum_error_correction_overhead
 *   human_readable: Quantum Error Correction Overhead and Resource Asymmetry
 *   domain: quantum_computing/physical_implementation
 *
 * SUMMARY:
 *   Quantum error correction imposes an overhead cost — additional qubits,
 *   gates, and time cycles — to achieve fault tolerance. The constraint
 *   structure is whether this overhead is an immutable physical law or a
 *   contingent architectural choice susceptible to engineering optimization.
 *   This distinction determines whether resource-constrained laboratories are
 *   trapped by fundamental limits or by technology choices that concentrate
 *   quantum computing power in well-funded institutions. The overhead
 *   requirement presents as distributed burden (all quantum computers must
 *   pay it) but benefits accrue asymmetrically: manufacturers of
 *   fault-tolerant qubits, software vendors, and institutions with sufficient
 *   capital capture rent from the overhead requirement. Near-term quantum
 *   utility research is constrained by the overhead argument even though
 *   hybrid and error-mitigated approaches could provide practical advantage
 *   without crossing into fault-tolerance regimes. The constraint exhibits
 *   tangled rope structure (genuine coordination problem of error correction
 *   combined with asymmetric extraction of research priority and resource
 *   allocation) with false mountain overlay (threshold theorem invoked as
 *   natural law when architectural choices are contingent). Theater ratio is
 *   low (0.35) because the overhead problem is technically real, not
 *   performative, but the framing of overhead as inevitable rather than
 *   optimizable introduces performative content.
 *
 * KEY AGENTS:
 *   - Resource-Constrained Laboratories: Primary victim (powerless/trapped) — cannot perform quantum computing research without overhead resources they lack; trapped in dependence on well-funded institutions that have solved overhead engineering.
 *   - Near-Term Quantum Utility Research: Secondary victim (moderate/constrained) — research agenda distorted away from practical advantage demonstration toward long-term fault-tolerance prerequisite work; benefit from overhead frameworks but face extraction via misdirected incentives.
 *   - Fault-Tolerant Qubit Manufacturers: Primary beneficiary (institutional/arbitrage) — overhead requirement creates direct market for their engineering solutions; can exit to classical computing or other domains if quantum fails.
 *   - Quantum Software and Compiler Vendors: Secondary beneficiary (institutional/arbitrage) — overhead creates demand for optimization software and compilation strategies; arbitrage exit available.
 *   - Standards and Benchmarking Community: Mixed (organized/mobile) — benefit from clarity on overhead measurement standards but also experience extraction pressure as overhead becomes political instrument in competitive benchmarking.
 *   - Error Mitigation Research Community: Organized agent with exit path (organized/constrained) — developing scaffold structures (hybrid approaches, variational methods) that bypass full overhead while fault tolerance matures.
 *   - Surface Code Research Establishment: Institutional inertia (institutional/arbitrage) — maintains surface code orthodoxy through research momentum, tool ecosystems, and publication patterns; piton classification reflects degraded function maintained through momentum rather than continuous optimization.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent architecture (surface codes, qubit scaling assumptions) as immutable threshold theorem physics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_error_correction_overhead, 0.52).
domain_priors:suppression_score(quantum_error_correction_overhead, 0.48).
domain_priors:theater_ratio(quantum_error_correction_overhead, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_error_correction_overhead, extractiveness, 0.52).
narrative_ontology:constraint_metric(quantum_error_correction_overhead, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(quantum_error_correction_overhead, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_error_correction_overhead, tangled_rope).
narrative_ontology:human_readable(quantum_error_correction_overhead, "Quantum Error Correction Overhead and Resource Asymmetry").
narrative_ontology:topic_domain(quantum_error_correction_overhead, "quantum_computing/physical_implementation").

domain_priors:requires_active_enforcement(quantum_error_correction_overhead).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_error_correction_overhead, fault_tolerant_qubit_manufacturers).
narrative_ontology:constraint_beneficiary(quantum_error_correction_overhead, quantum_software_vendors).
narrative_ontology:constraint_beneficiary(quantum_error_correction_overhead, theoretical_advantage_claimants).
narrative_ontology:constraint_victim(quantum_error_correction_overhead, resource_constrained_laboratories).
narrative_ontology:constraint_victim(quantum_error_correction_overhead, near_term_quantum_utility_research).
narrative_ontology:constraint_victim(quantum_error_correction_overhead, measurement_precision_budgets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-CONSTRAINED LABORATORY (SNARE) — Trapped by the overhead paradox: error correction requires order-of-magnitude more qubits than the logic problem itself, yet the overhead is presented as inevitable physics rather than a contingent architectural choice. Lab cannot exit without abandoning quantum computing entirely. Extraction mechanism: overhead requirements exhaust budgets and force dependence on well-funded institutions that have solved the overhead problem.
constraint_indexing:constraint_classification(quantum_error_correction_overhead, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEAR-TERM QUANTUM UTILITY RESEARCH (TANGLED ROPE) — Constrained by both overhead requirements and career incentive misalignment (fault tolerance papers get citations while near-term utility gets dismissed as 'NISQ'). But also benefits from theoretical frameworks and error mitigation techniques developed under overhead-driven research. Mixed extraction and coordination — the same overhead that constrains enables measurement of quantum advantage.
constraint_indexing:constraint_classification(quantum_error_correction_overhead, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FAULT-TOLERANT QUBIT MANUFACTURERS (ROPE) — Experiences overhead as pure coordination problem: larger addressable market for their products. Overhead requirement creates demand for their engineering solutions. Net beneficiary with arbitrage exit — can shift to alternative markets if quantum computing fails.
constraint_indexing:constraint_classification(quantum_error_correction_overhead, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: QUANTUM SOFTWARE VENDORS (ROPE) — Overhead creates technical problems that require software solutions: error correction code optimization, qubit allocation strategies, compilation overhead reduction. Pure coordination from this perspective — the overhead is real physics, and solving it creates market for their services.
constraint_indexing:constraint_classification(quantum_error_correction_overhead, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: QUANTUM COMPUTING STANDARDS COMMUNITY (TANGLED ROPE) — Organized agents (IEEE, ISO working groups, industry consortia) benefit from overhead clarification efforts and standardization work, but also face extraction pressure: overhead measurements and benchmarking become complex political instruments. As organized actors with substantial agency (mobile exit to other computing domains), they experience moderate extraction.
constraint_indexing:constraint_classification(quantum_error_correction_overhead, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ERROR MITIGATION RESEARCH (SCAFFOLD) — Emerging hybrid approaches (error mitigation, QAOA without full fault tolerance, variational algorithms) represent temporary scaffold structures: they reduce overhead requirements while fault tolerance research matures. Lower extraction because agents see a genuine exit path — as classical simulation improves or hybrid methods mature, the full-overhead requirement becomes optional rather than mandatory. Sunset clause is implicit: classical simulation limits will establish when hybrid approaches become equivalent to full quantum advantage.
constraint_indexing:constraint_classification(quantum_error_correction_overhead, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: SURFACE CODE ORTHODOXY (PITON) — Surface codes have become the institutionalized default error correction framework despite only modest theoretical advantages over other codes in many regimes. Research communities maintain surface code focus through inertia, publication patterns, and tool availability rather than continuous optimization. Theater ratio high because alternative codes are systematically underdeveloped despite theoretical promise. Piton classification: functional but degraded, maintained through institutional momentum.
constraint_indexing:constraint_classification(quantum_error_correction_overhead, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / THRESHOLD THEOREM (MOUNTAIN) — From civilizational/universal perspective, quantum error correction overhead is an immutable consequence of the threshold theorem: any quantum computer with error rates below threshold can achieve arbitrarily long computations through recursive error correction. This appears as natural law. However, structural data reveals this as false summit: the threshold theorem is a mathematical truth about ideal architectures, not about implementable systems. The contingent architecture choice (surface codes, qubit overhead scaling laws, interconnect topology) naturalizes what could be otherwise. The mountain classification instantiates the oracle gap.
constraint_indexing:constraint_classification(quantum_error_correction_overhead, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_error_correction_overhead_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quantum_error_correction_overhead, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quantum_error_correction_overhead, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_error_correction_overhead, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quantum_error_correction_overhead, TR),
    TR >= 0.70.

:- end_tests(quantum_error_correction_overhead_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The overhead requirement creates genuine asymmetry in resource allocation. Manufacturing and software vendors benefit from the requirement existing; resource-constrained labs bear the cost. The value reflects that this is a real coordination problem (error correction genuinely necessary for fault tolerance) layered with contingent extraction (overhead could be reduced through different architectural choices, but those choices concentrate power). The trajectory (0.35 → 0.52 over 15 years) reflects growing recognition that overhead can be engineered down but is being maintained artificially high through research focus allocation. Suppression (0.48): Moderate. Labs have genuine barriers to entry (capital requirements for qubit systems, expertise in error correction, computational resources for benchmarking) but not absolute barriers — alternative approaches (hybrid, error-mitigated) exist and are being developed. Suppression would be higher if alternatives were truly blocked, but the emergence of scaffold structures (error mitigation research) shows some mobility. Theater ratio (0.35): Low. The overhead problem is technically real — error correction does impose costs — but there is performative content in how overhead is framed as immutable destiny rather than optimizable engineering problem. The low ratio reflects that the constraint is mostly about real physics and engineering, not about theatrical mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The magnitude of perspectival gap reveals the constraint structure. The snare (resource-constrained lab) classifies overhead as immutable extraction. The rope (manufacturers) classify it as pure coordination. The tangled_rope (utility researchers, standards community) experience mixed burden and benefit. The scaffold (error mitigation community) see temporary overhead with exits. The piton (surface code orthodoxy) see degraded ritual. The mountain (analytical/threshold theorem view) risks naturalizing contingency. The gap is not disagreement about facts (all perspectives agree overhead exists) but about whether it is mutable or immutable, beneficial or extractive, necessary or contingent. This is the signature of a constraint where architecture choices have been naturalized as physical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Qubit manufacturers derive d ≈ 0.08–0.12 from beneficiary status + arbitrage exit + institutional power. They experience the constraint as pure coordination (rope) because they benefit and can exit. Resource-constrained labs derive d ≈ 0.92–0.98 from victim status + trapped exit + powerless status. They experience the constraint as extraction (snare) because they bear costs and cannot exit. Near-term utility researchers derive d ≈ 0.60–0.70 from victim status + constrained exit + moderate power. They experience mixed burden (tangled_rope) because they have some agency but real barriers. The directionality pipeline correctly produces perspectival gaps: same constraint, vastly different d values, different classifications. No overrides needed — the structural data produces the correct distribution.
 *
 * MANDATROPHY ANALYSIS:
 *   ENTANGLEMENT RESOLUTION: The mandatrophy here is 'Is overhead physical law or engineering contingency?' The threshold theorem (mountain) is a true mathematical fact: any quantum computer with error rates below threshold can achieve arbitrary depth through recursive error correction. But the theorem is about idealized architectures, not implementable systems. The actual overhead burden (overhead being measured, institutional momentum around surface codes, resource asymmetry) is tangled_rope structure, not mountain. The mountain classification appears only at the analytical/civilizational/universal perspective — the framework correctly identifies this as a false summit because structural data (extractiveness 0.52, suppression 0.48, theater 0.35, beneficiaries explicitly declared, victims explicitly declared) contradicts mountain thresholds. The engine's false summit detector will flag this. The resolution: threshold theorem is true but local (applies to asymptotic fault-tolerance regimes); the observed overhead is contingent (depends on qubit choice, error correction code, interconnect topology, safety margins). Near-term quantum utility research can likely achieve advantage without crossing into full-overhead regimes using hybrid approaches (error mitigation, variational algorithms) — the scaffold perspective is structurally accurate, not aspirational.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_vs_practical_overhead,
    'Is the observed overhead driven by fundamental threshold requirements or by engineering choices in specific implementations?',
    'Comparative analysis across different error correction codes and qubit technologies; measurement of overhead in theoretical vs practical threshold regimes; decomposition of overhead into threshold-driven and architecture-driven components',
    'If threshold-driven (70%+): mountain classification is justified; overhead is immutable physics. If architecture-driven (70%+): overhead is contingent; snare and tangled_rope classifications dominate; alternative architectures could reduce extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_vs_practical_overhead, empirical, 'Threshold theorem necessity vs. engineering choice in overhead').

omega_variable(
    resource_constrained_alternatives,
    'Can near-term quantum utility provide measurable advantage without crossing into fault-tolerance overhead regimes?',
    'Empirical demonstration of quantum advantage in hybrid or error-mitigated regimes; classical simulation limits for specific application classes; comparison of resource requirements (space and time) for hybrid vs fully fault-tolerant approaches on same benchmarks',
    'If yes: scaffold perspective is correct; near-term research has genuine exit path; overhead extraction can be bypassed. If no: all quantum computing requires full overhead; snare classification is structural; trapped agent assessment is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_constrained_alternatives, empirical, 'Whether hybrid approaches can achieve advantage without full overhead').

omega_variable(
    code_selection_optimality,
    'Are surface codes optimal for the dominant implementation modalities, or does their dominance reflect institutional momentum rather than theoretical superiority?',
    'Comparative implementation studies of surface codes vs alternatives (topological codes, concatenated codes, LDPC codes) on equivalent hardware; citation and funding analysis showing research allocation to surface codes vs alternatives; cost-benefit analysis across different physical platforms (superconducting, trapped-ion, photonic)',
    'If optimal: piton classification is incorrect; surface codes are justified choice. If institutional: piton confirmed; research communities are maintaining degraded orthodoxy; alternative codes could reduce overhead with equivalent theoretical grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(code_selection_optimality, empirical, 'Surface code dominance as optimality vs institutional momentum').

omega_variable(
    measurement_precision_tradeoff,
    'Does the stringency of overhead reduction targets (e.g., 1000:1 logical:physical qubit ratios) reflect physical necessity or arbitrary safety margins?',
    'Historical analysis of overhead targets; correlation between target stringency and measured implementations; failure mode analysis separating genuine failure points from conservative engineering margins; benchmark analysis showing quantum advantage achieved at different overhead ratios',
    'If arbitrary margins: targets are extractive theater (agency could reduce them). If necessary: targets reflect genuine physics constraint. Affects interpretation of suppression metric and theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_precision_tradeoff, empirical, 'Overhead targets as physical necessity vs engineering conservatism').

omega_variable(
    interconnect_topology_constraint,
    'Are measured overhead costs primarily determined by qubit-to-qubit coupling requirements (hardware topology) or by information-theoretic demands of error correction?',
    'Detailed cost accounting of overhead: syndrome extraction bandwidth, qubit connectivity requirements, gate fidelity requirements vs information-theoretic bounds; comparison across different topologies (2D surface, 3D, high-dimensional codes, percolation structures); analysis of how much overhead reduction comes from improved connectivity vs improved code theory',
    'If topology-dominated: hardware engineering could substantially reduce overhead through better interconnect designs (extraction can be bypassed). If information-theoretic: overhead is structural (extraction is built into physics). Affects directionality of hardware vs software vendors in extraction pipeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interconnect_topology_constraint, empirical, 'Overhead driven by hardware topology vs information-theoretic bounds').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_error_correction_overhead, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qec_tr_t0, quantum_error_correction_overhead, theater_ratio, 0, 0.28).
narrative_ontology:measurement(qec_tr_t5, quantum_error_correction_overhead, theater_ratio, 5, 0.32).
narrative_ontology:measurement(qec_tr_t10, quantum_error_correction_overhead, theater_ratio, 10, 0.35).
narrative_ontology:measurement(qec_tr_t15, quantum_error_correction_overhead, theater_ratio, 15, 0.33).

% Extraction over time
narrative_ontology:measurement(qec_be_t0, quantum_error_correction_overhead, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qec_be_t5, quantum_error_correction_overhead, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(qec_be_t10, quantum_error_correction_overhead, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(qec_be_t15, quantum_error_correction_overhead, base_extractiveness, 15, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_error_correction_overhead, resource_allocation).
narrative_ontology:affects_constraint(quantum_error_correction_overhead, quantum_advantage_timeline).
narrative_ontology:affects_constraint(quantum_error_correction_overhead, near_term_quantum_utility).
narrative_ontology:affects_constraint(quantum_error_correction_overhead, fault_tolerant_architecture_choices).

% DUAL FORMULATION NOTE:
% Quantum error correction overhead decomposes into at least three structurally distinct constraints: (1) information-theoretic overhead required by any error correction code (ε ≈ 0.15, mountain-like), (2) engineering overhead in current implementations (ε ≈ 0.45, tangled_rope), and (3) research priority asymmetry favoring fault tolerance over near-term utility (ε ≈ 0.65, snare-like for constrained agents). These are related but distinct constraints with different beneficiaries, victims, and measurable extraction mechanisms. The present story aggregates them at ε = 0.52 (weighted toward engineering/priority extraction). Decomposition is recommended for fine-grained policy analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
